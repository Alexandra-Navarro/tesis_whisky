# ============================================================
# 16e_sensibilidad_imputacion_gcfid.R
# Sensibilidad del modelo principal (M_pura: Random Forest restringido y
# Ridge) a la exclusion de las unidades sin ningun dato GC-FID medido
# directamente, cuyos 13 predictores del escenario principal quedan
# completamente sustituidos por mediana.
# Tesis whisky chileno - modelamiento quimico-sensorial
# ============================================================

# Pregunta que responde este script: GC-FID es la unica tecnica que aporta
# predictores al escenario principal, pero cubre solo 26 de las 34 unidades
# de M_pura (76%). Las 8 unidades restantes (23.5%) no tienen ningun dato
# GC-FID propio, por lo que sus 13 predictores quedan completamente
# sustituidos por la mediana del pliegue de entrenamiento (dos registros de
# Dalwhinnie, dos de Caol Ila y las cuatro del panel de noviembre). Para
# esas filas, el modelo predice a partir de valores completados y no de
# mediciones directas de esa unidad. Este script reajusta el modelo
# excluyendo esas 8 unidades y reporta MAE y R2 solo sobre las 26 unidades
# con al menos un dato GC-FID real, para verificar si el desempeno del
# modelo principal depende de esas filas totalmente imputadas.
#
# Mismo procedimiento de bootstrap agrupado que 16_validacion_bootstrap_cv.R
# (100 iteraciones, remuestreo con reemplazo por identificador_quimico,
# mismos hiperparametros de ranger/glmnet), aplicado en paralelo a dos
# versiones de M_pura: "completo" (34 unidades, 10 muestras independientes)
# y "sin_imputacion_total" (26 unidades, 6 muestras independientes, excluye
# las 8 unidades sin dato GC-FID medido directamente). No se modifica ni se
# sobrescribe M_pura en ningun archivo: la exclusion es solo interna a este
# script de sensibilidad.

# ------------------------------------------------------------
# 1. Configuracion
# ------------------------------------------------------------

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

paquetes_requeridos <- c("readxl", "dplyr", "tidyr", "stringr", "writexl", "ggplot2")
paquetes_faltantes <- paquetes_requeridos[!vapply(paquetes_requeridos, requireNamespace, logical(1), quietly = TRUE)]
if (length(paquetes_faltantes) > 0) {
  stop(
    "Faltan paquetes requeridos: ", paste(paquetes_faltantes, collapse = ", "),
    "\nInstalalos antes de ejecutar este script."
  )
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)
library(ggplot2)

paquete_ranger_disponible <- requireNamespace("ranger", quietly = TRUE)
if (!paquete_ranger_disponible) {
  stop("Este script requiere el paquete 'ranger' (Random Forest restringido). Instalalo con install.packages('ranger').")
}
if (!requireNamespace("glmnet", quietly = TRUE)) {
  stop("Este script requiere el paquete 'glmnet' (Ridge). Instalalo con install.packages('glmnet').")
}
library(ranger)
library(glmnet)

set.seed(123)

dir_modelamiento <- file.path(dir_data, "modelamiento")
dir_outputs <- file.path(dir_proyecto, "outputs")
dir_outputs_modelamiento <- file.path(dir_outputs, "modelamiento")
dir_outputs_sensibilidad <- file.path(dir_outputs_modelamiento, "sensibilidad_imputacion_gcfid")

dir.create(dir_modelamiento, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_outputs_sensibilidad, recursive = TRUE, showWarnings = FALSE)

ruta_datos_modelamiento <- file.path(dir_modelamiento, "datos_modelamiento.xlsx")
ruta_salida_excel <- file.path(dir_modelamiento, "sensibilidad_imputacion_gcfid.xlsx")

if (!file.exists(ruta_datos_modelamiento)) {
  stop("No existe el archivo requerido: ", ruta_datos_modelamiento,
       "\nEjecuta primero scripts/R/modelamiento/12_preparar_datos_modelamiento.R")
}

# Mismos parametros que 16_validacion_bootstrap_cv.R, para que la
# comparacion sea metodologicamente equivalente.
n_bootstrap <- 100
min_n_target_modelable <- 8
min_n_correlacion <- 5
max_predictores_arboles <- 10
max_predictores_supervisados <- 15
limite_inferior_target <- 0
limite_superior_target <- 5
n_arboles_rf <- 300
max_depth_rf <- 3

target_a_evaluar <- "y_fenolico_comun"
modelos_a_evaluar <- c("random_forest_restringido", "ridge")
escenario_id <- "M_pura"

# ------------------------------------------------------------
# 2. Funciones auxiliares (replicadas de 16_validacion_bootstrap_cv.R para
#    mantener exactamente la misma metodologia de bootstrap)
# ------------------------------------------------------------

guardar_excel_seguro <- function(lista_hojas, ruta) {
  intento <- tryCatch({
    writexl::write_xlsx(lista_hojas, ruta)
    TRUE
  }, error = function(e) FALSE)

  if (!intento) {
    ruta_alt <- sub("\\.xlsx$", paste0("_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"), ruta)
    writexl::write_xlsx(lista_hojas, ruta_alt)
    message("No se pudo sobrescribir el archivo. Se guardo una copia en: ", ruta_alt)
  }
}

convertir_numericamente <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(convertir_numero(x))
}

bt <- function(x) paste0("`", x, "`")
obtener_predictores_x <- function(df) grep("^x_", names(df), value = TRUE)

recortar_prediccion <- function(x) {
  pmin(pmax(as.numeric(x), limite_inferior_target), limite_superior_target)
}

calcular_metricas_vec <- function(y_obs, y_pred, y_train = NULL) {
  ok <- !is.na(y_obs) & !is.na(y_pred)
  y_obs <- as.numeric(y_obs[ok])
  y_pred <- as.numeric(y_pred[ok])

  if (length(y_obs) == 0) {
    return(data.frame(mae = NA_real_, rmse = NA_real_, r2 = NA_real_, sse = NA_real_, sst = NA_real_, spearman = NA_real_, bias = NA_real_))
  }

  referencia <- if (!is.null(y_train) && length(stats::na.omit(as.numeric(y_train))) > 0) {
    mean(as.numeric(y_train), na.rm = TRUE)
  } else {
    mean(y_obs)
  }

  error <- y_pred - y_obs
  sst <- sum((y_obs - referencia)^2)
  sse <- sum(error^2)
  r2 <- ifelse(sst > 0, 1 - sse / sst, NA_real_)
  spearman <- if (length(y_obs) >= 3 && dplyr::n_distinct(y_obs) >= 2 && dplyr::n_distinct(y_pred) >= 2) {
    suppressWarnings(stats::cor(y_obs, y_pred, method = "spearman"))
  } else {
    NA_real_
  }

  data.frame(
    mae = mean(abs(error)), rmse = sqrt(mean(error^2)), r2 = r2, sse = sse, sst = sst,
    spearman = spearman, bias = mean(error), stringsAsFactors = FALSE
  )
}

seleccionar_predictores_supervisados <- function(df_train, target, max_pred = max_predictores_arboles) {
  cols_x <- obtener_predictores_x(df_train)
  if (length(cols_x) == 0) return(character())

  y <- convertir_numericamente(df_train[[target]])

  resumen <- lapply(cols_x, function(x) {
    vx <- convertir_numericamente(df_train[[x]])
    completos <- !is.na(vx) & !is.na(y)
    n_comunes <- sum(completos)
    n_no_na_x <- sum(!is.na(vx))
    n_dist_x <- dplyr::n_distinct(vx[!is.na(vx)])
    n_dist_y <- dplyr::n_distinct(y[completos])
    desv_x <- ifelse(n_no_na_x > 1, suppressWarnings(stats::sd(vx, na.rm = TRUE)), NA_real_)

    r <- if (n_comunes >= min_n_correlacion && n_dist_x >= 2 && n_dist_y >= 2) {
      suppressWarnings(stats::cor(vx[completos], y[completos], method = "pearson"))
    } else {
      NA_real_
    }

    data.frame(
      predictor = x, r_target = r, abs_r_target = abs(r), n_comunes = n_comunes,
      n_no_na_x = n_no_na_x, n_distintos_predictor = n_dist_x,
      desviacion_predictor = desv_x, stringsAsFactors = FALSE
    )
  }) %>% bind_rows()

  seleccion <- resumen %>%
    filter(!is.na(abs_r_target), n_distintos_predictor >= 2) %>%
    arrange(desc(abs_r_target), desc(n_comunes), desc(desviacion_predictor)) %>%
    slice_head(n = max_pred) %>%
    pull(predictor)

  unique(seleccion)
}

imputar_con_mediana_train <- function(train, test, predictores) {
  if (length(predictores) == 0) return(list(train = train, test = test))

  for (p in predictores) {
    train[[p]] <- convertir_numericamente(train[[p]])
    test[[p]] <- convertir_numericamente(test[[p]])
    med <- suppressWarnings(stats::median(train[[p]], na.rm = TRUE))
    if (is.na(med) || is.infinite(med)) med <- 0
    train[[p]][is.na(train[[p]])] <- med
    test[[p]][is.na(test[[p]])] <- med
  }

  list(train = train, test = test)
}

filtrar_predictores_con_varianza <- function(train, predictores) {
  if (length(predictores) == 0) return(character())

  predictores[vapply(predictores, function(p) {
    vx <- convertir_numericamente(train[[p]])
    dplyr::n_distinct(vx[!is.na(vx)]) >= 2 &&
      !is.na(stats::sd(vx, na.rm = TRUE)) && stats::sd(vx, na.rm = TRUE) > 0
  }, logical(1))]
}

preparar_matrices_x <- function(train, test, target, predictores) {
  predictores <- filtrar_predictores_con_varianza(train, predictores)
  if (length(predictores) == 0) return(NULL)

  imputado <- imputar_con_mediana_train(train, test, predictores)
  train_imp <- imputado$train
  test_imp <- imputado$test

  predictores <- predictores[vapply(predictores, function(p) {
    stats::sd(train_imp[[p]], na.rm = TRUE) > 0
  }, logical(1))]

  if (length(predictores) == 0) return(NULL)

  list(
    train = train_imp, test = test_imp, predictores = predictores,
    x_train = as.matrix(train_imp[, predictores, drop = FALSE]),
    x_test = as.matrix(test_imp[, predictores, drop = FALSE]),
    y_train = convertir_numericamente(train_imp[[target]]),
    y_test = convertir_numericamente(test_imp[[target]])
  )
}

crear_split_bootstrap_agrupado <- function(df) {
  grupos <- unique(df$grupo_validacion)
  n_grupos <- length(grupos)
  if (n_grupos < 4) return(NULL)

  grupos_sample <- sample(grupos, size = n_grupos, replace = TRUE)
  grupos_oob <- setdiff(grupos, unique(grupos_sample))
  metodo_split <- "bootstrap_con_reemplazo"

  if (length(grupos_oob) < 2) {
    n_train_groups <- max(2, floor(0.75 * n_grupos))
    grupos_sample <- sample(grupos, size = n_train_groups, replace = FALSE)
    grupos_oob <- setdiff(grupos, grupos_sample)
    metodo_split <- "fallback_holdout_75_25"
  }

  indices_por_grupo <- split(seq_len(nrow(df)), df$grupo_validacion)
  train_idx <- unlist(indices_por_grupo[grupos_sample], use.names = FALSE)
  test_idx <- unlist(indices_por_grupo[grupos_oob], use.names = FALSE)

  if (length(train_idx) < 6 || length(test_idx) < 2) return(NULL)

  list(
    train = df[train_idx, , drop = FALSE], test = df[test_idx, , drop = FALSE],
    metodo_split = metodo_split
  )
}

ajustar_rf_boot <- function(datos, iteracion) {
  n_train <- length(datos$y_train)
  p <- length(datos$predictores)
  if (n_train < 8 || p < 1) return(list(pred = NULL, importancia = data.frame()))

  df_train <- as.data.frame(datos$train[, datos$predictores, drop = FALSE])
  df_test <- as.data.frame(datos$test[, datos$predictores, drop = FALSE])
  target_tmp <- "y_objetivo_tmp"
  df_train[[target_tmp]] <- datos$y_train

  mtry_val <- max(1, min(p, floor(sqrt(p))))
  min_node <- max(2, floor(0.15 * n_train))

  fit <- tryCatch({
    ranger::ranger(
      formula = stats::as.formula(paste(bt(target_tmp), "~", paste(bt(datos$predictores), collapse = " + "))),
      data = df_train, num.trees = n_arboles_rf, mtry = mtry_val,
      min.node.size = min_node, max.depth = max_depth_rf,
      respect.unordered.factors = "order", seed = 123 + iteracion,
      importance = "permutation"
    )
  }, error = function(e) NULL)

  if (is.null(fit)) return(list(pred = NULL, importancia = data.frame()))

  pred <- tryCatch(stats::predict(fit, data = df_test)$predictions, error = function(e) rep(mean(datos$y_train), nrow(df_test)))

  imp <- tryCatch(ranger::importance(fit), error = function(e) NULL)
  importancia <- if (!is.null(imp)) {
    data.frame(predictor = names(imp), importancia_abs = as.numeric(imp), stringsAsFactors = FALSE) %>%
      filter(!is.na(importancia_abs), importancia_abs > 0)
  } else {
    data.frame()
  }

  list(pred = recortar_prediccion(pred), importancia = importancia)
}

ajustar_ridge_boot <- function(datos, escenario, target, iteracion) {
  n_train <- length(datos$y_train)
  p <- length(datos$predictores)
  if (n_train < 6 || p < 1) return(list(pred = NULL, importancia = data.frame()))

  nfolds_inner <- min(5, max(3, floor(n_train / 2)))

  # Misma formula de semilla que 16_validacion_bootstrap_cv.R, para que
  # Ridge sea reproducible entre scripts (ver justificacion en ese archivo).
  set.seed(1000000 + iteracion + sum(utf8ToInt(paste0(escenario, "_", target, "_ridge"))))

  fit <- tryCatch({
    glmnet::cv.glmnet(
      x = datos$x_train, y = datos$y_train, family = "gaussian",
      alpha = 0, nfolds = nfolds_inner, standardize = TRUE, type.measure = "mse"
    )
  }, error = function(e) NULL)

  if (is.null(fit)) return(list(pred = NULL, importancia = data.frame()))

  pred <- tryCatch(
    as.numeric(stats::predict(fit, newx = datos$x_test, s = "lambda.min")),
    error = function(e) rep(mean(datos$y_train), nrow(datos$x_test))
  )

  coefs <- tryCatch(as.matrix(stats::coef(fit, s = "lambda.min")), error = function(e) NULL)
  importancia <- if (!is.null(coefs)) {
    data.frame(predictor = rownames(coefs), importancia_abs = abs(as.numeric(coefs[, 1])), stringsAsFactors = FALSE) %>%
      filter(predictor != "(Intercept)", !is.na(importancia_abs), importancia_abs > 0)
  } else {
    data.frame()
  }

  list(pred = recortar_prediccion(pred), importancia = importancia)
}

ejecutar_bootstrap_modelo <- function(df_base, target, version, modelo) {
  resultados <- list()
  importancias <- list()
  k <- 1
  ki <- 1
  max_pred <- if (modelo == "random_forest_restringido") max_predictores_arboles else max_predictores_supervisados

  # Misma semilla base que 16_validacion_bootstrap_cv.R para el escenario
  # principal (M_pura, y_fenolico_comun), de modo que el split de cada
  # iteracion b sea identico al de ese script para la version "completo".
  semilla_base <- 123 + sum(utf8ToInt(paste0(escenario_id, "_", target)))

  for (b in seq_len(n_bootstrap)) {
    set.seed(semilla_base + b)
    split_b <- crear_split_bootstrap_agrupado(df_base)
    if (is.null(split_b)) next

    train_b <- split_b$train
    test_b <- split_b$test

    predictores <- seleccionar_predictores_supervisados(train_b, target, max_pred = max_pred)
    datos <- preparar_matrices_x(train_b, test_b, target, predictores)
    if (is.null(datos)) next

    ajuste <- if (modelo == "random_forest_restringido") {
      ajustar_rf_boot(datos, b)
    } else {
      ajustar_ridge_boot(datos, escenario_id, target, b)
    }
    if (is.null(ajuste$pred)) next

    met <- calcular_metricas_vec(test_b[[target]], ajuste$pred, y_train = datos$y_train)

    resultados[[k]] <- data.frame(
      modelo = modelo, version = version, target = target, iteracion = b,
      n_train = nrow(train_b), n_test = nrow(test_b),
      metodo_split = split_b$metodo_split, n_predictores = length(datos$predictores),
      met, stringsAsFactors = FALSE
    )
    k <- k + 1

    if (nrow(ajuste$importancia) > 0) {
      importancias[[ki]] <- ajuste$importancia %>%
        mutate(modelo = modelo, version = version, target = target, iteracion = b)
      ki <- ki + 1
    }
  }

  list(metricas = bind_rows(resultados), importancia = bind_rows(importancias))
}

# ------------------------------------------------------------
# 3. Datos: M_pura completo vs. M_pura sin unidades totalmente imputadas
#    en GC-FID
# ------------------------------------------------------------

matriz_pura <- readxl::read_excel(ruta_datos_modelamiento, sheet = "01_pura")

# El escenario principal de la tesis (post inversion GC-MS) se define
# UNICAMENTE sobre los 13 predictores de GC-FID (seccion imp:datosfaltantes).
# Las columnas x_ de "01_pura" incluyen ademas JU, Folin y GC-MS, que deben
# excluirse explicitamente ANTES del bootstrap; de lo contrario, el selector
# de predictores por correlacion podria elegir GC-MS en algunas iteraciones
# (tiene cobertura real de 23.5%) y el "completo" de este script dejaria de
# coincidir con el M_pura de 13 predictores usado en el resto de la tesis.
predictores_gcfid_todos <- grep("^x_gcfid_", names(matriz_pura), value = TRUE)
predictores_no_gcfid <- setdiff(obtener_predictores_x(matriz_pura), predictores_gcfid_todos)
matriz_pura <- matriz_pura %>% select(-all_of(predictores_no_gcfid))

df_completo <- matriz_pura %>%
  mutate(across(all_of(obtener_predictores_x(matriz_pura)), convertir_numericamente))
df_completo$grupo_validacion <- as.character(df_completo$identificador_quimico)

predictores_gcfid <- grep("^x_gcfid_", names(df_completo), value = TRUE)
tiene_gcfid_real <- rowSums(!is.na(df_completo[, predictores_gcfid, drop = FALSE])) > 0
df_sin_imputacion_total <- df_completo[tiene_gcfid_real, , drop = FALSE]

cat("\nSENSIBILIDAD A LA IMPUTACION COMPLETA DE GC-FID (M_pura, Random Forest restringido y Ridge)\n")
cat("Predictores GC-FID:", length(predictores_gcfid), "\n")
cat("Unidades totales en M_pura:", nrow(df_completo), "\n")
cat("Muestras independientes totales (identificador_quimico):", dplyr::n_distinct(df_completo$identificador_quimico), "\n")
cat("Unidades sin ningun dato GC-FID medido directamente:", sum(!tiene_gcfid_real), "\n")
cat("Unidades tras exclusion (con al menos un dato GC-FID real):", nrow(df_sin_imputacion_total), "\n")
cat("Muestras independientes tras exclusion:", dplyr::n_distinct(df_sin_imputacion_total$identificador_quimico), "\n")

# ------------------------------------------------------------
# 4. Bootstrap para cada version
# ------------------------------------------------------------

metricas_sensibilidad <- list()
importancia_sensibilidad <- list()
k <- 1
ki <- 1

for (version in c("completo", "sin_imputacion_total")) {
  df_version <- if (version == "completo") df_completo else df_sin_imputacion_total
  df_modelo <- df_version %>% filter(!is.na(.data[[target_a_evaluar]]))

  if (nrow(df_modelo) < min_n_target_modelable || dplyr::n_distinct(df_modelo$grupo_validacion) < 4) {
    message("Omitido (n insuficiente): ", target_a_evaluar, " - ", version, " (n=", nrow(df_modelo), ")")
    next
  }

  for (modelo in modelos_a_evaluar) {
    message("Bootstrap ", modelo, ": ", target_a_evaluar, " - ", version, " (n=", nrow(df_modelo), ", grupos=", dplyr::n_distinct(df_modelo$grupo_validacion), ")")
    res <- ejecutar_bootstrap_modelo(df_modelo, target_a_evaluar, version, modelo)
    if (nrow(res$metricas) > 0) {
      metricas_sensibilidad[[k]] <- res$metricas
      k <- k + 1
    }
    if (nrow(res$importancia) > 0) {
      importancia_sensibilidad[[ki]] <- res$importancia
      ki <- ki + 1
    }
  }
}

metricas_sensibilidad <- bind_rows(metricas_sensibilidad)
importancia_sensibilidad <- bind_rows(importancia_sensibilidad)

# ------------------------------------------------------------
# 5. Resumen comparativo completo vs. sin_imputacion_total
# ------------------------------------------------------------

resumen_sensibilidad <- data.frame()
comparacion_diferencias <- data.frame()
if (nrow(metricas_sensibilidad) > 0) {
  resumen_sensibilidad <- metricas_sensibilidad %>%
    group_by(modelo, target, version) %>%
    summarise(
      n_bootstrap_exitosos = n(),
      n_fallback_holdout = sum(metodo_split == "fallback_holdout_75_25", na.rm = TRUE),
      mae_media = mean(mae, na.rm = TRUE),
      mae_p05 = as.numeric(stats::quantile(mae, 0.05, na.rm = TRUE)),
      mae_p50 = as.numeric(stats::quantile(mae, 0.50, na.rm = TRUE)),
      mae_p95 = as.numeric(stats::quantile(mae, 0.95, na.rm = TRUE)),
      rmse_media = mean(rmse, na.rm = TRUE),
      r2_media = mean(r2, na.rm = TRUE),
      r2_agregado = 1 - sum(sse, na.rm = TRUE) / sum(sst, na.rm = TRUE),
      r2_pct_positivo = round(100 * mean(r2 > 0, na.rm = TRUE), 1),
      spearman_media = mean(spearman, na.rm = TRUE),
      bias_media = mean(bias, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(modelo, target, version)

  comparacion_diferencias <- resumen_sensibilidad %>%
    select(modelo, target, version, mae_media, r2_agregado) %>%
    pivot_wider(names_from = version, values_from = c(mae_media, r2_agregado)) %>%
    mutate(
      diferencia_mae = mae_media_sin_imputacion_total - mae_media_completo,
      diferencia_mae_pct = round(100 * diferencia_mae / mae_media_completo, 1),
      diferencia_r2_agregado = r2_agregado_sin_imputacion_total - r2_agregado_completo,
      lectura = case_when(
        is.na(r2_agregado_sin_imputacion_total) ~ "no_comparable",
        r2_agregado_sin_imputacion_total > 0 ~ "senal_sobrevive: R2 agregado se mantiene positivo al excluir las unidades totalmente imputadas",
        TRUE ~ "senal_no_sobrevive: R2 agregado deja de ser positivo al excluir las unidades totalmente imputadas"
      )
    )
}

# ------------------------------------------------------------
# 6. Variables prioritarias por version (solo Random Forest)
# ------------------------------------------------------------

variables_por_version <- data.frame()
if (nrow(importancia_sensibilidad) > 0) {
  n_iter_por_version <- metricas_sensibilidad %>%
    filter(modelo == "random_forest_restringido") %>%
    group_by(version) %>%
    summarise(n_iteraciones = dplyr::n_distinct(iteracion), .groups = "drop")

  variables_por_version <- importancia_sensibilidad %>%
    filter(modelo == "random_forest_restringido") %>%
    group_by(version, predictor) %>%
    summarise(
      n_apariciones = n(),
      importancia_abs_media = mean(importancia_abs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(n_iter_por_version, by = "version") %>%
    mutate(frecuencia_seleccion = round(n_apariciones / n_iteraciones, 3)) %>%
    group_by(version) %>%
    arrange(desc(importancia_abs_media), .by_group = TRUE) %>%
    mutate(ranking = row_number()) %>%
    ungroup() %>%
    filter(ranking <= 10) %>%
    arrange(version, ranking)
}

# ------------------------------------------------------------
# 7. Grafico comparativo
# ------------------------------------------------------------

ruta_grafico <- NA_character_
if (nrow(metricas_sensibilidad) > 0) {
  p_comparacion <- ggplot(metricas_sensibilidad, aes(x = version, y = mae, fill = version)) +
    geom_boxplot(alpha = 0.6, outlier.alpha = 0.4) +
    facet_wrap(~modelo) +
    labs(
      title = "Sensibilidad del MAE bootstrap a la imputacion completa de GC-FID",
      subtitle = "Random Forest restringido y Ridge, M_pura, y_fenolico_comun, 100 iteraciones de bootstrap agrupado",
      x = NULL, y = "MAE (test bootstrap)", fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  ruta_grafico <- file.path(dir_outputs_sensibilidad, "01_comparacion_mae_completo_vs_sin_imputacion_total.png")
  ggsave(ruta_grafico, p_comparacion, width = 8, height = 6, dpi = 300)
}

# ------------------------------------------------------------
# 8. Exportacion
# ------------------------------------------------------------

resumen_general <- data.frame(
  indicador = c(
    "escenario", "target", "modelos_evaluados", "n_bootstrap_por_version",
    "n_unidades_completo", "n_muestras_independientes_completo",
    "n_unidades_sin_gcfid_real", "n_unidades_sin_imputacion_total",
    "n_muestras_independientes_sin_imputacion_total", "archivo_salida"
  ),
  valor = c(
    escenario_id, target_a_evaluar, paste(modelos_a_evaluar, collapse = "; "), as.character(n_bootstrap),
    as.character(nrow(df_completo)), as.character(dplyr::n_distinct(df_completo$identificador_quimico)),
    as.character(sum(!tiene_gcfid_real)),
    as.character(nrow(df_sin_imputacion_total)),
    as.character(dplyr::n_distinct(df_sin_imputacion_total$identificador_quimico)),
    ruta_salida_excel
  ),
  stringsAsFactors = FALSE
)

notas <- data.frame(
  punto = c(
    "Que compara", "Que es 'completo'", "Que es 'sin_imputacion_total'",
    "Metodologia", "Interpretacion", "Limite"
  ),
  descripcion = c(
    "Random Forest restringido y Ridge, con el mismo procedimiento de validacion (bootstrap agrupado por identificador_quimico, 100 iteraciones), cada uno ajustado dos veces sobre M_pura y_fenolico_comun: con las 34 unidades (26 con dato GC-FID real + 8 totalmente imputadas) y excluyendo las 8 totalmente imputadas.",
    paste0("M_pura completo: ", nrow(df_completo), " unidades analiticas, ", dplyr::n_distinct(df_completo$identificador_quimico), " muestras independientes."),
    paste0("M_pura excluyendo las unidades sin ningun dato GC-FID medido directamente: ", nrow(df_sin_imputacion_total), " unidades analiticas, ", dplyr::n_distinct(df_sin_imputacion_total$identificador_quimico), " muestras independientes (dos registros de Dalwhinnie, dos de Caol Ila y las cuatro del panel de noviembre quedan excluidos)."),
    "Hiperparametros y logica de bootstrap identicos a 16_validacion_bootstrap_cv.R para que la comparacion sea metodologicamente equivalente. R2 calculado de forma agregada (SSE y SST sumados sobre todas las predicciones fuera de bolsa, referencia = media del pliegue de entrenamiento de cada iteracion), no promediando el R2 de cada iteracion.",
    "Si el R2 agregado se mantiene positivo (o mejora) al excluir las unidades totalmente imputadas, la senal detectada no depende de las filas sin dato GC-FID real, y es evidencia mas fuerte de que el desempeno del modelo principal proviene de informacion quimica medida y no de valores completados por mediana.",
    "Con solo 6 muestras independientes tras la exclusion, la incertidumbre de esta version es mayor que la del escenario completo (10 muestras). No se elimina ninguna unidad de los archivos de datos ni de otros scripts, la exclusion es interna y exclusiva de este analisis de sensibilidad. Esta version conserva 3 de las 5 unidades de Dalwhinnie y 3 de las 5 de Caol Ila, por lo que no equivale al analisis de exclusion completa de muestras comerciales de 16d_sensibilidad_muestras_comerciales.R."
  ),
  stringsAsFactors = FALSE
)

lista_hojas <- list(
  "00_resumen" = resumen_general,
  "01_metricas_bootstrap" = metricas_sensibilidad,
  "02_resumen_comparativo" = resumen_sensibilidad,
  "03_comparacion_diferencias" = comparacion_diferencias,
  "04_variables_prioritarias" = variables_por_version,
  "05_notas" = notas
)
lista_hojas <- lista_hojas[vapply(lista_hojas, function(x) is.data.frame(x) && nrow(x) > 0, logical(1))]

guardar_excel_seguro(lista_hojas, ruta_salida_excel)

cat("\nProceso terminado correctamente.\n")
cat("Archivo generado:\n", ruta_salida_excel, "\n")
if (!is.na(ruta_grafico)) cat("Grafico generado:\n", ruta_grafico, "\n")
cat("\nResumen comparativo:\n")
print(resumen_sensibilidad)
cat("\nDiferencias completo vs. sin_imputacion_total:\n")
if (nrow(comparacion_diferencias) > 0) {
  print(comparacion_diferencias %>% select(modelo, target, diferencia_mae, diferencia_mae_pct, diferencia_r2_agregado, lectura))
}
cat("\nVariables prioritarias (Random Forest) por version:\n")
print(variables_por_version)
