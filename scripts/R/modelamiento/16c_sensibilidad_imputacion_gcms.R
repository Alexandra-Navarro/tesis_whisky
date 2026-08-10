# ============================================================
# 16c_sensibilidad_imputacion_gcms.R
# Sensibilidad de los modelos principales (M_pura: Ridge y Random Forest
# restringido) a la imputacion por mediana de las variables GC-MS, que
# tienen cobertura real muy baja dentro de M_pura (8/34 unidades, 23.5%).
# Tesis whisky chileno - modelamiento quimico-sensorial
# ============================================================

# Pregunta que responde este script: la estrategia de datos faltantes ya
# usada en el pipeline (14_modelo_base.R, 15_modelos_small_data.R,
# 16_validacion_bootstrap_cv.R) imputa cada predictor con la mediana del
# pliegue de entrenamiento correspondiente, calculada unicamente sobre las
# filas con dato real de ese pliegue, y aplicada por igual a train y test
# (sin fuga de informacion: la mediana nunca se calcula usando el pliegue
# de prueba). Para las variables GC-MS de M_pura, sin embargo, solo 8 de
# las 34 unidades tienen dato real (23.5% de cobertura); las 26 restantes
# quedan mayormente imputadas. Si esas variables igual aparecen en el
# consenso de importancia del escenario principal, cabe preguntarse si el
# resultado depende fuertemente de datos imputados en vez de medidos.
#
# Este script no cambia la estrategia de imputacion (se mantiene la misma
# en todo el pipeline principal); solo evalua, como analisis de
# sensibilidad, si excluir por completo las variables GC-MS de M_pura
# cambia sustancialmente el desempeno de los modelos principales. Si no
# cambia, es evidencia de que la conclusion principal no depende
# criticamente de datos GC-MS imputados. Si cambia, es un limite
# metodologico que vale la pena declarar explicitamente en la tesis.
#
# Mismo procedimiento de bootstrap agrupado que 16_validacion_bootstrap_cv.R
# (100 iteraciones, remuestreo con reemplazo por identificador_quimico, mismos
# hiperparametros de ranger/glmnet), aplicado en paralelo a dos versiones de
# M_pura: "completo" (32 predictores candidatos, incluye GC-MS con
# imputacion por mediana) y "sin_gcms" (excluyendo por completo los
# predictores x_gcms_*, sin necesidad de imputarlos). No se modifica ni se
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
dir_outputs_sensibilidad <- file.path(dir_outputs_modelamiento, "sensibilidad_imputacion_gcms")

dir.create(dir_modelamiento, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_outputs_sensibilidad, recursive = TRUE, showWarnings = FALSE)

ruta_datos_modelamiento <- file.path(dir_modelamiento, "datos_modelamiento.xlsx")
ruta_salida_excel <- file.path(dir_modelamiento, "sensibilidad_imputacion_gcms.xlsx")

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

targets_a_evaluar <- c("y_fenolico_comun", "y_frutal_comun")
modelos_a_evaluar <- c("ridge", "random_forest_restringido")
escenario_id <- "M_pura"

# ------------------------------------------------------------
# 2. Funciones auxiliares (replicadas de 16_validacion_bootstrap_cv.R /
#    16b_sensibilidad_outliers_quimicos.R para mantener exactamente la
#    misma metodologia de bootstrap, seleccion de predictores e imputacion)
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

  # Referencia de R2 = media del pliegue de entrenamiento de esa iteracion,
  # no del propio pliegue de prueba (ver 16_validacion_bootstrap_cv.R).
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
  if (n_train < 8 || p < 1) return(NULL)

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
      respect.unordered.factors = "order", seed = 123 + iteracion
    )
  }, error = function(e) NULL)

  if (is.null(fit)) return(NULL)

  pred <- tryCatch(stats::predict(fit, data = df_test)$predictions, error = function(e) rep(mean(datos$y_train), nrow(df_test)))
  recortar_prediccion(pred)
}

ajustar_ridge_boot <- function(datos, escenario, target, iteracion) {
  n_train <- length(datos$y_train)
  p <- length(datos$predictores)
  if (n_train < 6 || p < 1) return(NULL)

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

  if (is.null(fit)) return(NULL)

  pred <- tryCatch(
    as.numeric(stats::predict(fit, newx = datos$x_test, s = "lambda.min")),
    error = function(e) rep(mean(datos$y_train), nrow(datos$x_test))
  )
  recortar_prediccion(pred)
}

ejecutar_bootstrap_modelo <- function(df_base, target, version, modelo) {
  resultados <- list()
  k <- 1
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

    pred <- if (modelo == "random_forest_restringido") {
      ajustar_rf_boot(datos, b)
    } else {
      ajustar_ridge_boot(datos, escenario_id, target, b)
    }
    if (is.null(pred)) next

    met <- calcular_metricas_vec(test_b[[target]], pred, y_train = datos$y_train)

    resultados[[k]] <- data.frame(
      modelo = modelo, version = version, target = target, iteracion = b,
      n_train = nrow(train_b), n_test = nrow(test_b),
      metodo_split = split_b$metodo_split, n_predictores = length(datos$predictores),
      met, stringsAsFactors = FALSE
    )
    k <- k + 1
  }

  bind_rows(resultados)
}

# ------------------------------------------------------------
# 3. Datos: M_pura completo (con GC-MS imputado) vs. sin predictores GC-MS
# ------------------------------------------------------------

matriz_pura <- readxl::read_excel(ruta_datos_modelamiento, sheet = "01_pura")

# De las 5 variables GC-MS candidatas, 2 no son propiedades quimicas del
# destilado y se excluyen de raiz, en todas las versiones de este script
# (no solo de "sin_gcms"), porque no tienen una lectura quimica valida como
# predictor de caracter fenolico:
# - x_gcms_area_valida_pct = 100 - ctrl_gcms_siloxano_pct (02_extraer_quimica.R):
#   es un indicador de calidad del cromatograma (limpieza frente a
#   contaminacion por siloxano), no una propiedad del whisky.
# - x_gcms_aroma_fermentativo_pct = x_gcms_esteres_pct + x_gcms_aldehidos_pct
#   (02b_clasificar_compuestos_gcms.R): es la suma exacta de otras dos
#   variables ya incluidas como predictores (identidad matematica, no solo
#   correlacion alta), por lo que incluirla junto a esteres y aldehidos es
#   dependencia lineal perfecta por construccion, no aporta informacion
#   nueva y explica el |r|=0.9982 maximo reportado para el bloque quimico
#   (Tabla 4.1). Quedan como predictores GC-MS legitimos: esteres,
#   fenolicos y aldehidos.
matriz_pura <- matriz_pura %>%
  select(-x_gcms_area_valida_pct, -x_gcms_aroma_fermentativo_pct)

df_completo <- matriz_pura %>%
  mutate(across(all_of(obtener_predictores_x(matriz_pura)), convertir_numericamente))
df_completo$grupo_validacion <- as.character(df_completo$identificador_quimico)

predictores_gcms <- grep("^x_gcms_", names(df_completo), value = TRUE)
df_sin_gcms <- df_completo %>% select(-all_of(predictores_gcms))

# Cobertura real (no imputada) de las variables GC-MS dentro de M_pura.
cobertura_gcms <- df_completo %>%
  summarise(across(all_of(predictores_gcms), ~ sum(!is.na(.x)))) %>%
  tidyr::pivot_longer(everything(), names_to = "predictor", values_to = "n_con_dato_real") %>%
  mutate(n_total = nrow(df_completo), pct_cobertura = round(100 * n_con_dato_real / n_total, 1))

cat("\nSENSIBILIDAD A LA IMPUTACION DE VARIABLES GC-MS (M_pura, Ridge y Random Forest restringido)\n")
cat("Predictores GC-MS en M_pura:", length(predictores_gcms), "de", length(obtener_predictores_x(df_completo)), "predictores totales\n")
print(cobertura_gcms)

# ------------------------------------------------------------
# 4. Bootstrap para cada target x version
# ------------------------------------------------------------

metricas_sensibilidad <- list()
k <- 1

for (target in targets_a_evaluar) {
  if (!(target %in% names(df_completo))) next

  for (version in c("completo", "sin_gcms")) {
    df_version <- if (version == "completo") df_completo else df_sin_gcms
    df_modelo <- df_version %>% filter(!is.na(.data[[target]]))

    if (nrow(df_modelo) < min_n_target_modelable || dplyr::n_distinct(df_modelo$grupo_validacion) < 4) {
      message("Omitido (n insuficiente): ", target, " - ", version, " (n=", nrow(df_modelo), ")")
      next
    }

    for (modelo in modelos_a_evaluar) {
      message("Bootstrap ", modelo, ": ", target, " - ", version, " (n=", nrow(df_modelo), ")")
      res <- ejecutar_bootstrap_modelo(df_modelo, target, version, modelo)
      if (nrow(res) > 0) {
        metricas_sensibilidad[[k]] <- res
        k <- k + 1
      }
    }
  }
}

metricas_sensibilidad <- bind_rows(metricas_sensibilidad)

# ------------------------------------------------------------
# 5. Resumen comparativo completo vs. sin_gcms
# ------------------------------------------------------------

resumen_sensibilidad <- data.frame()
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
      spearman_media = mean(spearman, na.rm = TRUE),
      bias_media = mean(bias, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(modelo, target, version)

  comparacion_diferencias <- resumen_sensibilidad %>%
    select(modelo, target, version, mae_media, rmse_media, r2_media, spearman_media) %>%
    pivot_wider(names_from = version, values_from = c(mae_media, rmse_media, r2_media, spearman_media)) %>%
    mutate(
      diferencia_mae = mae_media_sin_gcms - mae_media_completo,
      diferencia_mae_pct = round(100 * diferencia_mae / mae_media_completo, 1),
      diferencia_r2 = r2_media_sin_gcms - r2_media_completo,
      lectura = case_when(
        is.na(diferencia_mae_pct) ~ "no_comparable",
        abs(diferencia_mae_pct) <= 10 ~ "robusto: el MAE cambia menos de 10% al quitar las variables GC-MS",
        abs(diferencia_mae_pct) <= 25 ~ "sensibilidad moderada: revisar si GC-MS aporta senal real o solo variabilidad de la imputacion",
        TRUE ~ "sensibilidad alta: el resultado depende fuertemente de las variables GC-MS imputadas"
      )
    )
} else {
  comparacion_diferencias <- data.frame()
}

# ------------------------------------------------------------
# 6. Grafico comparativo
# ------------------------------------------------------------

ruta_grafico <- NA_character_
if (nrow(metricas_sensibilidad) > 0) {
  p_comparacion <- ggplot(metricas_sensibilidad, aes(x = version, y = mae, fill = version)) +
    geom_boxplot(alpha = 0.6, outlier.alpha = 0.4) +
    facet_grid(modelo ~ target) +
    labs(
      title = "Sensibilidad del MAE bootstrap a las variables GC-MS imputadas",
      subtitle = "Ridge y Random Forest restringido, M_pura, 100 iteraciones de bootstrap agrupado",
      x = NULL, y = "MAE (test bootstrap)", fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  ruta_grafico <- file.path(dir_outputs_sensibilidad, "01_comparacion_mae_completo_vs_sin_gcms.png")
  ggsave(ruta_grafico, p_comparacion, width = 9, height = 8, dpi = 300)
}

# ------------------------------------------------------------
# 7. Exportacion
# ------------------------------------------------------------

resumen_general <- data.frame(
  indicador = c(
    "escenario", "modelo_evaluado", "n_bootstrap_por_version",
    "n_unidades", "n_predictores_gcms_excluidos", "n_predictores_totales_completo",
    "cobertura_real_gcms_min_pct", "cobertura_real_gcms_max_pct",
    "targets_evaluados", "estrategia_imputacion_pipeline_principal", "archivo_salida"
  ),
  valor = c(
    escenario_id, paste(modelos_a_evaluar, collapse = "; "), as.character(n_bootstrap),
    as.character(nrow(df_completo)), as.character(length(predictores_gcms)),
    as.character(length(obtener_predictores_x(df_completo))),
    as.character(min(cobertura_gcms$pct_cobertura)), as.character(max(cobertura_gcms$pct_cobertura)),
    paste(targets_a_evaluar, collapse = "; "),
    "Mediana calculada solo con el pliegue de entrenamiento de cada iteracion de bootstrap/CV, aplicada por igual a train y test (sin fuga de informacion).",
    ruta_salida_excel
  ),
  stringsAsFactors = FALSE
)

notas <- data.frame(
  punto = c(
    "Que compara", "Que es 'completo'", "Que es 'sin_gcms'",
    "Estrategia de datos faltantes en el pipeline principal",
    "Metodologia de este script", "Interpretacion", "Limite"
  ),
  descripcion = c(
    "Los mismos modelos (Ridge y Random Forest restringido) y el mismo procedimiento de validacion (bootstrap agrupado por identificador_quimico, 100 iteraciones), cada uno ajustado dos veces sobre M_pura: con todos los predictores candidatos (incluyendo GC-MS, imputados por mediana) y excluyendo por completo los predictores GC-MS.",
    paste0("M_pura completo: ", nrow(df_completo), " unidades, ", length(obtener_predictores_x(df_completo)), " predictores candidatos, incluyendo ", length(predictores_gcms), " variables GC-MS con cobertura real de entre ", min(cobertura_gcms$pct_cobertura), "% y ", max(cobertura_gcms$pct_cobertura), "% (el resto se completa por imputacion)."),
    paste0("M_pura sin los ", length(predictores_gcms), " predictores GC-MS: ", length(obtener_predictores_x(df_sin_gcms)), " predictores candidatos, todos con cobertura real igual o superior a la usada en el resto del pipeline."),
    "En 14_modelo_base.R, 15_modelos_small_data.R y 16_validacion_bootstrap_cv.R, cada predictor numerico se imputa con la mediana calculada unicamente sobre el pliegue de entrenamiento de esa iteracion (nunca sobre el pliegue de prueba), y esa misma mediana se aplica tanto a train como a test. Es la misma logica de imputacion usada en este script de sensibilidad.",
    "Hiperparametros y logica de bootstrap identicos a 16_validacion_bootstrap_cv.R para que la comparacion sea metodologicamente equivalente.",
    "Si el MAE medio cambia poco entre versiones, los resultados principales de la tesis no dependen criticamente de las variables GC-MS (mayormente imputadas). Si cambia mucho, es evidencia de que esas variables tienen peso real y su bajo nivel de cobertura debe declararse como limite explicito de la interpretacion.",
    "No se elimina ninguna variable de los archivos de datos ni de otros scripts; la exclusion es interna y exclusiva de este analisis de sensibilidad."
  ),
  stringsAsFactors = FALSE
)

lista_hojas <- list(
  "00_resumen" = resumen_general,
  "01_cobertura_gcms" = cobertura_gcms,
  "02_metricas_bootstrap" = metricas_sensibilidad,
  "03_resumen_comparativo" = resumen_sensibilidad,
  "04_comparacion_diferencias" = comparacion_diferencias,
  "05_notas" = notas
)
lista_hojas <- lista_hojas[vapply(lista_hojas, function(x) is.data.frame(x) && nrow(x) > 0, logical(1))]

guardar_excel_seguro(lista_hojas, ruta_salida_excel)

cat("\nProceso terminado correctamente.\n")
cat("Archivo generado:\n", ruta_salida_excel, "\n")
if (!is.na(ruta_grafico)) cat("Grafico generado:\n", ruta_grafico, "\n")
cat("\nResumen comparativo:\n")
print(resumen_sensibilidad)
cat("\nDiferencias completo vs. sin_gcms:\n")
print(comparacion_diferencias %>% select(modelo, target, diferencia_mae, diferencia_mae_pct, diferencia_r2, lectura))
