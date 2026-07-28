# ============================================================
# 16_validacion_bootstrap_cv.R
# Validacion bootstrap y estabilidad de modelos small data
# Tesis whisky chileno - modelamiento quimico-sensorial
# ============================================================

# Este script valida los modelos small data mediante bootstrap agrupado,
# respetando la estructura de replicas por muestra/unidad quimica.
#
# Objetivos:
# - Estimar distribuciones bootstrap de MAE, RMSE, R2, Spearman y bias.
# - Evaluar estabilidad de modelos frente a remuestreo.
# - Evaluar estabilidad de predictores importantes.
# - Comparar resultados bootstrap con la validacion cruzada previa.
#
# Importante:
# - La matriz expandida se valida agrupando por unidad analitica.
# - La matriz IA se mantiene como escenario exploratorio.
# - Random Forest y Gradient Boosting son opcionales: si no existen paquetes,
#   se omiten sin detener el flujo.

# ------------------------------------------------------------
# 1. Configuracion
# ------------------------------------------------------------

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

paquetes_requeridos <- c(
  "readxl", "dplyr", "tidyr", "stringr", "writexl", "ggplot2",
  "glmnet", "pls"
)

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
library(glmnet)
library(pls)

paquete_ranger_disponible <- requireNamespace("ranger", quietly = TRUE)
paquete_gbm_disponible <- requireNamespace("gbm", quietly = TRUE)

if (!paquete_ranger_disponible) {
  message("Paquete opcional 'ranger' no disponible: se omitira Random Forest restringido.")
}
if (!paquete_gbm_disponible) {
  message("Paquete opcional 'gbm' no disponible: se omitira Gradient Boosting restringido.")
}

set.seed(123)

# Rutas

dir_modelamiento <- file.path(dir_data, "modelamiento")
dir_outputs <- file.path(dir_proyecto, "outputs")
dir_outputs_modelamiento <- file.path(dir_outputs, "modelamiento")
dir_outputs_bootstrap <- file.path(dir_outputs_modelamiento, "validacion_bootstrap")

dir.create(dir_modelamiento, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_outputs_bootstrap, recursive = TRUE, showWarnings = FALSE)

ruta_datos_modelamiento <- file.path(dir_modelamiento, "datos_modelamiento.xlsx")
ruta_resultados_small_data <- file.path(dir_modelamiento, "resultados_modelos_small_data.xlsx")
ruta_salida_excel <- file.path(dir_modelamiento, "validacion_bootstrap_cv.xlsx")

if (!file.exists(ruta_datos_modelamiento)) {
  stop(
    "No existe el archivo requerido: ", ruta_datos_modelamiento,
    "\nEjecuta primero scripts/R/modelamiento/12_preparar_datos_modelamiento.R"
  )
}

# Parametros de validacion.
# Para una corrida preliminar rapida puede bajarse a 50.
n_bootstrap <- 100
min_n_target_modelable <- 8
min_n_correlacion <- 5
max_predictores_supervisados <- 15
max_predictores_arboles <- 10
limite_inferior_target <- 0
limite_superior_target <- 5

# Hiperparametros restringidos.
max_componentes_pls <- 5
n_arboles_rf <- 300
max_depth_rf <- 3
n_arboles_gbm <- 100
interaction_depth_gbm <- 1
shrinkage_gbm <- 0.05

# Targets que se validaran si tienen datos suficientes.
targets_a_validar <- c("y_fenolico_comun", "y_frutal_comun")

# Modelos candidatos.
modelos_a_validar <- c("ridge", "lasso", "elastic_net", "pls")
if (paquete_ranger_disponible) modelos_a_validar <- c(modelos_a_validar, "random_forest_restringido")
if (paquete_gbm_disponible) modelos_a_validar <- c(modelos_a_validar, "gradient_boosting_restringido")

# ------------------------------------------------------------
# 2. Funciones auxiliares generales
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

obtener_predictores_x <- function(df) {
  grep("^x_", names(df), value = TRUE)
}

recortar_prediccion <- function(x) {
  pmin(pmax(as.numeric(x), limite_inferior_target), limite_superior_target)
}

obtener_id_validacion <- function(df, escenario) {
  if (escenario == "M_expandida_evaluador") {
    if ("unidad_analitica_id" %in% names(df)) return(as.character(df$unidad_analitica_id))
    if ("muestra_base" %in% names(df)) return(as.character(df$muestra_base))
  }

  if ("unidad_analitica_id" %in% names(df)) return(as.character(df$unidad_analitica_id))
  if ("muestra_base" %in% names(df)) return(as.character(df$muestra_base))
  as.character(seq_len(nrow(df)))
}

preparar_df_modelo <- function(df, escenario, target) {
  cols_x <- obtener_predictores_x(df)
  if (!(target %in% names(df))) return(data.frame())

  df2 <- df %>%
    mutate(across(all_of(c(cols_x, target)), convertir_numericamente)) %>%
    filter(!is.na(.data[[target]]))

  if (nrow(df2) == 0) return(data.frame())

  df2$grupo_validacion <- obtener_id_validacion(df2, escenario)
  df2
}

resumir_target_escenario <- function(df, escenario, target) {
  if (!(target %in% names(df))) return(data.frame())

  valores <- convertir_numericamente(df[[target]])
  valores_validos <- valores[!is.na(valores)]

  data.frame(
    escenario = escenario,
    target = target,
    n_filas = nrow(df),
    n_no_na = length(valores_validos),
    n_valores_distintos = dplyr::n_distinct(valores_validos),
    desviacion = ifelse(length(valores_validos) > 1, stats::sd(valores_validos), NA_real_),
    ejecutar_bootstrap = length(valores_validos) >= min_n_target_modelable &&
      dplyr::n_distinct(valores_validos) >= 2 &&
      !is.na(stats::sd(valores_validos)) && stats::sd(valores_validos) > 0,
    prioridad = case_when(
      target == "y_fenolico_comun" ~ "principal",
      target == "y_frutal_comun" ~ "secundario_exploratorio",
      TRUE ~ "exploratorio"
    ),
    stringsAsFactors = FALSE
  )
}

calcular_metricas_vec <- function(y_obs, y_pred) {
  ok <- !is.na(y_obs) & !is.na(y_pred)
  y_obs <- as.numeric(y_obs[ok])
  y_pred <- as.numeric(y_pred[ok])

  if (length(y_obs) == 0) {
    return(data.frame(mae = NA_real_, rmse = NA_real_, r2 = NA_real_, spearman = NA_real_, bias = NA_real_))
  }

  error <- y_pred - y_obs
  sst <- sum((y_obs - mean(y_obs))^2)
  sse <- sum(error^2)
  r2 <- ifelse(sst > 0, 1 - sse / sst, NA_real_)
  spearman <- if (length(y_obs) >= 3 && dplyr::n_distinct(y_obs) >= 2 && dplyr::n_distinct(y_pred) >= 2) {
    suppressWarnings(stats::cor(y_obs, y_pred, method = "spearman"))
  } else {
    NA_real_
  }

  data.frame(
    mae = mean(abs(error)),
    rmse = sqrt(mean(error^2)),
    r2 = r2,
    spearman = spearman,
    bias = mean(error),
    stringsAsFactors = FALSE
  )
}

seleccionar_predictores_supervisados <- function(df_train, target, max_pred = max_predictores_supervisados) {
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
      predictor = x,
      r_target = r,
      abs_r_target = abs(r),
      n_comunes = n_comunes,
      n_no_na_x = n_no_na_x,
      n_distintos_predictor = n_dist_x,
      desviacion_predictor = desv_x,
      stringsAsFactors = FALSE
    )
  }) %>% bind_rows()

  seleccion <- resumen %>%
    filter(!is.na(abs_r_target), n_distintos_predictor >= 2) %>%
    arrange(desc(abs_r_target), desc(n_comunes), desc(desviacion_predictor)) %>%
    slice_head(n = max_pred) %>%
    pull(predictor)

  if (length(seleccion) == 0) {
    seleccion <- resumen %>%
      filter(n_no_na_x >= min_n_correlacion, n_distintos_predictor >= 2, !is.na(desviacion_predictor), desviacion_predictor > 0) %>%
      arrange(desc(n_no_na_x), desc(desviacion_predictor)) %>%
      slice_head(n = max_pred) %>%
      pull(predictor)
  }

  unique(seleccion)
}

imputar_con_mediana_train <- function(train, test, predictores) {
  if (length(predictores) == 0) return(list(train = train, test = test, medianas = data.frame()))

  medianas <- data.frame(predictor = predictores, mediana_train = NA_real_, stringsAsFactors = FALSE)

  for (p in predictores) {
    train[[p]] <- convertir_numericamente(train[[p]])
    test[[p]] <- convertir_numericamente(test[[p]])
    med <- suppressWarnings(stats::median(train[[p]], na.rm = TRUE))
    if (is.na(med) || is.infinite(med)) med <- 0
    train[[p]][is.na(train[[p]])] <- med
    test[[p]][is.na(test[[p]])] <- med
    medianas$mediana_train[medianas$predictor == p] <- med
  }

  list(train = train, test = test, medianas = medianas)
}

filtrar_predictores_con_varianza <- function(train, predictores) {
  if (length(predictores) == 0) return(character())

  predictores[vapply(predictores, function(p) {
    vx <- convertir_numericamente(train[[p]])
    dplyr::n_distinct(vx[!is.na(vx)]) >= 2 &&
      !is.na(stats::sd(vx, na.rm = TRUE)) &&
      stats::sd(vx, na.rm = TRUE) > 0
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
    train = train_imp,
    test = test_imp,
    predictores = predictores,
    x_train = as.matrix(train_imp[, predictores, drop = FALSE]),
    x_test = as.matrix(test_imp[, predictores, drop = FALSE]),
    y_train = convertir_numericamente(train_imp[[target]]),
    y_test = convertir_numericamente(test_imp[[target]]),
    medianas = imputado$medianas
  )
}

crear_split_bootstrap_agrupado <- function(df) {
  grupos <- unique(df$grupo_validacion)
  n_grupos <- length(grupos)

  if (n_grupos < 4) return(NULL)

  grupos_sample <- sample(grupos, size = n_grupos, replace = TRUE)
  grupos_oob <- setdiff(grupos, unique(grupos_sample))
  metodo_split <- "bootstrap_con_reemplazo"

  # Si por azar quedan muy pocos OOB, usar fallback 75/25 por grupos (sin
  # reemplazo). Se deja registrado en metodo_split para que el reporte final
  # pueda distinguir cuantas de las n_bootstrap iteraciones fueron bootstrap
  # real vs. este fallback determinista, en vez de mezclarlas sin trazabilidad.
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
    train = df[train_idx, , drop = FALSE],
    test = df[test_idx, , drop = FALSE],
    n_grupos_train = dplyr::n_distinct(df$grupo_validacion[train_idx]),
    n_grupos_test = dplyr::n_distinct(df$grupo_validacion[test_idx]),
    metodo_split = metodo_split
  )
}

# ------------------------------------------------------------
# 3. Ajuste de modelos en un bootstrap
# ------------------------------------------------------------

ajustar_glmnet_boot <- function(datos, alpha, nombre_modelo, escenario, target, iteracion, test) {
  n_train <- length(datos$y_train)
  p <- ncol(datos$x_train)
  if (n_train < 6 || p < 1) return(NULL)

  nfolds_inner <- min(5, max(3, floor(n_train / 2)))

  fit <- tryCatch({
    glmnet::cv.glmnet(
      x = datos$x_train,
      y = datos$y_train,
      family = "gaussian",
      alpha = alpha,
      nfolds = nfolds_inner,
      standardize = TRUE,
      type.measure = "mse"
    )
  }, error = function(e) NULL)

  if (is.null(fit)) return(NULL)

  pred <- tryCatch({
    as.numeric(stats::predict(fit, newx = datos$x_test, s = "lambda.min"))
  }, error = function(e) rep(mean(datos$y_train), nrow(datos$x_test)))

  coef_mat <- tryCatch(as.matrix(stats::coef(fit, s = "lambda.min")), error = function(e) NULL)

  importancia <- data.frame()
  if (!is.null(coef_mat)) {
    importancia <- data.frame(
      escenario = escenario,
      target = target,
      modelo = nombre_modelo,
      iteracion = iteracion,
      predictor = rownames(coef_mat),
      importancia = as.numeric(coef_mat[, 1]),
      importancia_abs = abs(as.numeric(coef_mat[, 1])),
      tipo_importancia = "coeficiente_lambda_min",
      stringsAsFactors = FALSE
    ) %>%
      filter(predictor != "(Intercept)", !is.na(importancia_abs), importancia_abs > 0)
  }

  list(
    pred = recortar_prediccion(pred),
    importancia = importancia,
    detalle = paste0("alpha=", alpha, "; lambda_min=", round(fit$lambda.min, 8), "; nfolds_inner=", nfolds_inner)
  )
}

ajustar_pls_boot <- function(datos, escenario, target, iteracion, test) {
  n_train <- length(datos$y_train)
  p <- ncol(datos$x_train)
  ncomp_max <- min(max_componentes_pls, p, max(1, n_train - 2))

  if (n_train < 6 || p < 1 || ncomp_max < 1) return(NULL)

  df_train <- as.data.frame(datos$x_train)
  df_test <- as.data.frame(datos$x_test)
  df_train[[target]] <- datos$y_train

  formula_pls <- stats::as.formula(paste(bt(target), "~", paste(bt(datos$predictores), collapse = " + ")))

  fit <- tryCatch({
    pls::plsr(formula_pls, data = df_train, ncomp = ncomp_max, validation = "LOO", scale = TRUE)
  }, error = function(e) NULL)

  if (is.null(fit)) return(NULL)

  rmsep <- tryCatch(as.numeric(pls::RMSEP(fit, estimate = "CV")$val), error = function(e) rep(NA_real_, ncomp_max + 1))
  if (length(rmsep) >= 2 && any(!is.na(rmsep[-1]))) {
    ncomp_sel <- which.min(rmsep[-1])
  } else {
    ncomp_sel <- 1
  }
  ncomp_sel <- min(max(1, ncomp_sel), ncomp_max)

  pred <- tryCatch({
    as.numeric(stats::predict(fit, newdata = df_test, ncomp = ncomp_sel))
  }, error = function(e) rep(mean(datos$y_train), nrow(df_test)))

  coef_pls <- tryCatch(pls::coef(fit, ncomp = ncomp_sel, intercept = FALSE), error = function(e) NULL)

  importancia <- data.frame()
  if (!is.null(coef_pls)) {
    coefs <- as.numeric(coef_pls)
    pred_names <- dimnames(coef_pls)[[1]]
    if (is.null(pred_names)) pred_names <- datos$predictores

    importancia <- data.frame(
      escenario = escenario,
      target = target,
      modelo = "pls",
      iteracion = iteracion,
      predictor = pred_names,
      importancia = coefs,
      importancia_abs = abs(coefs),
      tipo_importancia = "coeficiente_pls",
      stringsAsFactors = FALSE
    ) %>%
      filter(!is.na(importancia_abs), importancia_abs > 0)
  }

  list(
    pred = recortar_prediccion(pred),
    importancia = importancia,
    detalle = paste0("ncomp=", ncomp_sel, "; ncomp_max=", ncomp_max)
  )
}

ajustar_rf_boot <- function(datos, escenario, target, iteracion, test) {
  if (!paquete_ranger_disponible) return(NULL)

  n_train <- length(datos$y_train)
  p <- ncol(datos$x_train)
  if (n_train < 8 || p < 1) return(NULL)

  df_train <- as.data.frame(datos$x_train)
  df_test <- as.data.frame(datos$x_test)
  df_train[[target]] <- datos$y_train

  mtry_val <- max(1, min(p, floor(sqrt(p))))
  min_node <- max(2, floor(0.15 * n_train))

  fit <- tryCatch({
    ranger::ranger(
      formula = stats::as.formula(paste(bt(target), "~", paste(bt(datos$predictores), collapse = " + "))),
      data = df_train,
      num.trees = n_arboles_rf,
      mtry = mtry_val,
      min.node.size = min_node,
      max.depth = max_depth_rf,
      importance = "permutation",
      respect.unordered.factors = "order",
      seed = 123 + iteracion
    )
  }, error = function(e) NULL)

  if (is.null(fit)) return(NULL)

  pred <- tryCatch({
    stats::predict(fit, data = df_test)$predictions
  }, error = function(e) rep(mean(datos$y_train), nrow(df_test)))

  imp <- tryCatch(fit$variable.importance, error = function(e) NULL)

  importancia <- data.frame()
  if (!is.null(imp)) {
    importancia <- data.frame(
      escenario = escenario,
      target = target,
      modelo = "random_forest_restringido",
      iteracion = iteracion,
      predictor = names(imp),
      importancia = as.numeric(imp),
      importancia_abs = abs(as.numeric(imp)),
      tipo_importancia = "permutation_importance",
      stringsAsFactors = FALSE
    ) %>%
      filter(!is.na(importancia_abs), importancia_abs > 0)
  }

  list(
    pred = recortar_prediccion(pred),
    importancia = importancia,
    detalle = paste0("num.trees=", n_arboles_rf, "; mtry=", mtry_val, "; max.depth=", max_depth_rf)
  )
}

ajustar_gbm_boot <- function(datos, escenario, target, iteracion, test) {
  if (!paquete_gbm_disponible) return(NULL)

  n_train <- length(datos$y_train)
  p <- ncol(datos$x_train)
  if (n_train < 8 || p < 1) return(NULL)

  df_train <- as.data.frame(datos$x_train)
  df_test <- as.data.frame(datos$x_test)
  df_train[[target]] <- datos$y_train

  min_obs <- max(3, floor(0.15 * n_train))

  fit <- tryCatch({
    gbm::gbm(
      formula = stats::as.formula(paste(bt(target), "~", paste(bt(datos$predictores), collapse = " + "))),
      data = df_train,
      distribution = "gaussian",
      n.trees = n_arboles_gbm,
      interaction.depth = interaction_depth_gbm,
      shrinkage = shrinkage_gbm,
      n.minobsinnode = min_obs,
      bag.fraction = 0.8,
      train.fraction = 1,
      verbose = FALSE
    )
  }, error = function(e) NULL)

  if (is.null(fit)) return(NULL)

  pred <- tryCatch({
    stats::predict(fit, newdata = df_test, n.trees = n_arboles_gbm)
  }, error = function(e) rep(mean(datos$y_train), nrow(df_test)))

  imp <- tryCatch(gbm::summary.gbm(fit, plotit = FALSE), error = function(e) NULL)

  importancia <- data.frame()
  if (!is.null(imp) && nrow(imp) > 0) {
    importancia <- data.frame(
      escenario = escenario,
      target = target,
      modelo = "gradient_boosting_restringido",
      iteracion = iteracion,
      predictor = imp$var,
      importancia = imp$rel.inf,
      importancia_abs = abs(imp$rel.inf),
      tipo_importancia = "relative_influence",
      stringsAsFactors = FALSE
    ) %>%
      filter(!is.na(importancia_abs), importancia_abs > 0)
  }

  list(
    pred = recortar_prediccion(pred),
    importancia = importancia,
    detalle = paste0("n.trees=", n_arboles_gbm, "; depth=", interaction_depth_gbm, "; shrinkage=", shrinkage_gbm)
  )
}

ajustar_modelo_boot <- function(modelo, datos, escenario, target, iteracion, test) {
  if (modelo == "ridge") {
    return(ajustar_glmnet_boot(datos, alpha = 0, nombre_modelo = "ridge", escenario, target, iteracion, test))
  }
  if (modelo == "lasso") {
    return(ajustar_glmnet_boot(datos, alpha = 1, nombre_modelo = "lasso", escenario, target, iteracion, test))
  }
  if (modelo == "elastic_net") {
    return(ajustar_glmnet_boot(datos, alpha = 0.5, nombre_modelo = "elastic_net", escenario, target, iteracion, test))
  }
  if (modelo == "pls") {
    return(ajustar_pls_boot(datos, escenario, target, iteracion, test))
  }
  if (modelo == "random_forest_restringido") {
    return(ajustar_rf_boot(datos, escenario, target, iteracion, test))
  }
  if (modelo == "gradient_boosting_restringido") {
    return(ajustar_gbm_boot(datos, escenario, target, iteracion, test))
  }
  NULL
}

# ------------------------------------------------------------
# 4. Lectura de escenarios
# ------------------------------------------------------------

hojas_disponibles <- readxl::excel_sheets(ruta_datos_modelamiento)

mapa_escenarios <- c(
  "01_pura" = "M_pura",
  "02_expandida_evaluador" = "M_expandida_evaluador",
  "04_sensibilidad_con_ju" = "M_cata_individual",
  "05_ia_exploratoria" = "M_ia_exploratoria"
)

mapa_escenarios <- mapa_escenarios[names(mapa_escenarios) %in% hojas_disponibles]

if (length(mapa_escenarios) == 0) {
  stop("No se encontraron hojas de escenarios en datos_modelamiento.xlsx")
}

escenarios <- lapply(names(mapa_escenarios), function(hoja) {
  readxl::read_excel(ruta_datos_modelamiento, sheet = hoja) %>% as.data.frame()
})
names(escenarios) <- unname(mapa_escenarios)

# ------------------------------------------------------------
# 5. Ejecucion bootstrap
# ------------------------------------------------------------

targets_ejecutados <- bind_rows(lapply(names(escenarios), function(esc) {
  df <- escenarios[[esc]]
  bind_rows(lapply(targets_a_validar, function(tg) resumir_target_escenario(df, esc, tg)))
}))

plan_bootstrap <- targets_ejecutados %>%
  filter(ejecutar_bootstrap) %>%
  arrange(escenario, target)

metricas_bootstrap <- list()
importancia_bootstrap <- list()
modelos_ejecutados <- list()
contador_resultados <- 1
contador_importancia <- 1
contador_modelos <- 1

if (nrow(plan_bootstrap) == 0) {
  warning("No hay escenarios-target modelables para bootstrap.")
} else {
  for (i in seq_len(nrow(plan_bootstrap))) {
    escenario_i <- plan_bootstrap$escenario[i]
    target_i <- plan_bootstrap$target[i]

    message("Validacion bootstrap: ", escenario_i, " - ", target_i)

    # Semilla fija por combinacion escenario-target (no por posicion en el loop),
    # para que el resultado no dependa del orden alfabetico en que se procesan
    # los escenarios (p.ej. al renombrar un escenario cambia su posicion en
    # arrange(escenario, target), lo que antes alteraba el stream aleatorio
    # compartido y hacia que el "mejor modelo" pareciera cambiar sin que
    # cambiaran los datos).
    set.seed(123 + sum(utf8ToInt(paste0(escenario_i, "_", target_i))))

    df_base <- preparar_df_modelo(escenarios[[escenario_i]], escenario_i, target_i)

    if (nrow(df_base) < min_n_target_modelable || dplyr::n_distinct(df_base$grupo_validacion) < 4) {
      modelos_ejecutados[[contador_modelos]] <- data.frame(
        escenario = escenario_i,
        target = target_i,
        modelo = paste(modelos_a_validar, collapse = "; "),
        iteracion = NA_integer_,
        estado = "omitido_por_bajo_n_o_bajos_grupos",
        detalle = paste0("n=", nrow(df_base), "; grupos=", dplyr::n_distinct(df_base$grupo_validacion)),
        stringsAsFactors = FALSE
      )
      contador_modelos <- contador_modelos + 1
      next
    }

    for (b in seq_len(n_bootstrap)) {
      split_b <- crear_split_bootstrap_agrupado(df_base)
      if (is.null(split_b)) next

      train_b <- split_b$train
      test_b <- split_b$test

      for (modelo_i in modelos_a_validar) {
        max_pred <- ifelse(modelo_i %in% c("random_forest_restringido", "gradient_boosting_restringido"),
                           max_predictores_arboles, max_predictores_supervisados)

        predictores <- seleccionar_predictores_supervisados(train_b, target_i, max_pred = max_pred)
        datos <- preparar_matrices_x(train_b, test_b, target_i, predictores)

        if (is.null(datos)) {
          modelos_ejecutados[[contador_modelos]] <- data.frame(
            escenario = escenario_i,
            target = target_i,
            modelo = modelo_i,
            iteracion = b,
            estado = "omitido_sin_predictores_validos",
            detalle = NA_character_,
            stringsAsFactors = FALSE
          )
          contador_modelos <- contador_modelos + 1
          next
        }

        ajuste <- ajustar_modelo_boot(modelo_i, datos, escenario_i, target_i, b, test_b)

        if (is.null(ajuste)) {
          modelos_ejecutados[[contador_modelos]] <- data.frame(
            escenario = escenario_i,
            target = target_i,
            modelo = modelo_i,
            iteracion = b,
            estado = "fallo_ajuste",
            detalle = NA_character_,
            stringsAsFactors = FALSE
          )
          contador_modelos <- contador_modelos + 1
          next
        }

        met <- calcular_metricas_vec(test_b[[target_i]], ajuste$pred)

        metricas_bootstrap[[contador_resultados]] <- data.frame(
          escenario = escenario_i,
          target = target_i,
          modelo = modelo_i,
          iteracion = b,
          n_train = nrow(train_b),
          n_test = nrow(test_b),
          n_grupos_train = split_b$n_grupos_train,
          n_grupos_test = split_b$n_grupos_test,
          metodo_split = split_b$metodo_split,
          n_predictores = length(datos$predictores),
          met,
          detalle_modelo = ajuste$detalle,
          stringsAsFactors = FALSE
        )
        contador_resultados <- contador_resultados + 1

        if (!is.null(ajuste$importancia) && nrow(ajuste$importancia) > 0) {
          importancia_bootstrap[[contador_importancia]] <- ajuste$importancia %>%
            mutate(n_predictores_modelo = length(datos$predictores))
          contador_importancia <- contador_importancia + 1
        }

        modelos_ejecutados[[contador_modelos]] <- data.frame(
          escenario = escenario_i,
          target = target_i,
          modelo = modelo_i,
          iteracion = b,
          estado = "ajustado",
          detalle = ajuste$detalle,
          stringsAsFactors = FALSE
        )
        contador_modelos <- contador_modelos + 1
      }
    }
  }
}

metricas_bootstrap <- bind_rows(metricas_bootstrap)
importancia_bootstrap <- bind_rows(importancia_bootstrap)
modelos_ejecutados <- bind_rows(modelos_ejecutados)

# ------------------------------------------------------------
# 6. Resumen bootstrap y estabilidad
# ------------------------------------------------------------

resumen_metricas_bootstrap <- data.frame()
if (nrow(metricas_bootstrap) > 0) {
  resumen_metricas_bootstrap <- metricas_bootstrap %>%
    group_by(escenario, target, modelo) %>%
    summarise(
      n_bootstrap_exitosos = n(),
      n_fallback_holdout = sum(metodo_split == "fallback_holdout_75_25", na.rm = TRUE),
      pct_fallback_holdout = round(100 * n_fallback_holdout / n(), 1),
      n_train_promedio = mean(n_train, na.rm = TRUE),
      n_test_promedio = mean(n_test, na.rm = TRUE),
      mae_media = mean(mae, na.rm = TRUE),
      mae_sd = sd(mae, na.rm = TRUE),
      mae_p05 = as.numeric(stats::quantile(mae, 0.05, na.rm = TRUE)),
      mae_p50 = as.numeric(stats::quantile(mae, 0.50, na.rm = TRUE)),
      mae_p95 = as.numeric(stats::quantile(mae, 0.95, na.rm = TRUE)),
      rmse_media = mean(rmse, na.rm = TRUE),
      rmse_sd = sd(rmse, na.rm = TRUE),
      rmse_p05 = as.numeric(stats::quantile(rmse, 0.05, na.rm = TRUE)),
      rmse_p50 = as.numeric(stats::quantile(rmse, 0.50, na.rm = TRUE)),
      rmse_p95 = as.numeric(stats::quantile(rmse, 0.95, na.rm = TRUE)),
      r2_media = mean(r2, na.rm = TRUE),
      r2_p50 = as.numeric(stats::quantile(r2, 0.50, na.rm = TRUE)),
      spearman_media = mean(spearman, na.rm = TRUE),
      spearman_p50 = as.numeric(stats::quantile(spearman, 0.50, na.rm = TRUE)),
      bias_media = mean(bias, na.rm = TRUE),
      mae_ic90_ancho = mae_p95 - mae_p05,
      .groups = "drop"
    ) %>%
    mutate(
      estabilidad_error = case_when(
        is.na(mae_ic90_ancho) ~ "no_evaluable",
        mae_ic90_ancho <= 0.25 ~ "alta",
        mae_ic90_ancho <= 0.50 ~ "media",
        TRUE ~ "baja"
      ),
      ranking_mae = ave(mae_media, escenario, target, FUN = function(x) rank(x, ties.method = "min"))
    ) %>%
    arrange(escenario, target, ranking_mae, mae_media)
}

if (nrow(importancia_bootstrap) > 0) {
  importancia_bootstrap <- importancia_bootstrap %>%
    group_by(escenario, target, modelo, iteracion) %>%
    mutate(
      suma_importancia_abs = sum(importancia_abs, na.rm = TRUE),
      importancia_abs_norm = ifelse(suma_importancia_abs > 0, 100 * importancia_abs / suma_importancia_abs, 0)
    ) %>%
    ungroup()
}

estabilidad_variables <- data.frame()
if (nrow(importancia_bootstrap) > 0 && nrow(metricas_bootstrap) > 0) {
  n_boots_validos <- metricas_bootstrap %>%
    count(escenario, target, modelo, name = "n_bootstrap_exitosos")

  estabilidad_variables <- importancia_bootstrap %>%
    group_by(escenario, target, modelo, predictor) %>%
    summarise(
      n_bootstrap_con_variable = n_distinct(iteracion),
      importancia_abs_media = mean(importancia_abs, na.rm = TRUE),
      importancia_abs_mediana = median(importancia_abs, na.rm = TRUE),
      importancia_abs_norm_media = mean(importancia_abs_norm, na.rm = TRUE),
      importancia_abs_norm_mediana = median(importancia_abs_norm, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(n_boots_validos, by = c("escenario", "target", "modelo")) %>%
    mutate(
      frecuencia_seleccion = ifelse(n_bootstrap_exitosos > 0, n_bootstrap_con_variable / n_bootstrap_exitosos, NA_real_),
      estabilidad_variable = case_when(
        is.na(frecuencia_seleccion) ~ "no_evaluable",
        frecuencia_seleccion >= 0.80 ~ "alta",
        frecuencia_seleccion >= 0.50 ~ "media",
        TRUE ~ "baja"
      )
    ) %>%
    arrange(escenario, target, modelo, desc(frecuencia_seleccion), desc(importancia_abs_norm_media))
}

top_variables_estables <- data.frame()
if (nrow(estabilidad_variables) > 0) {
  top_variables_estables <- estabilidad_variables %>%
    group_by(escenario, target, modelo) %>%
    arrange(desc(frecuencia_seleccion), desc(importancia_abs_norm_media), .by_group = TRUE) %>%
    slice_head(n = 10) %>%
    ungroup()
}

# ------------------------------------------------------------
# 7. Comparacion con validacion cruzada previa
# ------------------------------------------------------------

comparacion_cv_bootstrap <- data.frame()
if (file.exists(ruta_resultados_small_data)) {
  hojas_small_data <- readxl::excel_sheets(ruta_resultados_small_data)
  if ("00_resumen_metricas" %in% hojas_small_data && nrow(resumen_metricas_bootstrap) > 0) {
    metricas_cv <- readxl::read_excel(ruta_resultados_small_data, sheet = "00_resumen_metricas") %>%
      as.data.frame() %>%
      select(any_of(c("escenario", "target", "modelo", "mae", "rmse", "r2", "spearman", "bias"))) %>%
      rename(
        mae_cv = mae,
        rmse_cv = rmse,
        r2_cv = r2,
        spearman_cv = spearman,
        bias_cv = bias
      )

    comparacion_cv_bootstrap <- resumen_metricas_bootstrap %>%
      left_join(metricas_cv, by = c("escenario", "target", "modelo")) %>%
      mutate(
        diferencia_mae_bootstrap_cv = mae_media - mae_cv,
        interpretacion = case_when(
          is.na(mae_cv) ~ "sin_cv_previa",
          mae_media <= mae_cv ~ "bootstrap_igual_o_mejor_que_cv",
          mae_media > mae_cv & diferencia_mae_bootstrap_cv <= 0.20 ~ "bootstrap_levemente_peor",
          TRUE ~ "bootstrap_mucho_peor_revisar_estabilidad"
        )
      ) %>%
      arrange(escenario, target, ranking_mae, mae_media)
  }
}

# ------------------------------------------------------------
# 8. Graficos
# ------------------------------------------------------------

graficos_generados <- data.frame(
  grafico = character(),
  ruta = character(),
  descripcion = character(),
  stringsAsFactors = FALSE
)

if (nrow(resumen_metricas_bootstrap) > 0) {
  datos_graf_mae <- resumen_metricas_bootstrap %>%
    filter(target == "y_fenolico_comun") %>%
    mutate(modelo = factor(modelo, levels = unique(modelo)))

  if (nrow(datos_graf_mae) > 0) {
    p_mae <- ggplot(datos_graf_mae, aes(x = mae_media, y = escenario, fill = modelo)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_errorbar(
        aes(xmin = mae_p05, xmax = mae_p95),
        position = position_dodge(width = 0.8),
        width = 0.25,
        linewidth = 0.4
      ) +
      labs(
        title = "MAE bootstrap de modelos small data para y_fenolico_comun",
        subtitle = "Barras: media bootstrap; segmentos: percentiles 5 y 95",
        x = "MAE bootstrap",
        y = "Escenario",
        fill = "Modelo"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "right")

    ruta_graf <- file.path(dir_outputs_bootstrap, "01_bootstrap_mae_y_fenolico.png")
    ggsave(ruta_graf, p_mae, width = 12, height = 7, dpi = 300)

    graficos_generados <- bind_rows(graficos_generados, data.frame(
      grafico = "01_bootstrap_mae_y_fenolico",
      ruta = ruta_graf,
      descripcion = "MAE bootstrap medio con intervalo percentil 5-95 para y_fenolico_comun.",
      stringsAsFactors = FALSE
    ))
  }
}

if (nrow(estabilidad_variables) > 0) {
  datos_var <- estabilidad_variables %>%
    filter(escenario == "M_pura", target == "y_fenolico_comun") %>%
    group_by(predictor) %>%
    summarise(
      frecuencia_media = mean(frecuencia_seleccion, na.rm = TRUE),
      importancia_norm_media = mean(importancia_abs_norm_media, na.rm = TRUE),
      n_modelos = n_distinct(modelo),
      .groups = "drop"
    ) %>%
    arrange(desc(frecuencia_media), desc(importancia_norm_media)) %>%
    slice_head(n = 12)

  if (nrow(datos_var) > 0) {
    datos_var <- datos_var %>% mutate(predictor = reorder(predictor, frecuencia_media))

    p_var <- ggplot(datos_var, aes(x = frecuencia_media, y = predictor)) +
      geom_col() +
      labs(
        title = "Predictores estables en M_pura - y_fenolico_comun",
        subtitle = "Frecuencia media de seleccion/importancia en bootstrap",
        x = "Frecuencia media de seleccion",
        y = "Predictor quimico"
      ) +
      theme_minimal(base_size = 12)

    ruta_graf <- file.path(dir_outputs_bootstrap, "02_variables_estables_M_pura_y_fenolico.png")
    ggsave(ruta_graf, p_var, width = 10, height = 7, dpi = 300)

    graficos_generados <- bind_rows(graficos_generados, data.frame(
      grafico = "02_variables_estables_M_pura_y_fenolico",
      ruta = ruta_graf,
      descripcion = "Predictores mas estables en bootstrap para la matriz pura y target fenolico.",
      stringsAsFactors = FALSE
    ))
  }
}

# ------------------------------------------------------------
# 9. Resumen ejecutivo y diccionario
# ------------------------------------------------------------

resumen_ejecucion <- data.frame(
  item = c(
    "n_bootstrap_configurado",
    "escenarios_leidos",
    "targets_evaluados",
    "modelos_candidatos",
    "modelos_ejecutados_exitosos",
    "filas_metricas_bootstrap",
    "filas_estabilidad_variables",
    "random_forest_disponible",
    "gradient_boosting_disponible",
    "archivo_salida"
  ),
  valor = c(
    as.character(n_bootstrap),
    as.character(length(escenarios)),
    as.character(nrow(plan_bootstrap)),
    paste(modelos_a_validar, collapse = "; "),
    as.character(sum(modelos_ejecutados$estado == "ajustado", na.rm = TRUE)),
    as.character(nrow(metricas_bootstrap)),
    as.character(nrow(estabilidad_variables)),
    as.character(paquete_ranger_disponible),
    as.character(paquete_gbm_disponible),
    ruta_salida_excel
  ),
  stringsAsFactors = FALSE
)

configuracion_bootstrap <- data.frame(
  parametro = c(
    "tipo_validacion",
    "unidad_remuestreo",
    "n_bootstrap",
    "targets",
    "modelos",
    "criterio_error_principal",
    "intervalo_reportado",
    "nota_expandida",
    "nota_ia"
  ),
  valor = c(
    "bootstrap agrupado con evaluacion out-of-bag",
    "grupo_validacion; en expandida corresponde a unidad analitica/muestra para evitar fuga",
    as.character(n_bootstrap),
    paste(targets_a_validar, collapse = "; "),
    paste(modelos_a_validar, collapse = "; "),
    "MAE",
    "percentiles 5 y 95",
    "No separar evaluadores de la misma unidad entre entrenamiento y prueba",
    "Escenario exploratorio; no reemplaza cata real"
  ),
  stringsAsFactors = FALSE
)

diccionario <- data.frame(
  hoja = c(
    "00_resumen",
    "01_metricas_bootstrap",
    "02_resumen_metricas_bootstrap",
    "03_estabilidad_variables",
    "04_top_variables_estables",
    "05_comparacion_cv_bootstrap",
    "06_targets_ejecutados",
    "07_modelos_ejecutados",
    "08_configuracion_bootstrap",
    "09_graficos_generados"
  ),
  descripcion = c(
    "Conteos generales de ejecucion.",
    "Metricas por iteracion bootstrap, escenario, target y modelo. Incluye metodo_split: 'bootstrap_con_reemplazo' (normal) o 'fallback_holdout_75_25' (cuando el remuestreo con reemplazo dejo menos de 2 grupos fuera de bolsa).",
    "Resumen de metricas bootstrap con media, desviacion y percentiles 5-95. Incluye n_fallback_holdout/pct_fallback_holdout: cuantas de las iteraciones exitosas usaron el fallback en vez del bootstrap con reemplazo real.",
    "Estabilidad de predictores por frecuencia de seleccion/importancia.",
    "Top 10 predictores estables por escenario, target y modelo.",
    "Comparacion entre validacion cruzada previa y bootstrap.",
    "Targets evaluados y decision de ejecucion.",
    "Registro de ajustes exitosos u omitidos por modelo e iteracion.",
    "Parametros metodologicos usados en bootstrap.",
    "Rutas de figuras generadas."
  ),
  stringsAsFactors = FALSE
)

# Limitar tamano de hojas muy grandes si fuese necesario.
# Se conservan metricas e importancias agregadas; no se exportan predicciones OOB fila a fila.

hojas_salida <- list(
  "00_resumen" = resumen_ejecucion,
  "01_metricas_bootstrap" = metricas_bootstrap,
  "02_resumen_metricas_boot" = resumen_metricas_bootstrap,
  "03_estabilidad_variables" = estabilidad_variables,
  "04_top_variables_estables" = top_variables_estables,
  "05_comparacion_cv_boot" = comparacion_cv_bootstrap,
  "06_targets_ejecutados" = targets_ejecutados,
  "07_modelos_ejecutados" = modelos_ejecutados,
  "08_configuracion_boot" = configuracion_bootstrap,
  "09_graficos_generados" = graficos_generados,
  "10_diccionario" = diccionario
)

# Remover hojas completamente vacias para evitar problemas de exportacion.
hojas_salida <- hojas_salida[vapply(hojas_salida, function(x) is.data.frame(x) && nrow(x) > 0, logical(1))]

guardar_excel_seguro(hojas_salida, ruta_salida_excel)

cat("\nProceso terminado correctamente.\n")
cat("Archivo generado:\n", ruta_salida_excel, "\n")
cat("Graficos generados en:\n", dir_outputs_bootstrap, "\n")
cat("\nLectura clave:\n")
cat("\n- El bootstrap evalua estabilidad de error y variables, no solo desempeno puntual.")
cat("\n- Random Forest debe confirmarse por estabilidad antes de declararlo modelo principal.")
cat("\n- PLS y modelos regularizados se interpretan con especial atencion por su adecuacion a colinealidad y small data.\n")
