# ============================================================
# 15_modelos_small_data.R
# Modelos supervisados adaptados a small data
# Tesis whisky chileno - modelamiento quimico-sensorial
# ============================================================

# Este script compara modelos supervisados mas robustos que la linea base:
# - Ridge
# - Lasso
# - Elastic Net
# - PLS
# - Random Forest restringido
# - Gradient Boosting restringido
#
# El objetivo no es maximizar desempeno de forma agresiva, sino comparar
# modelos adecuados para small data, controlando fuga de informacion y
# registrando estabilidad, predictores usados e importancia de variables.

# ------------------------------------------------------------
# 1. Configuracion
# ------------------------------------------------------------

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

# Paquetes obligatorios y opcionales.
# No se instalan paquetes automaticamente, porque en algunos entornos
# CRAN puede estar bloqueado o sin acceso a internet. Si falta un paquete
# obligatorio, el script se detiene con una instruccion clara. Si falta un
# paquete opcional, el modelo asociado se omite y queda registrado.
paquetes_obligatorios <- c(
  "readxl", "dplyr", "tidyr", "stringr", "writexl", "ggplot2",
  "glmnet", "pls"
)

paquetes_opcionales <- c("ranger", "gbm")

paquetes_obligatorios_faltantes <- paquetes_obligatorios[
  !vapply(paquetes_obligatorios, requireNamespace, logical(1), quietly = TRUE)
]

if (length(paquetes_obligatorios_faltantes) > 0) {
  stop(
    "Faltan paquetes obligatorios: ", paste(paquetes_obligatorios_faltantes, collapse = ", "),
    "
Instalalos manualmente con: install.packages(c('",
    paste(paquetes_obligatorios_faltantes, collapse = "', '"),
    "'))"
  )
}

paquetes_opcionales_disponibles <- vapply(
  paquetes_opcionales,
  requireNamespace,
  logical(1),
  quietly = TRUE
)

tiene_ranger <- isTRUE(paquetes_opcionales_disponibles[["ranger"]])
tiene_gbm <- isTRUE(paquetes_opcionales_disponibles[["gbm"]])

if (!tiene_ranger) {
  message("Paquete opcional 'ranger' no disponible: se omitira Random Forest restringido.")
}
if (!tiene_gbm) {
  message("Paquete opcional 'gbm' no disponible: se omitira Gradient Boosting restringido.")
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)
library(ggplot2)
library(glmnet)
library(pls)
if (tiene_ranger) library(ranger)
if (tiene_gbm) library(gbm)

set.seed(123)

# Rutas

dir_modelamiento <- file.path(dir_data, "modelamiento")
dir_outputs <- file.path(dir_proyecto, "outputs")
dir_outputs_modelamiento <- file.path(dir_outputs, "modelamiento")
dir_outputs_small_data <- file.path(dir_outputs_modelamiento, "modelos_small_data")

dir.create(dir_modelamiento, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_outputs_small_data, recursive = TRUE, showWarnings = FALSE)

ruta_datos_modelamiento <- file.path(dir_modelamiento, "datos_modelamiento.xlsx")
ruta_modelo_base <- file.path(dir_modelamiento, "resultados_modelo_base.xlsx")
ruta_salida_excel <- file.path(dir_modelamiento, "resultados_modelos_small_data.xlsx")

if (!file.exists(ruta_datos_modelamiento)) {
  stop(
    "No existe el archivo requerido: ", ruta_datos_modelamiento,
    "\nEjecuta primero scripts/R/modelamiento/12_preparar_datos_modelamiento.R"
  )
}

# Targets definidos en la fase diagnostica.
targets_candidatos <- c(
  "y_fenolico_comun",
  "y_frutal_comun",
  "y_ahumado_comun",
  "y_medicinal_comun"
)

# Criterios small data.
min_n_target_modelable <- 8
min_n_correlacion <- 5
max_predictores_supervisados <- 15
max_predictores_arboles <- 10
limite_inferior_target <- 0
limite_superior_target <- 5

# Parametros restringidos para reducir sobreajuste.
max_componentes_pls <- 5
n_arboles_rf <- 300
max_depth_rf <- 3
n_arboles_gbm <- 100
interaction_depth_gbm <- 1
shrinkage_gbm <- 0.05

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

bt <- function(x) {
  paste0("`", x, "`")
}

obtener_predictores_x <- function(df) {
  grep("^x_", names(df), value = TRUE)
}

recortar_prediccion <- function(x) {
  pmin(pmax(as.numeric(x), limite_inferior_target), limite_superior_target)
}

obtener_id_validacion <- function(df, escenario) {
  # Se replica la logica usada en el modelo base para mantener comparabilidad.
  # Se agrupa por identificador_quimico y no por muestra_base ni por
  # unidad_analitica_id (ver justificacion detallada en 14_modelo_base.R):
  # muestra_base deja dos muestras comerciales, Dalwhinnie y Caol Ila,
  # repartidas en dos codigos distintos cada una porque se midieron en
  # GC-FID y en GC-MS por separado, e identificador_quimico es la identidad
  # ya resuelta entre bloques.
  if ("identificador_quimico" %in% names(df)) return(as.character(df$identificador_quimico))
  if ("muestra_base" %in% names(df)) return(as.character(df$muestra_base))
  if ("unidad_analitica_id" %in% names(df)) return(as.character(df$unidad_analitica_id))
  as.character(seq_len(nrow(df)))
}

preparar_df_modelo <- function(df, target) {
  cols_x <- obtener_predictores_x(df)

  if (!(target %in% names(df))) return(data.frame())

  df %>%
    mutate(across(all_of(c(cols_x, target)), convertir_numericamente)) %>%
    filter(!is.na(.data[[target]]))
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
    ejecutar_modelos_small_data = length(valores_validos) >= min_n_target_modelable &&
      dplyr::n_distinct(valores_validos) >= 2 &&
      !is.na(stats::sd(valores_validos)) && stats::sd(valores_validos) > 0,
    prioridad = case_when(
      target == "y_fenolico_comun" & length(valores_validos) >= 15 ~ "principal",
      target == "y_frutal_comun" & length(valores_validos) >= min_n_target_modelable ~ "secundario_exploratorio",
      TRUE ~ "exploratorio_o_no_modelable"
    ),
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

  # Fallback no supervisado: si no hay correlaciones calculables, usar variables con mayor disponibilidad y variabilidad.
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
  if (length(predictores) == 0) {
    return(list(train = train, test = test, medianas = data.frame()))
  }

  medianas <- data.frame(
    predictor = predictores,
    mediana_train = NA_real_,
    stringsAsFactors = FALSE
  )

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
    dplyr::n_distinct(vx[!is.na(vx)]) >= 2 && !is.na(stats::sd(vx, na.rm = TRUE)) && stats::sd(vx, na.rm = TRUE) > 0
  }, logical(1))]
}

preparar_matrices_x <- function(train, test, target, predictores) {
  predictores <- filtrar_predictores_con_varianza(train, predictores)
  if (length(predictores) == 0) return(NULL)

  imputado <- imputar_con_mediana_train(train, test, predictores)
  train_imp <- imputado$train
  test_imp <- imputado$test

  # Remover predictores que quedan sin varianza despues de imputacion.
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

calcular_metricas_grupo <- function(df_m) {
  df_m <- df_m %>% filter(!is.na(y_obs), !is.na(y_pred))

  if (nrow(df_m) == 0) return(data.frame())

  error <- df_m$y_pred - df_m$y_obs
  sst <- sum((df_m$y_obs - mean(df_m$y_obs))^2)
  sse <- sum(error^2)
  r2 <- ifelse(sst > 0, 1 - sse / sst, NA_real_)
  spearman <- if (nrow(df_m) >= 3 && dplyr::n_distinct(df_m$y_pred) >= 2 && dplyr::n_distinct(df_m$y_obs) >= 2) {
    suppressWarnings(stats::cor(df_m$y_obs, df_m$y_pred, method = "spearman"))
  } else {
    NA_real_
  }

  data.frame(
    escenario = unique(df_m$escenario)[1],
    target = unique(df_m$target)[1],
    modelo = unique(df_m$modelo)[1],
    n_observaciones = nrow(df_m),
    n_grupos_validacion = dplyr::n_distinct(df_m$grupo_validacion),
    mae = round(mean(abs(error)), 6),
    rmse = round(sqrt(mean(error^2)), 6),
    r2 = round(r2, 6),
    spearman = round(spearman, 6),
    bias = round(mean(error), 6),
    stringsAsFactors = FALSE
  )
}

calcular_metricas <- function(df_pred) {
  if (nrow(df_pred) == 0) return(data.frame())

  claves <- df_pred %>%
    distinct(escenario, target, modelo) %>%
    arrange(escenario, target, modelo)

  bind_rows(lapply(seq_len(nrow(claves)), function(i) {
    df_m <- df_pred %>%
      filter(
        escenario == claves$escenario[i],
        target == claves$target[i],
        modelo == claves$modelo[i]
      )
    calcular_metricas_grupo(df_m)
  }))
}

registrar_prediccion <- function(escenario, target, modelo, test, y_pred_raw, n_train, predictores_usados, detalle_modelo = "") {
  data.frame(
    escenario = escenario,
    target = target,
    modelo = modelo,
    grupo_validacion = test$grupo_validacion,
    unidad_analitica_id = if ("unidad_analitica_id" %in% names(test)) as.character(test$unidad_analitica_id) else NA_character_,
    muestra_base = if ("muestra_base" %in% names(test)) as.character(test$muestra_base) else NA_character_,
    y_obs = test[[target]],
    y_pred_raw = as.numeric(y_pred_raw),
    y_pred = recortar_prediccion(y_pred_raw),
    n_train = n_train,
    predictores_usados = paste(predictores_usados, collapse = "; "),
    detalle_modelo = detalle_modelo,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 3. Funciones de modelos
# ------------------------------------------------------------

ajustar_glmnet_cv <- function(datos, alpha, nombre_modelo, escenario, target, test, fold_id) {
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

  pred_df <- registrar_prediccion(
    escenario = escenario,
    target = target,
    modelo = nombre_modelo,
    test = test,
    y_pred_raw = pred,
    n_train = n_train,
    predictores_usados = datos$predictores,
    detalle_modelo = paste0("alpha=", alpha, "; lambda_min=", round(fit$lambda.min, 8))
  )

  coef_mat <- tryCatch(as.matrix(stats::coef(fit, s = "lambda.min")), error = function(e) NULL)

  importancia <- data.frame()
  if (!is.null(coef_mat)) {
    importancia <- data.frame(
      escenario = escenario,
      target = target,
      modelo = nombre_modelo,
      fold_id = fold_id,
      predictor = rownames(coef_mat),
      importancia = as.numeric(coef_mat[, 1]),
      importancia_abs = abs(as.numeric(coef_mat[, 1])),
      tipo_importancia = "coeficiente_lambda_min",
      stringsAsFactors = FALSE
    ) %>%
      filter(predictor != "(Intercept)", importancia_abs > 0)
  }

  hiper <- data.frame(
    escenario = escenario,
    target = target,
    modelo = nombre_modelo,
    fold_id = fold_id,
    n_train = n_train,
    n_test = nrow(test),
    n_predictores = p,
    alpha = alpha,
    lambda_min = fit$lambda.min,
    lambda_1se = fit$lambda.1se,
    nfolds_inner = nfolds_inner,
    parametro_extra = NA_character_,
    estado_modelo = "ajustado",
    stringsAsFactors = FALSE
  )

  list(predicciones = pred_df, hiperparametros = hiper, importancia = importancia)
}

ajustar_pls <- function(datos, escenario, target, test, fold_id) {
  n_train <- length(datos$y_train)
  p <- ncol(datos$x_train)
  ncomp_max <- min(max_componentes_pls, p, max(1, n_train - 2))

  if (n_train < 6 || p < 1 || ncomp_max < 1) return(NULL)

  df_train <- as.data.frame(datos$x_train)
  df_test <- as.data.frame(datos$x_test)
  df_train[[target]] <- datos$y_train

  formula_pls <- stats::as.formula(paste(bt(target), "~", paste(bt(datos$predictores), collapse = " + ")))

  fit <- tryCatch({
    pls::plsr(
      formula_pls,
      data = df_train,
      ncomp = ncomp_max,
      validation = "LOO",
      scale = TRUE
    )
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

  pred_df <- registrar_prediccion(
    escenario = escenario,
    target = target,
    modelo = "pls",
    test = test,
    y_pred_raw = pred,
    n_train = n_train,
    predictores_usados = datos$predictores,
    detalle_modelo = paste0("ncomp=", ncomp_sel, "; ncomp_max=", ncomp_max)
  )

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
      fold_id = fold_id,
      predictor = pred_names,
      importancia = coefs,
      importancia_abs = abs(coefs),
      tipo_importancia = "coeficiente_pls",
      stringsAsFactors = FALSE
    ) %>%
      filter(!is.na(importancia_abs), importancia_abs > 0)
  }

  hiper <- data.frame(
    escenario = escenario,
    target = target,
    modelo = "pls",
    fold_id = fold_id,
    n_train = n_train,
    n_test = nrow(test),
    n_predictores = p,
    alpha = NA_real_,
    lambda_min = NA_real_,
    lambda_1se = NA_real_,
    nfolds_inner = NA_integer_,
    parametro_extra = paste0("ncomp=", ncomp_sel),
    estado_modelo = "ajustado",
    stringsAsFactors = FALSE
  )

  list(predicciones = pred_df, hiperparametros = hiper, importancia = importancia)
}

ajustar_random_forest <- function(datos, escenario, target, test, fold_id) {
  if (!tiene_ranger) return(NULL)
  n_train <- length(datos$y_train)
  p_total <- ncol(datos$x_train)

  if (n_train < 8 || p_total < 1) return(NULL)

  # Restringir arboles a los predictores mas relacionados con el target dentro del fold.
  pred_arbol <- head(datos$predictores, max_predictores_arboles)
  if (length(pred_arbol) == 0) return(NULL)

  df_train <- datos$train[, c(pred_arbol, target), drop = FALSE]
  df_test <- datos$test[, pred_arbol, drop = FALSE]

  mtry <- max(1, floor(sqrt(length(pred_arbol))))
  min_node <- max(3, floor(n_train / 5))

  fit <- tryCatch({
    ranger::ranger(
      dependent.variable.name = target,
      data = df_train,
      num.trees = n_arboles_rf,
      mtry = mtry,
      min.node.size = min_node,
      max.depth = max_depth_rf,
      importance = "permutation",
      seed = 123
    )
  }, error = function(e) NULL)

  if (is.null(fit)) return(NULL)

  pred <- tryCatch({
    as.numeric(stats::predict(fit, data = df_test)$predictions)
  }, error = function(e) rep(mean(datos$y_train), nrow(df_test)))

  pred_df <- registrar_prediccion(
    escenario = escenario,
    target = target,
    modelo = "random_forest_restringido",
    test = test,
    y_pred_raw = pred,
    n_train = n_train,
    predictores_usados = pred_arbol,
    detalle_modelo = paste0("num_trees=", n_arboles_rf, "; mtry=", mtry, "; min_node=", min_node, "; max_depth=", max_depth_rf)
  )

  imp <- tryCatch(fit$variable.importance, error = function(e) NULL)
  importancia <- data.frame()
  if (!is.null(imp)) {
    importancia <- data.frame(
      escenario = escenario,
      target = target,
      modelo = "random_forest_restringido",
      fold_id = fold_id,
      predictor = names(imp),
      importancia = as.numeric(imp),
      importancia_abs = abs(as.numeric(imp)),
      tipo_importancia = "permutation_importance",
      stringsAsFactors = FALSE
    ) %>%
      filter(!is.na(importancia_abs))
  }

  hiper <- data.frame(
    escenario = escenario,
    target = target,
    modelo = "random_forest_restringido",
    fold_id = fold_id,
    n_train = n_train,
    n_test = nrow(test),
    n_predictores = length(pred_arbol),
    alpha = NA_real_,
    lambda_min = NA_real_,
    lambda_1se = NA_real_,
    nfolds_inner = NA_integer_,
    parametro_extra = paste0("mtry=", mtry, "; min_node=", min_node, "; max_depth=", max_depth_rf),
    estado_modelo = "ajustado",
    stringsAsFactors = FALSE
  )

  list(predicciones = pred_df, hiperparametros = hiper, importancia = importancia)
}

ajustar_gradient_boosting <- function(datos, escenario, target, test, fold_id) {
  if (!tiene_gbm) return(NULL)
  n_train <- length(datos$y_train)
  p_total <- ncol(datos$x_train)

  if (n_train < 10 || p_total < 1) return(NULL)

  pred_arbol <- head(datos$predictores, max_predictores_arboles)
  if (length(pred_arbol) == 0) return(NULL)

  df_train <- datos$train[, c(pred_arbol, target), drop = FALSE]
  df_test <- datos$test[, pred_arbol, drop = FALSE]

  n_min <- max(3, floor(n_train / 4))
  formula_gbm <- stats::as.formula(paste(bt(target), "~", paste(bt(pred_arbol), collapse = " + ")))

  fit <- tryCatch({
    gbm::gbm(
      formula = formula_gbm,
      data = df_train,
      distribution = "gaussian",
      n.trees = n_arboles_gbm,
      interaction.depth = interaction_depth_gbm,
      shrinkage = shrinkage_gbm,
      n.minobsinnode = n_min,
      bag.fraction = 0.8,
      train.fraction = 1.0,
      verbose = FALSE
    )
  }, error = function(e) NULL)

  if (is.null(fit)) return(NULL)

  pred <- tryCatch({
    as.numeric(stats::predict(fit, newdata = df_test, n.trees = n_arboles_gbm))
  }, error = function(e) rep(mean(datos$y_train), nrow(df_test)))

  pred_df <- registrar_prediccion(
    escenario = escenario,
    target = target,
    modelo = "gradient_boosting_restringido",
    test = test,
    y_pred_raw = pred,
    n_train = n_train,
    predictores_usados = pred_arbol,
    detalle_modelo = paste0("n_trees=", n_arboles_gbm, "; depth=", interaction_depth_gbm, "; shrinkage=", shrinkage_gbm, "; n_min=", n_min)
  )

  imp <- tryCatch(suppressWarnings(summary(fit, plotit = FALSE)), error = function(e) NULL)
  importancia <- data.frame()
  if (!is.null(imp) && nrow(imp) > 0) {
    importancia <- data.frame(
      escenario = escenario,
      target = target,
      modelo = "gradient_boosting_restringido",
      fold_id = fold_id,
      predictor = imp$var,
      importancia = imp$rel.inf,
      importancia_abs = abs(imp$rel.inf),
      tipo_importancia = "relative_influence",
      stringsAsFactors = FALSE
    ) %>%
      filter(!is.na(importancia_abs))
  }

  hiper <- data.frame(
    escenario = escenario,
    target = target,
    modelo = "gradient_boosting_restringido",
    fold_id = fold_id,
    n_train = n_train,
    n_test = nrow(test),
    n_predictores = length(pred_arbol),
    alpha = NA_real_,
    lambda_min = NA_real_,
    lambda_1se = NA_real_,
    nfolds_inner = NA_integer_,
    parametro_extra = paste0("n_trees=", n_arboles_gbm, "; depth=", interaction_depth_gbm, "; shrinkage=", shrinkage_gbm),
    estado_modelo = "ajustado",
    stringsAsFactors = FALSE
  )

  list(predicciones = pred_df, hiperparametros = hiper, importancia = importancia)
}

# ------------------------------------------------------------
# 4. Validacion cruzada por escenario-target
# ------------------------------------------------------------

validar_modelos_small_data <- function(df, escenario, target) {
  df_modelo <- preparar_df_modelo(df, target)

  if (nrow(df_modelo) < min_n_target_modelable || dplyr::n_distinct(df_modelo[[target]]) < 2) {
    return(list(predicciones = data.frame(), hiperparametros = data.frame(), importancia = data.frame()))
  }

  df_modelo$grupo_validacion <- obtener_id_validacion(df_modelo, escenario)
  grupos <- unique(df_modelo$grupo_validacion)

  predicciones <- list()
  hiperparametros <- list()
  importancias <- list()
  idx_pred <- 1
  idx_hiper <- 1
  idx_imp <- 1

  for (fold_id in seq_along(grupos)) {
    g <- grupos[fold_id]
    train <- df_modelo %>% filter(grupo_validacion != g)
    test <- df_modelo %>% filter(grupo_validacion == g)

    if (nrow(train) < 6 || nrow(test) == 0) next

    predictores <- seleccionar_predictores_supervisados(train, target, max_predictores_supervisados)
    datos <- preparar_matrices_x(train, test, target, predictores)

    if (is.null(datos)) next

    modelos_fold <- list(
      ajustar_glmnet_cv(datos, alpha = 0, nombre_modelo = "ridge", escenario = escenario, target = target, test = test, fold_id = fold_id),
      ajustar_glmnet_cv(datos, alpha = 1, nombre_modelo = "lasso", escenario = escenario, target = target, test = test, fold_id = fold_id),
      ajustar_glmnet_cv(datos, alpha = 0.5, nombre_modelo = "elastic_net", escenario = escenario, target = target, test = test, fold_id = fold_id),
      ajustar_pls(datos, escenario = escenario, target = target, test = test, fold_id = fold_id),
      ajustar_random_forest(datos, escenario = escenario, target = target, test = test, fold_id = fold_id),
      ajustar_gradient_boosting(datos, escenario = escenario, target = target, test = test, fold_id = fold_id)
    )

    for (res in modelos_fold) {
      if (is.null(res)) next
      if (nrow(res$predicciones) > 0) {
        predicciones[[idx_pred]] <- res$predicciones
        idx_pred <- idx_pred + 1
      }
      if (nrow(res$hiperparametros) > 0) {
        hiperparametros[[idx_hiper]] <- res$hiperparametros
        idx_hiper <- idx_hiper + 1
      }
      if (nrow(res$importancia) > 0) {
        importancias[[idx_imp]] <- res$importancia
        idx_imp <- idx_imp + 1
      }
    }
  }

  list(
    predicciones = bind_rows(predicciones),
    hiperparametros = bind_rows(hiperparametros),
    importancia = bind_rows(importancias)
  )
}

# ------------------------------------------------------------
# 5. Lectura de escenarios
# ------------------------------------------------------------

escenarios <- data.frame(
  escenario = c("M_pura", "M_expandida_evaluador", "M_cata_individual", "M_ia_exploratoria"),
  hoja = c("01_pura", "02_expandida_evaluador", "04_sensibilidad_con_ju", "05_ia_exploratoria"),
  rol = c(
    "principal_real",
    "variabilidad_evaluador",
    "sensibilidad_con_cata_individual_ju",
    "ia_exploratoria_no_real"
  ),
  stringsAsFactors = FALSE
)

hojas_disponibles <- readxl::excel_sheets(ruta_datos_modelamiento)
escenarios <- escenarios %>% mutate(disponible = hoja %in% hojas_disponibles)

if (!all(escenarios$disponible)) {
  stop("Faltan hojas en datos_modelamiento.xlsx: ", paste(escenarios$hoja[!escenarios$disponible], collapse = ", "))
}

datos_escenarios <- list()
for (i in seq_len(nrow(escenarios))) {
  esc <- escenarios$escenario[i]
  hoja <- escenarios$hoja[i]
  datos_escenarios[[esc]] <- readxl::read_excel(ruta_datos_modelamiento, sheet = hoja)
}

# Las hojas de M_pura, M_expandida_evaluador y M_cata_individual comparten
# las mismas 32 columnas x_ candidatas (13 GC-FID + 13 JU + 1 Folin + 5
# GC-MS), pero el conjunto de predictores DECLARADO para esos escenarios
# excluye deliberadamente GC-MS (seccion imp:consideraciones): M_pura y
# M_expandida_evaluador usan solo los 13 de GC-FID, y M_cata_individual
# agrega los 13 de JU (26 en total), sin GC-MS. Sin esta exclusion, el
# selector de predictores por correlacion puede elegir GC-MS en algunas
# particiones, apartando los resultados del escenario de 13/26 predictores
# usado en el resto de la tesis (Tabla 4.1, mismo bug corregido en
# 13_diagnostico_modelamiento.R, 16_validacion_bootstrap_cv.R, 16b, 16c,
# 16d, 16f y 16h). M_ia_exploratoria no se toca.
if (!is.null(datos_escenarios[["M_pura"]])) {
  cols_excluir <- setdiff(obtener_predictores_x(datos_escenarios[["M_pura"]]), grep("^x_gcfid_", names(datos_escenarios[["M_pura"]]), value = TRUE))
  datos_escenarios[["M_pura"]] <- datos_escenarios[["M_pura"]][, setdiff(names(datos_escenarios[["M_pura"]]), cols_excluir), drop = FALSE]
}
if (!is.null(datos_escenarios[["M_expandida_evaluador"]])) {
  cols_excluir <- setdiff(obtener_predictores_x(datos_escenarios[["M_expandida_evaluador"]]), grep("^x_gcfid_", names(datos_escenarios[["M_expandida_evaluador"]]), value = TRUE))
  datos_escenarios[["M_expandida_evaluador"]] <- datos_escenarios[["M_expandida_evaluador"]][, setdiff(names(datos_escenarios[["M_expandida_evaluador"]]), cols_excluir), drop = FALSE]
}
if (!is.null(datos_escenarios[["M_cata_individual"]])) {
  predictores_gcfid_ju <- grep("^x_gcfid_|^x_ju_", names(datos_escenarios[["M_cata_individual"]]), value = TRUE)
  cols_excluir <- setdiff(obtener_predictores_x(datos_escenarios[["M_cata_individual"]]), predictores_gcfid_ju)
  datos_escenarios[["M_cata_individual"]] <- datos_escenarios[["M_cata_individual"]][, setdiff(names(datos_escenarios[["M_cata_individual"]]), cols_excluir), drop = FALSE]
}

# ------------------------------------------------------------
# 6. Diagnostico de targets a ejecutar
# ------------------------------------------------------------

targets_diagnostico <- bind_rows(lapply(names(datos_escenarios), function(esc) {
  df <- datos_escenarios[[esc]]
  bind_rows(lapply(targets_candidatos, function(t) resumir_target_escenario(df, esc, t)))
})) %>%
  mutate(
    decision = case_when(
      ejecutar_modelos_small_data & target == "y_fenolico_comun" ~ "ejecutar_principal",
      ejecutar_modelos_small_data & target == "y_frutal_comun" ~ "ejecutar_exploratorio",
      ejecutar_modelos_small_data ~ "ejecutar_solo_si_se_requiere",
      TRUE ~ "no_ejecutar"
    )
  )

# Se ejecutan los targets principal y secundario modelable, igual que en el modelo base.
targets_a_ejecutar <- targets_diagnostico %>%
  filter(decision %in% c("ejecutar_principal", "ejecutar_exploratorio"))

# ------------------------------------------------------------
# 7. Ajuste de modelos small data
# ------------------------------------------------------------

resultados_lista <- list()
for (i in seq_len(nrow(targets_a_ejecutar))) {
  esc <- targets_a_ejecutar$escenario[i]
  target <- targets_a_ejecutar$target[i]
  message("Ejecutando modelos small data: ", esc, " - ", target)
  resultados_lista[[paste(esc, target, sep = "__")]] <- validar_modelos_small_data(datos_escenarios[[esc]], esc, target)
}

predicciones_cv <- bind_rows(lapply(resultados_lista, function(x) x$predicciones))
hiperparametros <- bind_rows(lapply(resultados_lista, function(x) x$hiperparametros))
importancia_fold <- bind_rows(lapply(resultados_lista, function(x) x$importancia))

metricas <- calcular_metricas(predicciones_cv) %>%
  arrange(escenario, target, mae, rmse)

# ------------------------------------------------------------
# 8. Importancia agregada de variables
# ------------------------------------------------------------

importancia_global <- data.frame()
if (nrow(importancia_fold) > 0) {
  importancia_global <- importancia_fold %>%
    group_by(escenario, target, modelo, predictor, tipo_importancia) %>%
    summarise(
      n_folds_presente = n(),
      importancia_media = round(mean(importancia, na.rm = TRUE), 8),
      importancia_abs_media = round(mean(importancia_abs, na.rm = TRUE), 8),
      importancia_abs_mediana = round(stats::median(importancia_abs, na.rm = TRUE), 8),
      importancia_abs_sd = round(stats::sd(importancia_abs, na.rm = TRUE), 8),
      .groups = "drop"
    ) %>%
    group_by(escenario, target, modelo) %>%
    arrange(desc(importancia_abs_media), .by_group = TRUE) %>%
    mutate(ranking_importancia = row_number()) %>%
    ungroup()
}

# ------------------------------------------------------------
# 9. Comparacion contra modelos base
# ------------------------------------------------------------

comparacion_base <- data.frame()
metricas_base <- data.frame()

if (file.exists(ruta_modelo_base)) {
  hojas_base <- readxl::excel_sheets(ruta_modelo_base)
  if ("00_resumen_metricas" %in% hojas_base) {
    metricas_base <- readxl::read_excel(ruta_modelo_base, sheet = "00_resumen_metricas") %>%
      mutate(tipo_modelo = "base")

    metricas_small <- metricas %>% mutate(tipo_modelo = "small_data")

    comparacion_base <- bind_rows(
      metricas_base %>% select(any_of(names(metricas_small))),
      metricas_small
    ) %>%
      arrange(escenario, target, mae)

    # Referencias utiles para cada escenario-target.
    ref <- metricas_base %>%
      filter(modelo %in% c("media_entrenamiento", "lineal_reducido")) %>%
      select(escenario, target, modelo, mae, rmse) %>%
      pivot_wider(
        names_from = modelo,
        values_from = c(mae, rmse),
        names_sep = "_"
      )

    metricas <- metricas %>%
      left_join(ref, by = c("escenario", "target")) %>%
      mutate(
        mejora_mae_vs_media = ifelse(!is.na(mae_media_entrenamiento), round(mae_media_entrenamiento - mae, 6), NA_real_),
        mejora_mae_vs_lineal = ifelse(!is.na(mae_lineal_reducido), round(mae_lineal_reducido - mae, 6), NA_real_),
        lectura_comparativa = case_when(
          !is.na(mejora_mae_vs_lineal) & mejora_mae_vs_lineal > 0 ~ "mejora_frente_a_lineal_reducido",
          !is.na(mejora_mae_vs_media) & mejora_mae_vs_media > 0 ~ "mejora_solo_frente_a_media",
          TRUE ~ "no_mejora_frente_a_base"
        )
      )
  }
}

# ------------------------------------------------------------
# 10. Graficos
# ------------------------------------------------------------

ruta_grafico_mae <- file.path(dir_outputs_small_data, "01_modelos_small_data_mae_y_fenolico.png")
ruta_grafico_importancia <- file.path(dir_outputs_small_data, "02_importancia_variables_y_fenolico_M_pura.png")

if (nrow(metricas) > 0) {
  p_mae <- metricas %>%
    filter(target == "y_fenolico_comun") %>%
    ggplot(aes(x = escenario, y = mae, fill = modelo)) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(
      title = "MAE de modelos small data para y_fenolico_comun",
      subtitle = "Validacion cruzada agrupada cuando corresponde",
      x = "Escenario",
      y = "MAE"
    ) +
    theme_minimal(base_size = 11)

  ggsave(ruta_grafico_mae, p_mae, width = 10, height = 6, dpi = 300)
} else {
  ruta_grafico_mae <- NA_character_
}

if (nrow(importancia_global) > 0) {
  imp_plot <- importancia_global %>%
    filter(escenario == "M_pura", target == "y_fenolico_comun", ranking_importancia <= 10) %>%
    mutate(predictor = stringr::str_replace_all(predictor, "^x_", ""))

  if (nrow(imp_plot) > 0) {
    p_imp <- imp_plot %>%
      ggplot(aes(x = reorder(predictor, importancia_abs_media), y = importancia_abs_media, fill = modelo)) +
      geom_col(position = "dodge") +
      coord_flip() +
      labs(
        title = "Importancia de predictores en M_pura - y_fenolico_comun",
        subtitle = "Top 10 por modelo small data",
        x = "Predictor quimico",
        y = "Importancia absoluta media"
      ) +
      theme_minimal(base_size = 10)

    ggsave(ruta_grafico_importancia, p_imp, width = 10, height = 6, dpi = 300)
  } else {
    ruta_grafico_importancia <- NA_character_
  }
} else {
  ruta_grafico_importancia <- NA_character_
}

graficos_generados <- data.frame(
  tipo_grafico = c("barras_mae_modelos_small_data", "importancia_variables_M_pura"),
  ruta_grafico = c(ruta_grafico_mae, ruta_grafico_importancia),
  uso_recomendado = c(
    "Figura de comparacion de desempeno predictivo de modelos small data para el target principal.",
    "Figura de apoyo para interpretar compuestos relevantes en el escenario principal. Confirmar con bootstrap antes de usar como evidencia final."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 11. Configuracion y diccionario
# ------------------------------------------------------------

paquetes_estado <- data.frame(
  paquete = c(paquetes_obligatorios, paquetes_opcionales),
  tipo = c(rep("obligatorio", length(paquetes_obligatorios)), rep("opcional", length(paquetes_opcionales))),
  disponible = c(
    vapply(paquetes_obligatorios, requireNamespace, logical(1), quietly = TRUE),
    vapply(paquetes_opcionales, requireNamespace, logical(1), quietly = TRUE)
  ),
  impacto_si_falta = c(
    rep("el script no puede ejecutarse", length(paquetes_obligatorios)),
    "se omite Random Forest restringido",
    "se omite Gradient Boosting restringido"
  ),
  stringsAsFactors = FALSE
)

configuracion_modelos <- data.frame(
  modelo = c("ridge", "lasso", "elastic_net", "pls", "random_forest_restringido", "gradient_boosting_restringido"),
  familia = c("regularizado", "regularizado", "regularizado", "quimiometrico", "arboles", "boosting"),
  objetivo = c(
    "Controlar colinealidad mediante penalizacion L2.",
    "Seleccionar predictores mediante penalizacion L1.",
    "Combinar control de colinealidad y seleccion de variables.",
    "Reducir dimensionalidad latente en matriz quimica colineal.",
    "Explorar relaciones no lineales bajo restricciones de complejidad.",
    "Explorar no linealidad con arboles debiles y profundidad baja."
  ),
  restriccion_small_data = c(
    "Seleccion supervisada de predictores dentro de cada fold; lambda elegido por CV interna.",
    "Seleccion supervisada de predictores dentro de cada fold; lambda elegido por CV interna.",
    "Seleccion supervisada de predictores dentro de cada fold; lambda elegido por CV interna.",
    paste0("Maximo ", max_componentes_pls, " componentes; seleccion por LOO interna."),
    paste0("num.trees=", n_arboles_rf, ", max.depth=", max_depth_rf, ", predictores limitados a ", max_predictores_arboles, "."),
    paste0("n.trees=", n_arboles_gbm, ", depth=", interaction_depth_gbm, ", shrinkage=", shrinkage_gbm, ", predictores limitados a ", max_predictores_arboles, ".")
  ),
  stringsAsFactors = FALSE
)

diccionario <- data.frame(
  hoja = c(
    "00_resumen_metricas",
    "01_predicciones_cv",
    "02_targets_ejecutados",
    "03_configuracion_modelos",
    "04_hiperparametros_fold",
    "05_importancia_fold",
    "06_importancia_global",
    "07_comparacion_base",
    "08_graficos_generados",
    "09_diccionario"
  ),
  descripcion = c(
    "Metricas de validacion cruzada por escenario, target y modelo small data.",
    "Predicciones generadas en validacion cruzada agrupada cuando corresponde.",
    "Diagnostico de targets y decision de ejecucion.",
    "Configuracion metodologica de cada modelo.",
    "Hiperparametros seleccionados en cada fold.",
    "Importancia o coeficientes de variables por fold.",
    "Importancia agregada por escenario, target, modelo y predictor.",
    "Comparacion entre modelos base y modelos small data, si existe resultados_modelo_base.xlsx.",
    "Rutas de graficos generados.",
    "Definicion de hojas y criterios."
  ),
  criterio = c(
    "MAE es la metrica principal; RMSE, R2, Spearman y bias se usan como apoyo.",
    "La seleccion de predictores se realiza dentro de cada fold para evitar fuga de informacion.",
    "Se ejecutan targets principales y secundarios modelables definidos en diagnostico previo.",
    "Los modelos se restringen para evitar sobreajuste en small data.",
    "Ridge/Lasso/Elastic Net usan CV interna; PLS usa LOO interna; arboles usan parametros fijos conservadores.",
    "Las importancias no son evidencia final hasta evaluar estabilidad por bootstrap.",
    "Se usa como base para el script de interpretabilidad y bootstrap.",
    "La mejora frente al modelo base se calcula cuando el archivo base esta disponible.",
    "Graficos guardados en outputs/modelamiento/modelos_small_data.",
    "Archivo generado por scripts/R/modelamiento/15_modelos_small_data.R."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 12. Exportacion
# ------------------------------------------------------------

lista_salida <- list(
  "00_resumen_metricas" = metricas,
  "01_predicciones_cv" = predicciones_cv,
  "02_targets_ejecutados" = targets_diagnostico,
  "03_configuracion_modelos" = configuracion_modelos,
  "04_hiperparametros_fold" = hiperparametros,
  "05_importancia_fold" = importancia_fold,
  "06_importancia_global" = importancia_global,
  "07_comparacion_base" = comparacion_base,
  "08_graficos_generados" = graficos_generados,
  "09_diccionario" = diccionario,
  "10_paquetes" = paquetes_estado
)

guardar_excel_seguro(lista_salida, ruta_salida_excel)

cat("\nProceso terminado correctamente.\n")
cat("Archivo generado:\n", ruta_salida_excel, "\n")
cat("Graficos generados en:\n", dir_outputs_small_data, "\n")
cat("\nLectura clave:\n")
cat("\n- Ridge, Lasso, Elastic Net y PLS responden directamente a la colinealidad observada.")
cat("\n- Random Forest y Gradient Boosting se ejecutan con restricciones para reducir sobreajuste.")
cat("\n- Las importancias de variables deben confirmarse luego con bootstrap e interpretabilidad.\n")
