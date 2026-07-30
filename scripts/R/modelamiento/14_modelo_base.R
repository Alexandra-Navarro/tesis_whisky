# ============================================================
# 14_modelo_base.R
# Modelos base y VIF del modelo lineal reducido
# Tesis whisky chileno - small data
# ============================================================

# Este script implementa modelos base para cada escenario/target modelable:
# 1) modelo nulo: media del entrenamiento;
# 2) modelo lineal reducido: regresión lineal con predictores seleccionados por correlación.
# Además calcula el VIF del modelo lineal reducido, como diagnóstico complementario.
#
# La finalidad no es maximizar desempeño, sino establecer una línea base interpretable
# para comparar posteriormente contra Ridge, Lasso, Elastic Net, PLS, Random Forest
# y modelos small data.

# ------------------------------------------------------------
# 1. Configuración
# ------------------------------------------------------------

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

paquetes <- c("readxl", "dplyr", "tidyr", "stringr", "writexl", "ggplot2")
paquetes_faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)
library(ggplot2)

set.seed(123)

dir_modelamiento <- file.path(dir_data, "modelamiento")
dir_outputs <- file.path(dir_proyecto, "outputs")
dir_outputs_modelamiento <- file.path(dir_outputs, "modelamiento")
dir_outputs_modelo_base <- file.path(dir_outputs_modelamiento, "modelo_base")

dir.create(dir_modelamiento, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_outputs_modelo_base, recursive = TRUE, showWarnings = FALSE)

ruta_datos_modelamiento <- file.path(dir_modelamiento, "datos_modelamiento.xlsx")
ruta_salida_excel <- file.path(dir_modelamiento, "resultados_modelo_base.xlsx")

if (!file.exists(ruta_datos_modelamiento)) {
  stop(
    "No existe el archivo requerido: ", ruta_datos_modelamiento,
    "\nEjecuta primero scripts/R/modelamiento/12_preparar_datos_modelamiento.R"
  )
}

targets_candidatos <- c(
  "y_fenolico_comun",
  "y_frutal_comun",
  "y_ahumado_comun",
  "y_medicinal_comun"
)

max_predictores_lineal <- 3
min_n_target_modelable <- 8
min_n_correlacion <- 5
limite_inferior_target <- 0
limite_superior_target <- 5

# ------------------------------------------------------------
# 2. Funciones auxiliares
# ------------------------------------------------------------

guardar_excel_seguro <- function(lista_hojas, ruta) {
  intento <- tryCatch({
    writexl::write_xlsx(lista_hojas, ruta)
    TRUE
  }, error = function(e) FALSE)

  if (!intento) {
    ruta_alt <- sub("\\.xlsx$", paste0("_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"), ruta)
    writexl::write_xlsx(lista_hojas, ruta_alt)
    message("No se pudo sobrescribir el archivo. Se guardó una copia en: ", ruta_alt)
  }
}

convertir_numericamente <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(convertir_numero(x))
}

obtener_predictores_x <- function(df) {
  grep("^x_", names(df), value = TRUE)
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

preparar_df_modelo <- function(df, target) {
  cols_x <- obtener_predictores_x(df)
  cols_y <- intersect(target, names(df))

  if (length(cols_y) == 0) return(data.frame())

  df <- df %>%
    mutate(across(all_of(c(cols_x, target)), convertir_numericamente)) %>%
    filter(!is.na(.data[[target]]))

  df
}

resumir_target_escenario <- function(df, escenario, target) {
  if (!(target %in% names(df))) {
    return(data.frame())
  }

  valores <- convertir_numericamente(df[[target]])
  valores_validos <- valores[!is.na(valores)]

  data.frame(
    escenario = escenario,
    target = target,
    n_filas = nrow(df),
    n_no_na = length(valores_validos),
    n_valores_distintos = dplyr::n_distinct(valores_validos),
    desviacion = ifelse(length(valores_validos) > 1, stats::sd(valores_validos), NA_real_),
    ejecutar_modelo_base = length(valores_validos) >= min_n_target_modelable &&
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

seleccionar_predictores_lineales <- function(df_train, target, max_pred = max_predictores_lineal) {
  cols_x <- obtener_predictores_x(df_train)
  if (length(cols_x) == 0) return(character())

  y <- convertir_numericamente(df_train[[target]])

  resumen <- lapply(cols_x, function(x) {
    vx <- convertir_numericamente(df_train[[x]])
    completos <- !is.na(vx) & !is.na(y)
    n_comunes <- sum(completos)
    n_dist_x <- dplyr::n_distinct(vx[completos])
    n_dist_y <- dplyr::n_distinct(y[completos])

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
      n_distintos_predictor = n_dist_x,
      stringsAsFactors = FALSE
    )
  }) %>% bind_rows()

  resumen %>%
    filter(!is.na(abs_r_target)) %>%
    arrange(desc(abs_r_target), desc(n_comunes)) %>%
    slice_head(n = max_pred) %>%
    pull(predictor)
}

resumen_predictores_seleccionados <- function(df, escenario, target) {
  df_modelo <- preparar_df_modelo(df, target)
  pred <- seleccionar_predictores_lineales(df_modelo, target)

  if (length(pred) == 0) {
    return(data.frame(
      escenario = escenario,
      target = target,
      predictor = NA_character_,
      r_target = NA_real_,
      abs_r_target = NA_real_,
      n_comunes = NA_integer_,
      orden_seleccion = NA_integer_,
      stringsAsFactors = FALSE
    ))
  }

  y <- convertir_numericamente(df_modelo[[target]])
  bind_rows(lapply(seq_along(pred), function(i) {
    x <- pred[i]
    vx <- convertir_numericamente(df_modelo[[x]])
    completos <- !is.na(vx) & !is.na(y)
    r <- suppressWarnings(stats::cor(vx[completos], y[completos], method = "pearson"))
    data.frame(
      escenario = escenario,
      target = target,
      predictor = x,
      r_target = round(r, 6),
      abs_r_target = round(abs(r), 6),
      n_comunes = sum(completos),
      orden_seleccion = i,
      stringsAsFactors = FALSE
    )
  }))
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

recortar_prediccion <- function(x) {
  pmin(pmax(x, limite_inferior_target), limite_superior_target)
}

calcular_metricas_grupo <- function(df_m) {
  df_m <- df_m %>% filter(!is.na(y_obs), !is.na(y_pred))

  if (nrow(df_m) == 0) {
    return(data.frame())
  }

  error <- df_m$y_pred - df_m$y_obs
  sst <- sum((df_m$y_obs - mean(df_m$y_obs))^2)
  sse <- sum(error^2)
  r2 <- ifelse(sst > 0, 1 - sse / sst, NA_real_)
  modelo_actual <- unique(df_m$modelo)[1]
  spearman <- if (identical(modelo_actual, "media_entrenamiento")) {
    # El modelo nulo predice la media del entrenamiento de cada pliegue
    # excluyendo al grupo evaluado (leave-one-group-out): esa media es una
    # funcion monotona decreciente del valor excluido, por lo que al agrupar
    # las predicciones de todos los pliegues, la correlacion de rangos entre
    # y_obs e y_pred da -1.000 por construccion matematica, no porque el
    # modelo capture una asociacion negativa real. Se reporta como NA para
    # no sugerir una asociacion que no existe.
    NA_real_
  } else if (nrow(df_m) >= 3 && dplyr::n_distinct(df_m$y_pred) >= 2 && dplyr::n_distinct(df_m$y_obs) >= 2) {
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

calcular_vif_predictores <- function(df, escenario, target, predictores) {
  if (length(predictores) < 2) {
    return(data.frame(
      escenario = escenario,
      target = target,
      predictor = ifelse(length(predictores) == 1, predictores[1], NA_character_),
      vif = NA_real_,
      n_modelo = nrow(df),
      p_predictores = length(predictores),
      estado_vif = "no_calculable_menos_de_2_predictores",
      detalle_calculo = "modelo_lineal_reducido",
      stringsAsFactors = FALSE
    ))
  }

  x <- df[, predictores, drop = FALSE]
  x <- as.data.frame(lapply(x, convertir_numericamente))
  y <- convertir_numericamente(df[[target]])

  # Para que el VIF represente el modelo base efectivamente ajustado,
  # se calcula sobre la misma matriz preprocesada con imputación por mediana.
  for (p in predictores) {
    med <- suppressWarnings(stats::median(x[[p]], na.rm = TRUE))
    if (is.na(med) || is.infinite(med)) med <- 0
    x[[p]][is.na(x[[p]])] <- med
  }

  x <- x[!is.na(y), , drop = FALSE]

  if (nrow(x) <= length(predictores) + 2) {
    return(data.frame(
      escenario = escenario,
      target = target,
      predictor = predictores,
      vif = NA_real_,
      n_modelo = nrow(x),
      p_predictores = length(predictores),
      estado_vif = "no_calculable_n_insuficiente_para_p",
      detalle_calculo = "modelo_lineal_reducido_imputado_mediana",
      stringsAsFactors = FALSE
    ))
  }

  bind_rows(lapply(predictores, function(pred) {
    otros <- setdiff(predictores, pred)
    formula_vif <- stats::as.formula(paste(pred, "~", paste(otros, collapse = " + ")))

    r2 <- tryCatch({
      fit <- stats::lm(formula_vif, data = x)
      summary(fit)$r.squared
    }, error = function(e) NA_real_)

    vif <- if (is.na(r2)) NA_real_ else if (r2 >= 0.999999) Inf else 1 / (1 - r2)

    estado <- case_when(
      is.infinite(vif) ~ "vif_infinito_colinealidad_perfecta",
      is.na(vif) ~ "vif_no_calculable",
      vif >= 10 ~ "vif_muy_alto",
      vif >= 5 ~ "vif_alto",
      TRUE ~ "vif_aceptable"
    )

    data.frame(
      escenario = escenario,
      target = target,
      predictor = pred,
      vif = round(vif, 6),
      n_modelo = nrow(x),
      p_predictores = length(predictores),
      estado_vif = estado,
      detalle_calculo = "modelo_lineal_reducido_imputado_mediana",
      stringsAsFactors = FALSE
    )
  }))
}

ajustar_coeficientes_lineal <- function(df, escenario, target, predictores) {
  if (length(predictores) == 0) return(data.frame())

  df_modelo <- df %>% filter(!is.na(.data[[target]]))
  imputado <- imputar_con_mediana_train(df_modelo, df_modelo, predictores)
  df_fit <- imputado$train

  formula_lm <- stats::as.formula(paste(target, "~", paste(predictores, collapse = " + ")))

  fit <- tryCatch(stats::lm(formula_lm, data = df_fit), error = function(e) NULL)
  if (is.null(fit)) return(data.frame())

  coef <- summary(fit)$coefficients
  salida <- as.data.frame(coef, stringsAsFactors = FALSE)
  salida$termino <- rownames(coef)
  rownames(salida) <- NULL

  names(salida) <- c("estimacion", "error_estandar", "t_value", "p_value", "termino")

  salida %>%
    mutate(
      escenario = escenario,
      target = target,
      modelo = "lineal_reducido",
      .before = 1
    ) %>%
    select(escenario, target, modelo, termino, estimacion, error_estandar, t_value, p_value)
}

validar_modelos_base <- function(df, escenario, target) {
  df_modelo <- preparar_df_modelo(df, target)

  if (nrow(df_modelo) < min_n_target_modelable || dplyr::n_distinct(df_modelo[[target]]) < 2) {
    return(list(predicciones = data.frame(), predictores = data.frame(), vif = data.frame(), coeficientes = data.frame()))
  }

  df_modelo$grupo_validacion <- obtener_id_validacion(df_modelo, escenario)
  grupos <- unique(df_modelo$grupo_validacion)

  predicciones <- list()
  k <- 1

  for (g in grupos) {
    train <- df_modelo %>% filter(grupo_validacion != g)
    test <- df_modelo %>% filter(grupo_validacion == g)

    if (nrow(train) < 3 || nrow(test) == 0) next

    # Modelo nulo: media del entrenamiento.
    media_train <- mean(train[[target]], na.rm = TRUE)
    predicciones[[k]] <- data.frame(
      escenario = escenario,
      target = target,
      modelo = "media_entrenamiento",
      grupo_validacion = test$grupo_validacion,
      unidad_analitica_id = if ("unidad_analitica_id" %in% names(test)) as.character(test$unidad_analitica_id) else NA_character_,
      muestra_base = if ("muestra_base" %in% names(test)) as.character(test$muestra_base) else NA_character_,
      y_obs = test[[target]],
      y_pred_raw = media_train,
      y_pred = recortar_prediccion(media_train),
      n_train = nrow(train),
      predictores_usados = "ninguno",
      stringsAsFactors = FALSE
    )
    k <- k + 1

    # Modelo lineal reducido con selección dentro del fold.
    predictores <- seleccionar_predictores_lineales(train, target, max_predictores_lineal)

    if (length(predictores) == 0) {
      pred_lineal <- rep(media_train, nrow(test))
      predictores_txt <- "sin_predictores_validos; usa_media"
    } else {
      imputado <- imputar_con_mediana_train(train, test, predictores)
      train_imp <- imputado$train
      test_imp <- imputado$test
      formula_lm <- stats::as.formula(paste(target, "~", paste(predictores, collapse = " + ")))

      fit <- tryCatch(stats::lm(formula_lm, data = train_imp), error = function(e) NULL)

      if (is.null(fit)) {
        pred_lineal <- rep(media_train, nrow(test))
        predictores_txt <- "fallo_lm; usa_media"
      } else {
        pred_lineal <- tryCatch(as.numeric(stats::predict(fit, newdata = test_imp)), error = function(e) rep(media_train, nrow(test)))
        predictores_txt <- paste(predictores, collapse = "; ")
      }
    }

    predicciones[[k]] <- data.frame(
      escenario = escenario,
      target = target,
      modelo = "lineal_reducido",
      grupo_validacion = test$grupo_validacion,
      unidad_analitica_id = if ("unidad_analitica_id" %in% names(test)) as.character(test$unidad_analitica_id) else NA_character_,
      muestra_base = if ("muestra_base" %in% names(test)) as.character(test$muestra_base) else NA_character_,
      y_obs = test[[target]],
      y_pred_raw = pred_lineal,
      y_pred = recortar_prediccion(pred_lineal),
      n_train = nrow(train),
      predictores_usados = predictores_txt,
      stringsAsFactors = FALSE
    )
    k <- k + 1
  }

  pred_df <- bind_rows(predicciones)

  predictores_full <- resumen_predictores_seleccionados(df_modelo, escenario, target)
  pred_full <- predictores_full$predictor[!is.na(predictores_full$predictor)]
  vif_full <- calcular_vif_predictores(df_modelo, escenario, target, pred_full)
  coef_full <- ajustar_coeficientes_lineal(df_modelo, escenario, target, pred_full)

  list(
    predicciones = pred_df,
    predictores = predictores_full,
    vif = vif_full,
    coeficientes = coef_full
  )
}

# ------------------------------------------------------------
# 3. Lectura de escenarios
# ------------------------------------------------------------

escenarios <- data.frame(
  escenario = c("M_pura", "M_expandida_evaluador", "M_cata_individual", "M_ia_exploratoria"),
  hoja = c("01_pura", "02_expandida_evaluador", "04_sensibilidad_con_ju", "05_ia_exploratoria"),
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

# ------------------------------------------------------------
# 4. Diagnóstico de targets a ejecutar
# ------------------------------------------------------------

targets_diagnostico <- bind_rows(lapply(names(datos_escenarios), function(esc) {
  df <- datos_escenarios[[esc]]
  bind_rows(lapply(targets_candidatos, function(t) resumir_target_escenario(df, esc, t)))
})) %>%
  mutate(
    decision = case_when(
      ejecutar_modelo_base & target == "y_fenolico_comun" ~ "ejecutar_principal",
      ejecutar_modelo_base & target == "y_frutal_comun" ~ "ejecutar_exploratorio",
      ejecutar_modelo_base ~ "ejecutar_solo_si_se_requiere",
      TRUE ~ "no_ejecutar"
    )
  )

# Por ahora se ejecutan los targets modelables principales y secundarios.
targets_a_ejecutar <- targets_diagnostico %>%
  filter(decision %in% c("ejecutar_principal", "ejecutar_exploratorio"))

# ------------------------------------------------------------
# 5. Ajuste y validación de modelos base
# ------------------------------------------------------------

resultados_lista <- list()
for (i in seq_len(nrow(targets_a_ejecutar))) {
  esc <- targets_a_ejecutar$escenario[i]
  target <- targets_a_ejecutar$target[i]
  resultados_lista[[paste(esc, target, sep = "__")]] <- validar_modelos_base(datos_escenarios[[esc]], esc, target)
}

predicciones_cv <- bind_rows(lapply(resultados_lista, function(x) x$predicciones))
predictores_lineal <- bind_rows(lapply(resultados_lista, function(x) x$predictores))
vif_modelo_base <- bind_rows(lapply(resultados_lista, function(x) x$vif))
coeficientes_lineal <- bind_rows(lapply(resultados_lista, function(x) x$coeficientes))

metricas <- calcular_metricas(predicciones_cv) %>%
  arrange(escenario, target, modelo)

# Añadir lectura metodológica comparando contra la media del entrenamiento.
metricas_media <- metricas %>%
  filter(modelo == "media_entrenamiento") %>%
  transmute(escenario, target, mae_media = mae, rmse_media = rmse)

metricas <- metricas %>%
  left_join(metricas_media, by = c("escenario", "target")) %>%
  mutate(
    mejora_mae_vs_media = ifelse(!is.na(mae_media), round(mae_media - mae, 6), NA_real_),
    mejora_rmse_vs_media = ifelse(!is.na(rmse_media), round(rmse_media - rmse, 6), NA_real_),
    lectura = case_when(
      modelo == "media_entrenamiento" ~ "linea_base_minima; cualquier_modelo_debe_superar_este_error",
      modelo == "lineal_reducido" & !is.na(mejora_mae_vs_media) & mejora_mae_vs_media > 0 ~ "mejora_frente_a_media; interpretar_con_cautela_por_small_data_y_colinealidad",
      modelo == "lineal_reducido" ~ "no_mejora_claramente_frente_a_media_o_mejora_marginal",
      TRUE ~ ""
    )
  )

# ------------------------------------------------------------
# 6. Gráfico resumen de desempeño base
# ------------------------------------------------------------

ruta_grafico_metricas <- file.path(dir_outputs_modelo_base, "01_modelo_base_mae_por_escenario.png")

if (nrow(metricas) > 0) {
  p_metricas <- metricas %>%
    filter(target == "y_fenolico_comun") %>%
    ggplot(aes(x = escenario, y = mae, fill = modelo)) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(
      title = "MAE de modelos base para y_fenolico_comun",
      subtitle = "Validación cruzada agrupada cuando corresponde",
      x = "Escenario",
      y = "MAE"
    ) +
    theme_minimal(base_size = 11)

  ggsave(ruta_grafico_metricas, p_metricas, width = 9, height = 5, dpi = 300)
} else {
  ruta_grafico_metricas <- NA_character_
}

graficos_generados <- data.frame(
  tipo_grafico = "barras_mae_modelo_base",
  ruta_grafico = ruta_grafico_metricas,
  uso_recomendado = "Figura de apoyo para comparar modelo nulo versus regresión lineal reducida en el target principal.",
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 7. Diccionario y exportación
# ------------------------------------------------------------

diccionario <- data.frame(
  hoja = c(
    "00_resumen_metricas",
    "01_predicciones_cv",
    "02_targets_ejecutados",
    "03_predictores_lineal",
    "04_coeficientes_lineal",
    "05_vif_modelo_base",
    "06_graficos_generados",
    "07_diccionario"
  ),
  descripcion = c(
    "Métricas de validación de modelos base por escenario, target y modelo.",
    "Predicciones obtenidas mediante validación cruzada. En M_expandida_evaluador se agrupa por unidad química.",
    "Diagnóstico de targets y decisión de ejecución.",
    "Predictores seleccionados para el modelo lineal reducido completo.",
    "Coeficientes del modelo lineal reducido ajustado sobre todos los datos disponibles del escenario-target.",
    "VIF del modelo lineal reducido. Se calcula sobre la matriz preprocesada con imputación por mediana, porque ese es el dato usado por el modelo base.",
    "Ruta del gráfico de MAE generado.",
    "Definición de hojas y criterios metodológicos."
  ),
  criterio = c(
    "Métricas: MAE, RMSE, R2, Spearman y bias. MAE es la métrica principal.",
    "La selección de predictores del modelo lineal se realiza dentro de cada fold para evitar fuga de información en la validación.",
    "Se ejecutan principalmente y_fenolico_comun y y_frutal_comun cuando son modelables.",
    paste0("Se seleccionan hasta ", max_predictores_lineal, " predictores por correlación absoluta con el target."),
    "Coeficientes interpretables solo como línea base; no como evidencia definitiva de compuestos influyentes.",
    "VIF > 5 indica colinealidad alta; VIF > 10 indica colinealidad severa.",
    "Gráfico guardado en outputs/modelamiento/modelo_base.",
    "Archivo generado por scripts/R/modelamiento/14_modelo_base.R."
  ),
  stringsAsFactors = FALSE
)

lista_salida <- list(
  "00_resumen_metricas" = metricas,
  "01_predicciones_cv" = predicciones_cv,
  "02_targets_ejecutados" = targets_diagnostico,
  "03_predictores_lineal" = predictores_lineal,
  "04_coeficientes_lineal" = coeficientes_lineal,
  "05_vif_modelo_base" = vif_modelo_base,
  "06_graficos_generados" = graficos_generados,
  "07_diccionario" = diccionario
)

guardar_excel_seguro(lista_salida, ruta_salida_excel)

cat("\nProceso terminado correctamente.\n")
cat("Archivo generado:\n", ruta_salida_excel, "\n")
cat("Gráficos generados en:\n", dir_outputs_modelo_base, "\n")
cat("\nLectura clave:\n")
cat("\n- El modelo de media es la línea base mínima.")
cat("\n- La regresión lineal reducida es una línea base interpretable, no el modelo principal.")
cat("\n- El VIF del modelo base se calcula sobre los predictores realmente usados por la regresión lineal reducida.\n")
