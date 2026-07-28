# ============================================================
# 17_residuos_diagnostico.R
# Diagnostico de residuos para modelos base y small data
# Tesis whisky chileno - modelamiento quimico-sensorial
# ============================================================

# Este script consolida predicciones de:
# - 14_modelo_base.R
# - 15_modelos_small_data.R
#
# Objetivo:
# - Evaluar residuos de los modelos base y modelos supervisados.
# - Separar diagnostico de supuestos lineales de diagnostico predictivo.
# - Generar tablas y figuras para tesis/anexos.
#
# Nota metodologica:
# - Normalidad/homocedasticidad se interpreta principalmente en modelos lineales.
# - Para PLS, Random Forest y Gradient Boosting se revisa error predictivo,
#   sesgo, residuos vs predicho y observados vs predichos, no supuestos OLS estrictos.

# ------------------------------------------------------------
# 1. Configuracion
# ------------------------------------------------------------

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

paquetes_obligatorios <- c(
  "readxl", "dplyr", "tidyr", "stringr", "writexl", "ggplot2"
)

paquetes_faltantes <- paquetes_obligatorios[
  !vapply(paquetes_obligatorios, requireNamespace, logical(1), quietly = TRUE)
]

if (length(paquetes_faltantes) > 0) {
  stop(
    "Faltan paquetes obligatorios: ", paste(paquetes_faltantes, collapse = ", "),
    "\nInstalalos manualmente con: install.packages(c('",
    paste(paquetes_faltantes, collapse = "', '"),
    "'))"
  )
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)
library(ggplot2)

set.seed(123)

# Rutas

dir_modelamiento <- file.path(dir_data, "modelamiento")
dir_outputs <- file.path(dir_proyecto, "outputs")
dir_outputs_modelamiento <- file.path(dir_outputs, "modelamiento")
dir_outputs_residuos <- file.path(dir_outputs_modelamiento, "residuos_diagnostico")

dir.create(dir_modelamiento, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_outputs_residuos, recursive = TRUE, showWarnings = FALSE)

ruta_modelo_base <- file.path(dir_modelamiento, "resultados_modelo_base.xlsx")
ruta_modelos_small <- file.path(dir_modelamiento, "resultados_modelos_small_data.xlsx")
ruta_salida_excel <- file.path(dir_modelamiento, "residuos_diagnostico.xlsx")

if (!file.exists(ruta_modelo_base)) {
  stop(
    "No existe el archivo requerido: ", ruta_modelo_base,
    "\nEjecuta primero scripts/R/modelamiento/14_modelo_base.R"
  )
}

if (!file.exists(ruta_modelos_small)) {
  stop(
    "No existe el archivo requerido: ", ruta_modelos_small,
    "\nEjecuta primero scripts/R/modelamiento/15_modelos_small_data.R"
  )
}

# Modelos y targets prioritarios para graficos.
modelos_prioritarios <- c(
  "media_entrenamiento",
  "lineal_reducido",
  "ridge",
  "lasso",
  "elastic_net",
  "pls",
  "random_forest_restringido",
  "gradient_boosting_restringido"
)

target_principal <- "y_fenolico_comun"
escenario_principal <- "M_pura"

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
    message("No se pudo sobrescribir el archivo. Se guardo una copia en: ", ruta_alt)
  }
}

leer_hoja_si_existe <- function(ruta, hoja) {
  hojas <- readxl::excel_sheets(ruta)
  if (!hoja %in% hojas) return(data.frame())
  readxl::read_excel(ruta, sheet = hoja) %>% as.data.frame()
}

convertir_numericamente <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
}

clasificar_tipo_modelo <- function(modelo) {
  dplyr::case_when(
    modelo %in% c("media_entrenamiento") ~ "linea_base_nula",
    modelo %in% c("lineal_reducido") ~ "modelo_base_lineal",
    modelo %in% c("ridge", "lasso", "elastic_net") ~ "regularizado_lineal",
    modelo %in% c("pls") ~ "quimiometrico",
    modelo %in% c("random_forest_restringido", "gradient_boosting_restringido") ~ "arboles_restringidos",
    TRUE ~ "otro"
  )
}

clasificar_diagnostico_residuos <- function(modelo) {
  dplyr::case_when(
    modelo %in% c("lineal_reducido", "ridge", "lasso", "elastic_net") ~ "diagnostico_lineal_aplicable_con_cautela",
    modelo %in% c("pls") ~ "diagnostico_predictivo_quimiometrico",
    modelo %in% c("random_forest_restringido", "gradient_boosting_restringido") ~ "diagnostico_predictivo_no_lineal",
    modelo %in% c("media_entrenamiento") ~ "linea_base_sin_supuestos_modelo",
    TRUE ~ "diagnostico_descriptivo"
  )
}

calcular_metricas_residuales <- function(df) {
  y_obs <- convertir_numericamente(df$y_obs)
  y_pred <- convertir_numericamente(df$y_pred)
  resid <- y_obs - y_pred

  completos <- !is.na(y_obs) & !is.na(y_pred) & !is.na(resid)
  y_obs <- y_obs[completos]
  y_pred <- y_pred[completos]
  resid <- resid[completos]

  n <- length(resid)
  if (n == 0) {
    return(data.frame(
      n = 0,
      bias = NA_real_,
      mae = NA_real_,
      rmse = NA_real_,
      residuo_sd = NA_real_,
      residuo_mediana = NA_real_,
      residuo_p05 = NA_real_,
      residuo_p95 = NA_real_,
      max_abs_residuo = NA_real_,
      r_pred_obs = NA_real_,
      spearman_pred_obs = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  r_pred_obs <- if (n >= 3 && dplyr::n_distinct(y_pred) >= 2 && dplyr::n_distinct(y_obs) >= 2) {
    suppressWarnings(stats::cor(y_obs, y_pred, method = "pearson"))
  } else {
    NA_real_
  }

  spearman_pred_obs <- if (n >= 3 && dplyr::n_distinct(y_pred) >= 2 && dplyr::n_distinct(y_obs) >= 2) {
    suppressWarnings(stats::cor(y_obs, y_pred, method = "spearman"))
  } else {
    NA_real_
  }

  data.frame(
    n = n,
    bias = mean(resid, na.rm = TRUE),
    mae = mean(abs(resid), na.rm = TRUE),
    rmse = sqrt(mean(resid^2, na.rm = TRUE)),
    residuo_sd = stats::sd(resid, na.rm = TRUE),
    residuo_mediana = stats::median(resid, na.rm = TRUE),
    residuo_p05 = as.numeric(stats::quantile(resid, 0.05, na.rm = TRUE, names = FALSE)),
    residuo_p95 = as.numeric(stats::quantile(resid, 0.95, na.rm = TRUE, names = FALSE)),
    max_abs_residuo = max(abs(resid), na.rm = TRUE),
    r_pred_obs = r_pred_obs,
    spearman_pred_obs = spearman_pred_obs,
    stringsAsFactors = FALSE
  )
}

calcular_diagnostico_supuestos <- function(df) {
  resid <- convertir_numericamente(df$residuo)
  y_pred <- convertir_numericamente(df$y_pred)
  resid <- resid[!is.na(resid) & !is.na(y_pred)]
  y_pred <- y_pred[!is.na(convertir_numericamente(df$residuo)) & !is.na(convertir_numericamente(df$y_pred))]
  n <- length(resid)

  shapiro_p <- NA_real_
  if (n >= 3 && n <= 5000 && dplyr::n_distinct(resid) >= 3) {
    shapiro_p <- tryCatch(stats::shapiro.test(resid)$p.value, error = function(e) NA_real_)
  }

  cor_abs_resid_pred <- NA_real_
  cor_abs_resid_pred_p <- NA_real_
  if (n >= 5 && dplyr::n_distinct(y_pred) >= 2 && dplyr::n_distinct(abs(resid)) >= 2) {
    test_cor <- tryCatch(
      stats::cor.test(abs(resid), y_pred, method = "spearman", exact = FALSE),
      error = function(e) NULL
    )
    if (!is.null(test_cor)) {
      cor_abs_resid_pred <- as.numeric(test_cor$estimate)
      cor_abs_resid_pred_p <- as.numeric(test_cor$p.value)
    }
  }

  data.frame(
    n = n,
    shapiro_p = shapiro_p,
    normalidad_residuos = case_when(
      is.na(shapiro_p) ~ "no_evaluable",
      shapiro_p >= 0.05 ~ "sin_evidencia_fuerte_no_normalidad",
      TRUE ~ "posible_desviacion_normalidad"
    ),
    cor_abs_resid_pred = cor_abs_resid_pred,
    cor_abs_resid_pred_p = cor_abs_resid_pred_p,
    homocedasticidad_proxy = case_when(
      is.na(cor_abs_resid_pred) ~ "no_evaluable",
      abs(cor_abs_resid_pred) < 0.30 ~ "sin_indicio_fuerte_heterocedasticidad",
      abs(cor_abs_resid_pred) < 0.50 ~ "indicio_moderado_revisar",
      TRUE ~ "indicio_alto_revisar"
    ),
    stringsAsFactors = FALSE
  )
}

crear_residuos <- function(predicciones) {
  if (nrow(predicciones) == 0) return(data.frame())

  predicciones %>%
    mutate(
      y_obs = convertir_numericamente(y_obs),
      y_pred = convertir_numericamente(y_pred),
      y_pred_raw = if ("y_pred_raw" %in% names(.)) convertir_numericamente(y_pred_raw) else y_pred,
      residuo = y_obs - y_pred,
      residuo_abs = abs(residuo),
      residuo_cuadrado = residuo^2,
      tipo_modelo = clasificar_tipo_modelo(modelo),
      tipo_diagnostico = clasificar_diagnostico_residuos(modelo)
    ) %>%
    group_by(escenario, target, modelo) %>%
    mutate(
      residuo_z = ifelse(
        stats::sd(residuo, na.rm = TRUE) > 0,
        as.numeric(scale(residuo)),
        NA_real_
      ),
      atipico_residual = !is.na(residuo_z) & abs(residuo_z) >= 2
    ) %>%
    ungroup()
}

# ------------------------------------------------------------
# 3. Lectura y consolidacion de predicciones
# ------------------------------------------------------------

pred_base <- leer_hoja_si_existe(ruta_modelo_base, "01_predicciones_cv")
pred_small <- leer_hoja_si_existe(ruta_modelos_small, "01_predicciones_cv")

if (nrow(pred_base) == 0) {
  warning("No se encontro hoja 01_predicciones_cv en resultados_modelo_base.xlsx")
}
if (nrow(pred_small) == 0) {
  warning("No se encontro hoja 01_predicciones_cv en resultados_modelos_small_data.xlsx")
}

pred_base <- pred_base %>% mutate(fuente_modelo = "modelo_base")
pred_small <- pred_small %>% mutate(fuente_modelo = "modelos_small_data")

predicciones <- bind_rows(pred_base, pred_small) %>%
  filter(!is.na(y_obs), !is.na(y_pred)) %>%
  mutate(
    modelo = as.character(modelo),
    escenario = as.character(escenario),
    target = as.character(target)
  ) %>%
  filter(modelo %in% modelos_prioritarios)

residuos_cv <- crear_residuos(predicciones)

# ------------------------------------------------------------
# 4. Resumen de residuos y supuestos
# ------------------------------------------------------------

resumen_residuos <- data.frame()
if (nrow(residuos_cv) > 0) {
  claves <- residuos_cv %>%
    distinct(escenario, target, modelo, fuente_modelo, tipo_modelo, tipo_diagnostico) %>%
    arrange(escenario, target, modelo)

  resumen_residuos <- bind_rows(lapply(seq_len(nrow(claves)), function(i) {
    df_i <- residuos_cv %>%
      filter(
        escenario == claves$escenario[i],
        target == claves$target[i],
        modelo == claves$modelo[i]
      )
    met <- calcular_metricas_residuales(df_i)
    bind_cols(claves[i, ], met)
  })) %>%
    mutate(
      evaluacion_residuos = case_when(
        is.na(mae) ~ "sin_datos",
        tipo_modelo %in% c("arboles_restringidos") & abs(bias) <= 0.20 ~ "diagnostico_predictivo_aceptable_revisar_estabilidad",
        tipo_modelo %in% c("quimiometrico", "regularizado_lineal") & abs(bias) <= 0.20 ~ "residuos_aceptables_con_cautela",
        tipo_modelo %in% c("modelo_base_lineal") & abs(bias) <= 0.20 ~ "base_lineal_aceptable_como_referencia",
        TRUE ~ "revisar_sesgo_o_dispersion"
      )
    )
}

diagnostico_supuestos <- data.frame()
if (nrow(residuos_cv) > 0) {
  claves <- residuos_cv %>%
    distinct(escenario, target, modelo, tipo_modelo, tipo_diagnostico) %>%
    arrange(escenario, target, modelo)

  diagnostico_supuestos <- bind_rows(lapply(seq_len(nrow(claves)), function(i) {
    df_i <- residuos_cv %>%
      filter(
        escenario == claves$escenario[i],
        target == claves$target[i],
        modelo == claves$modelo[i]
      )
    diag <- calcular_diagnostico_supuestos(df_i)
    bind_cols(claves[i, ], diag)
  })) %>%
    mutate(
      interpretacion = case_when(
        tipo_diagnostico == "diagnostico_predictivo_no_lineal" ~ "No aplicar supuestos OLS estrictos; usar como diagnostico de error predictivo.",
        tipo_diagnostico == "diagnostico_predictivo_quimiometrico" ~ "Revisar patron de residuos y predicho vs observado; no interpretar como OLS clasico.",
        tipo_diagnostico == "linea_base_sin_supuestos_modelo" ~ "Linea base nula; usar solo como referencia de error.",
        normalidad_residuos == "posible_desviacion_normalidad" | homocedasticidad_proxy == "indicio_alto_revisar" ~ "Revisar supuestos antes de interpretar coeficientes.",
        TRUE ~ "Sin alerta fuerte en diagnostico residual descriptivo."
      )
    )
}

residuos_atipicos <- data.frame()
if (nrow(residuos_cv) > 0) {
  residuos_atipicos <- residuos_cv %>%
    filter(atipico_residual | residuo_abs >= stats::quantile(residuo_abs, 0.95, na.rm = TRUE)) %>%
    arrange(escenario, target, modelo, desc(residuo_abs)) %>%
    select(
      escenario, target, modelo, fuente_modelo, tipo_modelo,
      grupo_validacion, unidad_analitica_id, muestra_base,
      y_obs, y_pred, residuo, residuo_abs, residuo_z,
      atipico_residual
    )
}

modelos_revisados <- residuos_cv %>%
  distinct(escenario, target, modelo, fuente_modelo, tipo_modelo, tipo_diagnostico) %>%
  arrange(escenario, target, modelo) %>%
  mutate(
    uso_en_tesis = case_when(
      modelo == "media_entrenamiento" ~ "linea_base_minima",
      modelo == "lineal_reducido" ~ "modelo_base_comparativo_y_diagnostico_lineal",
      modelo %in% c("ridge", "lasso", "elastic_net") ~ "modelo_regularizado_para_colinealidad",
      modelo == "pls" ~ "modelo_quimiometrico_interpretable",
      modelo == "random_forest_restringido" ~ "modelo_predictivo_no_lineal_principal_si_bootstrap_lo_confirma",
      modelo == "gradient_boosting_restringido" ~ "modelo_no_lineal_secundario",
      TRUE ~ "otro"
    )
  )

# ------------------------------------------------------------
# 5. Graficos
# ------------------------------------------------------------

graficos_generados <- list()

# 5.1 Predicho vs observado para escenario principal y target fenolico.
datos_graf_principal <- residuos_cv %>%
  filter(
    escenario == escenario_principal,
    target == target_principal,
    modelo %in% c("lineal_reducido", "pls", "lasso", "ridge", "random_forest_restringido")
  ) %>%
  mutate(modelo = factor(modelo, levels = c("lineal_reducido", "ridge", "lasso", "pls", "random_forest_restringido")))

if (nrow(datos_graf_principal) > 0) {
  p_pred_obs <- ggplot(datos_graf_principal, aes(x = y_obs, y = y_pred)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_point(alpha = 0.75, size = 2) +
    facet_wrap(~ modelo, nrow = 1) +
    coord_equal(xlim = c(0, 5), ylim = c(0, 5)) +
    labs(
      title = "Predicho vs observado en M_pura - y_fenolico_comun",
      subtitle = "Linea diagonal: prediccion perfecta",
      x = "Valor observado",
      y = "Valor predicho"
    ) +
    theme_minimal(base_size = 11)

  ruta_graf <- file.path(dir_outputs_residuos, "01_predicho_vs_observado_M_pura_y_fenolico.png")
  ggsave(ruta_graf, p_pred_obs, width = 11, height = 4.5, dpi = 300)
  graficos_generados[[length(graficos_generados) + 1]] <- data.frame(
    grafico = "01_predicho_vs_observado_M_pura_y_fenolico",
    ruta = ruta_graf,
    descripcion = "Comparacion de valores observados y predichos para modelos seleccionados en matriz pura.",
    uso_recomendado = "cuerpo_o_anexo",
    stringsAsFactors = FALSE
  )
}

# 5.2 Residuos vs predicho.
if (nrow(datos_graf_principal) > 0) {
  p_res_pred <- ggplot(datos_graf_principal, aes(x = y_pred, y = residuo)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_point(alpha = 0.75, size = 2) +
    facet_wrap(~ modelo, nrow = 1) +
    labs(
      title = "Residuos vs predicho en M_pura - y_fenolico_comun",
      subtitle = "Permite detectar sesgo o patrones sistematicos de error",
      x = "Valor predicho",
      y = "Residuo observado - predicho"
    ) +
    theme_minimal(base_size = 11)

  ruta_graf <- file.path(dir_outputs_residuos, "02_residuos_vs_predicho_M_pura_y_fenolico.png")
  ggsave(ruta_graf, p_res_pred, width = 11, height = 4.5, dpi = 300)
  graficos_generados[[length(graficos_generados) + 1]] <- data.frame(
    grafico = "02_residuos_vs_predicho_M_pura_y_fenolico",
    ruta = ruta_graf,
    descripcion = "Grafico de residuos contra predicciones para modelos seleccionados en matriz pura.",
    uso_recomendado = "cuerpo_o_anexo",
    stringsAsFactors = FALSE
  )
}

# 5.3 QQ plot para modelo base lineal y PLS, solo diagnostico descriptivo.
datos_qq <- residuos_cv %>%
  filter(
    escenario == escenario_principal,
    target == target_principal,
    modelo %in% c("lineal_reducido", "pls", "random_forest_restringido")
  ) %>%
  mutate(modelo = factor(modelo, levels = c("lineal_reducido", "pls", "random_forest_restringido")))

if (nrow(datos_qq) > 0) {
  p_qq <- ggplot(datos_qq, aes(sample = residuo)) +
    stat_qq(alpha = 0.75, size = 1.8) +
    stat_qq_line(linewidth = 0.4) +
    facet_wrap(~ modelo, nrow = 1, scales = "free") +
    labs(
      title = "QQ plot de residuos en M_pura - y_fenolico_comun",
      subtitle = "Normalidad aplica principalmente al modelo lineal; en RF es descriptivo",
      x = "Cuantiles teoricos",
      y = "Cuantiles de residuos"
    ) +
    theme_minimal(base_size = 11)

  ruta_graf <- file.path(dir_outputs_residuos, "03_qqplot_residuos_M_pura_y_fenolico.png")
  ggsave(ruta_graf, p_qq, width = 10, height = 4.5, dpi = 300)
  graficos_generados[[length(graficos_generados) + 1]] <- data.frame(
    grafico = "03_qqplot_residuos_M_pura_y_fenolico",
    ruta = ruta_graf,
    descripcion = "QQ plot de residuos para modelo lineal, PLS y Random Forest en el escenario principal.",
    uso_recomendado = "anexo",
    stringsAsFactors = FALSE
  )
}

# 5.4 Distribucion de residuos por escenario para mejores modelos bootstrap/preseleccionados.
datos_box <- residuos_cv %>%
  filter(
    target == target_principal,
    modelo %in% c("random_forest_restringido", "pls", "ridge", "lineal_reducido")
  )

if (nrow(datos_box) > 0) {
  p_box <- ggplot(datos_box, aes(x = modelo, y = residuo, fill = modelo)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_boxplot(alpha = 0.75, outlier.alpha = 0.55) +
    facet_wrap(~ escenario, scales = "free_y") +
    labs(
      title = "Distribucion de residuos para y_fenolico_comun",
      subtitle = "Comparacion entre modelo base, PLS, Ridge y Random Forest",
      x = "Modelo",
      y = "Residuo observado - predicho"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "none")

  ruta_graf <- file.path(dir_outputs_residuos, "04_distribucion_residuos_y_fenolico.png")
  ggsave(ruta_graf, p_box, width = 11, height = 6, dpi = 300)
  graficos_generados[[length(graficos_generados) + 1]] <- data.frame(
    grafico = "04_distribucion_residuos_y_fenolico",
    ruta = ruta_graf,
    descripcion = "Distribucion de residuos por escenario para modelos seleccionados del target fenolico.",
    uso_recomendado = "anexo",
    stringsAsFactors = FALSE
  )
}

graficos_generados <- bind_rows(graficos_generados)
if (nrow(graficos_generados) == 0) {
  graficos_generados <- data.frame(
    grafico = character(),
    ruta = character(),
    descripcion = character(),
    uso_recomendado = character(),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 5b. Cruce con outliers detectados en el EDA quimico (T2/Q, Dixon)
# ------------------------------------------------------------
# El script 10_analisis_quimico.R (exploratorio) ya marca unidades analiticas
# como atipicas ANTES de modelar (T2 de Hotelling / Q-residual multivariado,
# test Q de Dixon por variable). Aqui se cruzan esas marcas de entrada con los
# residuos de salida: si una unidad quimicamente rara tambien predice mal, es
# evidencia triangulada; si predice bien, tambien vale la pena documentarlo.
# No se elimina ninguna observacion por esto, solo se marca para discusion.

ruta_analisis_quimico <- file.path(dir_procesamiento, "analisis_quimico.xlsx")

unidades_eda_flags <- data.frame(
  unidad_analitica_id = character(), flag_eda_t2 = logical(),
  flag_eda_q = logical(), n_variables_dixon = integer(), stringsAsFactors = FALSE
)

if (file.exists(ruta_analisis_quimico)) {
  hojas_quimico <- readxl::excel_sheets(ruta_analisis_quimico)

  t2q_eda <- data.frame()
  if ("13_t2q_outliers" %in% hojas_quimico) {
    t2q_eda <- readxl::read_excel(ruta_analisis_quimico, sheet = "13_t2q_outliers") %>%
      distinct(unidad_analitica_id, excede_t2, excede_q)
  }

  dixon_eda <- data.frame()
  if ("07_dixon_outliers" %in% hojas_quimico) {
    dixon_eda <- readxl::read_excel(ruta_analisis_quimico, sheet = "07_dixon_outliers") %>%
      filter(es_outlier_dixon %in% TRUE, !is.na(unidad_sospechosa)) %>%
      count(unidad_sospechosa, name = "n_variables_dixon") %>%
      rename(unidad_analitica_id = unidad_sospechosa)
  }

  unidades_eda_flags <- t2q_eda %>%
    {
      if (nrow(.) == 0) data.frame(unidad_analitica_id = character(), flag_eda_t2 = logical(), flag_eda_q = logical())
      else transmute(., unidad_analitica_id, flag_eda_t2 = excede_t2 %in% TRUE, flag_eda_q = excede_q %in% TRUE)
    } %>%
    full_join(dixon_eda, by = "unidad_analitica_id") %>%
    mutate(
      flag_eda_t2 = flag_eda_t2 %in% TRUE,
      flag_eda_q = flag_eda_q %in% TRUE,
      n_variables_dixon = ifelse(is.na(n_variables_dixon), 0L, n_variables_dixon)
    )
} else {
  message("No se encontro ", ruta_analisis_quimico, "; se omite el cruce con outliers del EDA (ejecuta scripts/R/exploratorio/10_analisis_quimico.R primero).")
}

cruce_outliers_eda_residuos <- data.frame()
resumen_cruce_outliers <- data.frame()

if (nrow(residuos_cv) > 0 && nrow(unidades_eda_flags) > 0) {
  cruce_outliers_eda_residuos <- residuos_cv %>%
    left_join(unidades_eda_flags, by = "unidad_analitica_id") %>%
    mutate(
      flag_eda_t2 = flag_eda_t2 %in% TRUE,
      flag_eda_q = flag_eda_q %in% TRUE,
      n_variables_dixon = ifelse(is.na(n_variables_dixon), 0L, n_variables_dixon),
      es_outlier_eda = flag_eda_t2 | flag_eda_q | n_variables_dixon > 0
    ) %>%
    select(
      escenario, target, modelo, unidad_analitica_id, muestra_base,
      y_obs, y_pred, residuo, residuo_abs, atipico_residual,
      flag_eda_t2, flag_eda_q, n_variables_dixon, es_outlier_eda
    )

  resumen_cruce_outliers <- cruce_outliers_eda_residuos %>%
    filter(!is.na(unidad_analitica_id)) %>%
    group_by(escenario, target, modelo, es_outlier_eda) %>%
    summarise(
      n_predicciones = n(),
      n_atipicos_residual = sum(atipico_residual, na.rm = TRUE),
      pct_atipicos_residual = round(100 * n_atipicos_residual / n_predicciones, 1),
      residuo_abs_medio = round(mean(residuo_abs, na.rm = TRUE), 4),
      .groups = "drop"
    ) %>%
    arrange(escenario, target, modelo, desc(es_outlier_eda)) %>%
    mutate(
      lectura = ifelse(
        es_outlier_eda,
        "Unidades ya marcadas como atipicas en el EDA quimico (T2/Q/Dixon) antes de modelar.",
        "Unidades sin marca de atipico en el EDA quimico."
      )
    )
}

# ------------------------------------------------------------
# 6. Resumen, diccionario y exportacion
# ------------------------------------------------------------

resumen <- data.frame(
  indicador = c(
    "filas_predicciones_leidas",
    "filas_residuos_cv",
    "escenarios_evaluados",
    "targets_evaluados",
    "modelos_evaluados",
    "residuos_atipicos_detectados",
    "unidades_con_marca_eda_t2_o_q",
    "unidades_con_marca_eda_dixon",
    "graficos_generados",
    "archivo_salida"
  ),
  valor = c(
    as.character(nrow(predicciones)),
    as.character(nrow(residuos_cv)),
    as.character(dplyr::n_distinct(residuos_cv$escenario)),
    as.character(dplyr::n_distinct(residuos_cv$target)),
    as.character(dplyr::n_distinct(residuos_cv$modelo)),
    as.character(nrow(residuos_atipicos)),
    as.character(sum(unidades_eda_flags$flag_eda_t2 | unidades_eda_flags$flag_eda_q, na.rm = TRUE)),
    as.character(sum(unidades_eda_flags$n_variables_dixon > 0, na.rm = TRUE)),
    as.character(nrow(graficos_generados)),
    ruta_salida_excel
  ),
  stringsAsFactors = FALSE
)

notas_metodologicas <- data.frame(
  punto = c(
    "alcance_residuos",
    "normalidad",
    "homocedasticidad",
    "modelos_no_lineales",
    "interpretacion_principal",
    "uso_recomendado"
  ),
  descripcion = c(
    "Los residuos se calculan sobre predicciones de validacion cruzada generadas en scripts 13 y 14.",
    "La prueba de Shapiro-Wilk se reporta como apoyo descriptivo; aplica principalmente a modelos lineales.",
    "La homocedasticidad se evalua mediante una proxy: correlacion de Spearman entre |residuo| y prediccion.",
    "Random Forest y Gradient Boosting no requieren supuestos OLS; se revisan por sesgo y patron de error.",
    "El foco principal es y_fenolico_comun en M_pura; y_frutal_comun se mantiene exploratorio.",
    "Usar predicho vs observado y residuos vs predicho en resultados; QQ plots preferentemente en anexo."
  ),
  stringsAsFactors = FALSE
)

diccionario <- data.frame(
  hoja = c(
    "00_resumen",
    "01_residuos_cv",
    "02_resumen_residuos",
    "03_diagnostico_supuestos",
    "04_residuos_atipicos",
    "05_modelos_revisados",
    "06_notas_metodologicas",
    "07_graficos_generados",
    "08_cruce_outliers_eda",
    "09_resumen_cruce_outliers"
  ),
  contenido = c(
    "Conteos generales del diagnostico residual.",
    "Predicciones de validacion cruzada con residuo, residuo absoluto y z residual.",
    "Metricas residuales por escenario, target y modelo.",
    "Pruebas/proxies de normalidad y heterocedasticidad, con notas de interpretacion.",
    "Observaciones con residuos altos o z residual absoluto >= 2.",
    "Modelos incluidos y uso metodologico en la tesis.",
    "Notas para redactar la seccion de diagnostico residual.",
    "Rutas de graficos PNG generados.",
    "Cada prediccion de residuos_cv, marcada con si esa unidad_analitica_id ya era atipica en el EDA quimico (T2 Hotelling, Q-residual o test Q de Dixon, script 10_analisis_quimico.R) antes de modelar.",
    "Comparacion agregada: tasa de residuos atipicos y residuo absoluto medio para unidades con y sin marca previa del EDA, por escenario/target/modelo."
  ),
  stringsAsFactors = FALSE
)

# Redondeo de tablas principales para lectura.
residuos_cv_export <- residuos_cv %>%
  mutate(across(where(is.numeric), ~ round(.x, 6)))

resumen_residuos_export <- resumen_residuos %>%
  mutate(across(where(is.numeric), ~ round(.x, 6)))

diagnostico_supuestos_export <- diagnostico_supuestos %>%
  mutate(across(where(is.numeric), ~ round(.x, 6)))

residuos_atipicos_export <- residuos_atipicos %>%
  mutate(across(where(is.numeric), ~ round(.x, 6)))

lista_hojas <- list(
  "00_resumen" = resumen,
  "01_residuos_cv" = residuos_cv_export,
  "02_resumen_residuos" = resumen_residuos_export,
  "03_diagnostico_supuestos" = diagnostico_supuestos_export,
  "04_residuos_atipicos" = residuos_atipicos_export,
  "05_modelos_revisados" = modelos_revisados,
  "06_notas_metodologicas" = notas_metodologicas,
  "07_graficos_generados" = graficos_generados,
  "08_cruce_outliers_eda" = cruce_outliers_eda_residuos,
  "09_resumen_cruce_outliers" = resumen_cruce_outliers,
  "10_diccionario" = diccionario
)

guardar_excel_seguro(lista_hojas, ruta_salida_excel)

cat("\nProceso terminado correctamente.\n")
cat("Archivo generado:\n", ruta_salida_excel, "\n")
cat("Graficos generados en:\n", dir_outputs_residuos, "\n")
cat("\nLectura clave:\n")
cat("\n- Los residuos del modelo lineal se usan para revisar supuestos de forma descriptiva.")
cat("\n- Para PLS y arboles restringidos se evalua principalmente error predictivo, sesgo y patron residual.")
cat("\n- Random Forest no debe evaluarse con supuestos OLS; se revisa predicho vs observado y distribucion de errores.\n")
