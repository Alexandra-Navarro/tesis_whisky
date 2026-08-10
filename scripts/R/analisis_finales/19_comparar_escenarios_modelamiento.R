# ============================================================
# 19_comparar_escenarios_modelamiento.R
# Comparacion final de escenarios, modelos y variables prioritarias
# Tesis: Analisis quimico-sensorial de whisky chileno
# ============================================================

# -----------------------------
# 0. Configuracion general
# -----------------------------

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

cargar_paquete <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Falta instalar el paquete requerido: ", pkg,
         ". Instalalo con install.packages('", pkg, "') y vuelve a ejecutar.")
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

paquetes <- c("readxl", "dplyr", "tidyr", "stringr", "writexl", "ggplot2", "purrr", "janitor")
invisible(lapply(paquetes, cargar_paquete))

if (!exists("dir_procesamiento")) {
  dir_procesamiento <- file.path("data", "procesamiento")
}

dir_modelamiento <- file.path("data", "modelamiento")
dir_outputs_modelamiento <- file.path("outputs", "modelamiento")
dir_outputs_comparacion <- file.path(dir_outputs_modelamiento, "comparacion_escenarios")

dir.create(dir_modelamiento, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_outputs_comparacion, recursive = TRUE, showWarnings = FALSE)

archivo_datos_modelamiento <- file.path(dir_modelamiento, "datos_modelamiento.xlsx")
archivo_base <- file.path(dir_modelamiento, "resultados_modelo_base.xlsx")
archivo_small <- file.path(dir_modelamiento, "resultados_modelos_small_data.xlsx")
archivo_boot <- file.path(dir_modelamiento, "validacion_bootstrap_cv.xlsx")
archivo_residuos <- file.path(dir_modelamiento, "residuos_diagnostico.xlsx")
archivo_interp <- file.path(dir_modelamiento, "interpretabilidad_modelos.xlsx")
archivo_salida <- file.path(dir_modelamiento, "comparacion_escenarios_modelamiento.xlsx")

# -----------------------------
# 1. Funciones auxiliares
# -----------------------------

leer_hoja_segura <- function(ruta, hoja) {
  if (!file.exists(ruta)) {
    warning("No existe el archivo: ", ruta)
    return(tibble())
  }
  hojas <- readxl::excel_sheets(ruta)
  if (!hoja %in% hojas) {
    warning("No existe la hoja '", hoja, "' en ", ruta)
    return(tibble())
  }
  readxl::read_excel(ruta, sheet = hoja) %>%
    janitor::clean_names()
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

primer_existente <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA_character_ else x[1]
}

clasificar_escenario <- function(escenario) {
  dplyr::case_when(
    escenario == "M_pura" ~ "principal_real",
    escenario == "M_expandida_evaluador" ~ "complementario_evaluador",
    escenario == "M_cata_individual" ~ "sensibilidad_con_ju",
    escenario == "M_ia_exploratoria" ~ "exploratorio_ia",
    TRUE ~ "otro"
  )
}

orden_escenario <- function(escenario) {
  dplyr::case_when(
    escenario == "M_pura" ~ 1,
    escenario == "M_cata_individual" ~ 2,
    escenario == "M_expandida_evaluador" ~ 3,
    escenario == "M_ia_exploratoria" ~ 4,
    TRUE ~ 99
  )
}

rol_modelo <- function(modelo) {
  dplyr::case_when(
    modelo == "media_entrenamiento" ~ "linea_base_nula",
    modelo == "lineal_reducido" ~ "linea_base_interpretable",
    modelo %in% c("ridge", "lasso", "elastic_net") ~ "regularizado",
    modelo == "pls" ~ "quimiometrico_interpretable",
    modelo == "random_forest_restringido" ~ "no_lineal_predictivo",
    modelo == "gradient_boosting_restringido" ~ "no_lineal_secundario",
    TRUE ~ "otro"
  )
}

interpretar_escenario <- function(escenario) {
  dplyr::case_when(
    escenario == "M_pura" ~ "Escenario principal con datos quimico-sensoriales reales (sin cata individual JU). Es la base central para conclusiones predictivas.",
    escenario == "M_expandida_evaluador" ~ "Escenario complementario para variabilidad entre evaluadores. No debe interpretarse como aumento independiente de muestras quimicas.",
    escenario == "M_cata_individual" ~ "Matriz pura mas la cata individual JU agregada. Permite evaluar el aporte y la heterogeneidad introducida por JU, comparando directamente contra M_pura.",
    escenario == "M_ia_exploratoria" ~ "Escenario sintetico exploratorio con imputacion sensorial asistida por IA. No reemplaza cata real.",
    TRUE ~ "Escenario no clasificado."
  )
}

decidir_modelo <- function(escenario, modelo, ranking_mae, mae_media, r2_media, spearman_media, estabilidad_error) {
  dplyr::case_when(
    escenario == "M_ia_exploratoria" & ranking_mae == 1 ~ "mejor_exploratorio_ia_no_real",
    modelo == "random_forest_restringido" & ranking_mae == 1 & mae_media <= 0.45 ~ "modelo_predictivo_recomendado",
    modelo == "pls" & ranking_mae <= 2 ~ "modelo_interpretable_recomendado",
    modelo %in% c("ridge", "lasso", "elastic_net") & ranking_mae <= 3 ~ "modelo_regularizado_de_apoyo",
    modelo == "gradient_boosting_restringido" & ranking_mae == 1 ~ "revisar_por_posible_sobreajuste",
    TRUE ~ "modelo_comparativo"
  )
}

# -----------------------------
# 2. Lectura de resultados previos
# -----------------------------

base_metricas <- leer_hoja_segura(archivo_base, "00_resumen_metricas")
small_metricas <- leer_hoja_segura(archivo_small, "00_resumen_metricas")
boot_resumen <- leer_hoja_segura(archivo_boot, "02_resumen_metricas_boot")
# media_entrenamiento y lineal_reducido ahora se evaluan dentro del mismo
# bootstrap agrupado que los candidatos (16_validacion_bootstrap_cv.R) para
# ser directamente comparables (OE4), pero este script solo compara y
# selecciona entre los seis modelos candidatos, no entre las lineas base.
if (nrow(boot_resumen) > 0 && "modelo" %in% names(boot_resumen)) {
  boot_resumen <- boot_resumen %>% dplyr::filter(!modelo %in% c("media_entrenamiento", "lineal_reducido"))
}
boot_cv <- leer_hoja_segura(archivo_boot, "05_comparacion_cv_boot")
residuos_resumen <- leer_hoja_segura(archivo_residuos, "02_resumen_residuos")
vars_finales <- leer_hoja_segura(archivo_interp, "05_variables_finales_tesis")
familias <- leer_hoja_segura(archivo_interp, "06_variables_por_familia")
colin_vars <- leer_hoja_segura(archivo_interp, "07_colinealidad_variables")

# -----------------------------
# 3. Tabla integrada de metricas finales
# -----------------------------

base_metricas2 <- base_metricas %>%
  mutate(
    fuente_resultado = "modelo_base",
    rol_modelo = rol_modelo(modelo),
    tipo_escenario = clasificar_escenario(escenario),
    orden_escenario = orden_escenario(escenario),
    mae_cv = safe_num(mae),
    rmse_cv = safe_num(rmse),
    r2_cv = safe_num(r2),
    spearman_cv = safe_num(spearman),
    bias_cv = safe_num(bias),
    mae_bootstrap = NA_real_,
    rmse_bootstrap = NA_real_,
    r2_bootstrap = NA_real_,
    spearman_bootstrap = NA_real_,
    mae_p05 = NA_real_,
    mae_p50 = NA_real_,
    mae_p95 = NA_real_,
    mae_ic90_ancho = NA_real_,
    estabilidad_error = NA_character_,
    ranking_mae_bootstrap = NA_real_
  ) %>%
  select(
    escenario, target, modelo, fuente_resultado, rol_modelo, tipo_escenario,
    orden_escenario, mae_cv, rmse_cv, r2_cv, spearman_cv, bias_cv,
    mae_bootstrap, rmse_bootstrap, r2_bootstrap, spearman_bootstrap,
    mae_p05, mae_p50, mae_p95, mae_ic90_ancho, estabilidad_error,
    ranking_mae_bootstrap
  )

small_metricas2 <- small_metricas %>%
  mutate(
    fuente_resultado = "validacion_cruzada_small_data",
    rol_modelo = rol_modelo(modelo),
    tipo_escenario = clasificar_escenario(escenario),
    orden_escenario = orden_escenario(escenario),
    mae_cv = safe_num(mae),
    rmse_cv = safe_num(rmse),
    r2_cv = safe_num(r2),
    spearman_cv = safe_num(spearman),
    bias_cv = safe_num(bias),
    mae_bootstrap = NA_real_,
    rmse_bootstrap = NA_real_,
    r2_bootstrap = NA_real_,
    spearman_bootstrap = NA_real_,
    mae_p05 = NA_real_,
    mae_p50 = NA_real_,
    mae_p95 = NA_real_,
    mae_ic90_ancho = NA_real_,
    estabilidad_error = NA_character_,
    ranking_mae_bootstrap = NA_real_
  ) %>%
  select(
    escenario, target, modelo, fuente_resultado, rol_modelo, tipo_escenario,
    orden_escenario, mae_cv, rmse_cv, r2_cv, spearman_cv, bias_cv,
    mae_bootstrap, rmse_bootstrap, r2_bootstrap, spearman_bootstrap,
    mae_p05, mae_p50, mae_p95, mae_ic90_ancho, estabilidad_error,
    ranking_mae_bootstrap
  )

boot_metricas2 <- boot_resumen %>%
  mutate(
    fuente_resultado = "bootstrap",
    rol_modelo = rol_modelo(modelo),
    tipo_escenario = clasificar_escenario(escenario),
    orden_escenario = orden_escenario(escenario),
    mae_cv = NA_real_,
    rmse_cv = NA_real_,
    r2_cv = NA_real_,
    spearman_cv = NA_real_,
    bias_cv = NA_real_,
    mae_bootstrap = safe_num(mae_media),
    rmse_bootstrap = safe_num(rmse_media),
    r2_bootstrap = safe_num(r2_agregado),
    spearman_bootstrap = safe_num(spearman_media),
    mae_p05 = safe_num(mae_p05),
    mae_p50 = safe_num(mae_p50),
    mae_p95 = safe_num(mae_p95),
    mae_ic90_ancho = safe_num(mae_ic90_ancho),
    ranking_mae_bootstrap = safe_num(ranking_mae)
  ) %>%
  select(
    escenario, target, modelo, fuente_resultado, rol_modelo, tipo_escenario,
    orden_escenario, mae_cv, rmse_cv, r2_cv, spearman_cv, bias_cv,
    mae_bootstrap, rmse_bootstrap, r2_bootstrap, spearman_bootstrap,
    mae_p05, mae_p50, mae_p95, mae_ic90_ancho, estabilidad_error,
    ranking_mae_bootstrap
  )

metricas_integradas <- bind_rows(base_metricas2, small_metricas2, boot_metricas2) %>%
  arrange(orden_escenario, target, fuente_resultado, modelo)

# -----------------------------
# 4. Seleccion de mejores modelos y decisiones finales
# -----------------------------

ranking_bootstrap <- boot_resumen %>%
  mutate(
    mae_media = safe_num(mae_media),
    rmse_media = safe_num(rmse_media),
    r2_media = safe_num(r2_media),
    r2_agregado = safe_num(r2_agregado),
    spearman_media = safe_num(spearman_media),
    mae_p05 = safe_num(mae_p05),
    mae_p95 = safe_num(mae_p95),
    mae_ic90_ancho = safe_num(mae_ic90_ancho),
    ranking_mae = safe_num(ranking_mae),
    tipo_escenario = clasificar_escenario(escenario),
    orden_escenario = orden_escenario(escenario),
    rol_modelo = rol_modelo(modelo),
    decision_modelo = decidir_modelo(
      escenario, modelo, ranking_mae, mae_media, r2_media, spearman_media, estabilidad_error
    )
  ) %>%
  arrange(orden_escenario, target, ranking_mae)

mejor_modelo_por_escenario <- ranking_bootstrap %>%
  group_by(escenario, target) %>%
  slice_min(order_by = mae_media, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    lectura_escenario = interpretar_escenario(escenario),
    recomendacion_uso = case_when(
      escenario == "M_pura" & target == "y_fenolico_comun" ~ "modelo_principal_predictivo",
      escenario == "M_cata_individual" & target == "y_fenolico_comun" ~ "comparacion_con_aporte_ju",
      escenario == "M_expandida_evaluador" ~ "complementario_no_principal",
      escenario == "M_ia_exploratoria" ~ "exploratorio_sintetico_no_real",
      target == "y_frutal_comun" ~ "exploratorio_por_baja_cobertura",
      TRUE ~ "comparativo"
    )
  ) %>%
  arrange(orden_escenario, target)

# Comparacion contra modelos base
base_ref <- base_metricas %>%
  filter(modelo %in% c("media_entrenamiento", "lineal_reducido")) %>%
  select(escenario, target, modelo_base = modelo, mae_base = mae, rmse_base = rmse, r2_base = r2, spearman_base = spearman) %>%
  mutate(across(c(mae_base, rmse_base, r2_base, spearman_base), safe_num))

mejor_small <- ranking_bootstrap %>%
  group_by(escenario, target) %>%
  slice_min(order_by = mae_media, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(escenario, target, mejor_modelo = modelo, mae_mejor = mae_media, rmse_mejor = rmse_media,
         r2_mejor = r2_agregado, spearman_mejor = spearman_media, mae_p05, mae_p95, estabilidad_error)

comparacion_base_final <- base_ref %>%
  left_join(mejor_small, by = c("escenario", "target")) %>%
  mutate(
    reduccion_mae_abs = mae_base - mae_mejor,
    reduccion_mae_pct = if_else(mae_base > 0, 100 * (mae_base - mae_mejor) / mae_base, NA_real_),
    lectura = case_when(
      reduccion_mae_pct >= 30 ~ "mejora_clara_frente_a_base",
      reduccion_mae_pct >= 10 ~ "mejora_moderada_frente_a_base",
      reduccion_mae_pct > 0 ~ "mejora_baja_frente_a_base",
      TRUE ~ "sin_mejora_frente_a_base"
    )
  ) %>%
  arrange(orden_escenario(escenario), target, modelo_base)

# Resumen por escenario para target principal
comparacion_escenarios <- mejor_modelo_por_escenario %>%
  filter(target == "y_fenolico_comun") %>%
  transmute(
    escenario,
    tipo_escenario,
    modelo_recomendado = modelo,
    mae_bootstrap = mae_media,
    mae_p05,
    mae_p95,
    rmse_bootstrap = rmse_media,
    r2_bootstrap = r2_agregado,
    spearman_bootstrap = spearman_media,
    estabilidad_error,
    recomendacion_uso,
    lectura_escenario
  ) %>%
  arrange(orden_escenario(escenario))

# -----------------------------
# 5. Variables prioritarias finales
# -----------------------------

variables_prioritarias <- vars_finales %>%
  mutate(
    orden_escenario = orden_escenario(escenario),
    lectura_final = case_when(
      prioridad_tesis == "prioritaria" & coherencia_target == "alta_directa" ~ "variable_prioritaria_directa",
      prioridad_tesis == "prioritaria" & str_detect(coherencia_target, "indirecta|contextual") ~ "variable_prioritaria_indirecta",
      prioridad_tesis == "apoyo_interpretativo" ~ "apoyo_interpretativo",
      TRUE ~ "baja_prioridad"
    )
  ) %>%
  arrange(orden_escenario, target, ranking_consenso)

variables_discusion_principal <- variables_prioritarias %>%
  filter(
    escenario %in% c("M_pura", "M_cata_individual"),
    target == "y_fenolico_comun",
    prioridad_tesis %in% c("prioritaria", "apoyo_interpretativo")
  ) %>%
  group_by(predictor_limpio, tecnica, familia_quimica, coherencia_target) %>%
  summarise(
    escenarios_donde_aparece = paste(sort(unique(escenario)), collapse = "; "),
    n_escenarios = n_distinct(escenario),
    frecuencia_media_promedio = mean(frecuencia_media, na.rm = TRUE),
    ranking_promedio = mean(ranking_consenso, na.rm = TRUE),
    mejor_prioridad = primer_existente(prioridad_tesis[order(ranking_consenso)]),
    justificacion_quimica = primer_existente(justificacion_quimica),
    .groups = "drop"
  ) %>%
  arrange(desc(n_escenarios), desc(frecuencia_media_promedio), ranking_promedio)

familias_principales <- familias %>%
  filter(escenario == "M_pura", target == "y_fenolico_comun") %>%
  arrange(desc(frecuencia_media_promedio))

colinealidad_prioritarias <- colin_vars %>%
  filter(target == "y_fenolico_comun") %>%
  arrange(orden_escenario(escenario), desc(abs_r))

# -----------------------------
# 6. Resumen ejecutivo y texto metodologico
# -----------------------------

mejor_a_pura <- mejor_modelo_por_escenario %>%
  filter(escenario == "M_pura", target == "y_fenolico_comun") %>%
  slice(1)

mejor_d_con_ju <- mejor_modelo_por_escenario %>%
  filter(escenario == "M_cata_individual", target == "y_fenolico_comun") %>%
  slice(1)

mejor_ia <- mejor_modelo_por_escenario %>%
  filter(escenario == "M_ia_exploratoria", target == "y_fenolico_comun") %>%
  slice(1)

resumen_ejecutivo <- tibble(
  elemento = c(
    "escenario_principal",
    "target_principal",
    "mejor_modelo_principal",
    "mae_bootstrap_modelo_principal",
    "r2_bootstrap_modelo_principal",
    "spearman_bootstrap_modelo_principal",
    "mejor_escenario_sensibilidad",
    "mejor_modelo_sensibilidad",
    "escenario_ia",
    "modelo_ia_mejor",
    "interpretacion_general"
  ),
  valor = c(
    "M_pura",
    "y_fenolico_comun",
    ifelse(nrow(mejor_a_pura) > 0, mejor_a_pura$modelo[1], NA_character_),
    ifelse(nrow(mejor_a_pura) > 0, round(mejor_a_pura$mae_media[1], 3), NA),
    ifelse(nrow(mejor_a_pura) > 0, round(mejor_a_pura$r2_agregado[1], 3), NA),
    ifelse(nrow(mejor_a_pura) > 0, round(mejor_a_pura$spearman_media[1], 3), NA),
    "M_cata_individual",
    ifelse(nrow(mejor_d_con_ju) > 0, mejor_d_con_ju$modelo[1], NA_character_),
    "M_ia_exploratoria",
    ifelse(nrow(mejor_ia) > 0, mejor_ia$modelo[1], NA_character_),
    "Bajo validacion agrupada por muestra unica (identificador_quimico), Random Forest restringido es el modelo candidato con menor error en M_pura para el target fenolico (MAE=0.388 frente a 0.459 de la media de entrenamiento, R2 agregado=0.273), superando a ambas lineas base evaluadas en el mismo esquema. Un analisis de sensibilidad que excluye las dos muestras comerciales de la matriz (Dalwhinnie y Caol Ila) muestra que esta senal se revierte a R2 agregado negativo, por lo que el resultado se interpreta como una senal predictiva moderada y condicionada por el contraste comercial-experimental, no como evidencia general para whisky chileno. M_cata_individual muestra una senal positiva mas fuerte pero comparte el mismo posible confundente. La matriz IA es solo exploratoria."
  )
)

texto_resultados <- tibble(
  seccion = c(
    "modelo_principal",
    "sensibilidad_ju",
    "matriz_expandida",
    "matriz_ia",
    "interpretabilidad",
    "limitacion"
  ),
  texto_base = c(
    "En la matriz pura, bajo validacion agrupada por muestra unica (identificador_quimico), Random Forest restringido para y_fenolico_comun obtiene el menor error entre los modelos candidatos (MAE=0.388) y supera a ambas lineas base evaluadas en el mismo esquema de validacion (media de entrenamiento MAE=0.459, R2 agregado=0 por definicion; modelo lineal reducido MAE=0.721, R2 agregado negativo), con un R2 bootstrap agregado de 0.273. Un analisis de sensibilidad adicional muestra que esta senal depende en gran medida de las dos muestras comerciales incluidas en la matriz (Dalwhinnie y Caol Ila): al excluirlas, el R2 agregado se vuelve negativo. El resultado se reporta por lo tanto como una senal predictiva moderada y condicionada, no como evidencia robusta de una relacion quimico-sensorial generalizable a whisky chileno experimental.",
    "El escenario sin JU permite evaluar la sensibilidad del resultado al retirar la cata individual JU. Si el error disminuye en este escenario, la lectura correcta es que JU aumenta cobertura, pero tambien introduce heterogeneidad metodologica.",
    "La matriz expandida por evaluador debe interpretarse como complemento para variabilidad sensorial. No debe tratarse como aumento independiente de muestras quimicas, por lo que su desempeno no reemplaza a la matriz pura.",
    "La matriz IA corresponde a imputacion sensorial sintetica asistida por IA. Sus resultados permiten explorar patrones, pero no constituyen evidencia sensorial humana ni deben mezclarse con la matriz real.",
    "La interpretabilidad debe centrarse en variables estables entre modelos y bootstrap. Fenoles volatiles como cresoles y guaiacol tienen lectura quimica directa para el atributo fenolico; variables fermentativas JU y senales GC-MS deben tratarse como evidencia contextual o indirecta.",
    "Dado el tamano muestral reducido, la alta colinealidad y la estructura no solapada entre tecnicas, los resultados deben presentarse como exploratorios y orientados a priorizacion de compuestos para futuras validaciones experimentales."
  )
)

figuras_tesis <- tibble(
  figura = c(
    "01_modelamiento_mae_bootstrap_y_fenolico.png",
    "02_modelamiento_mejor_modelo_por_escenario.png",
    "03_modelamiento_base_vs_mejor_y_fenolico.png",
    "04_modelamiento_variables_discusion.png"
  ),
  ruta = file.path(dir_outputs_comparacion, figura),
  uso_recomendado = c(
    "Cuerpo principal: comparacion final de modelos bootstrap.",
    "Cuerpo principal: mejor modelo por escenario para target principal.",
    "Cuerpo principal o anexo: comparacion contra linea base.",
    "Cuerpo principal: variables prioritarias para discusion quimica."
  )
)

# -----------------------------
# 7. Graficos finales
# -----------------------------

target_principal <- "y_fenolico_comun"

plot_boot <- ranking_bootstrap %>%
  filter(target == target_principal) %>%
  mutate(
    escenario = factor(escenario, levels = c("M_pura", "M_cata_individual", "M_expandida_evaluador", "M_ia_exploratoria")),
    modelo = factor(modelo, levels = c("random_forest_restringido", "pls", "ridge", "lasso", "elastic_net", "gradient_boosting_restringido"))
  )

if (nrow(plot_boot) > 0) {
  g1 <- ggplot(plot_boot, aes(x = mae_media, y = escenario, fill = modelo)) +
    geom_col(position = position_dodge(width = 0.82), width = 0.72) +
    geom_errorbar(
      aes(xmin = mae_p05, xmax = mae_p95),
      position = position_dodge(width = 0.82),
      width = 0.22,
      linewidth = 0.35
    ) +
    labs(
      title = "Comparacion final de modelos para y_fenolico_comun",
      subtitle = "MAE medio bootstrap; barras de error: percentiles 5 y 95",
      x = "MAE bootstrap",
      y = "Escenario",
      fill = "Modelo"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")

  ggsave(file.path(dir_outputs_comparacion, "01_modelamiento_mae_bootstrap_y_fenolico.png"),
         g1, width = 12, height = 7, dpi = 300)
}

plot_best <- comparacion_escenarios %>%
  mutate(escenario = factor(escenario, levels = c("M_pura", "M_cata_individual", "M_expandida_evaluador", "M_ia_exploratoria")))

if (nrow(plot_best) > 0) {
  g2 <- ggplot(plot_best, aes(x = mae_bootstrap, y = escenario)) +
    geom_col(fill = "grey35", width = 0.7) +
    geom_errorbar(aes(xmin = mae_p05, xmax = mae_p95), width = 0.22, linewidth = 0.35) +
    geom_text(aes(x = mae_p95, label = modelo_recomendado), hjust = -0.05, size = 3.5) +
    labs(
      title = "Mejor modelo por escenario para y_fenolico_comun",
      subtitle = "Seleccion segun menor MAE medio bootstrap",
      x = "MAE bootstrap",
      y = "Escenario"
    ) +
    coord_cartesian(xlim = c(0, max(plot_best$mae_p95, na.rm = TRUE) * 1.25)) +
    theme_minimal(base_size = 12)

  ggsave(file.path(dir_outputs_comparacion, "02_modelamiento_mejor_modelo_por_escenario.png"),
         g2, width = 10, height = 6, dpi = 300)
}

plot_base <- comparacion_base_final %>%
  filter(target == target_principal, modelo_base %in% c("media_entrenamiento", "lineal_reducido")) %>%
  select(escenario, modelo_base, mae_base, mejor_modelo, mae_mejor) %>%
  pivot_longer(
    cols = c(mae_base, mae_mejor),
    names_to = "tipo",
    values_to = "mae"
  ) %>%
  mutate(
    modelo_grafico = if_else(tipo == "mae_mejor", paste0("mejor: ", mejor_modelo), modelo_base),
    escenario = factor(escenario, levels = c("M_pura", "M_cata_individual", "M_expandida_evaluador", "M_ia_exploratoria"))
  )

if (nrow(plot_base) > 0) {
  g3 <- ggplot(plot_base, aes(x = mae, y = escenario, fill = modelo_grafico)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.68) +
    labs(
      title = "Comparacion entre modelos base y mejor modelo small data",
      subtitle = "Target principal: y_fenolico_comun",
      x = "MAE",
      y = "Escenario",
      fill = "Modelo"
    ) +
    theme_minimal(base_size = 12)

  ggsave(file.path(dir_outputs_comparacion, "03_modelamiento_base_vs_mejor_y_fenolico.png"),
         g3, width = 11, height = 6, dpi = 300)
}

plot_vars <- variables_discusion_principal %>%
  slice_max(order_by = frecuencia_media_promedio, n = 12, with_ties = FALSE) %>%
  mutate(predictor_limpio = reorder(predictor_limpio, frecuencia_media_promedio))

if (nrow(plot_vars) > 0) {
  g4 <- ggplot(plot_vars, aes(x = frecuencia_media_promedio, y = predictor_limpio)) +
    geom_col(fill = "grey35", width = 0.72) +
    labs(
      title = "Variables quimicas prioritarias para discusion",
      subtitle = "Consenso entre escenarios reales para y_fenolico_comun",
      x = "Frecuencia media de seleccion",
      y = "Predictor quimico"
    ) +
    theme_minimal(base_size = 12)

  ggsave(file.path(dir_outputs_comparacion, "04_modelamiento_variables_discusion.png"),
         g4, width = 10, height = 6.5, dpi = 300)
}

# -----------------------------
# 8. Diccionario y exportacion
# -----------------------------

diccionario <- tibble(
  hoja = c(
    "00_resumen_ejecutivo",
    "01_metricas_integradas",
    "02_ranking_bootstrap",
    "03_mejor_modelo_escenario",
    "04_comparacion_base_final",
    "05_comparacion_escenarios",
    "06_variables_prioritarias",
    "07_variables_discusion",
    "08_familias_principales",
    "09_colinealidad_prioritarias",
    "10_texto_resultados",
    "11_figuras_tesis"
  ),
  descripcion = c(
    "Resumen ejecutivo de la fase de modelamiento.",
    "Metricas integradas de modelos base, validacion cruzada y bootstrap.",
    "Ranking completo de modelos small data segun MAE bootstrap.",
    "Mejor modelo por escenario y target.",
    "Comparacion del mejor modelo frente a media y lineal reducido.",
    "Comparacion final entre escenarios para el target principal.",
    "Variables priorizadas por estabilidad, consenso y coherencia quimica.",
    "Variables recomendadas para discusion principal en escenarios reales.",
    "Resumen por familia o grupo quimico para M_pura.",
    "Advertencias de colinealidad para variables priorizadas.",
    "Textos base para redactar resultados y limitaciones.",
    "Rutas de graficos finales sugeridos para tesis."
  )
)

salida <- list(
  "00_resumen_ejecutivo" = resumen_ejecutivo,
  "01_metricas_integradas" = metricas_integradas,
  "02_ranking_bootstrap" = ranking_bootstrap,
  "03_mejor_modelo_escenario" = mejor_modelo_por_escenario,
  "04_comparacion_base_final" = comparacion_base_final,
  "05_comparacion_escenarios" = comparacion_escenarios,
  "06_variables_prioritarias" = variables_prioritarias,
  "07_variables_discusion" = variables_discusion_principal,
  "08_familias_principales" = familias_principales,
  "09_colinealidad_prioritarias" = colinealidad_prioritarias,
  "10_texto_resultados" = texto_resultados,
  "11_figuras_tesis" = figuras_tesis,
  "12_diccionario" = diccionario
)

writexl::write_xlsx(salida, archivo_salida)

cat("\nProceso terminado correctamente.\n")
cat("Archivo generado:\n ", normalizePath(archivo_salida, winslash = "/", mustWork = FALSE), "\n")
cat("Graficos generados en:\n ", normalizePath(dir_outputs_comparacion, winslash = "/", mustWork = FALSE), "\n")
cat("\nLectura clave:\n")
cat("\n- Esta salida consolida la seleccion final de modelos, escenarios y variables prioritarias.")
cat("\n- M_pura se mantiene como escenario principal real.")
cat("\n- M_ia_exploratoria se mantiene como escenario sintetico complementario.")
cat("\n- Las variables deben interpretarse considerando colinealidad y coherencia quimica.\n")
