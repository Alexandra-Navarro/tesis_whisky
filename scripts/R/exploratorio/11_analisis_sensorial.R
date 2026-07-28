# ============================================================
# 11_analisis_sensorial.R
# Analisis exploratorio de datos (EDA) sensorial
# Tesis whisky chileno - modelamiento quimico-sensorial
#
# Sigue el mismo esquema clasico de EDA que 10_analisis_quimico.R
# (Guia practica de introduccion al Analisis Exploratorio de
# Datos, datos.gob.es 2021), adaptado a evaluaciones sensoriales
# de catadores:
#   1. Analisis descriptivo (resumen + histogramas de targets)
#   2. Ajuste/verificacion de tipos de variable
#   3. Deteccion de datos ausentes (tabla + mapa de completitud)
#   4. Deteccion de valores atipicos (boxplots, regla IQR) -
#      solo se documentan, no se eliminan (evaluaciones reales
#      de catadores, no se alteran).
#   5. Correlacion entre targets sensoriales (tabla + heatmap).
#
# Analisis adicionales (2026-07-07):
#   6. Perfil por categoria de catador (Experto/Sommelier,
#      Aficionado, Consumidor General) en cata_social_enero_2025.
#   7. Radar charts reproducibles (por muestra y por categoria
#      de catador), antes armados a mano en Excel.
#   8. PCA/biplot de targets sensoriales por bloque.
#   9. Concordancia entre evaluadores (ICC(1) de una via).
#
# Analisis adicional (2026-07-08), mismo criterio de small data que
# en 10_analisis_quimico.R:
#   10. T2 de Hotelling y Q-residuals (SPE) sobre el PCA sensorial de
#       cada bloque: diagnostico multivariado de que muestras tienen
#       un perfil sensorial completo atipico. No se aplica el test Q
#       de Dixon aqui: esta diseñado para replicas instrumentales de
#       una sola magnitud, no para puntajes ordinales de multiples
#       catadores (para eso ya esta el ICC de la etapa 9).
# ============================================================

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

paquetes_exploratorio <- c("readxl", "dplyr", "tidyr", "stringr", "writexl", "ggplot2")
paquetes_faltantes <- paquetes_exploratorio[
  !vapply(paquetes_exploratorio, requireNamespace, logical(1), quietly = TRUE)
]
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

cat("\nANALISIS EXPLORATORIO SENSORIAL\n")

# ------------------------------------------------------------
# Rutas

ruta_sensorial <- file.path(dir_procesamiento, "sensorial_targets.xlsx")
ruta_salida_base <- file.path(dir_procesamiento, "analisis_sensorial.xlsx")

dir_outputs_exploratorio <- file.path(dir_proyecto, "outputs", "exploratorio", "sensorial")
dir.create(dir_outputs_exploratorio, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(ruta_sensorial)) {
  stop(paste("No existe el archivo sensorial:", ruta_sensorial))
}

# ------------------------------------------------------------
# Estilo comun para los graficos

color_acento <- "#2E5395"
color_alerta <- "#B00000"

tema_eda <- theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
    strip.text = element_text(size = 7)
  )

# ------------------------------------------------------------
# Funciones

guardar_excel_seguro <- function(hojas, ruta_salida) {
  if (file.exists(ruta_salida)) {
    eliminado <- tryCatch(
      file.remove(ruta_salida),
      warning = function(w) FALSE,
      error = function(e) FALSE
    )

    if (!eliminado && file.exists(ruta_salida)) {
      ruta_alt <- file.path(
        dirname(ruta_salida),
        paste0(
          tools::file_path_sans_ext(basename(ruta_salida)),
          "_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".xlsx"
        )
      )

      warning(
        paste(
          "No se pudo sobrescribir el archivo.",
          "Probablemente esta abierto. Se guardara copia en:",
          ruta_alt
        )
      )

      writexl::write_xlsx(hojas, path = ruta_alt)
      return(ruta_alt)
    }
  }

  writexl::write_xlsx(hojas, path = ruta_salida)
  ruta_salida
}

guardar_grafico_seguro <- function(plot, nombre_archivo, ancho = 9, alto = 6) {
  ruta <- file.path(dir_outputs_exploratorio, nombre_archivo)
  tryCatch(
    {
      ggsave(ruta, plot = plot, width = ancho, height = alto, dpi = 150)
      ruta
    },
    error = function(e) {
      message("No se pudo guardar el grafico ", nombre_archivo, ": ", conditionMessage(e))
      NA_character_
    }
  )
}

num_seguro <- function(x) suppressWarnings(as.numeric(x))

media_segura <- function(x) {
  x <- num_seguro(x)
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

sd_segura <- function(x) {
  x <- num_seguro(x)
  if (sum(!is.na(x)) < 2) return(NA_real_)
  sd(x, na.rm = TRUE)
}

mediana_segura <- function(x) {
  x <- num_seguro(x)
  if (all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

min_seguro <- function(x) {
  x <- num_seguro(x)
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

max_seguro <- function(x) {
  x <- num_seguro(x)
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

clasificar_bloque_sensorial <- function(bloque_sensorial) {
  case_when(
    bloque_sensorial == "cata_alexandra_260603" ~ "alexandra_260603",
    bloque_sensorial == "sensorial_cepas_ju" ~ "ju_cepas",
    bloque_sensorial == "cata_septiembre_2024" ~ "historico_septiembre",
    bloque_sensorial == "cata_social_enero_2025" ~ "historico_enero",
    bloque_sensorial == "panel_noviembre" ~ "noviembre",
    TRUE ~ "otro"
  )
}

clasificar_tipo_panel <- function(n_evaluadores) {
  case_when(
    is.na(n_evaluadores) ~ "sin_informacion",
    n_evaluadores == 1 ~ "cata_individual",
    n_evaluadores >= 2 & n_evaluadores < 10 ~ "panel_pequeno",
    n_evaluadores >= 10 ~ "panel_amplio",
    TRUE ~ "revisar"
  )
}

detectar_outliers_iqr <- function(df_largo, cols_grupo = "target") {
  df_largo %>%
    filter(!is.na(valor)) %>%
    group_by(across(all_of(cols_grupo))) %>%
    filter(n() >= 4) %>%
    mutate(
      q1 = quantile(valor, 0.25, na.rm = TRUE),
      q3 = quantile(valor, 0.75, na.rm = TRUE),
      iqr = q3 - q1,
      limite_inferior = q1 - 1.5 * iqr,
      limite_superior = q3 + 1.5 * iqr,
      es_outlier = valor < limite_inferior | valor > limite_superior
    ) %>%
    ungroup() %>%
    filter(es_outlier)
}

# ------------------------------------------------------------
# ETAPA 1a: Lectura y analisis descriptivo

sensorial_agregado <- readxl::read_excel(ruta_sensorial, sheet = "01_sensorial_agregado_wide") %>%
  as.data.frame(stringsAsFactors = FALSE)

sensorial_individual <- readxl::read_excel(ruta_sensorial, sheet = "02_sensorial_individual_wide") %>%
  as.data.frame(stringsAsFactors = FALSE)

sensorial_sin_quimica <- readxl::read_excel(ruta_sensorial, sheet = "05_sensorial_sin_quimica") %>%
  as.data.frame(stringsAsFactors = FALSE)

target_cols <- names(sensorial_individual)[str_detect(names(sensorial_individual), "^y_")]

targets_principales <- c("y_frutal_comun", "y_fenolico_comun", "y_ahumado_comun", "y_medicinal_comun")
targets_principales <- intersect(targets_principales, target_cols)

sensorial_individual <- sensorial_individual %>%
  mutate(
    tiene_quimica_confirmada = estandarizar_logico(tiene_quimica_confirmada),
    grupo_sensorial = clasificar_bloque_sensorial(bloque_sensorial)
  )

sensorial_agregado <- sensorial_agregado %>%
  mutate(
    tiene_quimica_confirmada = estandarizar_logico(tiene_quimica_confirmada),
    grupo_sensorial = clasificar_bloque_sensorial(bloque_sensorial),
    tipo_panel = clasificar_tipo_panel(n_evaluadores)
  )

sensorial_largo_completo <- sensorial_individual %>%
  select(
    observacion_sensorial_id, grupo_sensorial, bloque_sensorial, archivo_sensorial,
    muestra_cata, identificador_sensorial, tiene_quimica_confirmada,
    evaluador_id, nombre_catador, categoria_catador, all_of(target_cols)
  ) %>%
  pivot_longer(cols = all_of(target_cols), names_to = "target", values_to = "valor") %>%
  mutate(valor = num_seguro(valor))

resumen_global <- data.frame(
  indicador = c(
    "Unidad individual sensorial", "Muestras sensoriales totales", "Evaluaciones individuales totales",
    "Evaluaciones con quimica confirmada", "Evaluaciones sin quimica confirmada", "Bloques sensoriales",
    "Targets sensoriales detectados", "Muestras sensoriales agregadas", "Uso de este analisis",
    "Advertencia metodologica"
  ),
  valor = c(
    "Evaluacion individual de catador",
    n_distinct(paste(sensorial_individual$bloque_sensorial, sensorial_individual$muestra_cata)),
    nrow(sensorial_individual),
    sum(sensorial_individual$tiene_quimica_confirmada == TRUE, na.rm = TRUE),
    sum(sensorial_individual$tiene_quimica_confirmada == FALSE, na.rm = TRUE),
    n_distinct(sensorial_individual$bloque_sensorial), length(target_cols), nrow(sensorial_agregado),
    "Analisis exploratorio de datos (EDA), no usa predictores quimicos.",
    "Las evaluaciones individuales son reales, pero no equivalen a muestras quimicas independientes."
  ),
  stringsAsFactors = FALSE
)

resumen_bloques <- sensorial_individual %>%
  group_by(grupo_sensorial, bloque_sensorial, archivo_sensorial) %>%
  summarise(
    n_muestras_sensoriales = n_distinct(muestra_cata), n_evaluaciones_individuales = n(),
    n_evaluadores = n_distinct(evaluador_id),
    n_con_quimica = sum(tiene_quimica_confirmada == TRUE, na.rm = TRUE),
    n_sin_quimica = sum(tiene_quimica_confirmada == FALSE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(tipo_panel = clasificar_tipo_panel(n_evaluadores)) %>%
  arrange(grupo_sensorial, bloque_sensorial)

resumen_muestras <- sensorial_individual %>%
  group_by(grupo_sensorial, bloque_sensorial, muestra_cata, identificador_sensorial, tiene_quimica_confirmada) %>%
  summarise(n_evaluaciones = n(), n_evaluadores = n_distinct(evaluador_id), .groups = "drop") %>%
  mutate(tipo_panel = clasificar_tipo_panel(n_evaluadores)) %>%
  arrange(grupo_sensorial, bloque_sensorial, muestra_cata)

descriptivos_targets <- sensorial_largo_completo %>%
  group_by(target) %>%
  summarise(
    n_total = n(), n_con_valor = sum(!is.na(valor)), n_sin_valor = sum(is.na(valor)),
    media = media_segura(valor), desviacion_estandar = sd_segura(valor),
    minimo = min_seguro(valor), mediana = mediana_segura(valor), maximo = max_seguro(valor),
    proporcion_con_valor = round(n_con_valor / n_total, 4), .groups = "drop"
  ) %>%
  arrange(desc(proporcion_con_valor), target)

descriptivos_bloques <- sensorial_largo_completo %>%
  group_by(grupo_sensorial, bloque_sensorial, target) %>%
  summarise(
    n_total = n(), n_con_valor = sum(!is.na(valor)), media = media_segura(valor),
    desviacion_estandar = sd_segura(valor), mediana = mediana_segura(valor),
    proporcion_con_valor = round(n_con_valor / n_total, 4), .groups = "drop"
  ) %>%
  arrange(grupo_sensorial, bloque_sensorial, target)

# Etapa 1b: histogramas de distribucion de targets (todas las evaluaciones individuales)
p_hist <- sensorial_largo_completo %>%
  filter(!is.na(valor)) %>%
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 12, fill = color_acento, color = "white") +
  facet_wrap(~target, scales = "free_y") +
  tema_eda +
  theme(axis.text.x = element_text(angle = 0)) +
  labs(title = "Distribucion de targets sensoriales (evaluaciones individuales)", x = "Puntaje", y = "Frecuencia")

ruta_histograma_targets <- guardar_grafico_seguro(p_hist, "01_histogramas_targets.png", ancho = 11, alto = 9)

# ------------------------------------------------------------
# ETAPA 2: Ajuste / verificacion de tipos de variable

tipos_targets <- data.frame(
  target = target_cols,
  clase_r = sapply(sensorial_individual[target_cols], function(x) class(x)[1]),
  es_target_principal = target_cols %in% targets_principales,
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# ETAPA 3: Datos ausentes (completitud) + mapa visual

completitud_bloque_target <- sensorial_individual %>%
  group_by(bloque_sensorial) %>%
  summarise(across(all_of(target_cols), ~ mean(!is.na(.x))), .groups = "drop") %>%
  pivot_longer(cols = -bloque_sensorial, names_to = "target", values_to = "pct_completo")

p_completitud <- ggplot(completitud_bloque_target, aes(x = target, y = bloque_sensorial, fill = pct_completo)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#F4CCCC", high = color_acento, limits = c(0, 1), labels = scales::percent) +
  tema_eda +
  labs(
    title = "Mapa de calor de completitud de targets sensoriales por bloque de cata",
    subtitle = "Cada formulario mide un subconjunto distinto de descriptores; los vacios son estructurales.",
    x = NULL, y = NULL, fill = "% completo"
  )

ruta_completitud <- guardar_grafico_seguro(p_completitud, "02_mapa_completitud.png", ancho = 11, alto = 5)

completitud_muestras <- sensorial_individual %>%
  mutate(n_targets_disponibles = rowSums(!is.na(pick(all_of(target_cols))))) %>%
  group_by(grupo_sensorial, bloque_sensorial, muestra_cata, identificador_sensorial, tiene_quimica_confirmada) %>%
  summarise(
    n_evaluaciones = n(), n_targets_promedio = round(mean(n_targets_disponibles, na.rm = TRUE), 2),
    n_targets_min = min(n_targets_disponibles, na.rm = TRUE), n_targets_max = max(n_targets_disponibles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(grupo_sensorial, bloque_sensorial, muestra_cata)

# ------------------------------------------------------------
# ETAPA 4: Deteccion de valores atipicos (outliers) - solo se documentan

outliers_sensoriales <- detectar_outliers_iqr(sensorial_largo_completo, cols_grupo = "target") %>%
  select(
    observacion_sensorial_id, bloque_sensorial, muestra_cata, evaluador_id, nombre_catador,
    target, valor, limite_inferior, limite_superior
  ) %>%
  arrange(target, valor)

resumen_outliers <- outliers_sensoriales %>%
  count(target, name = "n_outliers") %>%
  arrange(desc(n_outliers))

p_box_bloque <- sensorial_largo_completo %>%
  filter(target %in% targets_principales, !is.na(valor)) %>%
  ggplot(aes(x = bloque_sensorial, y = valor)) +
  geom_boxplot(fill = color_acento, alpha = 0.5, outlier.colour = color_alerta) +
  facet_wrap(~target) +
  tema_eda +
  labs(title = "Targets principales por bloque sensorial", x = NULL, y = "Puntaje")

ruta_boxplot_bloque <- guardar_grafico_seguro(p_box_bloque, "03_boxplots_targets_por_bloque.png", ancho = 10, alto = 7)

rutas_boxplot_muestra <- character(0)
for (b in unique(sensorial_largo_completo$bloque_sensorial)) {
  datos_b <- sensorial_largo_completo %>% filter(bloque_sensorial == b, target %in% targets_principales, !is.na(valor))
  if (nrow(datos_b) == 0) next

  p <- ggplot(datos_b, aes(x = muestra_cata, y = valor)) +
    geom_boxplot(fill = color_acento, alpha = 0.5, outlier.colour = color_alerta) +
    facet_wrap(~target) +
    tema_eda +
    labs(title = paste0("Targets principales por muestra - ", b), x = NULL, y = "Puntaje")

  rutas_boxplot_muestra <- c(
    rutas_boxplot_muestra,
    guardar_grafico_seguro(p, paste0("04_boxplots_muestra_", b, ".png"), ancho = 9, alto = 6)
  )
}

# ------------------------------------------------------------
# ETAPA 5: Variabilidad y correlacion entre targets

variabilidad_principales <- sensorial_largo_completo %>%
  filter(target %in% targets_principales) %>%
  group_by(grupo_sensorial, bloque_sensorial, muestra_cata, identificador_sensorial, target) %>%
  summarise(
    n_evaluaciones = n(), n_con_valor = sum(!is.na(valor)), media = media_segura(valor),
    desviacion_estandar = sd_segura(valor), minimo = min_seguro(valor), mediana = mediana_segura(valor),
    maximo = max_seguro(valor),
    rango = ifelse(is.na(minimo) | is.na(maximo), NA_real_, maximo - minimo), .groups = "drop"
  ) %>%
  mutate(
    variabilidad_clase = case_when(
      is.na(desviacion_estandar) ~ "no_estimable",
      desviacion_estandar < 0.5 ~ "baja",
      desviacion_estandar >= 0.5 & desviacion_estandar < 1 ~ "media",
      desviacion_estandar >= 1 ~ "alta",
      TRUE ~ "revisar"
    )
  ) %>%
  arrange(desc(desviacion_estandar), bloque_sensorial, muestra_cata, target)

matriz_targets <- sensorial_individual[, target_cols, drop = FALSE]
matriz_targets[] <- lapply(matriz_targets, num_seguro)
vars_con_datos <- names(matriz_targets)[colSums(!is.na(matriz_targets)) >= 3]
matriz_targets <- matriz_targets[, vars_con_datos, drop = FALSE]

cor_targets <- suppressWarnings(cor(matriz_targets, use = "pairwise.complete.obs", method = "pearson"))
cor_targets_largo <- as.data.frame(as.table(cor_targets), stringsAsFactors = FALSE)
names(cor_targets_largo) <- c("target_1", "target_2", "correlacion")

correlaciones_targets_altas <- cor_targets_largo %>%
  filter(target_1 != target_2) %>%
  mutate(
    par = ifelse(target_1 < target_2, paste(target_1, target_2, sep = "***"), paste(target_2, target_1, sep = "***")),
    abs_correlacion = abs(correlacion)
  ) %>%
  distinct(par, .keep_all = TRUE) %>%
  filter(!is.na(correlacion)) %>%
  select(target_1, target_2, correlacion, abs_correlacion) %>%
  arrange(desc(abs_correlacion))

p_cor_targets <- ggplot(cor_targets_largo, aes(x = target_1, y = target_2, fill = correlacion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", correlacion)), size = 2.3) +
  scale_fill_gradient2(low = color_alerta, mid = "white", high = color_acento, midpoint = 0, limits = c(-1, 1), na.value = "grey90") +
  tema_eda +
  theme(axis.text.y = element_text(size = 7)) +
  labs(title = "Correlacion entre targets sensoriales (evaluaciones individuales)", x = NULL, y = NULL, fill = "r")

ruta_correlacion_targets <- guardar_grafico_seguro(p_cor_targets, "05_correlacion_targets.png", ancho = 11, alto = 10)

# ------------------------------------------------------------
# Perfil promedio por muestra y foco en targets de modelamiento (se mantienen del analisis anterior)

perfil_muestras <- sensorial_agregado %>%
  select(
    grupo_sensorial, bloque_sensorial, archivo_sensorial, muestra_cata, identificador_sensorial,
    tiene_quimica_confirmada, n_evaluadores, tipo_panel, all_of(target_cols)
  ) %>%
  arrange(grupo_sensorial, bloque_sensorial, muestra_cata)

perfil_muestras_largo <- perfil_muestras %>%
  pivot_longer(cols = all_of(target_cols), names_to = "target", values_to = "valor_promedio") %>%
  filter(!is.na(valor_promedio)) %>%
  arrange(grupo_sensorial, bloque_sensorial, muestra_cata, target)

targets_modelamiento <- intersect(c("y_frutal_comun", "y_fenolico_comun"), target_cols)

targets_modelamiento_resumen <- sensorial_largo_completo %>%
  filter(target %in% targets_modelamiento) %>%
  group_by(target, bloque_sensorial, tiene_quimica_confirmada) %>%
  summarise(
    n_total = n(), n_con_valor = sum(!is.na(valor)), n_muestras = n_distinct(muestra_cata),
    media = media_segura(valor), desviacion_estandar = sd_segura(valor), minimo = min_seguro(valor),
    mediana = mediana_segura(valor), maximo = max_seguro(valor),
    proporcion_con_valor = round(n_con_valor / n_total, 4), .groups = "drop"
  ) %>%
  arrange(target, bloque_sensorial)

targets_modelamiento_muestra <- sensorial_agregado %>%
  select(
    grupo_sensorial, bloque_sensorial, muestra_cata, identificador_sensorial,
    tiene_quimica_confirmada, n_evaluadores, any_of(targets_modelamiento)
  ) %>%
  arrange(grupo_sensorial, bloque_sensorial, muestra_cata)

sensorial_sin_quimica <- sensorial_sin_quimica %>%
  mutate(
    tiene_quimica_confirmada = estandarizar_logico(tiene_quimica_confirmada),
    grupo_sensorial = clasificar_bloque_sensorial(bloque_sensorial)
  ) %>%
  arrange(grupo_sensorial, bloque_sensorial, muestra_cata, evaluador_id)

# ------------------------------------------------------------
# ETAPA 6: Perfil por categoria de catador
# Solo "cata_social_enero_2025" registra la categoria del catador
# (Experto/Sommelier, Aficionado al whisky, Consumidor General de Destilados).

perfil_categoria_catador <- sensorial_largo_completo %>%
  filter(!is.na(categoria_catador), !is.na(valor)) %>%
  group_by(grupo_sensorial, bloque_sensorial, muestra_cata, identificador_sensorial, categoria_catador, target) %>%
  summarise(
    n_evaluaciones = n(), media = media_segura(valor), desviacion_estandar = sd_segura(valor),
    .groups = "drop"
  ) %>%
  arrange(bloque_sensorial, muestra_cata, target, categoria_catador)

consistencia_categoria_catador <- perfil_categoria_catador %>%
  group_by(categoria_catador) %>%
  summarise(
    n_evaluaciones_totales = sum(n_evaluaciones),
    sd_promedio = round(mean(desviacion_estandar, na.rm = TRUE), 3),
    media_promedio = round(mean(media, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(sd_promedio)

p_categoria_catador <- sensorial_largo_completo %>%
  filter(!is.na(categoria_catador), target %in% targets_principales, !is.na(valor)) %>%
  ggplot(aes(x = categoria_catador, y = valor)) +
  geom_boxplot(fill = color_acento, alpha = 0.5, outlier.colour = color_alerta) +
  facet_wrap(~target) +
  tema_eda +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(
    title = "Targets principales segun categoria de catador (cata social enero 2025)",
    x = NULL, y = "Puntaje"
  )

ruta_categoria_catador <- guardar_grafico_seguro(p_categoria_catador, "06_boxplot_categoria_catador.png", ancho = 9, alto = 6)

# ------------------------------------------------------------
# ETAPA 7: Radar charts reproducibles (perfil sensorial por muestra y por categoria de catador)

grupo_aroma <- intersect(
  c(
    "y_aroma_ahumado", "y_aroma_medicinal", "y_aroma_terroso", "y_aroma_amaderado",
    "y_aroma_cafe", "y_aroma_frutal", "y_aroma_vainilla", "y_aroma_cereal"
  ),
  target_cols
)
grupo_sabor <- intersect(
  c(
    "y_sabor_ahumado", "y_sabor_medicinal", "y_sabor_terroso", "y_sabor_amaderado",
    "y_sabor_cafe", "y_sabor_frutal", "y_sabor_vainilla", "y_sabor_cereal", "y_sabor_sensacion_boca"
  ),
  target_cols
)
grupo_regusto <- intersect(
  c(
    "y_regusto_duracion", "y_regusto_ahumado", "y_regusto_complejidad",
    "y_global_integracion_ahumado", "y_global_armonia", "y_global_tomabilidad"
  ),
  target_cols
)

grupos_radar <- list("Aroma" = grupo_aroma, "Sabor" = grupo_sabor, "Regusto-Final" = grupo_regusto)
grupos_radar <- grupos_radar[sapply(grupos_radar, length) >= 3]

etiquetas_descriptor <- c(
  y_aroma_ahumado = "Intensidad Ahumado", y_aroma_medicinal = "Medicinal", y_aroma_terroso = "Terroso",
  y_aroma_amaderado = "Amaderado", y_aroma_cafe = "Cafe", y_aroma_frutal = "Frutal",
  y_aroma_vainilla = "Vainilla", y_aroma_cereal = "Cereal",
  y_sabor_ahumado = "Intensidad de ahumado", y_sabor_medicinal = "Medicinal", y_sabor_terroso = "Terroso",
  y_sabor_amaderado = "Amaderado", y_sabor_cafe = "Cafe", y_sabor_frutal = "Frutal",
  y_sabor_vainilla = "Vainilla", y_sabor_cereal = "Cereal", y_sabor_sensacion_boca = "Sensacion en boca",
  y_regusto_duracion = "Duracion", y_regusto_ahumado = "Ahumado", y_regusto_complejidad = "Complejidad",
  y_global_integracion_ahumado = "Integracion del ahumado", y_global_armonia = "Armonia",
  y_global_tomabilidad = "Tomabilidad"
)

grafico_radar <- function(datos_largo, col_grupo, col_valor, orden_targets, titulo, valor_max = 5) {
  n <- length(orden_targets)

  datos <- datos_largo %>%
    filter(target %in% orden_targets, !is.na(.data[[col_valor]])) %>%
    mutate(target = factor(target, levels = orden_targets), angulo = as.numeric(target)) %>%
    arrange(.data[[col_grupo]], angulo)

  cierre <- datos %>% filter(angulo == 1) %>% mutate(angulo = n + 1)
  datos <- bind_rows(datos, cierre)

  ggplot(datos, aes(x = angulo, y = .data[[col_valor]], color = .data[[col_grupo]], group = .data[[col_grupo]])) +
    geom_polygon(aes(fill = .data[[col_grupo]]), alpha = 0.05, linewidth = 0) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.3) +
    coord_polar(theta = "x") +
    scale_x_continuous(breaks = 1:n, labels = unname(etiquetas_descriptor[orden_targets]), limits = c(1, n + 1)) +
    scale_y_continuous(limits = c(0, valor_max)) +
    tema_eda +
    theme(axis.text.x = element_text(angle = 0, size = 6.5), panel.grid.major.x = element_line(color = "grey85")) +
    labs(title = titulo, x = NULL, y = NULL, color = NULL, fill = NULL)
}

# Radar por muestra: compara el perfil promedio de todas las muestras de un mismo bloque
rutas_radar_muestras <- character(0)
for (b in unique(perfil_muestras_largo$bloque_sensorial)) {
  datos_b <- perfil_muestras_largo %>% filter(bloque_sensorial == b) %>% rename(valor = valor_promedio)
  if (n_distinct(datos_b$identificador_sensorial) < 2) next

  for (nombre_grupo in names(grupos_radar)) {
    orden <- grupos_radar[[nombre_grupo]]
    datos_bg <- datos_b %>% filter(target %in% orden)
    if (nrow(datos_bg) == 0 || n_distinct(datos_bg$target) < 3) next

    p <- grafico_radar(
      datos_bg, col_grupo = "identificador_sensorial", col_valor = "valor", orden_targets = orden,
      titulo = paste0(nombre_grupo, " - ", b)
    )
    archivo <- paste0("07_radar_muestras_", b, "_", limpiar_nombre_variable(nombre_grupo), ".png")
    rutas_radar_muestras <- c(rutas_radar_muestras, guardar_grafico_seguro(p, archivo, ancho = 7, alto = 6))
  }
}

# Radar por categoria de catador: para cada muestra de cata_social_enero_2025,
# compara Experto/Sommelier, Aficionado y Consumidor General contra el promedio general
rutas_radar_categoria <- character(0)
if (nrow(perfil_categoria_catador) > 0) {
  datos_categoria_largo <- sensorial_largo_completo %>%
    filter(!is.na(categoria_catador), !is.na(valor)) %>%
    group_by(bloque_sensorial, muestra_cata, identificador_sensorial, categoria_catador, target) %>%
    summarise(valor = media_segura(valor), .groups = "drop")

  datos_promedio_largo <- sensorial_largo_completo %>%
    filter(bloque_sensorial == "cata_social_enero_2025", !is.na(valor)) %>%
    group_by(bloque_sensorial, muestra_cata, identificador_sensorial, target) %>%
    summarise(valor = media_segura(valor), .groups = "drop") %>%
    mutate(categoria_catador = "Promedio")

  datos_radar_categoria <- bind_rows(datos_categoria_largo, datos_promedio_largo)

  for (m in unique(datos_radar_categoria$identificador_sensorial)) {
    datos_m <- datos_radar_categoria %>% filter(identificador_sensorial == m)

    for (nombre_grupo in names(grupos_radar)) {
      orden <- grupos_radar[[nombre_grupo]]
      datos_mg <- datos_m %>% filter(target %in% orden)
      if (nrow(datos_mg) == 0 || n_distinct(datos_mg$target) < 3) next

      p <- grafico_radar(
        datos_mg, col_grupo = "categoria_catador", col_valor = "valor", orden_targets = orden,
        titulo = paste0(nombre_grupo, " - ", m, " por categoria de catador")
      )
      archivo <- paste0(
        "08_radar_categoria_", limpiar_nombre_variable(m), "_", limpiar_nombre_variable(nombre_grupo), ".png"
      )
      rutas_radar_categoria <- c(rutas_radar_categoria, guardar_grafico_seguro(p, archivo, ancho = 7, alto = 6))
    }
  }
}

# ------------------------------------------------------------
# ETAPA 8: PCA / biplot de targets sensoriales por bloque
# Cada formulario mide un subconjunto distinto de descriptores, igual que en
# el analisis quimico (script 10): el PCA se calcula por bloque, no de forma global.

preparar_numerica_sensorial <- function(df, vars) {
  x <- df[, vars, drop = FALSE]
  x[] <- lapply(x, num_seguro)

  vars_validas <- names(x)[colSums(!is.na(x)) > 0]
  x <- x[, vars_validas, drop = FALSE]

  vars_con_varianza <- names(x)[sapply(x, function(z) {
    z <- z[!is.na(z)]
    length(unique(z)) > 1
  })]

  x[, vars_con_varianza, drop = FALSE]
}

# T2 de Hotelling y Q-residuals (SPE) sobre un PCA ya calculado con prcomp().
# n_comp = numero de componentes "retenidos" (el plano visualizado en el biplot);
# lo que queda fuera de esos componentes es lo que mide el Q-residual.
calcular_t2_q <- function(pca, n_comp = 2, alpha = 0.05) {
  n <- nrow(pca$x)
  k_total <- ncol(pca$x)
  n_comp <- min(n_comp, k_total)

  x_reconstruido_total <- pca$x %*% t(pca$rotation)
  scores_a <- pca$x[, seq_len(n_comp), drop = FALSE]
  rotation_a <- pca$rotation[, seq_len(n_comp), drop = FALSE]
  reconstruido_a <- scores_a %*% t(rotation_a)
  q_resid <- rowSums((x_reconstruido_total - reconstruido_a)^2)

  lambda_a <- (pca$sdev[seq_len(n_comp)])^2
  lambda_a[lambda_a <= 1e-12] <- NA_real_
  t2 <- rowSums(sweep(scores_a^2, 2, lambda_a, "/"), na.rm = TRUE)

  t2_lim <- if (n > n_comp) {
    (n_comp * (n - 1) / (n - n_comp)) * qf(1 - alpha, n_comp, n - n_comp)
  } else {
    NA_real_
  }

  lambda_resid <- (pca$sdev[seq_len(k_total)])^2
  lambda_resid <- lambda_resid[(n_comp + 1):k_total]
  lambda_resid <- lambda_resid[!is.na(lambda_resid) & lambda_resid > 1e-10]

  q_lim <- NA_real_
  if (length(lambda_resid) == 0) {
    q_lim <- 0
  } else {
    theta1 <- sum(lambda_resid)
    theta2 <- sum(lambda_resid^2)
    theta3 <- sum(lambda_resid^3)
    if (theta1 > 0 && theta2 > 0) {
      h0 <- 1 - (2 * theta1 * theta3) / (3 * theta2^2)
      if (is.finite(h0) && h0 != 0) {
        c_alpha <- qnorm(1 - alpha)
        termino <- (c_alpha * sqrt(2 * theta2 * h0^2) / theta1) + 1 + (theta2 * h0 * (h0 - 1)) / theta1^2
        if (is.finite(termino) && termino > 0) q_lim <- theta1 * termino^(1 / h0)
      }
    }
  }

  list(t2 = t2, q = q_resid, t2_lim = t2_lim, q_lim = q_lim, n_comp = n_comp)
}

calcular_pca_sensorial <- function(df, vars, grupo_pca) {
  x <- preparar_numerica_sensorial(df, vars)

  if (nrow(x) < 3 || ncol(x) < 2) {
    return(list(
      resumen = data.frame(
        grupo_pca = grupo_pca, estado = "no_calculado",
        n_filas = nrow(x), n_variables = ncol(x),
        pc1_varianza = NA_real_, pc2_varianza = NA_real_, pc1_pc2_varianza = NA_real_,
        motivo = "Se requieren al menos 3 muestras y 2 targets con variabilidad.",
        stringsAsFactors = FALSE
      ),
      scores = data.frame(), cargas = data.frame()
    ))
  }

  for (v in names(x)) x[[v]][is.na(x[[v]])] <- mean(x[[v]], na.rm = TRUE)

  pca <- tryCatch(prcomp(x, center = TRUE, scale. = TRUE), error = function(e) NULL)

  if (is.null(pca)) {
    return(list(
      resumen = data.frame(
        grupo_pca = grupo_pca, estado = "no_calculado",
        n_filas = nrow(x), n_variables = ncol(x),
        pc1_varianza = NA_real_, pc2_varianza = NA_real_, pc1_pc2_varianza = NA_real_,
        motivo = "Error al calcular PCA.", stringsAsFactors = FALSE
      ),
      scores = data.frame(), cargas = data.frame()
    ))
  }

  var_exp <- pca$sdev^2 / sum(pca$sdev^2)
  t2q <- calcular_t2_q(pca, n_comp = 2)

  resumen <- data.frame(
    grupo_pca = grupo_pca, estado = "calculado",
    n_filas = nrow(x), n_variables = ncol(x),
    pc1_varianza = ifelse(length(var_exp) >= 1, var_exp[1], NA_real_),
    pc2_varianza = ifelse(length(var_exp) >= 2, var_exp[2], NA_real_),
    pc1_pc2_varianza = ifelse(length(var_exp) >= 2, sum(var_exp[1:2]), NA_real_),
    t2_n_comp = t2q$n_comp, t2_limite = t2q$t2_lim, q_limite = t2q$q_lim,
    motivo = NA_character_, stringsAsFactors = FALSE
  )

  scores <- as.data.frame(pca$x)
  scores <- scores[, intersect(c("PC1", "PC2", "PC3"), names(scores)), drop = FALSE]
  scores$muestra_cata <- df$muestra_cata
  scores$identificador_sensorial <- df$identificador_sensorial
  scores$grupo_pca <- grupo_pca
  scores$t2_hotelling <- t2q$t2
  scores$q_residual <- t2q$q
  scores$excede_t2 <- if (is.na(t2q$t2_lim)) NA else t2q$t2 > t2q$t2_lim
  scores$excede_q <- if (is.na(t2q$q_lim)) NA else t2q$q > t2q$q_lim
  scores <- scores %>% select(grupo_pca, muestra_cata, identificador_sensorial, everything())

  cargas <- as.data.frame(pca$rotation)
  cargas <- cargas[, intersect(c("PC1", "PC2", "PC3"), names(cargas)), drop = FALSE]
  cargas$target <- rownames(cargas)
  cargas$grupo_pca <- grupo_pca
  rownames(cargas) <- NULL
  cargas <- cargas %>% select(grupo_pca, target, everything())

  list(resumen = resumen, scores = scores, cargas = cargas, t2_lim = t2q$t2_lim, q_lim = t2q$q_lim)
}

lista_pca_sensorial <- list()
rutas_biplot_sensorial <- character(0)
rutas_t2q_sensorial <- character(0)

for (b in unique(sensorial_agregado$bloque_sensorial)) {
  datos_b <- sensorial_agregado %>% filter(bloque_sensorial == b)
  vars_b <- target_cols[colSums(!is.na(datos_b[, target_cols, drop = FALSE])) > 0]

  lista_pca_sensorial[[b]] <- calcular_pca_sensorial(df = datos_b, vars = vars_b, grupo_pca = b)

  pca_b <- lista_pca_sensorial[[b]]
  if (pca_b$resumen$estado[1] == "calculado" && all(c("PC1", "PC2") %in% names(pca_b$scores))) {
    scores_b <- pca_b$scores
    cargas_b <- pca_b$cargas

    escala <- 1
    max_score <- max(abs(c(scores_b$PC1, scores_b$PC2)), na.rm = TRUE)
    max_carga <- max(abs(c(cargas_b$PC1, cargas_b$PC2)), na.rm = TRUE)
    if (is.finite(max_score) && is.finite(max_carga) && max_carga > 0) {
      escala <- (max_score / max_carga) * 0.8
    }

    p_biplot <- ggplot() +
      geom_hline(yintercept = 0, color = "grey80") +
      geom_vline(xintercept = 0, color = "grey80") +
      geom_point(data = scores_b, aes(x = PC1, y = PC2), color = color_acento, size = 2.5) +
      geom_text(
        data = scores_b, aes(x = PC1, y = PC2, label = identificador_sensorial),
        color = color_acento, vjust = -0.8, size = 3
      ) +
      geom_segment(
        data = cargas_b, aes(x = 0, y = 0, xend = PC1 * escala, yend = PC2 * escala),
        arrow = arrow(length = unit(0.15, "cm")), color = color_alerta
      ) +
      geom_text(
        data = cargas_b, aes(x = PC1 * escala * 1.1, y = PC2 * escala * 1.1, label = target),
        size = 2.2, color = color_alerta
      ) +
      scale_x_continuous(expand = expansion(mult = 0.25)) +
      scale_y_continuous(expand = expansion(mult = 0.25)) +
      tema_eda +
      theme(axis.text.x = element_text(angle = 0)) +
      labs(
        title = paste0("Biplot PCA sensorial - ", b),
        x = paste0("PC1 (", round(pca_b$resumen$pc1_varianza[1] * 100, 1), "%)"),
        y = paste0("PC2 (", round(pca_b$resumen$pc2_varianza[1] * 100, 1), "%)")
      )

    rutas_biplot_sensorial <- c(
      rutas_biplot_sensorial,
      guardar_grafico_seguro(p_biplot, paste0("09_biplot_pca_sensorial_", b, ".png"), ancho = 8, alto = 7)
    )

    if (!is.na(pca_b$t2_lim) && !is.na(pca_b$q_lim) && all(c("t2_hotelling", "q_residual") %in% names(scores_b))) {
      scores_b <- scores_b %>%
        mutate(es_outlier_t2q = (excede_t2 %in% TRUE) | (excede_q %in% TRUE))

      p_t2q <- ggplot(scores_b, aes(x = t2_hotelling, y = q_residual)) +
        geom_vline(xintercept = pca_b$t2_lim, linetype = "dashed", color = color_alerta) +
        geom_hline(yintercept = pca_b$q_lim, linetype = "dashed", color = color_alerta) +
        geom_point(aes(color = es_outlier_t2q), size = 2.5) +
        geom_text(
          data = scores_b %>% filter(es_outlier_t2q),
          aes(label = identificador_sensorial), size = 2.6, vjust = -0.8, color = color_alerta
        ) +
        scale_color_manual(values = c(`TRUE` = color_alerta, `FALSE` = color_acento), guide = "none") +
        scale_x_continuous(expand = expansion(mult = 0.22)) +
        scale_y_continuous(expand = expansion(mult = 0.12)) +
        tema_eda +
        theme(axis.text.x = element_text(angle = 0)) +
        labs(
          title = paste0("Distancia T2 - Q residual sensorial - ", b),
          subtitle = "Lineas punteadas = limites al 95% (Hotelling T2 / Jackson-Mudholkar Q)",
          x = "T2 de Hotelling", y = "Q-residual (SPE)"
        )

      rutas_t2q_sensorial <- c(
        rutas_t2q_sensorial,
        guardar_grafico_seguro(p_t2q, paste0("10_distancia_t2_q_", b, ".png"), ancho = 8, alto = 6)
      )
    }
  }
}

pca_sensorial_resumen <- bind_rows(lapply(lista_pca_sensorial, function(x) x$resumen))
pca_sensorial_scores <- bind_rows(lapply(lista_pca_sensorial, function(x) x$scores))
pca_sensorial_cargas <- bind_rows(lapply(lista_pca_sensorial, function(x) x$cargas))

t2q_sensorial_outliers <- pca_sensorial_scores %>%
  filter(excede_t2 %in% TRUE | excede_q %in% TRUE) %>%
  select(grupo_pca, muestra_cata, identificador_sensorial, t2_hotelling, excede_t2, q_residual, excede_q) %>%
  arrange(grupo_pca, desc(t2_hotelling))

# ------------------------------------------------------------
# ETAPA 9: Concordancia entre evaluadores (ICC(1) de una via, sin paquetes adicionales)
# Mide si el promedio por muestra es confiable frente al ruido entre evaluadores:
# ICC alto = los evaluadores diferencian las muestras de forma consistente en ese target.

calcular_icc1 <- function(grupo, valor) {
  datos <- data.frame(grupo = grupo, valor = valor, stringsAsFactors = FALSE)
  datos <- datos[!is.na(datos$valor) & !is.na(datos$grupo), ]

  p <- length(unique(datos$grupo))
  n_total <- nrow(datos)

  if (p < 2 || n_total < p + 1) {
    return(c(icc1 = NA_real_, n_grupos = p, n_observaciones = n_total))
  }

  medias_grupo <- tapply(datos$valor, datos$grupo, mean)
  n_grupo <- tapply(datos$valor, datos$grupo, length)

  if (all(n_grupo <= 1)) {
    return(c(icc1 = NA_real_, n_grupos = p, n_observaciones = n_total))
  }

  media_global <- mean(datos$valor)
  ssb <- sum(n_grupo * (medias_grupo - media_global)^2)
  ssw <- sum(sapply(names(medias_grupo), function(g) {
    sum((datos$valor[datos$grupo == g] - medias_grupo[g])^2)
  }))

  df_b <- p - 1
  df_w <- n_total - p
  if (df_w <= 0) return(c(icc1 = NA_real_, n_grupos = p, n_observaciones = n_total))

  msb <- ssb / df_b
  msw <- ssw / df_w
  n0 <- (n_total - sum(n_grupo^2) / n_total) / df_b

  if (n0 <= 0 || (msb + (n0 - 1) * msw) == 0) {
    return(c(icc1 = NA_real_, n_grupos = p, n_observaciones = n_total))
  }

  icc1 <- (msb - msw) / (msb + (n0 - 1) * msw)
  c(icc1 = icc1, n_grupos = p, n_observaciones = n_total)
}

interpretar_icc <- function(icc1) {
  case_when(
    is.na(icc1) ~ "no_calculable",
    icc1 < 0 ~ "sin concordancia (mas variacion entre evaluadores que entre muestras)",
    icc1 < 0.2 ~ "concordancia baja",
    icc1 < 0.5 ~ "concordancia moderada",
    icc1 < 0.75 ~ "concordancia buena",
    TRUE ~ "concordancia excelente"
  )
}

concordancia_muestras <- sensorial_largo_completo %>%
  filter(!is.na(valor)) %>%
  group_by(grupo_sensorial, bloque_sensorial, target) %>%
  summarise(resultado = list(calcular_icc1(muestra_cata, valor)), .groups = "drop") %>%
  mutate(
    icc1 = sapply(resultado, function(x) unname(x["icc1"])),
    n_muestras = sapply(resultado, function(x) unname(x["n_grupos"])),
    n_observaciones = sapply(resultado, function(x) unname(x["n_observaciones"]))
  ) %>%
  select(-resultado) %>%
  filter(!is.na(icc1)) %>%
  mutate(interpretacion = interpretar_icc(icc1)) %>%
  arrange(bloque_sensorial, desc(icc1))

concordancia_categoria_catador <- sensorial_largo_completo %>%
  filter(!is.na(valor), !is.na(categoria_catador), target %in% targets_principales) %>%
  group_by(categoria_catador, target) %>%
  summarise(resultado = list(calcular_icc1(muestra_cata, valor)), .groups = "drop") %>%
  mutate(
    icc1 = sapply(resultado, function(x) unname(x["icc1"])),
    n_muestras = sapply(resultado, function(x) unname(x["n_grupos"])),
    n_observaciones = sapply(resultado, function(x) unname(x["n_observaciones"]))
  ) %>%
  select(-resultado) %>%
  filter(!is.na(icc1)) %>%
  mutate(interpretacion = interpretar_icc(icc1)) %>%
  arrange(target, desc(icc1))

# ------------------------------------------------------------
# Notas metodologicas

notas <- data.frame(
  punto = c(
    "1. Analisis descriptivo", "2. Ajuste de tipos", "3. Datos ausentes", "4. Valores atipicos",
    "5. Correlacion entre targets", "6. Perfil por categoria de catador", "7. Radar charts",
    "8. PCA sensorial por bloque", "9. Concordancia entre evaluadores (ICC)",
    "10. T2 de Hotelling y Q-residuals",
    "Unidad individual", "Unidad agregada", "Uso en modelamiento"
  ),
  descripcion = c(
    "Resumen global, por bloque y por muestra, mas histogramas de distribucion de cada target sensorial.",
    "Los targets y_* se verifican como numericos.",
    "Se calcula completitud por target y bloque y se visualiza como mapa de calor; cada formulario mide un subconjunto distinto de descriptores.",
    "Se detectan outliers con la regla IQR por target. Se documentan en la hoja de outliers y se marcan en los boxplots, pero NO se eliminan: son evaluaciones reales de catadores.",
    "Se calcula la correlacion entre todos los targets sensoriales (a nivel de evaluacion individual) para ver que descriptores se mueven juntos.",
    "Solo 'cata_social_enero_2025' registra la categoria del catador (Experto/Sommelier, Aficionado al whisky, Consumidor General de Destilados). Se compara media y desviacion estandar de los targets principales por categoria.",
    "Se reproducen en R (ggplot2 + coord_polar) los graficos radar que antes se armaban a mano en Excel: uno por bloque comparando el perfil promedio de sus muestras (Aroma/Sabor/Regusto-Final), y uno por muestra de cata_social comparando las 3 categorias de catador contra el promedio general.",
    "PCA (prcomp, centrado y escalado) de los targets sensoriales calculado por bloque sensorial, ya que cada formulario mide un subconjunto distinto de descriptores (mismo criterio que el PCA quimico del script 10).",
    "ICC(1) de una via (formula ANOVA de efectos aleatorios, sin paquetes adicionales) por bloque y target: mide si los evaluadores diferencian las muestras de forma consistente (ICC alto) o si el ruido entre evaluadores domina (ICC bajo/negativo). Tambien se calcula por categoria de catador en cata_social para ver si expertos concuerdan mas que consumidores generales.",
    "Diagnostico multivariado (Jackson & Mudholkar, 1979; Jackson, 1991) sobre el mismo PCA sensorial de cada bloque (2 componentes, el plano del biplot): T2 mide que tan lejos esta una muestra del centro dentro de ese plano; Q-residual mide que tan mal la explica ese plano. No se aplica el test Q de Dixon a lo sensorial: esta diseñado para replicas instrumentales de una sola magnitud, no para puntajes ordinales de multiples catadores (para eso ya esta el ICC de la etapa 9).",
    "La unidad individual corresponde a una evaluacion realizada por un catador sobre una muestra.",
    "La unidad agregada corresponde al promedio de evaluadores por muestra sensorial.",
    "Para el modelamiento principal se recomienda usar la matriz pura agregada; y_fenolico_comun tiene mejor cobertura que y_frutal_comun."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Exportar

ruta_salida <- guardar_excel_seguro(
  hojas = list(
    "00_resumen" = resumen_global,
    "01_resumen_bloques" = resumen_bloques,
    "02_resumen_muestras" = resumen_muestras,
    "03_descriptivos_targets" = descriptivos_targets,
    "04_descriptivos_bloques" = descriptivos_bloques,
    "05_tipos_targets" = tipos_targets,
    "06_completitud_muestras" = completitud_muestras,
    "07_outliers_detectados" = outliers_sensoriales,
    "08_resumen_outliers" = resumen_outliers,
    "09_variabilidad_principales" = variabilidad_principales,
    "10_correlacion_targets" = correlaciones_targets_altas,
    "11_perfil_muestras" = perfil_muestras,
    "12_perfil_muestras_largo" = perfil_muestras_largo,
    "13_targets_modelamiento" = targets_modelamiento_resumen,
    "14_targets_muestra" = targets_modelamiento_muestra,
    "15_sensorial_sin_quimica" = sensorial_sin_quimica,
    "16_perfil_categoria_catador" = perfil_categoria_catador,
    "17_consistencia_catador" = consistencia_categoria_catador,
    "18_pca_sensorial_resumen" = pca_sensorial_resumen,
    "19_pca_sensorial_scores" = pca_sensorial_scores,
    "20_pca_sensorial_cargas" = pca_sensorial_cargas,
    "21_concordancia_muestras" = concordancia_muestras,
    "22_concordancia_catador" = concordancia_categoria_catador,
    "23_t2q_outliers" = t2q_sensorial_outliers,
    "24_notas" = notas
  ),
  ruta_salida = ruta_salida_base
)

cat("\nANALISIS SENSORIAL FINALIZADO\n")

cat("\nArchivo generado:\n")
cat(ruta_salida, "\n")

cat("\nGraficos generados en:\n")
cat(dir_outputs_exploratorio, "\n")

cat("\nResumen:\n")
print(resumen_global)

cat("\nOutliers detectados por target:\n")
print(resumen_outliers)

cat("\nCorrelaciones mas altas entre targets:\n")
print(head(correlaciones_targets_altas, 10))

cat("\nConsistencia por categoria de catador (menor sd_promedio = mas consistente):\n")
print(consistencia_categoria_catador)

cat("\nConcordancia entre evaluadores por muestra (ICC1, top 10):\n")
print(head(concordancia_muestras, 10))

cat("\nProceso terminado correctamente.\n")
