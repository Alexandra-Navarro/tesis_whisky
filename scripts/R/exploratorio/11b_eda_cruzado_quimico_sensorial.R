# ============================================================
# 11b_eda_cruzado_quimico_sensorial.R
# Analisis exploratorio de datos (EDA) cruzado quimico-sensorial
# Tesis whisky chileno - modelamiento quimico-sensorial
#
# 10_analisis_quimico.R y 11_analisis_sensorial.R analizan cada
# lado por separado. Este script da una primera mirada exploratoria
# a la relacion predictor (x_*) - target (y_*) usando la matriz pura
# (M_pura, escenario principal), ANTES del diagnostico formal de
# colinealidad y modelamiento (scripts 13/13b en adelante):
#   1. Lectura de matriz_pura.xlsx (escenario M_pura)
#   2. Correlacion cruzada x_* vs y_* (Pearson, pairwise)
#   3. Heatmap de correlacion para los targets principales
#   4. Heatmap de correlacion por grupo quimico (tecnica analitica)
#   5. Tabla de pares predictor-target con mayor correlacion
#
# Es exploratorio, no reemplaza el diagnostico de colinealidad ni
# el modelamiento supervisado (ver scripts/R/modelamiento/13*).
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

cat("\nANALISIS EXPLORATORIO CRUZADO QUIMICO-SENSORIAL\n")

# ------------------------------------------------------------
# Rutas

ruta_matriz_pura <- file.path(dir_procesamiento, "matriz_pura.xlsx")
ruta_salida_base <- file.path(dir_procesamiento, "analisis_cruzado_quimico_sensorial.xlsx")

dir_outputs_exploratorio <- file.path(dir_proyecto, "outputs", "exploratorio", "cruzado")
dir.create(dir_outputs_exploratorio, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(ruta_matriz_pura)) {
  stop(paste(
    "No existe la matriz pura:", ruta_matriz_pura,
    "\nEjecuta primero scripts/R/procesamiento/04_matriz_pura.R"
  ))
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

# ------------------------------------------------------------
# ETAPA 1: Lectura (escenario M_pura)

matriz_pura <- readxl::read_excel(ruta_matriz_pura, sheet = "01_matriz_pura") %>%
  as.data.frame(stringsAsFactors = FALSE)

x_cols <- names(matriz_pura)[str_detect(names(matriz_pura), "^x_")]
y_cols <- names(matriz_pura)[str_detect(names(matriz_pura), "^y_")]
targets_principales <- intersect(
  c("y_frutal_comun", "y_fenolico_comun", "y_ahumado_comun", "y_medicinal_comun"),
  y_cols
)

matriz_num <- matriz_pura[, c(x_cols, y_cols), drop = FALSE]
matriz_num[] <- lapply(matriz_num, num_seguro)

resumen_global <- data.frame(
  indicador = c(
    "Escenario", "Unidades analiticas (filas)", "Predictores quimicos (x_*)",
    "Targets sensoriales (y_*)", "Targets principales", "Uso de este analisis",
    "Advertencia metodologica"
  ),
  valor = c(
    "M_pura (quimica real + cata real agregada, evidencia principal)",
    nrow(matriz_pura), length(x_cols), length(y_cols),
    paste(targets_principales, collapse = ", "),
    "Vista exploratoria previa (EDA), no reemplaza el diagnostico de colinealidad ni el modelamiento supervisado.",
    "El VIF global no aplica por baja superposicion entre tecnicas analiticas (ver colinealidad_modelamiento.xlsx); esta hoja usa correlacion pareada simple, con n variable por par segun el bloque quimico disponible."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# ETAPA 2: Correlacion cruzada x_* vs y_* (Pearson, pairwise)

correlacion_cruzada <- expand.grid(predictor = x_cols, target = y_cols, stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(
    n_pares = sum(!is.na(matriz_num[[predictor]]) & !is.na(matriz_num[[target]])),
    correlacion = if (n_pares >= 5) {
      suppressWarnings(cor(matriz_num[[predictor]], matriz_num[[target]], use = "pairwise.complete.obs"))
    } else {
      NA_real_
    }
  ) %>%
  ungroup() %>%
  mutate(abs_correlacion = abs(correlacion)) %>%
  filter(!is.na(correlacion))

correlacion_cruzada_principales <- correlacion_cruzada %>%
  filter(target %in% targets_principales) %>%
  arrange(target, desc(abs_correlacion))

pares_correlacion_alta <- correlacion_cruzada %>%
  filter(abs_correlacion >= 0.5) %>%
  arrange(desc(abs_correlacion))

# ------------------------------------------------------------
# ETAPA 3: Heatmap de correlacion para los targets principales (todos los predictores)

p_heatmap_principales <- correlacion_cruzada_principales %>%
  ggplot(aes(x = target, y = predictor, fill = correlacion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", correlacion)), size = 2) +
  scale_fill_gradient2(low = color_alerta, mid = "white", high = color_acento, midpoint = 0, limits = c(-1, 1), na.value = "grey90") +
  tema_eda +
  theme(axis.text.y = element_text(size = 6), axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(
    title = "Correlacion predictor quimico - target sensorial principal (M_pura)",
    subtitle = "n de pares varia por predictor segun el bloque analitico disponible; ver columna n_pares en la hoja de datos.",
    x = NULL, y = NULL, fill = "r"
  )

ruta_heatmap_principales <- guardar_grafico_seguro(p_heatmap_principales, "01_heatmap_predictores_targets_principales.png", ancho = 8, alto = 11)

# ------------------------------------------------------------
# ETAPA 4: Heatmap de correlacion por grupo quimico (tecnica analitica)

clasificar_grupo_predictor <- function(predictor) {
  case_when(
    str_detect(predictor, "^x_gcfid") ~ "GC-FID",
    str_detect(predictor, "^x_gcms") ~ "GC-MS",
    str_detect(predictor, "^x_ju") ~ "JU",
    str_detect(predictor, "^x_folin") ~ "Fenoles totales",
    TRUE ~ "otro"
  )
}

correlacion_cruzada_principales <- correlacion_cruzada_principales %>%
  mutate(grupo_predictor = clasificar_grupo_predictor(predictor))

rutas_heatmap_grupo <- character(0)
for (g in sort(unique(correlacion_cruzada_principales$grupo_predictor))) {
  datos_g <- correlacion_cruzada_principales %>% filter(grupo_predictor == g)
  if (nrow(datos_g) == 0) next

  p_g <- ggplot(datos_g, aes(x = target, y = predictor, fill = correlacion)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", correlacion)), size = 2.3) +
    scale_fill_gradient2(low = color_alerta, mid = "white", high = color_acento, midpoint = 0, limits = c(-1, 1)) +
    tema_eda +
    theme(axis.text.y = element_text(size = 7), axis.text.x = element_text(angle = 20, hjust = 1)) +
    labs(title = paste0("Correlacion predictor-target - ", g, " (M_pura)"), x = NULL, y = NULL, fill = "r")

  rutas_heatmap_grupo <- c(
    rutas_heatmap_grupo,
    guardar_grafico_seguro(p_g, paste0("02_heatmap_", limpiar_nombre_variable(g), ".png"), ancho = 7, alto = 6)
  )
}

# ------------------------------------------------------------
# Notas metodologicas

notas <- data.frame(
  punto = c(
    "1. Lectura", "2. Correlacion cruzada", "3-4. Heatmaps", "5. Pares de alta correlacion",
    "Relacion con modelamiento", "Advertencia"
  ),
  descripcion = c(
    "Se usa la matriz pura (M_pura), escenario principal real: quimica real cruzada con cata real agregada por muestra.",
    "Correlacion de Pearson pareada (pairwise.complete.obs) entre cada predictor x_* y cada target y_*; se exige al menos 5 pares con datos para reportar el valor.",
    "Los heatmaps se calculan para los 4 targets principales (frutal, fenolico, ahumado, medicinal comunes); el heatmap por grupo separa GC-FID, GC-MS, JU y fenoles totales porque no comparten unidades analiticas.",
    "Tabla ordenada de todos los pares predictor-target con |r| >= 0.5, util como insumo temprano antes del diagnostico formal.",
    "Este analisis es un vistazo exploratorio (EDA); el diagnostico de colinealidad y los modelos supervisados se hacen en scripts/R/modelamiento/13 en adelante, con validacion cruzada y bootstrap.",
    "No interpretar estas correlaciones como relaciones causales ni como importancia de variable: n_pares es pequeno y variable por bloque (small data), y hay predictores fermentativos indirectos (JU) que no son compuestos fenolicos causales."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Exportar

ruta_salida <- guardar_excel_seguro(
  hojas = list(
    "00_resumen" = resumen_global,
    "01_correlacion_cruzada" = correlacion_cruzada,
    "02_correlacion_principales" = correlacion_cruzada_principales,
    "03_pares_correlacion_alta" = pares_correlacion_alta,
    "04_notas" = notas
  ),
  ruta_salida = ruta_salida_base
)

cat("\nANALISIS CRUZADO QUIMICO-SENSORIAL FINALIZADO\n")

cat("\nArchivo generado:\n")
cat(ruta_salida, "\n")

cat("\nGraficos generados en:\n")
cat(dir_outputs_exploratorio, "\n")

cat("\nResumen:\n")
print(resumen_global)

cat("\nPares con mayor correlacion (top 15):\n")
print(head(pares_correlacion_alta, 15))

cat("\nProceso terminado correctamente.\n")
