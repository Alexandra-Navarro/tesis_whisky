# ============================================================
# Reune en un solo archivo las 6 matrices ya construidas por los
# scripts 04 a 08 (la matriz IA se deja en sus dos variantes,
# completa y modelable, hasta decidir cual mostrar).
#
# Entradas (data/procesamiento/):
#   matriz_pura.xlsx                          -> 01_matriz_pura
#   matriz_pura_expandida_evaluador.xlsx       -> 01_matriz_expandida
#   matrices_sin_cruce.xlsx                   -> 01_quimica_sin_cata
#   matrices_sin_cruce.xlsx                   -> 02_sensorial_sin_quimica
#   matriz_sensibilidad_cata_individual.xlsx  -> 03_con_cata_individual
#   matriz_datos_ia.xlsx                      -> 02_matriz_ia_completa
#   matriz_datos_ia.xlsx                      -> 03_matriz_ia_modelable
#
# Salida:
#   outputs/presentacion/resumen_seis_matrices.xlsx
# ============================================================

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

paquetes_presentacion <- c("readxl", "dplyr", "openxlsx")
paquetes_faltantes <- paquetes_presentacion[
  !vapply(paquetes_presentacion, requireNamespace, logical(1), quietly = TRUE)
]
if (length(paquetes_faltantes) > 0) {
  stop(
    "Faltan paquetes requeridos: ", paste(paquetes_faltantes, collapse = ", "),
    "\nInstalalos antes de ejecutar este script."
  )
}

library(readxl)
library(dplyr)
library(openxlsx)

cat("\nEXCEL DE PRESENTACION: UNA MATRIZ POR HOJA\n")

# ------------------------------------------------------------
# Rutas

dir_outputs <- file.path(dir_proyecto, "outputs")
dir_outputs_presentacion <- file.path(dir_outputs, "presentacion")
dir.create(dir_outputs_presentacion, recursive = TRUE, showWarnings = FALSE)

ruta_salida <- file.path(dir_outputs_presentacion, "resumen_seis_matrices.xlsx")

rutas_origen <- list(
  matriz_pura = file.path(dir_procesamiento, "matriz_pura.xlsx"),
  matriz_expandida = file.path(dir_procesamiento, "matriz_pura_expandida_evaluador.xlsx"),
  matrices_sin_cruce = file.path(dir_procesamiento, "matrices_sin_cruce.xlsx"),
  matriz_sensibilidad = file.path(dir_procesamiento, "matriz_sensibilidad_cata_individual.xlsx"),
  matriz_ia = file.path(dir_procesamiento, "matriz_datos_ia.xlsx")
)

for (nombre in names(rutas_origen)) {
  if (!file.exists(rutas_origen[[nombre]])) {
    stop(
      "No existe el archivo requerido (", nombre, "): ", rutas_origen[[nombre]],
      "\nEjecuta primero los scripts 04 a 08."
    )
  }
}

# ------------------------------------------------------------
# Definicion de las 7 hojas (6 matrices, IA en dos variantes)

definicion_hojas <- list(
  list(
    numero = "1",
    hoja = "1_Matriz_pura",
    titulo = "1. Matriz pura",
    descripcion = "Quimica real cruzada con el promedio de cata real. Escenario principal de la tesis.",
    archivo = rutas_origen$matriz_pura,
    sheet_origen = "01_matriz_pura"
  ),
  list(
    numero = "2",
    hoja = "2_Matriz_expandida_evaluador",
    titulo = "2. Matriz expandida por evaluador",
    descripcion = "52 unidades quimicas expandidas a evaluaciones individuales. No son muestras independientes adicionales.",
    archivo = rutas_origen$matriz_expandida,
    sheet_origen = "01_matriz_expandida"
  ),
  list(
    numero = "3",
    hoja = "3_Matriz_solo_quimica",
    titulo = "3. Matriz solo quimica",
    descripcion = "Unidades quimicas reales sin target sensorial confirmado. Solo diagnostico, no modelable.",
    archivo = rutas_origen$matrices_sin_cruce,
    sheet_origen = "01_quimica_sin_cata"
  ),
  list(
    numero = "4",
    hoja = "4_Matriz_solo_sensorial",
    titulo = "4. Matriz solo sensorial",
    descripcion = "Evaluaciones sensoriales reales sin quimica asociada confirmada. Solo diagnostico, no modelable.",
    archivo = rutas_origen$matrices_sin_cruce,
    sheet_origen = "02_sensorial_sin_quimica"
  ),
  list(
    numero = "5",
    hoja = "5_Matriz_sensibilidad_JU",
    titulo = "5. Matriz sensibilidad cata individual JU",
    descripcion = "Matriz pura completa, incluyendo la cata individual real del bloque JU.",
    archivo = rutas_origen$matriz_sensibilidad,
    sheet_origen = "03_con_cata_individual"
  ),
  list(
    numero = "6a",
    hoja = "6a_Matriz_IA_completa",
    titulo = "6a. Matriz IA exploratoria (completa)",
    descripcion = "Todas las unidades quimicas sin cata, incluyendo las no estimables por IA. No es cata real.",
    archivo = rutas_origen$matriz_ia,
    sheet_origen = "02_matriz_ia_completa"
  ),
  list(
    numero = "6b",
    hoja = "6b_Matriz_IA_modelable",
    titulo = "6b. Matriz IA exploratoria (modelable)",
    descripcion = "Subconjunto con al menos un target IA estimado. Es la version recomendada para analisis exploratorio. No es cata real.",
    archivo = rutas_origen$matriz_ia,
    sheet_origen = "03_matriz_ia_modelable"
  )
)

# ------------------------------------------------------------
# Estilos (diseno minimalista: un solo color de acento)

color_acento <- "#2E5395"
color_texto_claro <- "#FFFFFF"
color_descripcion <- "#595959"

estilo_titulo <- createStyle(
  fontSize = 13, fontColour = color_texto_claro, fgFill = color_acento,
  textDecoration = "bold", valign = "center", halign = "left"
)
estilo_descripcion <- createStyle(
  fontSize = 10, fontColour = color_descripcion, textDecoration = "italic",
  valign = "center", halign = "left", wrapText = TRUE
)
estilo_encabezado <- createStyle(
  fontSize = 10, fontColour = color_texto_claro, fgFill = color_acento,
  textDecoration = "bold", halign = "center", valign = "center",
  border = "TopBottom", borderColour = color_acento
)
estilo_indice_header <- createStyle(
  fontSize = 11, fontColour = color_texto_claro, fgFill = color_acento,
  textDecoration = "bold", halign = "center", valign = "center"
)

FILA_TITULO <- 1
FILA_DESCRIPCION <- 2
FILA_ENCABEZADO <- 4

escribir_hoja_matriz <- function(wb, definicion, datos) {
  addWorksheet(wb, definicion$hoja)
  n_col <- max(ncol(datos), 1)

  mergeCells(wb, definicion$hoja, cols = 1:n_col, rows = FILA_TITULO)
  writeData(wb, definicion$hoja, definicion$titulo, startCol = 1, startRow = FILA_TITULO)
  addStyle(wb, definicion$hoja, estilo_titulo, rows = FILA_TITULO, cols = 1:n_col, gridExpand = TRUE)
  setRowHeights(wb, definicion$hoja, rows = FILA_TITULO, heights = 22)

  mergeCells(wb, definicion$hoja, cols = 1:n_col, rows = FILA_DESCRIPCION)
  writeData(wb, definicion$hoja, definicion$descripcion, startCol = 1, startRow = FILA_DESCRIPCION)
  addStyle(wb, definicion$hoja, estilo_descripcion, rows = FILA_DESCRIPCION, cols = 1:n_col, gridExpand = TRUE)
  setRowHeights(wb, definicion$hoja, rows = FILA_DESCRIPCION, heights = 30)

  writeData(
    wb, definicion$hoja, datos,
    startCol = 1, startRow = FILA_ENCABEZADO,
    headerStyle = estilo_encabezado, withFilter = TRUE
  )

  freezePane(wb, definicion$hoja, firstActiveRow = FILA_ENCABEZADO + 1, firstActiveCol = 1)
  setColWidths(wb, definicion$hoja, cols = 1:n_col, widths = "auto")

  invisible(NULL)
}

# ------------------------------------------------------------
# Lectura de las 7 hojas y construccion del libro

wb <- createWorkbook()

resumen_indice <- vector("list", length(definicion_hojas))

for (i in seq_along(definicion_hojas)) {
  def <- definicion_hojas[[i]]

  datos <- readxl::read_excel(def$archivo, sheet = def$sheet_origen) %>%
    as.data.frame(stringsAsFactors = FALSE)

  escribir_hoja_matriz(wb, def, datos)

  resumen_indice[[i]] <- data.frame(
    numero = def$numero,
    matriz = def$titulo,
    archivo_origen = basename(def$archivo),
    hoja_origen = def$sheet_origen,
    n_filas = nrow(datos),
    n_columnas = ncol(datos),
    stringsAsFactors = FALSE
  )
}

resumen_indice <- bind_rows(resumen_indice)

# ------------------------------------------------------------
# Hoja indice (primera hoja del libro)

addWorksheet(wb, "Indice")
writeData(
  wb, "Indice", "Indice de matrices - tesis whisky chileno",
  startCol = 1, startRow = FILA_TITULO
)
mergeCells(wb, "Indice", cols = 1:ncol(resumen_indice), rows = FILA_TITULO)
addStyle(wb, "Indice", estilo_titulo, rows = FILA_TITULO, cols = 1:ncol(resumen_indice), gridExpand = TRUE)
setRowHeights(wb, "Indice", rows = FILA_TITULO, heights = 22)

writeData(
  wb, "Indice", resumen_indice,
  startCol = 1, startRow = FILA_ENCABEZADO,
  headerStyle = estilo_indice_header
)
setColWidths(wb, "Indice", cols = 1:ncol(resumen_indice), widths = "auto")
freezePane(wb, "Indice", firstActiveRow = FILA_ENCABEZADO + 1, firstActiveCol = 1)

worksheetOrder(wb) <- c(
  which(names(wb) == "Indice"),
  which(names(wb) != "Indice")
)

# ------------------------------------------------------------
# Guardado seguro (si el archivo esta abierto, guarda una copia con timestamp)

guardar_workbook_seguro <- function(wb, ruta) {
  guardar_alterno <- function() {
    ruta_alt <- file.path(
      dirname(ruta),
      paste0(
        tools::file_path_sans_ext(basename(ruta)),
        "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".xlsx"
      )
    )
    saveWorkbook(wb, ruta_alt, overwrite = TRUE)
    message(
      "No se pudo sobrescribir el archivo (probablemente esta abierto). ",
      "Se guardo copia en: ", ruta_alt
    )
    ruta_alt
  }

  tryCatch(
    withCallingHandlers(
      {
        saveWorkbook(wb, ruta, overwrite = TRUE)
        ruta
      },
      warning = function(w) {
        stop(conditionMessage(w))
      }
    ),
    error = function(e) guardar_alterno()
  )
}

ruta_guardada <- guardar_workbook_seguro(wb, ruta_salida)

cat("\nArchivo generado:\n")
cat(ruta_guardada, "\n")

cat("\nResumen:\n")
print(resumen_indice)

cat("\nProceso terminado correctamente.\n")
