# ============================================================
# 09b_generar_excel_presentacion_comision.R
# Excel de presentacion para comision: una matriz por hoja,
# solo con columnas de identificacion de muestra + variables
# quimicas (x_/ctrl_) y/o sensoriales (y_). Sin columnas de
# bookkeeping interno (tipo_dato, bloque_id, archivo/hoja de
# origen, tecnica_quimica, replica_id, evaluador_id, etc.).
#
# Version simplificada de 09_generar_excel_presentacion.R.
# Aqui la matriz IA queda en una sola hoja (modelable), mas una
# 7ma hoja adicional con el detalle de consenso por familia de
# IA (Gemini/Claude/GPT) antes del consenso final entre las 3,
# recalculado directamente desde las catas IA crudas (no se
# modifica matriz_datos_ia.xlsx ni el script 08 para esto).
#
# Entradas (data/procesamiento/):
#   matriz_pura.xlsx                          -> 01_matriz_pura
#   matriz_pura_expandida_evaluador.xlsx       -> 01_matriz_expandida
#   matrices_sin_cruce.xlsx                   -> 01_quimica_sin_cata
#   matrices_sin_cruce.xlsx                   -> 02_sensorial_sin_quimica
#   matriz_sensibilidad_cata_individual.xlsx  -> 03_con_cata_individual
#   matriz_datos_ia.xlsx                      -> 03_matriz_ia_modelable
#   cata_ia_geminis.xlsx / catas_ia_claude.xlsx / catas_ia_gpt.xlsx (hoja 7)
#
# Salida:
#   outputs/presentacion/resumen_seis_matrices_comision.xlsx
# ============================================================

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

paquetes_presentacion <- c("readxl", "dplyr", "stringr", "tidyr", "openxlsx")
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
library(stringr)
library(tidyr)
library(openxlsx)

cat("\nEXCEL DE PRESENTACION PARA COMISION: UNA MATRIZ POR HOJA\n")

# ------------------------------------------------------------
# Rutas

dir_outputs <- file.path(dir_proyecto, "outputs")
dir_outputs_presentacion <- file.path(dir_outputs, "presentacion")
dir.create(dir_outputs_presentacion, recursive = TRUE, showWarnings = FALSE)

ruta_salida <- file.path(dir_outputs_presentacion, "resumen_seis_matrices_comision.xlsx")

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
# Definicion de las 6 hojas (6 matrices, IA solo en su version modelable)

definicion_hojas <- list(
  list(
    hoja = "1_Matriz_pura",
    titulo = "1. Matriz pura",
    descripcion = "Quimica real cruzada con el promedio de cata real. Escenario principal de la tesis.",
    archivo = rutas_origen$matriz_pura,
    sheet_origen = "01_matriz_pura"
  ),
  list(
    hoja = "2_Matriz_expandida_evaluador",
    titulo = "2. Matriz expandida por evaluador",
    descripcion = "Unidades quimicas expandidas a evaluaciones individuales. No son muestras independientes adicionales.",
    archivo = rutas_origen$matriz_expandida,
    sheet_origen = "01_matriz_expandida"
  ),
  list(
    hoja = "3_Matriz_solo_quimica",
    titulo = "3. Matriz solo quimica",
    descripcion = "Unidades quimicas reales sin target sensorial confirmado. Solo diagnostico, no modelable.",
    archivo = rutas_origen$matrices_sin_cruce,
    sheet_origen = "01_quimica_sin_cata"
  ),
  list(
    hoja = "4_Matriz_solo_sensorial",
    titulo = "4. Matriz solo sensorial",
    descripcion = "Evaluaciones sensoriales reales sin quimica asociada confirmada. Solo diagnostico, no modelable.",
    archivo = rutas_origen$matrices_sin_cruce,
    sheet_origen = "02_sensorial_sin_quimica"
  ),
  list(
    hoja = "5_Matriz_sensibilidad_JU",
    titulo = "5. Matriz sensibilidad cata individual JU",
    descripcion = "Matriz pura completa, incluyendo la cata individual real del bloque JU.",
    archivo = rutas_origen$matriz_sensibilidad,
    sheet_origen = "03_con_cata_individual"
  ),
  list(
    hoja = "6_Matriz_IA_modelable",
    titulo = "6. Matriz IA exploratoria",
    descripcion = "Subconjunto con al menos un target IA estimado. No es cata real; escenario sintetico exploratorio.",
    archivo = rutas_origen$matriz_ia,
    sheet_origen = "03_matriz_ia_modelable"
  )
)

# ------------------------------------------------------------
# Filtro de columnas: solo identificador de muestra + x_/ctrl_ (quimica) + y_ (sensorial)

filtrar_columnas_comision <- function(datos) {
  nombres <- names(datos)

  cols_id_candidatas <- nombres[str_detect(nombres, regex("^(muestra|identificador)", ignore_case = TRUE))]
  cols_quimica <- nombres[str_detect(nombres, "^(x_|ctrl_)")]
  cols_sensorial <- nombres[str_detect(nombres, "^y_")]

  # Si dos columnas de identificacion son exactamente iguales en todas las
  # filas (p.ej. identificador_quimico == identificador_sensorial tras el
  # cruce, o muestra_cata == muestra_cata_asociada), se conserva solo la
  # primera para no repetir la misma informacion dos veces.
  cols_id <- character(0)
  for (col in cols_id_candidatas) {
    es_duplicado <- any(vapply(
      cols_id,
      function(previa) identical(as.character(datos[[col]]), as.character(datos[[previa]])),
      logical(1)
    ))

    if (!es_duplicado) {
      cols_id <- c(cols_id, col)
    }
  }

  cols_finales <- unique(c(cols_id, cols_quimica, cols_sensorial))

  datos_filtrados <- datos %>% select(all_of(cols_finales))

  # Se descarta cualquier columna que quede completamente vacia en esta hoja
  # (p.ej. muestra_cata_asociada en la matriz solo quimica, que no aplica
  # porque esas filas no tienen cata confirmada).
  cols_no_vacias <- names(datos_filtrados)[
    vapply(datos_filtrados, function(col) !all(is.na(col)), logical(1))
  ]

  datos_filtrados %>% select(all_of(cols_no_vacias))
}

# ------------------------------------------------------------
# Estilos (mismo diseno minimalista del script 09)

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
# Lectura, filtro de columnas y construccion del libro

wb <- createWorkbook()
resumen_indice <- vector("list", length(definicion_hojas))

for (i in seq_along(definicion_hojas)) {
  def <- definicion_hojas[[i]]

  datos <- readxl::read_excel(def$archivo, sheet = def$sheet_origen) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    filtrar_columnas_comision()

  escribir_hoja_matriz(wb, def, datos)

  resumen_indice[[i]] <- data.frame(
    hoja = def$hoja,
    n_filas = nrow(datos),
    n_columnas = ncol(datos),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Hoja 7: detalle de consenso por familia de IA (Gemini/Claude/GPT),
# antes del consenso final entre las 3 (el que ya se ve en la hoja 6).
# Se recalcula aqui desde las catas IA crudas, aplicando las mismas
# correcciones metodologicas obligatorias que 08_matriz_datos_ia.R,
# para que sea consistente con el consenso final de la hoja 6.
# No se modifica matriz_datos_ia.xlsx ni el script 08.

archivos_ia <- data.frame(
  modelo_familia = c("Gemini", "Claude", "GPT"),
  archivo = c("cata_ia_geminis.xlsx", "catas_ia_claude.xlsx", "catas_ia_gpt.xlsx"),
  stringsAsFactors = FALSE
)

columnas_obligatorias_ia <- c(
  "id_muestra_ia", "bloque_id", "grupo_matriz", "muestra_base",
  "identificador_quimico", "y_fenolico_comun_ia", "y_frutal_comun_ia",
  "y_ahumado_comun_ia", "y_medicinal_comun_ia", "estado_estimacion_ia"
)

targets_ia <- c("y_fenolico_comun_ia", "y_frutal_comun_ia", "y_ahumado_comun_ia", "y_medicinal_comun_ia")

ids_ju_solo_fermentacion <- c(
  "ju_260507_quimica_cepas_702_2",
  "ju_260507_quimica_cepas_815_1",
  "ju_260507_quimica_cepas_815_2",
  "ju_260507_quimica_cepas_sc458_2e",
  "ju_260507_quimica_cepas_sc476_2e"
)

ids_ju_con_esteres_cresoles <- c("ju_260507_quimica_cepas_m1", "ju_260507_quimica_cepas_sc481_3e")

convertir_numero_ia <- function(x) {
  x_chr <- str_trim(as.character(x))
  x_chr[x_chr %in% c("", "NA", "N/A", "na", "n/a", "NULL", "null", "no_estimable")] <- NA_character_
  x_chr <- str_replace_all(x_chr, ",", ".")
  suppressWarnings(as.numeric(x_chr))
}

limitar_rango_ia <- function(x, minimo = 0, maximo = 5) {
  x <- if_else(!is.na(x) & (x < minimo | x > maximo), NA_real_, x)
  pmin(pmax(x, minimo, na.rm = FALSE), maximo, na.rm = FALSE)
}

mediana_na_ia <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  stats::median(x, na.rm = TRUE)
}

primer_no_vacio_ia <- function(x) {
  y <- str_trim(as.character(x))
  y <- y[!is.na(y) & y != ""]
  if (length(y) == 0) return(NA_character_)
  y[1]
}

leer_catas_familia <- function(modelo_familia, archivo) {
  ruta <- file.path(dir_procesamiento, archivo)
  if (!file.exists(ruta)) {
    warning("No se encontro archivo de catas IA: ", archivo)
    return(data.frame())
  }

  hojas <- readxl::excel_sheets(ruta)
  lista <- list()

  for (h in hojas) {
    datos_h <- readxl::read_excel(ruta, sheet = h, na = c("", "NA", "N/A", "na", "n/a", "NULL", "null"))
    if (!all(columnas_obligatorias_ia %in% names(datos_h))) next

    lista[[length(lista) + 1]] <- datos_h %>%
      select(all_of(columnas_obligatorias_ia)) %>%
      filter(!is.na(.data$id_muestra_ia)) %>%
      mutate(
        across(all_of(targets_ia), convertir_numero_ia),
        across(all_of(targets_ia), ~ limitar_rango_ia(.x, 0, 5)),
        modelo_familia = modelo_familia
      )
  }

  bind_rows(lista)
}

catas_ia_todas <- bind_rows(
  lapply(seq_len(nrow(archivos_ia)), function(i) {
    leer_catas_familia(archivos_ia$modelo_familia[i], archivos_ia$archivo[i])
  })
)

catas_ia_todas <- catas_ia_todas %>%
  mutate(
    across(all_of(targets_ia), ~ if_else(.data$estado_estimacion_ia == "no_estimable", NA_real_, .x)),
    across(all_of(targets_ia), ~ if_else(.data$grupo_matriz == "gcms", NA_real_, .x)),
    y_frutal_comun_ia = if_else(.data$grupo_matriz == "fenoles_totales", NA_real_, .data$y_frutal_comun_ia),
    y_fenolico_comun_ia = if_else(.data$grupo_matriz == "fenoles_totales", pmin(.data$y_fenolico_comun_ia, 3, na.rm = FALSE), .data$y_fenolico_comun_ia),
    y_ahumado_comun_ia = if_else(.data$grupo_matriz == "fenoles_totales", pmin(.data$y_ahumado_comun_ia, 3, na.rm = FALSE), .data$y_ahumado_comun_ia),
    y_medicinal_comun_ia = if_else(.data$grupo_matriz == "fenoles_totales", pmin(.data$y_medicinal_comun_ia, 1, na.rm = FALSE), .data$y_medicinal_comun_ia),
    y_frutal_comun_ia = if_else(.data$id_muestra_ia %in% ids_ju_solo_fermentacion, NA_real_, .data$y_frutal_comun_ia),
    y_fenolico_comun_ia = if_else(.data$id_muestra_ia %in% ids_ju_solo_fermentacion, pmin(.data$y_fenolico_comun_ia, 1, na.rm = FALSE), .data$y_fenolico_comun_ia),
    y_ahumado_comun_ia = if_else(.data$id_muestra_ia %in% ids_ju_solo_fermentacion, pmin(.data$y_ahumado_comun_ia, 1, na.rm = FALSE), .data$y_ahumado_comun_ia),
    y_medicinal_comun_ia = if_else(.data$id_muestra_ia %in% ids_ju_solo_fermentacion, pmin(.data$y_medicinal_comun_ia, 1, na.rm = FALSE), .data$y_medicinal_comun_ia),
    y_fenolico_comun_ia = if_else(.data$id_muestra_ia %in% ids_ju_con_esteres_cresoles, pmin(.data$y_fenolico_comun_ia, 3, na.rm = FALSE), .data$y_fenolico_comun_ia),
    y_frutal_comun_ia = if_else(.data$id_muestra_ia %in% ids_ju_con_esteres_cresoles, pmin(.data$y_frutal_comun_ia, 3, na.rm = FALSE), .data$y_frutal_comun_ia),
    y_ahumado_comun_ia = if_else(.data$id_muestra_ia %in% ids_ju_con_esteres_cresoles, pmin(.data$y_ahumado_comun_ia, 1, na.rm = FALSE), .data$y_ahumado_comun_ia),
    y_medicinal_comun_ia = if_else(.data$id_muestra_ia %in% ids_ju_con_esteres_cresoles, pmin(.data$y_medicinal_comun_ia, 3, na.rm = FALSE), .data$y_medicinal_comun_ia)
  )

consenso_por_familia <- catas_ia_todas %>%
  group_by(.data$modelo_familia, .data$id_muestra_ia) %>%
  summarise(
    muestra_base = primer_no_vacio_ia(.data$muestra_base),
    identificador_quimico = primer_no_vacio_ia(.data$identificador_quimico),
    y_fenolico_comun_ia = mediana_na_ia(.data$y_fenolico_comun_ia),
    y_frutal_comun_ia = mediana_na_ia(.data$y_frutal_comun_ia),
    y_ahumado_comun_ia = mediana_na_ia(.data$y_ahumado_comun_ia),
    y_medicinal_comun_ia = mediana_na_ia(.data$y_medicinal_comun_ia),
    .groups = "drop"
  )

detalle_por_ia <- consenso_por_familia %>%
  pivot_wider(
    id_cols = c(id_muestra_ia, muestra_base, identificador_quimico),
    names_from = modelo_familia,
    values_from = all_of(targets_ia),
    names_glue = "{.value}_{modelo_familia}"
  ) %>%
  select(-id_muestra_ia) %>%
  arrange(muestra_base) %>%
  as.data.frame(stringsAsFactors = FALSE)

definicion_hoja_ia_detalle <- list(
  hoja = "7_Detalle_por_IA",
  titulo = "7. Detalle de consenso por IA (Gemini / Claude / GPT)",
  descripcion = "Estimacion de cada familia de IA antes del consenso final entre las 3 (mostrado en la hoja 6). No es cata real; escenario sintetico exploratorio."
)

escribir_hoja_matriz(wb, definicion_hoja_ia_detalle, detalle_por_ia)

resumen_indice[[length(definicion_hojas) + 1]] <- data.frame(
  hoja = definicion_hoja_ia_detalle$hoja,
  n_filas = nrow(detalle_por_ia),
  n_columnas = ncol(detalle_por_ia),
  stringsAsFactors = FALSE
)

resumen_indice <- bind_rows(resumen_indice)

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
