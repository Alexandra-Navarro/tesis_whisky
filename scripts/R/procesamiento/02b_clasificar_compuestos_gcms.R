
# 02b_clasificar_compuestos_gcms.R
#
# Documenta y valida la clasificacion por familia quimica de los compuestos
# GC-MS que no traen columna "nature" en el archivo original (bloques
# gcms_constanza_comerciales y gcms_constanza_experimentales), usando el
# clasificador por nombre definido en 00_resumen_datos_iniciales.R
# (clasificar_compuesto / reglas_clasificacion_gcms).
#
# Esta clasificacion SI se usa en produccion: 02_extraer_quimica.R la aplica
# para calcular x_gcms_esteres_pct / x_gcms_fenolicos_pct / x_gcms_aldehidos_pct
# tanto en gcms_noviembre (que tenia "nature" real, usado para validar el
# clasificador al 100%) como en los bloques Constanza (que no la tenian).
#
# Este script genera la tabla de detalle compuesto -> familia -> variable
# final y la comparacion "antes vs despues" que se muestran en el reporte de
# procesamiento (reportes_rpubs/01_procesamiento.Rmd).

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

cat("
CLASIFICACION DE COMPUESTOS GC-MS POR FAMILIA (BASADA EN NOMBRE)
")

ruta_original <- function(clave) {
  if (!clave %in% names(rutas_originales)) {
    return(NA_character_)
  }

  ruta <- rutas_originales[[clave]]

  if (is.null(ruta) || is.na(ruta) || !file.exists(ruta)) {
    return(NA_character_)
  }

  ruta
}

# ------------------------------------------------------------
# 1. Reglas de clasificacion por nombre (definidas en 00, orden = prioridad)
# ------------------------------------------------------------

reglas_clasificacion <- reglas_clasificacion_gcms
familia_a_variable_final <- familia_gcms_a_variable_final
# ------------------------------------------------------------
# 2. Validacion de las reglas contra gcms_noviembre (que SI tiene "nature")
# ------------------------------------------------------------

ruta_noviembre <- ruta_original("gcms_noviembre")

noviembre_raw <- readxl::read_excel(ruta_noviembre, sheet = "Hoja1") %>%
  janitor::clean_names() %>%
  filter(!is.na(compound_name), compound_name != "") %>%
  distinct(compound_name, nature)

noviembre_raw <- noviembre_raw %>%
  mutate(
    familia_por_nombre = clasificar_compuesto(compound_name),
    nature_norm = tolower(trimws(ifelse(is.na(nature), "", nature)))
  )

validacion_reglas <- noviembre_raw %>%
  mutate(
    familia_original_norm = dplyr::case_when(
      str_detect(nature_norm, "ester") ~ "Ester",
      str_detect(nature_norm, "phenol|cresol") ~ "Fenolico (fenoles, cresoles, guaiacoles)",
      str_detect(nature_norm, "aldeh") ~ "Aldehido",
      str_detect(nature_norm, "aromatic") ~ "Hidrocarburo aromatico",
      str_detect(nature_norm, "hydrocarbon") ~ "Hidrocarburo (alcano/alqueno)",
      TRUE ~ "Sin clasificar"
    ),
    coincide = familia_original_norm == familia_por_nombre
  )

pct_coincidencia <- round(100 * mean(validacion_reglas$coincide, na.rm = TRUE), 1)

cat("\nValidacion de reglas contra gcms_noviembre (tiene 'nature' real):\n")
cat("Coincidencia reglas por nombre vs familia original:", pct_coincidencia, "% de", nrow(validacion_reglas), "compuestos\n")

# ------------------------------------------------------------
# 3. Extraccion de compuestos crudos GC-MS Constanza (zip, sin "nature")
# ------------------------------------------------------------

ruta_zip <- ruta_original("gcms_constanza_zip")

extraer_compuestos_zip <- function(ruta_zip) {
  if (is.na(ruta_zip)) {
    warning("No se encontro Resultados GC-MS-Constanza Vidal.zip")
    return(data.frame())
  }

  temp_dir <- tempfile("gcms_clasificar_")
  dir.create(temp_dir, recursive = TRUE)
  unzip(ruta_zip, exdir = temp_dir)

  archivos_xls <- list.files(temp_dir, pattern = "\\.xls$", recursive = TRUE, full.names = TRUE)
  archivos_xls <- archivos_xls[
    str_detect(normalizar_texto(archivos_xls), "muestras 2-controles comerciales|muestras 3-experimental")
  ]

  lista <- list()

  for (f in archivos_xls) {
    raw <- tryCatch(readxl::read_excel(f, col_names = FALSE), error = function(e) NULL)
    if (is.null(raw)) next

    raw <- as.data.frame(raw, stringsAsFactors = FALSE)
    raw[] <- lapply(raw, as.character)

    fila_header <- NA_integer_
    for (i in seq_len(nrow(raw))) {
      fila <- tolower(unlist(raw[i, ], use.names = FALSE))
      fila[is.na(fila)] <- ""
      if (any(str_detect(fila, "compound")) && any(str_detect(fila, "^area$|area "))) {
        fila_header <- i
        break
      }
    }
    if (is.na(fila_header)) next

    headers <- unlist(raw[fila_header, ], use.names = FALSE)
    col_compuesto <- which(str_detect(tolower(headers), "compound"))[1]
    col_area <- which(tolower(headers) == "area")[1]

    if (is.na(col_compuesto) || is.na(col_area)) next

    datos_f <- raw[(fila_header + 1):nrow(raw), , drop = FALSE]

    df <- data.frame(
      archivo = basename(f),
      compound_name = datos_f[[col_compuesto]],
      area = suppressWarnings(as.numeric(datos_f[[col_area]])),
      stringsAsFactors = FALSE
    ) %>%
      filter(!is.na(compound_name), compound_name != "", !is.na(area), area > 0)

    lista[[f]] <- df
  }

  bind_rows(lista)
}

compuestos_zip <- extraer_compuestos_zip(ruta_zip)

# Reconstruir metadata (bloque_id / muestra_base) por archivo, igual que en 02_extraer_quimica.R
parsear_metadata_zip_simple <- function(archivo) {
  nombre_norm <- normalizar_texto(archivo)

  bloque_id <- NA_character_
  muestra_base <- NA_character_

  if (str_detect(nombre_norm, "c([1-6])") && !str_detect(nombre_norm, "1-5|4-5")) {
    bloque_id <- "gcms_constanza_comerciales"
    cnum <- str_match(nombre_norm, "c([1-6])")[, 2]
    muestra_base <- paste0("C", cnum)
  } else if (str_detect(nombre_norm, "1-5-a-30")) {
    bloque_id <- "gcms_constanza_experimentales"; muestra_base <- "XP3"
  } else if (str_detect(nombre_norm, "1-5-25")) {
    bloque_id <- "gcms_constanza_experimentales"; muestra_base <- "XP1"
  } else if (str_detect(nombre_norm, "1-5-30")) {
    bloque_id <- "gcms_constanza_experimentales"; muestra_base <- "XP2"
  } else if (str_detect(nombre_norm, "4-5-25")) {
    bloque_id <- "gcms_constanza_experimentales"; muestra_base <- "XP4"
  } else if (str_detect(nombre_norm, "4-5-30")) {
    bloque_id <- "gcms_constanza_experimentales"; muestra_base <- "XP5"
  }

  data.frame(archivo = basename(archivo), bloque_id = bloque_id, muestra_base = muestra_base, stringsAsFactors = FALSE)
}

meta_archivos <- bind_rows(lapply(unique(compuestos_zip$archivo), parsear_metadata_zip_simple))

compuestos_zip <- compuestos_zip %>%
  left_join(meta_archivos, by = "archivo") %>%
  filter(!is.na(bloque_id))

# ------------------------------------------------------------
# 4. Clasificar compuestos Constanza por nombre
# ------------------------------------------------------------

compuestos_zip <- compuestos_zip %>%
  mutate(
    familia_asignada = clasificar_compuesto(compound_name),
    variable_final_propuesta = familia_a_variable_final(familia_asignada)
  )

tabla_compuestos_unicos_zip <- compuestos_zip %>%
  distinct(compound_name, familia_asignada, variable_final_propuesta) %>%
  arrange(familia_asignada, compound_name)

resumen_familias_zip <- compuestos_zip %>%
  distinct(compound_name, familia_asignada) %>%
  count(familia_asignada, name = "n_compuestos_unicos") %>%
  arrange(desc(n_compuestos_unicos))

# Tabla combinada: Noviembre (nature real) + Constanza (nombre)
tabla_noviembre_export <- noviembre_raw %>%
  transmute(
    fuente = "gcms_noviembre",
    compound_name,
    familia_original_nature = nature,
    familia_asignada = familia_por_nombre,
    variable_final_propuesta = familia_a_variable_final(familia_por_nombre)
  )

tabla_zip_export <- tabla_compuestos_unicos_zip %>%
  transmute(
    fuente = "gcms_constanza (comerciales + experimentales)",
    compound_name,
    familia_original_nature = NA_character_,
    familia_asignada,
    variable_final_propuesta
  )

tabla_compuestos_completa <- bind_rows(tabla_noviembre_export, tabla_zip_export) %>%
  arrange(fuente, familia_asignada, compound_name)

resumen_familias_todas <- tabla_compuestos_completa %>%
  count(fuente, familia_asignada, name = "n_compuestos_unicos") %>%
  arrange(fuente, desc(n_compuestos_unicos))

# ------------------------------------------------------------
# 5. Comparacion "antes vs despues" por unidad analitica (Constanza)
# ------------------------------------------------------------

comparacion_antes_despues <- compuestos_zip %>%
  group_by(bloque_id, muestra_base) %>%
  mutate(
    area_total = sum(area, na.rm = TRUE),
    area_pct = area / area_total * 100
  ) %>%
  summarise(
    n_compuestos = n_distinct(compound_name),
    esteres_pct_actual = 0,
    esteres_pct_reclasificado = sum(area_pct[familia_asignada == "Ester"], na.rm = TRUE),
    fenolicos_pct_actual = 0,
    fenolicos_pct_reclasificado = sum(area_pct[familia_asignada == "Fenolico (fenoles, cresoles, guaiacoles)"], na.rm = TRUE),
    aldehidos_pct_actual = 0,
    aldehidos_pct_reclasificado = sum(area_pct[familia_asignada == "Aldehido"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    aroma_fermentativo_pct_reclasificado = esteres_pct_reclasificado + aldehidos_pct_reclasificado
  )

# ------------------------------------------------------------
# 6. Resumen global y export
# ------------------------------------------------------------

resumen_global <- data.frame(
  indicador = c(
    "Compuestos unicos gcms_noviembre (con 'nature' real)",
    "Compuestos unicos gcms_constanza (sin 'nature', clasificados por nombre)",
    "Coincidencia reglas por nombre vs 'nature' real (validacion en Noviembre)",
    "Unidades Constanza recalculadas (comerciales + experimentales)",
    "Familias detectadas (todas las fuentes)"
  ),
  valor = c(
    n_distinct(noviembre_raw$compound_name),
    n_distinct(tabla_compuestos_unicos_zip$compound_name),
    paste0(pct_coincidencia, "%"),
    n_distinct(paste(compuestos_zip$bloque_id, compuestos_zip$muestra_base)),
    n_distinct(tabla_compuestos_completa$familia_asignada)
  ),
  stringsAsFactors = FALSE
)

dir.create(dir_procesamiento, recursive = TRUE, showWarnings = FALSE)

ruta_salida <- file.path(dir_procesamiento, "clasificacion_compuestos_gcms.xlsx")

if (file.exists(ruta_salida)) {
  try(file.remove(ruta_salida), silent = TRUE)
}

writexl::write_xlsx(
  list(
    "00_resumen" = resumen_global,
    "01_reglas_clasificacion" = reglas_clasificacion,
    "02_compuestos_clasificados" = tabla_compuestos_completa,
    "03_resumen_por_familia" = resumen_familias_todas,
    "04_validacion_reglas_noviembre" = validacion_reglas %>%
      select(compound_name, nature, familia_original_norm, familia_por_nombre, coincide),
    "05_comparacion_antes_despues" = comparacion_antes_despues
  ),
  path = ruta_salida
)

cat("\nArchivo generado:\n")
cat(ruta_salida, "\n")

cat("\nResumen global:\n")
print(resumen_global)

cat("\nResumen por familia (todas las fuentes):\n")
print(as.data.frame(resumen_familias_todas))

cat("\nComparacion antes vs despues (primeras filas):\n")
print(as.data.frame(head(comparacion_antes_despues, 10)))

cat("\nProceso terminado correctamente.\n")
