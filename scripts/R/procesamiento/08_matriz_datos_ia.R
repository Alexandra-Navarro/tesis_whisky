# ============================================================
# Matriz IA final para muestras químicas sin cata real
# Versión reducida: solo hojas y columnas necesarias para análisis/modelamiento.
#
# Entradas esperadas:
#   data/procesamiento/catas_ia/cata_ia_geminis.xlsx
#   data/procesamiento/catas_ia/catas_ia_claude.xlsx
#   data/procesamiento/catas_ia/catas_ia_gpt.xlsx
#   data/procesamiento/matrices_sin_cruce.xlsx, hoja 01_quimica_sin_cata
#
# Salida:
#   data/procesamiento/matriz_datos_ia.xlsx
# ============================================================

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

paquetes_necesarios <- c("readxl", "dplyr", "tidyr", "stringr", "writexl", "tibble")
for (p in paquetes_necesarios) {
  if (!requireNamespace(p, quietly = TRUE)) {
    stop("Falta instalar el paquete: ", p, call. = FALSE)
  }
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(writexl)
  library(tibble)
})

# ------------------------------------------------------------
# Rutas

dir_catas_ia <- file.path(dir_procesamiento, "catas_ia")
ruta_matrices_sin_cruce <- file.path(dir_procesamiento, "matrices_sin_cruce.xlsx")
ruta_salida <- file.path(dir_procesamiento, "matriz_datos_ia.xlsx")

archivos_ia <- tibble::tibble(
  modelo_familia = c("Gemini", "Claude", "GPT"),
  archivo = c("cata_ia_geminis.xlsx", "catas_ia_claude.xlsx", "catas_ia_gpt.xlsx")
)

buscar_archivo <- function(nombre_archivo) {
  rutas_posibles <- c(
    file.path(dir_catas_ia, nombre_archivo),
    file.path(dir_procesamiento, nombre_archivo)
  )

  ruta_ok <- rutas_posibles[file.exists(rutas_posibles)][1]
  if (is.na(ruta_ok)) {
    stop(
      "No se encontró el archivo: ", nombre_archivo, "\n",
      "Ubícalo en data/procesamiento/catas_ia/ o en data/procesamiento/.",
      call. = FALSE
    )
  }
  ruta_ok
}

if (!file.exists(ruta_matrices_sin_cruce)) {
  stop("No se encontró matrices_sin_cruce.xlsx en data/procesamiento.", call. = FALSE)
}

# ------------------------------------------------------------
# Columnas y funciones mínimas

columnas_obligatorias <- c(
  "id_muestra_ia", "bloque_id", "grupo_matriz", "muestra_base",
  "identificador_quimico", "tecnica_quimica", "n_unidades_analiticas",
  "y_fenolico_comun_ia", "y_frutal_comun_ia",
  "y_ahumado_comun_ia", "y_medicinal_comun_ia",
  "confianza_ia", "criterio_estimacion_ia", "anclajes_reales_usados",
  "similitud_con_anclajes", "base_evidencia_ia", "regla_quimica_aplicada",
  "estado_estimacion_ia", "requiere_revision_humana",
  "modelo_ia", "prompt_version"
)

targets_ia <- c(
  "y_fenolico_comun_ia",
  "y_frutal_comun_ia",
  "y_ahumado_comun_ia",
  "y_medicinal_comun_ia"
)

ids_ju_solo_fermentacion <- c(
  "ju_260507_quimica_cepas_702_2",
  "ju_260507_quimica_cepas_815_1",
  "ju_260507_quimica_cepas_815_2",
  "ju_260507_quimica_cepas_sc458_2e",
  "ju_260507_quimica_cepas_sc476_2e"
)

convertir_numero_ia <- function(x) {
  x_chr <- stringr::str_trim(as.character(x))
  x_chr[x_chr %in% c("", "NA", "N/A", "na", "n/a", "NULL", "null", "no_estimable")] <- NA_character_
  x_chr <- stringr::str_replace_all(x_chr, ",", ".")
  suppressWarnings(as.numeric(x_chr))
}

convertir_logico_ia <- function(x) {
  x_chr <- stringr::str_to_lower(stringr::str_trim(as.character(x)))
  dplyr::case_when(
    x_chr %in% c("true", "verdadero", "sí", "si", "1") ~ TRUE,
    x_chr %in% c("false", "falso", "no", "0") ~ FALSE,
    TRUE ~ NA
  )
}

primer_no_vacio <- function(x) {
  y <- stringr::str_trim(as.character(x))
  y <- y[!is.na(y) & y != ""]
  if (length(y) == 0) return(NA_character_)
  y[1]
}

mediana_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  stats::median(x, na.rm = TRUE)
}

media_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

rango_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
}

limitar_rango <- function(x, minimo = 0, maximo = 5) {
  x <- dplyr::if_else(!is.na(x) & (x < minimo | x > maximo), NA_real_, x)
  pmin(pmax(x, minimo, na.rm = FALSE), maximo, na.rm = FALSE)
}

guardar_excel_seguro <- function(lista_hojas, ruta) {
  tryCatch(
    {
      writexl::write_xlsx(lista_hojas, ruta)
      ruta
    },
    error = function(e) {
      ruta_alt <- file.path(
        dirname(ruta),
        paste0(
          tools::file_path_sans_ext(basename(ruta)),
          "_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".xlsx"
        )
      )
      writexl::write_xlsx(lista_hojas, ruta_alt)
      message("No se pudo sobrescribir el archivo original. Se guardó copia en: ", ruta_alt)
      ruta_alt
    }
  )
}

# ------------------------------------------------------------
# Lectura y limpieza de catas IA

leer_catas_modelo <- function(modelo_familia, archivo) {
  ruta <- buscar_archivo(archivo)
  hojas <- readxl::excel_sheets(ruta)

  lista <- list()
  revision <- list()

  for (h in hojas) {
    datos_h <- tryCatch(
      readxl::read_excel(ruta, sheet = h, na = c("", "NA", "N/A", "na", "n/a", "NULL", "null")),
      error = function(e) NULL
    )

    if (is.null(datos_h)) next

    faltantes <- setdiff(columnas_obligatorias, names(datos_h))
    hoja_valida <- length(faltantes) == 0

    revision[[length(revision) + 1]] <- tibble::tibble(
      modelo_familia = modelo_familia,
      archivo_origen = archivo,
      hoja_origen = h,
      hoja_usada = hoja_valida,
      n_filas = nrow(datos_h),
      n_columnas = ncol(datos_h),
      columnas_faltantes = ifelse(length(faltantes) == 0, "", paste(faltantes, collapse = "; "))
    )

    if (!hoja_valida) next

    datos_limpios <- datos_h %>%
      dplyr::select(dplyr::all_of(columnas_obligatorias)) %>%
      dplyr::filter(!is.na(.data$id_muestra_ia)) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(c("id_muestra_ia", "bloque_id", "grupo_matriz", "muestra_base", "identificador_quimico", "tecnica_quimica")),
          ~ stringr::str_trim(as.character(.x))
        ),
        dplyr::across(dplyr::all_of(targets_ia), convertir_numero_ia),
        dplyr::across(dplyr::all_of(targets_ia), ~ limitar_rango(.x, 0, 5)),
        confianza_ia = limitar_rango(convertir_numero_ia(.data$confianza_ia), 0, 1),
        similitud_con_anclajes = limitar_rango(convertir_numero_ia(.data$similitud_con_anclajes), 0, 1),
        n_unidades_analiticas = convertir_numero_ia(.data$n_unidades_analiticas),
        requiere_revision_humana = convertir_logico_ia(.data$requiere_revision_humana),
        dplyr::across(
          dplyr::all_of(c("criterio_estimacion_ia", "anclajes_reales_usados", "base_evidencia_ia", "regla_quimica_aplicada", "estado_estimacion_ia", "modelo_ia", "prompt_version")),
          ~ stringr::str_trim(as.character(.x))
        ),
        modelo_familia = modelo_familia,
        archivo_origen = archivo,
        hoja_origen = h,
        replica_ia = paste0(modelo_familia, "_", h)
      )

    lista[[length(lista) + 1]] <- datos_limpios
  }

  list(
    catas = dplyr::bind_rows(lista),
    revision = dplyr::bind_rows(revision)
  )
}

lecturas <- lapply(seq_len(nrow(archivos_ia)), function(i) {
  leer_catas_modelo(archivos_ia$modelo_familia[i], archivos_ia$archivo[i])
})

catas_ia_largo <- dplyr::bind_rows(lapply(lecturas, `[[`, "catas"))
revision_lectura <- dplyr::bind_rows(lapply(lecturas, `[[`, "revision"))

if (nrow(catas_ia_largo) == 0) {
  stop("No se encontró ninguna hoja válida de catas IA.", call. = FALSE)
}

# ------------------------------------------------------------
# Correcciones metodológicas obligatorias

catas_ia_largo <- catas_ia_largo %>%
  dplyr::mutate(
    # Si una fila fue marcada como no estimable, sus targets deben quedar vacíos.
    dplyr::across(dplyr::all_of(targets_ia), ~ dplyr::if_else(.data$estado_estimacion_ia == "no_estimable", NA_real_, .x)),

    # GC-MS sin evidencia aromática: todos los targets vacíos.
    dplyr::across(dplyr::all_of(targets_ia), ~ dplyr::if_else(.data$grupo_matriz == "gcms", NA_real_, .x)),
    estado_estimacion_ia = dplyr::if_else(.data$grupo_matriz == "gcms", "no_estimable", .data$estado_estimacion_ia),
    confianza_ia = dplyr::if_else(.data$grupo_matriz == "gcms", 0.20, .data$confianza_ia),

    # Folin: no hay ésteres medidos; frutal vacío. Fenólico/ahumado/medicinal restringidos.
    y_frutal_comun_ia = dplyr::if_else(.data$grupo_matriz == "fenoles_totales", NA_real_, .data$y_frutal_comun_ia),
    y_fenolico_comun_ia = dplyr::if_else(.data$grupo_matriz == "fenoles_totales", pmin(.data$y_fenolico_comun_ia, 3, na.rm = FALSE), .data$y_fenolico_comun_ia),
    y_ahumado_comun_ia = dplyr::if_else(.data$grupo_matriz == "fenoles_totales", pmin(.data$y_ahumado_comun_ia, 3, na.rm = FALSE), .data$y_ahumado_comun_ia),
    y_medicinal_comun_ia = dplyr::if_else(.data$grupo_matriz == "fenoles_totales", pmin(.data$y_medicinal_comun_ia, 1, na.rm = FALSE), .data$y_medicinal_comun_ia),

    # JU solo fermentativo: frutal vacío; otros targets máximos 1.
    y_frutal_comun_ia = dplyr::if_else(.data$id_muestra_ia %in% ids_ju_solo_fermentacion, NA_real_, .data$y_frutal_comun_ia),
    y_fenolico_comun_ia = dplyr::if_else(.data$id_muestra_ia %in% ids_ju_solo_fermentacion, pmin(.data$y_fenolico_comun_ia, 1, na.rm = FALSE), .data$y_fenolico_comun_ia),
    y_ahumado_comun_ia = dplyr::if_else(.data$id_muestra_ia %in% ids_ju_solo_fermentacion, pmin(.data$y_ahumado_comun_ia, 1, na.rm = FALSE), .data$y_ahumado_comun_ia),
    y_medicinal_comun_ia = dplyr::if_else(.data$id_muestra_ia %in% ids_ju_solo_fermentacion, pmin(.data$y_medicinal_comun_ia, 1, na.rm = FALSE), .data$y_medicinal_comun_ia),

    # JU con ésteres/cresoles: no superar 3; ahumado máximo 1 si no hay proceso explícito de humo.
    y_fenolico_comun_ia = dplyr::if_else(.data$id_muestra_ia %in% c("ju_260507_quimica_cepas_m1", "ju_260507_quimica_cepas_sc481_3e"), pmin(.data$y_fenolico_comun_ia, 3, na.rm = FALSE), .data$y_fenolico_comun_ia),
    y_frutal_comun_ia = dplyr::if_else(.data$id_muestra_ia %in% c("ju_260507_quimica_cepas_m1", "ju_260507_quimica_cepas_sc481_3e"), pmin(.data$y_frutal_comun_ia, 3, na.rm = FALSE), .data$y_frutal_comun_ia),
    y_ahumado_comun_ia = dplyr::if_else(.data$id_muestra_ia %in% c("ju_260507_quimica_cepas_m1", "ju_260507_quimica_cepas_sc481_3e"), pmin(.data$y_ahumado_comun_ia, 1, na.rm = FALSE), .data$y_ahumado_comun_ia),
    y_medicinal_comun_ia = dplyr::if_else(.data$id_muestra_ia %in% c("ju_260507_quimica_cepas_m1", "ju_260507_quimica_cepas_sc481_3e"), pmin(.data$y_medicinal_comun_ia, 3, na.rm = FALSE), .data$y_medicinal_comun_ia)
  )

# ------------------------------------------------------------
# Consenso por familia IA y consenso final por muestra

consenso_modelo <- catas_ia_largo %>%
  dplyr::group_by(.data$modelo_familia, .data$id_muestra_ia) %>%
  dplyr::summarise(
    bloque_id = primer_no_vacio(.data$bloque_id),
    grupo_matriz = primer_no_vacio(.data$grupo_matriz),
    muestra_base = primer_no_vacio(.data$muestra_base),
    identificador_quimico = primer_no_vacio(.data$identificador_quimico),
    tecnica_quimica = primer_no_vacio(.data$tecnica_quimica),
    y_fenolico_comun_ia = mediana_na(.data$y_fenolico_comun_ia),
    y_frutal_comun_ia = mediana_na(.data$y_frutal_comun_ia),
    y_ahumado_comun_ia = mediana_na(.data$y_ahumado_comun_ia),
    y_medicinal_comun_ia = mediana_na(.data$y_medicinal_comun_ia),
    confianza_modelo = media_na(.data$confianza_ia),
    n_replicas_modelo = dplyr::n_distinct(.data$replica_ia),
    requiere_revision_humana_modelo = any(.data$requiere_revision_humana %in% TRUE, na.rm = TRUE),
    .groups = "drop"
  )

consenso_muestra <- consenso_modelo %>%
  dplyr::group_by(.data$id_muestra_ia) %>%
  dplyr::summarise(
    bloque_id = primer_no_vacio(.data$bloque_id),
    grupo_matriz = primer_no_vacio(.data$grupo_matriz),
    muestra_base = primer_no_vacio(.data$muestra_base),
    identificador_quimico = primer_no_vacio(.data$identificador_quimico),
    tecnica_quimica = primer_no_vacio(.data$tecnica_quimica),

    y_fenolico_comun_ia = mediana_na(.data$y_fenolico_comun_ia),
    y_frutal_comun_ia = mediana_na(.data$y_frutal_comun_ia),
    y_ahumado_comun_ia = mediana_na(.data$y_ahumado_comun_ia),
    y_medicinal_comun_ia = mediana_na(.data$y_medicinal_comun_ia),

    n_modelos_fenolico = sum(!is.na(.data$y_fenolico_comun_ia)),
    n_modelos_frutal = sum(!is.na(.data$y_frutal_comun_ia)),
    n_modelos_ahumado = sum(!is.na(.data$y_ahumado_comun_ia)),
    n_modelos_medicinal = sum(!is.na(.data$y_medicinal_comun_ia)),

    rango_fenolico = rango_na(.data$y_fenolico_comun_ia),
    rango_frutal = rango_na(.data$y_frutal_comun_ia),
    rango_ahumado = rango_na(.data$y_ahumado_comun_ia),
    rango_medicinal = rango_na(.data$y_medicinal_comun_ia),

    confianza_ia_promedio = media_na(.data$confianza_modelo),
    modelos_familia_disponibles = paste(sort(unique(.data$modelo_familia)), collapse = "; "),
    n_modelos_familia = dplyr::n_distinct(.data$modelo_familia),
    requiere_revision_humana_ia = any(.data$requiere_revision_humana_modelo %in% TRUE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    n_targets_estimados_ia = rowSums(!is.na(dplyr::select(., dplyr::all_of(targets_ia)))),
    n_modelos_validos_promedio = dplyr::case_when(
      .data$n_targets_estimados_ia == 0 ~ 0,
      TRUE ~ rowMeans(
        dplyr::select(., dplyr::all_of(c("n_modelos_fenolico", "n_modelos_frutal", "n_modelos_ahumado", "n_modelos_medicinal"))),
        na.rm = TRUE
      )
    ),
    estado_consenso_ia = dplyr::case_when(
      .data$n_targets_estimados_ia == 0 ~ "no_estimable",
      .data$n_modelos_validos_promedio >= 2.5 ~ "consenso_alto",
      .data$n_modelos_validos_promedio >= 2.0 ~ "consenso_medio",
      TRUE ~ "consenso_debil"
    )
  )

# ------------------------------------------------------------
# Unión con química sin cata y selección de columnas necesarias

quimica_sin_cata <- readxl::read_excel(
  ruta_matrices_sin_cruce,
  sheet = "01_quimica_sin_cata",
  na = c("", "NA", "N/A", "na", "n/a")
) %>%
  dplyr::mutate(
    bloque_id = stringr::str_trim(as.character(.data$bloque_id)),
    muestra_base = stringr::str_trim(as.character(.data$muestra_base))
  )

columnas_id_quimica <- c(
  "unidad_analitica_id", "bloque_id", "grupo_matriz", "muestra_base",
  "identificador_quimico", "replica_id", "tecnica_quimica",
  "archivo_quimico", "hoja_quimica", "n_variables_quimicas_disponibles"
)

columnas_quimicas <- names(quimica_sin_cata)[
  stringr::str_starts(names(quimica_sin_cata), "x_") |
    stringr::str_starts(names(quimica_sin_cata), "ctrl_") |
    names(quimica_sin_cata) %in% c("n_compuestos_detectados")
]

columnas_consenso <- c(
  "id_muestra_ia",
  targets_ia,
  "n_modelos_fenolico", "n_modelos_frutal", "n_modelos_ahumado", "n_modelos_medicinal",
  "rango_fenolico", "rango_frutal", "rango_ahumado", "rango_medicinal",
  "confianza_ia_promedio", "modelos_familia_disponibles", "n_modelos_familia",
  "requiere_revision_humana_ia", "n_targets_estimados_ia", "estado_consenso_ia"
)

matriz_ia_completa <- quimica_sin_cata %>%
  dplyr::left_join(
    consenso_muestra,
    by = c("bloque_id", "muestra_base"),
    suffix = c("", "_ia")
  ) %>%
  dplyr::mutate(
    tiene_consenso_ia = !is.na(.data$id_muestra_ia),
    tiene_algun_target_ia = rowSums(!is.na(dplyr::select(., dplyr::all_of(targets_ia)))) > 0,
    fuente_target = dplyr::case_when(
      .data$tiene_algun_target_ia ~ "cata_ia_consenso",
      .data$tiene_consenso_ia ~ "ia_no_estimable",
      TRUE ~ "sin_cata_ia"
    )
  ) %>%
  dplyr::select(
    dplyr::any_of(columnas_id_quimica),
    dplyr::any_of(columnas_quimicas),
    dplyr::any_of(columnas_consenso),
    dplyr::all_of(c("tiene_consenso_ia", "tiene_algun_target_ia", "fuente_target"))
  )

matriz_ia_modelable <- matriz_ia_completa %>%
  dplyr::filter(.data$tiene_algun_target_ia)

# ------------------------------------------------------------
# Resumen y revisión mínima

revision_cruce <- matriz_ia_completa %>%
  dplyr::summarise(
    unidades_quimicas_sin_cata = dplyr::n(),
    unidades_con_consenso_ia = sum(.data$tiene_consenso_ia, na.rm = TRUE),
    unidades_modelables_con_ia = sum(.data$tiene_algun_target_ia, na.rm = TRUE),
    unidades_no_estimables_ia = sum(.data$fuente_target == "ia_no_estimable", na.rm = TRUE),
    muestras_base_quimicas = dplyr::n_distinct(.data$muestra_base),
    muestras_base_con_consenso_ia = dplyr::n_distinct(.data$muestra_base[.data$tiene_consenso_ia])
  )

revision_bloques <- matriz_ia_completa %>%
  dplyr::group_by(.data$bloque_id, .data$grupo_matriz) %>%
  dplyr::summarise(
    unidades = dplyr::n(),
    muestras_base = dplyr::n_distinct(.data$muestra_base),
    unidades_modelables_con_ia = sum(.data$tiene_algun_target_ia, na.rm = TRUE),
    unidades_no_estimables_ia = sum(.data$fuente_target == "ia_no_estimable", na.rm = TRUE),
    .groups = "drop"
  )

resumen <- tibble::tibble(
  indicador = c(
    "archivos_ia_esperados",
    "archivos_ia_leidos",
    "hojas_ia_validas",
    "filas_catas_ia_largo",
    "muestras_ia_unicas",
    "filas_consenso_muestra",
    "unidades_quimicas_sin_cata",
    "unidades_matriz_ia_completa",
    "unidades_matriz_ia_modelable",
    "unidades_ia_no_estimable",
    "archivo_salida",
    "fecha_generacion"
  ),
  valor = c(
    nrow(archivos_ia),
    sum(file.exists(file.path(dir_catas_ia, archivos_ia$archivo)) | file.exists(file.path(dir_procesamiento, archivos_ia$archivo))),
    sum(revision_lectura$hoja_usada, na.rm = TRUE),
    nrow(catas_ia_largo),
    dplyr::n_distinct(catas_ia_largo$id_muestra_ia),
    nrow(consenso_muestra),
    nrow(quimica_sin_cata),
    nrow(matriz_ia_completa),
    nrow(matriz_ia_modelable),
    sum(matriz_ia_completa$fuente_target == "ia_no_estimable", na.rm = TRUE),
    ruta_salida,
    as.character(Sys.time())
  )
)

revision <- dplyr::bind_rows(
  revision_lectura %>%
    dplyr::transmute(
      seccion = "lectura_archivos",
      indicador = paste(.data$modelo_familia, .data$hoja_origen, sep = " | "),
      valor = paste0("usada=", .data$hoja_usada, "; filas=", .data$n_filas, "; columnas=", .data$n_columnas)
    ),
  revision_cruce %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "indicador", values_to = "valor") %>%
    dplyr::mutate(seccion = "cruce_quimica_ia", valor = as.character(.data$valor)) %>%
    dplyr::select(dplyr::all_of(c("seccion", "indicador", "valor"))),
  revision_bloques %>%
    dplyr::transmute(
      seccion = "bloques",
      indicador = paste(.data$bloque_id, .data$grupo_matriz, sep = " | "),
      valor = paste0(
        "unidades=", .data$unidades,
        "; muestras=", .data$muestras_base,
        "; modelables=", .data$unidades_modelables_con_ia,
        "; no_estimables=", .data$unidades_no_estimables_ia
      )
    )
)

diccionario <- tibble::tribble(
  ~columna, ~descripcion,
  "y_*_comun_ia", "Target sensorial sintético final. Es la mediana entre consensos internos de Gemini, Claude y GPT.",
  "n_modelos_*", "Número de familias IA que entregaron valor válido para ese target.",
  "rango_*", "Máximo menos mínimo entre familias IA. Mayor rango implica mayor desacuerdo.",
  "confianza_ia_promedio", "Promedio de confianza entre familias IA.",
  "estado_consenso_ia", "no_estimable, consenso_debil, consenso_medio o consenso_alto.",
  "matriz_ia_completa", "63 unidades químicas sin cata, incluyendo no estimables.",
  "matriz_ia_modelable", "Subconjunto con al menos un target IA estimado. Es la hoja recomendada para análisis exploratorio."
)

# ------------------------------------------------------------
# Exportar solo hojas necesarias

hojas_salida <- list(
  "00_resumen" = resumen,
  "01_consenso_muestra" = consenso_muestra,
  "02_matriz_ia_completa" = matriz_ia_completa,
  "03_matriz_ia_modelable" = matriz_ia_modelable,
  "04_revision" = revision,
  "05_diccionario" = diccionario
)

ruta_guardada <- guardar_excel_seguro(hojas_salida, ruta_salida)

cat("\nProceso terminado correctamente.\n")
cat("Archivo generado:\n", ruta_guardada, "\n")
cat("\nResumen clave:\n")
print(resumen)
