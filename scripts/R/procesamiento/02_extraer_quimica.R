
# 02_extraer_quimica.R

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

cat("\nEXTRACCIÓN DE QUÍMICA\n")

dir.create(dir_procesamiento, recursive = TRUE, showWarnings = FALSE)

#Funciones generales

crear_unidad_id <- function(bloque_id, muestra_base, replica_id) {
  paste0(
    limpiar_nombre_variable(bloque_id),
    "__",
    limpiar_nombre_variable(muestra_base),
    "_r",
    replica_id
  )
}

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
          "Probablemente está abierto. Se guardará copia en:",
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

meta_quimica <- c(
  "unidad_analitica_id",
  "bloque_id",
  "fuente_archivo",
  "hoja_fuente",
  "tecnica_quimica",
  "muestra_base",
  "identificador",
  "replica_id",
  "archivo_sensorial_asociado",
  "muestra_cata_asociada",
  "tiene_cata_confirmada"
)

armar_wide <- function(largo) {
  if (nrow(largo) == 0) {
    return(data.frame())
  }

  meta <- largo %>%
    distinct(across(all_of(meta_quimica)))

  largo %>%
    select(unidad_analitica_id, variable_quimica, valor) %>%
    pivot_wider(
      names_from = variable_quimica,
      values_from = valor,
      values_fn = mean
    ) %>%
    left_join(meta, by = "unidad_analitica_id") %>%
    relocate(all_of(meta_quimica))
}

armar_largo_desde_wide <- function(wide, tipo_medida = "indice") {
  if (nrow(wide) == 0) {
    return(data.frame())
  }

  vars <- setdiff(names(wide), meta_quimica)

  wide %>%
    pivot_longer(
      cols = all_of(vars),
      names_to = "variable_quimica",
      values_to = "valor"
    ) %>%
    filter(!is.na(valor)) %>%
    mutate(
      compuesto_original = variable_quimica,
      unidad = case_when(
        str_detect(variable_quimica, "pct") ~ "%",
        str_detect(variable_quimica, "^n_") ~ "conteo",
        str_detect(variable_quimica, "ppm") ~ "ppm",
        TRUE ~ "valor"
      ),
      tipo_medida = tipo_medida
    ) %>%
    select(
      all_of(meta_quimica),
      compuesto_original,
      variable_quimica,
      valor,
      unidad,
      tipo_medida
    )
}

# Mapeos GC-FID Constanza


mapear_constanza_base <- function(x) {
  x <- normalizar_texto(x)

  case_when(
    str_detect(x, "c70") ~ "M1",
    str_detect(x, "b1") ~ "M2",
    str_detect(x, "h76") ~ "M3",
    str_detect(x, "dalwhinnie") ~ "M4",
    str_detect(x, "caol") ~ "M7",
    TRUE ~ NA_character_
  )
}

mapear_constanza_id <- function(muestra_base) {
  case_when(
    muestra_base == "M1" ~ "C70",
    muestra_base == "M2" ~ "B1",
    muestra_base == "M3" ~ "H76",
    muestra_base == "M4" ~ "C1 / Dalwhinnie",
    muestra_base == "M7" ~ "C2 / Caol Ila",
    TRUE ~ NA_character_
  )
}

mapear_constanza_cata <- function(muestra_base) {
  case_when(
    muestra_base == "M1" ~ "Muestra 1",
    muestra_base == "M2" ~ "Muestra 2",
    muestra_base == "M3" ~ "Muestra 3",
    muestra_base == "M4" ~ "Muestra 4",
    muestra_base == "M7" ~ "Muestra 7",
    TRUE ~ NA_character_
  )
}

# GC-FID Constanza

extraer_gcfid_constanza <- function(ruta, hoja, bloque_id, archivo_sensorial) {
  if (is.na(ruta)) {
    warning("No se encontró archivo GC-FID Constanza.")
    return(list(largo = data.frame(), wide = data.frame()))
  }

  raw <- readxl::read_excel(ruta, sheet = hoja, col_names = FALSE)
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)

  obtener_celda <- function(i, j) {
    if (i > nrow(raw) || j > ncol(raw)) {
      return(NA)
    }

    raw[[j]][i]
  }

  salida <- list()
  k <- 1
  cols_muestra <- c(2, 5, 8)

  for (fila in seq_len(nrow(raw))) {
    for (col_muestra in cols_muestra) {
      col_valor <- col_muestra + 1

      compuesto <- obtener_celda(fila, col_valor)
      header_muestra <- obtener_celda(fila + 1, col_muestra)
      header_valor <- obtener_celda(fila + 1, col_valor)

      es_bloque <- !is.na(compuesto) &&
        normalizar_texto(header_muestra) == "muestras" &&
        str_detect(normalizar_texto(header_valor), "concentracion")

      if (!es_bloque) {
        next
      }

      fila_actual <- fila + 2

      while (fila_actual <= nrow(raw)) {
        muestra_raw <- obtener_celda(fila_actual, col_muestra)
        valor_raw <- obtener_celda(fila_actual, col_valor)

        fila_vacia <- (is.na(muestra_raw) || normalizar_texto(muestra_raw) == "") &&
          (is.na(valor_raw) || normalizar_texto(valor_raw) == "")

        if (fila_vacia) {
          break
        }

        muestra_base <- mapear_constanza_base(muestra_raw)

        if (!is.na(muestra_base)) {
          salida[[k]] <- data.frame(
            bloque_id = bloque_id,
            fuente_archivo = basename(ruta),
            hoja_fuente = hoja,
            tecnica_quimica = "GC-FID",
            muestra_base = muestra_base,
            identificador = mapear_constanza_id(muestra_base),
            archivo_sensorial_asociado = archivo_sensorial,
            muestra_cata_asociada = mapear_constanza_cata(muestra_base),
            tiene_cata_confirmada = TRUE,
            compuesto_original = as.character(compuesto),
            variable_quimica = paste0(
              "x_gcfid_",
              limpiar_nombre_variable(compuesto),
              "_ppm"
            ),
            valor = convertir_ppm(valor_raw),
            unidad = "ppm",
            tipo_medida = "concentracion",
            stringsAsFactors = FALSE
          )

          k <- k + 1
        }

        fila_actual <- fila_actual + 1
      }
    }
  }

  if (length(salida) == 0) {
    return(list(largo = data.frame(), wide = data.frame()))
  }

  largo <- bind_rows(salida) %>%
    group_by(bloque_id, muestra_base, compuesto_original) %>%
    mutate(replica_id = row_number()) %>%
    ungroup() %>%
    mutate(
      unidad_analitica_id = crear_unidad_id(
        bloque_id,
        muestra_base,
        replica_id
      )
    ) %>%
    select(
      all_of(meta_quimica),
      compuesto_original,
      variable_quimica,
      valor,
      unidad,
      tipo_medida
    )

  list(
    largo = largo,
    wide = armar_wide(largo)
  )
}

gcfid_septiembre <- extraer_gcfid_constanza(
  ruta = ruta_original("gcfid_constanza"),
  hoja = "Cata septiembre 2024",
  bloque_id = "gcfid_constanza_septiembre",
  archivo_sensorial = "Cata sensorial_Septiembre_2024.xlsx"
)

gcfid_enero <- extraer_gcfid_constanza(
  ruta = ruta_original("gcfid_constanza"),
  hoja = "Cata Enero 2025",
  bloque_id = "gcfid_constanza_enero",
  archivo_sensorial = "Cata Social_Enero_2025.xlsx"
)


# 260507 Datos tesis JU
extraer_ju <- function(ruta) {
  if (is.na(ruta)) {
    warning("No se encontró 260507_Datos_tesis_JU.xlsx")
    return(list(largo = data.frame(), wide = data.frame()))
  }

  datos <- readxl::read_excel(ruta, sheet = "Hoja1") %>%
    janitor::clean_names()

  if (!"strain" %in% names(datos)) {
    stop("No se encontró la columna strain en 260507_Datos_tesis_JU.xlsx.")
  }

  muestras_con_cata <- c(
    "B1.1",
    "SC476_1e",
    "SC481_1e",
    "5.1",
    "705.1",
    "815.3"
  )

  datos <- datos %>%
    mutate(
      bloque_id = "ju_260507_quimica_cepas",
      fuente_archivo = basename(ruta),
      hoja_fuente = "Hoja1",
      tecnica_quimica = "Variables fermentativas / química JU",
      muestra_base = as.character(strain),
      identificador = as.character(strain),
      replica_id = ave(seq_along(muestra_base), muestra_base, FUN = seq_along),
      unidad_analitica_id = crear_unidad_id(
        bloque_id,
        muestra_base,
        replica_id
      ),
      tiene_cata_confirmada = muestra_base %in% muestras_con_cata,
      archivo_sensorial_asociado = ifelse(
        tiene_cata_confirmada,
        "sensorial_cepas_JU.xlsx",
        NA_character_
      ),
      muestra_cata_asociada = ifelse(
        tiene_cata_confirmada,
        muestra_base,
        NA_character_
      )
    )

  excluir <- c(meta_quimica, "strain", "specie")
  vars <- setdiff(names(datos), excluir)
  vars <- vars[sapply(datos[vars], is.numeric)]

  largo <- datos %>%
    select(all_of(meta_quimica), all_of(vars)) %>%
    pivot_longer(
      cols = all_of(vars),
      names_to = "variable_raw",
      values_to = "valor"
    ) %>%
    mutate(
      compuesto_original = variable_raw,
      variable_quimica = paste0("x_ju_", limpiar_nombre_variable(variable_raw)),
      unidad = case_when(
        str_detect(variable_raw, "ppm") ~ "ppm",
        str_detect(variable_raw, "percent|faci") ~ "%",
        str_detect(variable_raw, "g_l") ~ "g/L",
        TRUE ~ "valor"
      ),
      tipo_medida = "variable_quimica_ju"
    ) %>%
    select(
      all_of(meta_quimica),
      compuesto_original,
      variable_quimica,
      valor,
      unidad,
      tipo_medida
    )

  list(
    largo = largo,
    wide = armar_wide(largo)
  )
}

ju <- extraer_ju(ruta_original("datos_tesis_ju"))

# 260603 Alexandra: CVs GC-FID
extraer_alexandra_cvs <- function(ruta) {
  if (is.na(ruta)) {
    warning("No se encontró 260603_Alexandra.xlsx")
    return(list(largo = data.frame(), wide = data.frame()))
  }

  raw <- readxl::read_excel(
    ruta,
    sheet = "CVs GC-FID",
    col_names = FALSE
  )

  raw <- as.data.frame(raw, stringsAsFactors = FALSE)

  obtener_celda <- function(i, j) {
    if (i > nrow(raw) || j > ncol(raw)) {
      return(NA)
    }

    raw[[j]][i]
  }

  obtener_compuesto <- function(fila_header) {
    if (fila_header <= 1) {
      return(NA_character_)
    }

    fila_anterior <- unlist(raw[fila_header - 1, ], use.names = FALSE)
    fila_txt <- as.character(fila_anterior)
    fila_norm <- normalizar_texto(fila_txt)

    candidatos <- fila_txt[
      !is.na(fila_txt) &
        fila_norm != "" &
        !stringr::str_detect(fila_norm, "^[0-9]+$")
    ]

    if (length(candidatos) == 0) {
      return(paste0("compuesto_fila_", fila_header))
    }

    candidatos[1]
  }

  salida <- list()
  k <- 1

  for (fila_header in seq_len(nrow(raw))) {
    valores_header <- unlist(raw[fila_header, ], use.names = FALSE)
    valores_norm <- normalizar_texto(valores_header)

    col_muestra <- which(valores_norm == "muestras")[1]
    col_valor <- which(stringr::str_detect(valores_norm, "concentracion"))[1]

    if (is.na(col_muestra) || is.na(col_valor)) {
      next
    }

    compuesto <- obtener_compuesto(fila_header)
    fila_actual <- fila_header + 1

    while (fila_actual <= nrow(raw)) {
      muestra_raw <- obtener_celda(fila_actual, col_muestra)
      valor_raw <- obtener_celda(fila_actual, col_valor)

      fila_vacia <- (
        is.na(muestra_raw) || normalizar_texto(muestra_raw) == ""
      ) && (
        is.na(valor_raw) || normalizar_texto(valor_raw) == ""
      )

      if (fila_vacia) {
        break
      }

      muestra_norm <- normalizar_texto(muestra_raw)

      if (stringr::str_detect(muestra_norm, "pilsner|h76")) {
        replica_extraida <- stringr::str_match(
          muestra_norm,
          "r\\s*([0-9]+)"
        )[, 2]

        replica_extraida <- suppressWarnings(as.integer(replica_extraida))

        salida[[k]] <- data.frame(
          bloque_id = "alexandra_260603_pilsner_gcfid",
          fuente_archivo = basename(ruta),
          hoja_fuente = "CVs GC-FID",
          tecnica_quimica = "GC-FID",
          muestra_base = "Pilsner_H76R1R2",
          identificador = "Pilsner sin ahumar H76",
          replica_id = replica_extraida,
          archivo_sensorial_asociado = "260603_Resultados cata_Alexandra.xlsx",
          muestra_cata_asociada = "Muestra 1",
          tiene_cata_confirmada = TRUE,
          compuesto_original = as.character(compuesto),
          variable_quimica = paste0(
            "x_gcfid_",
            limpiar_nombre_variable(compuesto),
            "_ppm"
          ),
          valor = convertir_ppm(valor_raw),
          unidad = "ppm",
          tipo_medida = "concentracion",
          stringsAsFactors = FALSE
        )

        k <- k + 1
      }

      fila_actual <- fila_actual + 1
    }
  }

  if (length(salida) == 0) {
    warning("No se extrajeron réplicas desde la hoja CVs GC-FID.")
    return(list(largo = data.frame(), wide = data.frame()))
  }

  largo <- dplyr::bind_rows(salida) %>%
    dplyr::group_by(
      bloque_id,
      muestra_base,
      compuesto_original
    ) %>%
    dplyr::mutate(
      replica_id = ifelse(
        is.na(replica_id),
        dplyr::row_number(),
        replica_id
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      replica_id = as.integer(replica_id),
      unidad_analitica_id = crear_unidad_id(
        bloque_id,
        muestra_base,
        replica_id
      )
    ) %>%
    dplyr::select(
      dplyr::all_of(meta_quimica),
      compuesto_original,
      variable_quimica,
      valor,
      unidad,
      tipo_medida
    )

  list(
    largo = largo,
    wide = armar_wide(largo)
  )
}

alexandra_cvs <- extraer_alexandra_cvs(
  ruta_original("quimica_alexandra_260603")
)

# 260603 Alexandra: fenoles totales

extraer_alexandra_fenoles <- function(ruta) {
  if (is.na(ruta)) {
    warning("No se encontró 260603_Alexandra.xlsx")
    return(list(largo = data.frame(), wide = data.frame()))
  }

  datos <- readxl::read_excel(ruta, sheet = "Fenoles totales", skip = 2) %>%
    janitor::clean_names()

  col_muestra <- names(datos)[str_detect(names(datos), "^muestra$|muestras")][1]
  col_fenoles <- names(datos)[str_detect(names(datos), "fenol|galico|tpc")][1]
  col_cantidad <- names(datos)[str_detect(names(datos), "cantidad.*malta|malta.*kg")][1]
  col_batch <- names(datos)[str_detect(names(datos), "batch|fermentacion")][1]

  if (is.na(col_muestra) || is.na(col_fenoles)) {
    stop("No se encontraron columnas mínimas en Fenoles totales.")
  }

  datos <- datos %>%
    filter(
      !is.na(.data[[col_muestra]]),
      normalizar_texto(.data[[col_muestra]]) != ""
    ) %>%
    mutate(
      muestra_nombre_raw = as.character(.data[[col_muestra]]),
      muestra_norm = normalizar_texto(muestra_nombre_raw),
      replica_id = as.integer(str_match(muestra_norm, "r\\s*([0-9]+)$")[, 2]),
      nombre_base = str_remove(muestra_nombre_raw, "\\s+[Rr]\\s*[0-9]+$"),
      nombre_base = trimws(nombre_base),
      nombre_base = str_replace_all(nombre_base, "\\(200g\\)", "(200 g)"),
      cantidad_malta_kg = if (!is.na(col_cantidad)) {
        convertir_numero(.data[[col_cantidad]])
      } else {
        NA_real_
      },
      batch_fermentacion_l = if (!is.na(col_batch)) {
        convertir_numero(.data[[col_batch]])
      } else {
        NA_real_
      },
      identificador = paste0(
        nombre_base,
        " | malta_kg=",
        cantidad_malta_kg,
        " | batch_l=",
        batch_fermentacion_l
      )
    )

  condiciones <- datos %>%
    distinct(identificador) %>%
    mutate(muestra_base = paste0("Malta_", row_number()))

  datos <- datos %>%
    left_join(condiciones, by = "identificador") %>%
    group_by(identificador) %>%
    mutate(replica_id = ifelse(is.na(replica_id), row_number(), replica_id)) %>%
    ungroup() %>%
    mutate(
      bloque_id = "alexandra_260603_fenoles_totales",
      fuente_archivo = basename(ruta),
      hoja_fuente = "Fenoles totales",
      tecnica_quimica = "Fenoles totales",
      unidad_analitica_id = crear_unidad_id(
        bloque_id,
        muestra_base,
        replica_id
      ),
      archivo_sensorial_asociado = NA_character_,
      muestra_cata_asociada = NA_character_,
      tiene_cata_confirmada = FALSE
    )

  largo <- datos %>%
    transmute(
      across(all_of(meta_quimica)),
      compuesto_original = "Fenoles totales Folin",
      variable_quimica = "x_folin_fenoles_totales_ug_ag_ml",
      valor = convertir_numero(.data[[col_fenoles]]),
      unidad = "ug AG/mL",
      tipo_medida = "fenoles_totales"
    )

  list(
    largo = largo,
    wide = armar_wide(largo)
  )
}

alexandra_fenoles <- extraer_alexandra_fenoles(
  ruta_original("quimica_alexandra_260603")
)


# GC-MS noviembre

extraer_gcms_noviembre <- function(ruta) {
  if (is.na(ruta)) {
    warning("No se encontró Panel sensorial_06_Noviembre_GC_MS.xlsx")
    return(list(largo = data.frame(), wide = data.frame()))
  }

  datos <- readxl::read_excel(ruta, sheet = "Hoja1") %>%
    janitor::clean_names()

  cols_muestras <- names(datos)[str_detect(names(datos), "^m[0-9]+$")]

  compuestos <- datos %>%
    select(compound_name, nature, all_of(cols_muestras)) %>%
    pivot_longer(
      cols = all_of(cols_muestras),
      names_to = "muestra",
      values_to = "area"
    ) %>%
    mutate(
      muestra = toupper(muestra),
      muestra_base = paste0("NOV_", muestra),
      identificador = muestra_base,
      replica_id = 1,
      unidad_analitica_id = crear_unidad_id(
        "gcms_noviembre",
        muestra_base,
        replica_id
      ),
      area = convertir_numero(area),
      nature = ifelse(is.na(nature), "", as.character(nature)),
      compound_name = ifelse(is.na(compound_name), "", as.character(compound_name))
    ) %>%
    filter(!is.na(area), area > 0) %>%
    group_by(unidad_analitica_id) %>%
    mutate(
      area_total = sum(area, na.rm = TRUE),
      area_pct = area / area_total * 100
    ) %>%
    ungroup() %>%
    mutate(
      familia_por_nombre = clasificar_compuesto(compound_name)
    )

  wide <- compuestos %>%
    group_by(unidad_analitica_id, muestra_base, identificador, replica_id) %>%
    summarise(
      x_gcms_esteres_pct = sum(
        area_pct[familia_por_nombre == "Ester"],
        na.rm = TRUE
      ),
      x_gcms_fenolicos_pct = sum(
        area_pct[familia_por_nombre == "Fenolico (fenoles, cresoles, guaiacoles)"],
        na.rm = TRUE
      ),
      x_gcms_aldehidos_pct = sum(
        area_pct[familia_por_nombre == "Aldehido"],
        na.rm = TRUE
      ),
      x_gcms_aroma_fermentativo_pct = sum(
        area_pct[familia_por_nombre %in% c("Ester", "Aldehido")],
        na.rm = TRUE
      ),
      ctrl_gcms_siloxano_pct = sum(
        area_pct[familia_por_nombre == "Siloxano (contaminante de columna)"],
        na.rm = TRUE
      ),
      n_compuestos_detectados = n(),
      .groups = "drop"
    ) %>%
    mutate(
      x_gcms_area_valida_pct = 100 - ctrl_gcms_siloxano_pct,
      bloque_id = "gcms_noviembre",
      fuente_archivo = basename(ruta),
      hoja_fuente = "Hoja1",
      tecnica_quimica = "GC-MS",
      archivo_sensorial_asociado = "Panel sensorial_06_Noviembre.xlsx",
      muestra_cata_asociada = str_replace(muestra_base, "NOV_M", "Muestra "),
      tiene_cata_confirmada = TRUE
    ) %>%
    relocate(all_of(meta_quimica))

  list(
    wide = wide,
    largo = armar_largo_desde_wide(wide, tipo_medida = "indice_gcms")
  )
}

gcms_noviembre <- extraer_gcms_noviembre(
  ruta_original("gcms_noviembre")
)

# 8. GC-MS Constanza ZIP

parsear_metadata_zip <- function(archivo_extraido) {
  ruta_norm <- normalizar_texto(archivo_extraido)
  nombre <- basename(archivo_extraido)
  nombre_norm <- normalizar_texto(nombre)

  bloque_id <- NA_character_
  muestra_base <- NA_character_
  identificador <- NA_character_

  if (str_detect(ruta_norm, "muestras 2-controles comerciales")) {
    bloque_id <- "gcms_constanza_comerciales"

    cnum <- str_match(nombre_norm, "c([1-6])")[, 2]

    if (is.na(cnum)) {
      cnum <- str_match(nombre_norm, "control\\s*([1-6])")[, 2]
    }

    muestra_base <- paste0("C", cnum)

    identificador <- case_when(
      muestra_base == "C1" ~ "C1 / Dalwhinnie",
      muestra_base == "C2" ~ "C2 / Caol Ila",
      TRUE ~ muestra_base
    )
  }

  if (str_detect(ruta_norm, "muestras 3-experimental")) {
    bloque_id <- "gcms_constanza_experimentales"

    condicion <- case_when(
      str_detect(nombre_norm, "1-5-a-30") ~ "1-5-a-30",
      str_detect(nombre_norm, "1-5-25") ~ "1-5-25",
      str_detect(nombre_norm, "1-5-30") ~ "1-5-30",
      str_detect(nombre_norm, "4-5-25") ~ "4-5-25",
      str_detect(nombre_norm, "4-5-30") ~ "4-5-30",
      TRUE ~ NA_character_
    )

    muestra_base <- case_when(
      condicion == "1-5-25" ~ "XP1",
      condicion == "1-5-30" ~ "XP2",
      condicion == "1-5-a-30" ~ "XP3",
      condicion == "4-5-25" ~ "XP4",
      condicion == "4-5-30" ~ "XP5",
      TRUE ~ NA_character_
    )

    identificador <- condicion
  }

  tiene_cata <- muestra_base %in% c("C1", "C2")

  data.frame(
    archivo_extraido = archivo_extraido,
    nombre_archivo = nombre,
    bloque_id = bloque_id,
    muestra_base = muestra_base,
    identificador = identificador,
    tecnica_quimica = "GC-MS",
    archivo_sensorial_asociado = ifelse(
      tiene_cata,
      "Cata Social_Enero_2025.xlsx",
      NA_character_
    ),
    muestra_cata_asociada = case_when(
      muestra_base == "C1" ~ "Muestra 4",
      muestra_base == "C2" ~ "Muestra 7",
      TRUE ~ NA_character_
    ),
    tiene_cata_confirmada = tiene_cata,
    stringsAsFactors = FALSE
  )
}

leer_gcms_xls <- function(path_xls) {
  raw <- tryCatch(
    readxl::read_excel(path_xls, col_names = FALSE),
    error = function(e) NULL
  )

  if (is.null(raw)) {
    return(list(compuestos = data.frame(), fallo = "No se pudo leer el xls."))
  }

  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  raw[] <- lapply(raw, as.character)

  fila_header <- NA_integer_

  for (i in seq_len(nrow(raw))) {
    fila <- normalizar_texto(unlist(raw[i, ], use.names = FALSE))

    if (
      any(str_detect(fila, "compound|compuesto")) &&
        any(str_detect(fila, "area"))
    ) {
      fila_header <- i
      break
    }
  }

  if (is.na(fila_header)) {
    return(list(compuestos = data.frame(), fallo = "No se encontró encabezado."))
  }

  headers <- limpiar_nombre_variable(unlist(raw[fila_header, ], use.names = FALSE))
  tabla <- raw[(fila_header + 1):nrow(raw), , drop = FALSE]
  names(tabla) <- make.unique(headers)

  col_compuesto <- names(tabla)[str_detect(names(tabla), "compound|compuesto|name")][1]
  col_nature <- names(tabla)[str_detect(names(tabla), "nature|naturaleza|family|familia")][1]
  col_area <- names(tabla)[str_detect(names(tabla), "^area$|area")][1]

  if (is.na(col_compuesto) || is.na(col_area)) {
    return(list(compuestos = data.frame(), fallo = "Faltan columnas compuesto/área."))
  }

  compuestos <- data.frame(
    compound_name = as.character(tabla[[col_compuesto]]),
    nature = ifelse(is.na(col_nature), NA_character_, as.character(tabla[[col_nature]])),
    area = convertir_numero(tabla[[col_area]]),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      nature = ifelse(is.na(nature), "", nature),
      compound_name = ifelse(is.na(compound_name), "", compound_name)
    ) %>%
    filter(
      compound_name != "",
      !is.na(area),
      area > 0
    )

  if (nrow(compuestos) == 0) {
    return(list(compuestos = data.frame(), fallo = "Sin compuestos con área válida."))
  }

  list(compuestos = compuestos, fallo = NA_character_)
}

extraer_gcms_zip <- function(ruta_zip) {
  if (is.na(ruta_zip)) {
    warning("No se encontró Resultados GC-MS-Constanza Vidal.zip")
    return(list(largo = data.frame(), wide = data.frame(), fallos = data.frame()))
  }

  temp_dir <- tempfile("gcms_constanza_")
  dir.create(temp_dir, recursive = TRUE)

  unzip(ruta_zip, exdir = temp_dir)

  archivos_xls <- list.files(
    temp_dir,
    pattern = "\\.xls$",
    recursive = TRUE,
    full.names = TRUE
  )

  archivos_xls <- archivos_xls[
    str_detect(
      normalizar_texto(archivos_xls),
      "muestras 2-controles comerciales|muestras 3-experimental"
    )
  ]

  if (length(archivos_xls) == 0) {
    return(list(largo = data.frame(), wide = data.frame(), fallos = data.frame()))
  }

  metadata <- bind_rows(lapply(archivos_xls, parsear_metadata_zip)) %>%
    filter(!is.na(bloque_id), !is.na(muestra_base)) %>%
    arrange(bloque_id, muestra_base, nombre_archivo) %>%
    group_by(bloque_id, muestra_base) %>%
    mutate(
      replica_id = row_number(),
      unidad_analitica_id = crear_unidad_id(bloque_id, muestra_base, replica_id),
      fuente_archivo = basename(ruta_zip),
      hoja_fuente = nombre_archivo
    ) %>%
    ungroup()

  lista_compuestos <- list()
  lista_fallos <- list()

  for (i in seq_len(nrow(metadata))) {
    meta_i <- metadata[i, ]
    lectura <- leer_gcms_xls(meta_i$archivo_extraido)

    if (!is.na(lectura$fallo)) {
      lista_fallos[[i]] <- data.frame(
        unidad_analitica_id = meta_i$unidad_analitica_id,
        bloque_id = meta_i$bloque_id,
        nombre_archivo = meta_i$nombre_archivo,
        motivo_fallo = lectura$fallo,
        stringsAsFactors = FALSE
      )
    }

    if (nrow(lectura$compuestos) > 0) {
      lista_compuestos[[i]] <- lectura$compuestos %>%
        mutate(
          unidad_analitica_id = meta_i$unidad_analitica_id,
          bloque_id = meta_i$bloque_id,
          fuente_archivo = meta_i$fuente_archivo,
          hoja_fuente = meta_i$hoja_fuente,
          tecnica_quimica = "GC-MS",
          muestra_base = meta_i$muestra_base,
          identificador = meta_i$identificador,
          replica_id = meta_i$replica_id,
          archivo_sensorial_asociado = meta_i$archivo_sensorial_asociado,
          muestra_cata_asociada = meta_i$muestra_cata_asociada,
          tiene_cata_confirmada = meta_i$tiene_cata_confirmada
        ) %>%
        group_by(unidad_analitica_id) %>%
        mutate(
          area_total = sum(area, na.rm = TRUE),
          area_pct = area / area_total * 100
        ) %>%
        ungroup() %>%
        mutate(
          familia_por_nombre = clasificar_compuesto(compound_name)
        )
    }
  }

  compuestos <- bind_rows(lista_compuestos)
  fallos <- bind_rows(lista_fallos)

  if (nrow(compuestos) == 0) {
    wide <- metadata %>%
      transmute(
        unidad_analitica_id,
        bloque_id,
        fuente_archivo,
        hoja_fuente,
        tecnica_quimica,
        muestra_base,
        identificador,
        replica_id,
        archivo_sensorial_asociado,
        muestra_cata_asociada,
        tiene_cata_confirmada
      )

    return(list(largo = data.frame(), wide = wide, fallos = fallos))
  }

  wide_features <- compuestos %>%
    group_by(unidad_analitica_id) %>%
    summarise(
      x_gcms_esteres_pct = sum(
        area_pct[familia_por_nombre == "Ester"],
        na.rm = TRUE
      ),
      x_gcms_fenolicos_pct = sum(
        area_pct[familia_por_nombre == "Fenolico (fenoles, cresoles, guaiacoles)"],
        na.rm = TRUE
      ),
      x_gcms_aldehidos_pct = sum(
        area_pct[familia_por_nombre == "Aldehido"],
        na.rm = TRUE
      ),
      x_gcms_aroma_fermentativo_pct = sum(
        area_pct[familia_por_nombre %in% c("Ester", "Aldehido")],
        na.rm = TRUE
      ),
      ctrl_gcms_siloxano_pct = sum(
        area_pct[familia_por_nombre == "Siloxano (contaminante de columna)"],
        na.rm = TRUE
      ),
      n_compuestos_detectados = n(),
      .groups = "drop"
    ) %>%
    mutate(
      x_gcms_area_valida_pct = 100 - ctrl_gcms_siloxano_pct
    )

  wide <- metadata %>%
    transmute(
      unidad_analitica_id,
      bloque_id,
      fuente_archivo,
      hoja_fuente,
      tecnica_quimica,
      muestra_base,
      identificador,
      replica_id,
      archivo_sensorial_asociado,
      muestra_cata_asociada,
      tiene_cata_confirmada
    ) %>%
    left_join(wide_features, by = "unidad_analitica_id")

  list(
    wide = wide,
    largo = armar_largo_desde_wide(wide, tipo_medida = "indice_gcms"),
    fallos = fallos
  )
}

gcms_zip <- extraer_gcms_zip(
  ruta_original("gcms_constanza_zip")
)

# Unir fuentes químicas

quimica_largo <- bind_rows(
  gcfid_septiembre$largo,
  gcfid_enero$largo,
  ju$largo,
  alexandra_cvs$largo,
  alexandra_fenoles$largo,
  gcms_noviembre$largo,
  gcms_zip$largo
) %>%
  arrange(bloque_id, muestra_base, replica_id, variable_quimica)

quimica_wide <- bind_rows(
  gcfid_septiembre$wide,
  gcfid_enero$wide,
  ju$wide,
  alexandra_cvs$wide,
  alexandra_fenoles$wide,
  gcms_noviembre$wide,
  gcms_zip$wide
) %>%
  arrange(bloque_id, muestra_base, replica_id)

# ------------------------------------------------------------
# 10. Revisiones
# ------------------------------------------------------------

conteos_esperados <- data.frame(
  bloque_id = c(
    "alexandra_260603_fenoles_totales",
    "alexandra_260603_pilsner_gcfid",
    "ju_260507_quimica_cepas",
    "gcfid_constanza_septiembre",
    "gcfid_constanza_enero",
    "gcms_constanza_experimentales",
    "gcms_constanza_comerciales",
    "gcms_noviembre"
  ),
  unidades_esperadas = c(24, 2, 39, 9, 15, 10, 12, 4),
  con_cata_esperadas = c(0, 2, 18, 9, 15, 0, 4, 4),
  sin_cata_esperadas = c(24, 0, 21, 0, 0, 10, 8, 0),
  stringsAsFactors = FALSE
)

conteos_observados <- quimica_wide %>%
  group_by(bloque_id) %>%
  summarise(
    unidades_observadas = n(),
    con_cata_observadas = sum(tiene_cata_confirmada == TRUE, na.rm = TRUE),
    sin_cata_observadas = sum(tiene_cata_confirmada == FALSE, na.rm = TRUE),
    .groups = "drop"
  )

revision_conteos <- conteos_esperados %>%
  left_join(conteos_observados, by = "bloque_id") %>%
  mutate(
    across(
      c(unidades_observadas, con_cata_observadas, sin_cata_observadas),
      ~ ifelse(is.na(.x), 0, .x)
    ),
    coincide_unidades = unidades_esperadas == unidades_observadas,
    coincide_con_cata = con_cata_esperadas == con_cata_observadas,
    coincide_sin_cata = sin_cata_esperadas == sin_cata_observadas
  )

resumen_bloque <- quimica_wide %>%
  group_by(bloque_id, tecnica_quimica, tiene_cata_confirmada) %>%
  summarise(
    n_unidades = n(),
    n_muestras_base = n_distinct(muestra_base),
    .groups = "drop"
  ) %>%
  arrange(bloque_id, desc(tiene_cata_confirmada))

vars_quimicas <- setdiff(names(quimica_wide), meta_quimica)

resumen_global <- data.frame(
  indicador = c(
    "Unidades analíticas químicas extraídas",
    "Unidades con cata confirmada",
    "Unidades sin cata confirmada",
    "Variables químicas detectadas",
    "Bloques químicos extraídos",
    "Archivo de salida"
  ),
  valor = c(
    nrow(quimica_wide),
    sum(quimica_wide$tiene_cata_confirmada == TRUE, na.rm = TRUE),
    sum(quimica_wide$tiene_cata_confirmada == FALSE, na.rm = TRUE),
    length(vars_quimicas),
    n_distinct(quimica_wide$bloque_id),
    "quimica_unidades.xlsx"
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 11. Exportar
# ------------------------------------------------------------

ruta_salida_base <- file.path(
  dir_procesamiento,
  "quimica_unidades.xlsx"
)

ruta_salida <- guardar_excel_seguro(
  hojas = list(
    "00_resumen" = resumen_global,
    "01_quimica_wide" = quimica_wide,
    "02_quimica_largo" = quimica_largo,
    "03_revision_conteos" = revision_conteos,
    "04_resumen_bloque" = resumen_bloque
  ),
  ruta_salida = ruta_salida_base
)


cat("\nEXTRACCIÓN QUÍMICA FINALIZADA\n")

cat("\nArchivo generado:\n")
cat(ruta_salida, "\n")

cat("\nResumen global:\n")
print(resumen_global)

cat("\nRevisión de conteos:\n")
print(revision_conteos)

cat("\nProceso terminado correctamente.\n")
