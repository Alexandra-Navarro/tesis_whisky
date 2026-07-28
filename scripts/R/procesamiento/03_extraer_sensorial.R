source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

cat("\nSe extraen datos sensoriales\n")

dir.create(dir_procesamiento, recursive = TRUE, showWarnings = FALSE)

#Funciones principales
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

guardar_excel <- function(hojas, ruta_salida) {
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

buscar_columna <- function(df, patron_incluir, patron_excluir = NULL, ocurrencia = 1) {
  nombres <- names(df)
  nombres_norm <- normalizar_texto(nombres)
  
  pos <- which(
    stringr::str_detect(
      nombres_norm,
      stringr::regex(patron_incluir, ignore_case = TRUE)
    )
  )
  
  if (!is.null(patron_excluir)) {
    pos_excluir <- which(
      stringr::str_detect(
        nombres_norm,
        stringr::regex(patron_excluir, ignore_case = TRUE)
      )
    )
    
    pos <- setdiff(pos, pos_excluir)
  }
  
  if (length(pos) < ocurrencia) {
    return(NA_character_)
  }
  
  nombres[pos[ocurrencia]]
}

row_mean_seguro <- function(df, cols) {
  cols_presentes <- intersect(cols, names(df))
  
  if (length(cols_presentes) == 0) {
    return(rep(NA_real_, nrow(df)))
  }
  
  datos <- df[, cols_presentes, drop = FALSE]
  datos[] <- lapply(datos, convertir_numero)
  
  out <- rowMeans(datos, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  
  out
}

crear_evaluador_id <- function(bloque_sensorial, muestra_cata, nombre_catador, fila) {
  bloque_sensorial <- as.character(bloque_sensorial)
  muestra_cata <- as.character(muestra_cata)
  nombre_catador <- as.character(nombre_catador)
  fila <- as.character(fila)
  
  nombre_limpio <- limpiar_nombre_variable(nombre_catador)
  
  faltantes <- is.na(nombre_limpio) | nombre_limpio == ""
  nombre_limpio[faltantes] <- paste0("evaluador_", fila[faltantes])
  
  paste0(
    limpiar_nombre_variable(bloque_sensorial),
    "__",
    limpiar_nombre_variable(muestra_cata),
    "__",
    nombre_limpio
  )
}

crear_observacion_sensorial_id <- function(bloque_sensorial, muestra_cata, evaluador_id) {
  paste0(
    limpiar_nombre_variable(bloque_sensorial),
    "__",
    limpiar_nombre_variable(muestra_cata),
    "__",
    limpiar_nombre_variable(evaluador_id)
  )
}


mapear_num_muestra_formulario <- function(num_muestra, bloque_sensorial) {
  bloque_sensorial <- as.character(bloque_sensorial[1])
  
  num_txt <- as.character(num_muestra)
  num_txt <- trimws(num_txt)
  
  if (bloque_sensorial == "cata_septiembre_2024") {
    return(
      case_when(
        num_txt == "1" ~ "Muestra 1",
        num_txt == "2" ~ "Muestra 2",
        num_txt == "3" ~ "Muestra 3",
        TRUE ~ paste("Muestra", num_txt)
      )
    )
  }
  
  #Mapear cada una de las muestras de las catas para separarlas e identificarlas correctamente
  if (bloque_sensorial == "cata_social_enero_2025") {
    return(
      case_when(
        num_txt == "1" ~ "Muestra 1",
        num_txt == "2" ~ "Muestra 2",
        num_txt == "3" ~ "Muestra 3",
        num_txt == "4" ~ "Muestra 4",
        num_txt == "7" ~ "Muestra 7",
        TRUE ~ paste("Muestra", num_txt)
      )
    )
  }
  
  if (bloque_sensorial == "cata_alexandra_260603") {
    return(
      case_when(
        num_txt == "1" ~ "Muestra 1",
        num_txt == "2" ~ "Muestra 2",
        num_txt == "3" ~ "Muestra 3",
        num_txt == "4" ~ "Muestra 4",
        TRUE ~ paste("Muestra", num_txt)
      )
    )
  }
  
  paste("Muestra", num_txt)
}

mapear_identificador_sensorial <- function(muestra_cata, bloque_sensorial) {
  bloque_sensorial <- as.character(bloque_sensorial[1])
  
  if (bloque_sensorial == "cata_septiembre_2024") {
    return(
      case_when(
        muestra_cata == "Muestra 1" ~ "C70",
        muestra_cata == "Muestra 2" ~ "B1",
        muestra_cata == "Muestra 3" ~ "H76",
        TRUE ~ as.character(muestra_cata)
      )
    )
  }
  
  if (bloque_sensorial == "cata_social_enero_2025") {
    return(
      case_when(
        muestra_cata == "Muestra 1" ~ "C70",
        muestra_cata == "Muestra 2" ~ "B1",
        muestra_cata == "Muestra 3" ~ "H76",
        muestra_cata == "Muestra 4" ~ "C1 / Dalwhinnie",
        muestra_cata == "Muestra 7" ~ "C2 / Caol Ila",
        TRUE ~ as.character(muestra_cata)
      )
    )
  }
  
  if (bloque_sensorial == "cata_alexandra_260603") {
    return(
      case_when(
        muestra_cata == "Muestra 1" ~ "Pilsner sin ahumar H76",
        muestra_cata == "Muestra 2" ~ "Muestra 2 Alexandra",
        muestra_cata == "Muestra 3" ~ "Muestra 3 Alexandra",
        muestra_cata == "Muestra 4" ~ "Muestra 4 Alexandra",
        TRUE ~ as.character(muestra_cata)
      )
    )
  }
  
  as.character(muestra_cata)
}

mapear_quimica_confirmada_sensorial <- function(muestra_cata, bloque_sensorial) {
  bloque_sensorial <- as.character(bloque_sensorial[1])
  
  if (bloque_sensorial == "cata_septiembre_2024") {
    return(muestra_cata %in% c("Muestra 1", "Muestra 2", "Muestra 3"))
  }
  
  if (bloque_sensorial == "cata_social_enero_2025") {
    return(muestra_cata %in% c("Muestra 1", "Muestra 2", "Muestra 3", "Muestra 4", "Muestra 7"))
  }
  
  if (bloque_sensorial == "cata_alexandra_260603") {
    return(muestra_cata == "Muestra 1")
  }
  
  if (bloque_sensorial == "panel_noviembre") {
    return(muestra_cata %in% c("Muestra 1", "Muestra 2", "Muestra 3", "Muestra 4"))
  }
  
  if (bloque_sensorial == "sensorial_cepas_ju") {
    return(rep(TRUE, length(muestra_cata)))
  }
  
  rep(FALSE, length(muestra_cata))
}

# En esta sección se extraen los datos de los formularios 

extraer_formulario_sensorial <- function(ruta, hoja, bloque_sensorial, archivo_sensorial) {
  if (is.na(ruta)) {
    warning(paste("No se encontró archivo sensorial:", archivo_sensorial))
    return(list(individual_wide = data.frame(), individual_largo = data.frame()))
  }
  
  datos <- readxl::read_excel(
    ruta,
    sheet = hoja,
    .name_repair = "unique"
  )
  
  datos <- as.data.frame(datos, stringsAsFactors = FALSE)
  
  col_catador <- buscar_columna(
    datos,
    "nombre.*catador|catador/a|nombre_catador|nombre"
  )

  col_categoria_catador <- buscar_columna(
    datos,
    "categoriz.*catador|categoria.*catador"
  )

  col_muestra <- buscar_columna(
    datos,
    "num_muestra|n.*muestra|muestra n|muestra.*analizar|cata sensorial.*muestra|muestra"
  )
  
  if (is.na(col_muestra)) {
    stop(paste("No se encontró columna de muestra en", archivo_sensorial))
  }
  
  #Targets sensoriales
  cols_targets <- list(
    y_aroma_ahumado = buscar_columna(datos, "aroma.*ahumado|aroma.*intensidad", "notas|texto|comentario"),
    y_aroma_medicinal = buscar_columna(datos, "aroma.*medicinal", "notas|texto|comentario"),
    y_aroma_terroso = buscar_columna(datos, "aroma.*terroso", "notas|texto|comentario"),
    y_aroma_amaderado = buscar_columna(datos, "aroma.*amaderado", "notas|texto|comentario"),
    y_aroma_cafe = buscar_columna(datos, "aroma.*cafe|aroma.*café", "notas|texto|comentario"),
    y_aroma_frutal = buscar_columna(datos, "aroma.*frutal", "notas|texto|comentario"),
    y_aroma_vainilla = buscar_columna(datos, "aroma.*vainilla", "notas|texto|comentario"),
    y_aroma_cereal = buscar_columna(datos, "aroma.*cereal", "notas|texto|comentario"),
    y_sabor_ahumado = buscar_columna(datos, "sabor.*ahumado|intensidad de ahumado", "notas|texto|comentario"),
    y_sabor_medicinal = buscar_columna(datos, "sabor.*medicinal", "notas|texto|comentario"),
    y_sabor_terroso = buscar_columna(datos, "sabor.*terroso", "notas|texto|comentario"),
    y_sabor_amaderado = buscar_columna(datos, "sabor.*amaderado", "notas|texto|comentario"),
    y_sabor_cafe = buscar_columna(datos, "sabor.*cafe|sabor.*café", "notas|texto|comentario"),
    y_sabor_frutal = buscar_columna(datos, "sabor.*frutal", "notas|texto|comentario"),
    y_sabor_vainilla = buscar_columna(datos, "sabor.*vainilla", "notas|texto|comentario"),
    y_sabor_cereal = buscar_columna(datos, "sabor.*cereal", "notas|texto|comentario"),
    y_sabor_sensacion_boca = buscar_columna(datos, "sensacion en boca|sensación en boca", "notas|texto|comentario"),
    y_regusto_duracion = buscar_columna(datos, "regusto.*duracion|regusto.*duración|duracion", "notas|texto|comentario"),
    y_regusto_ahumado = buscar_columna(datos, "regusto.*ahumado", "notas|texto|comentario"),
    y_regusto_complejidad = buscar_columna(datos, "regusto.*complejidad|complejidad", "notas|texto|comentario"),
    y_global_integracion_ahumado = buscar_columna(datos, "integracion.*ahumado|integración.*ahumado", "notas|texto|comentario"),
    y_global_armonia = buscar_columna(datos, "armonia|armonía", "notas|texto|comentario"),
    y_global_tomabilidad = buscar_columna(datos, "tomabilidad", "notas|texto|comentario")
  )
  
  cols_targets <- cols_targets[!is.na(unlist(cols_targets))]
  
  if (length(cols_targets) == 0) {
    stop(paste("No se detectaron columnas sensoriales numéricas en", archivo_sensorial))
  }
  
  individual_wide <- datos %>%
    mutate(
      archivo_sensorial = archivo_sensorial,
      hoja_fuente = hoja,
      bloque_sensorial = bloque_sensorial,
      fila_respuesta = row_number(),
      nombre_catador = if (!is.na(col_catador)) {
        as.character(.data[[col_catador]])
      } else {
        paste0("evaluador_", fila_respuesta)
      },
      categoria_catador = if (!is.na(col_categoria_catador)) {
        trimws(as.character(.data[[col_categoria_catador]]))
      } else {
        NA_character_
      },
      muestra_cata = mapear_num_muestra_formulario(
        .data[[col_muestra]],
        bloque_sensorial
      ),
      identificador_sensorial = mapear_identificador_sensorial(
        muestra_cata,
        bloque_sensorial
      ),
      tiene_quimica_confirmada = mapear_quimica_confirmada_sensorial(
        muestra_cata,
        bloque_sensorial
      ),
      evaluador_id = crear_evaluador_id(
        bloque_sensorial,
        muestra_cata,
        nombre_catador,
        fila_respuesta
      ),
      observacion_sensorial_id = crear_observacion_sensorial_id(
        bloque_sensorial,
        muestra_cata,
        evaluador_id
      )
    )
  
  for (target in names(cols_targets)) {
    col <- cols_targets[[target]]
    individual_wide[[target]] <- convertir_numero(individual_wide[[col]])
  }
  
  individual_wide <- individual_wide %>%
    mutate(
      y_frutal_comun = row_mean_seguro(
        .,
        c("y_aroma_frutal", "y_sabor_frutal")
      ),
      y_ahumado_comun = row_mean_seguro(
        .,
        c("y_aroma_ahumado", "y_sabor_ahumado", "y_regusto_ahumado")
      ),
      y_medicinal_comun = row_mean_seguro(
        .,
        c("y_aroma_medicinal", "y_sabor_medicinal")
      ),
      y_fenolico_comun = row_mean_seguro(
        .,
        c(
          "y_aroma_ahumado",
          "y_sabor_ahumado",
          "y_regusto_ahumado",
          "y_aroma_medicinal",
          "y_sabor_medicinal"
        )
      )
    )
  
  meta_cols <- c(
    "observacion_sensorial_id",
    "archivo_sensorial",
    "hoja_fuente",
    "bloque_sensorial",
    "fila_respuesta",
    "muestra_cata",
    "identificador_sensorial",
    "tiene_quimica_confirmada",
    "evaluador_id",
    "nombre_catador",
    "categoria_catador"
  )

  target_cols <- names(individual_wide)[stringr::str_detect(names(individual_wide), "^y_")]
  
  individual_wide <- individual_wide %>%
    select(
      all_of(meta_cols),
      all_of(target_cols)
    )
  
  individual_largo <- individual_wide %>%
    pivot_longer(
      cols = all_of(target_cols),
      names_to = "target",
      values_to = "valor"
    ) %>%
    filter(!is.na(valor))
  
  list(
    individual_wide = individual_wide,
    individual_largo = individual_largo
  )
}

#Extraer datos del panel de noviembre

extraer_panel_noviembre <- function(ruta) {
  if (is.na(ruta)) {
    warning("No se encontró Panel sensorial_06_Noviembre.xlsx")
    return(list(individual_wide = data.frame(), individual_largo = data.frame()))
  }
  
  cata_raw <- readxl::read_excel(
    ruta,
    sheet = "Hoja1",
    col_names = FALSE
  )
  
  cata_raw <- as.data.frame(cata_raw, stringsAsFactors = FALSE)
  
  obtener_celda <- function(i, j) {
    if (i > nrow(cata_raw) || j > ncol(cata_raw)) {
      return(NA)
    }
    
    cata_raw[[j]][i]
  }
  
  bloques <- data.frame(
    fila = integer(),
    columna = integer(),
    titulo = character(),
    muestra_num = integer(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(cata_raw))) {
    for (j in seq_len(ncol(cata_raw))) {
      valor <- obtener_celda(i, j)
      
      if (!is.na(valor)) {
        valor_txt <- normalizar_texto(valor)
        
        if (stringr::str_detect(valor_txt, "^muestra\\s*[0-9]+")) {
          muestra_num <- stringr::str_match(
            valor_txt,
            "^muestra\\s*([0-9]+)"
          )[, 2]
          
          bloques <- rbind(
            bloques,
            data.frame(
              fila = i,
              columna = j,
              titulo = as.character(valor),
              muestra_num = as.integer(muestra_num),
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }
  }
  
  if (nrow(bloques) == 0) {
    stop("No se detectaron bloques de muestras en Panel sensorial_06_Noviembre.xlsx")
  }
  
  registros <- list()
  k <- 1
  
  for (b in seq_len(nrow(bloques))) {
    fila_inicio <- bloques$fila[b]
    muestra_num <- bloques$muestra_num[b]
    muestra_cata <- paste("Muestra", muestra_num)
    identificador_sensorial <- paste0("NOV_M", muestra_num)
    
    filas_busqueda <- fila_inicio:min(fila_inicio + 6, nrow(cata_raw))
    
    fila_header <- NA_integer_
    col_descriptor <- NA_integer_
    col_prom <- NA_integer_
    
    for (f in filas_busqueda) {
      valores_fila <- unlist(cata_raw[f, ], use.names = FALSE)
      valores_norm <- normalizar_texto(valores_fila)
      
      pos_descriptor <- which(valores_norm == "descriptor")
      pos_prom <- which(valores_norm == "prom")
      
      if (length(pos_descriptor) > 0 && length(pos_prom) > 0) {
        fila_header <- f
        col_descriptor <- pos_descriptor[1]
        col_prom <- pos_prom[1]
        break
      }
    }
    
    if (is.na(fila_header)) {
      warning(paste("No se encontró encabezado para", muestra_cata))
      next
    }
    
    cols_evaluadores <- (col_descriptor + 1):(col_prom - 1)
    nombres_evaluadores <- as.character(
      unlist(cata_raw[fila_header, cols_evaluadores], use.names = FALSE)
    )
    
    fila_actual <- fila_header + 1
    
    while (fila_actual <= nrow(cata_raw)) {
      descriptor <- obtener_celda(fila_actual, col_descriptor)
      
      if (is.na(descriptor) || trimws(as.character(descriptor)) == "") {
        break
      }
      
      descriptor_norm <- normalizar_texto(descriptor)
      
      if (stringr::str_detect(descriptor_norm, "^muestra\\s*[0-9]+")) {
        break
      }
      
      target <- case_when(
        descriptor_norm == "intensidad" ~ "y_intensidad",
        descriptor_norm == "frutal" ~ "y_frutal_comun",
        descriptor_norm == "floral" ~ "y_floral",
        descriptor_norm == "nuez" ~ "y_nuez",
        descriptor_norm == "sulfuroso" ~ "y_sulfuroso",
        descriptor_norm == "mantecoso" ~ "y_mantecoso",
        descriptor_norm == "maltoso" ~ "y_maltoso",
        descriptor_norm == "caramelo" ~ "y_caramelo",
        descriptor_norm == "fenolico" ~ "y_fenolico_comun",
        TRUE ~ paste0("y_", limpiar_nombre_variable(descriptor_norm))
      )
      
      for (e in seq_along(cols_evaluadores)) {
        col_eval <- cols_evaluadores[e]
        nombre_eval <- nombres_evaluadores[e]
        valor <- convertir_numero(obtener_celda(fila_actual, col_eval))
        
        evaluador_id <- crear_evaluador_id(
          "panel_noviembre",
          muestra_cata,
          nombre_eval,
          e
        )
        
        registros[[k]] <- data.frame(
          observacion_sensorial_id = crear_observacion_sensorial_id(
            "panel_noviembre",
            muestra_cata,
            evaluador_id
          ),
          archivo_sensorial = "Panel sensorial_06_Noviembre.xlsx",
          hoja_fuente = "Hoja1",
          bloque_sensorial = "panel_noviembre",
          fila_respuesta = e,
          muestra_cata = muestra_cata,
          identificador_sensorial = identificador_sensorial,
          tiene_quimica_confirmada = mapear_quimica_confirmada_sensorial(
            muestra_cata,
            "panel_noviembre"
          ),
          evaluador_id = evaluador_id,
          nombre_catador = nombre_eval,
          target = target,
          descriptor_original = as.character(descriptor),
          valor = valor,
          stringsAsFactors = FALSE
        )
        
        k <- k + 1
      }
      
      fila_actual <- fila_actual + 1
    }
  }
  
  individual_largo <- bind_rows(registros) %>%
    filter(!is.na(valor))
  
  individual_wide <- individual_largo %>%
    select(
      observacion_sensorial_id,
      archivo_sensorial,
      hoja_fuente,
      bloque_sensorial,
      fila_respuesta,
      muestra_cata,
      identificador_sensorial,
      tiene_quimica_confirmada,
      evaluador_id,
      nombre_catador,
      target,
      valor
    ) %>%
    pivot_wider(
      names_from = target,
      values_from = valor,
      values_fn = mean
    )
  
  list(
    individual_wide = individual_wide,
    individual_largo = individual_largo
  )
}

#Extracción de datos sensoriales de JU

extraer_sensorial_ju <- function(ruta) {
  if (is.na(ruta)) {
    warning("No se encontró sensorial_cepas_JU.xlsx")
    return(list(individual_wide = data.frame(), individual_largo = data.frame()))
  }
  
  resultado <- extraer_formulario_sensorial(
    ruta = ruta,
    hoja = "Hoja1",
    bloque_sensorial = "sensorial_cepas_ju",
    archivo_sensorial = "sensorial_cepas_JU.xlsx"
  )
  
  datos <- readxl::read_excel(
    ruta,
    sheet = "Hoja1",
    .name_repair = "unique"
  ) %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  col_muestra <- buscar_columna(
    datos,
    "cata sensorial.*muestra|muestra n|muestra"
  )
  
  if (!is.na(col_muestra)) {
    muestras <- as.character(datos[[col_muestra]])
    muestras <- trimws(muestras)
    
    if (length(muestras) == nrow(resultado$individual_wide)) {
      resultado$individual_wide$muestra_cata <- muestras
      resultado$individual_wide$identificador_sensorial <- muestras
      resultado$individual_wide$tiene_quimica_confirmada <- TRUE
      resultado$individual_wide$observacion_sensorial_id <- paste0(
        "sensorial_cepas_ju__",
        limpiar_nombre_variable(muestras),
        "__",
        limpiar_nombre_variable(resultado$individual_wide$evaluador_id)
      )
      
      resultado$individual_largo <- resultado$individual_wide %>%
        pivot_longer(
          cols = starts_with("y_"),
          names_to = "target",
          values_to = "valor"
        ) %>%
        filter(!is.na(valor))
    }
  }
  
  resultado
}

# Ejecutar la extracción de datos sensoriales para cada bloque

sensorial_alexandra <- extraer_formulario_sensorial(
  ruta = ruta_original("cata_alexandra_260603"),
  hoja = "Respuestas de formulario 1",
  bloque_sensorial = "cata_alexandra_260603",
  archivo_sensorial = "260603_Resultados cata_Alexandra.xlsx"
)

sensorial_ju <- extraer_sensorial_ju(
  ruta = ruta_original("sensorial_cepas_ju")
)

sensorial_septiembre <- extraer_formulario_sensorial(
  ruta = ruta_original("cata_septiembre"),
  hoja = "Respuestas de formulario 1",
  bloque_sensorial = "cata_septiembre_2024",
  archivo_sensorial = "Cata sensorial_Septiembre_2024.xlsx"
)

sensorial_enero <- extraer_formulario_sensorial(
  ruta = ruta_original("cata_enero"),
  hoja = "Respuestas de formulario 1",
  bloque_sensorial = "cata_social_enero_2025",
  archivo_sensorial = "Cata Social_Enero_2025.xlsx"
)

sensorial_noviembre <- extraer_panel_noviembre(
  ruta = ruta_original("panel_noviembre")
)

#Se unen todos los resultados individuales en un solo data frame

sensorial_individual_wide <- bind_rows(
  sensorial_alexandra$individual_wide,
  sensorial_ju$individual_wide,
  sensorial_septiembre$individual_wide,
  sensorial_enero$individual_wide,
  sensorial_noviembre$individual_wide
) %>%
  arrange(
    bloque_sensorial,
    muestra_cata,
    evaluador_id
  )

sensorial_largo <- bind_rows(
  sensorial_alexandra$individual_largo,
  sensorial_ju$individual_largo,
  sensorial_septiembre$individual_largo,
  sensorial_enero$individual_largo,
  sensorial_noviembre$individual_largo
) %>%
  arrange(
    bloque_sensorial,
    muestra_cata,
    evaluador_id,
    target
  )

target_cols <- names(sensorial_individual_wide)[
  stringr::str_detect(names(sensorial_individual_wide), "^y_")
]

sensorial_agregado_wide <- sensorial_individual_wide %>%
  group_by(
    archivo_sensorial,
    hoja_fuente,
    bloque_sensorial,
    muestra_cata,
    identificador_sensorial,
    tiene_quimica_confirmada
  ) %>%
  summarise(
    n_evaluadores = n_distinct(evaluador_id),
    across(
      all_of(target_cols),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      all_of(target_cols),
      ~ ifelse(is.nan(.x), NA_real_, .x)
    )
  ) %>%
  arrange(
    bloque_sensorial,
    muestra_cata
  )

sensorial_agregado_largo <- sensorial_agregado_wide %>%
  pivot_longer(
    cols = all_of(target_cols),
    names_to = "target",
    values_to = "valor_promedio"
  ) %>%
  filter(!is.na(valor_promedio))

sensorial_sin_quimica <- sensorial_individual_wide %>%
  filter(tiene_quimica_confirmada == FALSE) %>%
  arrange(bloque_sensorial, muestra_cata)


# Revisión de conteos de muestras y evaluaciones sensoriales

conteos_observados <- sensorial_individual_wide %>%
  group_by(bloque_sensorial) %>%
  summarise(
    muestras_sensoriales_observadas = n_distinct(muestra_cata),
    evaluaciones_observadas = n(),
    muestras_con_quimica_observadas = n_distinct(
      muestra_cata[tiene_quimica_confirmada == TRUE]
    ),
    muestras_sin_quimica_observadas = n_distinct(
      muestra_cata[tiene_quimica_confirmada == FALSE]
    ),
    evaluaciones_con_quimica_observadas = sum(tiene_quimica_confirmada == TRUE, na.rm = TRUE),
    evaluaciones_sin_quimica_observadas = sum(tiene_quimica_confirmada == FALSE, na.rm = TRUE),
    .groups = "drop"
  )

conteos_esperados <- data.frame(
  bloque_sensorial = c(
    "cata_alexandra_260603",
    "sensorial_cepas_ju",
    "cata_septiembre_2024",
    "cata_social_enero_2025",
    "panel_noviembre"
  ),
  muestras_sensoriales_esperadas = c(4, 6, 3, 5, 5),
  evaluaciones_esperadas = c(36, 6, 26, 145, 50),
  muestras_con_quimica_esperadas = c(1, 6, 3, 5, 4),
  muestras_sin_quimica_esperadas = c(3, 0, 0, 0, 1),
  evaluaciones_con_quimica_esperadas = c(9, 6, 26, 145, 40),
  evaluaciones_sin_quimica_esperadas = c(27, 0, 0, 0, 10),
  stringsAsFactors = FALSE
)

revision_conteos <- conteos_esperados %>%
  left_join(
    conteos_observados,
    by = "bloque_sensorial"
  ) %>%
  mutate(
    across(
      ends_with("_observadas"),
      ~ ifelse(is.na(.x), 0, .x)
    ),
    coincide_muestras = muestras_sensoriales_esperadas == muestras_sensoriales_observadas,
    coincide_evaluaciones = evaluaciones_esperadas == evaluaciones_observadas,
    coincide_muestras_con_quimica = muestras_con_quimica_esperadas == muestras_con_quimica_observadas,
    coincide_muestras_sin_quimica = muestras_sin_quimica_esperadas == muestras_sin_quimica_observadas,
    coincide_eval_con_quimica = evaluaciones_con_quimica_esperadas == evaluaciones_con_quimica_observadas,
    coincide_eval_sin_quimica = evaluaciones_sin_quimica_esperadas == evaluaciones_sin_quimica_observadas
  )

resumen_global <- data.frame(
  indicador = c(
    "Muestras sensoriales extraídas",
    "Evaluaciones sensoriales individuales extraídas",
    "Evaluaciones con química confirmada",
    "Evaluaciones sin química confirmada",
    "Targets sensoriales detectados",
    "Bloques sensoriales extraídos",
    "Evaluaciones con categoría de catador",
    "Archivo de salida",
    "Advertencia metodológica"
  ),
  valor = c(
    n_distinct(
      paste(
        sensorial_individual_wide$bloque_sensorial,
        sensorial_individual_wide$muestra_cata
      )
    ),
    nrow(sensorial_individual_wide),
    sum(sensorial_individual_wide$tiene_quimica_confirmada == TRUE, na.rm = TRUE),
    sum(sensorial_individual_wide$tiene_quimica_confirmada == FALSE, na.rm = TRUE),
    length(target_cols),
    n_distinct(sensorial_individual_wide$bloque_sensorial),
    sum(!is.na(sensorial_individual_wide$categoria_catador)),
    "sensorial_targets.xlsx",
    "La matriz individual contiene evaluaciones reales por catador; no debe interpretarse como muestras independientes."
  ),
  stringsAsFactors = FALSE
)

# Diccionario de targets sensoriales
diccionario_targets <- data.frame(
  target = c(
    "y_frutal_comun",
    "y_fenolico_comun",
    "y_ahumado_comun",
    "y_medicinal_comun",
    "y_aroma_frutal",
    "y_sabor_frutal",
    "y_aroma_ahumado",
    "y_sabor_ahumado",
    "y_regusto_ahumado",
    "y_aroma_medicinal",
    "y_sabor_medicinal",
    "y_intensidad",
    "y_floral",
    "y_nuez",
    "y_sulfuroso",
    "y_mantecoso",
    "y_maltoso",
    "y_caramelo"
  ),
  descripcion = c(
    "Target común frutal. En formularios corresponde al promedio de aroma frutal y sabor frutal; en noviembre corresponde al descriptor Frutal.",
    "Target común fenólico. En formularios corresponde al promedio de ahumado y medicinal; en noviembre corresponde al descriptor Fenólico.",
    "Promedio de aroma ahumado, sabor ahumado y regusto ahumado.",
    "Promedio de aroma medicinal y sabor medicinal.",
    "Puntaje individual de aroma frutal.",
    "Puntaje individual de sabor frutal.",
    "Puntaje individual de aroma ahumado.",
    "Puntaje individual de sabor ahumado.",
    "Puntaje individual de regusto ahumado.",
    "Puntaje individual de aroma medicinal.",
    "Puntaje individual de sabor medicinal.",
    "Descriptor de intensidad del panel de noviembre.",
    "Descriptor floral del panel de noviembre.",
    "Descriptor nuez del panel de noviembre.",
    "Descriptor sulfuroso del panel de noviembre.",
    "Descriptor mantecoso del panel de noviembre.",
    "Descriptor maltoso del panel de noviembre.",
    "Descriptor caramelo del panel de noviembre."
  ),
  uso_recomendado = c(
    "Target principal común.",
    "Target principal común.",
    "Descriptor complementario.",
    "Descriptor complementario.",
    "Descriptor específico.",
    "Descriptor específico.",
    "Descriptor específico.",
    "Descriptor específico.",
    "Descriptor específico.",
    "Descriptor específico.",
    "Descriptor específico.",
    "Descriptor noviembre.",
    "Descriptor noviembre.",
    "Descriptor noviembre.",
    "Descriptor noviembre.",
    "Descriptor noviembre.",
    "Descriptor noviembre.",
    "Descriptor noviembre."
  ),
  stringsAsFactors = FALSE
)

# Exportar resultados a un archivo Excel

ruta_salida_base <- file.path(
  dir_procesamiento,
  "sensorial_targets.xlsx"
)

#Nombre de las hojas en el archivo de salida
ruta_salida <- guardar_excel(
  hojas = list(
    "00_resumen" = resumen_global,
    "01_sensorial_agregado_wide" = sensorial_agregado_wide,
    "02_sensorial_individual_wide" = sensorial_individual_wide,
    "03_sensorial_largo" = sensorial_largo,
    "04_agregado_largo" = sensorial_agregado_largo,
    "05_sensorial_sin_quimica" = sensorial_sin_quimica,
    "06_revision_conteos" = revision_conteos,
    "07_diccionario_targets" = diccionario_targets
  ),
  ruta_salida = ruta_salida_base
)

cat("\nSe obtienen los siguientes resultados\n")

cat("\nArchivo generado:\n")
cat(ruta_salida, "\n")

cat("\nResumen global:\n")
print(resumen_global)

cat("\nRevisión de conteos:\n")
print(revision_conteos)

cat("\nProceso terminado correctamente.\n")

