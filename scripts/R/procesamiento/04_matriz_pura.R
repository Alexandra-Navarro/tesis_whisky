source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

cat("\nSe construye la matriz pura\n")

dir.create(dir_procesamiento, recursive = TRUE, showWarnings = FALSE)

# Obtenemos las rutas de los archivos creados anteriormente

#Datos quimicos
ruta_quimica <- file.path(
  dir_procesamiento,
  "quimica_unidades.xlsx"
)

#Datos sensoriales
ruta_sensorial <- file.path(
  dir_procesamiento,
  "sensorial_targets.xlsx"
)

#Salida
ruta_salida_base <- file.path(
  dir_procesamiento,
  "matriz_pura.xlsx"
)

if (!file.exists(ruta_quimica)) {
  stop(paste("No existe el archivo químico:", ruta_quimica))
}

if (!file.exists(ruta_sensorial)) {
  stop(paste("No existe el archivo sensorial:", ruta_sensorial))
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

fuente_target <- function(bloque_sensorial) {
  case_when(
    bloque_sensorial %in% c(
      "cata_alexandra_260603",
      "sensorial_cepas_ju",
      "cata_septiembre_2024",
      "cata_social_enero_2025",
      "panel_noviembre"
    ) ~ "cata_real_agregada",
    TRUE ~ NA_character_
  )
}

#Lectura de datos

quimica_wide <- readxl::read_excel(
  ruta_quimica,
  sheet = "01_quimica_wide"
) %>%
  as.data.frame(stringsAsFactors = FALSE)

sensorial_agregado <- readxl::read_excel(
  ruta_sensorial,
  sheet = "01_sensorial_agregado_wide"
) %>%
  as.data.frame(stringsAsFactors = FALSE)

# Preparacion de los datos
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

vars_quimicas <- setdiff(names(quimica_wide), meta_quimica)

quimica_preparada <- quimica_wide %>%
  mutate(
    tiene_cata_confirmada = estandarizar_logico(tiene_cata_confirmada),
    replica_id = as.integer(replica_id),
    bloque_sensorial = asignar_bloque_sensorial(bloque_id, muestra_base),
    muestra_cata = as.character(muestra_cata_asociada)
  ) %>%
  rename(
    archivo_quimico = fuente_archivo,
    hoja_quimica = hoja_fuente,
    identificador_quimico = identificador,
    tiene_cata_confirmada_quimica = tiene_cata_confirmada
  )

quimica_con_cata <- quimica_preparada %>%
  filter(tiene_cata_confirmada_quimica == TRUE)

#Se agrega el panel sensorial
target_cols <- names(sensorial_agregado)[
  stringr::str_detect(names(sensorial_agregado), "^y_")
]

sensorial_preparado <- sensorial_agregado %>%
  mutate(
    tiene_quimica_confirmada = estandarizar_logico(tiene_quimica_confirmada),
    muestra_cata = as.character(muestra_cata)
  ) %>%
  rename(
    hoja_sensorial = hoja_fuente,
    tiene_quimica_confirmada_sensorial = tiene_quimica_confirmada
  ) %>%
  select(
    archivo_sensorial,
    hoja_sensorial,
    bloque_sensorial,
    muestra_cata,
    identificador_sensorial,
    tiene_quimica_confirmada_sensorial,
    n_evaluadores,
    all_of(target_cols)
  )

# Construcción competa de la matriz pura

matriz_pura <- quimica_con_cata %>%
  left_join(
    sensorial_preparado,
    by = c("bloque_sensorial", "muestra_cata")
  ) %>%
  mutate(
    tipo_dato = "puro_agregado",
    grupo_matriz = clasificar_grupo_matriz(bloque_id),
    nivel_quimico = "unidad_analitica",
    nivel_sensorial = "promedio_muestra",
    fuente_target = fuente_target(bloque_sensorial),
    cruce_quimica_sensorial_ok = !is.na(archivo_sensorial),
    observacion_metodologica = "Química real cruzada con promedio de cata real. Las réplicas químicas comparten el mismo promedio sensorial de su muestra base."
  )

matriz_pura$n_targets_disponibles <- rowSums(
  !is.na(matriz_pura[, target_cols, drop = FALSE])
)

#Se ordenan las columnas de la matriz pura para que las metadatos, targets y predictores quimicos estén al inicio

meta_matriz <- c(
  "tipo_dato",
  "grupo_matriz",
  "unidad_analitica_id",
  "bloque_id",
  "bloque_sensorial",
  "muestra_base",
  "muestra_cata",
  "identificador_quimico",
  "identificador_sensorial",
  "replica_id",
  "tecnica_quimica",
  "archivo_quimico",
  "hoja_quimica",
  "archivo_sensorial",
  "hoja_sensorial",
  "archivo_sensorial_asociado",
  "muestra_cata_asociada",
  "tiene_cata_confirmada_quimica",
  "tiene_quimica_confirmada_sensorial",
  "n_evaluadores",
  "nivel_quimico",
  "nivel_sensorial",
  "fuente_target",
  "cruce_quimica_sensorial_ok",
  "n_targets_disponibles",
  "observacion_metodologica"
)

predictores_quimicos <- names(matriz_pura)[
  stringr::str_detect(names(matriz_pura), "^(x_|ctrl_|n_)")
]

matriz_pura <- matriz_pura %>%
  select(
    all_of(intersect(meta_matriz, names(.))),
    all_of(intersect(target_cols, names(.))),
    all_of(intersect(predictores_quimicos, names(.))),
    everything()
  ) %>%
  arrange(
    grupo_matriz,
    bloque_id,
    muestra_base,
    replica_id
  )

# ------------------------------------------------------------
# Definicion 2026-07-10: la matriz pura NO incluye la cata individual JU.
# La cata individual JU es real, pero corresponde a un solo catador por
# muestra (no un promedio de panel como los demas bloques), por lo que
# tiene un margen de error distinto y no debe mezclarse con la cata
# promedio de panel que define "pura". Esas filas se separan aqui y
# quedan disponibles en su propia hoja para que
# 07_matriz_sensibilidad_cata_individual.R construya la matriz
# "sensorial cata individual" (pura + JU agregada), sin que JU forme
# parte de la matriz pura oficial ni de ningun escenario derivado de ella
# (matriz expandida por evaluador, matrices sin cruce, etc.).

es_cata_individual_ju <- matriz_pura$bloque_id == "ju_260507_quimica_cepas" |
  matriz_pura$bloque_sensorial == "sensorial_cepas_ju"

filas_cata_individual_ju <- matriz_pura %>% filter(es_cata_individual_ju)
matriz_pura <- matriz_pura %>% filter(!es_cata_individual_ju)

targets_principales <- intersect(
  c("y_frutal_comun", "y_fenolico_comun"),
  names(matriz_pura)
)

matriz_modelamiento <- matriz_pura %>%
  select(
    all_of(intersect(meta_matriz, names(.))),
    all_of(targets_principales),
    all_of(predictores_quimicos)
  ) %>%
  mutate(
    modelable_frutal = if ("y_frutal_comun" %in% names(.)) {
      !is.na(y_frutal_comun)
    } else {
      FALSE
    },
    modelable_fenolico = if ("y_fenolico_comun" %in% names(.)) {
      !is.na(y_fenolico_comun)
    } else {
      FALSE
    }
  )

#Revisión del cruce de información

revision_bloques <- matriz_pura %>%
  group_by(bloque_id, bloque_sensorial) %>%
  summarise(
    unidades_quimicas = n(),
    muestras_base = n_distinct(muestra_base),
    muestras_cata = n_distinct(muestra_cata),
    unidades_cruzadas = sum(cruce_quimica_sensorial_ok, na.rm = TRUE),
    unidades_sin_cruce = sum(!cruce_quimica_sensorial_ok, na.rm = TRUE),
    coincide_cruce = unidades_quimicas == unidades_cruzadas,
    .groups = "drop"
  ) %>%
  arrange(bloque_id)

revision_global <- data.frame(
  indicador = c(
    "Tipo de matriz",
    "Unidad de fila",
    "Tipo de target",
    "Unidades químicas con cata",
    "Filas en matriz pura",
    "Filas excluidas (cata individual JU)",
    "Filas con cruce correcto",
    "Filas sin cruce sensorial",
    "Targets sensoriales incorporados",
    "Predictores químicos incorporados",
    "Filas modelables para y_frutal_comun",
    "Filas modelables para y_fenolico_comun",
    "Advertencia metodológica",
    "Advertencia metodológica (cata individual JU)"
  ),
  valor = c(
    "puro_agregado",
    "Unidad analítica química",
    "Promedio real de cata por muestra",
    nrow(quimica_con_cata),
    nrow(matriz_pura),
    nrow(filas_cata_individual_ju),
    sum(matriz_pura$cruce_quimica_sensorial_ok, na.rm = TRUE),
    sum(!matriz_pura$cruce_quimica_sensorial_ok, na.rm = TRUE),
    length(target_cols),
    length(predictores_quimicos),
    sum(matriz_modelamiento$modelable_frutal, na.rm = TRUE),
    sum(matriz_modelamiento$modelable_fenolico, na.rm = TRUE),
    "Las réplicas químicas son unidades analíticas reales; si comparten una misma cata promedio, no deben interpretarse como muestras sensoriales independientes.",
    "La matriz pura excluye la cata individual del bloque JU (un solo catador por muestra, no promedio de panel). Esas filas quedan en la hoja 06_filas_cata_individual_ju y se usan en matriz_sensibilidad_cata_individual.xlsx para construir la matriz 'pura + cata individual JU'."
  ),
  stringsAsFactors = FALSE
)

#Targets

completitud_targets <- matriz_pura %>%
  select(unidad_analitica_id, bloque_id, bloque_sensorial, all_of(target_cols)) %>%
  pivot_longer(
    cols = all_of(target_cols),
    names_to = "target",
    values_to = "valor"
  ) %>%
  group_by(target) %>%
  summarise(
    n_total = n(),
    n_con_valor = sum(!is.na(valor)),
    n_sin_valor = sum(is.na(valor)),
    proporcion_con_valor = round(n_con_valor / n_total, 4),
    .groups = "drop"
  ) %>%
  arrange(desc(proporcion_con_valor), target)


#Diccionario de la matriz pura
diccionario <- data.frame(
  columna = c(
    "tipo_dato",
    "unidad_analitica_id",
    "bloque_id",
    "bloque_sensorial",
    "muestra_base",
    "muestra_cata",
    "replica_id",
    "n_evaluadores",
    "y_frutal_comun",
    "y_fenolico_comun",
    "cruce_quimica_sensorial_ok"
  ),
  descripcion = c(
    "Clasificación metodológica del dato; aquí corresponde a puro_agregado.",
    "Identificador único de la unidad analítica química.",
    "Bloque químico de origen.",
    "Bloque sensorial usado para el cruce.",
    "Muestra o condición base química.",
    "Muestra sensorial asociada.",
    "Número de réplica química dentro de la muestra base.",
    "Cantidad de evaluadores usados para calcular el promedio sensorial.",
    "Target común frutal.",
    "Target común fenólico.",
    "Indica si la unidad química encontró target sensorial agregado."
  ),
  stringsAsFactors = FALSE
)

# Generar archivo de salida Excel

ruta_salida <- guardar_excel(
  hojas = list(
    "00_resumen" = revision_global,
    "01_matriz_pura" = matriz_pura,
    "02_modelamiento" = matriz_modelamiento,
    "03_revision_bloques" = revision_bloques,
    "04_completitud_targets" = completitud_targets,
    "05_diccionario" = diccionario,
    "06_filas_cata_individual_ju" = filas_cata_individual_ju
  ),
  ruta_salida = ruta_salida_base
)

cat("\nMatriz pura generada\n")

cat("\nArchivo generado:\n")
cat(ruta_salida, "\n")

cat("\nResumen:\n")
print(revision_global)

cat("\nRevisión por bloque:\n")
print(revision_bloques)

cat("\nProceso terminado correctamente.\n")