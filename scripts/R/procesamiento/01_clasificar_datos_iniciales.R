# Clasificación inicial de archivos, bloques y matrices
# Salida: Excel con clasificación de archivos, bloques, matrices y resumen general

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

cat("\nCLASIFICACIÓN DE DATOS INICIALES\n")

dir.create(dir_procesamiento, recursive = TRUE, showWarnings = FALSE)

# Validación de carpeta de archivos originales

if (!dir.exists(dir_original)) {
  stop(paste("No existe la carpeta de archivos originales:", dir_original))
}

# Clasificación de archivos originales
archivos_clasificados <- resumen_archivos_esperados %>%
  mutate(
    tipo_fuente = case_when(
      clave_archivo %in% c(
        "cata_septiembre",
        "cata_enero",
        "panel_noviembre",
        "sensorial_cepas_ju",
        "cata_alexandra_260603"
      ) ~ "sensorial",

      clave_archivo %in% c(
        "gcfid_constanza",
        "gcms_noviembre",
        "datos_tesis_ju",
        "quimica_alexandra_260603",
        "gcms_constanza_zip"
      ) ~ "quimica",

      clave_archivo == "data_muestras" ~ "referencia",

      TRUE ~ "otro"
    ),
    estado_archivo = ifelse(encontrado, "detectado", "no_detectado"),
    nombre_detectado = ifelse(encontrado, basename(ruta_resuelta), NA),
    carpeta_detectada = ifelse(encontrado, dirname(ruta_resuelta), NA),
    uso = case_when(
      tipo_fuente == "sensorial" ~ "Fuente para extraer targets sensoriales.",
      tipo_fuente == "quimica" ~ "Fuente para extraer unidades analíticas químicas.",
      tipo_fuente == "referencia" ~ "Referencia histórica; no controla el análisis nuevo.",
      TRUE ~ "Revisar."
    )
  ) %>%
  select(
    clave_archivo,
    tipo_fuente,
    estado_archivo,
    nombre_detectado,
    carpeta_detectada,
    ruta_resuelta,
    uso
  )

# Bloques químicos confirmados
bloques_quimicos <- data.frame(
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

  archivo_quimico = c(
    "260603_Alexandra.xlsx",
    "260603_Alexandra.xlsx",
    "260507_Datos_tesis_JU.xlsx",
    "Resultados GC-FID Constanza Vidal_edited.xlsx",
    "Resultados GC-FID Constanza Vidal_edited.xlsx",
    "Resultados GC-MS-Constanza Vidal.zip",
    "Resultados GC-MS-Constanza Vidal.zip",
    "Panel sensorial_06_Noviembre_GC_MS.xlsx"
  ),

  tecnica_quimica = c(
    "Fenoles totales",
    "GC-FID",
    "Variables fermentativas / química JU",
    "GC-FID",
    "GC-FID",
    "GC-MS",
    "GC-MS",
    "GC-MS"
  ),

  descripcion_muestras = c(
    "8 maltas con 3 réplicas cada una.",
    "1 pilsner con 2 réplicas.",
    "13 cepas o condiciones con 3 réplicas cada una.",
    "C70, B1 y H76, cada una con 3 réplicas.",
    "C70, B1, H76, Dalwhinnie y Caol Ila, cada una con 3 réplicas.",
    "XP1 a XP5, cada condición con 2 unidades analíticas.",
    "C1 a C6, cada comercial con 2 unidades analíticas.",
    "NOV_M1 a NOV_M4, una unidad analítica por muestra."
  ),

  muestras_base = c(8, 1, 13, 3, 5, 5, 6, 4),
  replicas_por_base = c(3, 2, 3, 3, 3, 2, 2, 1),
  unidades_analiticas = c(24, 2, 39, 9, 15, 10, 12, 4),
  unidades_con_cata = c(0, 2, 18, 9, 15, 0, 4, 4),
  unidades_sin_cata = c(24, 0, 21, 0, 0, 10, 8, 0),

  archivo_sensorial_asociado = c(
    NA,
    "260603_Resultados cata_Alexandra.xlsx",
    "sensorial_cepas_JU.xlsx",
    "Cata sensorial_Septiembre_2024.xlsx",
    "Cata Social_Enero_2025.xlsx",
    NA,
    "Cata Social_Enero_2025.xlsx",
    "Panel sensorial_06_Noviembre.xlsx"
  ),

  evaluaciones_cata = c(0, 9, 6, 26, 145, 0, 57, 40),
  observaciones_expandidas = c(0, 18, 18, 78, 435, 0, 114, 40),

  uso = c(
    "quimica_sin_cata",
    "puro_agregado / puro_expandido_evaluador",
    "puro_agregado / quimica_sin_cata",
    "puro_agregado / puro_expandido_evaluador",
    "puro_agregado / puro_expandido_evaluador",
    "quimica_sin_cata",
    "puro_agregado parcial / quimica_sin_cata",
    "puro_agregado / puro_expandido_evaluador"
  ),

  comentario = c(
    "No tiene cata confirmada.",
    "Solo la Muestra 1 de la cata se asocia a estas 2 réplicas.",
    "6 muestras base tienen cata; 7 no tienen cata.",
    "Septiembre se trata como bloque separado.",
    "Enero se trata como bloque separado.",
    "Datos experimentales sin cata confirmada.",
    "C1 y C2 tienen cata confirmada; C3-C6 no confirmada.",
    "NOV_M5 tiene cata, pero no tiene GC-MS."
  ),

  stringsAsFactors = FALSE
)

# Bloques sensoriales confirmados
bloques_sensoriales <- data.frame(
  bloque_id = c(
    "cata_alexandra_260603",
    "sensorial_cepas_ju",
    "cata_septiembre_2024",
    "cata_social_enero_2025",
    "panel_noviembre"
  ),

  archivo_sensorial = c(
    "260603_Resultados cata_Alexandra.xlsx",
    "sensorial_cepas_JU.xlsx",
    "Cata sensorial_Septiembre_2024.xlsx",
    "Cata Social_Enero_2025.xlsx",
    "Panel sensorial_06_Noviembre.xlsx"
  ),

  muestras_sensoriales = c(4, 6, 3, 5, 5),
  evaluaciones_totales = c(36, 6, 26, 145, 50),
  muestras_con_quimica = c(1, 6, 3, 5, 4),
  muestras_sin_quimica = c(3, 0, 0, 0, 1),
  evaluaciones_con_quimica = c(9, 6, 26, 145, 40),
  evaluaciones_sin_quimica = c(27, 0, 0, 0, 10),

  comentario = c(
    "Solo la Muestra 1 tiene química asociada confirmada.",
    "Cada muestra sensorial se asocia a 3 réplicas químicas.",
    "C70, B1 y H76. Septiembre se trata como bloque separado.",
    "C70, B1, H76, Dalwhinnie y Caol Ila. Enero se trata como bloque separado.",
    "Muestra 5 tiene cata, pero no tiene GC-MS."
  ),

  stringsAsFactors = FALSE
)

# Matrices planificadas

total_unidades_quimicas <- sum(bloques_quimicos$unidades_analiticas)
total_unidades_con_cata <- sum(bloques_quimicos$unidades_con_cata)
total_unidades_sin_cata <- sum(bloques_quimicos$unidades_sin_cata)
total_observaciones_expandidas <- sum(bloques_quimicos$observaciones_expandidas)
total_sensorial_sin_quimica <- sum(bloques_sensoriales$evaluaciones_sin_quimica)

matrices_planificadas <- data.frame(
  matriz = c(
    "matriz_pura_agregada",
    "matriz_pura_expandida_evaluador",
    "matriz_quimica_sin_cata",
    "matriz_sensorial_sin_quimica",
    "matriz_cata_individual",
    "matriz_sintetica_ia"
  ),

  unidad_fila = c(
    "Unidad analítica química.",
    "Unidad analítica química × evaluación individual.",
    "Unidad analítica química.",
    "Evaluación sensorial individual.",
    "Unidad analítica química.",
    "Unidad analítica química."
  ),

  tipo_target = c(
    "Promedio real de cata.",
    "Puntaje real individual.",
    "Sin target.",
    "Puntaje real sin química asociada.",
    "Target asignado por experto individual.",
    "Target generado por IA."
  ),

  conteo_estimado = c(
    total_unidades_con_cata,
    total_observaciones_expandidas,
    total_unidades_sin_cata,
    total_sensorial_sin_quimica,
    NA,
    NA
  ),

  clasificacion = c(
    "puro",
    "puro_expandido",
    "quimica_pura_sin_target",
    "sensorial_puro_sin_quimica",
    "contaminado_controlado",
    "contaminado_sintetico"
  ),

  uso = c(
    "Análisis principal.",
    "Variabilidad sensorial y sensibilidad.",
    "Análisis químico no supervisado.",
    "Análisis sensorial descriptivo.",
    "Escenario incrementado con error experto.",
    "Escenario sintético para análisis de sensibilidad."
  ),

  stringsAsFactors = FALSE
)

#Resumen general

resumen_general <- data.frame(
  indicador = c(
    "Carpeta de archivos originales",
    "Archivos originales esperados",
    "Archivos originales detectados",
    "Archivos originales no detectados",
    "Bloques químicos",
    "Bloques sensoriales",
    "Unidades analíticas químicas totales",
    "Unidades analíticas con cata",
    "Unidades analíticas sin cata",
    "Evaluaciones sensoriales totales",
    "Evaluaciones sensoriales sin química",
    "Observaciones expandidas estimadas",
    "Criterio de cruce"
  ),

  valor = c(
    dir_original,
    nrow(archivos_clasificados),
    sum(archivos_clasificados$estado_archivo == "detectado"),
    sum(archivos_clasificados$estado_archivo == "no_detectado"),
    nrow(bloques_quimicos),
    nrow(bloques_sensoriales),
    total_unidades_quimicas,
    total_unidades_con_cata,
    total_unidades_sin_cata,
    sum(bloques_sensoriales$evaluaciones_totales),
    total_sensorial_sin_quimica,
    total_observaciones_expandidas,
    "Septiembre y enero se tratan como bloques separados."
  ),

  stringsAsFactors = FALSE
)

# Mapeo usado para cruces

mapeo_usado <- mapeo_muestras %>%
  select(
    bloque,
    muestra_base,
    identificador,
    muestra_cata
  )

#Exportar Excel
ruta_salida <- file.path(
  dir_procesamiento,
  "clasificacion_datos_iniciales.xlsx"
)

write_xlsx(
  list(
    "Resumen" = resumen_general,
    "01_archivos" = archivos_clasificados,
    "02_bloques_quimicos" = bloques_quimicos,
    "03_bloques_sensoriales" = bloques_sensoriales,
    "04_matrices" = matrices_planificadas,
    "05_mapeo_muestras" = mapeo_usado
  ),
  path = ruta_salida
)


cat("\nClasificación generada\n")

cat("\nArchivo generado:\n")
cat(ruta_salida, "\n")

cat("\nResumen general:\n")
print(resumen_general)

cat("\nArchivos detectados por tipo:\n")
print(table(archivos_clasificados$tipo_fuente, archivos_clasificados$estado_archivo))

cat("\nMatrices planificadas:\n")
print(matrices_planificadas)

cat("\nProceso terminado correctamente.\n")