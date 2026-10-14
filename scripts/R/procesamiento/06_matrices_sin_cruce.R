
source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

cat("\nCONSTRUCCIÓN DE MATRICES SIN CRUCE\n")

dir.create(dir_procesamiento, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Rutas

ruta_quimica <- file.path(
dir_procesamiento,
"quimica_unidades.xlsx"
)

ruta_sensorial <- file.path(
dir_procesamiento,
"sensorial_targets.xlsx"
)

ruta_salida_base <- file.path(
dir_procesamiento,
"matrices_sin_cruce.xlsx"
)

if (!file.exists(ruta_quimica)) {
stop(paste("No existe el archivo químico:", ruta_quimica))
}

if (!file.exists(ruta_sensorial)) {
stop(paste("No existe el archivo sensorial:", ruta_sensorial))
}

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

clasificar_grupo_quimico <- function(bloque_id) {
case_when(
bloque_id == "alexandra_260603_fenoles_totales" ~ "fenoles_totales",
bloque_id == "ju_260507_quimica_cepas" ~ "ju_cepas",
bloque_id %in% c(
"gcms_constanza_experimentales",
"gcms_constanza_comerciales"
) ~ "gcms",
TRUE ~ "otro"
)
}

uso_quimica_sin_cata <- function(bloque_id) {
case_when(
bloque_id == "alexandra_260603_fenoles_totales" ~
"Análisis químico exploratorio; no modelable sin target sensorial.",
bloque_id == "ju_260507_quimica_cepas" ~
"Análisis químico exploratorio de cepas sin cata asociada.",
bloque_id == "gcms_constanza_experimentales" ~
"Análisis químico exploratorio de muestras experimentales sin cata.",
bloque_id == "gcms_constanza_comerciales" ~
"Análisis químico exploratorio de comerciales sin cata confirmada.",
TRUE ~
"Química real sin target sensorial confirmado."
)
}

uso_sensorial_sin_quimica <- function(bloque_sensorial) {
case_when(
bloque_sensorial == "cata_alexandra_260603" ~
"Cata real sin química asociada; usar solo para análisis sensorial descriptivo.",
bloque_sensorial == "panel_noviembre" ~
"Cata real sin GC-MS asociado; usar solo para análisis sensorial descriptivo.",
TRUE ~
"Cata real sin química asociada confirmada."
)
}

# ------------------------------------------------------------
# Leer datos procesados

quimica_wide <- readxl::read_excel(
ruta_quimica,
sheet = "01_quimica_wide"
) %>%
as.data.frame(stringsAsFactors = FALSE)

sensorial_individual <- readxl::read_excel(
ruta_sensorial,
sheet = "02_sensorial_individual_wide"
) %>%
as.data.frame(stringsAsFactors = FALSE)

sensorial_agregado <- readxl::read_excel(
ruta_sensorial,
sheet = "01_sensorial_agregado_wide"
) %>%
as.data.frame(stringsAsFactors = FALSE)

# ------------------------------------------------------------
# Matriz química sin cata

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

matriz_quimica_sin_cata <- quimica_wide %>%
mutate(
tiene_cata_confirmada = estandarizar_logico(tiene_cata_confirmada),
replica_id = as.integer(replica_id)
) %>%
filter(
tiene_cata_confirmada == FALSE | is.na(tiene_cata_confirmada)
) %>%
rename(
archivo_quimico = fuente_archivo,
hoja_quimica = hoja_fuente,
identificador_quimico = identificador
) %>%
mutate(
tipo_dato = "quimica_sin_cata",
grupo_matriz = clasificar_grupo_quimico(bloque_id),
nivel_quimico = "unidad_analitica",
nivel_sensorial = "sin_target",
estado_modelamiento = "no_supervisado",
cruce_quimica_sensorial_ok = FALSE,
uso_recomendado = uso_quimica_sin_cata(bloque_id),
observacion_metodologica = "Química real sin target sensorial confirmado. No usar en modelamiento supervisado química-sensorial."
)

if (length(vars_quimicas) > 0) {
matriz_quimica_sin_cata$n_variables_quimicas_disponibles <- rowSums(
!is.na(matriz_quimica_sin_cata[, vars_quimicas, drop = FALSE])
)
} else {
matriz_quimica_sin_cata$n_variables_quimicas_disponibles <- 0
}

meta_quimica_sin_cata <- c(
"tipo_dato",
"grupo_matriz",
"unidad_analitica_id",
"bloque_id",
"muestra_base",
"identificador_quimico",
"replica_id",
"tecnica_quimica",
"archivo_quimico",
"hoja_quimica",
"tiene_cata_confirmada",
"nivel_quimico",
"nivel_sensorial",
"estado_modelamiento",
"cruce_quimica_sensorial_ok",
"n_variables_quimicas_disponibles",
"uso_recomendado",
"observacion_metodologica"
)

matriz_quimica_sin_cata <- matriz_quimica_sin_cata %>%
select(
all_of(intersect(meta_quimica_sin_cata, names(.))),
all_of(intersect(vars_quimicas, names(.))),
everything()
) %>%
arrange(
grupo_matriz,
bloque_id,
muestra_base,
replica_id
)

# ------------------------------------------------------------
# Matriz sensorial sin química individual

target_cols_individual <- names(sensorial_individual)[
stringr::str_detect(names(sensorial_individual), "^y_")
]

matriz_sensorial_sin_quimica <- sensorial_individual %>%
mutate(
tiene_quimica_confirmada = estandarizar_logico(tiene_quimica_confirmada)
) %>%
filter(
tiene_quimica_confirmada == FALSE | is.na(tiene_quimica_confirmada)
) %>%
rename(
hoja_sensorial = hoja_fuente
) %>%
mutate(
tipo_dato = "sensorial_sin_quimica",
nivel_quimico = "sin_quimica",
nivel_sensorial = "evaluacion_individual",
estado_modelamiento = "solo_sensorial",
cruce_quimica_sensorial_ok = FALSE,
uso_recomendado = uso_sensorial_sin_quimica(bloque_sensorial),
observacion_metodologica = "Cata real sin química asociada confirmada. No usar en modelamiento química-sensorial."
)

if (length(target_cols_individual) > 0) {
matriz_sensorial_sin_quimica$n_targets_disponibles <- rowSums(
!is.na(matriz_sensorial_sin_quimica[, target_cols_individual, drop = FALSE])
)
} else {
matriz_sensorial_sin_quimica$n_targets_disponibles <- 0
}

meta_sensorial_sin_quimica <- c(
"tipo_dato",
"observacion_sensorial_id",
"archivo_sensorial",
"hoja_sensorial",
"bloque_sensorial",
"muestra_cata",
"identificador_sensorial",
"evaluador_id",
"nombre_catador",
"fila_respuesta",
"tiene_quimica_confirmada",
"nivel_quimico",
"nivel_sensorial",
"estado_modelamiento",
"cruce_quimica_sensorial_ok",
"n_targets_disponibles",
"uso_recomendado",
"observacion_metodologica"
)

matriz_sensorial_sin_quimica <- matriz_sensorial_sin_quimica %>%
select(
all_of(intersect(meta_sensorial_sin_quimica, names(.))),
all_of(intersect(target_cols_individual, names(.))),
everything()
) %>%
arrange(
bloque_sensorial,
muestra_cata,
evaluador_id
)

# -----------------------------------------------------------
#  Sensorial sin química agregado

target_cols_agregado <- names(sensorial_agregado)[
stringr::str_detect(names(sensorial_agregado), "^y_")
]

sensorial_sin_quimica_agregado <- sensorial_agregado %>%
mutate(
tiene_quimica_confirmada = estandarizar_logico(tiene_quimica_confirmada)
) %>%
filter(
tiene_quimica_confirmada == FALSE | is.na(tiene_quimica_confirmada)
) %>%
rename(
hoja_sensorial = hoja_fuente
) %>%
mutate(
tipo_dato = "sensorial_sin_quimica_agregado",
nivel_quimico = "sin_quimica",
nivel_sensorial = "promedio_muestra",
estado_modelamiento = "solo_sensorial",
cruce_quimica_sensorial_ok = FALSE,
uso_recomendado = uso_sensorial_sin_quimica(bloque_sensorial),
observacion_metodologica = "Promedio de cata real sin química asociada confirmada."
)

if (length(target_cols_agregado) > 0) {
sensorial_sin_quimica_agregado$n_targets_disponibles <- rowSums(
!is.na(sensorial_sin_quimica_agregado[, target_cols_agregado, drop = FALSE])
)
} else {
sensorial_sin_quimica_agregado$n_targets_disponibles <- 0
}

sensorial_sin_quimica_agregado <- sensorial_sin_quimica_agregado %>%
arrange(
bloque_sensorial,
muestra_cata
)

# ------------------------------------------------------------
# Resumen por bloque

resumen_bloques_quimica <- matriz_quimica_sin_cata %>%
group_by(grupo_matriz, bloque_id, tecnica_quimica) %>%
summarise(
n_unidades_analiticas = n(),
n_muestras_base = n_distinct(muestra_base),
.groups = "drop"
) %>%
mutate(tipo_resumen = "quimica_sin_cata")

resumen_bloques_sensorial <- matriz_sensorial_sin_quimica %>%
group_by(bloque_sensorial, archivo_sensorial) %>%
summarise(
n_muestras_sensoriales = n_distinct(muestra_cata),
n_evaluaciones_individuales = n(),
n_evaluadores = n_distinct(evaluador_id),
.groups = "drop"
) %>%
mutate(tipo_resumen = "sensorial_sin_quimica")

# ------------------------------------------------------------

# 8. Revisión de conteos

# ------------------------------------------------------------

revision_conteos <- data.frame(
matriz = c(
"quimica_sin_cata",
"sensorial_sin_quimica",
"sensorial_sin_quimica_agregado"
),
unidad_fila = c(
"unidad_analitica_quimica",
"evaluacion_sensorial_individual",
"muestra_sensorial_promediada"
),
conteo_observado = c(
nrow(matriz_quimica_sin_cata),
nrow(matriz_sensorial_sin_quimica),
nrow(sensorial_sin_quimica_agregado)
),
conteo_esperado = c(
63,
37,
4
),
coincide = c(
nrow(matriz_quimica_sin_cata) == 63,
nrow(matriz_sensorial_sin_quimica) == 37,
nrow(sensorial_sin_quimica_agregado) == 4
),
stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Resumen global

resumen_global <- data.frame(
indicador = c(
"Matriz química sin cata - filas",
"Matriz sensorial sin química - filas individuales",
"Sensorial sin química agregado - filas",
"Uso de matriz química sin cata",
"Uso de matriz sensorial sin química",
"Advertencia metodológica"
),
valor = c(
nrow(matriz_quimica_sin_cata),
nrow(matriz_sensorial_sin_quimica),
nrow(sensorial_sin_quimica_agregado),
"Análisis químico no supervisado; no usar como matriz supervisada sin target.",
"Análisis sensorial descriptivo; no usar como matriz química-sensorial sin química.",
"Estas matrices contienen datos reales, pero no tienen cruce química-sensorial completo."
),
stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Diccionario

diccionario <- data.frame(
hoja = c(
"01_quimica_sin_cata",
"02_sensorial_sin_quimica",
"03_sensorial_agregado",
"04_resumen_quimica",
"05_resumen_sensorial",
"06_revision_conteos"
),
descripcion = c(
"Unidades analíticas químicas reales sin target sensorial confirmado.",
"Evaluaciones sensoriales individuales reales sin química asociada confirmada.",
"Promedios sensoriales por muestra sin química asociada confirmada.",
"Resumen por bloque de la química sin cata.",
"Resumen por bloque de las catas sin química.",
"Control de conteos esperados versus observados."
),
stringsAsFactors = FALSE
)

# ------------------------------------------------------------
#Exportar

ruta_salida <- guardar_excel_seguro(
hojas = list(
"00_resumen" = resumen_global,
"01_quimica_sin_cata" = matriz_quimica_sin_cata,
"02_sensorial_sin_quimica" = matriz_sensorial_sin_quimica,
"03_sensorial_agregado" = sensorial_sin_quimica_agregado,
"04_resumen_quimica" = resumen_bloques_quimica,
"05_resumen_sensorial" = resumen_bloques_sensorial,
"06_revision_conteos" = revision_conteos,
"07_diccionario" = diccionario
),
ruta_salida = ruta_salida_base
)

cat("\nMATRICES SIN CRUCE GENERADAS\n")

cat("\nArchivo generado:\n")
cat(ruta_salida, "\n")

cat("\nResumen:\n")
print(resumen_global)

cat("\nRevisión de conteos:\n")
print(revision_conteos)

cat("\nProceso terminado correctamente.\n")
