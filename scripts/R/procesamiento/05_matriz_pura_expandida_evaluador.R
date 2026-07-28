
source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

cat("\nCONSTRUCCIÓN DE MATRIZ EXPANDIDA POR EVALUADOR\n")

dir.create(dir_procesamiento, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------

# 1. Rutas

# ------------------------------------------------------------

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
"matriz_pura_expandida_evaluador.xlsx"
)

if (!file.exists(ruta_quimica)) {
stop(paste("No existe el archivo químico:", ruta_quimica))
}

if (!file.exists(ruta_sensorial)) {
stop(paste("No existe el archivo sensorial:", ruta_sensorial))
}

# ------------------------------------------------------------

# 2. Funciones auxiliares

# ------------------------------------------------------------

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

crear_observacion_expandida_id <- function(unidad_analitica_id, observacion_sensorial_id) {
paste0(
unidad_analitica_id,
"__",
limpiar_nombre_variable(observacion_sensorial_id)
)
}

left_join_expandido <- function(x, y, by) {
if (utils::packageVersion("dplyr") >= "1.1.0") {
return(
left_join(
x,
y,
by = by,
relationship = "many-to-many"
)
)
}

left_join(x, y, by = by)
}

# ------------------------------------------------------------

# 3. Leer datos procesados

# ------------------------------------------------------------

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

# ------------------------------------------------------------

# 4. Preparar química con cata

# ------------------------------------------------------------

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

quimica_con_cata <- quimica_wide %>%
mutate(
tiene_cata_confirmada = estandarizar_logico(tiene_cata_confirmada),
replica_id = as.integer(replica_id),
bloque_sensorial = asignar_bloque_sensorial(bloque_id, muestra_base),
muestra_cata = as.character(muestra_cata_asociada)
) %>%
filter(tiene_cata_confirmada == TRUE) %>%
rename(
archivo_quimico = fuente_archivo,
hoja_quimica = hoja_fuente,
identificador_quimico = identificador,
tiene_cata_confirmada_quimica = tiene_cata_confirmada
)

# ------------------------------------------------------------

# 5. Preparar sensorial individual con química

# ------------------------------------------------------------

target_cols <- names(sensorial_individual)[
stringr::str_detect(names(sensorial_individual), "^y_")
]

sensorial_con_quimica <- sensorial_individual %>%
mutate(
tiene_quimica_confirmada = estandarizar_logico(tiene_quimica_confirmada),
muestra_cata = as.character(muestra_cata)
) %>%
filter(tiene_quimica_confirmada == TRUE) %>%
rename(
hoja_sensorial = hoja_fuente,
tiene_quimica_confirmada_sensorial = tiene_quimica_confirmada
) %>%
select(
observacion_sensorial_id,
archivo_sensorial,
hoja_sensorial,
bloque_sensorial,
fila_respuesta,
muestra_cata,
identificador_sensorial,
tiene_quimica_confirmada_sensorial,
evaluador_id,
nombre_catador,
categoria_catador,
all_of(target_cols)
)

# ------------------------------------------------------------

# 6. Construir matriz expandida

# ------------------------------------------------------------

matriz_expandida <- left_join_expandido(
quimica_con_cata,
sensorial_con_quimica,
by = c("bloque_sensorial", "muestra_cata")
) %>%
mutate(
observacion_expandida_id = crear_observacion_expandida_id(
unidad_analitica_id,
observacion_sensorial_id
),
tipo_dato = "puro_expandido_evaluador",
grupo_matriz = clasificar_grupo_matriz(bloque_id),
nivel_quimico = "unidad_analitica",
nivel_sensorial = "evaluacion_individual",
fuente_target = "cata_real_individual",
cruce_quimica_sensorial_ok = !is.na(observacion_sensorial_id),
observacion_metodologica = "Química real cruzada con evaluación sensorial individual real. Las filas expandidas no son muestras independientes."
)

matriz_expandida$n_targets_disponibles <- rowSums(
!is.na(matriz_expandida[, target_cols, drop = FALSE])
)

# ------------------------------------------------------------

# 7. Ordenar columnas

# ------------------------------------------------------------

meta_matriz <- c(
"tipo_dato",
"grupo_matriz",
"observacion_expandida_id",
"unidad_analitica_id",
"observacion_sensorial_id",
"bloque_id",
"bloque_sensorial",
"muestra_base",
"muestra_cata",
"identificador_quimico",
"identificador_sensorial",
"replica_id",
"evaluador_id",
"nombre_catador",
"categoria_catador",
"fila_respuesta",
"tecnica_quimica",
"archivo_quimico",
"hoja_quimica",
"archivo_sensorial",
"hoja_sensorial",
"archivo_sensorial_asociado",
"muestra_cata_asociada",
"tiene_cata_confirmada_quimica",
"tiene_quimica_confirmada_sensorial",
"nivel_quimico",
"nivel_sensorial",
"fuente_target",
"cruce_quimica_sensorial_ok",
"n_targets_disponibles",
"observacion_metodologica"
)

predictores_quimicos <- intersect(vars_quimicas, names(matriz_expandida))

matriz_expandida <- matriz_expandida %>%
select(
all_of(intersect(meta_matriz, names(.))),
all_of(intersect(target_cols, names(.))),
all_of(predictores_quimicos),
everything()
) %>%
arrange(
grupo_matriz,
bloque_id,
muestra_base,
replica_id,
evaluador_id
)

# ------------------------------------------------------------
# Definicion 2026-07-10: igual que en la matriz pura (04_matriz_pura.R),
# se excluye la cata individual JU (un solo catador por muestra, no
# promedio/expansion de panel). Estas filas quedan aparte para
# trazabilidad, pero no forman parte de la matriz expandida oficial.

es_cata_individual_ju <- matriz_expandida$bloque_id == "ju_260507_quimica_cepas" |
matriz_expandida$bloque_sensorial == "sensorial_cepas_ju"

filas_cata_individual_ju <- matriz_expandida %>% filter(es_cata_individual_ju)
matriz_expandida <- matriz_expandida %>% filter(!es_cata_individual_ju)

# ------------------------------------------------------------

# 8. Matriz para modelamiento

# ------------------------------------------------------------

targets_principales <- intersect(
c("y_frutal_comun", "y_fenolico_comun"),
names(matriz_expandida)
)

matriz_modelamiento <- matriz_expandida %>%
select(
all_of(intersect(meta_matriz, names(.))),
all_of(targets_principales),
all_of(predictores_quimicos)
)

if ("y_frutal_comun" %in% names(matriz_modelamiento)) {
matriz_modelamiento$modelable_frutal <- !is.na(matriz_modelamiento$y_frutal_comun)
} else {
matriz_modelamiento$modelable_frutal <- FALSE
}

if ("y_fenolico_comun" %in% names(matriz_modelamiento)) {
matriz_modelamiento$modelable_fenolico <- !is.na(matriz_modelamiento$y_fenolico_comun)
} else {
matriz_modelamiento$modelable_fenolico <- FALSE
}

# ------------------------------------------------------------

# 9. Revisión de cruces por bloque

# ------------------------------------------------------------

evaluaciones_por_muestra <- sensorial_con_quimica %>%
group_by(bloque_sensorial, muestra_cata) %>%
summarise(
evaluaciones_por_muestra = n(),
evaluadores_por_muestra = n_distinct(evaluador_id),
.groups = "drop"
)

esperado_por_bloque <- quimica_con_cata %>%
left_join(
evaluaciones_por_muestra,
by = c("bloque_sensorial", "muestra_cata")
) %>%
group_by(bloque_id, bloque_sensorial) %>%
summarise(
unidades_quimicas = n_distinct(unidad_analitica_id),
observaciones_esperadas = sum(evaluaciones_por_muestra, na.rm = TRUE),
.groups = "drop"
)

observado_por_bloque <- matriz_expandida %>%
group_by(bloque_id, bloque_sensorial) %>%
summarise(
observaciones_observadas = sum(cruce_quimica_sensorial_ok, na.rm = TRUE),
observaciones_sin_cruce = sum(!cruce_quimica_sensorial_ok, na.rm = TRUE),
muestras_cata = n_distinct(muestra_cata),
evaluadores = n_distinct(evaluador_id[cruce_quimica_sensorial_ok == TRUE]),
.groups = "drop"
)

revision_bloques <- esperado_por_bloque %>%
left_join(
observado_por_bloque,
by = c("bloque_id", "bloque_sensorial")
) %>%
mutate(
observaciones_observadas = ifelse(is.na(observaciones_observadas), 0, observaciones_observadas),
observaciones_sin_cruce = ifelse(is.na(observaciones_sin_cruce), 0, observaciones_sin_cruce),
coincide_cruce = observaciones_esperadas == observaciones_observadas
) %>%
arrange(bloque_id)

# ------------------------------------------------------------

# 10. Resumen y diccionario

# ------------------------------------------------------------

resumen_global <- data.frame(
indicador = c(
"Tipo de matriz",
"Unidad de fila",
"Tipo de target",
"Unidades químicas con cata",
"Filas en matriz expandida",
"Filas excluidas (cata individual JU)",
"Filas con cruce correcto",
"Filas sin cruce sensorial",
"Targets sensoriales incorporados",
"Predictores químicos incorporados",
"Filas modelables para y_frutal_comun",
"Filas modelables para y_fenolico_comun",
"Advertencia metodológica"
),
valor = c(
"puro_expandido_evaluador",
"Unidad analítica química × evaluador",
"Evaluación sensorial individual real",
nrow(quimica_con_cata),
nrow(matriz_expandida),
nrow(filas_cata_individual_ju),
sum(matriz_expandida$cruce_quimica_sensorial_ok, na.rm = TRUE),
sum(!matriz_expandida$cruce_quimica_sensorial_ok, na.rm = TRUE),
length(target_cols),
length(predictores_quimicos),
sum(matriz_modelamiento$modelable_frutal, na.rm = TRUE),
sum(matriz_modelamiento$modelable_fenolico, na.rm = TRUE),
"La matriz expandida contiene evaluaciones reales por catador, pero no debe interpretarse como muestras independientes. Excluye la cata individual JU (igual criterio que la matriz pura); esas filas quedan en la hoja filas_cata_individual_ju."
),
stringsAsFactors = FALSE
)

diccionario <- data.frame(
columna = c(
"tipo_dato",
"observacion_expandida_id",
"unidad_analitica_id",
"observacion_sensorial_id",
"muestra_base",
"muestra_cata",
"replica_id",
"evaluador_id",
"y_frutal_comun",
"y_fenolico_comun",
"cruce_quimica_sensorial_ok"
),
descripcion = c(
"Clasificación metodológica del dato; aquí corresponde a puro_expandido_evaluador.",
"Identificador único de la fila expandida.",
"Identificador de la unidad analítica química.",
"Identificador de la evaluación sensorial individual.",
"Muestra o condición base química.",
"Muestra sensorial asociada.",
"Número de réplica química.",
"Identificador del evaluador.",
"Target frutal individual o equivalente común.",
"Target fenólico individual o equivalente común.",
"Indica si la unidad química encontró evaluación sensorial individual."
),
stringsAsFactors = FALSE
)

# ------------------------------------------------------------

# 11. Exportar

# ------------------------------------------------------------

ruta_salida <- guardar_excel_seguro(
hojas = list(
"00_resumen" = resumen_global,
"01_matriz_expandida" = matriz_expandida,
"02_modelamiento" = matriz_modelamiento,
"03_revision_bloques" = revision_bloques,
"04_diccionario" = diccionario,
"05_filas_cata_individual_ju" = filas_cata_individual_ju
),
ruta_salida = ruta_salida_base
)

# ------------------------------------------------------------

# 12. Mensaje final

# ------------------------------------------------------------

cat("\nMATRIZ EXPANDIDA POR EVALUADOR GENERADA\n")

cat("\nArchivo generado:\n")
cat(ruta_salida, "\n")

cat("\nResumen:\n")
print(resumen_global)

cat("\nRevisión por bloque:\n")
print(revision_bloques)

cat("\nProceso terminado correctamente.\n")
