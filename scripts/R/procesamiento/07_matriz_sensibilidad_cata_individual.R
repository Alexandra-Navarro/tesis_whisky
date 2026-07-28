
source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

cat("\nMATRIZ DE SENSIBILIDAD: CATA INDIVIDUAL JU\n")

dir.create(dir_procesamiento, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Rutas

ruta_matriz_pura <- file.path(
dir_procesamiento,
"matriz_pura.xlsx"
)

ruta_salida_base <- file.path(
dir_procesamiento,
"matriz_sensibilidad_cata_individual.xlsx"
)

if (!file.exists(ruta_matriz_pura)) {
stop(paste("No existe el archivo:", ruta_matriz_pura))
}

# ------------------------------------------------------------
# Función 
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

# ------------------------------------------------------------

# Leer matriz pura (ya sin JU) y las filas de cata individual JU
# (separadas por 04_matriz_pura.R)

matriz_pura <- readxl::read_excel(
ruta_matriz_pura,
sheet = "01_matriz_pura"
) %>%
as.data.frame(stringsAsFactors = FALSE)

filas_cata_individual_ju <- readxl::read_excel(
ruta_matriz_pura,
sheet = "06_filas_cata_individual_ju"
) %>%
as.data.frame(stringsAsFactors = FALSE)

# ------------------------------------------------------------

# Construir matrices de sensibilidad

matriz_sin_cata_individual <- matriz_pura %>%
mutate(
escenario_sensibilidad = "sin_cata_individual_ju",
observacion_sensibilidad = "Matriz pura, sin cata individual JU (JU nunca esta incluida en la matriz pura oficial)."
)

aporte_cata_individual <- filas_cata_individual_ju %>%
mutate(
escenario_sensibilidad = "aporte_cata_individual_ju",
observacion_sensibilidad = "Filas aportadas por la cata individual real del bloque JU, ausentes de la matriz pura oficial."
)

matriz_con_cata_individual <- bind_rows(matriz_pura, filas_cata_individual_ju) %>%
mutate(
escenario_sensibilidad = "con_cata_individual_ju",
observacion_sensibilidad = "Matriz pura mas la cata individual JU agregada (matriz 5: 'sensorial cata individual')."
)

# ------------------------------------------------------------
# Matriz resumida para modelamiento

targets_principales <- intersect(
c("y_frutal_comun", "y_fenolico_comun"),
names(matriz_pura)
)

predictores_quimicos <- names(matriz_pura)[
stringr::str_detect(names(matriz_pura), "^(x_|ctrl_|n_)")
]

meta_modelamiento <- c(
"escenario_sensibilidad",
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
"n_evaluadores",
"cruce_quimica_sensorial_ok"
)

modelamiento_sin_cata_individual <- matriz_sin_cata_individual %>%
select(
all_of(intersect(meta_modelamiento, names(.))),
all_of(targets_principales),
all_of(predictores_quimicos)
) %>%
mutate(
modelable_fenolico = ifelse(
"y_fenolico_comun" %in% names(.),
!is.na(y_fenolico_comun),
FALSE
),
modelable_frutal = ifelse(
"y_frutal_comun" %in% names(.),
!is.na(y_frutal_comun),
FALSE
)
)

modelamiento_con_cata_individual <- matriz_con_cata_individual %>%
select(
all_of(intersect(meta_modelamiento, names(.))),
all_of(targets_principales),
all_of(predictores_quimicos)
) %>%
mutate(
modelable_fenolico = ifelse(
"y_fenolico_comun" %in% names(.),
!is.na(y_fenolico_comun),
FALSE
),
modelable_frutal = ifelse(
"y_frutal_comun" %in% names(.),
!is.na(y_frutal_comun),
FALSE
)
)

matriz_modelamiento_sensibilidad <- bind_rows(
modelamiento_sin_cata_individual,
modelamiento_con_cata_individual
)

# ------------------------------------------------------------
#  Revisión por escenario

revision_escenarios <- bind_rows(
matriz_sin_cata_individual %>%
mutate(escenario = "sin_cata_individual_ju"),
aporte_cata_individual %>%
mutate(escenario = "aporte_cata_individual_ju"),
matriz_con_cata_individual %>%
mutate(escenario = "con_cata_individual_ju")
) %>%
group_by(escenario) %>%
summarise(
n_filas = n(),
n_bloques_quimicos = n_distinct(bloque_id),
n_muestras_base = n_distinct(muestra_base),
n_con_y_fenolico = ifelse(
"y_fenolico_comun" %in% names(.),
sum(!is.na(y_fenolico_comun)),
NA_integer_
),
n_con_y_frutal = ifelse(
"y_frutal_comun" %in% names(.),
sum(!is.na(y_frutal_comun)),
NA_integer_
),
.groups = "drop"
) %>%
arrange(escenario)

revision_bloques <- matriz_con_cata_individual %>%
group_by(
escenario_sensibilidad,
bloque_id,
bloque_sensorial
) %>%
summarise(
n_filas = n(),
n_muestras_base = n_distinct(muestra_base),
n_evaluadores_promedio = round(mean(n_evaluadores, na.rm = TRUE), 2),
.groups = "drop"
) %>%
arrange(
bloque_id,
bloque_sensorial
)

# ------------------------------------------------------------
#Resumen global

resumen_global <- data.frame(
indicador = c(
"Tipo de matriz",
"Objetivo",
"Filas matriz pura (sin JU, oficial)",
"Filas sin cata individual JU (= matriz pura)",
"Filas aportadas por cata individual JU",
"Filas con cata individual JU (matriz 5: pura + JU)",
"Target principal más completo",
"Advertencia metodológica"
),
valor = c(
"matriz_sensibilidad_cata_individual",
"Evaluar el efecto de incorporar la cata individual real del bloque JU sobre la matriz pura oficial.",
nrow(matriz_pura),
nrow(matriz_sin_cata_individual),
nrow(aporte_cata_individual),
nrow(matriz_con_cata_individual),
"y_fenolico_comun",
"La cata JU es real, pero corresponde a un catador individual; no debe confundirse con proxy experto ni con dato sintético. Por eso la matriz pura oficial (04_matriz_pura.R) la excluye, y esta matriz de sensibilidad permite evaluar el efecto de agregarla."
),
stringsAsFactors = FALSE
)

# ------------------------------------------------------------
#  Diccionario

diccionario <- data.frame(
hoja = c(
"00_resumen",
"01_sin_cata_individual",
"02_aporte_cata_individual",
"03_con_cata_individual",
"04_modelamiento",
"05_revision_escenarios",
"06_revision_bloques"
),
descripcion = c(
"Resumen general de la matriz de sensibilidad.",
"La matriz pura oficial (ya sin JU por definicion desde 04_matriz_pura.R); se incluye aqui solo para comparar lado a lado.",
"Filas aportadas por la cata individual real JU (ausentes de la matriz pura oficial).",
"Matriz 5 de las 6 de presentacion: 'Matriz sensorial cata individual' = matriz pura + la cata individual JU agregada.",
"Versión reducida para comparar modelamiento con y sin cata individual.",
"Conteos comparativos entre escenarios.",
"Distribución por bloque químico y sensorial."
),
stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Exportar

ruta_salida <- guardar_excel_seguro(
hojas = list(
"00_resumen" = resumen_global,
"01_sin_cata_individual" = matriz_sin_cata_individual,
"02_aporte_cata_individual" = aporte_cata_individual,
"03_con_cata_individual" = matriz_con_cata_individual,
"04_modelamiento" = matriz_modelamiento_sensibilidad,
"05_revision_escenarios" = revision_escenarios,
"06_revision_bloques" = revision_bloques,
"07_diccionario" = diccionario
),
ruta_salida = ruta_salida_base
)

cat("\nMATRIZ DE SENSIBILIDAD GENERADA\n")

cat("\nArchivo generado:\n")
cat(ruta_salida, "\n")

cat("\nResumen:\n")
print(resumen_global)

cat("\nRevisión de escenarios:\n")
print(revision_escenarios)

cat("\nProceso terminado correctamente.\n")
