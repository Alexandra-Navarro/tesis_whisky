
# Se realiza un resumen de los archivos del proyecto
# Instalacion de paquetes y librerias

paquetes_base <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "janitor",
  "writexl"
)

for (pkg in paquetes_base) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(writexl)

# Ruta del proyecto
dir_proyecto <- "C:/Users/alena/OneDrive - usach.cl/Escritorio/tesis_repo/Modelamiento" # nolint

#Rutas separadas para cada procedimiento
dir_data <- file.path(dir_proyecto, "data")
dir_original <- file.path(dir_data, "originales")
dir_procesamiento <- file.path(dir_data, "procesamiento")
dir_archivos <- file.path(dir_data, "archivos")
dir_scripts <- file.path(dir_proyecto, "scripts")
dir_scripts_r <- file.path(dir_scripts, "R")


# Normailizamos todos los textos

normalizar_texto <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)
  x <- tolower(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  return(x) # nolint
}

normalizar_nombre_archivo <- function(x) {
  x <- normalizar_texto(x)
  x <- gsub("\\.xlsx$|\\.xls$|\\.csv$|\\.zip$", "", x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  return(x) # nolint
}

limpiar_nombre_variable <- function(x) {
  x <- normalizar_texto(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  return(x) # nolint
}

convertir_numero <- function(x) {
  x <- as.character(x)
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}

convertir_ppm <- function(x) {
  x_txt <- normalizar_texto(x)
  x_txt[x_txt %in% c("nd", "n.d.", "no detectado", "no detectable")] <- "0"
  x_txt <- gsub(",", ".", x_txt)
  suppressWarnings(as.numeric(x_txt))
}

estandarizar_logico <- function(x) { 
  if (is.logical(x)) {
    return(x)
  }
  x_txt <- normalizar_texto(x) # nolint
  dplyr::case_when(
    x_txt %in% c("true", "t", "si", "sí", "1", "yes") ~ TRUE,
    x_txt %in% c("false", "f", "no", "0", "not") ~ FALSE,
    TRUE ~ NA
  )
}

# Clasificacion de compuestos GC-MS por familia quimica (por nombre)
# Usada cuando el archivo original no trae columna de familia ("nature"),
# como en los reportes crudos de libreria NIST/Wiley del bloque Constanza.
# Validada al 100% contra los 24 compuestos de gcms_noviembre que si tienen
# "nature" real (ver scripts/R/procesamiento/02b_clasificar_compuestos_gcms.R).

reglas_clasificacion_gcms <- data.frame(
  orden = 1:11,
  familia = c(
    "Siloxano (contaminante de columna)",
    "Ftalato (contaminante/plastificante)",
    "Fenolico (fenoles, cresoles, guaiacoles)",
    "Ester",
    "Aldehido",
    "Acido carboxilico",
    "Alcohol",
    "Cetona",
    "Tiol",
    "Hidrocarburo aromatico",
    "Hidrocarburo (alcano/alqueno)"
  ),
  patron_regex = c(
    "silox",
    "phthalate|ftalato",
    "phenol|cresol|guaiacol|creosol|syringol|catechol",
    "ester|[a-z]+ate(,|$| )",
    "aldehyde|[a-z]+anal(,|$| )|[a-z]+enal(,|$| )",
    "acid",
    "alcohol|[a-z]+anol(,|$| )|[a-z]+enol(,|$| )",
    "ketone|[a-z]+anone(,|$| )|[a-z]+enone(,|$| )",
    "thiol|mercaptan",
    "benzene|toluene|styrene|naphthalene|xylene|phenyl",
    "[a-z]+ane(,|$| )|[a-z]+ene(,|$| )"
  ),
  variable_final = c(
    "ctrl_gcms_siloxano_pct (excluido del area valida)",
    "no clasificado en variable final (contaminante)",
    "x_gcms_fenolicos_pct",
    "x_gcms_esteres_pct / x_gcms_aroma_fermentativo_pct",
    "x_gcms_aldehidos_pct / x_gcms_aroma_fermentativo_pct",
    "no clasificado en variable final actual",
    "no clasificado en variable final actual",
    "no clasificado en variable final actual",
    "no clasificado en variable final actual",
    "no clasificado en variable final actual",
    "no clasificado en variable final actual"
  ),
  stringsAsFactors = FALSE
)

clasificar_compuesto <- function(nombre) {
  n <- as.character(nombre)
  n <- gsub("[   ]", " ", n)
  n <- tolower(trimws(n))

  dplyr::case_when(
    str_detect(n, "silox") ~ "Siloxano (contaminante de columna)",
    str_detect(n, "phthalate|ftalato") ~ "Ftalato (contaminante/plastificante)",
    str_detect(n, "phenol|cresol|guaiacol|creosol|syringol|catechol") ~ "Fenolico (fenoles, cresoles, guaiacoles)",
    str_detect(n, "ester|[a-z]+ate(,|$| )") ~ "Ester",
    str_detect(n, "aldehyde|[a-z]+anal(,|$| )|[a-z]+enal(,|$| )") ~ "Aldehido",
    str_detect(n, "acid") ~ "Acido carboxilico",
    str_detect(n, "alcohol|[a-z]+anol(,|$| )|[a-z]+enol(,|$| )") ~ "Alcohol",
    str_detect(n, "ketone|[a-z]+anone(,|$| )|[a-z]+enone(,|$| )") ~ "Cetona",
    str_detect(n, "thiol|mercaptan") ~ "Tiol",
    str_detect(n, "benzene|toluene|styrene|naphthalene|xylene|phenyl") ~ "Hidrocarburo aromatico",
    str_detect(n, "[a-z]+ane(,|$| )|[a-z]+ene(,|$| )") ~ "Hidrocarburo (alcano/alqueno)",
    TRUE ~ "Sin clasificar"
  )
}

familia_gcms_a_variable_final <- function(familia) {
  dplyr::case_when(
    familia == "Fenolico (fenoles, cresoles, guaiacoles)" ~ "x_gcms_fenolicos_pct",
    familia == "Ester" ~ "x_gcms_esteres_pct / x_gcms_aroma_fermentativo_pct",
    familia == "Aldehido" ~ "x_gcms_aldehidos_pct / x_gcms_aroma_fermentativo_pct",
    familia == "Siloxano (contaminante de columna)" ~ "ctrl_gcms_siloxano_pct (excluido del area valida)",
    TRUE ~ "no clasificado en variable final actual"
  )
}

#Separación de los archivos esperados

archivos_actuales <- list(
  # Sensoriales
  cata_septiembre = c(
    "Cata sensorial_Septiembre_2024.xlsx",
    "Cata sensorial Septiembre 2024.xlsx"
  ),
  cata_enero = c(
    "Cata Social_Enero_2025.xlsx",
    "Cata Social_Enero_ 2025.xlsx",
    "Cata Social sensorial Whisky_Enero_ 2025.xlsx"
  ),
  panel_noviembre = c(
    "Panel sensorial_06_Noviembre.xlsx",
    "Panel sensorial 06 Noviembre.xlsx"
  ),
  sensorial_cepas_ju = c(
    "sensorial_cepas_JU.xlsx"
  ),
  cata_alexandra_260603 = c(
    "260603_Resultados cata_Alexandra.xlsx"
  ),
  # Químicos
  gcfid_constanza = c(
    "Resultados GC-FID Constanza Vidal_edited.xlsx"
  ),
  gcms_noviembre = c(
    "Panel sensorial_06_Noviembre_GC_MS.xlsx",
    "Panel sensorial_06_Noviembre_GC-MS.xlsx"
  ),
  datos_tesis_ju = c(
    "260507_Datos_tesis_JU.xlsx"
  ),
  quimica_alexandra_260603 = c(
    "260603_Alexandra.xlsx"
  ),
  gcms_constanza_zip = c(
    "Resultados GC-MS-Constanza Vidal.zip"
  ),
  # Referencia de los nombres y resumen de todo 
  data_muestras = c(
    "Data muestras.xlsx"
  )
)

# Buscar archivos originales
buscar_ruta <- function(candidatos) {
  rutas_posibles <- c()
  for (cand in candidatos) {
    rutas_posibles <- c(
      rutas_posibles,
      file.path(dir_original, cand),
      file.path(dir_original, "quimica", cand),
      file.path(dir_original, "sensorial", cand),
      file.path(dir_original, "referencia", cand)
    )
  }
  rutas_existentes <- rutas_posibles[file.exists(rutas_posibles)]
  if (length(rutas_existentes) > 0) {
    return(normalizePath(rutas_existentes[1], winslash = "/", mustWork = TRUE))
  }
  if (!dir.exists(dir_original)) {
    return(NA_character_)
  }
  archivos_originales <- list.files(
    dir_original,
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(archivos_originales) == 0) {
    return(NA_character_)
  }

  base_normalizada <- normalizar_nombre_archivo(basename(archivos_originales))
  candidatos_norm <- normalizar_nombre_archivo(candidatos)
  pos <- which(base_normalizada %in% candidatos_norm)

  if (length(pos) > 0) {
    return(normalizePath(archivos_originales[pos[1]], winslash = "/", mustWork = TRUE)) # nolint
  }

  return(NA_character_) # nolint
}

rutas_originales <- lapply(
  archivos_actuales,
  buscar_ruta
)

resumen_archivos_esperados <- data.frame(
  clave_archivo = names(archivos_actuales),
  candidatos = sapply(
    archivos_actuales,
    function(x) paste(x, collapse = " | ")
  ),
  ruta_resuelta = unlist(rutas_originales),
  encontrado = !is.na(unlist(rutas_originales)),
  stringsAsFactors = FALSE
)

#Tipos de datos y recomendaciones de uso para cada bloque

tipos_dato <- data.frame(
  t_dato = c(
    "puro_agregado",
    "puro_expandido_evaluador",
    "quimica",
    "sensorial",
    "Individual_experto",
    "sintetico_ia"
  ),
  descripcion = c(
    "Química real y cata real resumida por promedio.",
    "Química real cruzada con evaluaciones individuales reales.",
    "Química real sin target sensorial disponible.",
    "Cata real sin química asociada confirmada.",
    "Química real con target asignado por experto individual.",
    "Química real con target generado por inteligencia artificial."
  ),
  uso_recomendado = c(
    "Análisis principal.",
    "Análisis complementario de variabilidad sensorial.",
    "Análisis químico no supervisado.",
    "Análisis sensorial descriptivo.",
    "Escenario incrementado con error experto.",
    "Escenario contaminado/sintético para sensibilidad."
  ),
  stringsAsFactors = FALSE
)

#Mapeo de muestras clave para cada bloque, con identificadores claros

mapeo_muestras<- data.frame(
  bloque = c(
    "gcfid_septiembre",
    "gcfid_septiembre",
    "gcfid_septiembre",
    "gcfid_enero",
    "gcfid_enero",
    "gcfid_enero",
    "gcfid_enero",
    "gcfid_enero",
    "noviembre",
    "noviembre",
    "noviembre",
    "noviembre",
    "noviembre",
    "alexandra_260603",
    "ju_260507",
    "ju_260507",
    "ju_260507",
    "ju_260507",
    "ju_260507",
    "ju_260507"
  ),
  muestra_base = c(
    "M1", "M2", "M3",
    "M1", "M2", "M3", "M4", "M7",
    "NOV_M1", "NOV_M2", "NOV_M3", "NOV_M4", "NOV_M5",
    "Pilsner_H76R1R2",
    "B1.1", "SC476_1e", "SC481_1e", "5.1", "705.1", "815.3"
  ),
  identificador = c(
    "C70", "B1", "H76",
    "C70", "B1", "H76", "C1 / Dalwhinnie", "C2 / Caol Ila",
    "NOV_M1", "NOV_M2", "NOV_M3", "NOV_M4", "NOV_M5",
    "Pilsner sin ahumar H76 R1 y R2",
    "B1.1", "SC476_1e", "SC481_1e", "5.1", "705.1", "815.3"
  ),
  muestra_cata = c(
    "Muestra 1", "Muestra 2", "Muestra 3",
    "Muestra 1", "Muestra 2", "Muestra 3", "Muestra 4", "Muestra 7",
    "Muestra 1", "Muestra 2", "Muestra 3", "Muestra 4", "Muestra 5",
    "Muestra 1",
    "B1.1", "SC476_1e", "SC481_1e", "5.1", "705.1", "815.3"
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 8. Funciones comunes para matrices posteriores
# ------------------------------------------------------------

asignar_bloque_sensorial <- function(bloque_id, muestra_base) {
  
  dplyr::case_when(
    bloque_id == "alexandra_260603_pilsner_gcfid" ~ "cata_alexandra_260603",
    bloque_id == "ju_260507_quimica_cepas" ~ "sensorial_cepas_ju",
    bloque_id == "gcfid_constanza_septiembre" ~ "cata_septiembre_2024",
    bloque_id == "gcfid_constanza_enero" ~ "cata_social_enero_2025",
    bloque_id == "gcms_constanza_comerciales" & muestra_base %in% c("C1", "C2") ~ "cata_social_enero_2025",
    bloque_id == "gcms_noviembre" ~ "panel_noviembre",
    TRUE ~ NA_character_
  )
}

clasificar_grupo_matriz <- function(bloque_id) {
  
  dplyr::case_when(
    bloque_id %in% c(
      "gcfid_constanza_septiembre",
      "gcfid_constanza_enero",
      "alexandra_260603_pilsner_gcfid"
    ) ~ "historico_gcfid",
    
    bloque_id %in% c(
      "gcms_noviembre",
      "gcms_constanza_comerciales",
      "gcms_constanza_experimentales"
    ) ~ "gcms",
    
    bloque_id == "ju_260507_quimica_cepas" ~ "ju_cepas",
    
    bloque_id == "alexandra_260603_fenoles_totales" ~ "fenoles_totales",
    
    TRUE ~ "otro"
  )
}

crear_observacion_expandida_id <- function(unidad_analitica_id, observacion_sensorial_id) {
  paste0(
    unidad_analitica_id,
    "__",
    limpiar_nombre_variable(observacion_sensorial_id) # nolint
  )
}

cat("\nProceso terminado correctamente.\n")