# ============================================================
# 12_preparar_datos_modelamiento.R


# Este script toma las matrices ya generadas en data/procesamiento,
# estandariza identificadores, predictores químicos y targets sensoriales,
# y deja un archivo único para la etapa de modelamiento.

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

paquetes_modelamiento <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "writexl"
)

for (pkg in paquetes_modelamiento) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

set.seed(123)

dir_modelamiento <- file.path(dir_data, "modelamiento")
if (!dir.exists(dir_modelamiento)) {
  dir.create(dir_modelamiento, recursive = TRUE)
}

archivo_salida <- file.path(dir_modelamiento, "datos_modelamiento.xlsx")

# ------------------------------------------------------------
# 2. Parámetros metodológicos
# ------------------------------------------------------------

targets_modelamiento <- c(
  "y_fenolico_comun",
  "y_frutal_comun",
  "y_ahumado_comun",
  "y_medicinal_comun"
)

columnas_id_preferidas <- c(
  "escenario_id",
  "escenario_nombre",
  "tipo_escenario",
  "fuente_matriz",
  "es_sintetico_ia",
  "unidad_analitica_id",
  "observacion_expandida_id",
  "observacion_sensorial_id",
  "bloque_id",
  "grupo_matriz",
  "muestra_base",
  "identificador_quimico",
  "tecnica_quimica",
  "bloque_sensorial",
  "muestra_sensorial_id",
  "evaluador_id",
  "catador_id",
  "categoria_catador"
)

columnas_diagnostico_preferidas <- c(
  "n_unidades_analiticas",
  "n_variables_quimicas_disponibles",
  "n_evaluadores",
  "n_modelos_ia",
  "confianza_promedio_ia",
  "estado_consenso_ia",
  "requiere_revision_humana",
  "tipo_target",
  "origen_target"
)

# ------------------------------------------------------------
# Funciones auxiliares

leer_hoja_preferida <- function(ruta, hojas_preferidas = NULL) {
  if (!file.exists(ruta)) {
    stop("No existe el archivo: ", ruta)
  }

  hojas <- readxl::excel_sheets(ruta)

  if (!is.null(hojas_preferidas)) {
    hoja <- intersect(hojas_preferidas, hojas)
    if (length(hoja) > 0) {
      return(readxl::read_xlsx(ruta, sheet = hoja[1]))
    }
  }

  readxl::read_xlsx(ruta, sheet = hojas[1])
}

convertir_vacios_a_na <- function(df) {
  df %>%
    mutate(across(
      where(is.character),
      ~ dplyr::na_if(trimws(.x), "")
    ))
}

convertir_columnas_numericas <- function(df, cols) {
  cols_presentes <- intersect(cols, names(df))
  if (length(cols_presentes) == 0) {
    return(df)
  }

  df %>%
    mutate(across(
      all_of(cols_presentes),
      ~ convertir_numero(.x)
    ))
}

normalizar_targets_ia <- function(df) {
  # Si la matriz IA trae targets con sufijo _ia, se copian al nombre común
  # para poder comparar escenarios con una nomenclatura uniforme.
  for (target in targets_modelamiento) {
    target_ia <- paste0(target, "_ia")
    if (!(target %in% names(df)) && target_ia %in% names(df)) {
      df[[target]] <- df[[target_ia]]
    }
  }
  df
}

preparar_escenario <- function(df, escenario_id, escenario_nombre,
                               tipo_escenario, fuente_matriz,
                               es_sintetico_ia = FALSE) {
  df <- df %>%
    janitor::clean_names() %>%
    convertir_vacios_a_na() %>%
    normalizar_targets_ia()

  columnas_x <- names(df)[stringr::str_detect(names(df), "^x_")]

  df <- df %>%
    convertir_columnas_numericas(c(columnas_x, targets_modelamiento))

  if (!("grupo_matriz" %in% names(df)) && "bloque_id" %in% names(df)) {
    df <- df %>% mutate(grupo_matriz = clasificar_grupo_matriz(bloque_id))
  }

  df <- df %>%
    mutate(
      escenario_id = escenario_id,
      escenario_nombre = escenario_nombre,
      tipo_escenario = tipo_escenario,
      fuente_matriz = fuente_matriz,
      es_sintetico_ia = es_sintetico_ia,
      .before = 1
    )

  columnas_id <- intersect(columnas_id_preferidas, names(df))
  columnas_diag <- intersect(columnas_diagnostico_preferidas, names(df))
  columnas_y <- intersect(targets_modelamiento, names(df))
  columnas_x <- names(df)[stringr::str_detect(names(df), "^x_")]

  df %>%
    select(any_of(c(columnas_id, columnas_diag, columnas_x, columnas_y)))
}

resumir_escenario <- function(df, escenario_id, escenario_nombre) {
  columnas_x <- names(df)[stringr::str_detect(names(df), "^x_")]
  columnas_y <- intersect(targets_modelamiento, names(df))

  data.frame(
    escenario_id = escenario_id,
    escenario_nombre = escenario_nombre,
    n_filas = nrow(df),
    n_unidades_analiticas = if ("unidad_analitica_id" %in% names(df)) n_distinct(df$unidad_analitica_id) else NA_integer_,
    n_muestras_base = if ("muestra_base" %in% names(df)) n_distinct(df$muestra_base) else NA_integer_,
    n_bloques = if ("bloque_id" %in% names(df)) n_distinct(df$bloque_id) else NA_integer_,
    n_predictores_quimicos = length(columnas_x),
    n_targets_presentes = length(columnas_y),
    targets_con_datos = paste(columnas_y[colSums(!is.na(df[columnas_y])) > 0], collapse = "; "),
    stringsAsFactors = FALSE
  )
}

resumir_targets <- function(lista_escenarios) {
  salida <- list()
  k <- 1

  for (nombre in names(lista_escenarios)) {
    df <- lista_escenarios[[nombre]]
    columnas_y <- intersect(targets_modelamiento, names(df))

    for (target in columnas_y) {
      valores <- df[[target]]
      valores_validos <- valores[!is.na(valores)]
      salida[[k]] <- data.frame(
        escenario_id = unique(df$escenario_id)[1],
        escenario_nombre = unique(df$escenario_nombre)[1],
        target = target,
        n_filas = nrow(df),
        n_no_na = length(valores_validos),
        prop_no_na = ifelse(nrow(df) > 0, length(valores_validos) / nrow(df), NA_real_),
        n_valores_distintos = dplyr::n_distinct(valores_validos),
        minimo = ifelse(length(valores_validos) > 0, min(valores_validos), NA_real_),
        maximo = ifelse(length(valores_validos) > 0, max(valores_validos), NA_real_),
        media = ifelse(length(valores_validos) > 0, mean(valores_validos), NA_real_),
        desviacion = ifelse(length(valores_validos) > 1, sd(valores_validos), NA_real_),
        n_bloques_con_dato = if ("bloque_id" %in% names(df)) {
          dplyr::n_distinct(df$bloque_id[!is.na(valores)])
        } else {
          NA_integer_
        },
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
  }

  if (length(salida) == 0) {
    return(data.frame())
  }

  bind_rows(salida) %>%
    mutate(
      recomendacion_inicial = case_when(
        n_no_na >= 15 & n_valores_distintos >= 3 & !is.na(desviacion) & desviacion > 0 ~ "modelable_principal",
        n_no_na >= 8 & n_valores_distintos >= 2 & !is.na(desviacion) & desviacion > 0 ~ "modelable_exploratorio",
        TRUE ~ "no_modelable"
      )
    )
}

resumir_predictores <- function(lista_escenarios) {
  salida <- list()
  k <- 1

  for (nombre in names(lista_escenarios)) {
    df <- lista_escenarios[[nombre]]
    columnas_x <- names(df)[stringr::str_detect(names(df), "^x_")]

    for (x in columnas_x) {
      valores <- df[[x]]
      valores_validos <- valores[!is.na(valores)]
      salida[[k]] <- data.frame(
        escenario_id = unique(df$escenario_id)[1],
        escenario_nombre = unique(df$escenario_nombre)[1],
        predictor = x,
        n_filas = nrow(df),
        n_no_na = length(valores_validos),
        prop_no_na = ifelse(nrow(df) > 0, length(valores_validos) / nrow(df), NA_real_),
        n_valores_distintos = dplyr::n_distinct(valores_validos),
        minimo = ifelse(length(valores_validos) > 0, min(valores_validos), NA_real_),
        maximo = ifelse(length(valores_validos) > 0, max(valores_validos), NA_real_),
        media = ifelse(length(valores_validos) > 0, mean(valores_validos), NA_real_),
        desviacion = ifelse(length(valores_validos) > 1, sd(valores_validos), NA_real_),
        usar_como_predictor = length(valores_validos) >= 5 &
          dplyr::n_distinct(valores_validos) >= 2 &
          !is.na(ifelse(length(valores_validos) > 1, sd(valores_validos), NA_real_)) &
          ifelse(length(valores_validos) > 1, sd(valores_validos), 0) > 0,
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
  }

  if (length(salida) == 0) {
    return(data.frame())
  }

  bind_rows(salida)
}

guardar_excel_seguro <- function(lista_hojas, ruta) {
  if (file.exists(ruta)) {
    ok <- tryCatch({
      file.remove(ruta)
    }, error = function(e) FALSE)
    if (!ok) {
      stop(
        "No se pudo sobrescribir el archivo. Cierra Excel y vuelve a ejecutar: ",
        ruta
      )
    }
  }
  writexl::write_xlsx(lista_hojas, path = ruta)
}

# ------------------------------------------------------------
# 4. Lectura de matrices

ruta_matriz_pura <- file.path(dir_procesamiento, "matriz_pura.xlsx")
ruta_matriz_expandida <- file.path(dir_procesamiento, "matriz_pura_expandida_evaluador.xlsx")
ruta_matriz_sensibilidad <- file.path(dir_procesamiento, "matriz_sensibilidad_cata_individual.xlsx")
ruta_matriz_ia <- file.path(dir_procesamiento, "matriz_datos_ia.xlsx")
ruta_matrices_sin_cruce <- file.path(dir_procesamiento, "matrices_sin_cruce.xlsx")

matriz_pura_raw <- leer_hoja_preferida(
  ruta_matriz_pura,
  c("01_matriz_pura", "matriz_pura", "01_pura")
)

matriz_expandida_raw <- leer_hoja_preferida(
  ruta_matriz_expandida,
  c("01_matriz_expandida", "01_matriz_pura_expandida", "01_expandida", "matriz_expandida")
)

sensibilidad_con_ju_raw <- leer_hoja_preferida(
  ruta_matriz_sensibilidad,
  c("03_con_cata_individual", "con_cata_individual", "03_con_ju")
)

ia_raw <- leer_hoja_preferida(
  ruta_matriz_ia,
  c("03_matriz_ia_modelable", "05_matriz_ia_modelable", "matriz_ia_modelable")
)

# Matrices base sin cruce (solo química / solo sensorial): no participan del
# modelamiento supervisado (falta la mitad de las variables x/y), pero se
# preparan igual como escenarios extra para diagnóstico/colinealidad
# (scripts 13 y 13b), tal como se acordó dejarlas para análisis exploratorio.
quimica_sola_raw <- leer_hoja_preferida(
  ruta_matrices_sin_cruce,
  c("01_quimica_sin_cata", "quimica_sin_cata")
)

sensorial_sola_raw <- leer_hoja_preferida(
  ruta_matrices_sin_cruce,
  c("02_sensorial_sin_quimica", "sensorial_sin_quimica")
)

# ------------------------------------------------------------
# 5. Preparación de escenarios
# ------------------------------------------------------------

escenario_pura <- preparar_escenario(
  matriz_pura_raw,
  escenario_id = "M_pura",
  escenario_nombre = "Matriz pura real",
  tipo_escenario = "principal_real",
  fuente_matriz = "matriz_pura.xlsx",
  es_sintetico_ia = FALSE
)

escenario_expandida <- preparar_escenario(
  matriz_expandida_raw,
  escenario_id = "M_expandida_evaluador",
  escenario_nombre = "Matriz expandida por evaluador",
  tipo_escenario = "variabilidad_evaluador",
  fuente_matriz = "matriz_pura_expandida_evaluador.xlsx",
  es_sintetico_ia = FALSE
)

# Desde 2026-07-10 la matriz pura (M_pura) ya excluye la cata individual
# JU por definicion (ver 04_matriz_pura.R), asi que "sin cata individual"
# es ahora identico a M_pura -se retiro como escenario separado para no
# duplicar computo sobre datos identicos-. M_cata_individual pasa a ser el
# escenario de sensibilidad util: pura + la cata individual JU agregada,
# comparable directamente contra M_pura.
escenario_con_ju <- preparar_escenario(
  sensibilidad_con_ju_raw,
  escenario_id = "M_cata_individual",
  escenario_nombre = "Matriz pura + cata individual JU",
  tipo_escenario = "sensibilidad",
  fuente_matriz = "matriz_sensibilidad_cata_individual.xlsx::03_con_cata_individual",
  es_sintetico_ia = FALSE
)

escenario_ia <- preparar_escenario(
  ia_raw,
  escenario_id = "M_ia_exploratoria",
  escenario_nombre = "Matriz IA exploratoria",
  tipo_escenario = "sintetico_ia_exploratorio",
  fuente_matriz = "matriz_datos_ia.xlsx::03_matriz_ia_modelable",
  es_sintetico_ia = TRUE
)

escenario_quimica_sola <- preparar_escenario(
  quimica_sola_raw,
  escenario_id = "M_quimica",
  escenario_nombre = "Matriz solo química (sin cruce sensorial)",
  tipo_escenario = "exploratorio_diagnostico_sin_targets",
  fuente_matriz = "matrices_sin_cruce.xlsx::01_quimica_sin_cata",
  es_sintetico_ia = FALSE
)

escenario_sensorial_sola <- preparar_escenario(
  sensorial_sola_raw,
  escenario_id = "M_sensorial",
  escenario_nombre = "Matriz solo sensorial (sin cruce químico)",
  tipo_escenario = "exploratorio_diagnostico_sin_predictores",
  fuente_matriz = "matrices_sin_cruce.xlsx::02_sensorial_sin_quimica",
  es_sintetico_ia = FALSE
)

lista_escenarios <- list(
  pura = escenario_pura,
  expandida_evaluador = escenario_expandida,
  sensibilidad_con_ju = escenario_con_ju,
  ia_exploratoria = escenario_ia,
  quimica_sola = escenario_quimica_sola,
  sensorial_sola = escenario_sensorial_sola
)

# ------------------------------------------------------------
# Resúmenes

resumen <- bind_rows(
  resumir_escenario(escenario_pura, "M_pura", "Matriz pura real"),
  resumir_escenario(escenario_expandida, "M_expandida_evaluador", "Matriz expandida por evaluador"),
  resumir_escenario(escenario_con_ju, "M_cata_individual", "Matriz pura + cata individual JU"),
  resumir_escenario(escenario_ia, "M_ia_exploratoria", "Matriz IA exploratoria"),
  resumir_escenario(escenario_quimica_sola, "M_quimica", "Matriz solo química"),
  resumir_escenario(escenario_sensorial_sola, "M_sensorial", "Matriz solo sensorial")
) %>%
  mutate(
    observacion_metodologica = case_when(
      escenario_id == "M_pura" ~ "Escenario principal real. No incluye la cata individual JU (ver M_cata_individual).",
      escenario_id == "M_expandida_evaluador" ~ "Usar solo con validación agrupada por muestra para evitar fuga de información.",
      escenario_id == "M_cata_individual" ~ "Escenario de sensibilidad: matriz pura (M_pura) mas la cata individual JU agregada. Comparar directamente contra M_pura para evaluar el efecto de incluir esa fuente.",
      escenario_id == "M_ia_exploratoria" ~ "Escenario sintético exploratorio; no equivale a cata real.",
      escenario_id == "M_quimica" ~ "Sin targets sensoriales (y_*); no apto para modelamiento supervisado. Solo para diagnóstico/colinealidad de predictores químicos (scripts 13 y 13b).",
      escenario_id == "M_sensorial" ~ "Sin predictores químicos (x_*); no apto para modelamiento supervisado. Solo para diagnóstico exploratorio de los targets sensoriales (scripts 13 y 13b).",
      TRUE ~ ""
    )
  )

targets_disponibles <- resumir_targets(lista_escenarios)
variables_quimicas <- resumir_predictores(lista_escenarios)

diccionario <- data.frame(
  campo = c(
    "escenario_id",
    "escenario_nombre",
    "tipo_escenario",
    "es_sintetico_ia",
    "x_*",
    "y_fenolico_comun",
    "y_frutal_comun",
    "y_ahumado_comun",
    "y_medicinal_comun"
  ),
  descripcion = c(
    "Código corto del escenario de modelamiento.",
    "Nombre descriptivo del escenario.",
    "Tipo metodológico del escenario.",
    "TRUE si el target proviene de consenso IA; FALSE si proviene de cata real.",
    "Predictores químicos estandarizados.",
    "Target común fenólico.",
    "Target común frutal.",
    "Target común ahumado.",
    "Target común medicinal."
  ),
  uso = c(
    "Identificación de escenario.",
    "Reporte.",
    "Comparación metodológica.",
    "Evitar mezclar IA con cata real sin declarar.",
    "Variables X para modelamiento.",
    "Target principal recomendado.",
    "Target secundario/exploratorio.",
    "Target exploratorio.",
    "Target exploratorio."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Exportación

hojas_salida <- list(
  "00_resumen" = resumen,
  "01_pura" = escenario_pura,
  "02_expandida_evaluador" = escenario_expandida,
  "04_sensibilidad_con_ju" = escenario_con_ju,
  "05_ia_exploratoria" = escenario_ia,
  "06_quimica_sola" = escenario_quimica_sola,
  "07_sensorial_sola" = escenario_sensorial_sola,
  "08_targets_disponibles" = targets_disponibles,
  "09_variables_quimicas" = variables_quimicas,
  "10_diccionario" = diccionario
)

guardar_excel_seguro(hojas_salida, archivo_salida)

cat("\nArchivo generado correctamente:\n")
cat(archivo_salida, "\n")
cat("\nResumen de escenarios:\n")
print(resumen)
cat("\nProceso terminado correctamente.\n")
