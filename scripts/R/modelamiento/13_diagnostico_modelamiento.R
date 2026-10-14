# 13_diagnostico_modelamiento.R
# Diagnostico pre-modelamiento: viabilidad estadistica por escenario y alerta
# temprana de outliers del EDA quimico.
# Proyecto: analisis quimico-sensorial de whisky chileno
#
# Hasta 2026-07-08 este script era una copia exacta de
# 13b_colinealidad_modelamiento.R (mismo contenido, mismo archivo de salida).
# Se separo con contenido propio, complementario a 13b:
#   1. Viabilidad estadistica por escenario/target: filas modelables,
#      predictores usables, "grados de libertad" aproximados (n - p) y
#      alerta de severidad para el contexto de small data. 13b diagnostica
#      colinealidad entre predictores; este script diagnostica si hay
#      margen suficiente de datos para ajustar algo en primer lugar.
#   2. Alerta temprana de outliers: cruza, ANTES de modelar, que unidades
#      analiticas de cada escenario ya fueron marcadas como atipicas en el
#      EDA quimico (10_analisis_quimico.R: T2 de Hotelling, Q-residual,
#      test Q de Dixon). El cruce completo contra los residuos de los
#      modelos ya ajustados se hace despues, en
#      17_residuos_diagnostico.R (hojas 08_cruce_outliers_eda y
#      09_resumen_cruce_outliers).

# -------------------------------------------------------------------------
# 0. Configuracion general
# -------------------------------------------------------------------------

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

paquetes <- c("readxl", "dplyr", "tidyr", "stringr", "writexl", "ggplot2")
paquetes_faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(paquetes_faltantes) > 0) {
  stop(
    "Faltan paquetes requeridos: ", paste(paquetes_faltantes, collapse = ", "),
    "\nInstalalos antes de ejecutar este script."
  )
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)
library(ggplot2)

dir_modelamiento <- file.path(dir_data, "modelamiento")
dir_outputs <- file.path(dir_proyecto, "outputs")
dir_outputs_modelamiento <- file.path(dir_outputs, "modelamiento")
dir_outputs_diagnostico <- file.path(dir_outputs_modelamiento, "diagnostico")

dir.create(dir_modelamiento, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_outputs_diagnostico, recursive = TRUE, showWarnings = FALSE)

ruta_datos_modelamiento <- file.path(dir_modelamiento, "datos_modelamiento.xlsx")
ruta_analisis_quimico <- file.path(dir_procesamiento, "analisis_quimico.xlsx")
ruta_salida_excel <- file.path(dir_modelamiento, "diagnostico_modelamiento.xlsx")

if (!file.exists(ruta_datos_modelamiento)) {
  stop("No existe el archivo requerido: ", ruta_datos_modelamiento,
       "\nEjecuta primero scripts/R/modelamiento/12_preparar_datos_modelamiento.R")
}

targets_modelamiento <- c("y_fenolico_comun", "y_frutal_comun", "y_ahumado_comun", "y_medicinal_comun")

# -------------------------------------------------------------------------
# 1. Funciones auxiliares
# -------------------------------------------------------------------------

guardar_excel_seguro <- function(lista_hojas, ruta) {
  intento <- tryCatch({
    writexl::write_xlsx(lista_hojas, ruta)
    TRUE
  }, error = function(e) FALSE)

  if (!intento) {
    ruta_alt <- sub("\\.xlsx$", paste0("_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"), ruta)
    writexl::write_xlsx(lista_hojas, ruta_alt)
    message("No se pudo sobrescribir el archivo. Se guardo una copia en: ", ruta_alt)
  }
}

obtener_predictores_x <- function(df) grep("^x_", names(df), value = TRUE)

predictor_usable <- function(v) {
  v_no_na <- v[!is.na(v)]
  length(v_no_na) >= 3 && length(unique(v_no_na)) >= 2 && stats::sd(v_no_na) > 0
}

# -------------------------------------------------------------------------
# 2. Lectura de escenarios (mismos 7 que 13b, para mantener comparabilidad)
# -------------------------------------------------------------------------

escenarios_def <- data.frame(
  escenario = c(
    "M_pura", "M_expandida_evaluador", "M_cata_individual",
    "M_ia_exploratoria", "M_quimica", "M_sensorial"
  ),
  hoja = c(
    "01_pura", "02_expandida_evaluador", "04_sensibilidad_con_ju",
    "05_ia_exploratoria", "06_quimica_sola", "07_sensorial_sola"
  ),
  apto_modelamiento_supervisado = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

hojas_disponibles <- readxl::excel_sheets(ruta_datos_modelamiento)
escenarios_def <- escenarios_def %>% mutate(disponible = hoja %in% hojas_disponibles)

if (!all(escenarios_def$disponible)) {
  stop("Faltan hojas en datos_modelamiento.xlsx: ",
       paste(escenarios_def$hoja[!escenarios_def$disponible], collapse = ", "))
}

datos_escenarios <- list()
for (i in seq_len(nrow(escenarios_def))) {
  datos_escenarios[[escenarios_def$escenario[i]]] <- readxl::read_excel(ruta_datos_modelamiento, sheet = escenarios_def$hoja[i])
}

# Las hojas de M_pura, M_expandida_evaluador y M_cata_individual comparten
# las mismas 32 columnas x_ candidatas (13 GC-FID + 13 JU + 1 Folin + 5
# GC-MS), pero el conjunto de predictores DECLARADO para esos tres
# escenarios excluye deliberadamente GC-MS (seccion imp:consideraciones):
# M_pura y M_expandida_evaluador usan solo los 13 de GC-FID, y
# M_cata_individual agrega los 13 de JU (26 en total), sin GC-MS. Sin esta
# exclusion, predictor_usable() cuenta tambien los 5 de GC-MS (tienen
# suficiente cobertura real como para pasar el chequeo basico de varianza),
# inflando n_predictores_usables a 18 y 31 respectivamente, en vez de los
# 13 y 26 declarados en el resto de la tesis (Tabla 4.1). M_ia_exploratoria
# y M_quimica no se tocan: su conteo ya coincide con lo declarado, porque
# GC-MS no pasa el chequeo de varianza dentro de esas hojas por su propia
# escasez de datos, no por una exclusion explicita.
escenarios_excluir_gcms <- c("M_pura", "M_expandida_evaluador", "M_cata_individual")
for (esc in escenarios_excluir_gcms) {
  if (!is.null(datos_escenarios[[esc]])) {
    cols_gcms <- grep("^x_gcms_", names(datos_escenarios[[esc]]), value = TRUE)
    datos_escenarios[[esc]] <- datos_escenarios[[esc]] %>% select(-all_of(cols_gcms))
  }
}

# -------------------------------------------------------------------------
# 3. Viabilidad estadistica por escenario x target
# -------------------------------------------------------------------------
# "Grados de libertad aproximados" = filas modelables (con target no vacio) -
# predictores usables. Es una heuristica de EDA/small data, no un calculo
# formal de un modelo especifico (cada modelo de 14-16 hace su propia
# seleccion de predictores dentro de cada fold). Sirve para detectar de
# entrada escenarios/targets donde el margen es minimo o negativo.

viabilidad_escenarios <- bind_rows(lapply(names(datos_escenarios), function(esc) {
  df <- datos_escenarios[[esc]]
  predictores <- obtener_predictores_x(df)
  n_predictores_usables <- if (length(predictores) == 0) {
    0L
  } else {
    sum(vapply(df[, predictores, drop = FALSE], predictor_usable, logical(1)))
  }

  targets_presentes <- intersect(targets_modelamiento, names(df))

  if (length(targets_presentes) == 0) {
    return(data.frame(
      escenario = esc, target = NA_character_, n_filas_totales = nrow(df),
      n_filas_modelables = NA_integer_, n_predictores_usables = n_predictores_usables,
      grados_libertad_aprox = NA_integer_, severidad_small_data = "sin_target",
      stringsAsFactors = FALSE
    ))
  }

  bind_rows(lapply(targets_presentes, function(tg) {
    n_modelable <- sum(!is.na(df[[tg]]))
    gl <- n_modelable - n_predictores_usables
    severidad <- case_when(
      n_modelable < 8 ~ "critico_muy_pocos_datos",
      gl < 5 ~ "critico_gl_insuficiente",
      gl < 15 ~ "alerta_gl_ajustado",
      TRUE ~ "aceptable_para_small_data"
    )

    data.frame(
      escenario = esc, target = tg, n_filas_totales = nrow(df),
      n_filas_modelables = n_modelable, n_predictores_usables = n_predictores_usables,
      grados_libertad_aprox = gl, severidad_small_data = severidad,
      stringsAsFactors = FALSE
    )
  }))
})) %>%
  left_join(escenarios_def %>% select(escenario, apto_modelamiento_supervisado), by = "escenario") %>%
  arrange(escenario, target)

resumen_viabilidad <- viabilidad_escenarios %>%
  filter(apto_modelamiento_supervisado) %>%
  count(severidad_small_data, name = "n_combinaciones_escenario_target") %>%
  arrange(desc(n_combinaciones_escenario_target))

p_viabilidad <- viabilidad_escenarios %>%
  filter(apto_modelamiento_supervisado, !is.na(target)) %>%
  ggplot(aes(x = target, y = grados_libertad_aprox, fill = severidad_small_data)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, color = "grey30") +
  facet_wrap(~escenario, scales = "free_x") +
  coord_flip() +
  labs(
    title = "Grados de libertad aproximados por escenario y target",
    subtitle = "n filas modelables - n predictores quimicos usables",
    x = NULL, y = "Grados de libertad aproximados", fill = "Severidad"
  ) +
  theme_minimal(base_size = 10)

ruta_grafico_viabilidad <- file.path(dir_outputs_diagnostico, "01_grados_libertad_por_escenario.png")
ggsave(ruta_grafico_viabilidad, p_viabilidad, width = 10, height = 7, dpi = 300)

# -------------------------------------------------------------------------
# 4. Alerta temprana: unidades ya marcadas como atipicas en el EDA quimico
# -------------------------------------------------------------------------
# Solo tiene sentido para escenarios anclados en unidad_analitica_id real
# (M_pura, M_expandida_evaluador, M_cata_individual); M_ia_exploratoria es
# sintetico y F/G no pasan por modelamiento supervisado.

alerta_outliers_eda <- data.frame()

if (file.exists(ruta_analisis_quimico)) {
  hojas_quimico <- readxl::excel_sheets(ruta_analisis_quimico)

  t2q_eda <- if ("13_t2q_outliers" %in% hojas_quimico) {
    readxl::read_excel(ruta_analisis_quimico, sheet = "13_t2q_outliers") %>%
      distinct(unidad_analitica_id, excede_t2, excede_q)
  } else {
    data.frame()
  }

  dixon_eda <- if ("07_dixon_outliers" %in% hojas_quimico) {
    readxl::read_excel(ruta_analisis_quimico, sheet = "07_dixon_outliers") %>%
      filter(es_outlier_dixon %in% TRUE, !is.na(unidad_sospechosa)) %>%
      count(unidad_sospechosa, name = "n_variables_dixon") %>%
      rename(unidad_analitica_id = unidad_sospechosa)
  } else {
    data.frame()
  }

  escenarios_con_quimica_real <- escenarios_def$escenario[
    escenarios_def$escenario %in% c("M_pura", "M_expandida_evaluador", "M_cata_individual")
  ]

  alerta_outliers_eda <- bind_rows(lapply(escenarios_con_quimica_real, function(esc) {
    df <- datos_escenarios[[esc]]
    if (!("unidad_analitica_id" %in% names(df))) return(data.frame())

    unidades_esc <- df %>% distinct(unidad_analitica_id)

    unidades_esc %>%
      left_join(if (nrow(t2q_eda) > 0) t2q_eda else data.frame(unidad_analitica_id = character()), by = "unidad_analitica_id") %>%
      left_join(if (nrow(dixon_eda) > 0) dixon_eda else data.frame(unidad_analitica_id = character()), by = "unidad_analitica_id") %>%
      mutate(
        escenario = esc,
        excede_t2 = excede_t2 %in% TRUE,
        excede_q = excede_q %in% TRUE,
        n_variables_dixon = ifelse(is.na(n_variables_dixon), 0L, n_variables_dixon),
        marcada_en_eda = excede_t2 | excede_q | n_variables_dixon > 0
      ) %>%
      select(escenario, unidad_analitica_id, excede_t2, excede_q, n_variables_dixon, marcada_en_eda)
  })) %>%
    filter(marcada_en_eda) %>%
    arrange(escenario, desc(n_variables_dixon), desc(excede_t2))
} else {
  message("No se encontro ", ruta_analisis_quimico, "; se omite la alerta temprana de outliers (ejecuta scripts/R/exploratorio/10_analisis_quimico.R primero).")
}

resumen_alerta_outliers <- if (nrow(alerta_outliers_eda) > 0) {
  alerta_outliers_eda %>%
    count(escenario, name = "n_unidades_marcadas") %>%
    left_join(
      bind_rows(lapply(names(datos_escenarios), function(esc) {
        data.frame(escenario = esc, n_unidades_totales = dplyr::n_distinct(datos_escenarios[[esc]]$unidad_analitica_id), stringsAsFactors = FALSE)
      })),
      by = "escenario"
    ) %>%
    mutate(pct_unidades_marcadas = round(100 * n_unidades_marcadas / n_unidades_totales, 1))
} else {
  data.frame()
}

# -------------------------------------------------------------------------
# 5. Diccionario y exportacion
# -------------------------------------------------------------------------

diccionario <- data.frame(
  hoja = c(
    "00_resumen", "01_viabilidad_escenarios", "02_resumen_viabilidad",
    "03_alerta_outliers_eda", "04_resumen_alerta_outliers", "05_diccionario"
  ),
  descripcion = c(
    "Conteos generales del diagnostico pre-modelamiento.",
    "Por escenario x target: filas modelables, predictores usables, grados de libertad aproximados y severidad para small data.",
    "Conteo de combinaciones escenario-target por nivel de severidad (solo escenarios aptos para modelamiento supervisado).",
    "Unidades analiticas de M_pura/B_expandida/M_cata_individual ya marcadas como atipicas en el EDA quimico (T2 Hotelling, Q-residual o Dixon) antes de ajustar cualquier modelo.",
    "Resumen por escenario: cuantas unidades y que porcentaje del total ya estaban marcadas en el EDA.",
    "Definicion de hojas y criterios aplicados."
  ),
  criterio = c(
    "",
    "grados_libertad_aprox = n_filas_modelables - n_predictores_usables. Severidad: critico si n<8 o gl<5; alerta si gl<15; aceptable en otro caso (umbrales heuristicos para small data, no un test formal).",
    "",
    "Requiere que exista data/procesamiento/analisis_quimico.xlsx (script 10). El cruce completo contra los residuos post-modelamiento esta en 17_residuos_diagnostico.R.",
    "",
    "Archivo generado por scripts/R/modelamiento/13_diagnostico_modelamiento.R."
  ),
  stringsAsFactors = FALSE
)

resumen_global <- data.frame(
  indicador = c(
    "escenarios_evaluados", "combinaciones_escenario_target_criticas",
    "combinaciones_escenario_target_alerta", "unidades_marcadas_en_eda_total",
    "grafico_generado", "archivo_salida"
  ),
  valor = c(
    as.character(nrow(escenarios_def)),
    as.character(sum(viabilidad_escenarios$severidad_small_data %in% c("critico_muy_pocos_datos", "critico_gl_insuficiente"))),
    as.character(sum(viabilidad_escenarios$severidad_small_data == "alerta_gl_ajustado")),
    as.character(nrow(alerta_outliers_eda)),
    ruta_grafico_viabilidad,
    ruta_salida_excel
  ),
  stringsAsFactors = FALSE
)

lista_salida <- list(
  "00_resumen" = resumen_global,
  "01_viabilidad_escenarios" = viabilidad_escenarios,
  "02_resumen_viabilidad" = resumen_viabilidad,
  "03_alerta_outliers_eda" = alerta_outliers_eda,
  "04_resumen_alerta_outliers" = resumen_alerta_outliers,
  "05_diccionario" = diccionario
)

guardar_excel_seguro(lista_salida, ruta_salida_excel)

cat("\nProceso terminado correctamente.\n")
cat("Archivo generado:\n", ruta_salida_excel, "\n")
cat("Grafico generado:\n", ruta_grafico_viabilidad, "\n")
cat("\nViabilidad por escenario/target:\n")
print(viabilidad_escenarios)
cat("\nUnidades marcadas en el EDA quimico, por escenario:\n")
print(resumen_alerta_outliers)
