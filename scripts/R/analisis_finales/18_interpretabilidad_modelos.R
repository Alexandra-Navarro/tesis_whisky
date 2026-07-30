# ============================================================
# 18_interpretabilidad_modelos.R
# Interpretabilidad y consolidacion de variables quimicas relevantes
# Tesis whisky chileno - modelamiento quimico-sensorial
# ============================================================

# Objetivo:
# - Consolidar la interpretabilidad de los modelos small data.
# - Integrar importancia por validacion cruzada, estabilidad bootstrap y coherencia quimica.
# - Generar tablas finales de variables candidatas para la tesis.
#
# Insumos principales:
# - resultados_modelos_small_data.xlsx
# - validacion_bootstrap_cv.xlsx
# - residuos_diagnostico.xlsx
# - colinealidad_modelamiento.xlsx
#
# Salidas:
# - data/modelamiento/interpretabilidad_modelos.xlsx
# - outputs/modelamiento/interpretabilidad/*.png

# ------------------------------------------------------------
# 1. Configuracion
# ------------------------------------------------------------

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

paquetes_requeridos <- c(
  "readxl", "dplyr", "tidyr", "stringr", "writexl", "ggplot2"
)

paquetes_faltantes <- paquetes_requeridos[
  !vapply(paquetes_requeridos, requireNamespace, logical(1), quietly = TRUE)
]

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

set.seed(123)

dir_modelamiento <- file.path(dir_data, "modelamiento")
dir_outputs <- file.path(dir_proyecto, "outputs")
dir_outputs_modelamiento <- file.path(dir_outputs, "modelamiento")
dir_outputs_interpretabilidad <- file.path(dir_outputs_modelamiento, "interpretabilidad")

dir.create(dir_modelamiento, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_outputs_interpretabilidad, recursive = TRUE, showWarnings = FALSE)

ruta_small_data <- file.path(dir_modelamiento, "resultados_modelos_small_data.xlsx")
ruta_bootstrap <- file.path(dir_modelamiento, "validacion_bootstrap_cv.xlsx")
ruta_residuos <- file.path(dir_modelamiento, "residuos_diagnostico.xlsx")
ruta_colinealidad <- file.path(dir_modelamiento, "colinealidad_modelamiento.xlsx")
ruta_salida_excel <- file.path(dir_modelamiento, "interpretabilidad_modelos.xlsx")

archivos_requeridos <- c(ruta_small_data, ruta_bootstrap)
archivos_faltantes <- archivos_requeridos[!file.exists(archivos_requeridos)]

if (length(archivos_faltantes) > 0) {
  stop(
    "Faltan archivos requeridos:\n", paste(archivos_faltantes, collapse = "\n"),
    "\nEjecuta primero los scripts 14 y 15."
  )
}

# Modelos a considerar para la decision principal.
# Gradient boosting queda como apoyo, no como base de interpretacion final.
modelos_prioritarios <- c(
  "random_forest_restringido",
  "pls",
  "lasso",
  "elastic_net",
  "ridge"
)

modelos_apoyo <- c("gradient_boosting_restringido")

target_principal <- "y_fenolico_comun"
escenario_principal <- "M_pura"

# ------------------------------------------------------------
# 2. Funciones auxiliares
# ------------------------------------------------------------

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

leer_hoja_si_existe <- function(ruta, hoja) {
  if (!file.exists(ruta)) return(data.frame())
  hojas <- readxl::excel_sheets(ruta)
  if (!hoja %in% hojas) return(data.frame())
  readxl::read_excel(ruta, sheet = hoja) %>% as.data.frame()
}

limpiar_nombre_predictor <- function(x) {
  x %>%
    as.character() %>%
    str_remove("^x_") %>%
    str_replace_all("_", " ") %>%
    str_squish()
}

clasificar_tecnica_predictor <- function(predictor) {
  p <- str_to_lower(as.character(predictor))
  case_when(
    str_detect(p, "^x_gcfid_|gcfid") ~ "GC-FID",
    str_detect(p, "^x_gcms_|gcms") ~ "GC-MS",
    str_detect(p, "^x_ju_|ju_") ~ "JU",
    str_detect(p, "folin|fenoles_totales") ~ "Folin",
    TRUE ~ "otra"
  )
}

clasificar_familia_quimica <- function(predictor) {
  p <- str_to_lower(as.character(predictor))
  case_when(
    str_detect(p, "cresol|guaiacol|fenol|phenol") ~ "fenoles_volatiles",
    str_detect(p, "ester|ethyl|octanoate|decanoate|dodecanoate|myristate|hexadecanoate|isoamyl|acetate") ~ "esteres",
    str_detect(p, "co2|maltose|ethanol|faci|ferulic") ~ "fermentacion_indirecta",
    str_detect(p, "aldeh|furfural") ~ "aldehidos_furanicos",
    str_detect(p, "area_valida") ~ "calidad_senal_analitica",
    str_detect(p, "folin|fenoles_totales") ~ "fenoles_totales",
    TRUE ~ "otros"
  )
}

clasificar_coherencia_target <- function(target, familia, predictor) {
  p <- str_to_lower(as.character(predictor))

  case_when(
    target == "y_fenolico_comun" & familia %in% c("fenoles_volatiles", "fenoles_totales") ~ "alta_directa",
    target == "y_fenolico_comun" & familia %in% c("fermentacion_indirecta") ~ "media_indirecta",
    target == "y_fenolico_comun" & familia %in% c("esteres") ~ "media_contextual",
    target == "y_fenolico_comun" & familia %in% c("aldehidos_furanicos", "calidad_senal_analitica") ~ "baja_contextual",

    target == "y_frutal_comun" & familia %in% c("esteres") ~ "alta_directa",
    target == "y_frutal_comun" & familia %in% c("fermentacion_indirecta") ~ "media_indirecta",
    target == "y_frutal_comun" & familia %in% c("fenoles_volatiles", "fenoles_totales") ~ "baja_no_directa",

    str_detect(target, "ahum") & familia %in% c("fenoles_volatiles", "fenoles_totales") ~ "alta_directa",
    str_detect(target, "medic") & familia %in% c("fenoles_volatiles") ~ "alta_directa",
    TRUE ~ "no_prioritaria"
  )
}

explicar_coherencia <- function(target, familia, predictor) {
  case_when(
    target == "y_fenolico_comun" & familia == "fenoles_volatiles" ~
      "Coherente: cresoles, guaiacol u otros fenoles volatiles se relacionan directamente con notas fenolicas/ahumadas.",
    target == "y_fenolico_comun" & familia == "fenoles_totales" ~
      "Coherente con cautela: fenoles totales indican potencial fenolico global, aunque no identifican compuestos volatiles especificos.",
    target == "y_fenolico_comun" & familia == "fermentacion_indirecta" ~
      "Interpretacion indirecta: puede capturar diferencias de fermentacion o bloque analitico, no causalidad fenolica directa.",
    target == "y_fenolico_comun" & familia == "esteres" ~
      "Interpretacion contextual: los esteres pueden separar perfiles de muestra, pero no son evidencia fenolica directa.",
    target == "y_frutal_comun" & familia == "esteres" ~
      "Coherente: los esteres suelen asociarse a notas frutales, florales y fermentativas.",
    target == "y_frutal_comun" & familia == "fermentacion_indirecta" ~
      "Interpretacion indirecta: variables fermentativas pueden asociarse a generacion aromatica, pero no miden esteres directamente.",
    TRUE ~ "Debe interpretarse como asociacion estadistica exploratoria y contrastarse con literatura/criterio experto."
  )
}

# NOTA (fix ranking frecuencia/magnitud): antes esta funcion solo exigia
# frecuencia_media >= 0.70 para "prioritaria", sin mirar la magnitud del
# efecto. Eso permitia que un predictor seleccionado a menudo pero con
# aporte casi nulo (ej. un ester de contexto con importancia normalizada
# ~2, frente a otros con importancia ~37) quedara etiquetado como
# "prioritaria" solo por frecuencia. Se agrega el umbral
# importancia_norm_minmax (magnitud normalizada 0-1 dentro de cada
# escenario/target) para que la magnitud tambien condicione la etiqueta.
clasificar_prioridad_variable <- function(frecuencia_media, importancia_norm_minmax, n_modelos, coherencia, target, escenario) {
  case_when(
    escenario == "M_ia_exploratoria" ~ "solo_exploratoria_ia",
    target != "y_fenolico_comun" ~ "exploratoria_target_secundario",
    frecuencia_media >= 0.70 & importancia_norm_minmax >= 0.30 & n_modelos >= 2 & coherencia %in% c("alta_directa", "media_indirecta", "media_contextual") ~ "prioritaria",
    frecuencia_media >= 0.50 & n_modelos >= 2 ~ "apoyo_interpretativo",
    frecuencia_media >= 0.80 & n_modelos == 1 & coherencia %in% c("alta_directa", "media_indirecta") ~ "candidata_especifica_modelo",
    TRUE ~ "baja_prioridad"
  )
}

# ------------------------------------------------------------
# 3. Lectura de insumos
# ------------------------------------------------------------

metricas_small <- leer_hoja_si_existe(ruta_small_data, "00_resumen_metricas")
importancia_cv <- leer_hoja_si_existe(ruta_small_data, "06_importancia_global")
importancia_fold <- leer_hoja_si_existe(ruta_small_data, "05_importancia_fold")

metricas_boot <- leer_hoja_si_existe(ruta_bootstrap, "02_resumen_metricas_boot")
estabilidad_boot <- leer_hoja_si_existe(ruta_bootstrap, "03_estabilidad_variables")
top_boot <- leer_hoja_si_existe(ruta_bootstrap, "04_top_variables_estables")
comparacion_cv_boot <- leer_hoja_si_existe(ruta_bootstrap, "05_comparacion_cv_boot")

resumen_residuos <- leer_hoja_si_existe(ruta_residuos, "02_resumen_residuos")
diagnostico_supuestos <- leer_hoja_si_existe(ruta_residuos, "03_diagnostico_supuestos")
residuos_atipicos <- leer_hoja_si_existe(ruta_residuos, "04_residuos_atipicos")

pares_colineales <- leer_hoja_si_existe(ruta_colinealidad, "02_pares_alta_correlacion")

if (nrow(metricas_small) == 0) {
  stop("No se encontro la hoja 00_resumen_metricas en resultados_modelos_small_data.xlsx")
}
if (nrow(estabilidad_boot) == 0) {
  stop("No se encontro la hoja 03_estabilidad_variables en validacion_bootstrap_cv.xlsx")
}

# ------------------------------------------------------------
# 4. Modelos candidatos por desempeno y estabilidad
# ------------------------------------------------------------

modelos_candidatos <- data.frame()

if (nrow(metricas_boot) > 0) {
  modelos_candidatos <- metricas_boot %>%
    mutate(
      modelo_prioritario = modelo %in% modelos_prioritarios,
      modelo_apoyo = modelo %in% modelos_apoyo,
      rol_interpretativo = case_when(
        modelo == "random_forest_restringido" ~ "principal_predictivo_si_estable",
        modelo == "pls" ~ "quimiometrico_interpretable",
        modelo %in% c("lasso", "elastic_net", "ridge") ~ "regularizado_colinealidad",
        modelo == "gradient_boosting_restringido" ~ "comparacion_no_lineal_apoyo",
        TRUE ~ "otro"
      ),
      candidato_final = case_when(
        target == target_principal & modelo_prioritario & ranking_mae <= 3 ~ TRUE,
        target == target_principal & modelo %in% c("pls", "lasso", "ridge", "elastic_net") ~ TRUE,
        TRUE ~ FALSE
      ),
      lectura_desempeno = case_when(
        modelo == "gradient_boosting_restringido" ~ "usar_con_cautela_por_posible_sobreajuste",
        ranking_mae == 1 ~ "mejor_mae_bootstrap_del_escenario",
        ranking_mae <= 3 ~ "desempeno_competitivo",
        TRUE ~ "desempeno_secundario"
      )
    ) %>%
    arrange(escenario, target, ranking_mae, mae_media)
}

# ------------------------------------------------------------
# 5. Estabilidad y consenso de variables
# ------------------------------------------------------------

estabilidad_enriquecida <- estabilidad_boot %>%
  mutate(
    tecnica = clasificar_tecnica_predictor(predictor),
    familia_quimica = clasificar_familia_quimica(predictor),
    predictor_limpio = limpiar_nombre_predictor(predictor),
    coherencia_target = clasificar_coherencia_target(target, familia_quimica, predictor),
    justificacion_quimica = explicar_coherencia(target, familia_quimica, predictor),
    tipo_modelo = case_when(
      modelo == "random_forest_restringido" ~ "no_lineal_arbol",
      modelo == "gradient_boosting_restringido" ~ "no_lineal_arbol_apoyo",
      modelo == "pls" ~ "quimiometrico",
      modelo %in% c("ridge", "lasso", "elastic_net") ~ "regularizado_lineal",
      TRUE ~ "otro"
    ),
    usar_en_consenso_final = modelo %in% modelos_prioritarios
  )

consenso_variables <- estabilidad_enriquecida %>%
  filter(usar_en_consenso_final) %>%
  group_by(escenario, target, predictor, predictor_limpio, tecnica, familia_quimica, coherencia_target) %>%
  summarise(
    n_modelos_con_variable = n_distinct(modelo),
    modelos_que_la_reportan = paste(sort(unique(modelo)), collapse = "; "),
    frecuencia_media = mean(frecuencia_seleccion, na.rm = TRUE),
    frecuencia_max = max(frecuencia_seleccion, na.rm = TRUE),
    importancia_norm_media = mean(importancia_abs_norm_media, na.rm = TRUE),
    importancia_norm_max = max(importancia_abs_norm_media, na.rm = TRUE),
    n_bootstrap_total_modelos = sum(n_bootstrap_con_variable, na.rm = TRUE),
    justificacion_quimica = dplyr::first(justificacion_quimica),
    .groups = "drop"
  ) %>%
  group_by(escenario, target) %>%
  mutate(
    # Magnitud normalizada 0-1 dentro de cada escenario/target: la escala
    # cruda de importancia_norm_media no es comparable con frecuencia_media
    # (esta ultima ya vive en [0,1]), asi que sin normalizar, la magnitud
    # nunca pesaba realmente frente a la frecuencia al ordenar.
    importancia_norm_minmax = if (n() > 1 && diff(range(importancia_norm_media, na.rm = TRUE)) > 0) {
      (importancia_norm_media - min(importancia_norm_media, na.rm = TRUE)) /
        (max(importancia_norm_media, na.rm = TRUE) - min(importancia_norm_media, na.rm = TRUE))
    } else {
      rep(0.5, dplyr::n())
    },
    # Puntaje combinado: promedio simple de frecuencia y magnitud, ambas ya
    # en escala 0-1. Reporta y combina frecuencia+magnitud en lugar de
    # ordenar solo por frecuencia (ver nota en clasificar_prioridad_variable).
    score_combinado = (frecuencia_media + importancia_norm_minmax) / 2
  ) %>%
  ungroup() %>%
  mutate(
    estabilidad_consenso = case_when(
      frecuencia_media >= 0.70 & n_modelos_con_variable >= 2 ~ "alta",
      frecuencia_media >= 0.50 & n_modelos_con_variable >= 2 ~ "media",
      frecuencia_max >= 0.80 ~ "alta_en_un_modelo",
      frecuencia_media >= 0.30 ~ "baja_media",
      TRUE ~ "baja"
    ),
    prioridad_tesis = clasificar_prioridad_variable(
      frecuencia_media,
      importancia_norm_minmax,
      n_modelos_con_variable,
      coherencia_target,
      target,
      escenario
    )
  ) %>%
  group_by(escenario, target) %>%
  arrange(
    factor(prioridad_tesis, levels = c("prioritaria", "apoyo_interpretativo", "candidata_especifica_modelo", "exploratoria_target_secundario", "solo_exploratoria_ia", "baja_prioridad")),
    desc(score_combinado),
    desc(n_modelos_con_variable),
    .by_group = TRUE
  ) %>%
  mutate(ranking_consenso = row_number()) %>%
  ungroup()

# Variables finales recomendadas para resultados/discusion.
variables_finales_tesis <- consenso_variables %>%
  filter(
    target == target_principal,
    escenario %in% c("M_pura", "M_cata_individual"),
    prioridad_tesis %in% c("prioritaria", "apoyo_interpretativo", "candidata_especifica_modelo")
  ) %>%
  arrange(
    escenario,
    factor(prioridad_tesis, levels = c("prioritaria", "apoyo_interpretativo", "candidata_especifica_modelo")),
    desc(score_combinado)
  )

variables_por_familia <- consenso_variables %>%
  group_by(escenario, target, familia_quimica, tecnica) %>%
  summarise(
    n_variables = n_distinct(predictor),
    frecuencia_media_promedio = mean(frecuencia_media, na.rm = TRUE),
    importancia_norm_promedio = mean(importancia_norm_media, na.rm = TRUE),
    n_prioritarias = sum(prioridad_tesis %in% c("prioritaria", "apoyo_interpretativo"), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(escenario, target, desc(n_prioritarias), desc(frecuencia_media_promedio))

# ------------------------------------------------------------
# 6. Relacion con colinealidad
# ------------------------------------------------------------

colinealidad_variables_finales <- data.frame()

if (nrow(pares_colineales) > 0 && nrow(consenso_variables) > 0) {
  # Estandariza nombres posibles de columnas segun version del script 12b.
  nombres_pares <- names(pares_colineales)

  col_x1 <- intersect(c("predictor_1", "variable_1", "var1", "x1"), nombres_pares)[1]
  col_x2 <- intersect(c("predictor_2", "variable_2", "var2", "x2"), nombres_pares)[1]
  # abs_r es el nombre real que produce 13b_colinealidad_modelamiento.R; se
  # mantienen los demas como alternativas por si cambia la version del script.
  col_r <- intersect(c("abs_r", "r", "correlacion", "correlacion_pearson", "abs_correlacion", "r_pearson"), nombres_pares)[1]

  if (!is.na(col_x1) && !is.na(col_x2)) {
    vars_clave <- consenso_variables %>%
      filter(target == target_principal, prioridad_tesis != "baja_prioridad") %>%
      select(escenario, target, predictor, prioridad_tesis, familia_quimica, coherencia_target)

    colinealidad_variables_finales <- pares_colineales %>%
      rename(
        predictor_1_tmp = all_of(col_x1),
        predictor_2_tmp = all_of(col_x2)
      ) %>%
      mutate(
        predictor_1 = as.character(predictor_1_tmp),
        predictor_2 = as.character(predictor_2_tmp),
        correlacion_valor = if (!is.na(col_r)) as.numeric(.data[[col_r]]) else NA_real_
      ) %>%
      select(-any_of(c("predictor_1_tmp", "predictor_2_tmp"))) %>%
      inner_join(vars_clave, by = c("escenario", "predictor_1" = "predictor")) %>%
      rename(variable_clave = predictor_1, variable_colineal = predictor_2) %>%
      mutate(
        lectura = "Variable priorizada presenta colinealidad alta con otro predictor; interpretar seleccion como familia/conjunto quimico, no efecto aislado."
      )
  }
}

# ------------------------------------------------------------
# 7. Graficos
# ------------------------------------------------------------

graficos_generados <- data.frame(
  grafico = character(),
  ruta_grafico = character(),
  descripcion = character(),
  stringsAsFactors = FALSE
)

# Grafico 1: consenso de variables M_pura fenolico.
ruta_graf_1 <- file.path(dir_outputs_interpretabilidad, "01_consenso_variables_M_pura_y_fenolico.png")
plot_1 <- consenso_variables %>%
  filter(escenario == escenario_principal, target == target_principal, ranking_consenso <= 12) %>%
  mutate(etiqueta = limpiar_nombre_predictor(predictor))

if (nrow(plot_1) > 0) {
  p1 <- ggplot(plot_1, aes(x = reorder(etiqueta, frecuencia_media), y = frecuencia_media)) +
    geom_col(fill = "grey35") +
    coord_flip() +
    labs(
      title = "Variables quimicas estables en M_pura - y_fenolico_comun",
      subtitle = "Consenso entre modelos prioritarios y bootstrap",
      x = "Predictor quimico",
      y = "Frecuencia media de seleccion"
    ) +
    theme_minimal(base_size = 12)

  ggsave(ruta_graf_1, p1, width = 9, height = 6, dpi = 300)
  graficos_generados <- bind_rows(graficos_generados, data.frame(
    grafico = "01_consenso_variables_M_pura_y_fenolico",
    ruta_grafico = ruta_graf_1,
    descripcion = "Ranking de variables estables para el escenario principal.",
    stringsAsFactors = FALSE
  ))
}

# Grafico 2: importancia por familia en M_pura fenolico.
ruta_graf_2 <- file.path(dir_outputs_interpretabilidad, "02_familias_quimicas_M_pura_y_fenolico.png")
plot_2 <- variables_por_familia %>%
  filter(escenario == escenario_principal, target == target_principal) %>%
  arrange(desc(frecuencia_media_promedio))

if (nrow(plot_2) > 0) {
  p2 <- ggplot(plot_2, aes(x = reorder(familia_quimica, frecuencia_media_promedio), y = frecuencia_media_promedio)) +
    geom_col(fill = "grey35") +
    coord_flip() +
    labs(
      title = "Familias quimicas asociadas a y_fenolico_comun",
      subtitle = "Promedio de estabilidad de variables por familia en M_pura",
      x = "Familia quimica",
      y = "Frecuencia media promedio"
    ) +
    theme_minimal(base_size = 12)

  ggsave(ruta_graf_2, p2, width = 8, height = 5, dpi = 300)
  graficos_generados <- bind_rows(graficos_generados, data.frame(
    grafico = "02_familias_quimicas_M_pura_y_fenolico",
    ruta_grafico = ruta_graf_2,
    descripcion = "Resumen por familias quimicas para el target principal.",
    stringsAsFactors = FALSE
  ))
}

# Grafico 3: matriz variable-modelo para M_pura fenolico.
ruta_graf_3 <- file.path(dir_outputs_interpretabilidad, "03_variable_modelo_M_pura_y_fenolico.png")
plot_3_vars <- consenso_variables %>%
  filter(escenario == escenario_principal, target == target_principal, ranking_consenso <= 10) %>%
  pull(predictor)

plot_3 <- estabilidad_enriquecida %>%
  filter(
    escenario == escenario_principal,
    target == target_principal,
    predictor %in% plot_3_vars,
    modelo %in% c(modelos_prioritarios, modelos_apoyo)
  ) %>%
  mutate(etiqueta = limpiar_nombre_predictor(predictor))

if (nrow(plot_3) > 0) {
  p3 <- ggplot(plot_3, aes(x = modelo, y = reorder(etiqueta, frecuencia_seleccion), fill = frecuencia_seleccion)) +
    geom_tile(color = "white") +
    labs(
      title = "Estabilidad por modelo en M_pura - y_fenolico_comun",
      subtitle = "Frecuencia de seleccion/importancia por bootstrap",
      x = "Modelo",
      y = "Predictor quimico",
      fill = "Frecuencia"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(ruta_graf_3, p3, width = 9, height = 6, dpi = 300)
  graficos_generados <- bind_rows(graficos_generados, data.frame(
    grafico = "03_variable_modelo_M_pura_y_fenolico",
    ruta_grafico = ruta_graf_3,
    descripcion = "Mapa de estabilidad por modelo para variables principales.",
    stringsAsFactors = FALSE
  ))
}

# Grafico 4: variables prioritarias por escenario real.
ruta_graf_4 <- file.path(dir_outputs_interpretabilidad, "04_variables_prioritarias_escenarios_reales.png")
plot_4 <- variables_finales_tesis %>%
  filter(ranking_consenso <= 8) %>%
  mutate(etiqueta = limpiar_nombre_predictor(predictor))

if (nrow(plot_4) > 0) {
  p4 <- ggplot(plot_4, aes(x = reorder(etiqueta, frecuencia_media), y = frecuencia_media)) +
    geom_col(fill = "grey35") +
    coord_flip() +
    facet_wrap(~ escenario, scales = "free_y") +
    labs(
      title = "Variables prioritarias por escenario real",
      subtitle = "Target principal: y_fenolico_comun",
      x = "Predictor quimico",
      y = "Frecuencia media de seleccion"
    ) +
    theme_minimal(base_size = 11)

  ggsave(ruta_graf_4, p4, width = 11, height = 6, dpi = 300)
  graficos_generados <- bind_rows(graficos_generados, data.frame(
    grafico = "04_variables_prioritarias_escenarios_reales",
    ruta_grafico = ruta_graf_4,
    descripcion = "Variables prioritarias en M_pura y M_cata_individual.",
    stringsAsFactors = FALSE
  ))
}

# ------------------------------------------------------------
# 8. Resumen y notas metodologicas
# ------------------------------------------------------------

resumen_ejecucion <- data.frame(
  elemento = c(
    "modelos_candidatos",
    "variables_estabilidad_bootstrap",
    "variables_consenso",
    "variables_finales_tesis",
    "graficos_generados",
    "escenario_principal",
    "target_principal"
  ),
  valor = c(
    nrow(modelos_candidatos),
    nrow(estabilidad_enriquecida),
    nrow(consenso_variables),
    nrow(variables_finales_tesis),
    nrow(graficos_generados),
    escenario_principal,
    target_principal
  ),
  stringsAsFactors = FALSE
)

notas_metodologicas <- data.frame(
  tema = c(
    "interpretabilidad_no_causalidad",
    "random_forest",
    "pls",
    "regularizados",
    "gradient_boosting",
    "colinealidad",
    "matriz_ia"
  ),
  nota = c(
    "Las variables priorizadas representan asociaciones predictivas estables, no efectos causales aislados.",
    "Random Forest se interpreta por estabilidad e importancia de variables, no por coeficientes.",
    "PLS se reporta como modelo quimiometrico interpretable frente a colinealidad.",
    "Ridge, Lasso y Elastic Net apoyan la evaluacion de predictores en contexto de colinealidad.",
    "Gradient Boosting se conserva como modelo de comparacion, pero no se usa como fuente principal de interpretacion si concentra importancia en una sola variable.",
    "En presencia de predictores colineales, se interpretan familias o grupos de compuestos, no coeficientes individuales como efectos independientes.",
    "La matriz IA se mantiene como escenario exploratorio de imputacion sensorial sintetica, no como cata real."
  ),
  stringsAsFactors = FALSE
)

diccionario <- data.frame(
  hoja = c(
    "00_resumen",
    "01_modelos_candidatos",
    "02_importancia_cv",
    "03_estabilidad_bootstrap",
    "04_consenso_variables",
    "05_variables_finales_tesis",
    "06_variables_por_familia",
    "07_colinealidad_variables",
    "08_residuos_apoyo",
    "09_notas_metodologicas",
    "10_graficos_generados"
  ),
  descripcion = c(
    "Resumen de conteos del script.",
    "Modelos ordenados por desempeno bootstrap y rol interpretativo.",
    "Importancia agregada desde validacion cruzada small data.",
    "Estabilidad de variables por modelo en bootstrap.",
    "Consenso de variables entre modelos prioritarios.",
    "Variables recomendadas para discusion de tesis en escenarios reales.",
    "Resumen por familia quimica y tecnica analitica.",
    "Relacion entre variables priorizadas y pares colineales, si esta disponible.",
    "Resumen residual de apoyo, si fue generado por el script 16.",
    "Criterios metodologicos para interpretar resultados.",
    "Rutas de figuras generadas."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 9. Exportacion
# ------------------------------------------------------------

hojas_salida <- list(
  "00_resumen" = resumen_ejecucion,
  "01_modelos_candidatos" = modelos_candidatos,
  "02_importancia_cv" = importancia_cv,
  "03_estabilidad_bootstrap" = estabilidad_enriquecida,
  "04_consenso_variables" = consenso_variables,
  "05_variables_finales_tesis" = variables_finales_tesis,
  "06_variables_por_familia" = variables_por_familia,
  "07_colinealidad_variables" = colinealidad_variables_finales,
  "08_residuos_apoyo" = resumen_residuos,
  "09_notas_metodologicas" = notas_metodologicas,
  "10_graficos_generados" = graficos_generados,
  "11_diccionario" = diccionario
)

hojas_salida <- hojas_salida[vapply(hojas_salida, function(x) is.data.frame(x) && nrow(x) > 0, logical(1))]

guardar_excel_seguro(hojas_salida, ruta_salida_excel)

cat("\nProceso terminado correctamente.\n")
cat("Archivo generado:\n ", ruta_salida_excel, "\n")
cat("Graficos generados en:\n ", dir_outputs_interpretabilidad, "\n")
cat("\nLectura clave:\n")
cat("\n- Las variables finales se priorizan por estabilidad bootstrap, consenso entre modelos y coherencia quimica.")
cat("\n- Random Forest aporta desempeno predictivo; PLS y regularizados aportan interpretabilidad complementaria.")
cat("\n- Las variables colineales deben interpretarse como grupos quimicos, no como efectos independientes.\n")
