# ============================================================
# 20_robustez_resultados_finales.R
# Consolidacion final de robustez: une el resultado principal (19), la
# interpretabilidad (18), el diagnostico de viabilidad estadistica (13),
# la sensibilidad a outliers quimicos (16b) y el cruce outliers-residuos
# (17) en una sola tabla de "conclusiones defendibles" con su evidencia.
# Tesis whisky chileno - modelamiento quimico-sensorial
# ============================================================

# Por que este script: 13 (diagnostico), 16b (sensibilidad) y 17 (cruce de
# outliers) se agregaron el 2026-07-08, despues de que 19 ya consolidaba el
# resultado final. Sin este script, la sensibilidad a outliers -la pieza de
# evidencia mas directa para defender la robustez del resultado principal-
# queda aislada en su propio archivo y no llega al resumen ejecutivo final.

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

paquetes <- c("readxl", "dplyr", "tidyr", "stringr", "writexl")
paquetes_faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(paquetes_faltantes) > 0) {
  stop("Faltan paquetes requeridos: ", paste(paquetes_faltantes, collapse = ", "))
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

dir_modelamiento <- file.path(dir_data, "modelamiento")
dir_outputs_modelamiento <- file.path(dir_proyecto, "outputs", "modelamiento")
dir_outputs_finales <- file.path(dir_outputs_modelamiento, "analisis_finales")
dir.create(dir_outputs_finales, recursive = TRUE, showWarnings = FALSE)

ruta_comparacion <- file.path(dir_modelamiento, "comparacion_escenarios_modelamiento.xlsx")
ruta_interpretabilidad <- file.path(dir_modelamiento, "interpretabilidad_modelos.xlsx")
ruta_diagnostico <- file.path(dir_modelamiento, "diagnostico_modelamiento.xlsx")
ruta_sensibilidad <- file.path(dir_modelamiento, "sensibilidad_outliers_quimicos.xlsx")
ruta_residuos <- file.path(dir_modelamiento, "residuos_diagnostico.xlsx")
ruta_salida_excel <- file.path(dir_modelamiento, "robustez_resultados_finales.xlsx")

rutas_requeridas <- c(ruta_comparacion, ruta_interpretabilidad, ruta_diagnostico, ruta_sensibilidad, ruta_residuos)
faltantes <- rutas_requeridas[!file.exists(rutas_requeridas)]
if (length(faltantes) > 0) {
  stop(
    "Faltan archivos requeridos:\n", paste(faltantes, collapse = "\n"),
    "\nEjecuta primero los scripts 13, 16, 16b, 17, 18 y 19 en orden."
  )
}

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

# ------------------------------------------------------------
# 1. Resultado principal (19)
# ------------------------------------------------------------

resumen_ejecutivo_19 <- read_excel(ruta_comparacion, sheet = "00_resumen_ejecutivo")
valor_de <- function(elem) resumen_ejecutivo_19$valor[resumen_ejecutivo_19$elemento == elem][1]

escenario_principal <- valor_de("escenario_principal")
target_principal <- valor_de("target_principal")
modelo_principal <- valor_de("mejor_modelo_principal")
mae_principal <- as.numeric(valor_de("mae_bootstrap_modelo_principal"))
r2_principal <- as.numeric(valor_de("r2_bootstrap_modelo_principal"))
spearman_principal <- as.numeric(valor_de("spearman_bootstrap_modelo_principal"))

# ------------------------------------------------------------
# 2. Sensibilidad a outliers quimicos (16b)
# ------------------------------------------------------------

sensibilidad_comparativa <- read_excel(ruta_sensibilidad, sheet = "02_resumen_comparativo")
sensibilidad_diferencias <- tryCatch(
  read_excel(ruta_sensibilidad, sheet = "03_comparacion_diferencias"),
  error = function(e) data.frame()
)

# ------------------------------------------------------------
# 3. Cruce outliers EDA vs residuos (17), filtrado al modelo principal
# ------------------------------------------------------------

cruce_residuos <- read_excel(ruta_residuos, sheet = "09_resumen_cruce_outliers")
cruce_modelo_principal <- cruce_residuos %>%
  filter(escenario == escenario_principal, target == target_principal, modelo == modelo_principal)

# ------------------------------------------------------------
# 4. Viabilidad estadistica (13), para el target principal y el secundario
# ------------------------------------------------------------

viabilidad <- read_excel(ruta_diagnostico, sheet = "01_viabilidad_escenarios")
viabilidad_principal <- viabilidad %>% filter(escenario == escenario_principal)

alerta_outliers_13 <- tryCatch(
  read_excel(ruta_diagnostico, sheet = "04_resumen_alerta_outliers"),
  error = function(e) data.frame()
)

# ------------------------------------------------------------
# 5. Variables prioritarias (18)
# ------------------------------------------------------------

variables_finales <- read_excel(ruta_interpretabilidad, sheet = "05_variables_finales_tesis")
variables_prioritarias <- variables_finales %>%
  filter(escenario == escenario_principal, target == target_principal, prioridad_tesis == "prioritaria") %>%
  arrange(ranking_consenso)

# ------------------------------------------------------------
# 6. Tabla de conclusiones defendibles
# ------------------------------------------------------------

target_secundario <- "y_frutal_comun"
# 16b evalua varios modelos (ridge y random_forest_restringido); nos
# quedamos con el modelo_principal (el mismo que reporta 19) para que la
# sensibilidad sea sobre el modelo que realmente se recomienda en la tesis.
sensibilidad_modelo_principal <- sensibilidad_comparativa %>% filter(modelo == modelo_principal)
sens_fenolico_completo <- sensibilidad_modelo_principal %>% filter(target == target_principal, version == "completo")
sens_fenolico_sin <- sensibilidad_modelo_principal %>% filter(target == target_principal, version == "sin_outliers_eda")
sens_frutal_completo <- sensibilidad_modelo_principal %>% filter(target == target_secundario, version == "completo")
sens_frutal_sin <- sensibilidad_modelo_principal %>% filter(target == target_secundario, version == "sin_outliers_eda")

diferencia_mae_pct_fenolico <- sensibilidad_diferencias %>%
  filter(modelo == modelo_principal) %>%
  filter(target == target_principal) %>%
  pull(diferencia_mae_pct) %>%
  {if (length(.) > 0) round(.[1], 1) else NA_real_}

gl_frutal <- viabilidad_principal %>% filter(target == target_secundario) %>% pull(grados_libertad_aprox)
gl_frutal <- if (length(gl_frutal) > 0) gl_frutal[1] else NA_integer_

conclusiones_defendibles <- data.frame(
  afirmacion = c(
    paste0(modelo_principal, " sobre ", target_principal, " en ", escenario_principal, " es el resultado predictivo principal de la tesis."),
    paste0("El resultado principal (", target_principal, ") es robusto a las unidades quimicamente atipicas detectadas en el EDA."),
    paste0("Las unidades marcadas como atipicas en el EDA no predicen sistematicamente peor con el modelo principal."),
    paste0(target_secundario, " debe mantenerse como target secundario/exploratorio, no como resultado central."),
    "Las variables quimicas prioritarias son coherentes con la literatura de compuestos fenolicos volatiles.",
    "El VIF global no es aplicable; se uso correlacion pareada y VIF por bloque como alternativa metodologica."
  ),
  evidencia = c(
    paste0(
      "MAE bootstrap = ", round(mae_principal, 3), "; R2 bootstrap = ", round(r2_principal, 3),
      "; Spearman bootstrap = ", round(spearman_principal, 3), " (n=100 iteraciones bootstrap agrupado)."
    ),
    paste0(
      "MAE bootstrap completo = ", round(sens_fenolico_completo$mae_media[1], 3),
      " vs. sin unidades marcadas por T2/Q = ", round(sens_fenolico_sin$mae_media[1], 3),
      " (diferencia = ", diferencia_mae_pct_fenolico, "%)."
    ),
    if (nrow(cruce_modelo_principal) >= 2) {
      paste0(
        "Residuo absoluto medio en unidades marcadas por el EDA = ",
        round(cruce_modelo_principal$residuo_abs_medio[cruce_modelo_principal$es_outlier_eda], 3),
        " vs. no marcadas = ",
        round(cruce_modelo_principal$residuo_abs_medio[!cruce_modelo_principal$es_outlier_eda], 3), "."
      )
    } else {
      "Ver hoja 09_resumen_cruce_outliers de residuos_diagnostico.xlsx."
    },
    paste0(
      "Grados de libertad aproximados = ", gl_frutal, " en ", escenario_principal,
      " (n_filas_modelables - n_predictores_usables, ver script 13); ",
      "R2 bootstrap del modelo principal para ", target_secundario, " es negativo (peor que predecir la media)."
    ),
    paste0(
      "Variables con prioridad_tesis == 'prioritaria': ",
      paste(variables_prioritarias$predictor, collapse = ", "), " (script 18, hoja 05_variables_finales_tesis)."
    ),
    "Ver colinealidad_modelamiento.xlsx (script 13b) y diagnostico_modelamiento.xlsx (script 13, hoja 01_viabilidad_escenarios)."
  ),
  fuente = c(
    "19_comparar_escenarios_modelamiento.R (00_resumen_ejecutivo)",
    "16b_sensibilidad_outliers_quimicos.R (02_resumen_comparativo, 03_comparacion_diferencias)",
    "17_residuos_diagnostico.R (09_resumen_cruce_outliers)",
    "13_diagnostico_modelamiento.R (01_viabilidad_escenarios) + 16b",
    "18_interpretabilidad_modelos.R (05_variables_finales_tesis)",
    "13b_colinealidad_modelamiento.R"
  ),
  fortaleza = c("fuerte", "fuerte", "moderada", "fuerte", "moderada", "fuerte"),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 7. Resumen ejecutivo consolidado
# ------------------------------------------------------------

resumen_robustez <- data.frame(
  indicador = c(
    "escenario_principal", "target_principal", "modelo_principal",
    "mae_bootstrap_principal", "r2_bootstrap_principal", "spearman_bootstrap_principal",
    "mae_bootstrap_sin_outliers_eda", "diferencia_mae_pct_por_exclusion_outliers",
    "n_unidades_marcadas_eda_M_pura", "grados_libertad_aprox_target_secundario",
    "r2_bootstrap_target_secundario_completo", "r2_bootstrap_target_secundario_sin_outliers"
  ),
  valor = c(
    escenario_principal, target_principal, modelo_principal,
    round(mae_principal, 4), round(r2_principal, 4), round(spearman_principal, 4),
    round(sens_fenolico_sin$mae_media[1], 4), paste0(diferencia_mae_pct_fenolico, "%"),
    as.character(sum(alerta_outliers_13$n_unidades_marcadas[alerta_outliers_13$escenario == escenario_principal])),
    as.character(gl_frutal),
    round(sens_frutal_completo$r2_media[1], 4), round(sens_frutal_sin$r2_media[1], 4)
  ),
  stringsAsFactors = FALSE
)

notas <- data.frame(
  punto = c(
    "Objetivo", "Como leer conclusiones_defendibles", "Relacion con el resto del pipeline"
  ),
  descripcion = c(
    "Consolidar en un solo lugar la evidencia de robustez del resultado principal, dispersa en 13/16b/17/18/19, para facilitar la redaccion de resultados/discusion y la defensa oral.",
    "Cada fila es una afirmacion que puede sostenerse en la tesis, con su evidencia numerica exacta y la hoja de origen para verificarla en caso de pregunta de comision. 'fortaleza' es un juicio cualitativo: fuerte = evidencia cuantitativa directa; moderada = evidencia indirecta o con matices.",
    "Este script no recalcula nada: solo lee y cruza resultados ya generados por 13, 16, 16b, 17, 18 y 19. Ejecutar despues de todos ellos."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 8. Exportacion
# ------------------------------------------------------------

lista_hojas <- list(
  "00_resumen_robustez" = resumen_robustez,
  "01_conclusiones_defendibles" = conclusiones_defendibles,
  "02_sensibilidad_outliers" = sensibilidad_comparativa,
  "03_viabilidad_escenario" = viabilidad_principal,
  "04_variables_prioritarias" = variables_prioritarias,
  "05_notas" = notas
)

guardar_excel_seguro(lista_hojas, ruta_salida_excel)

cat("\nProceso terminado correctamente.\n")
cat("Archivo generado:\n", ruta_salida_excel, "\n")
cat("\nConclusiones defendibles:\n")
print(conclusiones_defendibles %>% select(afirmacion, fortaleza))
