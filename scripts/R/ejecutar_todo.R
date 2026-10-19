# ============================================================
# Ejecuta el flujo completo del analisis en el orden correcto
# ============================================================
# Uso: con el directorio de trabajo en la raiz del proyecto (por ejemplo, al
# abrir Modelamiento.Rproj), ejecutar
#   source("scripts/R/ejecutar_todo.R")
# o desde una terminal, en la raiz del proyecto,
#   Rscript scripts/R/ejecutar_todo.R
#
# Requiere los datos de entrada descritos en data/LEEME.md. Cada script se
# ejecuta en un proceso de R independiente, igual que si se corriera por
# separado, para que ninguno dependa de objetos que dejo el anterior. Si un
# script falla, la ejecucion se detiene e indica cual fue.
#
# 16i_verificacion_semilla.R no se incluye por defecto: es una verificacion
# opcional que repite la validacion del script 16 con 30 semillas y no
# alimenta a ningun otro script. Para incluirla, cambiar incluir_16i a TRUE.

incluir_16i <- FALSE

if (!file.exists("scripts/R/procesamiento/00_resumen_datos_iniciales.R")) {
  stop("El directorio de trabajo debe ser la raiz del proyecto (donde esta Modelamiento.Rproj).")
}

# El 00 no se lista porque cada script lo carga al comenzar.
scripts <- c(
  "procesamiento/01_clasificar_datos_iniciales.R",
  "procesamiento/02_extraer_quimica.R",
  "procesamiento/02b_clasificar_compuestos_gcms.R",
  "procesamiento/03_extraer_sensorial.R",
  "procesamiento/04_matriz_pura.R",
  "procesamiento/05_matriz_pura_expandida_evaluador.R",
  "procesamiento/06_matrices_sin_cruce.R",
  "procesamiento/07_matriz_sensibilidad_cata_individual.R",
  "procesamiento/08_matriz_datos_ia.R",
  "procesamiento/09_generar_excel_presentacion.R",
  "procesamiento/09b_generar_excel_presentacion_comision.R",
  "exploratorio/10_analisis_quimico.R",
  "exploratorio/11_analisis_sensorial.R",
  "exploratorio/11b_eda_cruzado_quimico_sensorial.R",
  "modelamiento/12_preparar_datos_modelamiento.R",
  "modelamiento/13_diagnostico_modelamiento.R",
  "modelamiento/13b_colinealidad_modelamiento.R",
  "modelamiento/14_modelo_base.R",
  "modelamiento/15_modelos_small_data.R",
  "modelamiento/16_validacion_bootstrap_cv.R",
  "modelamiento/16b_sensibilidad_outliers_quimicos.R",
  "modelamiento/16c_sensibilidad_imputacion_gcms.R",
  "modelamiento/16d_sensibilidad_muestras_comerciales.R",
  "modelamiento/16e_sensibilidad_imputacion_gcfid.R",
  "modelamiento/16f_sensibilidad_escala_septiembre.R",
  "modelamiento/16h_sensibilidad_comerciales_cata_individual.R",
  if (incluir_16i) "modelamiento/16i_verificacion_semilla.R",
  "modelamiento/17_residuos_diagnostico.R",
  "analisis_finales/18_interpretabilidad_modelos.R",
  "analisis_finales/19_comparar_escenarios_modelamiento.R",
  "analisis_finales/20_robustez_resultados_finales.R"
)

rscript <- file.path(R.home("bin"), "Rscript")
inicio_total <- Sys.time()

for (i in seq_along(scripts)) {
  ruta <- file.path("scripts", "R", scripts[i])
  cat(sprintf("[%2d/%d] %s ... ", i, length(scripts), basename(ruta)))
  inicio <- Sys.time()
  estado <- system2(rscript, args = shQuote(ruta), stdout = FALSE, stderr = FALSE)
  segundos <- round(as.numeric(difftime(Sys.time(), inicio, units = "secs")))
  if (!identical(as.integer(estado), 0L)) {
    cat("ERROR\n")
    stop("Fallo ", basename(ruta), ". Ejecutarlo por separado para ver el detalle del error.")
  }
  cat(sprintf("ok (%d s)\n", segundos))
}

cat(sprintf(
  "\nFlujo completo ejecutado en %.1f minutos. Resultados en data/ y outputs/.\n",
  as.numeric(difftime(Sys.time(), inicio_total, units = "mins"))
))
