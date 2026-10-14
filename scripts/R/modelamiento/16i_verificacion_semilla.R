# ============================================================
# 16i_verificacion_semilla.R
# Verificacion del efecto de la semilla aleatoria sobre los resultados
# Tesis whisky chileno - modelamiento quimico-sensorial
# ============================================================

# Este script responde a una pregunta puntual: cuanto cambian las metricas del
# modelo principal (M_pura, y_fenolico_comun) si se cambia la semilla 123 usada
# en 16_validacion_bootstrap_cv.R por otra.
#
# Metodo:
# - NO modifica 16_validacion_bootstrap_cv.R. Lee su texto, sustituye en memoria
#   las semillas por un parametro, restringe la ejecucion a M_pura y
#   y_fenolico_comun y redirige sus salidas a una carpeta propia.
# - Ejecuta esa copia una vez por semilla, cada una en un proceso R separado.
# - Con la semilla 123 la copia reproduce exactamente los resultados de 16
#   (se verifica al final y queda registrado en la hoja 00_resumen).
#
# Importante - por que las semillas estan separadas por 1000:
# - En 16, la iteracion b del bootstrap usa la semilla (semilla_base + b), con
#   b entre 1 y 100. Dos semillas S1 y S2 con |S1 - S2| < 100 comparten parte de
#   sus remuestreos, de modo que no serian corridas independientes (con
#   semillas consecutivas, 83 de cada 100 remuestreos se repiten). Con una
#   separacion de 1000, las ventanas de remuestreo son disjuntas.
#
# Resultados:
# - data/modelamiento/verificacion_semilla.xlsx
# - outputs/modelamiento/verificacion_semilla/por_semilla/ (una planilla por semilla)

# ------------------------------------------------------------
# 1. Configuracion
# ------------------------------------------------------------

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

library(readxl)
library(dplyr)
library(writexl)

semilla_tesis <- 123L
n_semillas <- 30L
paso_semillas <- 1000L
semillas <- semilla_tesis + paso_semillas * seq_len(n_semillas)

escenario_principal <- "M_pura"
target_principal <- "y_fenolico_comun"

dir_modelamiento <- file.path(dir_data, "modelamiento")
dir_outputs <- file.path(dir_proyecto, "outputs")
dir_salida <- file.path(dir_outputs, "modelamiento", "verificacion_semilla")
dir_por_semilla <- file.path(dir_salida, "por_semilla")
dir.create(dir_por_semilla, recursive = TRUE, showWarnings = FALSE)

ruta_script_base <- file.path(dir_scripts_r, "modelamiento", "16_validacion_bootstrap_cv.R")
ruta_referencia <- file.path(dir_modelamiento, "validacion_bootstrap_cv.xlsx")
ruta_salida_excel <- file.path(dir_modelamiento, "verificacion_semilla.xlsx")

if (!file.exists(ruta_script_base)) stop("No se encontro: ", ruta_script_base)
if (!file.exists(ruta_referencia)) {
  stop("Falta ", ruta_referencia, ". Ejecuta antes 16_validacion_bootstrap_cv.R.")
}

# ------------------------------------------------------------
# 2. Copia parcheada del script 16
# ------------------------------------------------------------

sustituir_una_vez <- function(texto, viejo, nuevo) {
  n <- lengths(regmatches(texto, gregexpr(viejo, texto, fixed = TRUE)))
  if (n != 1) stop("Se esperaba 1 coincidencia y hay ", n, " para: ", substr(viejo, 1, 70))
  sub(viejo, nuevo, texto, fixed = TRUE)
}

crear_script_parcheado <- function(ruta_destino) {
  txt <- paste(readLines(ruta_script_base, warn = FALSE, encoding = "latin1"), collapse = "\n")

  cabecera <- paste0(
    "args_semilla <- commandArgs(trailingOnly = TRUE)\n",
    "SEMILLA <- as.integer(args_semilla[1])\n",
    "stopifnot(!is.na(SEMILLA))\n"
  )
  txt <- paste0(cabecera, txt)

  # semillas: global, base por escenario-target y semilla de Random Forest
  txt <- sustituir_una_vez(txt, "set.seed(123)\n", "set.seed(SEMILLA)\n")
  txt <- sustituir_una_vez(
    txt,
    "semilla_base <- 123 + sum(utf8ToInt(paste0(escenario_i, \"_\", target_i)))",
    "semilla_base <- SEMILLA + sum(utf8ToInt(paste0(escenario_i, \"_\", target_i)))"
  )
  txt <- sustituir_una_vez(txt, "seed = 123 + iteracion", "seed = SEMILLA + iteracion")
  # semilla de los pliegues internos de cv.glmnet: el desplazamiento vale 0 con la semilla 123
  txt <- sustituir_una_vez(
    txt,
    "set.seed(1000000 + iteracion + sum(utf8ToInt(paste0(escenario, \"_\", target, \"_\", nombre_modelo))))",
    "set.seed(1000000 + (SEMILLA - 123L) + iteracion + sum(utf8ToInt(paste0(escenario, \"_\", target, \"_\", nombre_modelo))))"
  )

  # salidas fuera de las rutas originales
  txt <- sustituir_una_vez(
    txt,
    "dir_outputs <- file.path(dir_proyecto, \"outputs\")",
    paste0("dir_outputs <- file.path(\"", gsub("\\\\", "/", dir_salida), "\", \"figuras_16\")")
  )
  txt <- sustituir_una_vez(
    txt,
    "ruta_salida_excel <- file.path(dir_modelamiento, \"validacion_bootstrap_cv.xlsx\")",
    paste0("ruta_salida_excel <- file.path(\"", gsub("\\\\", "/", dir_por_semilla),
           "\", paste0(\"boot_semilla_\", SEMILLA, \".xlsx\"))")
  )

  # solo el escenario y target principal. La semilla base depende del NOMBRE del
  # escenario y del target, no de su posicion, por lo que filtrar no altera los
  # remuestreos de lo que si se ejecuta.
  txt <- sustituir_una_vez(
    txt,
    "plan_bootstrap <- targets_ejecutados %>%\n  filter(ejecutar_bootstrap) %>%\n  arrange(escenario, target)",
    paste0("plan_bootstrap <- targets_ejecutados %>%\n  filter(ejecutar_bootstrap) %>%\n",
           "  filter(escenario == \"", escenario_principal, "\", target == \"", target_principal, "\") %>%\n",
           "  arrange(escenario, target)")
  )

  writeLines(txt, ruta_destino, useBytes = TRUE)
  invisible(ruta_destino)
}

ruta_parcheado <- file.path(tempdir(), "16_bootstrap_semilla_parametrizada.R")
crear_script_parcheado(ruta_parcheado)

# ------------------------------------------------------------
# 3. Ejecucion por semilla
# ------------------------------------------------------------

semillas_a_correr <- c(semilla_tesis, semillas)
rscript <- file.path(R.home("bin"), "Rscript")

for (s in semillas_a_correr) {
  ruta_s <- file.path(dir_por_semilla, paste0("boot_semilla_", s, ".xlsx"))
  if (file.exists(ruta_s)) {
    message("Semilla ", s, ": ya calculada, se reutiliza.")
    next
  }
  message("Semilla ", s, ": ejecutando bootstrap (", which(semillas_a_correr == s), " de ", length(semillas_a_correr), ")")
  salida <- suppressWarnings(system2(rscript, args = c(shQuote(ruta_parcheado), s), stdout = TRUE, stderr = TRUE))
  if (!file.exists(ruta_s)) {
    stop("La corrida con semilla ", s, " no genero resultados. Ultimas lineas:\n",
         paste(tail(salida, 8), collapse = "\n"))
  }
}

# ------------------------------------------------------------
# 4. Consolidacion
# ------------------------------------------------------------

leer_semilla <- function(s) {
  ruta_s <- file.path(dir_por_semilla, paste0("boot_semilla_", s, ".xlsx"))
  d <- as.data.frame(read_excel(ruta_s, sheet = "02_resumen_metricas_boot"))
  d <- d[d$escenario == escenario_principal & d$target == target_principal,
         c("modelo", "mae_media", "r2_agregado", "r2_pct_positivo")]
  d$semilla <- s
  d
}

control <- leer_semilla(semilla_tesis)
metricas <- do.call(rbind, lapply(semillas, leer_semilla))

# Verificacion: la copia parcheada debe reproducir 16 con la semilla 123.
referencia <- as.data.frame(read_excel(ruta_referencia, sheet = "02_resumen_metricas_boot"))
referencia <- referencia[referencia$escenario == escenario_principal & referencia$target == target_principal, ]
cols_cmp <- c("mae_media", "r2_agregado", "r2_pct_positivo")
control_o <- control[order(control$modelo), ]
ref_o <- referencia[match(control_o$modelo, referencia$modelo), ]
dif_maxima <- max(abs(as.matrix(control_o[, cols_cmp]) - as.matrix(ref_o[, cols_cmp])), na.rm = TRUE)
control_ok <- is.finite(dif_maxima) && dif_maxima < 1e-9
if (!control_ok) {
  warning("La corrida de control NO reproduce 16_validacion_bootstrap_cv.R (diferencia maxima ", dif_maxima, ").")
}

resumen_modelos <- metricas %>%
  group_by(modelo) %>%
  summarise(
    mae_media_semillas = mean(mae_media),
    mae_sd_semillas = sd(mae_media),
    r2_agregado_media = mean(r2_agregado),
    r2_agregado_sd = sd(r2_agregado),
    r2_agregado_min = min(r2_agregado),
    r2_agregado_mediana = median(r2_agregado),
    r2_agregado_max = max(r2_agregado),
    r2_agregado_p025 = unname(quantile(r2_agregado, 0.025)),
    r2_agregado_p975 = unname(quantile(r2_agregado, 0.975)),
    n_semillas_r2_positivo = sum(r2_agregado > 0),
    n_semillas = dplyr::n(),
    .groups = "drop"
  ) %>%
  left_join(
    control %>% transmute(modelo, mae_semilla_123 = mae_media, r2_agregado_semilla_123 = r2_agregado),
    by = "modelo"
  ) %>%
  arrange(mae_media_semillas)

por_semilla <- split(metricas, metricas$semilla)
gana <- function(m, ref) sum(vapply(por_semilla, function(d) {
  d$mae_media[d$modelo == m] < d$mae_media[d$modelo == ref]
}, logical(1)))
mejor_candidato <- vapply(por_semilla, function(d) {
  d <- d[!d$modelo %in% c("media_entrenamiento", "lineal_reducido"), ]
  d$modelo[which.min(d$mae_media)]
}, character(1))

rf <- metricas[metricas$modelo == "random_forest_restringido", ]
rf_control <- control[control$modelo == "random_forest_restringido", ]

modelo_principal <- data.frame(
  indicador = c(
    "r2_agregado_semilla_123", "mae_semilla_123",
    "r2_agregado_media_semillas", "r2_agregado_mediana_semillas", "r2_agregado_sd_semillas",
    "r2_agregado_min_semillas", "r2_agregado_max_semillas",
    "r2_agregado_ic95_empirico_inferior", "r2_agregado_ic95_empirico_superior",
    "mae_media_semillas",
    "semillas_con_r2_agregado_positivo",
    "semillas_con_r2_igual_o_mayor_que_semilla_123",
    "semillas_donde_rf_es_mejor_candidato_por_mae",
    "semillas_donde_rf_supera_media_entrenamiento",
    "semillas_donde_rf_supera_lineal_reducido",
    "semillas_donde_rf_supera_ridge"
  ),
  valor = c(
    round(rf_control$r2_agregado, 4), round(rf_control$mae_media, 4),
    round(mean(rf$r2_agregado), 4), round(median(rf$r2_agregado), 4), round(sd(rf$r2_agregado), 4),
    round(min(rf$r2_agregado), 4), round(max(rf$r2_agregado), 4),
    round(unname(quantile(rf$r2_agregado, 0.025)), 4), round(unname(quantile(rf$r2_agregado, 0.975)), 4),
    round(mean(rf$mae_media), 4),
    paste0(sum(rf$r2_agregado > 0), " de ", nrow(rf)),
    paste0(sum(rf$r2_agregado >= rf_control$r2_agregado), " de ", nrow(rf)),
    paste0(sum(mejor_candidato == "random_forest_restringido"), " de ", length(por_semilla)),
    paste0(gana("random_forest_restringido", "media_entrenamiento"), " de ", length(por_semilla)),
    paste0(gana("random_forest_restringido", "lineal_reducido"), " de ", length(por_semilla)),
    paste0(gana("random_forest_restringido", "ridge"), " de ", length(por_semilla))
  ),
  stringsAsFactors = FALSE
)

resumen_ejecucion <- data.frame(
  indicador = c("escenario", "target", "semilla_de_la_tesis", "semillas_evaluadas", "separacion_entre_semillas",
                "n_bootstrap_por_semilla", "control_reproduce_script_16", "diferencia_maxima_control", "archivo_salida"),
  valor = c(escenario_principal, target_principal, semilla_tesis,
            paste(range(semillas), collapse = " a "), paso_semillas,
            100, ifelse(control_ok, "si", "NO"), format(dif_maxima, digits = 3), ruta_salida_excel),
  stringsAsFactors = FALSE
)

notas <- data.frame(
  punto = c("Que compara", "Por que 1000", "Interpretacion", "Limite"),
  descripcion = c(
    "Las metricas bootstrap de los modelos candidatos y base en M_pura, y_fenolico_comun, con la semilla 123 (tesis) frente a 30 semillas independientes.",
    "La iteracion b usa la semilla (semilla_base + b) con b de 1 a 100; semillas separadas por menos de 100 comparten remuestreos. Con 1000 las ventanas son disjuntas.",
    "La semilla fija el remuestreo del bootstrap y la inicializacion de Random Forest. Si el R2 de la semilla 123 queda sobre el rango de las demas, ese valor es un caso favorable del procedimiento y debe leerse como tal.",
    "Solo cubre M_pura y y_fenolico_comun. No verifica el efecto de la semilla en las otras sensibilidades (16b a 16h) ni en los otros escenarios."
  ),
  stringsAsFactors = FALSE
)

hojas <- list(
  "00_resumen" = resumen_ejecucion,
  "01_metricas_por_semilla" = rbind(control, metricas),
  "02_resumen_por_modelo" = resumen_modelos,
  "03_modelo_principal" = modelo_principal,
  "04_notas" = notas
)

if (file.exists(ruta_salida_excel)) file.remove(ruta_salida_excel)
writexl::write_xlsx(hojas, ruta_salida_excel)

cat("\nProceso terminado correctamente.\n")
cat("Archivo generado:\n", ruta_salida_excel, "\n")
cat("Control con semilla 123 reproduce 16_validacion_bootstrap_cv.R:", ifelse(control_ok, "SI", "NO"), "\n")
cat("\nRandom Forest restringido, R2 agregado:\n")
cat(sprintf("  semilla 123 (tesis): %.3f\n", rf_control$r2_agregado))
cat(sprintf("  %d semillas independientes: media %.3f, rango [%.3f, %.3f]\n",
            nrow(rf), mean(rf$r2_agregado), min(rf$r2_agregado), max(rf$r2_agregado)))
