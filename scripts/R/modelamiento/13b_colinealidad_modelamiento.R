# 13b_colinealidad_modelamiento.R
# Diagnóstico separado de colinealidad para la fase de modelamiento
# Proyecto: análisis químico-sensorial de whisky chileno

# -------------------------------------------------------------------------
# 0. Configuración general
# -------------------------------------------------------------------------

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

paquetes <- c("readxl", "dplyr", "tidyr", "stringr", "writexl", "ggplot2")
paquetes_faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(paquetes_faltantes) > 0) {
  stop(
    "Faltan paquetes requeridos: ", paste(paquetes_faltantes, collapse = ", "),
    "\nInstálalos antes de ejecutar este script."
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
dir_outputs_colinealidad <- file.path(dir_outputs_modelamiento, "colinealidad")

dir.create(dir_modelamiento, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_outputs_colinealidad, recursive = TRUE, showWarnings = FALSE)

ruta_datos_modelamiento <- file.path(dir_modelamiento, "datos_modelamiento.xlsx")
ruta_salida_excel <- file.path(dir_modelamiento, "colinealidad_modelamiento.xlsx")

if (!file.exists(ruta_datos_modelamiento)) {
  stop("No existe el archivo requerido: ", ruta_datos_modelamiento,
       "\nEjecuta primero scripts/R/modelamiento/12_preparar_datos_modelamiento.R")
}

umbral_correlacion_alta <- 0.85
umbral_correlacion_muy_alta <- 0.95
max_variables_heatmap <- 25

# -------------------------------------------------------------------------
# 1. Funciones auxiliares
# -------------------------------------------------------------------------

guardar_excel_seguro <- function(lista_hojas, ruta) {
  intento <- tryCatch({
    writexl::write_xlsx(lista_hojas, ruta)
    TRUE
  }, error = function(e) {
    FALSE
  })

  if (!intento) {
    ruta_alt <- sub("\\.xlsx$", paste0("_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"), ruta)
    writexl::write_xlsx(lista_hojas, ruta_alt)
    message("No se pudo sobrescribir el archivo. Se guardó una copia en: ", ruta_alt)
  }
}

convertir_numericamente <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(x))
}

etiqueta_variable <- function(x) {
  bloque <- dplyr::case_when(
    str_detect(x, "^x_gcfid_") ~ "GC-FID",
    str_detect(x, "^x_gcms_") ~ "GC-MS",
    str_detect(x, "^x_ju_") ~ "JU",
    str_detect(x, "^x_folin_") ~ "Folin",
    TRUE ~ NA_character_
  )
  unidad <- dplyr::case_when(
    str_detect(x, "_ppm$") ~ "ppm",
    str_detect(x, "_pct$|_percent$") ~ "%",
    str_detect(x, "_g_l$") ~ "g/L",
    str_detect(x, "_ug_ag_ml$") ~ "µg AG/mL",
    TRUE ~ NA_character_
  )
  base <- x %>%
    str_remove("^x_gcfid_|^x_gcms_|^x_ju_|^x_folin_|^x_") %>%
    str_replace_all("_ppm$|_pct$|_percent$|_g_l$|_ug_ag_ml$", "") %>%
    str_replace_all("_faci$", " FACI") %>%
    str_replace_all("_", " ") %>%
    str_trunc(30)
  etiqueta <- ifelse(!is.na(unidad), paste0(base, " (", unidad, ")"), base)
  ifelse(!is.na(bloque), paste0("[", bloque, "] ", etiqueta), etiqueta)
}

bloque_de_variable <- function(x) {
  dplyr::case_when(
    str_detect(x, "^x_gcfid_") ~ 1L,
    str_detect(x, "^x_gcms_") ~ 2L,
    str_detect(x, "^x_ju_") ~ 3L,
    str_detect(x, "^x_folin_") ~ 4L,
    TRUE ~ 5L
  )
}

clasificar_correlacion <- function(abs_r) {
  dplyr::case_when(
    is.na(abs_r) ~ "no_calculable",
    abs_r >= umbral_correlacion_muy_alta ~ "muy_alta",
    abs_r >= umbral_correlacion_alta ~ "alta",
    TRUE ~ "moderada_o_baja"
  )
}

obtener_predictores_x <- function(df) {
  grep("^x_", names(df), value = TRUE)
}

preparar_matriz_x <- function(df) {
  predictores <- obtener_predictores_x(df)
  if (length(predictores) == 0) return(data.frame())

  x <- df[, predictores, drop = FALSE]
  x <- as.data.frame(lapply(x, convertir_numericamente))
  names(x) <- predictores

  # Eliminar predictores completamente vacíos o sin variabilidad.
  mantener <- vapply(x, function(v) {
    v_no_na <- v[!is.na(v)]
    length(v_no_na) >= 3 && length(unique(v_no_na)) >= 2 && stats::sd(v_no_na) > 0
  }, logical(1))

  x[, mantener, drop = FALSE]
}

resumen_predictores_escenario <- function(df, escenario) {
  predictores <- obtener_predictores_x(df)
  if (length(predictores) == 0) {
    return(data.frame(
      escenario = character(), predictor = character(), n_filas = integer(),
      n_no_na = integer(), pct_no_na = numeric(), n_distintos = integer(),
      desviacion_estandar = numeric(), usable_correlacion = logical(), motivo = character()
    ))
  }

  salida <- lapply(predictores, function(p) {
    v <- convertir_numericamente(df[[p]])
    v_no_na <- v[!is.na(v)]
    n_no_na <- length(v_no_na)
    n_dist <- length(unique(v_no_na))
    desv <- ifelse(n_no_na >= 2, stats::sd(v_no_na), NA_real_)
    usable <- n_no_na >= 3 && n_dist >= 2 && !is.na(desv) && desv > 0

    motivo <- dplyr::case_when(
      n_no_na == 0 ~ "sin_datos",
      n_no_na < 3 ~ "menos_de_3_valores",
      n_dist < 2 ~ "sin_variabilidad",
      is.na(desv) || desv == 0 ~ "desviacion_cero",
      TRUE ~ "usable"
    )

    data.frame(
      escenario = escenario,
      predictor = p,
      n_filas = nrow(df),
      n_no_na = n_no_na,
      pct_no_na = round(100 * n_no_na / nrow(df), 2),
      n_distintos = n_dist,
      desviacion_estandar = round(desv, 6),
      usable_correlacion = usable,
      motivo = motivo,
      stringsAsFactors = FALSE
    )
  })

  bind_rows(salida)
}

calcular_pares_correlacion <- function(df, escenario) {
  x <- preparar_matriz_x(df)

  if (ncol(x) < 2) {
    return(data.frame(
      escenario = character(), predictor_1 = character(), predictor_2 = character(),
      r_pearson = numeric(), abs_r = numeric(), n_comunes = integer(),
      nivel_colinealidad = character(), recomendacion = character()
    ))
  }

  pred <- names(x)
  resultados <- list()
  k <- 1

  for (i in seq_len(length(pred) - 1)) {
    for (j in (i + 1):length(pred)) {
      v1 <- x[[pred[i]]]
      v2 <- x[[pred[j]]]
      completos <- !is.na(v1) & !is.na(v2)
      n_comunes <- sum(completos)

      r <- if (n_comunes >= 3) {
        suppressWarnings(stats::cor(v1[completos], v2[completos], method = "pearson"))
      } else {
        NA_real_
      }

      abs_r <- abs(r)
      nivel <- clasificar_correlacion(abs_r)

      recomendacion <- dplyr::case_when(
        is.na(r) ~ "no_interpretar; pocos_datos_comunes",
        abs_r >= umbral_correlacion_muy_alta ~ "colinealidad_muy_alta; evitar_modelos_lineales_sin_regularizacion",
        abs_r >= umbral_correlacion_alta ~ "colinealidad_alta; preferir_ridge_lasso_elastic_net_o_pls",
        TRUE ~ "sin_alerta_fuerte"
      )

      resultados[[k]] <- data.frame(
        escenario = escenario,
        predictor_1 = pred[i],
        predictor_2 = pred[j],
        r_pearson = round(r, 6),
        abs_r = round(abs_r, 6),
        n_comunes = n_comunes,
        nivel_colinealidad = nivel,
        recomendacion = recomendacion,
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
  }

  bind_rows(resultados) %>%
    arrange(desc(abs_r))
}

calcular_vif_escenario <- function(df, escenario) {
  x <- preparar_matriz_x(df)

  if (ncol(x) < 2) {
    return(data.frame(
      escenario = escenario,
      predictor = NA_character_,
      vif = NA_real_,
      n_completo = 0,
      p_predictores = ncol(x),
      estado_vif = "no_calculable_menos_de_2_predictores",
      stringsAsFactors = FALSE
    ))
  }

  # Para VIF se requieren filas completas en el subconjunto usado.
  # Se priorizan predictores con alta completitud para evitar perder todas las filas.
  completitud <- vapply(x, function(v) mean(!is.na(v)), numeric(1))
  x_vif <- x[, completitud >= 0.80, drop = FALSE]

  if (ncol(x_vif) < 2) {
    return(data.frame(
      escenario = escenario,
      predictor = NA_character_,
      vif = NA_real_,
      n_completo = sum(stats::complete.cases(x)),
      p_predictores = ncol(x_vif),
      estado_vif = "no_calculable_por_baja_completitud",
      stringsAsFactors = FALSE
    ))
  }

  x_complete <- x_vif[stats::complete.cases(x_vif), , drop = FALSE]
  n_complete <- nrow(x_complete)
  p <- ncol(x_complete)

  if (n_complete <= p + 2) {
    return(data.frame(
      escenario = escenario,
      predictor = names(x_vif),
      vif = NA_real_,
      n_completo = n_complete,
      p_predictores = p,
      estado_vif = "no_calculable_n_insuficiente_para_p",
      stringsAsFactors = FALSE
    ))
  }

  salida <- lapply(names(x_complete), function(pred) {
    otros <- setdiff(names(x_complete), pred)
    formula_vif <- stats::as.formula(paste(pred, "~", paste(otros, collapse = " + ")))

    r2 <- tryCatch({
      fit <- stats::lm(formula_vif, data = x_complete)
      summary(fit)$r.squared
    }, error = function(e) NA_real_)

    vif <- if (is.na(r2)) NA_real_ else if (r2 >= 0.999999) Inf else 1 / (1 - r2)

    estado <- dplyr::case_when(
      is.infinite(vif) ~ "vif_infinito_colinealidad_perfecta",
      is.na(vif) ~ "vif_no_calculable",
      vif >= 10 ~ "vif_muy_alto",
      vif >= 5 ~ "vif_alto",
      TRUE ~ "vif_aceptable"
    )

    data.frame(
      escenario = escenario,
      predictor = pred,
      vif = round(vif, 6),
      n_completo = n_complete,
      p_predictores = p,
      estado_vif = estado,
      stringsAsFactors = FALSE
    )
  })

  bind_rows(salida) %>%
    arrange(desc(vif))
}

graficar_barras_colinealidad <- function(resumen_escenarios) {
  ruta <- file.path(dir_outputs_colinealidad, "01_pares_alta_colinealidad_por_escenario.png")

  p <- ggplot(resumen_escenarios, aes(x = reorder(escenario, n_pares_alta_correlacion), y = n_pares_alta_correlacion)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Pares de predictores químicos con alta colinealidad",
      subtitle = paste0("Correlación absoluta de Pearson >= ", umbral_correlacion_alta),
      x = "Escenario",
      y = "Número de pares"
    ) +
    theme_minimal(base_size = 11)

  ggsave(ruta, p, width = 9, height = 5, dpi = 300)
  ruta
}

graficar_heatmap_escenario <- function(df, escenario, pares_altos) {
  x <- preparar_matriz_x(df)
  if (ncol(x) < 2) return(NA_character_)

  pares_esc <- pares_altos %>% filter(escenario == !!escenario, abs_r >= umbral_correlacion_alta)

  if (nrow(pares_esc) > 0) {
    frecuencia <- bind_rows(
      pares_esc %>% transmute(predictor = predictor_1),
      pares_esc %>% transmute(predictor = predictor_2)
    ) %>%
      count(predictor, name = "n_pares") %>%
      arrange(desc(n_pares))

    vars <- frecuencia$predictor[seq_len(min(nrow(frecuencia), max_variables_heatmap))]
  } else {
    # Si no hay pares altos, graficar los predictores más variables.
    sds <- vapply(x, function(v) stats::sd(v, na.rm = TRUE), numeric(1))
    vars <- names(sort(sds, decreasing = TRUE))[seq_len(min(length(sds), max_variables_heatmap))]
  }

  vars <- vars[vars %in% names(x)]
  if (length(vars) < 2) return(NA_character_)

  # Ordenar las variables por bloque analitico (GC-FID, GC-MS, JU, Folin) para
  # que el heatmap agrupe visualmente cada bloque en vez de mezclarlos.
  vars <- vars[order(bloque_de_variable(vars))]

  cor_mat <- suppressWarnings(stats::cor(x[, vars, drop = FALSE], use = "pairwise.complete.obs", method = "pearson"))
  cor_long <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
  names(cor_long) <- c("predictor_1", "predictor_2", "r")
  cor_long$predictor_1_label <- etiqueta_variable(cor_long$predictor_1)
  cor_long$predictor_2_label <- etiqueta_variable(cor_long$predictor_2)

  niveles <- etiqueta_variable(vars)
  cor_long$predictor_1_label <- factor(cor_long$predictor_1_label, levels = niveles)
  cor_long$predictor_2_label <- factor(cor_long$predictor_2_label, levels = rev(niveles))

  # Los pares de variables de distintos bloques quimicos no tienen unidades
  # analiticas en comun, por lo que su r queda NA -- se descartan del grafico
  # en vez de pintarse como celdas grises, para no desperdiciar la mitad del
  # area del heatmap en "sin dato".
  cor_long <- cor_long[!is.na(cor_long$r), ]

  ruta <- file.path(dir_outputs_colinealidad, paste0("02_heatmap_colinealidad_", escenario, ".png"))

  n_vars <- length(vars)
  tam_texto <- if (n_vars > 18) 2.2 else if (n_vars > 10) 2.8 else 3.2

  p <- ggplot(cor_long, aes(x = predictor_1_label, y = predictor_2_label, fill = r)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", r)), size = tam_texto, color = "grey15") +
    scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0, limits = c(-1, 1), name = "r") +
    scale_x_discrete(drop = TRUE) +
    scale_y_discrete(drop = TRUE) +
    labs(
      title = paste("Heatmap de colinealidad -", escenario),
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.75)),
      axis.text.y = element_text(size = rel(0.75)),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "grey97", color = NA)
    )

  ancho <- min(6 + n_vars * 0.32, 16)
  alto <- min(5 + n_vars * 0.28, 13)
  ggsave(ruta, p, width = ancho, height = alto, dpi = 300)
  ruta
}

# -------------------------------------------------------------------------
# 2. Lectura de escenarios
# -------------------------------------------------------------------------

escenarios <- data.frame(
  escenario = c(
    "M_pura",
    "M_expandida_evaluador",
    "M_cata_individual",
    "M_ia_exploratoria",
    "M_quimica",
    "M_sensorial"
  ),
  hoja = c(
    "01_pura",
    "02_expandida_evaluador",
    "04_sensibilidad_con_ju",
    "05_ia_exploratoria",
    "06_quimica_sola",
    "07_sensorial_sola"
  ),
  stringsAsFactors = FALSE
)

hojas_disponibles <- readxl::excel_sheets(ruta_datos_modelamiento)

escenarios <- escenarios %>%
  mutate(disponible = hoja %in% hojas_disponibles)

if (!all(escenarios$disponible)) {
  stop("Faltan hojas en datos_modelamiento.xlsx: ",
       paste(escenarios$hoja[!escenarios$disponible], collapse = ", "))
}

datos_escenarios <- list()
for (i in seq_len(nrow(escenarios))) {
  esc <- escenarios$escenario[i]
  hoja <- escenarios$hoja[i]
  datos_escenarios[[esc]] <- readxl::read_excel(ruta_datos_modelamiento, sheet = hoja)
}

# Las hojas de M_pura, M_expandida_evaluador y M_cata_individual comparten
# las mismas 32 columnas x_ candidatas (13 GC-FID + 13 JU + 1 Folin + 5
# GC-MS), pero el conjunto de predictores DECLARADO para esos escenarios
# excluye deliberadamente GC-MS (seccion imp:consideraciones): M_pura y
# M_expandida_evaluador usan solo los 13 de GC-FID, y M_cata_individual
# agrega los 13 de JU (26 en total), sin GC-MS. Sin esta exclusion, el
# diagnostico de colinealidad tambien evalua los pares de GC-MS (incluida
# la identidad exacta aroma_fermentativo = esteres + aldehidos, seccion
# res:sensibilidad), inflando el maximo |r| y el conteo de predictores
# usables muy por encima de los 13/26 declarados en el resto de la tesis
# (Tabla 4.1; mismo bug corregido en 13_diagnostico_modelamiento.R,
# 14_modelo_base.R, 15_modelos_small_data.R, 16_validacion_bootstrap_cv.R,
# 16b, 16c, 16d, 16f y 16h). M_ia_exploratoria y M_quimica no se tocan.
if (!is.null(datos_escenarios[["M_pura"]])) {
  cols_excluir <- setdiff(obtener_predictores_x(datos_escenarios[["M_pura"]]), grep("^x_gcfid_", names(datos_escenarios[["M_pura"]]), value = TRUE))
  datos_escenarios[["M_pura"]] <- datos_escenarios[["M_pura"]][, setdiff(names(datos_escenarios[["M_pura"]]), cols_excluir), drop = FALSE]
}
if (!is.null(datos_escenarios[["M_expandida_evaluador"]])) {
  cols_excluir <- setdiff(obtener_predictores_x(datos_escenarios[["M_expandida_evaluador"]]), grep("^x_gcfid_", names(datos_escenarios[["M_expandida_evaluador"]]), value = TRUE))
  datos_escenarios[["M_expandida_evaluador"]] <- datos_escenarios[["M_expandida_evaluador"]][, setdiff(names(datos_escenarios[["M_expandida_evaluador"]]), cols_excluir), drop = FALSE]
}
if (!is.null(datos_escenarios[["M_cata_individual"]])) {
  predictores_gcfid_ju <- grep("^x_gcfid_|^x_ju_", names(datos_escenarios[["M_cata_individual"]]), value = TRUE)
  cols_excluir <- setdiff(obtener_predictores_x(datos_escenarios[["M_cata_individual"]]), predictores_gcfid_ju)
  datos_escenarios[["M_cata_individual"]] <- datos_escenarios[["M_cata_individual"]][, setdiff(names(datos_escenarios[["M_cata_individual"]]), cols_excluir), drop = FALSE]
}

# -------------------------------------------------------------------------
# 3. Cálculo de colinealidad
# -------------------------------------------------------------------------

predictores_resumen <- bind_rows(lapply(names(datos_escenarios), function(esc) {
  resumen_predictores_escenario(datos_escenarios[[esc]], esc)
}))

pares_correlacion <- bind_rows(lapply(names(datos_escenarios), function(esc) {
  calcular_pares_correlacion(datos_escenarios[[esc]], esc)
}))

pares_alta_correlacion <- pares_correlacion %>%
  filter(!is.na(abs_r), abs_r >= umbral_correlacion_alta) %>%
  arrange(escenario, desc(abs_r))

top_correlaciones <- pares_correlacion %>%
  filter(!is.na(abs_r)) %>%
  group_by(escenario) %>%
  slice_max(order_by = abs_r, n = 30, with_ties = FALSE) %>%
  ungroup()

vif_lineal <- bind_rows(lapply(names(datos_escenarios), function(esc) {
  calcular_vif_escenario(datos_escenarios[[esc]], esc)
}))

recomendacion_reduccion <- if (nrow(pares_alta_correlacion) > 0) {
  bind_rows(
    pares_alta_correlacion %>% transmute(escenario, predictor = predictor_1, abs_r),
    pares_alta_correlacion %>% transmute(escenario, predictor = predictor_2, abs_r)
  ) %>%
    group_by(escenario, predictor) %>%
    summarise(
      n_pares_alta_correlacion = n(),
      max_abs_r = round(max(abs_r, na.rm = TRUE), 6),
      .groups = "drop"
    ) %>%
    arrange(escenario, desc(n_pares_alta_correlacion), desc(max_abs_r)) %>%
    mutate(
      recomendacion = case_when(
        max_abs_r >= umbral_correlacion_muy_alta ~ "candidato_a_agrupar_o_regularizar; revisar_si_representa_la_misma_familia_quimica",
        TRUE ~ "mantener_con_cautela; usar_modelos_regularizados_o_pls"
      )
    )
} else {
  data.frame(
    escenario = character(), predictor = character(), n_pares_alta_correlacion = integer(),
    max_abs_r = numeric(), recomendacion = character()
  )
}

resumen_escenarios <- predictores_resumen %>%
  group_by(escenario) %>%
  summarise(
    n_predictores_total = n(),
    n_predictores_usables = sum(usable_correlacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    pares_alta_correlacion %>%
      group_by(escenario) %>%
      summarise(
        n_pares_alta_correlacion = n(),
        n_pares_muy_alta_correlacion = sum(abs_r >= umbral_correlacion_muy_alta, na.rm = TRUE),
        max_abs_r = round(max(abs_r, na.rm = TRUE), 6),
        .groups = "drop"
      ),
    by = "escenario"
  ) %>%
  mutate(
    n_pares_alta_correlacion = ifelse(is.na(n_pares_alta_correlacion), 0, n_pares_alta_correlacion),
    n_pares_muy_alta_correlacion = ifelse(is.na(n_pares_muy_alta_correlacion), 0, n_pares_muy_alta_correlacion),
    max_abs_r = ifelse(is.na(max_abs_r), NA_real_, max_abs_r),
    lectura = case_when(
      n_pares_alta_correlacion == 0 ~ "sin_alerta_fuerte_por_correlacion",
      n_pares_muy_alta_correlacion > 0 ~ "colinealidad_muy_alta; evitar_regresion_lineal_multiple_sin_regularizacion",
      TRUE ~ "colinealidad_alta; preferir_ridge_lasso_elastic_net_o_pls"
    )
  )

# -------------------------------------------------------------------------
# 4. Gráficos
# -------------------------------------------------------------------------

ruta_grafico_barras <- graficar_barras_colinealidad(resumen_escenarios)

rutas_heatmaps <- data.frame(
  escenario = character(),
  ruta_grafico = character(),
  stringsAsFactors = FALSE
)

for (esc in names(datos_escenarios)) {
  ruta_heatmap <- graficar_heatmap_escenario(datos_escenarios[[esc]], esc, pares_alta_correlacion)
  rutas_heatmaps <- bind_rows(
    rutas_heatmaps,
    data.frame(escenario = esc, ruta_grafico = ruta_heatmap, stringsAsFactors = FALSE)
  )
}

graficos_generados <- bind_rows(
  data.frame(
    tipo_grafico = "barras_resumen",
    escenario = "todos",
    ruta_grafico = ruta_grafico_barras,
    uso_recomendado = "Figura resumen para mostrar cantidad de pares altamente colineales por escenario.",
    stringsAsFactors = FALSE
  ),
  rutas_heatmaps %>%
    mutate(
      tipo_grafico = "heatmap_correlacion",
      uso_recomendado = "Figura representativa para visualizar bloques de predictores químicos correlacionados."
    ) %>%
    select(tipo_grafico, escenario, ruta_grafico, uso_recomendado)
)

# -------------------------------------------------------------------------
# 5. Diccionario y exportación
# -------------------------------------------------------------------------

diccionario <- data.frame(
  hoja = c(
    "00_resumen",
    "01_predictores_resumen",
    "02_pares_alta_correlacion",
    "03_top_correlaciones",
    "04_vif_lineal",
    "05_recomendacion_reduccion",
    "06_graficos_generados",
    "07_diccionario"
  ),
  descripcion = c(
    "Resumen por escenario del número de predictores y pares con alta colinealidad.",
    "Disponibilidad, variabilidad y usabilidad de cada predictor químico.",
    "Pares de predictores con |r| igual o superior al umbral definido.",
    "Top 30 de correlaciones absolutas por escenario, aunque no todas superen el umbral.",
    "VIF para modelos lineales cuando existe completitud y tamaño suficiente.",
    "Predictores que aparecen repetidamente en pares altamente correlacionados y recomendación metodológica.",
    "Rutas de las figuras PNG generadas para la tesis o presentación.",
    "Definición de hojas y criterios aplicados."
  ),
  criterio = c(
    paste0("Alta colinealidad: |r| >= ", umbral_correlacion_alta, "; muy alta: |r| >= ", umbral_correlacion_muy_alta, "."),
    "Un predictor es usable si tiene al menos 3 valores no vacíos, 2 valores distintos y desviación estándar positiva.",
    "Correlación de Pearson con uso pairwise.complete.obs.",
    "Ordenado por correlación absoluta descendente dentro de cada escenario.",
    "VIF calculado solo con predictores de completitud >= 80% y filas completas suficientes.",
    "Alta frecuencia indica variables candidatas a agrupación, selección o regularización.",
    "Los heatmaps muestran predictores involucrados en colinealidad alta o, si no existen, los más variables.",
    "Archivo generado por scripts/R/modelamiento/13b_colinealidad_modelamiento.R."
  ),
  stringsAsFactors = FALSE
)

lista_salida <- list(
  "00_resumen" = resumen_escenarios,
  "01_predictores_resumen" = predictores_resumen,
  "02_pares_alta_correlacion" = pares_alta_correlacion,
  "03_top_correlaciones" = top_correlaciones,
  "04_vif_lineal" = vif_lineal,
  "05_recomendacion_reduccion" = recomendacion_reduccion,
  "06_graficos_generados" = graficos_generados,
  "07_diccionario" = diccionario
)

guardar_excel_seguro(lista_salida, ruta_salida_excel)

cat("\nProceso terminado correctamente.\n")
cat("Archivo generado:\n", ruta_salida_excel, "\n")
cat("Gráficos generados en:\n", dir_outputs_colinealidad, "\n")
