# ============================================================
# 10_analisis_quimico.R
# Analisis exploratorio de datos (EDA) quimico
# Tesis whisky chileno - modelamiento quimico-sensorial
#
# Sigue el esquema clasico de EDA (Guia practica de introduccion
# al Analisis Exploratorio de Datos, datos.gob.es 2021) adaptado
# a datos quimicos con bloques analiticos no solapados:
#   1. Analisis descriptivo (resumen + histogramas)
#   2. Ajuste/verificacion de tipos de variable
#   3. Deteccion de datos ausentes (tabla + mapa de completitud)
#   4. Deteccion de valores atipicos (boxplots, regla IQR) -
#      solo se documentan, no se eliminan ni imputan (los datos
#      reales no se alteran, ver advertencia metodologica).
#   5. Analisis de correlacion (tabla + heatmap) y PCA por grupo
#      quimico (GC-FID, GC-MS, JU, fenoles totales), porque las
#      tecnicas no comparten variables entre si.
#
# Analisis adicionales (2026-07-08), justificados por el contexto
# de small data (bloques con apenas 4-9 unidades analiticas):
#   4b. Test Q de Dixon por variable (paquete "outliers"), mas
#       apropiado que la regla IQR para muestras chicas (n entre
#       3 y 30) y estandar en quimica analitica cuantitativa.
#   5b. T2 de Hotelling y Q-residuals (SPE) sobre el PCA de cada
#       grupo quimico: diagnostico multivariado que complementa
#       el Dixon/IQR univariado, porque una combinacion de valores
#       puede ser atipica aunque cada variable se vea normal por
#       separado.
# ============================================================

source("scripts/R/procesamiento/00_resumen_datos_iniciales.R")

paquetes_exploratorio <- c("readxl", "dplyr", "tidyr", "stringr", "writexl", "ggplot2", "outliers")
paquetes_faltantes <- paquetes_exploratorio[
  !vapply(paquetes_exploratorio, requireNamespace, logical(1), quietly = TRUE)
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
library(outliers)

cat("\nANALISIS EXPLORATORIO QUIMICO\n")

# ------------------------------------------------------------
# Rutas

ruta_quimica <- file.path(dir_procesamiento, "quimica_unidades.xlsx")
ruta_salida_base <- file.path(dir_procesamiento, "analisis_quimico.xlsx")

dir_outputs_exploratorio <- file.path(dir_proyecto, "outputs", "exploratorio", "quimico")
dir.create(dir_outputs_exploratorio, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(ruta_quimica)) {
  stop(paste("No existe el archivo quimico:", ruta_quimica))
}

# ------------------------------------------------------------
# Estilo comun para los graficos

color_acento <- "#2E5395"
color_alerta <- "#B00000"

tema_eda <- theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
    strip.text = element_text(size = 7)
  )

# ------------------------------------------------------------
# Funciones

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
          "Probablemente esta abierto. Se guardara copia en:",
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

guardar_grafico_seguro <- function(plot, nombre_archivo, ancho = 9, alto = 6) {
  ruta <- file.path(dir_outputs_exploratorio, nombre_archivo)
  tryCatch(
    {
      ggsave(ruta, plot = plot, width = ancho, height = alto, dpi = 150)
      ruta
    },
    error = function(e) {
      message("No se pudo guardar el grafico ", nombre_archivo, ": ", conditionMessage(e))
      NA_character_
    }
  )
}

clasificar_grupo_quimico <- function(bloque_id) {
  case_when(
    bloque_id %in% c(
      "gcfid_constanza_septiembre",
      "gcfid_constanza_enero",
      "alexandra_260603_pilsner_gcfid"
    ) ~ "GCFID",
    bloque_id %in% c(
      "gcms_noviembre",
      "gcms_constanza_comerciales",
      "gcms_constanza_experimentales"
    ) ~ "GCMS",
    bloque_id == "ju_260507_quimica_cepas" ~ "JU",
    bloque_id == "alexandra_260603_fenoles_totales" ~ "Fenoles_totales",
    TRUE ~ "otro"
  )
}

obtener_vars_quimicas <- function(df) {
  vars <- names(df)[str_detect(names(df), "^(x_|ctrl_|n_)")]
  vars[sapply(df[vars], function(x) any(!is.na(suppressWarnings(as.numeric(x)))))]
}

preparar_numerica <- function(df, vars) {
  x <- df[, vars, drop = FALSE]
  x[] <- lapply(x, function(z) suppressWarnings(as.numeric(z)))

  vars_validas <- names(x)[colSums(!is.na(x)) > 0]
  x <- x[, vars_validas, drop = FALSE]

  vars_con_varianza <- names(x)[sapply(x, function(z) {
    z <- z[!is.na(z)]
    length(unique(z)) > 1
  })]

  x[, vars_con_varianza, drop = FALSE]
}

calcular_pca <- function(df, vars, grupo_pca) {
  x <- preparar_numerica(df, vars)

  if (nrow(x) < 3 || ncol(x) < 2) {
    return(list(
      resumen = data.frame(
        grupo_pca = grupo_pca, estado = "no_calculado",
        n_filas = nrow(x), n_variables = ncol(x),
        pc1_varianza = NA_real_, pc2_varianza = NA_real_, pc1_pc2_varianza = NA_real_,
        motivo = "Se requieren al menos 3 filas y 2 variables con variabilidad.",
        stringsAsFactors = FALSE
      ),
      scores = data.frame(), cargas = data.frame()
    ))
  }

  for (v in names(x)) {
    x[[v]][is.na(x[[v]])] <- mean(x[[v]], na.rm = TRUE)
  }

  pca <- tryCatch(prcomp(x, center = TRUE, scale. = TRUE), error = function(e) NULL)

  if (is.null(pca)) {
    return(list(
      resumen = data.frame(
        grupo_pca = grupo_pca, estado = "no_calculado",
        n_filas = nrow(x), n_variables = ncol(x),
        pc1_varianza = NA_real_, pc2_varianza = NA_real_, pc1_pc2_varianza = NA_real_,
        motivo = "Error al calcular PCA.",
        stringsAsFactors = FALSE
      ),
      scores = data.frame(), cargas = data.frame()
    ))
  }

  var_exp <- pca$sdev^2 / sum(pca$sdev^2)
  t2q <- calcular_t2_q(pca, n_comp = 2)

  resumen <- data.frame(
    grupo_pca = grupo_pca, estado = "calculado",
    n_filas = nrow(x), n_variables = ncol(x),
    pc1_varianza = ifelse(length(var_exp) >= 1, var_exp[1], NA_real_),
    pc2_varianza = ifelse(length(var_exp) >= 2, var_exp[2], NA_real_),
    pc1_pc2_varianza = ifelse(length(var_exp) >= 2, sum(var_exp[1:2]), NA_real_),
    t2_n_comp = t2q$n_comp, t2_limite = t2q$t2_lim, q_limite = t2q$q_lim,
    motivo = NA_character_,
    stringsAsFactors = FALSE
  )

  scores <- as.data.frame(pca$x)
  scores <- scores[, intersect(c("PC1", "PC2", "PC3"), names(scores)), drop = FALSE]
  scores$unidad_analitica_id <- df$unidad_analitica_id
  scores$bloque_id <- df$bloque_id
  scores$muestra_base <- df$muestra_base
  scores$replica_id <- df$replica_id
  scores$grupo_pca <- grupo_pca
  scores$t2_hotelling <- t2q$t2
  scores$q_residual <- t2q$q
  scores$excede_t2 <- if (is.na(t2q$t2_lim)) NA else t2q$t2 > t2q$t2_lim
  scores$excede_q <- if (is.na(t2q$q_lim)) NA else t2q$q > t2q$q_lim
  scores <- scores %>% select(grupo_pca, unidad_analitica_id, bloque_id, muestra_base, replica_id, everything())

  cargas <- as.data.frame(pca$rotation)
  cargas <- cargas[, intersect(c("PC1", "PC2", "PC3"), names(cargas)), drop = FALSE]
  cargas$variable_quimica <- rownames(cargas)
  cargas$grupo_pca <- grupo_pca
  rownames(cargas) <- NULL
  cargas <- cargas %>% select(grupo_pca, variable_quimica, everything())

  list(resumen = resumen, scores = scores, cargas = cargas, t2_lim = t2q$t2_lim, q_lim = t2q$q_lim)
}

calcular_correlaciones_altas <- function(df, vars, grupo_correlacion) {
  x <- preparar_numerica(df, vars)
  if (nrow(x) < 3 || ncol(x) < 2) return(data.frame())

  for (v in names(x)) x[[v]][is.na(x[[v]])] <- mean(x[[v]], na.rm = TRUE)

  cor_mat <- suppressWarnings(cor(x, use = "pairwise.complete.obs", method = "pearson"))
  cor_df <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
  names(cor_df) <- c("variable_1", "variable_2", "correlacion")

  cor_df %>%
    filter(variable_1 != variable_2) %>%
    mutate(
      par = ifelse(variable_1 < variable_2, paste(variable_1, variable_2, sep = "***"), paste(variable_2, variable_1, sep = "***")),
      abs_correlacion = abs(correlacion),
      grupo_correlacion = grupo_correlacion
    ) %>%
    distinct(par, .keep_all = TRUE) %>%
    filter(abs_correlacion >= 0.8) %>%
    select(grupo_correlacion, variable_1, variable_2, correlacion, abs_correlacion) %>%
    arrange(grupo_correlacion, desc(abs_correlacion))
}

detectar_outliers_iqr <- function(df_largo) {
  df_largo %>%
    filter(!is.na(valor)) %>%
    group_by(variable_quimica) %>%
    filter(n() >= 4) %>%
    mutate(
      q1 = quantile(valor, 0.25, na.rm = TRUE),
      q3 = quantile(valor, 0.75, na.rm = TRUE),
      iqr = q3 - q1,
      limite_inferior = q1 - 1.5 * iqr,
      limite_superior = q3 + 1.5 * iqr,
      es_outlier = valor < limite_inferior | valor > limite_superior
    ) %>%
    ungroup() %>%
    filter(es_outlier) %>%
    select(
      unidad_analitica_id, bloque_id, muestra_base, replica_id,
      variable_quimica, valor, limite_inferior, limite_superior
    ) %>%
    arrange(variable_quimica, valor)
}

calcular_dixon_variable <- function(valores, ids = NULL, alpha = 0.05) {
  if (is.null(ids)) ids <- as.character(seq_along(valores))
  ids <- ids[!is.na(valores)]
  valores <- valores[!is.na(valores)]
  n <- length(valores)

  if (n < 3 || n > 30 || length(unique(valores)) < 2) {
    return(data.frame(
      n = n, valor_sospechoso = NA_real_, unidad_sospechosa = NA_character_, direccion = NA_character_,
      estadistico_q = NA_real_, p_value = NA_real_, es_outlier_dixon = NA,
      motivo = "n fuera de rango [3,30] o sin variabilidad", stringsAsFactors = FALSE
    ))
  }

  resultado <- tryCatch(outliers::dixon.test(valores), error = function(e) NULL)
  if (is.null(resultado)) {
    return(data.frame(
      n = n, valor_sospechoso = NA_real_, unidad_sospechosa = NA_character_, direccion = NA_character_,
      estadistico_q = NA_real_, p_value = NA_real_, es_outlier_dixon = NA,
      motivo = "error al calcular dixon.test", stringsAsFactors = FALSE
    ))
  }

  es_maximo <- grepl("highest", resultado$alternative)
  pos_sospechoso <- if (es_maximo) which.max(valores) else which.min(valores)

  data.frame(
    n = n, valor_sospechoso = valores[pos_sospechoso], unidad_sospechosa = ids[pos_sospechoso],
    direccion = if (es_maximo) "mas_alto" else "mas_bajo",
    estadistico_q = unname(resultado$statistic), p_value = resultado$p.value,
    es_outlier_dixon = resultado$p.value < alpha, motivo = NA_character_,
    stringsAsFactors = FALSE
  )
}

# T2 de Hotelling y Q-residuals (SPE) sobre un PCA ya calculado con prcomp().
# n_comp = numero de componentes "retenidos" (el plano visualizado en el biplot);
# lo que queda fuera de esos componentes es lo que mide el Q-residual.
calcular_t2_q <- function(pca, n_comp = 2, alpha = 0.05) {
  n <- nrow(pca$x)
  k_total <- ncol(pca$x)
  n_comp <- min(n_comp, k_total)

  x_reconstruido_total <- pca$x %*% t(pca$rotation)
  scores_a <- pca$x[, seq_len(n_comp), drop = FALSE]
  rotation_a <- pca$rotation[, seq_len(n_comp), drop = FALSE]
  reconstruido_a <- scores_a %*% t(rotation_a)
  q_resid <- rowSums((x_reconstruido_total - reconstruido_a)^2)

  lambda_a <- (pca$sdev[seq_len(n_comp)])^2
  lambda_a[lambda_a <= 1e-12] <- NA_real_
  t2 <- rowSums(sweep(scores_a^2, 2, lambda_a, "/"), na.rm = TRUE)

  t2_lim <- if (n > n_comp) {
    (n_comp * (n - 1) / (n - n_comp)) * qf(1 - alpha, n_comp, n - n_comp)
  } else {
    NA_real_
  }

  lambda_resid <- (pca$sdev[seq_len(k_total)])^2
  lambda_resid <- lambda_resid[(n_comp + 1):k_total]
  lambda_resid <- lambda_resid[!is.na(lambda_resid) & lambda_resid > 1e-10]

  q_lim <- NA_real_
  if (length(lambda_resid) == 0) {
    q_lim <- 0
  } else {
    theta1 <- sum(lambda_resid)
    theta2 <- sum(lambda_resid^2)
    theta3 <- sum(lambda_resid^3)
    if (theta1 > 0 && theta2 > 0) {
      h0 <- 1 - (2 * theta1 * theta3) / (3 * theta2^2)
      if (is.finite(h0) && h0 != 0) {
        c_alpha <- qnorm(1 - alpha)
        termino <- (c_alpha * sqrt(2 * theta2 * h0^2) / theta1) + 1 + (theta2 * h0 * (h0 - 1)) / theta1^2
        if (is.finite(termino) && termino > 0) q_lim <- theta1 * termino^(1 / h0)
      }
    }
  }

  list(t2 = t2, q = q_resid, t2_lim = t2_lim, q_lim = q_lim, n_comp = n_comp)
}

# ------------------------------------------------------------
# ETAPA 1a: Lectura y analisis descriptivo

quimica_wide <- readxl::read_excel(ruta_quimica, sheet = "01_quimica_wide") %>%
  as.data.frame(stringsAsFactors = FALSE)

quimica_wide <- quimica_wide %>%
  mutate(
    tiene_cata_confirmada = estandarizar_logico(tiene_cata_confirmada),
    replica_id = as.integer(replica_id),
    grupo_quimico = clasificar_grupo_quimico(bloque_id)
  )

vars_quimicas <- obtener_vars_quimicas(quimica_wide)

resumen_global <- data.frame(
  indicador = c(
    "Unidad de analisis quimica", "Unidades analiticas quimicas totales",
    "Unidades con cata confirmada", "Unidades sin cata confirmada",
    "Bloques quimicos", "Grupos quimicos (tecnica analitica)",
    "Variables quimicas detectadas", "Uso de este analisis"
  ),
  valor = c(
    "Unidad analitica quimica", nrow(quimica_wide),
    sum(quimica_wide$tiene_cata_confirmada == TRUE, na.rm = TRUE),
    sum(quimica_wide$tiene_cata_confirmada == FALSE, na.rm = TRUE),
    n_distinct(quimica_wide$bloque_id), n_distinct(quimica_wide$grupo_quimico),
    length(vars_quimicas),
    "Analisis exploratorio de datos (EDA) no supervisado. No usa targets sensoriales."
  ),
  stringsAsFactors = FALSE
)

quimica_wide$n_variables_quimicas_disponibles <- rowSums(!is.na(quimica_wide[, vars_quimicas, drop = FALSE]))

resumen_bloques <- quimica_wide %>%
  group_by(grupo_quimico, bloque_id, tecnica_quimica) %>%
  summarise(
    n_unidades_analiticas = n(), n_muestras_base = n_distinct(muestra_base),
    n_con_cata = sum(tiene_cata_confirmada == TRUE, na.rm = TRUE),
    n_sin_cata = sum(tiene_cata_confirmada == FALSE, na.rm = TRUE),
    n_variables_promedio = round(mean(n_variables_quimicas_disponibles), 2),
    .groups = "drop"
  ) %>%
  arrange(grupo_quimico, bloque_id)

quimica_largo <- quimica_wide %>%
  select(unidad_analitica_id, grupo_quimico, bloque_id, tecnica_quimica, muestra_base, replica_id, tiene_cata_confirmada, all_of(vars_quimicas)) %>%
  pivot_longer(cols = all_of(vars_quimicas), names_to = "variable_quimica", values_to = "valor") %>%
  mutate(valor = suppressWarnings(as.numeric(valor)))

descriptivos_variables <- quimica_largo %>%
  group_by(grupo_quimico, variable_quimica) %>%
  summarise(
    n_total = n(), n_con_valor = sum(!is.na(valor)), n_sin_valor = sum(is.na(valor)),
    media = mean(valor, na.rm = TRUE), desviacion_estandar = sd(valor, na.rm = TRUE),
    minimo = min(valor, na.rm = TRUE), mediana = median(valor, na.rm = TRUE), maximo = max(valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(c(media, desviacion_estandar, minimo, mediana, maximo), ~ ifelse(is.infinite(.x) | is.nan(.x), NA_real_, .x)),
    proporcion_con_valor = round(n_con_valor / n_total, 4)
  ) %>%
  arrange(desc(proporcion_con_valor), variable_quimica)

grupos_quimicos <- sort(unique(quimica_wide$grupo_quimico))

# Nombre presentable del grupo quimico (para titulos de figuras)
nombre_bonito_grupo <- function(g) {
  dplyr::case_when(
    g == "GCFID" ~ "GC-FID",
    g == "GCMS" ~ "GC-MS",
    g == "JU" ~ "JU (fermentacion)",
    g == "Fenoles_totales" ~ "Fenoles totales (Folin)",
    TRUE ~ g
  )
}

# Limpia un nombre tecnico de variable quimica (ej. "x_gcfid_o_cresol_ppm")
# a una etiqueta presentable con su unidad (ej. "O-cresol (ppm)"), para usar
# en ejes/facetas de las figuras en vez del nombre de columna crudo.
limpiar_nombre_variable_quimica <- function(x) {
  x <- gsub("^x_gcfid_|^x_gcms_|^x_ju_|^x_folin_|^ctrl_gcms_|^n_", "", x)

  unidad <- dplyr::case_when(
    grepl("_ppm$", x) ~ "ppm",
    grepl("_pct$|_percent$", x) ~ "%",
    grepl("_g_l$", x) ~ "g/L",
    grepl("_ug_ag_ml$", x) ~ "µg AG/mL",
    x == "compuestos_detectados" ~ "conteo",
    TRUE ~ NA_character_
  )

  x <- gsub("_ppm$|_pct$|_percent$|_g_l$|_ug_ag_ml$", "", x)
  x <- gsub("_faci$", " FACI", x)
  x <- gsub("_", " ", x)
  x <- trimws(x)
  x <- gsub("^o cresol", "o-cresol", x)
  x <- gsub("^p cresol", "p-cresol", x)
  x <- gsub("^4 ethyl", "4-etil", x)

  x <- dplyr::case_when(
    x == "esteres" ~ "Ésteres",
    x == "fenolicos" ~ "Fenólicos",
    x == "aldehidos" ~ "Aldehídos",
    x == "area valida" ~ "Área válida",
    x == "siloxano" ~ "Siloxanos",
    TRUE ~ stringr::str_to_sentence(x)
  )

  ifelse(!is.na(unidad), paste0(x, " (", unidad, ")"), x)
}

# Etapa 1b: histogramas de distribucion, un panel por grupo quimico
rutas_histogramas <- character(0)
for (g in grupos_quimicos) {
  datos_g <- quimica_largo %>%
    filter(grupo_quimico == g, !is.na(valor)) %>%
    mutate(variable_quimica_bonita = limpiar_nombre_variable_quimica(variable_quimica))
  if (nrow(datos_g) == 0) next

  p <- ggplot(datos_g, aes(x = valor)) +
    geom_histogram(bins = 15, fill = color_acento, color = "white") +
    facet_wrap(~variable_quimica_bonita, scales = "free") +
    tema_eda +
    labs(
      title = paste0("Distribucion de variables quimicas: ", nombre_bonito_grupo(g)),
      x = "Valor de la variable", y = "Frecuencia"
    )

  rutas_histogramas <- c(
    rutas_histogramas,
    guardar_grafico_seguro(p, paste0("01_histogramas_", g, ".png"), ancho = 10, alto = 7)
  )
}

# ------------------------------------------------------------
# ETAPA 2: Ajuste / verificacion de tipos de variable

tipos_variables <- data.frame(
  variable_quimica = vars_quimicas,
  clase_r = sapply(quimica_wide[vars_quimicas], function(x) class(x)[1]),
  grupo_variable = case_when(
    str_detect(vars_quimicas, "^x_gcfid") ~ "GC-FID",
    str_detect(vars_quimicas, "^x_gcms|^ctrl_gcms|^n_compuestos") ~ "GC-MS",
    str_detect(vars_quimicas, "^x_ju") ~ "JU",
    str_detect(vars_quimicas, "^x_folin") ~ "Fenoles totales",
    TRUE ~ "otro"
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# ETAPA 3: Datos ausentes (completitud) + mapa visual

completitud_variables <- descriptivos_variables %>%
  select(grupo_quimico, variable_quimica, n_total, n_con_valor, n_sin_valor, proporcion_con_valor) %>%
  arrange(desc(proporcion_con_valor), variable_quimica)

completitud_bloque_variable <- quimica_wide %>%
  group_by(bloque_id) %>%
  summarise(across(all_of(vars_quimicas), ~ mean(!is.na(.x))), .groups = "drop") %>%
  pivot_longer(cols = -bloque_id, names_to = "variable_quimica", values_to = "pct_completo")

p_completitud <- ggplot(completitud_bloque_variable, aes(x = variable_quimica, y = bloque_id, fill = pct_completo)) +
  geom_tile(color = "white", linewidth = 0.7) +
  scale_fill_gradient(low = "#F4CCCC", high = color_acento, limits = c(0, 1), labels = scales::percent) +
  tema_eda +
  theme(
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 11),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 11, color = "grey35"),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  ) +
  labs(
    title = "Mapa de calor de completitud de variables quimicas por bloque analitico",
    subtitle = "Los vacios son estructurales: cada tecnica mide variables distintas, no errores de captura.",
    x = NULL, y = NULL, fill = "% completo"
  )

# Imagen ampliada (18x9 in) para que las 39 etiquetas de variables sean
# legibles a tamano impreso, manteniendo la paleta y el estilo original
# (rediseno de tamano/tipografia, Figura 3.2, observacion de comision).
ruta_completitud <- guardar_grafico_seguro(p_completitud, "02_mapa_completitud.png", ancho = 18, alto = 9)

# ------------------------------------------------------------
# ETAPA 4: Deteccion de valores atipicos (outliers) - solo se documentan

outliers_quimicos <- detectar_outliers_iqr(quimica_largo)

rutas_boxplots <- character(0)
for (g in grupos_quimicos) {
  datos_g <- quimica_largo %>%
    filter(grupo_quimico == g, !is.na(valor)) %>%
    mutate(variable_quimica_bonita = limpiar_nombre_variable_quimica(variable_quimica))
  if (nrow(datos_g) == 0) next

  p <- ggplot(datos_g, aes(x = variable_quimica_bonita, y = valor)) +
    geom_boxplot(fill = color_acento, alpha = 0.5, outlier.colour = color_alerta) +
    facet_wrap(~variable_quimica_bonita, scales = "free") +
    tema_eda +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(
      title = paste0("Boxplots y valores atipicos: ", nombre_bonito_grupo(g)),
      x = NULL, y = "Valor de la variable"
    )

  rutas_boxplots <- c(
    rutas_boxplots,
    guardar_grafico_seguro(p, paste0("03_boxplots_", g, ".png"), ancho = 10, alto = 7)
  )
}

resumen_outliers <- outliers_quimicos %>%
  count(variable_quimica, name = "n_outliers") %>%
  arrange(desc(n_outliers))

# ------------------------------------------------------------
# ETAPA 4b: Test Q de Dixon por variable (complementa la regla IQR)
# La regla IQR esta mal calibrada para n tan chico (Hoaglin, Iglewicz &
# Tukey, 1986, JASA); el test Q de Dixon (1950/1953) es el estandar en
# quimica analitica cuantitativa para n entre 3 y 30, que es el rango en
# el que caen casi todos los bloques quimicos de este proyecto.

dixon_outliers <- quimica_largo %>%
  filter(!is.na(valor)) %>%
  group_by(grupo_quimico, variable_quimica) %>%
  summarise(resultado = list(calcular_dixon_variable(valor, ids = unidad_analitica_id)), .groups = "drop") %>%
  tidyr::unnest(resultado) %>%
  filter(!is.na(es_outlier_dixon)) %>%
  arrange(desc(es_outlier_dixon), p_value)

resumen_dixon <- dixon_outliers %>%
  count(es_outlier_dixon, name = "n_variables") %>%
  mutate(es_outlier_dixon = ifelse(es_outlier_dixon, "outlier_detectado", "sin_outlier"))

# ------------------------------------------------------------
# ETAPA 5: Correlaciones y PCA por grupo quimico

lista_pca <- list()
lista_correlaciones <- list()
rutas_correlacion <- character(0)
rutas_biplot <- character(0)
rutas_t2q <- character(0)

for (g in grupos_quimicos) {
  datos_g <- quimica_wide %>% filter(grupo_quimico == g)
  vars_g <- vars_quimicas[colSums(!is.na(datos_g[, vars_quimicas, drop = FALSE])) > 0]

  lista_pca[[g]] <- calcular_pca(df = datos_g, vars = vars_g, grupo_pca = g)
  lista_correlaciones[[g]] <- calcular_correlaciones_altas(df = datos_g, vars = vars_g, grupo_correlacion = g)

  x_g <- preparar_numerica(datos_g, vars_g)
  if (ncol(x_g) >= 2 && nrow(x_g) >= 3) {
    for (v in names(x_g)) x_g[[v]][is.na(x_g[[v]])] <- mean(x_g[[v]], na.rm = TRUE)
    cor_mat <- suppressWarnings(cor(x_g, use = "pairwise.complete.obs"))
    cor_largo <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
    names(cor_largo) <- c("variable_1", "variable_2", "correlacion")

    p_cor <- ggplot(cor_largo, aes(x = variable_1, y = variable_2, fill = correlacion)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.2f", correlacion)), size = 2) +
      scale_fill_gradient2(low = color_alerta, mid = "white", high = color_acento, midpoint = 0, limits = c(-1, 1)) +
      tema_eda +
      theme(axis.text.y = element_text(size = 6)) +
      labs(title = paste0("Correlacion entre variables quimicas - ", g), x = NULL, y = NULL, fill = "r")

    rutas_correlacion <- c(
      rutas_correlacion,
      guardar_grafico_seguro(p_cor, paste0("04_correlacion_", g, ".png"), ancho = 9, alto = 8)
    )
  }

  pca_g <- lista_pca[[g]]
  if (pca_g$resumen$estado[1] == "calculado" && all(c("PC1", "PC2") %in% names(pca_g$scores))) {
    scores_g <- pca_g$scores
    cargas_g <- pca_g$cargas

    escala <- 1
    max_score <- max(abs(c(scores_g$PC1, scores_g$PC2)), na.rm = TRUE)
    max_carga <- max(abs(c(cargas_g$PC1, cargas_g$PC2)), na.rm = TRUE)
    if (is.finite(max_score) && is.finite(max_carga) && max_carga > 0) {
      escala <- (max_score / max_carga) * 0.8
    }

    p_biplot <- ggplot() +
      geom_hline(yintercept = 0, color = "grey80") +
      geom_vline(xintercept = 0, color = "grey80") +
      geom_point(data = scores_g, aes(x = PC1, y = PC2), color = color_acento, size = 2) +
      geom_segment(
        data = cargas_g, aes(x = 0, y = 0, xend = PC1 * escala, yend = PC2 * escala),
        arrow = arrow(length = unit(0.2, "cm")), color = color_alerta
      ) +
      geom_text(
        data = cargas_g, aes(x = PC1 * escala * 1.1, y = PC2 * escala * 1.1, label = variable_quimica),
        size = 2.5, color = color_alerta
      ) +
      scale_x_continuous(expand = expansion(mult = 0.18)) +
      scale_y_continuous(expand = expansion(mult = 0.18)) +
      tema_eda +
      theme(axis.text.x = element_text(angle = 0)) +
      labs(
        title = paste0("Biplot PCA - ", g),
        x = paste0("PC1 (", round(pca_g$resumen$pc1_varianza[1] * 100, 1), "%)"),
        y = paste0("PC2 (", round(pca_g$resumen$pc2_varianza[1] * 100, 1), "%)")
      )

    rutas_biplot <- c(
      rutas_biplot,
      guardar_grafico_seguro(p_biplot, paste0("05_biplot_pca_", g, ".png"), ancho = 8, alto = 7)
    )

    if (!is.na(pca_g$t2_lim) && !is.na(pca_g$q_lim) && all(c("t2_hotelling", "q_residual") %in% names(scores_g))) {
      scores_g <- scores_g %>%
        mutate(es_outlier_t2q = (excede_t2 %in% TRUE) | (excede_q %in% TRUE))

      p_t2q <- ggplot(scores_g, aes(x = t2_hotelling, y = q_residual)) +
        geom_vline(xintercept = pca_g$t2_lim, linetype = "dashed", color = color_alerta) +
        geom_hline(yintercept = pca_g$q_lim, linetype = "dashed", color = color_alerta) +
        geom_point(aes(color = es_outlier_t2q), size = 2.3) +
        geom_text(
          data = scores_g %>% filter(es_outlier_t2q),
          aes(label = unidad_analitica_id), size = 2.3, vjust = -0.8, color = color_alerta
        ) +
        scale_color_manual(values = c(`TRUE` = color_alerta, `FALSE` = color_acento), guide = "none") +
        scale_x_continuous(expand = expansion(mult = 0.22)) +
        scale_y_continuous(expand = expansion(mult = 0.12)) +
        tema_eda +
        theme(axis.text.x = element_text(angle = 0)) +
        labs(
          title = paste0("Distancia T2 - Q residual - ", g),
          subtitle = "Lineas punteadas = limites al 95% (Hotelling T2 / Jackson-Mudholkar Q)",
          x = "T2 de Hotelling", y = "Q-residual (SPE)"
        )

      rutas_t2q <- c(
        rutas_t2q,
        guardar_grafico_seguro(p_t2q, paste0("06_distancia_t2_q_", g, ".png"), ancho = 8, alto = 6)
      )
    }
  }
}

pca_resumen <- bind_rows(lapply(lista_pca, function(x) x$resumen))
pca_scores <- bind_rows(lapply(lista_pca, function(x) x$scores))
pca_cargas <- bind_rows(lapply(lista_pca, function(x) x$cargas))
correlaciones_altas <- bind_rows(lista_correlaciones)

t2q_outliers <- pca_scores %>%
  filter(excede_t2 %in% TRUE | excede_q %in% TRUE) %>%
  select(grupo_pca, unidad_analitica_id, bloque_id, muestra_base, t2_hotelling, excede_t2, q_residual, excede_q) %>%
  arrange(grupo_pca, desc(t2_hotelling))

# ------------------------------------------------------------
# Diccionario y notas metodologicas

diccionario_variables <- tipos_variables %>% select(variable_quimica, grupo_variable, clase_r)

notas_metodologicas <- data.frame(
  punto = c(
    "1. Analisis descriptivo", "2. Ajuste de tipos", "3. Datos ausentes",
    "4. Valores atipicos (IQR)", "4b. Valores atipicos (Dixon)", "5. Correlacion y PCA",
    "5b. T2 de Hotelling y Q-residuals", "Separacion por grupo quimico", "Uso del analisis"
  ),
  descripcion = c(
    "Resumen global, por bloque y descriptivos (media, mediana, sd, min, max) por variable, mas histogramas de distribucion por grupo quimico.",
    "Todas las variables x_/ctrl_/n_ se verifican como numericas (conversion aplicada en la extraccion, script 02).",
    "Se calcula completitud por variable y bloque, y se visualiza como mapa de calor. Los vacios son estructurales (tecnicas no solapadas), no errores.",
    "Se detectan outliers con la regla IQR (Q1-1.5*IQR / Q3+1.5*IQR) por variable. Se documentan en la hoja de outliers y se marcan en los boxplots, pero NO se eliminan ni imputan: los datos reales no se alteran.",
    "La regla IQR esta mal calibrada para n tan chico (Hoaglin, Iglewicz & Tukey, 1986). Se complementa con el test Q de Dixon (1950/1953; paquete 'outliers'), el estandar en quimica analitica cuantitativa para n entre 3 y 30 -el rango de casi todos los bloques quimicos de este proyecto. Tampoco se elimina nada: solo se documenta.",
    "Correlaciones >= 0.8 y PCA se calculan por grupo quimico (GC-FID, GC-MS, JU, fenoles totales) porque las tecnicas no comparten variables entre si; un PCA/correlacion global no es interpretable.",
    "Diagnostico multivariado (Jackson & Mudholkar, 1979; Jackson, 1991) calculado sobre el mismo PCA de cada grupo (2 componentes, el plano del biplot): T2 mide que tan lejos esta una unidad analitica del centro dentro de ese plano; Q-residual mide que tan mal la explica ese plano (variacion fuera de PC1-PC2). Complementa el IQR/Dixon univariado porque una combinacion de valores puede ser atipica aunque cada variable se vea normal por separado.",
    "GC-FID, GC-MS, JU y fenoles totales se analizan por separado en todas las etapas por la misma razon: bloques analiticos no solapados.",
    "Este analisis es exploratorio (EDA) y no usa targets sensoriales; es insumo para decisiones de preparacion de datos, no para modelamiento supervisado."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Exportar

ruta_salida <- guardar_excel_seguro(
  hojas = list(
    "00_resumen" = resumen_global,
    "01_resumen_bloques" = resumen_bloques,
    "02_descriptivos" = descriptivos_variables,
    "03_tipos_variables" = tipos_variables,
    "04_completitud" = completitud_variables,
    "05_outliers_detectados" = outliers_quimicos,
    "06_resumen_outliers" = resumen_outliers,
    "07_dixon_outliers" = dixon_outliers,
    "08_resumen_dixon" = resumen_dixon,
    "09_correlaciones_altas" = correlaciones_altas,
    "10_pca_resumen" = pca_resumen,
    "11_pca_scores" = pca_scores,
    "12_pca_cargas" = pca_cargas,
    "13_t2q_outliers" = t2q_outliers,
    "14_diccionario_variables" = diccionario_variables,
    "15_notas" = notas_metodologicas
  ),
  ruta_salida = ruta_salida_base
)

cat("\nANALISIS QUIMICO FINALIZADO\n")

cat("\nArchivo generado:\n")
cat(ruta_salida, "\n")

cat("\nGraficos generados en:\n")
cat(dir_outputs_exploratorio, "\n")

cat("\nResumen:\n")
print(resumen_global)

cat("\nOutliers detectados por variable (IQR):\n")
print(resumen_outliers)

cat("\nOutliers detectados por variable (Dixon Q):\n")
print(resumen_dixon)

cat("\nPCA resumen (incluye limites T2/Q):\n")
print(pca_resumen)

cat("\nUnidades analiticas que exceden T2 y/o Q-residual:\n")
print(t2q_outliers)

cat("\nProceso terminado correctamente.\n")
