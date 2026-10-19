# Análisis químico-sensorial de la calidad del whisky chileno

Código de respaldo de la memoria *Análisis químico-sensorial de la calidad del whisky
chileno mediante técnicas de aprendizaje supervisado* (Alexandra Navarro Calderón,
Departamento de Ingeniería Informática, Universidad de Santiago de Chile).

Este documento corresponde al Anexo H de la memoria ("Reproducibilidad: código,
entorno y ejecución") y resume la estructura del proyecto y los pasos necesarios para
reproducir los resultados reportados.

Reporte navegable de las tres etapas del análisis (procesamiento, análisis exploratorio y
modelamiento), publicado en RPubs: <https://rpubs.com/Alexandra_Navarro/1446879>. El
archivo fuente es [`reportes_rpubs/reporte_completo.Rmd`](reportes_rpubs/reporte_completo.Rmd).

## Datos

Este repositorio contiene **solo el código**. Los archivos originales de laboratorio y
las fichas de cata pertenecen al proyecto de caracterización del whisky chileno del
equipo iBio y su publicación no está autorizada, por lo que no se incluyen en este
repositorio público. **El acceso a estos datos queda sujeto a la autorización del equipo
responsable del proyecto iBio.** Tampoco se incluyen `data/procesamiento/`,
`data/modelamiento/` ni `outputs/`, porque contienen datos derivados; los scripts las
vuelven a generar. El archivo [`data/LEEME.md`](data/LEEME.md) indica qué archivos
espera el código y dónde deben copiarse.

## Estructura

```
Modelamiento/
├── Modelamiento.Rproj          Proyecto de RStudio (fija la raíz de trabajo)
├── data/                       (no publicada, ver data/LEEME.md)
│   ├── originales/             Archivos de laboratorio y fichas de cata originales
│   ├── procesamiento/          Datos extraídos, normalizados y matrices de análisis
│   └── modelamiento/           Insumos preparados para la etapa de modelamiento
├── outputs/                    (no publicada, la generan los scripts)
│   ├── exploratorio/           Resultados y figuras del análisis exploratorio
│   ├── modelamiento/           Métricas, validación bootstrap y sensibilidad
│   ├── predicciones/           Predicciones por escenario y target
│   └── presentacion/           Planillas consolidadas para revisión externa
└── scripts/R/
    ├── procesamiento/          00 a 09b: extracción, normalización y matrices
    ├── exploratorio/           10 a 11b: análisis exploratorio
    ├── modelamiento/           12 a 17: modelos, validación y sensibilidad
    └── analisis_finales/       18 a 20: consenso, comparación y robustez
```

## Entorno de software

Los resultados se obtuvieron con **R 4.5.3** sobre Windows 11, con las siguientes
versiones de paquetes:

| Paquete  | Versión | Paquete  | Versión |
|----------|---------|----------|---------|
| readxl   | 1.5.0   | openxlsx | 4.2.8.1 |
| dplyr    | 1.2.1   | ggplot2  | 4.0.3   |
| tidyr    | 1.3.2   | outliers | 0.15    |
| tibble   | 3.3.1   | glmnet   | 5.0     |
| stringr  | 1.6.0   | pls      | 2.9.0   |
| janitor  | 2.2.1   | ranger   | 0.18.0  |
| writexl  | 1.5.4   | gbm      | 2.2.3   |

`glmnet`, `pls`, `ranger` y `gbm` corresponden a los modelos supervisados. Si `ranger`
o `gbm` no están instalados, esos modelos se omiten y la omisión queda registrada en la
planilla de salida del script correspondiente.

## Cómo ejecutar

1. Instalar R 4.5.3 o superior (RStudio es opcional pero recomendado).
2. Clonar este repositorio, conservando su estructura interna. En Windows conviene
   ubicarlo en una ruta corta: si la carpeta base es muy larga, algunas salidas superan
   el límite de 260 caracteres de ruta del sistema y el script que las guarda falla.
3. Abrir `Modelamiento.Rproj`, o bien fijar el directorio de trabajo en la raíz de
   `Modelamiento/` con `setwd()`. Todas las rutas internas se construyen desde esa raíz.
4. `scripts/R/procesamiento/00_resumen_datos_iniciales.R` es el punto de entrada: define
   las rutas, carga los paquetes base y declara las funciones de normalización. Toma por
   defecto el directorio de trabajo actual y solo usa una ruta fija de respaldo si ese
   directorio no contiene la carpeta `data/`, por lo que normalmente no requiere edición.
5. Instalar los paquetes de la tabla anterior. El script `00` instala automáticamente los
   paquetes base que falten; el resto se instala con `install.packages()`.
6. Contar con los datos originales (su acceso queda sujeto a la autorización del equipo
   responsable del proyecto iBio) y copiarlos en `data/originales/` con sus nombres
   originales, según [`data/LEEME.md`](data/LEEME.md), ya que la clasificación inicial se
   apoya en ellos.
7. Ejecutar los scripts en orden de numeración, o el flujo completo de una vez con
   `source("scripts/R/ejecutar_todo.R")` (o `Rscript scripts/R/ejecutar_todo.R` desde la
   raíz). Cada script vuelve a cargar el `00`, por lo que una etapa puede re-ejecutarse sin
   repetir las anteriores si sus insumos existen.
8. Revisar las salidas en `data/procesamiento/`, `data/modelamiento/` y `outputs/`.

Las semillas están fijadas de forma explícita (semilla global `123`, más semillas
derivadas por escenario, target, modelo e iteración de bootstrap), por lo que una nueva
ejecución sobre los mismos datos reproduce exactamente los valores reportados en la
memoria.

## Orden de ejecución

Los scripts con sufijo de letra son variantes de su script base y se ejecutan justo
después de él: `02b` después de `02`, `09b` después de `09`, `11b` después de `11`,
`13b` después de `13`, y `16b`, `16c`, `16d`, `16e`, `16f` y `16h` después de `16` y
antes de `18` a `20`, ya que `20` consolida sus resultados. No existe un `16g`. `16i` es
opcional y no alimenta a ningún otro script.

| Script | Qué produce |
|--------|-------------|
| `ejecutar_todo.R` | Ejecuta los scripts `01` a `20` en orden, cada uno en un proceso de R independiente (excluye `16i`, opcional) |
| `00_resumen_datos_iniciales.R` | Configuración central: rutas, paquetes y normalización de nombres |
| `01_clasificar_datos_iniciales.R` | Clasificación de archivos originales en bloques químicos y sensoriales |
| `02_extraer_quimica.R` | Extracción y normalización de GC-FID, GC-MS, fenoles totales y cepas fermentativas |
| `02b_clasificar_compuestos_gcms.R` | Clasificación por familia química de compuestos GC-MS y validación del clasificador |
| `03_extraer_sensorial.R` | Extracción de fichas de cata y construcción de los targets comunes |
| `04_matriz_pura.R` | Matriz `M_pura` (escenario principal) |
| `05_matriz_pura_expandida_evaluador.R` | Matriz `M_expandida_evaluador` |
| `06_matrices_sin_cruce.R` | Matrices `M_quimica` y `M_sensorial` |
| `07_matriz_sensibilidad_cata_individual.R` | Matriz `M_cata_individual` |
| `08_matriz_datos_ia.R` | Matriz `M_ia_exploratoria` (escenario complementario) |
| `09_generar_excel_presentacion.R`, `09b_...comision.R` | Planillas consolidadas de las matrices |
| `10_analisis_quimico.R` | EDA químico: completitud, atípicos y correlaciones |
| `11_analisis_sensorial.R` | EDA sensorial: concordancia entre evaluadores y estructura de targets |
| `11b_eda_cruzado_quimico_sensorial.R` | EDA cruzado químico-sensorial |
| `12_preparar_datos_modelamiento.R` | Preparación de las matrices para modelamiento |
| `13_diagnostico_modelamiento.R` | Viabilidad estadística por escenario y target |
| `13b_colinealidad_modelamiento.R` | Diagnóstico de colinealidad entre predictores |
| `14_modelo_base.R` | Modelos base (media de entrenamiento y lineal reducido) y VIF |
| `15_modelos_small_data.R` | Ridge, Lasso, Elastic Net, PLS, Random Forest y Gradient Boosting restringidos |
| `16_validacion_bootstrap_cv.R` | Validación bootstrap agrupada (100 iteraciones) |
| `16b`, `16c`, `16d`, `16e`, `16f`, `16h` | Sensibilidad: atípicos químicos, imputación GC-MS y GC-FID, muestras comerciales, escala septiembre |
| `16i_verificacion_semilla.R` | Verificación del efecto de la semilla: repite `16` con 30 semillas independientes (ejecutar después de `16`) |
| `17_residuos_diagnostico.R` | Diagnóstico de residuos |
| `18_interpretabilidad_modelos.R` | Consenso de variables y variables prioritarias |
| `19_comparar_escenarios_modelamiento.R` | Comparación final entre escenarios y modelos |
| `20_robustez_resultados_finales.R` | Consolidación de la robustez de los resultados principales |

## Configuración de los modelos

| Modelo | Paquete | Configuración | n mínimo |
|--------|---------|---------------|----------|
| Media de entrenamiento | stats | Predicción constante igual a la media del target en entrenamiento | 3 |
| Lineal reducido | stats (`lm`) | Hasta 3 predictores seleccionados por correlación dentro de la partición | 3 |
| Ridge | glmnet | `alpha = 0`, `lambda = lambda.min` por CV interna, `standardize = TRUE` | 6 |
| Lasso | glmnet | `alpha = 1`, `lambda = lambda.min` por CV interna, `standardize = TRUE` | 6 |
| Elastic Net | glmnet | `alpha = 0.5`, `lambda = lambda.min` por CV interna, `standardize = TRUE` | 6 |
| PLS | pls | Hasta `min(5, p, n-2)` componentes, elegidos por CV leave-one-out, `scale = TRUE` | 6 |
| Random Forest restringido | ranger | 300 árboles, `max.depth = 3`, `mtry = floor(sqrt(p))`, `min.node.size = max(3, floor(n/5))`, importancia por permutación, hasta 10 predictores | 8 |
| Gradient Boosting restringido | gbm | Gaussiana, 100 árboles, `interaction.depth = 1`, `shrinkage = 0.05`, `n.minobsinnode = max(3, floor(n/4))`, `bag.fraction = 0.8`, hasta 10 predictores | 10 |

La preparación de los datos (selección de predictores por correlación absoluta,
imputación por mediana y recorte de predicciones al rango `[0, 5]`) se realiza
íntegramente dentro de cada partición de entrenamiento, para no introducir fuga de
información. Si una partición no alcanza el n mínimo, no deja predictores utilizables o
el ajuste falla, el modelo se omite en esa iteración sin imputar un valor, y el número de
iteraciones válidas se reporta junto a cada métrica. Como respaldo, si un modelo ya
ajustado no lograra predecir se usaría la media de entrenamiento; en la ejecución
reportada en la memoria este respaldo no se activó en ninguna iteración.
