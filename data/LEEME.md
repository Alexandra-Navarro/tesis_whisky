# Datos del proyecto

Los datos de este estudio **no se publican en el repositorio**. Corresponden a los
resultados de laboratorio y a las fichas de cata del proyecto de caracterización del
whisky chileno del equipo iBio (*Millennium Institute for Integrative Biology*), y su
publicación no está autorizada. **El acceso a estos datos queda sujeto a la autorización
del equipo responsable del proyecto iBio.**

## Archivos que espera el código

Para reproducir el análisis, los archivos originales deben copiarse en
`data/originales/` con la siguiente estructura y con sus nombres originales, ya que el
script `01_clasificar_datos_iniciales.R` los reconoce por nombre:

```
data/originales/
├── quimica/
│   ├── 260507_Datos_tesis_JU.xlsx
│   ├── 260603_quimica.xlsx
│   ├── Panel sensorial_06_Noviembre_GC_MS.xlsx
│   ├── Resultados GC-FID_edited.xlsx
│   └── Resultados GC-MS.zip
├── referencia/
│   └── Data muestras.xlsx
└── sensorial/
    ├── 260603_Resultados cata.xlsx
    ├── Cata Social_Enero_ 2025.xlsx
    ├── Cata sensorial_Septiembre_2024.xlsx
    ├── Panel sensorial_06_Noviembre.xlsx
    └── sensorial_cepas_JU.xlsx
```

Además, el escenario exploratorio `M_ia_exploratoria` (script `08_matriz_datos_ia.R`)
usa las catas generadas por modelos de lenguaje, que no se obtienen de ningún script y
deben copiarse en `data/procesamiento/catas_ia/`:

```
data/procesamiento/catas_ia/
├── catas_ia_claude.xlsx
├── catas_ia_gpt.xlsx
└── cata_ia_geminis.xlsx
```

El resto de `data/procesamiento/`, `data/modelamiento/` y `outputs/` no se incluye
porque contiene datos derivados de esos archivos. Los scripts lo vuelven a generar al
ejecutarse en orden; el script `scripts/R/ejecutar_todo.R` ejecuta el flujo completo
(ver `README.md`).
