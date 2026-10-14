# Datos del proyecto

Los datos de este estudio **no se publican en el repositorio**. Corresponden a los
resultados de laboratorio y a las fichas de cata del proyecto de caracterización del
whisky chileno del equipo iBio (*Millennium Institute for Integrative Biology*), y su
publicación no está autorizada. Están disponibles **a solicitud a la autora**, Alexandra
Navarro Calderón. Las fichas de cata se comparten sin la identidad de los catadores.

## Archivos que espera el código

Para reproducir el análisis, los archivos originales deben copiarse en
`data/originales/` con la siguiente estructura y con sus nombres originales, ya que el
script `01_clasificar_datos_iniciales.R` los reconoce por nombre:

```
data/originales/
├── quimica/
│   ├── 260507_Datos_tesis_JU.xlsx
│   ├── 260603_Alexandra.xlsx
│   ├── Panel sensorial_06_Noviembre_GC_MS.xlsx
│   ├── Resultados GC-FID Constanza Vidal_edited.xlsx
│   └── Resultados GC-MS-Constanza Vidal.zip
├── referencia/
│   └── Data muestras.xlsx
└── sensorial/
    ├── 260603_Resultados cata_Alexandra.xlsx
    ├── Cata Social_Enero_ 2025.xlsx
    ├── Cata sensorial_Septiembre_2024.xlsx
    ├── Panel sensorial_06_Noviembre.xlsx
    └── sensorial_cepas_JU.xlsx
```

Las carpetas `data/procesamiento/`, `data/modelamiento/` y `outputs/` no se incluyen
porque contienen datos derivados de esos archivos. Los scripts las vuelven a generar al
ejecutarse en orden (ver `README.md`).
