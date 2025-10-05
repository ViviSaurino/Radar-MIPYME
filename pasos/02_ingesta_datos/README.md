# Paso 2 — Ingesta de datos

## Objetivo
Ingerir fuentes crudas (CENEC, denuncias, llamadas, etc.) a **data_clean/** con el mínimo tratamiento.

## Entradas
- Archivos en **data_raw/** (xlsx/csv).

## Salidas
- Archivos `.csv` en **data_clean/**.

## Cómo reproducir
```r
library(readr); library(readxl); library(janitor); library(dplyr)
dir.create('data_clean', showWarnings = FALSE)

# Ejemplos (ajusta nombres reales):
cenec <- readxl::read_xlsx('data_raw/Callao_CENEC.xlsx') |> janitor::clean_names()
write_csv(cenec, 'data_clean/clean_cenec.csv')

den   <- readxl::read_xlsx('data_raw/DATASET_Denuncias_Policiales.xlsx') |> clean_names()
write_csv(den, 'data_clean/clean_denuncias.csv')

llam  <- readxl::read_xlsx('data_raw/camaras_incidencias_callao.xlsx') |> clean_names()
write_csv(llam, 'data_clean/clean_llamadas.csv')
```
