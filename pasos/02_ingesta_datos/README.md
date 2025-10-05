# Paso 2 — Ingesta de datos

## Objetivo
Leer insumos (CENEC, denuncias, llamadas, cámaras, ENAPRES, etc.).

## Código de ejemplo (ajusta nombres reales):
```r
library(readr); library(dplyr); library(janitor)
# Ejemplos (ajusta a tus archivos reales en data_raw/)
cenec      <- readr::read_csv('data_raw/Callao_CENEC.xlsx', show_col_types = FALSE) |> clean_names()
denuncias  <- readr::read_csv('data_raw/DATASET_Denuncias_Policiales.xlsx', show_col_types = FALSE) |> clean_names()
llamadas   <- readr::read_csv('data_raw/enapres_2024.xlsx', show_col_types = FALSE) |> clean_names()
# Guarda copias intermedias limpias si aplica:
# write_csv(cenec,     'data_clean/clean_cenec.csv')
# write_csv(denuncias, 'data_clean/clean_denuncias.csv')
# write_csv(llamadas,  'data_clean/clean_llamadas.csv')
```
