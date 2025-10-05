# Paso 3 — Limpieza y estandarización

## Objetivo
Estandarizar nombres, ubigeos, fechas; quitar duplicados; crear llaves.

## Código de ejemplo
```r
library(dplyr); library(stringi); library(readr); library(janitor)
# Lee limpios del Paso 2 (o de data_clean/ si ya los guardaste)
# cenec     <- read_csv('data_clean/clean_cenec.csv')
# denuncias <- read_csv('data_clean/clean_denuncias.csv')

normalize <- function(x){
  x |>
    mutate(across(where(is.character), ~stri_trans_general(., 'Latin-ASCII'))) |>
    mutate(across(where(is.character), toupper)) |>
    clean_names()
}
# cenec     <- normalize(cenec)
# denuncias <- normalize(denuncias)
# write_csv(cenec,     'data_clean/clean_cenec.csv')
# write_csv(denuncias, 'data_clean/clean_denuncias.csv')
```
