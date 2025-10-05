# Paso 3 — Limpieza y estandarización

## Objetivo
Estandarizar nombres, corregir tipos, asegurar llaves (ubigeo–distrito–año).

## Cómo reproducir
```r
library(dplyr); library(janitor); library(readr); library(stringi)

fix_txt <- function(x){
  x <- toupper(x); x <- stringi::stri_trans_general(x, 'Latin-ASCII'); trimws(x)
}

den <- read_csv('data_clean/clean_denuncias.csv', show_col_types = FALSE) |>
  clean_names() |> mutate(distrito = fix_txt(distrito))
write_csv(den, 'data_clean/clean_denuncias.csv')

llam <- read_csv('data_clean/clean_llamadas.csv', show_col_types = FALSE) |>
  clean_names() |> mutate(distrito = fix_txt(distrito))
write_csv(llam, 'data_clean/clean_llamadas.csv')

cenec <- read_csv('data_clean/clean_cenec.csv', show_col_types = FALSE) |>
  clean_names() |> mutate(distrito = fix_txt(distrito))
write_csv(cenec, 'data_clean/clean_cenec.csv')
```
