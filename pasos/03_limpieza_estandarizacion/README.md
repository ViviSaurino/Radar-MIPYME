# Paso 3 — Limpieza y estandarización

## Objetivo
Estandarizar textos (mayúsculas, sin tildes), tipos y llaves (`distrito–anio`).

## Código
```r
library(dplyr); library(janitor); library(readr); library(stringi)

fix_txt <- function(x){ x <- toupper(x); x <- stringi::stri_trans_general(x,'Latin-ASCII'); trimws(x) }
to_int  <- function(x) suppressWarnings(as.integer(x))

den <- read_csv('data_clean/clean_denuncias.csv', show_col_types = FALSE) |>
  clean_names() |>
  mutate(distrito = fix_txt(distrito), anio = to_int(anio), n_denuncias = to_int(n_denuncias))
write_csv(den, 'data_clean/clean_denuncias.csv')

llam <- read_csv('data_clean/clean_llamadas.csv', show_col_types = FALSE) |>
  clean_names() |>
  mutate(distrito = fix_txt(distrito), anio = to_int(anio), n_llamadas = to_int(n_llamadas))
write_csv(llam, 'data_clean/clean_llamadas.csv')

cenec <- read_csv('data_clean/clean_cenec.csv', show_col_types = FALSE) |>
  clean_names() |>
  mutate(distrito = fix_txt(distrito), anio = to_int(anio),
         n_empresas = to_int(n_empresas),
         ingresos_anuales = suppressWarnings(as.numeric(ingresos_anuales)),
         ciiu = ifelse(is.na(ciiu),'SIN_CIIU', as.character(ciiu)))
write_csv(cenec, 'data_clean/clean_cenec.csv')
```
