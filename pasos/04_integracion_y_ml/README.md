# Paso 4 — Integración y cálculo de tasas (data_ml.csv)

## Objetivo
Integrar tablas por `ubigeo / distrito / anio` y crear tasas por 1,000 empresas.

## Código de ejemplo
```r
library(dplyr); library(readr); library(tidyr); library(janitor)
# Lee insumos ya estandarizados
cenec     <- read_csv('data_clean/clean_cenec.csv', show_col_types = FALSE)     |> clean_names()
denuncias <- read_csv('data_clean/clean_denuncias.csv', show_col_types = FALSE) |> clean_names()
llamadas  <- read_csv('data_clean/clean_llamadas.csv', show_col_types = FALSE)  |> clean_names()
# Une por llaves comunes (ajusta nombres de columnas reales)
base <- denuncias |>
  full_join(llamadas, by = c('ubigeo','distrito','anio')) |>
  left_join(cenec,    by = c('ubigeo','distrito','anio')) |>
  mutate(
    n_empresas = coalesce(as.integer(n_empresas), 0L),
    n_denuncias = coalesce(as.integer(n_denuncias), 0L),
    n_llamadas  = coalesce(as.integer(n_llamadas),  0L),
    denuncias_por_1k_emp = if_else(n_empresas > 0, 1000 * n_denuncias / n_empresas, NA_real_),
    llamadas_por_1k_emp  = if_else(n_empresas > 0, 1000 * n_llamadas  / n_empresas, NA_real_)
  )
readr::write_csv(base, 'data_out/data_ml.csv', na = '')
```
