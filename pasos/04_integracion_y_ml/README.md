# Paso 4 — Integración y métricas (ML-ready)

## Objetivo
Unir fuentes por **distrito–año**, imputar mínimos y calcular **tasas por 1,000 empresas** → `data_out/data_ml.csv`.

## Cómo reproducir
```r
library(dplyr); library(tidyr); library(readr); library(janitor)
to_int <- function(x) suppressWarnings(as.integer(x))

den  <- read_csv('data_clean/clean_denuncias.csv', show_col_types = FALSE) |> clean_names()
llam <- read_csv('data_clean/clean_llamadas.csv',  show_col_types = FALSE) |> clean_names()
emp  <- read_csv('data_clean/clean_cenec.csv',     show_col_types = FALSE) |> clean_names()

# Asegura nombres:
den  <- den  |> rename(distrito = distrito, anio = anio, n_denuncias = n_denuncias)
llam <- llam |> rename(distrito = distrito, anio = anio, n_llamadas = n_llamadas)
emp  <- emp  |> rename(distrito = distrito, anio = anio, n_empresas = n_empresas)

df <- den |>
  full_join(llam, by = c('distrito','anio')) |>
  full_join(emp,  by = c('distrito','anio')) |>
  mutate(across(c(n_denuncias,n_llamadas,n_empresas), to_int))

# Imputa empresas por mediana de distrito si faltan o son 0:
df <- df |> group_by(distrito) |>
  mutate(n_empresas = ifelse(is.na(n_empresas) | n_empresas==0,
                             median(n_empresas, na.rm = TRUE), n_empresas)) |>
  ungroup()

# Tasas (por 1,000 empresas):
df <- df |> mutate(
  denuncias_por_1k_emp = 1000 * n_denuncias / n_empresas,
  llamadas_por_1k_emp  = 1000 * n_llamadas  / n_empresas
)

dir.create('data_out', showWarnings = FALSE)
write_csv(df, 'data_out/data_ml.csv')
```
