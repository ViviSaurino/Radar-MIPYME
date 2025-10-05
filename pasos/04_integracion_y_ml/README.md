# Paso 4 — Integración y métricas (ML-ready)

## Objetivo
Unir por **distrito–anio**, imputar `n_empresas` y calcular tasas **por 1,000 empresas**. Conservar `ingresos_anuales` y `ciiu` (si existen).

## Código
```r
library(dplyr); library(readr); library(tidyr); library(janitor)
to_int <- function(x) suppressWarnings(as.integer(x))

den  <- read_csv('data_clean/clean_denuncias.csv', show_col_types = FALSE) |> clean_names()
llam <- read_csv('data_clean/clean_llamadas.csv',  show_col_types = FALSE) |> clean_names()
emp  <- read_csv('data_clean/clean_cenec.csv',     show_col_types = FALSE) |> clean_names()

# Renombrado defensivo:
den  <- den  |> rename(n_denuncias = n_denuncias)
llam <- llam |> rename(n_llamadas  = n_llamadas)
emp  <- emp  |> rename(n_empresas  = n_empresas)

df <- den |>
  full_join(llam, by=c('distrito','anio')) |>
  full_join(emp,  by=c('distrito','anio')) |>
  mutate(across(c(n_denuncias,n_llamadas,n_empresas), to_int))

# Imputa stock 0/NA por mediana distrital
df <- df |> group_by(distrito) |>
  mutate(n_empresas = ifelse(is.na(n_empresas) | n_empresas==0,
                             median(n_empresas, na.rm=TRUE), n_empresas)) |>
  ungroup()

# Tasas por 1,000
df <- df |> mutate(
  denuncias_por_1k_emp = 1000 * n_denuncias / n_empresas,
  llamadas_por_1k_emp  = 1000 * n_llamadas  / n_empresas
)

# (Opcional) Winsorize suave para outliers de tasas
wins <- function(x, p=c(.01,.99)){
  q <- quantile(x, probs=p, na.rm=TRUE); pmin(pmax(x,q[1]), q[2])
}
df <- df |> mutate(across(c(denuncias_por_1k_emp, llamadas_por_1k_emp), wins))

dir.create('data_out', showWarnings = FALSE)
write_csv(df, 'data_out/data_ml.csv')
```

## Nota
Si `ingresos_anuales` y `ciiu` existen en CENEC, ya viajan en `data_ml.csv` y podrán usarse en el tablero (filtros, comparaciones por sector, etc.).
