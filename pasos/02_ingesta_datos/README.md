# Paso 2 — Ingesta de datos

## Objetivo
Cargar fuentes crudas (CENEC, denuncias, llamadas, cámaras, ENAPRES si aplica) a **data_clean/** en CSV limpios.

## Entradas esperadas
- `data_raw/Callao_CENEC.xlsx` (empresas, e idealmente `ingresos_anuales` y `ciiu`)
- `data_raw/DATASET_Denuncias_Policiales.xlsx`
- `data_raw/camaras_incidencias_callao.xlsx`

## Salidas
- `data_clean/clean_cenec.csv`, `clean_denuncias.csv`, `clean_llamadas.csv` …

## Código
```r
library(readr); library(readxl); library(janitor); library(dplyr)
dir.create('data_clean', showWarnings = FALSE)

norm_names <- function(df) janitor::clean_names(df)

cenec <- readxl::read_xlsx('data_raw/Callao_CENEC.xlsx') |> norm_names()
write_csv(cenec, 'data_clean/clean_cenec.csv')

den <- readxl::read_xlsx('data_raw/DATASET_Denuncias_Policiales.xlsx') |> norm_names()
write_csv(den, 'data_clean/clean_denuncias.csv')

llam <- readxl::read_xlsx('data_raw/camaras_incidencias_callao.xlsx') |> norm_names()
write_csv(llam, 'data_clean/clean_llamadas.csv')
```


### Código completo (paso2_limpieza_integracion.R)

```r

```

