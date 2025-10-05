# Paso 1 — Preparación mínima

## Objetivo
Dejar listo el entorno de trabajo, estructura de carpetas y dependencias.

## Estructura esperada
```text
data_raw/         # datos crudos (NO versionar)
data_clean/       # intermedios limpios
data_out/         # salidas para Shiny (SÍ versionar)
app/              # Shiny (app.R)
pasos/            # documentación por paso
```

## Cómo reproducir
```r
options(stringsAsFactors = FALSE)
dir.create('data_clean', showWarnings = FALSE)
dir.create('data_out',  showWarnings = FALSE)
pkgs <- c('readr','readxl','dplyr','tidyr','janitor','ggplot2')
inst <- !pkgs %in% rownames(installed.packages()); if(any(inst)) install.packages(pkgs[inst])
```
