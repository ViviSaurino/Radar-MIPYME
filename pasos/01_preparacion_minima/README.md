# Paso 1 — Preparación mínima

## Objetivo
Dejar listo el entorno de trabajo, estructura de carpetas y dependencias.

## Estructura esperada (mínimo)
```text
data_raw/         # datos originales (no versionar)
data_clean/       # intermedios limpios
data_out/         # salidas listas para app (versionar)
app/              # Shiny (app.R está aquí)
pasos/            # documentación por paso
```

## Código de referencia
```r
options(stringsAsFactors = FALSE)
dir.create('data_clean', showWarnings = FALSE)
dir.create('data_out',  showWarnings = FALSE)
# instala paquetes si hiciera falta:
pkgs <- c('dplyr','tidyr','readr','janitor','ggplot2')
inst <- !pkgs %in% rownames(installed.packages())
if (any(inst)) install.packages(pkgs[inst])
```
