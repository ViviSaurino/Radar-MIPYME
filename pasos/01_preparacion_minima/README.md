# Paso 1 — Preparación mínima

## Objetivo
Dejar lista la estructura de carpetas, dependencias y reglas de versionado.

## Estructura del proyecto
```text
data_raw/         # datos crudos (NO se versionan)
data_clean/       # intermedios limpios
data_out/         # salidas finales para tablero (sí se versionan)
app/              # Shiny (app.R vive aquí)
pasos/            # documentación por paso
```

## Reglas de versionado
Archivos pesados y sensibles se agregan a `.gitignore` (xlsx, pdf, qmd, data_raw/, rsconnect/, etc.).

## Código
```r
options(stringsAsFactors = FALSE)
dirs <- c('data_clean','data_out','app','pasos')
invisible(lapply(dirs, dir.create, showWarnings = FALSE))

pkgs <- c('readr','readxl','dplyr','tidyr','janitor','ggplot2','stringi')
inst <- !pkgs %in% rownames(installed.packages()); if(any(inst)) install.packages(pkgs[inst])
```
