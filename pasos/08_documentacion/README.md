# Paso 8 — Documentación y reproducibilidad

## Objetivo
Dejar el pipeline ejecutable por cualquier integrante del equipo.

## Checklist final
- Pasos 1–7 reproducibles en orden.
- `data_out/data_ml.csv` actualizado.
- App corre local y en shinyapps.io.

## Deploy
```r
library(rsconnect)
# rsconnect::setAccountInfo(name='TU_USUARIO', token='...', secret='...')
rsconnect::deployApp(appDir='app', appPrimaryDoc='app.R',
                     appName='observatorio_callao_demo',
                     appTitle='Observatorio Callao — Demo')
```
