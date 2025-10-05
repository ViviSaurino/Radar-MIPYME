# Paso 8 — Documentación y reproducibilidad

## Objetivo
Dejar instrucciones claras para ejecutar el pipeline y publicar.

## Checklist
- Estructura de carpetas consistente.
- `data_out/data_ml.csv` presente.
- App corre local y en shinyapps.io.

## Cómo publicar
```r
library(rsconnect)
# rsconnect::setAccountInfo(name='TU_USUARIO', token='...', secret='...')
rsconnect::deployApp(appDir = 'app', appPrimaryDoc = 'app.R',
                     appName = 'observatorio_callao_demo',
                     appTitle = 'Observatorio Callao — Demo')
```
