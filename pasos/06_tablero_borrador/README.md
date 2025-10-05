# Paso 6 — Tablero (borrador)

## Objetivo
Probar la app Shiny local y ajustar inputs/plots antes del deploy.

## Cómo ejecutar
```r
shiny::runApp('app')   # o abre app/app.R y pulsa Run
```

## Checklist
- `data_out/data_ml.csv` existe y se carga sin warnings.
- Inputs responden (filtros de distrito, años, métricas).
- Gráficos y tabla muestran datos esperados.

## Deploy (recordatorio)
```r
library(rsconnect)
# rsconnect::setAccountInfo(name='TU_USUARIO', token='...', secret='...')
rsconnect::deployApp(appDir='app', appPrimaryDoc='app.R',
  appName='observatorio_callao_demo', appTitle='Observatorio Callao — Demo')
```
