# Paso 5 — EDA básica

## Objetivo
Validar rangos, distribuciones y outliers antes del tablero.

## Cómo reproducir
```r
library(readr); library(dplyr); library(ggplot2)
d <- read_csv('data_out/data_ml.csv', show_col_types = FALSE)
summary(d[,c('n_empresas','n_denuncias','n_llamadas','denuncias_por_1k_emp','llamadas_por_1k_emp')])
ggplot(d, aes(denuncias_por_1k_emp)) + geom_histogram(bins=30)
ggplot(d, aes(llamadas_por_1k_emp)) + geom_histogram(bins=30)
```
