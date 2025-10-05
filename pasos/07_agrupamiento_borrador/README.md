# Paso 7 — Clustering (borrador y validación)

## Objetivo
Explorar K-means / Ward / DBSCAN con métricas de tasas.

## Código de ejemplo
```r
library(readr); library(dplyr);
df <- read_csv('data_out/data_ml.csv', show_col_types = FALSE)
X  <- df |> select(denuncias_por_1k_emp, llamadas_por_1k_emp) |> as.matrix() |> scale()
set.seed(123); km <- kmeans(X, centers = 3, nstart = 20)
table(km$cluster)
```
