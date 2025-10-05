# Paso 7 — Agrupamiento (borrador)

## Objetivo
Probar K-means / Ward / DBSCAN con variables de tasas.

## Cómo reproducir
```r
library(readr); library(dplyr)
d <- read_csv('data_out/data_ml.csv', show_col_types = FALSE) |> distinct(distrito, anio, .keep_all = TRUE)
X <- d |> select(denuncias_por_1k_emp, llamadas_por_1k_emp) |> scale() |> as.matrix()
set.seed(123); k3 <- kmeans(X, centers = 3, nstart = 25)
table(k3$cluster)
```
