# Paso 7 — Agrupamiento (borrador)

## Objetivo
Explorar K-means / Ward / DBSCAN con variables de tasas (y opcionalmente ingresos).

## Código
```r
library(readr); library(dplyr)
d <- read_csv('data_out/data_ml.csv', show_col_types = FALSE) |> distinct(distrito, anio, .keep_all = TRUE)
vars <- c('denuncias_por_1k_emp','llamadas_por_1k_emp')
if ('ingresos_anuales' %in% names(d)) vars <- c(vars,'ingresos_anuales')
X <- d |> select(all_of(vars)) |> mutate(across(everything(), as.numeric)) |> scale() |> as.matrix()

set.seed(123); km3 <- kmeans(X, centers=3, nstart=25)
table(km3$cluster)
```

El tablero final ya incorpora clustering interactivo en la pestaña *Clustering*.
