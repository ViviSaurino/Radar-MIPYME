# Paso 5 — EDA básica

## Objetivo
Entender distribuciones, outliers y tendencias.

## Código de ejemplo
```r
library(readr); library(dplyr); library(ggplot2)
df <- read_csv('data_out/data_ml.csv', show_col_types = FALSE)
ggplot(df, aes(denuncias_por_1k_emp)) + geom_histogram(bins = 30)
df |>
  group_by(distrito) |>
  summarise(media = mean(denuncias_por_1k_emp, na.rm = TRUE)) |>
  arrange(desc(media)) |>
  head(10)
```
