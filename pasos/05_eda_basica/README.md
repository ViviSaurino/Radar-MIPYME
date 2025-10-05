# Paso 5 — EDA básica

## Objetivo
Validar distribuciones y relaciones clave antes del tablero.

## Código
```r
library(readr); library(dplyr); library(ggplot2)
d <- read_csv('data_out/data_ml.csv', show_col_types = FALSE)

# Resumen rápido
d |> select(n_empresas, n_denuncias, n_llamadas, denuncias_por_1k_emp, llamadas_por_1k_emp) |> summary()

# Histogramas
ggplot(d, aes(denuncias_por_1k_emp)) + geom_histogram(bins=30)
ggplot(d, aes(llamadas_por_1k_emp))  + geom_histogram(bins=30)

# Relación tasas
ggplot(d, aes(denuncias_por_1k_emp, llamadas_por_1k_emp)) + geom_point(alpha=.7) + geom_smooth(method='lm', se=FALSE)

# (Opcional) por CIIU si existe
if ('ciiu' %in% names(d)) {
  ggplot(d, aes(ciiu, denuncias_por_1k_emp)) + geom_boxplot() + coord_flip()
}
```
