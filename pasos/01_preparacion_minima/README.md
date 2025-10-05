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


## Código completo
```r
### Paso 1 — Auditor de archivos Excel
# Lee todos los .xlsx, estandariza nombres de columnas y sugiere campos clave (ubigeo/fecha/año)

library(readxl)   # para leer Excel
library(janitor)  # clean_names(): limpia nombres de columnas
library(dplyr)    # pipes y manipulación
library(stringi)  # utilidades de texto

# Función “segura” para leer Excel (si falla, devuelve tibble vacío con el error)
safe_read_excel <- function(path, sheet = 1) {
  tryCatch(
    readxl::read_excel(path, sheet = sheet),
    error = function(e) tibble::tibble(.error = conditionMessage(e))
  )
}

# Auditor: imprime resumen y devuelve fila con candidatos a ubigeo/fecha/año
auditar <- function(path) {
  cat("\n==== ", path, " ====\n")
  df_raw <- safe_read_excel(path)          # 1) lee
  df     <- df_raw %>% janitor::clean_names()  # 2) limpia nombres de columnas
  nms    <- names(df)

  print(utils::head(df, 3))                # 3) muestra las 3 primeras filas (vista rápida)
  cat("\nCols:", paste(nms, collapse = ", "), "\n")

  # 4) detecta candidatos por patrón
  cand_ubi  <- nms[grepl("ubigeo|ubi_geo|ubig|^ubi$", nms, ignore.case = TRUE)]
  cand_fec  <- nms[grepl("^fech|date|fch|fec_", nms, ignore.case = TRUE)]
  cand_anio <- nms[grepl("anio|ano|year", nms, ignore.case = TRUE)]

  # 5) imprime sugerencias
  cat("Sospechas -> UBIGEO:", paste(cand_ubi, collapse=" / "),
      "| FECHA:", paste(cand_fec, collapse=" / "),
      "| ANIO:",  paste(cand_anio, collapse=" / "), "\n")

  # 6) devuelve resumen (para armar reporte CSV)
  tibble::tibble(
    file = path, n_rows = nrow(df), n_cols = ncol(df),
    cand_ubigeo  = paste(cand_ubi, collapse = " / "),
    cand_fecha   = paste(cand_fec, collapse = " / "),
    cand_anio    = paste(cand_anio, collapse = " / "),
    example_cols = paste(utils::head(nms, 12), collapse = ", ")
  )
}

files   <- list.files(pattern = "\\.xlsx$", ignore.case = TRUE)  # 7) lista archivos .xlsx
resumen <- purrr::map_dfr(files, auditar)                        # 8) aplica auditor a cada archivo
readr::write_csv(resumen, "auditoria_xlsx.csv")                  # 9) guarda reporte
cat("\n📄 Reporte guardado en auditoria_xlsx.csv\n")
```
