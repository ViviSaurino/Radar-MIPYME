# PASO 3 — Chequeos rápidos y saneamiento ligero (robusto + incluye Cámaras)
# ------------------------------------------------------------------------------
# Qué hace:
# 1) Lee data_clean.csv (generado en PASO 2) y normaliza nombres clave.
# 2) Asegura presencia/tipo de: ubigeo, distrito, anio, n_empresas, n_denuncias, n_llamadas, n_camaras.
# 3) Imprime HEAD/GLIMPSE, lista años y distritos, y arma una tabla distrito–año.
# 4) Calcula totales globales y conteo de NA por columna.
# 5) Detecta llaves duplicadas (ubigeo–distrito–anio) y las guarda en CSV si existen.
# 6) Guarda resúmenes en CSV (para inspección o trazabilidad).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(janitor)
  library(readr)
  library(rlang)
  library(tidyr)
})

# Helper: elegir la PRIMERA columna que matchee cualquiera de los patrones
pick_col <- function(nms, pats) {
  hit <- nms[Reduce(`|`, lapply(pats, function(p) grepl(p, nms, ignore.case = TRUE)))]
  if (length(hit) == 0) NA_character_ else hit[1]
}

# 1) Leer data_clean.csv
path_clean <- "data_clean.csv"
if (!file.exists(path_clean)) stop("No encuentro 'data_clean.csv'. Ejecuta primero el PASO 2.")

dc <- read.csv(path_clean, fileEncoding = "UTF-8", stringsAsFactors = FALSE) %>%
  janitor::clean_names()

# 2) Canonicalizar nombres clave (ubigeo, dpto, prov, distr, anio, y métricas)
nms <- names(dc)

col_ubigeo <- pick_col(nms, c("^ubigeo$"))
col_dep    <- pick_col(nms, c("^departamento$"))
col_prov   <- pick_col(nms, c("^provincia$"))
col_dist   <- pick_col(nms, c("^distrito$"))
col_anio   <- pick_col(nms, c("^anio$", "^ano$", "^year$"))

col_emp    <- pick_col(nms, c("^n_?empresas$", "^empresas_stock$", "^empresas$"))
col_den    <- pick_col(nms, c("^n_?denuncias$", "denuncias$"))
col_ll     <- pick_col(nms, c("^n_?llamadas$", "llamadas$"))
col_cam    <- pick_col(nms, c("^n_?camaras$", "camaras$"))

rename_map <- c(
  ubigeo       = col_ubigeo,
  departamento = col_dep,
  provincia    = col_prov,
  distrito     = col_dist,
  anio         = col_anio,
  n_empresas   = col_emp,
  n_denuncias  = col_den,
  n_llamadas   = col_ll,
  n_camaras    = col_cam
)
rename_map <- rename_map[!is.na(rename_map)]             # sólo las que existen
dc <- dplyr::rename(dc, !!!set_names(rename_map, names(rename_map)))

# 3) Validaciones mínimas (cámaras no es crítica: si falta, se creará en 0)
need_cols <- c("ubigeo","distrito","anio","n_empresas","n_denuncias","n_llamadas")
faltan <- setdiff(need_cols, names(dc))
if (length(faltan) > 0) {
  stop("Faltan columnas críticas en data_clean: ", paste(faltan, collapse=", "),
       ". Revisa el PASO 2 y vuelve a generar 'data_clean.csv'.")
}

# 4) Asegurar n_camaras (si no estaba en el archivo, la creamos en 0 y avisamos)
if (!"n_camaras" %in% names(dc)) {
  warning("No encontré 'n_camaras' en data_clean. La creo en 0 (revisa PASO 2 si corresponde).")
  dc$n_camaras <- 0L
}

# 5) Tipos/formatos: anio integer, métricas integer; ubigeo como texto de 6 dígitos si aplica
to_int <- function(x) suppressWarnings(as.integer(x))
dc <- dc %>%
  mutate(
    anio        = to_int(anio),
    n_empresas  = to_int(n_empresas),
    n_denuncias = to_int(n_denuncias),
    n_llamadas  = to_int(n_llamadas),
    n_camaras   = to_int(n_camaras),
    ubigeo      = if ("ubigeo" %in% names(.)) {
      # deja ubigeo como carácter para preservar ceros a la izquierda (si existieran)
      as.character(ubigeo)
    } else ubigeo
  )

# 6) Vistas rápidas
cat("\n---- HEAD(10) ----\n"); print(utils::head(dc, 10))
cat("\n---- GLIMPSE ----\n"); print(utils::capture.output(dplyr::glimpse(dc)))

# 7) Años y distritos presentes
years <- sort(unique(dc$anio))
dists <- sort(unique(dc$distrito))
cat("\nAños en data_clean: ", paste(years, collapse = ", "), "\n", sep = "")
cat("Distritos en data_clean: ", paste(dists, collapse = " | "), "\n", sep = "")

# 8) Resumen por distrito-año (conteos brutos)
tabla_dya <- dc %>%
  group_by(distrito, anio) %>%
  summarise(
    empresas_stock = sum(n_empresas,   na.rm = TRUE),
    denuncias      = sum(n_denuncias,  na.rm = TRUE),
    llamadas       = sum(n_llamadas,   na.rm = TRUE),
    camaras        = sum(n_camaras,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(distrito, anio)

cat("\n---- Tabla distrito-año (primeras 20 filas) ----\n")
print(utils::head(tabla_dya, 20))

# 9) Totales globales
totales <- dc %>%
  summarise(
    emp_tot_anual_prom = round(mean(n_empresas, na.rm = TRUE), 0),
    denuncias_tot      = sum(n_denuncias, na.rm = TRUE),
    llamadas_tot       = sum(n_llamadas,  na.rm = TRUE),
    camaras_tot        = sum(n_camaras,   na.rm = TRUE)
  )
cat("\n---- Totales globales ----\n"); print(totales)

# 10) NA por columna
na_check <- dc %>%
  summarise(across(c(ubigeo, departamento, provincia, distrito, anio,
                     n_empresas, n_denuncias, n_llamadas, n_camaras),
                   ~ sum(is.na(.x))))
cat("\n---- NA por columna ----\n"); print(na_check)

# 11) Duplicados de llave (ubigeo–distrito–anio)
dup <- dc %>%
  count(ubigeo, distrito, anio, name = "n") %>%
  filter(n > 1) %>%
  arrange(desc(n), ubigeo, distrito, anio)

if (nrow(dup) > 0) {
  message("⚠️ Ojo: hay llaves duplicadas en data_clean. Guardando 'duplicados_llave.csv'.")
  readr::write_csv(dup, "duplicados_llave.csv")
} else {
  message("✅ Sin duplicados de llave (ubigeo–distrito–anio).")
}

# 12) Guardar resúmenes
readr::write_csv(tabla_dya, "resumen_distrito_anio.csv")
readr::write_csv(totales,  "resumen_totales.csv")
readr::write_csv(na_check, "resumen_na.csv")
cat("\n📄 Guardados: resumen_distrito_anio.csv, resumen_totales.csv, resumen_na.csv",
    if (nrow(dup) > 0) " y duplicados_llave.csv" else "", "\n", sep = "")
```
