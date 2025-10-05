# Paso 2 — Ingesta de datos

## Objetivo
Cargar fuentes crudas (CENEC, denuncias, llamadas, cámaras, ENAPRES si aplica) a **data_clean/** en CSV limpios.

## Entradas esperadas
- `data_raw/Callao_CENEC.xlsx` (empresas, e idealmente *ingresos_anuales* y *ciiu*)
- `data_raw/DATASET_Denuncias_Policiales.xlsx`
- `data_raw/camaras_incindencias_callao.xlsx`

## Salidas
- `data_clean/clean_cenec.csv`,  `clean_denuncias.csv`,  `clean_llamadas.csv` …

### Código completo

```r

# PASO 2 — Limpieza e integración (incluye Cámaras + chequeos)  [FIX anio_hint]
# ------------------------------------------------------------------------------
# - Lee de:    data_raw/  (Excel)
# - Escribe:   data_clean/ (data_clean.csv)  y  data_out/ (data_ml.csv)
# - Parche:    infiere "anio" desde nombre de archivo si falta en columnas/fechas

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(janitor); library(stringi)
  library(lubridate); library(tidyr); library(purrr)
})
options(dplyr.summarise.inform = FALSE)

DIR_RAW   <- "data_raw"
DIR_CLEAN <- "data_clean"
DIR_OUT   <- "data_out"

if (!dir.exists(DIR_RAW))   stop("No existe la carpeta "", DIR_RAW, "".")
if (!dir.exists(DIR_CLEAN)) dir.create(DIR_CLEAN, recursive = TRUE)
if (!dir.exists(DIR_OUT))   dir.create(DIR_OUT,   recursive = TRUE)

norm_txt <- function(x){
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- toupper(x); x <- trimws(x); gsub("\\s+", " ", x)
}
fix_ubigeo6 <- function(x){
  x <- as.character(x); x[is.na(x)] <- ""; x <- gsub("\\D", "", x)
  x <- ifelse(nchar(x) == 5, paste0("0", x), x); x[nchar(x) != 6] <- NA; x
}
ymd_num <- function(x) suppressWarnings(as.Date(as.character(x), "%Y%m%d"))
safe_col <- function(df, colname) if (is.null(colname) || is.na(colname) || !colname %in% names(df)) rep(NA, nrow(df)) else df[[colname]]
detect_col <- function(nms, ...) {
  pats <- c(...); hit <- nms[Reduce(`|`, lapply(pats, function(p) grepl(p, nms, ignore.case = TRUE)))]
  if (length(hit) == 0) NA_character_ else hit[1]
}
pad2 <- function(v) sprintf("%02d", suppressWarnings(as.integer(v)))

year_from_filename <- function(path){
  m <- regmatches(path, regexpr("(20[1-3][0-9])", path))
  y <- suppressWarnings(as.integer(m))
  if (length(y) == 1 && is.finite(y)) y else NA_integer_
}
read_any_with_year <- function(path){
  df <- readxl::read_excel(path)
  y  <- year_from_filename(basename(path))
  df$anio_hint <- if (!is.na(y)) as.integer(y) else NA_integer_
  df
}

pick_file <- function(pattern, dir = DIR_RAW){
  f <- list.files(dir, pattern = pattern, ignore.case = TRUE, full.names = TRUE)
  if (length(f) == 0) {
    cat("\nArchivos dentro de", dir, ":\n"); print(list.files(dir))
    stop("No encuentro archivo que cumpla el patrón: ", pattern, " en ", dir)
  }
  if (length(f) > 1) message("⚠️ Encontré varios para '", pattern, "'. Tomo: ", basename(f[1]))
  f[1]
}

f_cenec     <- pick_file(".*CENEC.*\\.xlsx$")
f_denuncias <- pick_file("DATASET.*Denuncias.*\\.xlsx$")
f_llamadas  <- pick_file("^llamadas.*emergencias.*\\.xlsx$")
f_camaras   <- pick_file("^camaras.*callao.*\\.xlsx$")

message("\nArchivos usados:",
        "\n- CENEC:     ", basename(f_cenec),
        "\n- DENUNCIAS: ", basename(f_denuncias),
        "\n- LLAMADAS:  ", basename(f_llamadas),
        "\n- CAMARAS:   ", basename(f_camaras), "\n")

# 1) CENEC
cenec <- read_any_with_year(f_cenec) |> clean_names()
nms   <- names(cenec)
c_ubi  <- detect_col(nms, "ubigeo")
c_dep  <- detect_col(nms, "depa|depart")
c_prov <- detect_col(nms, "^prov")
c_dist <- detect_col(nms, "^dist")
c_anio <- detect_col(nms, "anio|ano|year")
c_ciiu <- detect_col(nms, "ciiu|actividad")
c_ccdd <- detect_col(nms, "^ccdd$"); c_ccpp <- detect_col(nms, "^ccpp$"); c_ccdi <- detect_col(nms, "^ccdi$")
cenec_clean <- cenec |>
  mutate(
    ubigeo_detect = fix_ubigeo6(safe_col(cur_data(), c_ubi)),
    ubigeo_from_ccd = ifelse(
      is.na(ubigeo_detect) & !is.na(c_ccdd) & !is.na(c_ccpp) & !is.na(c_ccdi),
      fix_ubigeo6(paste0(pad2(safe_col(cur_data(), c_ccdd)),
                         pad2(safe_col(cur_data(), c_ccpp)),
                         pad2(safe_col(cur_data(), c_ccdi)))),
      NA
    ),
    ubigeo       = coalesce(ubigeo_detect, ubigeo_from_ccd),
    departamento = norm_txt(safe_col(cur_data(), c_dep)),
    provincia    = norm_txt(safe_col(cur_data(), c_prov)),
    distrito     = norm_txt(safe_col(cur_data(), c_dist)),
    anio         = suppressWarnings(as.integer(safe_col(cur_data(), c_anio))),
    ciiu         = as.character(safe_col(cur_data(), c_ciiu))
  ) |>
  filter(is.na(departamento) | grepl("CALLAO", departamento, fixed = TRUE)) |>
  mutate(n_empresas = 1L) |>
  group_by(ubigeo, departamento, provincia, distrito, anio) |>
  summarise(n_empresas = sum(n_empresas), .groups = "drop") |>
  filter(!is.na(ubigeo))

# 2) DENUNCIAS
den <- read_any_with_year(f_denuncias) |> clean_names()
nms <- names(den)
c_dep   <- detect_col(nms, "dpto.*hecho")
c_prov  <- detect_col(nms, "prov.*hecho")
c_dist  <- detect_col(nms, "dist.*hecho")
c_ubi   <- detect_col(nms, "ubigeo.*hecho")
c_anio  <- detect_col(nms, "^anio$|^ano$|year")
den_clean <- den |>
  transmute(
    departamento = norm_txt(safe_col(cur_data(), c_dep)),
    provincia    = norm_txt(safe_col(cur_data(), c_prov)),
    distrito     = norm_txt(safe_col(cur_data(), c_dist)),
    ubigeo       = fix_ubigeo6(safe_col(cur_data(), c_ubi)),
    anio_raw     = suppressWarnings(as.integer(safe_col(cur_data(), c_anio))),
    anio         = coalesce(anio_raw, as.integer(anio_hint))
  ) |>
  filter(is.na(departamento) | grepl("CALLAO", departamento, fixed = TRUE)) |>
  mutate(n_denuncias = 1L) |>
  group_by(ubigeo, departamento, provincia, distrito, anio) |>
  summarise(n_denuncias = sum(n_denuncias), .groups = "drop") |>
  filter(!is.na(ubigeo))

# 3) LLAMADAS
ll <- read_any_with_year(f_llamadas) |> clean_names()
nms <- names(ll)
c_dep  <- detect_col(nms, "^depa|^depart")
c_prov <- detect_col(nms, "^prov")
c_dist <- detect_col(nms, "^dist")
c_ubi  <- detect_col(nms, "ubigeo$")
c_f1   <- detect_col(nms, "fecha.*llamada")
c_f2   <- detect_col(nms, "fecha.*ocurrenc")
c_f3   <- detect_col(nms, "fecha.*corte|fecha.*del.*corte")
ll_clean <- ll |>
  mutate(
    departamento = norm_txt(safe_col(cur_data(), c_dep)),
    provincia    = norm_txt(safe_col(cur_data(), c_prov)),
    distrito     = norm_txt(safe_col(cur_data(), c_dist)),
    ubigeo       = fix_ubigeo6(safe_col(cur_data(), c_ubi)),
    fecha        = coalesce(ymd_num(safe_col(cur_data(), c_f1)),
                            ymd_num(safe_col(cur_data(), c_f2)),
                            ymd_num(safe_col(cur_data(), c_f3))),
    anio_calc    = year(fecha),
    anio         = coalesce(anio_calc, as.integer(anio_hint))
  ) |>
  filter(is.na(departamento) | grepl("CALLAO", departamento, fixed = TRUE)) |>
  filter(!is.na(anio)) |>
  mutate(n_llamadas = 1L) |>
  group_by(ubigeo, departamento, provincia, distrito, anio) |>
  summarise(n_llamadas = sum(n_llamadas), .groups = "drop") |>
  filter(!is.na(ubigeo))

# 4) CÁMARAS
cam <- read_any_with_year(f_camaras) |> clean_names()
nms <- names(cam)
c_dep   <- detect_col(nms, "^depa|^depart")
c_prov  <- detect_col(nms, "^prov")
c_dist  <- detect_col(nms, "^dist")
c_ubi   <- detect_col(nms, "ubigeo$")
c_fc1   <- detect_col(nms, "fecha.*del.*caso|fecha.*_del_.*caso|fecha.*caso")
c_fc2   <- detect_col(nms, "fecha.*de.*corte|fecha.*_de_.*corte|fecha.*corte")
camaras_clean <- cam |>
  mutate(
    departamento = norm_txt(safe_col(cur_data(), c_dep)),
    provincia    = norm_txt(safe_col(cur_data(), c_prov)),
    distrito     = norm_txt(safe_col(cur_data(), c_dist)),
    ubigeo       = fix_ubigeo6(safe_col(cur_data(), c_ubi)),
    fecha        = coalesce(ymd_num(safe_col(cur_data(), c_fc1)),
                            ymd_num(safe_col(cur_data(), c_fc2))),
    anio_calc    = year(fecha),
    anio         = coalesce(anio_calc, as.integer(anio_hint))
  ) |>
  filter(is.na(departamento) | grepl("CALLAO", departamento, fixed = TRUE)) |>
  filter(!is.na(anio)) |>
  mutate(n_camaras = 1L) |>
  group_by(ubigeo, departamento, provincia, distrito, anio) |>
  summarise(n_camaras = sum(n_camaras), .groups = "drop") |>
  filter(!is.na(ubigeo))

# 5) Replicar CENEC a años observados (si no trae "anio")
years_from_sources <- sort(unique(c(den_clean$anio, ll_clean$anio, camaras_clean$anio)))
years_from_sources <- years_from_sources[!is.na(years_from_sources)]
if (length(years_from_sources) == 0)
  warning("No se detectaron años en DENUNCIAS/LLAMADAS/CÁMARAS; no puedo replicar CENEC.")
if (!("anio" %in% names(cenec_clean)) || all(is.na(cenec_clean$anio))) {
  cenec_stock <- cenec_clean |>
    group_by(ubigeo, departamento, provincia, distrito) |>
    summarise(n_empresas = sum(n_empresas, na.rm = TRUE), .groups = "drop")
  cenec_clean <- tidyr::crossing(cenec_stock, tibble(anio = years_from_sources)) |>
    arrange(distrito, anio)
  message("CENEC replicado a años: ", paste(years_from_sources, collapse = ", "))
}

# 6) Integración de fuentes
data_clean <- cenec_clean |>
  full_join(den_clean,     by = c("ubigeo","departamento","provincia","distrito","anio")) |>
  full_join(ll_clean,      by = c("ubigeo","departamento","provincia","distrito","anio")) |>
  full_join(camaras_clean, by = c("ubigeo","departamento","provincia","distrito","anio")) |>
  mutate(
    across(c(n_empresas, n_denuncias, n_llamadas, n_camaras), ~replace_na(., 0L)),
    across(c(n_empresas, n_denuncias, n_llamadas, n_camaras), ~as.integer(.))
  ) |>
  arrange(distrito, anio)

write.csv(data_clean, file.path(DIR_CLEAN, "data_clean.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 7) Métricas para ML (tasas / 1,000 emp)
data_ml <- data_clean |>
  mutate(
    emp_div = ifelse(n_empresas > 0, n_empresas, NA_real_),
    denuncias_por_1k_emp = ifelse(!is.na(emp_div), 1000 * n_denuncias / emp_div, NA_real_),
    llamadas_por_1k_emp  = ifelse(!is.na(emp_div), 1000 * n_llamadas  / emp_div, NA_real_),
    camaras_por_1k_emp   = ifelse(!is.na(emp_div), 1000 * n_camaras   / emp_div, NA_real_)
  ) |>
  select(ubigeo, departamento, provincia, distrito, anio,
         n_empresas, n_denuncias, n_llamadas, n_camaras,
         denuncias_por_1k_emp, llamadas_por_1k_emp, camaras_por_1k_emp)

write.csv(data_ml, file.path(DIR_OUT, "data_ml.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 8) Chequeos finales
read.csv(file.path(DIR_CLEAN, "data_clean.csv"), fileEncoding = "UTF-8") |>
  summarise(
    n_empresas_tot = sum(n_empresas, na.rm = TRUE),
    n_denuncias_tot = sum(n_denuncias, na.rm = TRUE),
    n_llamadas_tot  = sum(n_llamadas,  na.rm = TRUE),
    n_camaras_tot   = sum(n_camaras,   na.rm = TRUE)
  ) |>
  print()

read.csv(file.path(DIR_CLEAN, "data_clean.csv"), fileEncoding = "UTF-8") |>
  dplyr::count(distrito, anio, wt = n_denuncias, name = "denuncias") |>
  arrange(desc(denuncias), distrito, anio) |>
  head(12) |>
  print()

dc  <- read.csv(file.path(DIR_CLEAN, "data_clean.csv"), fileEncoding = "UTF-8")
dup <- dc |>
  dplyr::count(ubigeo, distrito, anio, name = "n") |>
  filter(n > 1)
if (nrow(dup) > 0) message("⚠️ Ojo: hay llaves duplicadas en data_clean. Revisa el objeto 'dup'.")

cat("\n✅ Listo:",
    "\n- ", file.path(DIR_CLEAN, "data_clean.csv"), " (conteos)",
    "\n- ", file.path(DIR_OUT,   "data_ml.csv"),    " (tasas por 1,000 empresas)\n")

```

