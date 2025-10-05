# PASO 9 — app.R (Integración final: Dashboard + Exploratorio + Clustering + Acerca de)
# -------------------------------------------------------------------------------
# Paso 0 (solo recordatorio para despliegue - NO ejecuta nada):
#  - En la raíz del proyecto crea un archivo .rscignore con (sugerido):
#      data_raw/
#      data/tmp/
#      docs/
#      *.qmd
#      *.xlsx
#      *.xls
#      *.pdf
#      .git/
#      .Rproj.user/
#  - Asegúrate de usar rutas relativas (ya se hace aquí).
#  - Despliegue ejemplo forzando app.R:
#      library(rsconnect)
#      rsconnect::setAccountInfo(name="TU_CUENTA", token="...", secret="...")
#      rsconnect::deployApp(appName="observatorio-callao",
#                           appTitle="Observatorio Callao",
#                           appPrimaryDoc="app.R")
# -------------------------------------------------------------------------------

options(shiny.minified = TRUE)
suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tidyr); library(ggplot2)
  library(janitor); library(readr)
})
# Robustez de locale en Linux (no rompe si falla)
try(Sys.setlocale(category = "LC_ALL", locale = "C.UTF-8"), silent = TRUE)

use_dt     <- requireNamespace("DT", quietly = TRUE)
use_plotly <- requireNamespace("plotly", quietly = TRUE)
if (use_dt)     library(DT)
if (use_plotly) library(plotly)

# ----- Helpers -----
pick_col <- function(nms, pats){
  h <- nms[Reduce(`|`, lapply(pats, \(p) grepl(p, nms, ignore.case = TRUE)))]
  if (length(h)) h[1] else NA_character_
}
to_int <- function(x) suppressWarnings(as.integer(x))
`%||%` <- function(x, y) if (!is.null(x)) x else y

# ----- Cargar data_ml.csv (data_out primero, luego raíz) -----
path_ml <- if (file.exists(file.path("data_out","data_ml.csv"))) {
  file.path("data_out","data_ml.csv")
} else if (file.exists("data_ml.csv")) {
  "data_ml.csv"
} else {
  stop("No encuentro 'data_out/data_ml.csv' ni './data_ml.csv'. Ejecuta antes el PASO 4.")
}

df <- readr::read_csv(path_ml, show_col_types = FALSE,
                      locale = readr::locale(encoding = "UTF-8")) |> 
  janitor::clean_names()

# Mapear nombres (soporta variantes)
nms <- names(df)
col_dep   <- pick_col(nms, c("^departamento$"))
col_prov  <- pick_col(nms, c("^provincia$"))
col_dis   <- pick_col(nms, c("^distrito$"))
col_anio  <- pick_col(nms, c("^anio$","^ano$","^year$"))
col_emp   <- pick_col(nms, c("^n_?empresas$","^empresas_stock$","^empresas$"))
col_den   <- pick_col(nms, c("^n_?denuncias$","denuncias$"))
col_lla   <- pick_col(nms, c("^n_?llamadas$","llamadas$"))
col_cam   <- pick_col(nms, c("^n_?camaras$","camaras$"))
col_den1k <- pick_col(nms, c("^denuncias_.*1k.*emp$","denuncias_por_1k_emp$"))
col_lla1k <- pick_col(nms, c("^llamadas_.*1k.*emp$","llamadas_por_1k_emp$"))
col_cam1k <- pick_col(nms, c("^camaras_.*1k.*emp$","camaras_por_1k_emp$"))

rn <- c(
  departamento=col_dep, provincia=col_prov, distrito=col_dis, anio=col_anio,
  n_empresas=col_emp, n_denuncias=col_den, n_llamadas=col_lla, n_camaras=col_cam,
  denuncias_por_1k_emp=col_den1k, llamadas_por_1k_emp=col_lla1k, camaras_por_1k_emp=col_cam1k
)
df <- dplyr::rename(df, !!!setNames(rn[!is.na(rn)], names(rn)[!is.na(rn)]))

need <- c("distrito","anio","n_empresas","n_denuncias","n_llamadas")
miss <- setdiff(need, names(df))
if (length(miss)) stop("Faltan columnas mínimas para la app: ", paste(miss, collapse=", "))

# Campos de cámaras (por si no existen)
if (!"n_camaras" %in% names(df))          df$n_camaras <- 0L
if (!"camaras_por_1k_emp" %in% names(df)) df$camaras_por_1k_emp <- NA_real_

# Tipos
df <- df |>
  mutate(
    anio        = to_int(anio),
    n_empresas  = to_int(n_empresas),
    n_denuncias = to_int(n_denuncias),
    n_llamadas  = to_int(n_llamadas),
    n_camaras   = to_int(n_camaras)
  )

# ----- Opciones dinámicas -----
distritos <- sort(unique(df$distrito))
anios     <- sort(unique(df$anio))
metricas_choices <- intersect(
  c("denuncias_por_1k_emp","llamadas_por_1k_emp","camaras_por_1k_emp"),
  names(df)
)
if (length(metricas_choices) < 2) metricas_choices <- metricas_choices[1]
metric_default_x <- if ("denuncias_por_1k_emp" %in% metricas_choices) "denuncias_por_1k_emp" else metricas_choices[1]
metric_default_y <- if (length(metricas_choices) >= 2 && "llamadas_por_1k_emp" %in% metricas_choices)
  "llamadas_por_1k_emp" else metricas_choices[min(2, length(metricas_choices))]
default_metricas <- unique(na.omit(c(metric_default_x, metric_default_y)))
if (length(default_metricas) == 0) default_metricas <- metricas_choices[1]
if (length(default_metricas) == 1 && length(metricas_choices) >= 2)
  default_metricas <- c(default_metricas, setdiff(metricas_choices, default_metricas)[1])

# =============================== UI DASHBOARD ================================
ui_dash <- fluidPage(
  titlePanel("Observatorio Callao — Tasas por 1,000 empresas"),
  sidebarLayout(
    sidebarPanel(width=3,
                 selectInput("dist","Distrito:", choices=c("Todos", distritos), selected="Todos"),
                 sliderInput("rng","Rango de años:", min=min(anios,na.rm=TRUE), max=max(anios,na.rm=TRUE),
                             value=c(min(anios,na.rm=TRUE), max(anios,na.rm=TRUE)), step=1, sep=""),
                 checkboxGroupInput("metricas","Métricas a visualizar:",
                                    choices=setNames(
                                      metricas_choices,
                                      c("Denuncias / 1k emp","Llamadas / 1k emp","Cámaras / 1k emp")[
                                        match(metricas_choices, c("denuncias_por_1k_emp","llamadas_por_1k_emp","camaras_por_1k_emp"))
                                      ]
                                    ),
                                    selected = default_metricas
                 ),
                 tags$hr(),
                 downloadButton("dl","Descargar CSV filtrado")
    ),
    mainPanel(width=9,
              fluidRow(column(12,
                              if (use_plotly) plotlyOutput("p_barras", height="380px") else plotOutput("p_barras","380px"),
                              helpText("Comparación del último año disponible. Selecciona métricas en la izquierda.")
              )),
              fluidRow(column(12,
                              if (use_plotly) plotlyOutput("p_lineas", height="380px") else plotOutput("p_lineas","380px"),
                              helpText("Evolución temporal promedio o por distrito (según filtro).")
              )),
              tags$hr(),
              fluidRow(column(12, if (use_dt) DTOutput("tab") else tableOutput("tab")))
    )
  )
)

# =============================== SERVER DASHBOARD ============================
server_dash <- function(input, output, session){
  observeEvent(input$metricas, {
    if (is.null(input$metricas) || !length(input$metricas)) {
      updateCheckboxGroupInput(session,"metricas", selected = default_metricas)
    }
  }, ignoreInit = TRUE)
  
  dff <- reactive({
    req(input$rng)
    d <- df |> filter(between(anio, input$rng[1], input$rng[2]))
    if (!is.null(input$dist) && nzchar(input$dist) && input$dist != "Todos") d <- d |> filter(distrito == input$dist)
    d
  })
  
  render_barras <- function(){
    d <- dff(); validate(need(nrow(d)>0, "Sin datos en el rango."))
    yr <- suppressWarnings(max(d$anio, na.rm=TRUE)); validate(need(is.finite(yr), "Año inválido."))
    d <- d |> filter(anio==yr); validate(need(nrow(d)>0, "Sin datos para el último año."))
    mets <- (input$metricas %||% character(0)); mets <- mets[mets %in% names(d)]
    validate(need(length(mets)>=1, "Elige al menos una métrica."))
    d_long <- d |> select(distrito, all_of(mets)) |> pivot_longer(-distrito, names_to="indicador", values_to="valor")
    ggplot(d_long,
           aes(x = reorder(distrito, valor, FUN = max), y = valor, fill = indicador)) +
      geom_col(position="dodge", na.rm=TRUE) + coord_flip() +
      labs(x=NULL, y="Tasa por 1,000 empresas", title=paste0("Último año: ", yr)) +
      theme_minimal(base_size = 12)
  }
  output$p_barras <- if (use_plotly) plotly::renderPlotly(plotly::ggplotly(render_barras())) else renderPlot(print(render_barras()))
  
  render_lineas <- function(){
    d <- dff(); validate(need(nrow(d)>0, "Sin datos en el rango."))
    mets <- (input$metricas %||% character(0)); mets <- mets[mets %in% names(d)]
    validate(need(length(mets)>=1, "Elige al menos una métrica."))
    if (is.null(input$dist) || input$dist=="Todos") {
      d <- d |> group_by(anio) |> summarise(across(all_of(mets), ~mean(.x, na.rm=TRUE)), .groups="drop") |>
        mutate(distrito="PROMEDIO CALLAO")
    } else d <- d |> filter(distrito==input$dist)
    d_long <- d |> select(anio, distrito, all_of(mets)) |> pivot_longer(-c(anio,distrito), names_to="indicador", values_to="valor")
    ggplot(d_long, aes(anio, valor, color=indicador, group=indicador)) +
      geom_line(na.rm=TRUE) + geom_point(na.rm=TRUE) +
      labs(x="Año", y="Tasa por 1,000 empresas",
           title=paste0("Evolución — ", paste(unique(d$distrito), collapse=" / "))) +
      theme_minimal(base_size = 12)
  }
  output$p_lineas <- if (use_plotly) plotly::renderPlotly(plotly::ggplotly(render_lineas())) else renderPlot(print(render_lineas()))
  
  if (use_dt) {
    output$tab <- DT::renderDT(DT::datatable(dff() |> arrange(distrito, anio),
                                             options=list(pageLength=10, scrollX=TRUE), rownames=FALSE))
  } else {
    output$tab <- renderTable(dff() |> arrange(distrito, anio) |> head(20))
  }
  
  output$dl <- downloadHandler(
    filename = function() paste0("subconjunto_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(dff(), file, na = "")
  )
}

# =============================== UI EXPLORATORIO (mini-EDA) ===================
metricas_eda <- metricas_choices
if (!length(metricas_eda)) metricas_eda <- c("denuncias_por_1k_emp")

ui_eda <- fluidPage(
  titlePanel("Exploratorio — Mini EDA"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput("var_eda", "Indicador:", choices = metricas_eda, selected = metricas_eda[1]),
                 sliderInput("rng_eda","Rango de años:", min=min(anios,na.rm=TRUE), max=max(anios,na.rm=TRUE),
                             value=c(max(min(anios,na.rm=TRUE), max(anios,na.rm=TRUE)-5), max(anios,na.rm=TRUE)),
                             step=1, sep=""),
                 selectInput("dist_eda","Distrito (opcional):", choices=c("Todos", distritos), selected="Todos"),
                 checkboxInput("log_y","Escala logarítmica", FALSE)
    ),
    mainPanel(width = 9,
              fluidRow(column(12, plotOutput("eda_hist", height="300px"))),
              fluidRow(column(12, plotOutput("eda_box",  height="320px"))),
              tags$hr(),
              h4("Ficha del dataset (glance)"),
              tableOutput("eda_info"),
              tags$hr(),
              h4("Resumen numérico"),
              tableOutput("eda_sum")
    )
  )
)

server_eda <- function(input, output, session){
  dat_eda <- reactive({
    req(input$var_eda, input$rng_eda)
    d <- df |> filter(between(anio, input$rng_eda[1], input$rng_eda[2]))
    if (!is.null(input$dist_eda) && input$dist_eda != "Todos") d <- d |> filter(distrito == input$dist_eda)
    d |> mutate(var = suppressWarnings(as.numeric(.data[[input$var_eda]]))) |> filter(!is.na(var) & is.finite(var))
  })
  
  output$eda_hist <- renderPlot({
    d <- dat_eda(); validate(need(nrow(d)>0, "Sin datos en el rango/selección."))
    g <- ggplot(d, aes(var)) + geom_histogram(bins = 30) +
      labs(title = paste("Distribución de", input$var_eda),
           x = input$var_eda, y = "Frecuencia") +
      theme_minimal(base_size = 12)
    if (isTRUE(input$log_y)) g <- g + scale_y_log10()
    g
  })
  
  output$eda_box <- renderPlot({
    d <- dat_eda(); validate(need(nrow(d)>0, "Sin datos para boxplot."))
    # Top 12 distritos por mediana (n constante para evitar error)
    dd <- d |>
      dplyr::group_by(distrito) |>
      dplyr::summarise(var = median(var, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(var)) |>
      dplyr::slice_head(n = 12)
    
    ggplot(
      d |> dplyr::semi_join(dd, by = "distrito"),
      aes(x = reorder(distrito, var, FUN = median, na.rm = TRUE), y = var)
    ) +
      geom_boxplot(outlier.alpha = .3) + coord_flip() +
      labs(title = paste("Distribución por distrito —", input$var_eda),
           x = NULL, y = input$var_eda) +
      theme_minimal(base_size = 12)
  })
  
  output$eda_info <- renderTable({
    cols_clave <- intersect(c("n_empresas","n_denuncias","n_llamadas","n_camaras",
                              "denuncias_por_1k_emp","llamadas_por_1k_emp","camaras_por_1k_emp"),
                            names(df))
    n_na <- sapply(df[cols_clave], function(z) sum(is.na(z)))
    p_na <- round(100 * n_na / nrow(df), 2)
    
    tibble::tibble(
      filas_total      = nrow(df),
      distritos        = dplyr::n_distinct(df$distrito),
      anio_min         = suppressWarnings(min(df$anio, na.rm = TRUE)),
      anio_max         = suppressWarnings(max(df$anio, na.rm = TRUE)),
      tasas_disponibles= paste(intersect(c("denuncias_por_1k_emp","llamadas_por_1k_emp","camaras_por_1k_emp"),
                                         names(df)), collapse = ", ")
    ) |>
      dplyr::bind_rows(
        tibble::tibble(
          filas_total = NA, distritos = NA, anio_min = NA, anio_max = NA,
          tasas_disponibles = paste0("%NA (claves): ",
                                     paste(paste(names(p_na), p_na, "%"), collapse = " | "))
        )
      )
  }, rownames = FALSE)
  
  output$eda_sum <- renderTable({
    d <- dat_eda()
    tibble(
      n       = nrow(d),
      media   = mean(d$var, na.rm = TRUE),
      mediana = median(d$var, na.rm = TRUE),
      sd      = sd(d$var, na.rm = TRUE),
      min     = min(d$var, na.rm = TRUE),
      p25     = quantile(d$var, .25, na.rm = TRUE),
      p75     = quantile(d$var, .75, na.rm = TRUE),
      max     = max(d$var, na.rm = TRUE)
    ) |> mutate(across(where(is.numeric), ~round(.x, 3)))
  }, rownames = FALSE)
}

# =============================== UI/Server Clustering =========================
vars_posibles <- intersect(c("denuncias_por_1k_emp","llamadas_por_1k_emp","camaras_por_1k_emp"), names(df))
if (length(vars_posibles) < 2) {
  stop("Clustering requiere ≥2 variables de tasa (p.ej., denuncias_por_1k_emp, llamadas_por_1k_emp).")
}
sel_inicial <- head(vars_posibles, n = max(2, length(vars_posibles)))

ui_clu <- fluidPage(
  titlePanel("Clustering — Distritos (tasas por 1,000 empresas)"),
  wellPanel(
    tags$small(
      strong("Cómo leer: "), "El scatter muestra PC1–PC2 (PCA sobre las variables seleccionadas). ",
      "El número de clusters (k) para K-means/Ward se sugiere con ", em("Elbow"),
      " y ", em("Silhouette"), ". DBSCAN detecta grupos de densidad y marca ruido (cluster 0)."
    )
  ),
  sidebarLayout(
    sidebarPanel(width=3,
                 selectInput("anio_clu","Año:", choices=sort(unique(df$anio)), selected=max(df$anio,na.rm=TRUE)),
                 checkboxGroupInput("vars_clu","Variables (≥2):", choices=vars_posibles, selected=sel_inicial),
                 checkboxInput("scale_x","Estandarizar (recomendado)", TRUE), hr(),
                 radioButtons("metodo","Método:", choices=c("K-means","Jerárquico (Ward)","DBSCAN"), selected="K-means"),
                 conditionalPanel("input.metodo == 'K-means'", numericInput("k_kmeans","k clusters",3,2,10,1)),
                 conditionalPanel("input.metodo == 'Jerárquico (Ward)'", numericInput("k_hclust","k cortes",3,2,10,1)),
                 conditionalPanel("input.metodo == 'DBSCAN'",
                                  numericInput("eps","eps (radio)", .8, .05, 5, .05), numericInput("minPts","minPts", 3, 2, 50, 1)
                 ),
                 actionButton("run_clu","Ejecutar clustering", class="btn-primary"), hr(),
                 downloadButton("dl_clu","Descargar asignación CSV")
    ),
    mainPanel(width=9,
              fluidRow(column(12, plotOutput("plot_clusters", height="420px"))),
              conditionalPanel("input.metodo == 'Jerárquico (Ward)'", hr(), fluidRow(column(12, plotOutput("plot_dendro","300px")))),
              hr(), h4("Validación"), fluidRow(column(6, plotOutput("plot_elbow","260px")), column(6, plotOutput("plot_sil","260px"))),
              hr(), h4("Asignación distrito–cluster"), tableOutput("tab_asig"), hr(),
              h4("Resumen por cluster (promedios)"), tableOutput("tab_resumen")
    )
  )
)

server_clu <- function(input, output, session){
  observeEvent(input$vars_clu, { if (length(input$vars_clu)<2) updateCheckboxGroupInput(session,"vars_clu", selected=head(vars_posibles,2)) }, ignoreInit=TRUE)
  
  base_clu <- reactive({
    req(input$anio_clu, length(input$vars_clu)>=2)
    out <- df |> filter(anio==input$anio_clu) |> select(distrito, all_of(input$vars_clu)) |> distinct()
    for (v in input$vars_clu) {
      out[[v]] <- suppressWarnings(as.numeric(out[[v]]))
      out[[v]][is.infinite(out[[v]])] <- NA_real_
      med <- suppressWarnings(stats::median(out[[v]], na.rm=TRUE))
      if (is.finite(med)) out[[v]][is.na(out[[v]])] <- med
    }
    out
  })
  
  X_mat <- reactive({ X <- as.matrix(base_clu()[, input$vars_clu, drop=FALSE]); if (isTRUE(input$scale_x)) X <- scale(X); X })
  pca2  <- reactive({ stats::prcomp(X_mat(), center=FALSE, scale.=FALSE) })
  
  res_clu <- eventReactive(input$run_clu, {
    X <- X_mat(); validate(need(nrow(X)>=2, "Muy pocos casos."))
    if (input$metodo=="K-means"){
      k <- input$k_kmeans; validate(need(is.finite(k)&&k>=2, "k ≥ 2"))
      set.seed(123); km <- stats::kmeans(X, centers=k, nstart=25)
      list(metodo="kmeans", clusters=km$cluster, modelo=km)
    } else if (input$metodo=="Jerárquico (Ward)"){
      k <- input$k_hclust; validate(need(is.finite(k)&&k>=2, "k ≥ 2"))
      hc <- stats::hclust(stats::dist(X), method="ward.D2")
      list(metodo="hclust", clusters=stats::cutree(hc,k), modelo=hc)
    } else {
      if (!requireNamespace("dbscan", quietly = TRUE)) stop("Instala 'dbscan'.")
      db <- dbscan::dbscan(X, eps=input$eps, minPts=input$minPts)
      list(metodo="dbscan", clusters=db$cluster, modelo=db)
    }
  }, ignoreInit=TRUE)
  
  output$plot_clusters <- renderPlot({
    req(res_clu())
    d <- base_clu(); pc <- pca2()
    pts <- as.data.frame(pc$x[,1:2,drop=FALSE]); names(pts) <- c("PC1","PC2")
    pts$distrito <- d$distrito; pts$cluster <- factor(res_clu()$clusters)
    ggplot(pts, aes(PC1,PC2, color=cluster, label=distrito)) +
      geom_point(size=3) + geom_text(vjust=-1, show.legend=FALSE, size=3) +
      labs(title=paste0("Método: ", res_clu()$metodo, " — Año: ", input$anio_clu),
           x="PC1", y="PC2", color="Cluster") + theme_minimal(base_size = 12)
  })
  
  output$plot_dendro <- renderPlot({
    req(res_clu()); validate(need(res_clu()$metodo=="hclust",""))
    plot(res_clu()$modelo, main="Dendrograma (Ward.D2)")
    rect.hclust(res_clu()$modelo, k=input$k_hclust, border="steelblue")
  })
  
  sse_for_clusters <- function(X, cl){
    s <- 0
    for (k in sort(unique(cl))){
      id <- which(cl==k)
      if (length(id)>1){
        C <- colMeans(X[id,,drop=FALSE])
        D <- sweep(X[id,,drop=FALSE],2,C,"-")
        s <- s + sum(D*D)
      }
    }
    s
  }
  k_grid <- reactive({ n <- nrow(X_mat()); if (!is.finite(n) || n<3) integer(0) else 2:min(8, n-1) })
  
  output$plot_elbow <- renderPlot({
    X <- X_mat(); ks <- k_grid(); validate(need(length(ks)>0,"n muy pequeño"))
    if (input$metodo=="K-means"){
      sse <- sapply(ks, function(k){ set.seed(123); km <- stats::kmeans(X, k, nstart=15); sum(km$withinss) })
    } else if (input$metodo=="Jerárquico (Ward)"){
      hc <- stats::hclust(stats::dist(X), method="ward.D2")
      sse <- sapply(ks, function(k){ cl <- stats::cutree(hc,k); sse_for_clusters(X,cl) })
    } else { plot.new(); title("Elbow no aplica a DBSCAN"); return(invisible(NULL)) }
    ggplot(data.frame(k=ks, SSE=sse), aes(k,SSE)) + geom_line() + geom_point() +
      labs(title="Elbow (SSE vs k)", x="k", y="SSE") + theme_minimal(base_size = 12)
  })
  
  output$plot_sil <- renderPlot({
    if (!requireNamespace("cluster", quietly=TRUE)){ plot.new(); title("Instala 'cluster' para Silhouette"); return(invisible(NULL)) }
    X <- X_mat(); ks <- k_grid(); validate(need(length(ks)>0,"n muy pequeño"))
    if (input$metodo=="K-means"){
      sil <- sapply(ks, function(k){ set.seed(123); km <- stats::kmeans(X,k,nstart=15); m <- cluster::silhouette(km$cluster, dist(X)); mean(m[,3], na.rm=TRUE) })
    } else if (input$metodo=="Jerárquico (Ward)"){
      hc <- stats::hclust(stats::dist(X), method="ward.D2")
      sil <- sapply(ks, function(k){ cl <- stats::cutree(hc,k); m <- cluster::silhouette(cl, dist(X)); mean(m[,3], na.rm=TRUE) })
    } else { plot.new(); title("Silhouette por k no aplica a DBSCAN"); return(invisible(NULL)) }
    ggplot(data.frame(k=ks, silhouette=sil), aes(k, silhouette)) + geom_line() + geom_point() +
      labs(title="Silhouette promedio vs k", x="k", y="Silhouette") + theme_minimal(base_size = 12)
  })
  
  asig_df <- reactive({ req(res_clu()); base_clu() |> mutate(cluster=as.integer(res_clu()$clusters)) |> arrange(cluster, distrito) })
  resumen_df <- reactive({ asig_df() |> group_by(cluster) |> summarise(across(all_of(input$vars_clu), ~round(mean(.x, na.rm=TRUE),3)), .groups="drop") })
  output$tab_asig    <- renderTable(asig_df(), rownames=FALSE)
  output$tab_resumen <- renderTable(resumen_df(), rownames=FALSE)
  output$dl_clu <- downloadHandler(
    filename = function() paste0("clusters_", gsub("\\s+|\\(|\\)","", input$metodo), "_", input$anio_clu, ".csv"),
    content  = function(file) utils::write.csv(asig_df(), file, row.names=FALSE, fileEncoding="UTF-8")
  )
}

# =============================== UI ACERCA DE ================================
ui_about <- fluidPage(
  tags$head(tags$style(HTML("
    .about h3 { margin-top: 1.2rem; }
    .about p  { text-align: justify; }
    .about ul { margin-top: .25rem; }
  "))),
  div(class = "about",
      h2("Acerca de / Metodología"),
      p("Este observatorio resume indicadores de seguridad y actividad empresarial para la Provincia Constitucional del Callao. ",
        "Integra múltiples fuentes oficiales y expone tasas comparables por 1,000 empresas."),
      h3("Fuentes"),
      tags$ul(
        tags$li(strong("CENEC:"), " stock anual de empresas."),
        tags$li(strong("Denuncias Policiales:"), " conteos por distrito y año."),
        tags$li(strong("Central de Emergencias (llamadas):"), " registros por fecha."),
        tags$li(strong("ENAPRES (INEI):"), " módulo de Seguridad Ciudadana (percepción y victimización)."),
        tags$li(strong("Cámaras de videovigilancia:"), " reportes con fecha de caso/corte.")
      ),
      h3("Limpieza e Integración"),
      p("Se estandarizaron nombres (mayúsculas, sin tildes), se validó el ",
        "código de ", code("ubigeo"), " y se agregaron conteos por ",
        code("ubigeo–distrito–año"), ". Cuando CENEC no reportó año, ",
        "se replicó su stock a los años observados en las otras fuentes."),
      h3("Indicadores"),
      tags$ul(
        tags$li(code("denuncias_por_1k_emp"), ": 1000 × denuncias / empresas"),
        tags$li(code("llamadas_por_1k_emp"),  ": 1000 × llamadas / empresas"),
        tags$li(code("camaras_por_1k_emp"),   ": 1000 × cámaras / empresas (si está disponible)")
      ),
      h3("Imputaciones y Robustez"),
      p("Si el stock de empresas de un año es cero/NA, se imputa con la mediana del distrito. ",
        "Las tasas se recalculan con el stock imputado. También se aplicó un winsorize suave (1%-99%) para mitigar outliers."),
      h3("Clustering"),
      p("Se ofrece K-means, Jerárquico (Ward) y DBSCAN. La visualización usa PCA (PC1–PC2). ",
        "Para K-means/Ward se muestran curvas de validación: Elbow (SSE vs k) y Silhouette promedio vs k."),
      h3("Descarga y Reproducibilidad"),
      p("El botón de descarga exporta el subconjunto filtrado a CSV. ",
        "El flujo de preparación de datos se documenta en los pasos 2–4 del proyecto.")
  )
)

# =============================== UI INTRO / OBJETIVOS =========================
ui_intro <- fluidPage(
  titlePanel("Introducción / Objetivos"),
  fluidRow(
    column(12,
           tags$div(
             style = "max-width:900px;",
             h3("Objetivo"),
             p("Construir un observatorio interactivo para la Provincia Constitucional del Callao ",
               "que combine indicadores de seguridad y actividad empresarial, con énfasis en tasas ",
               "por 1,000 empresas y segmentación de distritos mediante clustering."),
             h3("Dataset y justificación"),
             p("Integramos fuentes oficiales (CENEC, Denuncias Policiales, Central 105, ENAPRES y ",
               "reportes de cámaras). Estandarizamos nombres y calculamos tasas por 1,000 empresas ",
               "para hacer comparables los distritos y los años."),
             h3("Público objetivo"),
             tags$ul(
               tags$li("Autoridades locales y Policía (priorización territorial)."),
               tags$li("Observatorios y áreas de planeamiento (seguimiento de indicadores)."),
               tags$li("Academia/estudiantes (reutilización y aprendizaje).")
             ),
             h3("Qué contiene el dashboard"),
             tags$ul(
               tags$li(strong("Dashboard:"), " últimas comparaciones y evolución temporal, filtros por distrito/año."),
               tags$li(strong("Exploratorio:"), " histogramas/boxplots y resumen numérico rápido."),
               tags$li(strong("Clustering:"), " K-means, Jerárquico (Ward) y DBSCAN, con validaciones Elbow y Silhouette."),
               tags$li(strong("Acerca de:"), " fuentes y metodología.")
             )
           )
    )
  )
)

# =============================== APP COMBINADA ================================
ui_app <- navbarPage("Observatorio Callao",
                     tabPanel("Intro / Objetivos", ui_intro),
                     tabPanel("Dashboard",    ui_dash),
                     tabPanel("Exploratorio", ui_eda),
                     tabPanel("Clustering",   ui_clu),
                     tabPanel("Acerca de",    ui_about)
)

server_app <- function(input, output, session){
  environment(server_dash) <- environment()
  environment(server_eda)  <- environment()
  environment(server_clu)  <- environment()
  server_dash(input, output, session)
  server_eda(input, output, session)
  server_clu(input, output, session)
}

# ===== Helper de despliegue (opcional; no altera la app) ======================
# Úsalo desde la consola si ya configuraste tu cuenta con setAccountInfo().
# Ejemplo:
#   deploy_to_shinyapps("observatorio_callao_demo", account = "vivisg18")
deploy_to_shinyapps <- function(appName = "observatorio_callao_demo",
                                account = "vivisg18",
                                appTitle = "Observatorio Callao — Demo",
                                applicationDir = ".") {
  if (!requireNamespace("rsconnect", quietly = TRUE))
    stop("Instala primero el paquete 'rsconnect'.")
  rsconnect::deployApp(
    appName        = appName,
    appTitle       = appTitle,
    account        = account,
    applicationDir = applicationDir,
    appPrimaryDoc  = "app.R"   # <- fuerza a usar este archivo como entrada
  )
  message("Publicada. Para logs en vivo:\n",
          "rsconnect::showLogs(appName = '", appName,
          "', account = '", account, "', streaming = TRUE)")
}

shinyApp(ui_app, server_app)
