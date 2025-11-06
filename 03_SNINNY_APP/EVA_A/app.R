# =========================================================
# Shiny App: EVA — Explorador territorial + Clusters espaciales (municipal)
# Descargas bajo cada visual (PNG de mapa/serie/ranking/clusters)
# y pie con CSV + PDF + GitHub al final de cada tab
# =========================================================

# ------------------------------
# 1) Paquetes
# ------------------------------
paquetes <- c(
  "tidyverse","ggplot2","readxl","tidyr","dplyr","data.table",
  "scales","zoo","janitor","lubridate","openxlsx",
  "shiny","shinydashboard","plotly","bsicons","bslib","DT",
  "shinyWidgets","httr","jsonlite","tinytex",
  # Espacial / mapas
  "sf","leaflet","stringi","spdep","htmltools",
  # Descargas / gráficos / reporte
  "rmarkdown","knitr","ragg",
  # PNG para mapas (captura widgets html)
  "webshot2","htmlwidgets","mapview",
  # NUEVO: para armar el título dinámico
  "glue"
)
suppressWarnings(invisible(sapply(paquetes, require, character.only = TRUE)))
options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

# Color de borde global
BORDER_COL <- "#a1d99b"

# URL de tu repositorio (cámbiala)
github_url <- "https://github.com/tu_usuario/tu_repo"

# === Ruta fija de la app y del Rmd ===
APP_DIR  <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/EVA_A"
RMD_PATH <- file.path(APP_DIR, "informe_eva.Rmd")
if (!file.exists(RMD_PATH)) {
  stop(sprintf("No encuentro el Rmd en: %s", RMD_PATH))
}

# ------------------------------
# 2) Datos: EVA + Shapefiles
# ------------------------------
eva_df <- readRDS("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/EVA_A/data/011_UPRA_EVA-A.rds")
ruta_shp_mpios <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/EVA_A/data/shp/MGN_ANM_MPIOS.shp"
ruta_shp_dptos <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/EVA_A/data/shp/MGN_ANM_DPTOS.shp"

mpios_sf_raw <- sf::st_read(ruta_shp_mpios, quiet = TRUE)
depto_sf_raw <- sf::st_read(ruta_shp_dptos, quiet = TRUE)

# ------------------------------
# 2.1) Helpers columnas shapefile
# ------------------------------
pick_first <- function(nms, candidates) {
  cand <- candidates[candidates %in% nms]
  if (length(cand) == 0) return(NA_character_)
  cand[1]
}
muni_name_cands       <- c("MUNICIPIO_D","MPIO_CNMBR","NOMBRE_MPIO","NOMBRE_MUNICIP","NOMBRE","MUNICIPIO")
depto_name_cands      <- c("DEPARTAMENTO_D","DPTO_CNMBR","NOMBRE_DPT","NOMBRE_DEPTO","DEPARTAMEN","DEPARTAMENTO")
depto_code_cands      <- c("DPTO_CCDGO","COD_DPTO","DPTO","CODIGO_DPTO","COD_DEPART","DPTO_COD")
muni_depto_code_cands <- c("DPTO_CCDGO","COD_DPTO","DPTO","CODIGO_DPTO","DPTO_COD")

mpios_nms <- names(mpios_sf_raw)
depto_nms <- names(depto_sf_raw)

muni_name_col  <- pick_first(mpios_nms, muni_name_cands)
muni_dpto_code <- pick_first(mpios_nms, muni_depto_code_cands)
depto_name_col <- pick_first(depto_nms, depto_name_cands)
depto_code_col <- pick_first(depto_nms, depto_code_cands)

if (is.na(muni_name_col))  stop("No se encontró la columna de nombre de municipio en mpios_sf_raw.")
if (is.na(muni_dpto_code)) stop("No se encontró la columna de código de departamento en mpios_sf_raw.")
if (is.na(depto_name_col) || is.na(depto_code_col)) stop("Falta nombre o código de dpto en depto_sf_raw.")

# ------------------------------
# 2.2) Construcción de sf normalizados
# ------------------------------
depto_key <- depto_sf_raw |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    dpto_code      = .data[[depto_code_col]],
    DEPARTAMENTO_D = .data[[depto_name_col]]
  )

mpios_sf <- mpios_sf_raw |>
  dplyr::mutate(
    MUNICIPIO_D = .data[[muni_name_col]],
    dpto_code   = .data[[muni_dpto_code]]
  ) |>
  dplyr::left_join(depto_key, by = "dpto_code")

mpios_sf <- sf::st_transform(mpios_sf, 4326) |> sf::st_make_valid() |> sf::st_zm(drop = TRUE, what = "ZM")
depto_sf <- sf::st_transform(depto_sf_raw, 4326) |> sf::st_make_valid() |> sf::st_zm(drop = TRUE, what = "ZM") |>
  dplyr::mutate(DEPARTAMENTO_D = .data[[depto_name_col]])

# Normalizar textos
norm_txt <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
mpios_sf <- mpios_sf |>
  dplyr::mutate(
    MUNICIPIO_D    = norm_txt(MUNICIPIO_D),
    DEPARTAMENTO_D = toupper(norm_txt(DEPARTAMENTO_D))
  )
depto_sf <- depto_sf |>
  dplyr::mutate(
    DEPARTAMENTO_D = toupper(norm_txt(DEPARTAMENTO_D))
  )
eva_df <- eva_df |>
  dplyr::mutate(
    MUNICIPIO_D    = norm_txt(MUNICIPIO_D),
    DEPARTAMENTO_D = toupper(norm_txt(DEPARTAMENTO_D))
  )

# ------------------------------
# 3) UI (tabs)
# ------------------------------
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      /* ====== Estilos base ====== */
      h2#app-title { text-align:center; margin-top: 10px; margin-bottom: 10px; }
      .left-pane  { height: 640px; }
      .right-pane { height: 310px; margin-bottom: 20px; }
      .card { background: #ffffff; border: 1px solid #e6e6e6; border-radius: 12px;
              padding: 12px; box-shadow: 0 1px 6px rgba(0,0,0,0.05); }
      .filter-label { font-weight: 600; margin-bottom: 4px; }
      .top-filters .col-sm-2, .top-filters .col-sm-7 { margin-bottom: 10px; }
      .btn-group .btn { margin-right: 6px; margin-bottom: 6px; }
      .dl-footer { margin-top: 10px; text-align: right; }
      .dl-under { margin-top: 8px; text-align: right; }
      .btn, .btn-default { font-size: 12px; padding: 6px 10px; border-radius: 8px; }
      .btn + .btn { margin-left: 6px; }

      /* Tarjetas visuales con borde verde */
      .viz-card {
        border: 1.5px solid #a1d99b !important;
        border-radius: 12px;
        box-shadow: 0 1px 6px rgba(0,0,0,0.05);
      }

      /* ====== Bordes/acentos #a1d99b para TODOS los filtros ====== */
      .form-control,
      .form-select { border-color: #a1d99b !important; border-width: 1.5px; border-radius: 8px; }
      .form-control:focus,
      .form-select:focus { border-color: #a1d99b !important; box-shadow: 0 0 0 0.2rem rgba(161,217,155,0.15); }

      .selectize-control .selectize-input { border-color: #a1d99b !important; border-width: 1.5px; border-radius: 8px; }
      .selectize-control .selectize-input.focus { border-color: #a1d99b !important; box-shadow: 0 0 0 0.2rem rgba(161,217,155,0.15); }

      .bootstrap-select .dropdown-toggle { border-color: #a1d99b !important; border-width: 1.5px; border-radius: 8px; }
      .bootstrap-select .dropdown-toggle:focus,
      .bootstrap-select .dropdown-toggle:active { border-color: #a1d99b !important; box-shadow: 0 0 0 0.2rem rgba(161,217,155,0.15); }

      /* Slider (ionRangeSlider) */
      .irs--shiny .irs-line { background: #a1d99b22; border-color: #a1d99b; }
      .irs--shiny .irs-bar { background: #a1d99b; border-color: #a1d99b; }
      .irs--shiny .irs-handle > i:first-child { background-color: #a1d99b; border-color: #a1d99b; }
      .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: #a1d99b; border-color: #a1d99b; }

      /* Checks / radios */
      .form-check-input:checked { background-color: #a1d99b; border-color: #a1d99b; }
      .form-check-input:focus { border-color: #a1d99b; box-shadow: 0 0 0 0.2rem rgba(161,217,155,0.15); }

      /* Tabs en negrilla */
      .nav-tabs .nav-link { font-weight: 700; }
      .nav-tabs .nav-link.active { font-weight: 800; }
    "))
  ),
  
  h2("Explorador territorial de indicadores agrícolas (EVA)", id = "app-title"),
  
  bslib::navset_tab(id = "tabs",
                    # =========================
                    # === Tab 1: Explorador ===
                    # =========================
                    bslib::nav_panel("Explorador territorial de indicadores productivos",
                                     # FILTROS
                                     fluidRow(
                                       class = "top-filters",
                                       column(
                                         width = 2,
                                         div(class="filter-label", "Seleccione Indicador:"),
                                         selectInput("f_indicador", NULL,
                                                     choices = c(
                                                       "Área sembrada (Ha)"     = "area_sembrada_ha",
                                                       "Área cosechada (Ha)"    = "area_cosechada_ha",
                                                       "Producción (Ton)"       = "produccion_t",
                                                       "Rendimiento (Ton/Ha)"   = "rendimiento_t_ha"
                                                     ),
                                                     selected = "area_sembrada_ha"
                                         )
                                       ),
                                       column(width = 2, div(class="filter-label", "Seleccione Año:"), uiOutput("anio_ui")),
                                       column(width = 2, div(class="filter-label", "Seleccione Departamento:"),
                                              selectInput("f_depto", NULL,
                                                          choices  = c("Todos", sort(unique(eva_df$DEPARTAMENTO_D))),
                                                          selected = "Todos")
                                       ),
                                       column(width = 2, div(class="filter-label", "Seleccione Municipio:"),
                                              selectInput("f_mpio", NULL, choices = c("Todos", sort(unique(eva_df$MUNICIPIO_D))), selected = "Todos")
                                       ),
                                       column(width = 2, div(class="filter-label", "Seleccione Cultivo:"),
                                              selectInput("f_cultivo", NULL, choices = c("Todos", sort(unique(eva_df$cultivo))), selected = "Todos")
                                       )
                                     ),
                                     # LAYOUT: mapa + dos gráficos
                                     fluidRow(
                                       column(
                                         width = 6,
                                         div(class = "card viz-card left-pane",
                                             h5(textOutput("titulo_mapa")),
                                             div(
                                               style = "display:flex; gap:10px; align-items:center; margin-bottom:8px;",
                                               actionButton("btn_volver", "◀ Volver a Departamentos", class = "btn btn-light"),
                                               strong(textOutput("nivel_txt", inline = TRUE))
                                             ),
                                             leafletOutput("map_eva", height = 560),
                                             div(class = "dl-under",
                                                 downloadButton("dl_png_mapa", label = "PNG — Mapa (simple)")
                                             )
                                         )
                                       ),
                                       column(
                                         width = 6,
                                         div(class = "card viz-card right-pane",
                                             h5(textOutput("titulo_serie")),
                                             plotlyOutput("plot_arriba", height = "240px"),
                                             div(class = "dl-under",
                                                 downloadButton("dl_png_series", label = "PNG — Serie temporal")
                                             )
                                         ),
                                         div(class = "card viz-card right-pane",
                                             h5(textOutput("titulo_ranking")),
                                             plotlyOutput("ranking_abajo", height = "300px"),
                                             div(class = "dl-under",
                                                 downloadButton("dl_png_ranking", label = "PNG — Ranking Top-10")
                                             )
                                         )
                                       )
                                     ),
                                     # Pie del TAB 1: CSV + PDF + GitHub
                                     fluidRow(
                                       column(
                                         width = 12,
                                         div(class = "dl-footer",
                                             downloadButton("dl_csv_expl", label = "Descargar CSV"),
                                             downloadButton("dl_pdf_expl", label = "Informe PDF (Rmd aparte)"),
                                             tags$a(href = github_url, target = "_blank",
                                                    class = "btn btn-dark", style = "color:white;",
                                                    list(bsicons::bs_icon("github"), " GitHub")
                                             )
                                         )
                                       )
                                     )
                    ),
                    
                    # =========================
                    # === Tab 2: CLUSTERS   ===
                    # =========================
                    bslib::nav_panel("Análisis de aglomeración y productos representativos",
                                     fluidRow(
                                       column(
                                         width =6,
                                         div(class="card viz-card",
                                             h5(textOutput("clus_titulo_mapa")),
                                             fluidRow(
                                               column(4,
                                                      selectInput("clus_depto", "Seleccione Departamento:",
                                                                  choices  = sort(unique(eva_df$DEPARTAMENTO_D)), selected = "Todos")
                                               ),
                                               column(3,
                                                      selectInput("clus_indicador", "Seleccione Indicador:",
                                                                  choices = c("Área sembrada (Ha)"  = "area_sembrada_ha",
                                                                              "Área cosechada (Ha)" = "area_cosechada_ha",
                                                                              "Producción (Ton)"    = "produccion_t",
                                                                              "Rendimiento (Ton/Ha)"= "rendimiento_t_ha"),
                                                                  selected = "area_sembrada_ha")
                                               ),
                                               column(2, uiOutput("clus_anio_ui")),
                                               column(3,
                                                      selectInput("clus_cultivo", "Seleccione Cultivo:",
                                                                  choices = c("Todos", sort(unique(eva_df$cultivo))),
                                                                  selected = "Todos")
                                               )
                                             ),
                                             leafletOutput("map_clusters", height = 620),
                                             div(class = "dl-under",
                                                 downloadButton("dl_png_clusters", label = "PNG — Mapa Clusters (simple)")
                                             )
                                         )
                                       ),
                                       column(
                                         width = 6,
                                         div(class="card viz-card",
                                             h5(textOutput("clus_titulo_tabla")),
                                             DT::dataTableOutput("clus_resumen", height = "620px")
                                         )
                                       )
                                     ),
                                     # Pie del TAB 2: CSV + PDF + GitHub (IDs distintos)
                                     fluidRow(
                                       column(
                                         width = 12,
                                         div(class = "dl-footer",
                                             downloadButton("dl_csv_clus", label = "Descargar CSV"),
                                             downloadButton("dl_pdf_clus", label = "Informe PDF (Rmd aparte)"),
                                             tags$a(href = github_url, target = "_blank",
                                                    class = "btn btn-dark", style = "color:white;",
                                                    list(bsicons::bs_icon("github"), " GitHub")
                                             )
                                         )
                                       )
                                     )
                    ),
                    
                    # =========================
                    # === Tab 3: HHI (serie encima del mapa; barras aparte) ===
                    # =========================
                    bslib::nav_panel("Concentración por cultivos (HHI)",
                                     # Controles
                                     fluidRow(
                                       column(
                                         width = 12,
                                         div(class = "card",
                                             fluidRow(
                                               column(3,
                                                      selectInput("hhi_base", "Seleccione Indicador:",
                                                                  choices = c("Producción (Ton)" = "produccion_t",
                                                                              "Área cosechada (Ha)" = "area_cosechada_ha",
                                                                              "Área sembrada (Ha)"  = "area_sembrada_ha"),
                                                                  selected = "produccion_t")
                                               ),
                                               column(3,
                                                      selectInput("hhi_metric", "Seleccione Métrica:",
                                                                  choices = c("HHI (nivel)" = "nivel", "Tasa de crecimiento (%)" = "crec"),
                                                                  selected = "nivel")
                                               ),
                                               column(3,
                                                      div(class="filter-label","Seleccione Año:"),
                                                      uiOutput("hhi_anio_ui")
                                               ),
                                               column(3,
                                                      div(class="filter-label","Seleccione Departamento:"),
                                                      selectInput("hhi_depto", NULL,
                                                                  choices  = c("Todos", sort(unique(eva_df$DEPARTAMENTO_D))),
                                                                  selected = "Todos")
                                               )
                                             ),
                                             fluidRow(
                                               column(3,
                                                      div(class="filter-label","Seleccione Municipio:"),
                                                      selectInput("hhi_mpio", NULL,
                                                                  choices = c("Todos",
                                                                              sort(unique(eva_df$MUNICIPIO_D[eva_df$DEPARTAMENTO_D == "SANTANDER"]))),
                                                                  selected = "Todos")
                                               ),
                                               column(9, htmlOutput("hhi_badge_note"))
                                             )
                                         )
                                       )
                                     ),
                                     
                                     # Dos columnas: IZQUIERDA = BARRAS ; DERECHA = SERIE (arriba) + MAPA (abajo)
                                     fluidRow(
                                       # Izquierda: barras
                                       column(
                                         width = 6,
                                         div(class="card viz-card",
                                             h5(textOutput("hhi_titulo_barras")),
                                             plotlyOutput("hhi_barras", height = "760px")
                                         )
                                       ),
                                       # Derecha: serie arriba + mapa abajo
                                       column(
                                         width = 6,
                                         div(class="card viz-card",
                                             h5(textOutput("hhi_titulo_serie")),
                                             plotlyOutput("hhi_serie", height = "280px")
                                         ),
                                         div(class="card viz-card",
                                             h5(textOutput("hhi_titulo_mapa")),
                                             leafletOutput("hhi_mapa", height = "460px")
                                         )
                                       )
                                     )
                    )
  )
)

# ------------------------------
# 4) Helpers globales
# ------------------------------
safe_chr <- function(x) { if (is.null(x)) "" else as.character(x) }

# Centroides/puntos para etiqueta, robusto y en CRS proyectado
xy_from_poly <- function(sfrow) {
  if (!inherits(sfrow, "sf") || nrow(sfrow) != 1 || any(sf::st_is_empty(sfrow$geometry))) {
    return(c(NA_real_, NA_real_))
  }
  sfrow <- sf::st_zm(sfrow, drop = TRUE, what = "ZM")
  cen <- sfrow |>
    sf::st_transform(3857) |>
    sf::st_point_on_surface() |>
    sf::st_transform(4326)
  cxy <- sf::st_coordinates(cen)
  if (nrow(cxy) < 1 || any(!is.finite(cxy[1, ]))) return(c(NA_real_, NA_real_))
  as.numeric(c(cxy[1], cxy[2]))
}

calc_lisa_and_class <- function(sf_obj, value_col, p_thr = 0.05) {
  sf_obj <- sf::st_make_valid(sf_obj)
  sf_obj <- sf::st_cast(sf_obj, "MULTIPOLYGON", warn = FALSE)
  v <- as.numeric(sf_obj[[value_col]]); v[is.na(v)] <- 0
  sf_obj$.__valor__ <- v
  if (nrow(sf_obj) < 3) {
    sf_obj$Ii <- NA_real_; sf_obj$pvalue <- NA_real_; sf_obj$cluster <- "No significativo"
    return(sf_obj)
  }
  nb <- spdep::poly2nb(sf_obj, queen = TRUE)
  empty_idx <- which(spdep::card(nb) == 0)
  if (length(empty_idx) > 0) {
    coords <- sf::st_coordinates(sf::st_centroid(sf::st_transform(sf_obj, 3857)))
    nb_knn <- spdep::knn2nb(spdep::knearneigh(coords, k = 1))
    for (i in empty_idx) nb[[i]] <- nb_knn[[i]]
  }
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  lm <- suppressWarnings(spdep::localmoran(sf_obj$.__valor__, lw, zero.policy = TRUE))
  sf_obj$Ii     <- lm[, 1]
  sf_obj$pvalue <- lm[, 5]
  m <- mean(sf_obj$.__valor__, na.rm = TRUE)
  sf_obj$cluster <- dplyr::case_when(
    sf_obj$.__valor__ >= m & sf_obj$Ii >  0 & sf_obj$pvalue <= p_thr ~ "Alto-Alto",
    sf_obj$.__valor__ <  m & sf_obj$Ii >  0 & sf_obj$pvalue <= p_thr ~ "Bajo-Bajo",
    sf_obj$.__valor__ >= m & sf_obj$Ii <  0 & sf_obj$pvalue <= p_thr ~ "Alto-Bajo",
    sf_obj$.__valor__ <  m & sf_obj$Ii <  0 & sf_obj$pvalue <= p_thr ~ "Bajo-Alto",
    TRUE ~ "No significativo"
  )
  sf_obj
}

# ======== PALETA DINÁMICA POR INDICADOR (5 rangos, último = color del indicador) ========
mix_hex <- function(c1, c2, t) {
  r1 <- grDevices::col2rgb(c1)[,1] / 255
  r2 <- grDevices::col2rgb(c2)[,1] / 255
  m  <- (1 - t) * r1 + t * r2
  grDevices::rgb(m[1], m[2], m[3])
}

shades5_from_base <- function(base_col) {
  ts <- c(0.10, 0.30, 0.55, 0.78, 1.00)
  vapply(ts, function(tt) mix_hex("#FFFFFF", base_col, tt), character(1))
}

make_bins5 <- function(values) {
  v <- as.numeric(values)
  v <- v[is.finite(v)]
  if (length(v) == 0) return(seq(0, 5))
  qs <- quantile(v, probs = seq(0, 1, length.out = 6), na.rm = TRUE, type = 7)
  qs <- as.numeric(unique(qs))
  if (length(qs) < 6) {
    r <- range(v, na.rm = TRUE)
    if (r[1] == r[2]) r <- c(0, max(1, r[2]))
    qs <- pretty(r, n = 5)
  }
  qs <- sort(unique(qs))
  if (length(qs) < 6) qs <- seq(min(qs), max(qs), length.out = 6)
  qs
}

palBin5_indicator <- function(values, base_col) {
  bins <- make_bins5(values)
  pals <- shades5_from_base(base_col)
  leaflet::colorBin(palette = pals, bins = bins, domain = values,
                    na.color = "#f0f0f0", right = FALSE)
}

# ------------------------------
# 5) SERVER
# ------------------------------
server <- function(input, output, session) {
  # ===== Helpers indicador rendimiento calculado =====
  is_yield <- reactive({ identical(input$f_indicador, "rendimiento_t_ha") })
  clus_is_yield <- reactive({ identical(input$clus_indicador, "rendimiento_t_ha") })
  
  # ===== TÍTULO DINÁMICO (Pestaña 1) =====
  indicador_titulo <- reactive({
    ind <- input$f_indicador
    if (is.null(ind) || is.na(ind)) ind <- "area_sembrada_ha"
    switch(ind,
           "area_sembrada_ha"  = "siembra (hectáreas sembradas)",
           "area_cosechada_ha" = "cosecha (hectáreas cosechadas)",
           "produccion_t"      = "producción (toneladas)",
           "rendimiento_t_ha"  = "productividad (rendimiento, Ton/Ha)",
           ind
    )
  })
  cultivo_frase <- reactive({
    c <- input$f_cultivo
    if (is.null(c) || c == "Todos" || is.na(c)) "" else paste("de", c)
  })
  ambito_frase <- reactive({
    if (nivel_mapa() == "depto") {
      "en el país"
    } else {
      dep <- depto_sel()
      if (is.null(dep) || dep == "Todos" || is.na(dep)) "en el país" else paste("en", dep)
    }
  })
  anio_frase <- reactive({
    a <- input$f_anio
    if (is.null(a) || is.na(a)) "" else paste0(" (", a, ")")
  })
  
  # Color único por indicador (útil para ranking y serie)
  indic_color <- reactive({
    switch(input$f_indicador,
           "area_sembrada_ha"  = "#007A3D",
           "area_cosechada_ha" = "#FBC02D",
           "produccion_t"      = "#F57C00",
           "rendimiento_t_ha"  = "#0099cc",
           "#7f7f7f"
    )
  })
  
  # ===== TAB 1 =====
  # Cascada dpto -> mpio
  observeEvent(input$f_depto, ignoreInit = TRUE, {
    if (is.null(input$f_depto) || input$f_depto == "Todos") {
      munis <- sort(unique(eva_df$MUNICIPIO_D))
    } else {
      munis <- sort(unique(eva_df$MUNICIPIO_D[eva_df$DEPARTAMENTO_D == input$f_depto]))
    }
    updateSelectInput(session, "f_mpio", choices = c("Todos", munis), selected = "Todos")
  })
  
  # Año según Indicador (Tab 1)
  year_col <- reactive({
    ind <- input$f_indicador
    if (is.null(ind)) return("ano_cosechado")
    if (ind == "area_sembrada_ha") "ano_sembrado" else "ano_cosechado"
  })
  output$anio_ui <- renderUI({
    yrs <- sort(unique(na.omit(eva_df[[year_col()]])))
    selectInput("f_anio", NULL, choices = yrs, selected = max(yrs))
  })
  observeEvent(input$f_indicador, ignoreInit = TRUE, {
    yrs <- sort(unique(na.omit(eva_df[[year_col()]])))
    updateSelectInput(session, "f_anio", choices = yrs, selected = max(yrs))
  })
  
  indicador_label <- reactive({
    dplyr::recode(
      input$f_indicador,
      "area_sembrada_ha"  = "Área sembrada (Ha)",
      "area_cosechada_ha" = "Área cosechada (Ha)",
      "produccion_t"      = "Producción (Ton)",
      "rendimiento_t_ha"  = "Rendimiento (Ton/Ha)",
      .default = input$f_indicador
    )
  })
  
  # Datos filtrados (Tab 1)
  datos_filtrados <- reactive({
    df <- eva_df
    if (!is.null(input$f_depto) && input$f_depto != "Todos")  df <- df |> dplyr::filter(.data$DEPARTAMENTO_D == input$f_depto)
    if (!is.null(input$f_mpio)  && input$f_mpio  != "Todos")  df <- df |> dplyr::filter(.data$MUNICIPIO_D == input$f_mpio)
    if (!is.null(input$f_cultivo)  && input$f_cultivo  != "Todos") df <- df |> dplyr::filter(.data$cultivo == input$f_cultivo)
    if (!is.null(input$f_anio)) df <- df |> dplyr::filter(.data[[year_col()]] == input$f_anio)
    
    df <- df |>
      dplyr::mutate(
        prod_num  = suppressWarnings(as.numeric(produccion_t)),
        area_cnum = suppressWarnings(as.numeric(area_cosechada_ha)),
        area_snum = suppressWarnings(as.numeric(area_sembrada_ha))
      )
    
    ind <- input$f_indicador; req(ind)
    if (ind == "rendimiento_t_ha") {
      df$valor <- NA_real_
    } else {
      df$valor <- dplyr::case_when(
        ind == "area_sembrada_ha"  ~ df$area_snum,
        ind == "area_cosechada_ha" ~ df$area_cnum,
        ind == "produccion_t"      ~ df$prod_num,
        TRUE ~ NA_real_
      )
    }
    df
  })
  
  # Badge filtros
  badge_filtros <- reactive({
    c <- if (is.null(input$f_cultivo)  || input$f_cultivo  == "Todos") "Todos" else input$f_cultivo
    tipo <- if (year_col() == "ano_sembrado") "Año (sembrado)" else "Año (cosechado)"
    yr <- if (is.null(input$f_anio)) "" else as.character(input$f_anio)
    htmltools::HTML(sprintf(
      '<div style="background:#fff;padding:6px 10px;border-radius:8px;
                   box-shadow:0 1px 6px rgba(0,0,0,.15);font-size:12px;line-height:1.3;
                   border:1px solid %s;">
         <b>Indicador:</b> %s<br>
         <b>Cultivo:</b> %s<br>
         <b>%s:</b> %s
       </div>', BORDER_COL, indicador_label(), c, tipo, yr))
  })
  
  # Estado de nivel del mapa
  nivel_mapa <- reactiveVal("depto")
  depto_sel  <- reactiveVal(NULL)
  output$nivel_txt <- renderText({
    if (nivel_mapa() == "depto") "Nivel: Departamentos" else paste0("Nivel: Municipios — ", depto_sel())
  })
  
  # --- Títulos Tab 1 ---
  output$titulo_mapa <- renderText({
    ind_txt  <- indicador_titulo()
    cult_txt <- cultivo_frase()
    ambito   <- ambito_frase()
    anio_txt <- anio_frase()
    if (nivel_mapa() == "depto") {
      glue::glue("¿Qué departamentos lideran la {ind_txt}{ifelse(cult_txt=='','', paste0(' ', cult_txt))} {ambito}{anio_txt}?")
    } else {
      dep <- depto_sel()
      glue::glue("¿Qué municipios de {dep} lideran la {ind_txt}{ifelse(cult_txt=='','', paste0(' ', cult_txt))}{anio_txt}?")
    }
  })
  
  # Agregaciones Tab 1
  agg_depto <- reactive({
    df <- datos_filtrados(); req(nrow(df) > 0)
    if (is_yield()) {
      df |>
        dplyr::group_by(DEPARTAMENTO_D) |>
        dplyr::summarise(
          prod = sum(prod_num,  na.rm = TRUE),
          area = sum(area_cnum,  na.rm = TRUE),
          valor = dplyr::if_else(area > 0, prod/area, NA_real_),
          .groups = "drop"
        ) |> dplyr::select(DEPARTAMENTO_D, valor)
    } else {
      df |>
        dplyr::group_by(DEPARTAMENTO_D) |>
        dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
    }
  })
  agg_mpio <- reactive({
    df <- datos_filtrados(); req(nrow(df) > 0)
    if (!is.null(depto_sel())) df <- df |> dplyr::filter(DEPARTAMENTO_D == depto_sel())
    if (is_yield()) {
      df |>
        dplyr::group_by(MUNICIPIO_D) |>
        dplyr::summarise(
          prod = sum(prod_num,  na.rm = TRUE),
          area = sum(area_cnum,  na.rm = TRUE),
          valor = dplyr::if_else(area > 0, prod/area, NA_real_),
          .groups = "drop"
        ) |> dplyr::select(MUNICIPIO_D, valor)
    } else {
      df |>
        dplyr::group_by(MUNICIPIO_D) |>
        dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
    }
  })
  
  output$titulo_serie <- renderText({
    ind_txt  <- indicador_titulo()
    cult_txt <- cultivo_frase()
    ambito   <- ambito_frase()
    rango    <- serie_rango_anios()
    glue::glue("¿Cómo ha evolucionado la {ind_txt}{ifelse(cult_txt==\"\",\"\", paste0(\" \", cult_txt))} {ambito}{ifelse(rango==\"\",\"\", paste0(\" (\", rango, \")\"))}?")
  })
  
  output$titulo_ranking <- renderText({
    ind_lab <- tolower(as.character(indicador_label()))
    cult_txt <- cultivo_frase()
    ambito <- ambito_frase()
    anio <- safe_chr(input$f_anio)
    glue::glue("¿Cuáles son los 10 municipios con mayor {ind_lab}{ifelse(cult_txt==\"\",\"\", paste0(\" \", cult_txt))} {ambito} en {anio}?")
  })
  
  # ========== MAPA Tab 1 ==========
  output$map_eva <- leaflet::renderLeaflet({
    mdat <- depto_sf |>
      dplyr::left_join(agg_depto(), by = "DEPARTAMENTO_D") |>
      dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
    pal  <- palBin5_indicator(mdat$valor, indic_color())
    leaflet::leaflet(mdat) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addPolygons(
        layerId = ~DEPARTAMENTO_D,
        fillColor = ~pal(valor),
        weight = 0.7, color = BORDER_COL, fillOpacity = 0.9,
        label = ~DEPARTAMENTO_D,
        labelOptions = leaflet::labelOptions(direction = "auto", textsize = "12px", sticky = TRUE,
                                             opacity = 0.9, style = list("font-weight" = "600")),
        highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) |>
      leaflet::addLegend(position = "bottomright", pal = pal, values = ~valor,
                         title = indicador_label(),
                         labFormat = leaflet::labelFormat(big.mark = ",")) |>
      leaflet::addControl(badge_filtros(), position = "topright", layerId = "badge_filtros")
  })
  observe({
    leaflet::leafletProxy("map_eva") |>
      leaflet::removeControl("badge_filtros") |>
      leaflet::addControl(badge_filtros(), position = "topright", layerId = "badge_filtros")
  })
  
  dibujar_deptos <- function() {
    mdat <- depto_sf |>
      dplyr::left_join(agg_depto(), by = "DEPARTAMENTO_D") |>
      dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
    pal  <- palBin5_indicator(mdat$valor, indic_color())
    leaflet::leafletProxy("map_eva", data = mdat) |>
      leaflet::clearPopups() |> leaflet::clearShapes() |> leaflet::clearControls() |>
      leaflet::addPolygons(layerId = ~DEPARTAMENTO_D, fillColor = ~pal(valor),
                           weight = 0.7, color = BORDER_COL, fillOpacity = 0.9, label = ~DEPARTAMENTO_D,
                           labelOptions = leaflet::labelOptions(direction = "auto", textsize = "12px", sticky = TRUE,
                                                                opacity = 0.9, style = list("font-weight" = "600")),
                           highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) |>
      leaflet::addLegend(position = "bottomright", pal = pal, values = ~valor,
                         title = indicador_label(),
                         labFormat = leaflet::labelFormat(big.mark = ",")) |>
      leaflet::addControl(badge_filtros(), position = "topright", layerId = "badge_filtros")
  }
  dibujar_mpios <- function(dep) {
    mdat <- mpios_sf |>
      dplyr::filter(DEPARTAMENTO_D == dep) |>
      dplyr::left_join(agg_mpio(), by = "MUNICIPIO_D") |>
      dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
    pal  <- palBin5_indicator(mdat$valor, indic_color())
    leaflet::leafletProxy("map_eva", data = mdat) |>
      leaflet::clearPopups() |> leaflet::clearShapes() |> leaflet::clearControls() |>
      leaflet::addPolygons(layerId = ~MUNICIPIO_D, fillColor = ~pal(valor),
                           weight = 0.4, color = BORDER_COL, fillOpacity = 0.9, label = ~MUNICIPIO_D,
                           labelOptions = leaflet::labelOptions(direction = "auto", textsize = "11px", sticky = TRUE,
                                                                opacity = 0.9, style = list("font-weight" = "600")),
                           highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) |>
      leaflet::addLegend(position = "bottomright", pal = pal, values = mdat$valor,
                         title = paste0(indicador_label(), " — ", dep),
                         labFormat = leaflet::labelFormat(big.mark = ",")) |>
      leaflet::addControl(badge_filtros(), position = "topright", layerId = "badge_filtros")
  }
  
  observeEvent(input$f_depto, {
    dep <- input$f_depto
    if (is.null(dep) || dep == "Todos") { nivel_mapa("depto"); depto_sel(NULL); dibujar_deptos() }
    else { nivel_mapa("mpio"); depto_sel(dep); dibujar_mpios(dep) }
  }, ignoreInit = TRUE)
  
  observeEvent(input$f_mpio, {
    dep <- depto_sel(); mp  <- input$f_mpio
    if (is.null(mp) || mp == "Todos") { if (!is.null(dep)) dibujar_mpios(dep); return() }
    if (is.null(dep) && !is.null(input$f_depto) && input$f_depto != "Todos") {
      dep <- input$f_depto; depto_sel(dep); nivel_mapa("mpio"); dibujar_mpios(dep)
    }
    base_m <- datos_filtrados() |> dplyr::filter(DEPARTAMENTO_D == dep, MUNICIPIO_D == mp)
    if (is_yield()) {
      sprod <- sum(base_m$prod_num,  na.rm = TRUE)
      sarea <- sum(base_m$area_cnum,  na.rm = TRUE)
      total_val <- if (sarea > 0) sprod/sarea else NA_real_
    } else {
      total_val <- base_m |> dplyr::summarise(v = sum(valor, na.rm = TRUE)) |> dplyr::pull(v)
    }
    total_val[is.na(total_val)] <- 0
    cult_txt <- if (is_yield()) {
      sprintf("Rendimiento agg.: %s", scales::comma(round(total_val,2)))
    } else {
      base_m |> dplyr::group_by(cultivo) |> dplyr::summarise(v = sum(valor, na.rm = TRUE), .groups="drop") |>
        dplyr::arrange(dplyr::desc(v)) |> dplyr::slice_head(n = 5) |>
        dplyr::mutate(linea = sprintf("%s: %s", cultivo, scales::comma(round(v,2)))) |> dplyr::pull(linea) |> paste(collapse="<br/>")
    }
    cult_html <- if (length(cult_txt) == 0) "<i>Sin detalles en el filtro</i>" else paste(cult_txt, collapse = "<br/>")
    html <- sprintf("<b>%s</b><br/>%s (total): %s<br/><hr style='margin:6px 0;'>%s",
                    mp, indicador_label(), scales::comma(round(total_val,2)), cult_html)
    mdat <- mpios_sf |> dplyr::filter(DEPARTAMENTO_D == dep, MUNICIPIO_D == mp)
    if (nrow(mdat) == 1) {
      xy <- xy_from_poly(mdat)
      if (all(is.finite(xy))) {
        leaflet::leafletProxy("map_eva") |>
          leaflet::clearPopups() |>
          leaflet::addPopups(lng = xy[1], lat = xy[2], popup = html,
                             options = leaflet::popupOptions(closeOnClick = TRUE)) |>
          leaflet::setView(lng = xy[1], lat = xy[2], zoom = 8)
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$map_eva_shape_click, {
    click <- input$map_eva_shape_click
    if (is.null(click$id)) return()
    if (nivel_mapa() == "depto") { depto_sel(click$id); nivel_mapa("mpio"); dibujar_mpios(click$id) }
    else {
      muni_id <- click$id
      base_m <- datos_filtrados() |> dplyr::filter(DEPARTAMENTO_D == depto_sel(), MUNICIPIO_D == muni_id)
      if (is_yield()) {
        sprod <- sum(base_m$prod_num,  na.rm = TRUE)
        sarea <- sum(base_m$area_cnum,  na.rm = TRUE)
        total_val <- if (sarea > 0) sprod/sarea else NA_real_
      } else {
        total_val <- base_m |> dplyr::summarise(v = sum(valor, na.rm = TRUE)) |> dplyr::pull(v)
      }
      total_val[is.na(total_val)] <- 0
      cult_txt <- if (is_yield()) {
        sprintf("Rendimiento agg.: %s", scales::comma(round(total_val,2)))
      } else {
        base_m |> dplyr::group_by(cultivo) |> dplyr::summarise(v = sum(valor, na.rm = TRUE), .groups="drop") |>
          dplyr::arrange(dplyr::desc(v)) |> dplyr::slice_head(n = 5) |>
          dplyr::mutate(linea = sprintf("%s: %s", cultivo, scales::comma(round(v,2)))) |> dplyr::pull(linea) |> paste(collapse="<br/>")
      }
      cult_html <- if (length(cult_txt) == 0) "<i>Sin detalles en el filtro</i>" else paste(cult_txt, collapse = "<br/>")
      html <- sprintf("<b>%s</b><br/>%s (total): %s<br/><hr style='margin:6px 0;'>%s",
                      muni_id, indicador_label(), scales::comma(round(total_val,2)), cult_html)
      mdat <- mpios_sf |> dplyr::filter(DEPARTAMENTO_D == depto_sel(), MUNICIPIO_D == muni_id)
      if (nrow(mdat) == 1) {
        xy <- xy_from_poly(mdat)
        if (all(is.finite(xy))) {
          leaflet::leafletProxy("map_eva") |>
            leaflet::clearPopups() |>
            leaflet::addPopups(lng = xy[1], lat = xy[2], popup = html,
                               options = leaflet::popupOptions(closeOnClick = TRUE))
        }
      }
    }
  })
  
  observeEvent(input$btn_volver, {
    updateSelectInput(session, "f_depto", selected = "Todos")
    updateSelectInput(session, "f_mpio",  selected = "Todos")
    nivel_mapa("depto"); depto_sel(NULL); dibujar_deptos()
  })
  
  # ======= Serie temporal (Tab 1) =======
  series_data <- reactive({
    base <- eva_df
    if (!is.null(input$f_depto) && input$f_depto != "Todos")  base <- base |> dplyr::filter(.data$DEPARTAMENTO_D == input$f_depto)
    if (!is.null(input$f_mpio)  && input$f_mpio  != "Todos")  base <- base |> dplyr::filter(.data$MUNICIPIO_D == input$f_mpio)
    if (!is.null(input$f_cultivo)  && input$f_cultivo  != "Todos") base <- base |> dplyr::filter(.data$cultivo == input$f_cultivo)
    
    base <- base |>
      dplyr::mutate(
        prod_num  = suppressWarnings(as.numeric(produccion_t)),
        area_cnum = suppressWarnings(as.numeric(area_cosechada_ha)),
        area_snum = suppressWarnings(as.numeric(area_sembrada_ha))
      )
    
    ycol <- if (identical(input$f_indicador, "area_sembrada_ha")) "ano_sembrado" else "ano_cosechado"
    if (is_yield()) {
      base |>
        dplyr::group_by(.data[[ycol]]) |>
        dplyr::summarise(
          prod = sum(prod_num,  na.rm = TRUE),
          area = sum(area_cnum,  na.rm = TRUE),
          valor_total = dplyr::if_else(area > 0, prod/area, NA_real_),
          .groups = "drop"
        ) |>
        dplyr::rename(anio = !!ycol)
    } else {
      ind <- input$f_indicador
      base <- base |> dplyr::mutate(
        valor = dplyr::case_when(
          ind == "area_sembrada_ha"  ~ area_snum,
          ind == "area_cosechada_ha" ~ area_cnum,
          ind == "produccion_t"      ~ prod_num,
          TRUE ~ NA_real_
        )
      )
      base |>
        dplyr::group_by(.data[[ycol]]) |>
        dplyr::summarise(valor_total = sum(valor, na.rm = TRUE), .groups = "drop") |>
        dplyr::rename(anio = !!ycol)
    }
  })
  
  serie_rango_anios <- reactive({
    df <- series_data()
    if (nrow(df) == 0 || all(is.na(df$anio))) return("")
    paste0(min(df$anio, na.rm = TRUE), "–", max(df$anio, na.rm = TRUE))
  })
  
  g_series <- reactive({
    df <- series_data()
    col <- indic_color()
    ggplot(df, aes(x = anio, y = valor_total)) +
      geom_line(linewidth = 0.9, color = col) +
      geom_point(size = 2.2, color = col) +
      scale_x_continuous(breaks = unique(df$anio)) +
      labs(x = if (year_col() == "ano_sembrado") "Año de sembrado" else "Año de cosechado",
           y = indicador_label(),
           title = paste0("Evolución de ", indicador_label())) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
  })
  
  output$plot_arriba <- plotly::renderPlotly({
    df_year <- series_data()
    ycol <- "anio"
    col  <- indic_color()
    plotly::plot_ly(
      data = df_year, x = ~.data[[ycol]], y = ~valor_total,
      type = "scatter", mode = "lines+markers",
      line   = list(color = col, width = 2),
      marker = list(color = col, size = 6),
      hovertemplate = paste0("<b>Año: %{x}</b><br>", as.character(indicador_label()), ": %{y:,}<extra></extra>")
    ) |>
      plotly::layout(
        xaxis = list(title = if (year_col() == "ano_sembrado") "Año de sembrado" else "Año de cosechado", tickmode = "linear", dtick = 1),
        yaxis = list(title = as.character(indicador_label()), separatethousands = TRUE),
        hovermode = "x unified", margin = list(l = 60, r = 20, t = 40, b = 50)
      )
  })
  
  # ======= Ranking Tab 1 =======
  ranking_data <- reactive({
    df <- datos_filtrados(); req(nrow(df) > 0)
    if (is_yield()) {
      df |>
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) |>
        dplyr::summarise(
          prod = sum(prod_num,  na.rm = TRUE),
          area = sum(area_cnum,  na.rm = TRUE),
          valor_total = dplyr::if_else(area > 0, prod/area, NA_real_),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(valor_total)) |>
        dplyr::slice_head(n = 10) |>
        dplyr::arrange(valor_total)
    } else {
      df |>
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) |>
        dplyr::summarise(valor_total = sum(valor, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(valor_total)) |>
        dplyr::slice_head(n = 10) |>
        dplyr::arrange(valor_total)
    }
  })
  
  ranking_scale <- reactive({
    plot_df <- ranking_data()
    mx <- suppressWarnings(max(plot_df$valor_total, na.rm = TRUE))
    if (!is.finite(mx)) mx <- 0
    if (mx > 1e6) {
      list(factor = 1e6, axis_label = paste0(as.character(indicador_label()), " (millones)"), unit_short = "millones")
    } else if (mx > 1e5) {
      list(factor = 1e3, axis_label = paste0(as.character(indicador_label()), " (miles)"), unit_short = "miles")
    } else {
      list(factor = 1, axis_label = as.character(indicador_label()), unit_short = "")
    }
  })
  
  g_ranking <- reactive({
    plot_df <- ranking_data()
    sc <- ranking_scale()
    plot_df <- plot_df |>
      dplyr::mutate(valor_scaled = valor_total / sc$factor)
    ggplot(plot_df,
           aes(x = valor_scaled,
               y = reorder(paste0(MUNICIPIO_D, " (", DEPARTAMENTO_D, ")"), valor_scaled))) +
      geom_col(fill = indic_color()) +
      geom_text(aes(label = scales::comma(round(valor_scaled, 2))),
                hjust = -0.1, size = 3) +
      scale_x_continuous(labels = function(x) scales::comma(x),
                         expand = expansion(mult = c(0, 0.10))) +
      labs(x = sc$axis_label, y = NULL, title = "") +
      theme_minimal(base_size = 12) +
      theme(axis.text.y = element_text(size = 9),
            plot.margin = margin(r = 30),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(color = "#e6e6e6"))
  })
  
  output$ranking_abajo <- plotly::renderPlotly({
    plot_df <- ranking_data()
    sc <- ranking_scale()
    col  <- indic_color()
    if (nrow(plot_df) == 0) {
      return(plotly::plot_ly() |>
               plotly::layout(annotations = list(text = "Sin datos para el ranking", x = 0.5, y = 0.5, showarrow = FALSE)))
    }
    plot_df <- plot_df |>
      dplyr::mutate(
        etiqueta = paste0(MUNICIPIO_D, " (", DEPARTAMENTO_D, ")"),
        valor_scaled = valor_total / sc$factor
      ) |>
      dplyr::arrange(valor_scaled)
    plotly::plot_ly(
      data = plot_df,
      x = ~valor_scaled, y = ~etiqueta, type = "bar", orientation = "h",
      marker = list(color = col),
      text = ~scales::comma(round(valor_scaled, 2)),
      textposition = "outside",
      hovertemplate = paste0(
        "<b>Municipio:</b> %{customdata[0]}",
        "<br><b>Departamento:</b> %{customdata[1]}",
        "<br><b>", as.character(indicador_label()), " (escala eje X):</b> %{x:,} ",
        sc$unit_short,
        "<br><b>", as.character(indicador_label()), " (sin escala):</b> %{customdata[2]:,}",
        "<extra></extra>"
      ),
      customdata = cbind(plot_df$MUNICIPIO_D, plot_df$DEPARTAMENTO_D, plot_df$valor_total)
    ) |>
      plotly::layout(
        xaxis = list(title = sc$axis_label, separatethousands = TRUE, gridcolor = "#e6e6e6"),
        yaxis = list(title = NULL),
        margin = list(l = 110, r = 40, t = 20, b = 40)
      )
  })
  
  # ===== TAB 2: CLUSTERS =====
  clus_year_col <- reactive({
    ind <- input$clus_indicador
    if (is.null(ind)) return("ano_cosechado")
    if (ind == "area_sembrada_ha") "ano_sembrado" else "ano_cosechado"
  })
  output$clus_anio_ui <- renderUI({
    yrs <- sort(unique(na.omit(eva_df[[clus_year_col()]])))
    selectInput("clus_anio", "Año", choices = yrs, selected = max(yrs))
  })
  observeEvent(input$clus_indicador, ignoreInit = TRUE, {
    yrs <- sort(unique(na.omit(eva_df[[clus_year_col()]])))
    updateSelectInput(session, "clus_anio", choices = yrs, selected = max(yrs))
  })
  
  clus_indicador_label <- reactive({
    dplyr::recode(
      safe_chr(input$clus_indicador),
      "area_sembrada_ha"  = "Área sembrada (Ha)",
      "area_cosechada_ha" = "Área cosechada (Ha)",
      "produccion_t"      = "Producción (Ton)",
      "rendimiento_t_ha"  = "Rendimiento (Ton/Ha)",
      .default = safe_chr(input$clus_indicador)
    )
  })
  
  output$clus_titulo_mapa <- renderText({
    ind_lab <- as.character(clus_indicador_label())
    dep     <- safe_chr(input$clus_depto)
    anio    <- safe_chr(input$clus_anio)
    cult    <- safe_chr(input$clus_cultivo)
    dep_txt  <- if (!is.na(dep)  && nzchar(dep))  paste0(" en ", dep) else ""
    anio_txt <- if (!is.na(anio) && nzchar(anio)) paste0(" (", anio, ")") else ""
    cult_txt <- if (!is.na(cult) && cult != "Todos" && nzchar(cult)) paste0(" — cultivo: ", cult) else ""
    glue::glue("¿Cómo se agrupan los municipios según sus niveles de {tolower(ind_lab)} y los de sus vecinos{dep_txt}{anio_txt}{cult_txt}?")
  })
  output$clus_titulo_tabla <- renderText({
    ind_lab <- as.character(clus_indicador_label())
    glue::glue("¿Qué municipios y valores corresponden a cada tipo de clúster ({ind_lab})?")
  })
  
  datos_cluster <- reactive({
    req(input$clus_depto, input$clus_indicador, input$clus_anio)
    df <- eva_df %>% dplyr::filter(DEPARTAMENTO_D == input$clus_depto)
    if (!is.null(input$clus_cultivo) && input$clus_cultivo != "Todos")
      df <- df %>% dplyr::filter(cultivo == input$clus_cultivo)
    df <- df |> dplyr::filter(.data[[clus_year_col()]] == input$clus_anio)
    
    df <- df |>
      dplyr::mutate(
        prod_num  = suppressWarnings(as.numeric(produccion_t)),
        area_cnum = suppressWarnings(as.numeric(area_cosechada_ha)),
        area_snum = suppressWarnings(as.numeric(area_sembrada_ha))
      )
    
    ind <- input$clus_indicador
    if (ind == "rendimiento_t_ha") {
      df$valor <- NA_real_
    } else {
      df$valor <- dplyr::case_when(
        ind == "area_sembrada_ha"  ~ df$area_snum,
        ind == "area_cosechada_ha" ~ df$area_cnum,
        ind == "produccion_t"      ~ df$prod_num,
        TRUE ~ NA_real_
      )
    }
    df
  })
  
  output$map_clusters <- leaflet::renderLeaflet({
    df <- datos_cluster(); req(nrow(df) > 0)
    if (clus_is_yield()) {
      agg_mun <- df %>%
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) %>%
        dplyr::summarise(
          prod  = sum(prod_num,  na.rm = TRUE),
          area  = sum(area_cnum,  na.rm = TRUE),
          valor = dplyr::if_else(area > 0, prod/area, NA_real_),
          .groups = "drop"
        ) %>% dplyr::select(MUNICIPIO_D, DEPARTAMENTO_D, valor)
    } else {
      agg_mun <- df %>%
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) %>%
        dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
    }
    
    mmun <- mpios_sf %>%
      dplyr::filter(DEPARTAMENTO_D == input$clus_depto) %>%
      dplyr::left_join(agg_mun, by = c("MUNICIPIO_D","DEPARTAMENTO_D")) %>%
      dplyr::mutate(
        valor           = ifelse(is.na(valor), 0, valor),
        MUNICIPIO_D     = as.character(MUNICIPIO_D),
        DEPARTAMENTO_D  = as.character(DEPARTAMENTO_D)
      )
    
    mmun <- sf::st_make_valid(mmun)
    mmun <- sf::st_zm(mmun, drop = TRUE, what = "ZM")
    if (!inherits(mmun, "sf")) mmun <- sf::st_as_sf(mmun)
    
    mmun <- calc_lisa_and_class(mmun, "valor", p_thr = 0.50)
    niveles <- c("Alto-Alto","Bajo-Bajo","Alto-Bajo","Bajo-Alto","No significativo")
    mmun$cluster <- factor(as.character(mmun$cluster), levels = niveles)
    p_thr <- 0.50
    
    exp_map <- c(
      "Alto-Alto"        = "Municipio con indicador alto rodeado de vecinos altos. Concentración de buen desempeño.",
      "Bajo-Bajo"        = "Municipio con indicador bajo rodeado de vecinos bajos. Concentración de rezagos.",
      "Alto-Bajo"        = "Municipio destacado (alto) rodeado de rezagos. Posible polo local.",
      "Bajo-Alto"        = "Municipio rezagado rodeado de vecinos con buen desempeño. Brecha relativa.",
      "No significativo" = "No se detecta un patrón espacial claro con el umbral actual."
    )
    accion_map <- c(
      "Alto-Alto"        = "Consolidar: proteger capacidades, invertir en infraestructura y logística, escalar encadenamientos.",
      "Bajo-Bajo"        = "Focalizar: asistencia técnica, infraestructura básica y acceso a financiamiento; intervenciones integrales.",
      "Alto-Bajo"        = "Difundir: programas de extensión para vecinos, articulación regional y cuidado de cuellos de botella.",
      "Bajo-Alto"        = "Cerrar brecha: apoyo específico (tecnología/insumos), conexión a mercados de los centros vecinos.",
      "No significativo" = "Monitorear: revisar datos y contexto; no priorizar intervención territorial solo por patrón spatial."
    )
    
    ind_lab  <- as.character(clus_indicador_label())[1]
    anio_lab <- as.character(input$clus_anio)
    
    mmun$popup_txt <- sprintf(
      "<div style='font-size:13px; line-height:1.25'>
       <b>%s</b><br/>
       <b>Tipo de clúster:</b> %s<br/>
       <b>%s (año %s):</b> %s<br/>
       <hr style='margin:6px 6px'/>
       <b>¿Qué significa?</b><br/>%s<br/>
       <b>Acción sugerida:</b><br/>%s<br/>
       <span style='color:#666'><small>Nota: clústeres definidos con p ≤ %s (LISA: asociación espacial, no causalidad).</small></span>
     </div>",
      as.character(mmun$MUNICIPIO_D),
      as.character(mmun$cluster),
      ind_lab, anio_lab, scales::comma(round(as.numeric(mmun$valor), 2)),
      exp_map[as.character(mmun$cluster)],
      accion_map[as.character(mmun$cluster)],
      format(p_thr, nsmall = 2)
    )
    mmun$popup_txt <- as.character(mmun$popup_txt)
    
    pal <- leaflet::colorFactor(c("#762A83", "#1B7837", "#C2A5CF", "#A6DBA0", "#D9D9D9"), levels = niveles)
    
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::addPolygons(
        data = mmun,
        fillColor = ~pal(cluster),
        color = BORDER_COL, weight = 0.4, fillOpacity = 0.85,
        popup = ~as.character(popup_txt),
        highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      )
  })
  
  output$clus_resumen <- DT::renderDataTable({
    df <- datos_cluster(); req(nrow(df) > 0)
    
    if (clus_is_yield()) {
      agg_mun <- df %>%
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) %>%
        dplyr::summarise(
          prod  = sum(prod_num,  na.rm = TRUE),
          area  = sum(area_cnum,  na.rm = TRUE),
          valor = dplyr::if_else(area > 0, prod/area, NA_real_),
          .groups = "drop"
        ) %>% dplyr::select(MUNICIPIO_D, DEPARTAMENTO_D, valor)
      mmun <- mpios_sf %>%
        dplyr::filter(DEPARTAMENTO_D == input$clus_depto)
    } else {
      agg_mun <- df %>%
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) %>%
        dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
      mmun <- mpios_sf %>%
        dplyr::filter(DEPARTAMENTO_D == input$clus_depto)
    }
    
    mmun <- mmun %>%
      dplyr::left_join(agg_mun, by = c("MUNICIPIO_D","DEPARTAMENTO_D")) %>%
      dplyr::mutate(
        valor           = ifelse(is.na(valor), 0, valor),
        MUNICIPIO_D     = as.character(MUNICIPIO_D),
        DEPARTAMENTO_D  = as.character(DEPARTAMENTO_D)
      )
    
    req(nrow(mmun) >= 3)
    req(any(mmun$valor > 0))
    
    mmun <- calc_lisa_and_class(mmun, "valor", p_thr = 0.50)
    clus_chr <- as.character(mmun$cluster)
    
    total_depto <- sum(mmun$valor, na.rm = TRUE)
    valor_num   <- as.numeric(mmun$valor)
    
    accion_map <- c(
      "Alto-Alto"        = "Consolidar",
      "Bajo-Bajo"        = "Focalizar",
      "Alto-Bajo"        = "Difundir",
      "Bajo-Alto"        = "Cerrar brecha",
      "No significativo" = "Monitorear"
    )
    
    out <- data.frame(
      Municipio                = as.character(mmun$MUNICIPIO_D),
      Departamento             = as.character(mmun$DEPARTAMENTO_D),
      Valor                    = round(valor_num, 2),
      `Participación dpto`     = if (total_depto > 0) paste0(round(100 * valor_num / total_depto, 1), "%") else "0.0%",
      `Ranking departamental`  = rank(-valor_num, ties.method = "min"),
      `Acción sugerida`        = unname(accion_map[clus_chr]),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    out <- out[order(out$`Ranking departamental`), ]
    
    DT::datatable(
      out,
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE, dom = "tip"),
      escape = TRUE
    )
  })
  
  # =========================
  # === MAPAS "SENCILLOS" PARA PNG (Tab 1 y 2) ===
  # =========================
  map_widget_simple <- reactive({
    if (nivel_mapa() == "depto") {
      mdat <- depto_sf |>
        dplyr::left_join(agg_depto(), by = "DEPARTAMENTO_D") |>
        dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
      pal  <- palBin5_indicator(mdat$valor, indic_color())
      
      leaflet::leaflet(mdat, options = leaflet::leafletOptions(zoomControl = FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          fillColor = ~pal(valor),
          weight = 0.5, color = BORDER_COL, fillOpacity = 0.9
        ) |>
        leaflet::addControl(
          html = htmltools::HTML(
            sprintf("<div style='font-weight:600;font-size:14px;background:#fff;padding:6px 8px;border-radius:8px;border:1px solid %s'>
                      %s por departamento — %s
                    </div>",
                    BORDER_COL,
                    as.character(indicador_label()), safe_chr(input$f_anio))
          ),
          position = "topleft"
        )
    } else {
      dep <- depto_sel()
      mdat <- mpios_sf |>
        dplyr::filter(DEPARTAMENTO_D == dep) |>
        dplyr::left_join(agg_mpio(), by = "MUNICIPIO_D") |>
        dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
      pal  <- palBin5_indicator(mdat$valor, indic_color())
      
      leaflet::leaflet(mdat, options = leaflet::leafletOptions(zoomControl = FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          fillColor = ~pal(valor),
          weight = 0.4, color = BORDER_COL, fillOpacity = 0.9
        ) |>
        leaflet::addControl(
          html = htmltools::HTML(
            sprintf("<div style='font-weight:600;font-size:14px;background:#fff;padding:6px 8px;border-radius:8px;border:1px solid %s'>
                      %s por municipios — %s
                    </div>",
                    BORDER_COL,
                    as.character(indicador_label()), safe_chr(input$f_anio))
          ),
          position = "topleft"
        )
    }
  })
  
  map_clusters_simple <- reactive({
    df <- datos_cluster()
    req(nrow(df) > 0)
    if (clus_is_yield()) {
      agg_mun <- df %>%
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) %>%
        dplyr::summarise(
          prod  = sum(prod_num,  na.rm = TRUE),
          area  = sum(area_cnum,  na.rm = TRUE),
          valor = dplyr::if_else(area > 0, prod/area, NA_real_),
          .groups = "drop"
        ) %>% dplyr::select(MUNICIPIO_D, DEPARTAMENTO_D, valor)
    } else {
      agg_mun <- df %>%
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) %>%
        dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
    }
    
    mmun <- mpios_sf %>%
      dplyr::filter(DEPARTAMENTO_D == input$clus_depto) %>%
      dplyr::left_join(agg_mun, by = c("MUNICIPIO_D","DEPARTAMENTO_D")) %>%
      dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
    mmun <- calc_lisa_and_class(mmun, "valor", p_thr = 0.50)
    
    niveles <- c("Alto-Alto","Bajo-Bajo","Alto-Bajo","Bajo-Alto","No significativo")
    pal <- leaflet::colorFactor(c("#762A83", "#1B7837", "#C2A5CF", "#A6DBA0", "#D9D9D9"), levels = niveles)
    
    leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::addPolygons(
        data = mmun,
        fillColor = ~pal(cluster),
        color = BORDER_COL, weight = 0.4, fillOpacity = 0.85
      ) %>%
      leaflet::addControl(
        html = htmltools::HTML(
          sprintf("<div style='font-weight:600;font-size:14px;background:#fff;padding:6px 8px;border-radius:8px;border:1px solid %s'>
                    Clusters LISA — %s (%s)
                  </div>",
                  BORDER_COL,
                  as.character(clus_indicador_label()), safe_chr(input$clus_anio))
        ),
        position = "topleft"
      )
  })
  
  # ---------------- HHI (TAB 3) ----------------
  hhi_year_col <- reactive({
    b <- input$hhi_base
    if (is.null(b)) return("ano_cosechado")
    if (b == "area_sembrada_ha") "ano_sembrado" else "ano_cosechado"
  })
  
  output$hhi_anio_ui <- renderUI({
    yrs <- sort(unique(na.omit(eva_df[[hhi_year_col()]])))
    selectInput("hhi_anio", NULL, choices = yrs, selected = max(yrs))
  })
  
  # Cascada dpto -> mpio (HHI) — solo para la SERIE (municipio específico)
  observeEvent(input$hhi_depto, ignoreInit = TRUE, {
    if (is.null(input$hhi_depto) || input$hhi_depto == "Todos") {
      munis <- sort(unique(eva_df$MUNICIPIO_D))
    } else {
      munis <- sort(unique(eva_df$MUNICIPIO_D[eva_df$DEPARTAMENTO_D == input$hhi_depto]))
    }
    updateSelectInput(session, "hhi_mpio", choices = c("Todos", munis), selected = "Todos")
  })
  
  # Nota metodológica (HHI)
  output$hhi_badge_note <- renderUI({
    htmltools::HTML(
      "<div style='background:#fff;padding:8px 10px;border-radius:8px;border:1px solid #e6e6e6;box-shadow:0 1px 6px rgba(0,0,0,.05);font-size:12px;'>
       <b>Nota:</b> La <b>serie</b> NO depende del filtro de año. <br>
       <b>Barras y mapa</b> usan el <b>año</b> elegido y cambian de nivel: <i>Departamentos</i> si Depto=Todos; <i>Municipios</i> si eliges un Departamento. <br>
       La métrica permite ver el <i>HHI (0–1)</i> o su <i>variación % anual</i>.
     </div>"
    )
  })
  
  # Base numérica común (no filtra por año aquí)
  hhi_base_df <- reactive({
    req(input$hhi_base)
    ycol <- hhi_year_col()
    df <- eva_df |>
      dplyr::mutate(
        prod_num  = suppressWarnings(as.numeric(produccion_t)),
        area_cnum = suppressWarnings(as.numeric(area_cosechada_ha)),
        area_snum = suppressWarnings(as.numeric(area_sembrada_ha)),
        anio_calc = .data[[ycol]]
      )
    base_col <- dplyr::case_when(
      input$hhi_base == "produccion_t"      ~ "prod_num",
      input$hhi_base == "area_cosechada_ha" ~ "area_cnum",
      input$hhi_base == "area_sembrada_ha"  ~ "area_snum",
      TRUE ~ "prod_num"
    )
    df$base_val <- df[[base_col]]
    
    if (!is.null(input$hhi_depto) && input$hhi_depto != "Todos")
      df <- df |> dplyr::filter(.data$DEPARTAMENTO_D == input$hhi_depto)
    if (!is.null(input$hhi_mpio) && input$hhi_mpio != "Todos")
      df <- df |> dplyr::filter(.data$MUNICIPIO_D == input$hhi_mpio)
    df
  })
  
  # Calcular HHI por grupo y año según contexto
  hhi_series_raw <- reactive({
    df <- hhi_base_df(); req(nrow(df) > 0)
    
    if (!is.null(input$hhi_mpio) && input$hhi_mpio != "Todos") {
      sums <- df |>
        dplyr::group_by(.data$MUNICIPIO_D, .data$anio_calc, .data$cultivo) |>
        dplyr::summarise(v = sum(.data$base_val, na.rm = TRUE), .groups = "drop")
      out <- sums |>
        dplyr::group_by(.data$MUNICIPIO_D, .data$anio_calc) |>
        dplyr::mutate(tot = sum(.data$v, na.rm = TRUE),
                      s = dplyr::if_else(.data$tot > 0, .data$v / .data$tot, NA_real_)) |>
        dplyr::summarise(HHI = sum(.data$s^2, na.rm = TRUE), .groups = "drop") |>
        dplyr::rename(grupo = MUNICIPIO_D, anio = anio_calc) |>
        dplyr::filter(grupo == input$hhi_mpio)
      return(out)
    }
    
    if (!is.null(input$hhi_depto) && input$hhi_depto != "Todos") {
      sums <- eva_df |>
        dplyr::filter(.data$DEPARTAMENTO_D == input$hhi_depto) |>
        dplyr::mutate(
          prod_num  = suppressWarnings(as.numeric(produccion_t)),
          area_cnum = suppressWarnings(as.numeric(area_cosechada_ha)),
          area_snum = suppressWarnings(as.numeric(area_sembrada_ha)),
          anio_calc = .data[[hhi_year_col()]],
          base_val  = dplyr::case_when(
            input$hhi_base == "produccion_t"      ~ prod_num,
            input$hhi_base == "area_cosechada_ha" ~ area_cnum,
            input$hhi_base == "area_sembrada_ha"  ~ area_snum,
            TRUE ~ prod_num
          )
        ) |>
        dplyr::group_by(.data$MUNICIPIO_D, .data$anio_calc, .data$cultivo) |>
        dplyr::summarise(v = sum(.data$base_val, na.rm = TRUE), .groups = "drop")
      muni_hhi <- sums |>
        dplyr::group_by(.data$MUNICIPIO_D, .data$anio_calc) |>
        dplyr::mutate(tot = sum(.data$v, na.rm = TRUE),
                      s = dplyr::if_else(.data$tot > 0, .data$v / .data$tot, NA_real_)) |>
        dplyr::summarise(HHI = sum(.data$s^2, na.rm = TRUE), .groups = "drop") |>
        dplyr::rename(anio = anio_calc)
      out <- muni_hhi |>
        dplyr::group_by(.data$anio) |>
        dplyr::summarise(HHI = mean(.data$HHI, na.rm = TRUE), .groups = "drop")
      out$grupo <- paste0("Promedio municipal — ", input$hhi_depto)
      return(out)
    }
    
    sums <- eva_df |>
      dplyr::mutate(
        prod_num  = suppressWarnings(as.numeric(produccion_t)),
        area_cnum = suppressWarnings(as.numeric(area_cosechada_ha)),
        area_snum = suppressWarnings(as.numeric(area_sembrada_ha)),
        anio_calc = .data[[hhi_year_col()]],
        base_val  = dplyr::case_when(
          input$hhi_base == "produccion_t"      ~ prod_num,
          input$hhi_base == "area_cosechada_ha" ~ area_cnum,
          input$hhi_base == "area_sembrada_ha"  ~ area_snum,
          TRUE ~ prod_num
        )
      ) |>
      dplyr::group_by(.data$DEPARTAMENTO_D, .data$anio_calc, .data$cultivo) |>
      dplyr::summarise(v = sum(.data$base_val, na.rm = TRUE), .groups = "drop")
    depto_hhi <- sums |>
      dplyr::group_by(.data$DEPARTAMENTO_D, .data$anio_calc) |>
      dplyr::mutate(tot = sum(.data$v, na.rm = TRUE),
                    s = dplyr::if_else(.data$tot > 0, .data$v / .data$tot, NA_real_)) |>
      dplyr::summarise(HHI = sum(.data$s^2, na.rm = TRUE), .groups = "drop") |>
      dplyr::rename(anio = anio_calc)
    out <- depto_hhi |>
      dplyr::group_by(.data$anio) |>
      dplyr::summarise(HHI = mean(.data$HHI, na.rm = TRUE), .groups = "drop")
    out$grupo <- "Promedio departamental"
    out
  })
  
  # Título serie
  output$hhi_titulo_serie <- renderText({
    base_lab <- dplyr::recode(input$hhi_base,
                              "produccion_t"="Producción", "area_cosechada_ha"="Área cosechada", "area_sembrada_ha"="Área sembrada")
    met_lab <- if (input$hhi_metric == "nivel") "HHI (0–1)" else "Crecimiento del HHI (%)"
    if (!is.null(input$hhi_mpio) && input$hhi_mpio != "Todos")
      return(glue::glue("{met_lab} — {base_lab} — Municipio: {input$hhi_mpio}"))
    if (!is.null(input$hhi_depto) && input$hhi_depto != "Todos")
      return(glue::glue("{met_lab} — {base_lab} — Promedio municipal — {input$hhi_depto}"))
    glue::glue("{met_lab} — {base_lab} — Promedio departamental")
  })
  
  # Helper crecimiento %
  add_growth <- function(df) {
    df |>
      dplyr::arrange(grupo, anio) |>
      dplyr::group_by(grupo) |>
      dplyr::mutate(crec = 100*(HHI / dplyr::lag(HHI) - 1)) |>
      dplyr::ungroup()
  }
  
  output$hhi_serie <- plotly::renderPlotly({
    sr <- hhi_series_raw(); req(nrow(sr) > 0)
    
    if (input$hhi_metric == "crec") {
      sr <- add_growth(sr)
      y <- ~crec; ylab <- "Crecimiento del HHI (%)"
      hover_t <- "<b>Año %{x}</b><br>Δ%% HHI: %{y:.1f}%<extra></extra>"
    } else {
      y <- ~HHI; ylab <- "HHI (0–1)"
      hover_t <- "<b>Año %{x}</b><br>HHI: %{y:.3f}<extra></extra>"
    }
    
    plotly::plot_ly(
      data = sr, x = ~anio, y = y,
      type = "scatter", mode = "lines+markers",
      line = list(width = 2), marker = list(size = 6),
      hovertemplate = hover_t
    ) |>
      plotly::layout(
        yaxis = list(title = ylab),
        xaxis = list(title = "Año", tickmode = "linear", dtick = 1),
        margin = list(l = 60, r = 20, t = 10, b = 40)
      )
  })
  
  # --------- Métrica por AÑO (para mapa y barras) ----------
  hhi_metric_by_year <- reactive({
    req(input$hhi_anio, input$hhi_metric)
    
    if (is.null(input$hhi_depto) || input$hhi_depto == "Todos") {
      # Nivel DEPARTAMENTAL
      df <- eva_df |>
        dplyr::mutate(
          prod_num  = suppressWarnings(as.numeric(produccion_t)),
          area_cnum = suppressWarnings(as.numeric(area_cosechada_ha)),
          area_snum = suppressWarnings(as.numeric(area_sembrada_ha)),
          anio_calc = .data[[hhi_year_col()]],
          base_val  = dplyr::case_when(
            input$hhi_base == "produccion_t"      ~ prod_num,
            input$hhi_base == "area_cosechada_ha" ~ area_cnum,
            input$hhi_base == "area_sembrada_ha"  ~ area_snum,
            TRUE ~ prod_num
          )
        )
      sums <- df |>
        dplyr::group_by(.data$DEPARTAMENTO_D, .data$anio_calc, .data$cultivo) |>
        dplyr::summarise(v = sum(.data$base_val, na.rm = TRUE), .groups = "drop")
      hhi <- sums |>
        dplyr::group_by(.data$DEPARTAMENTO_D, .data$anio_calc) |>
        dplyr::mutate(tot = sum(.data$v, na.rm = TRUE),
                      s = dplyr::if_else(.data$tot > 0, .data$v / .data$tot, NA_real_)) |>
        dplyr::summarise(HHI = sum(.data$s^2, na.rm = TRUE), .groups = "drop") |>
        dplyr::rename(grupo = DEPARTAMENTO_D, anio = anio_calc)
      sr <- hhi
    } else {
      # Nivel MUNICIPAL
      df <- eva_df |>
        dplyr::filter(.data$DEPARTAMENTO_D == input$hhi_depto) |>
        dplyr::mutate(
          prod_num  = suppressWarnings(as.numeric(produccion_t)),
          area_cnum = suppressWarnings(as.numeric(area_cosechada_ha)),
          area_snum = suppressWarnings(as.numeric(area_sembrada_ha)),
          anio_calc = .data[[hhi_year_col()]],
          base_val  = dplyr::case_when(
            input$hhi_base == "produccion_t"      ~ prod_num,
            input$hhi_base == "area_cosechada_ha" ~ area_cnum,
            input$hhi_base == "area_sembrada_ha"  ~ area_snum,
            TRUE ~ prod_num
          )
        )
      sums <- df |>
        dplyr::group_by(.data$MUNICIPIO_D, .data$anio_calc, .data$cultivo) |>
        dplyr::summarise(v = sum(.data$base_val, na.rm = TRUE), .groups = "drop")
      hhi <- sums |>
        dplyr::group_by(.data$MUNICIPIO_D, .data$anio_calc) |>
        dplyr::mutate(tot = sum(.data$v, na.rm = TRUE),
                      s = dplyr::if_else(.data$tot > 0, .data$v / .data$tot, NA_real_)) |>
        dplyr::summarise(HHI = sum(.data$s^2, na.rm = TRUE), .groups = "drop") |>
        dplyr::rename(grupo = MUNICIPIO_D, anio = anio_calc)
      sr <- hhi
    }
    
    if (input$hhi_metric == "crec") {
      sr <- sr |>
        dplyr::arrange(grupo, anio) |>
        dplyr::group_by(grupo) |>
        dplyr::mutate(crec = 100*(HHI / dplyr::lag(HHI) - 1)) |>
        dplyr::ungroup()
      sr |>
        dplyr::filter(anio == input$hhi_anio) |>
        dplyr::select(grupo, valor = crec)
    } else {
      sr |>
        dplyr::filter(anio == input$hhi_anio) |>
        dplyr::select(grupo, valor = HHI)
    }
  })
  
  # Títulos barras / mapa
  output$hhi_titulo_barras <- renderText({
    what <- if (input$hhi_metric == "nivel") "HHI (0–1)" else "Crecimiento del HHI (%)"
    if (is.null(input$hhi_depto) || input$hhi_depto == "Todos")
      paste0(what, " por departamento — ", input$hhi_anio)
    else
      paste0(what, " por municipio — ", input$hhi_depto, " — ", input$hhi_anio)
  })
  output$hhi_titulo_mapa <- renderText({
    what <- if (input$hhi_metric == "nivel") "HHI (0–1)" else "Crecimiento del HHI (%)"
    if (is.null(input$hhi_depto) || input$hhi_depto == "Todos")
      paste0(what, " por departamentos — ", input$hhi_anio)
    else
      paste0(what, " por municipios — ", input$hhi_depto, " — ", input$hhi_anio)
  })
  
  # Barras por año (izquierda)
  output$hhi_barras <- plotly::renderPlotly({
    md <- hhi_metric_by_year(); req(nrow(md) > 0)
    
    md <- md |>
      dplyr::arrange(valor) |>
      dplyr::mutate(grupo_ord = factor(grupo, levels = grupo))
    
    xlab    <- if (input$hhi_metric == "nivel") "HHI (0–1)" else "Crecimiento del HHI (%)"
    lab_txt <- if (input$hhi_metric == "nivel") sprintf("%.3f", md$valor) else sprintf("%.1f%%", md$valor)
    hover_x <- if (input$hhi_metric == "nivel") "%{x:.3f}" else "%{x:.1f}%"
    
    plotly::plot_ly(
      data = md,
      x = ~valor,
      y = ~grupo_ord,
      type = "bar",
      orientation = "h",
      text = lab_txt,
      textposition = "outside",
      hovertemplate = paste0("<b>%{y}</b><br>", xlab, ": ", hover_x, "<extra></extra>"),
      showlegend = FALSE
    ) |>
      plotly::layout(
        xaxis = list(title = xlab, separatethousands = TRUE),
        yaxis = list(
          title = "",
          showticklabels = TRUE,
          showgrid = FALSE,
          zeroline = FALSE,
          ticks = "",
          showline = FALSE
        ),
        margin = list(l = 150, r = 40, t = 10, b = 40)
      )
  })
  
  # Mapa por año (derecha)
  output$hhi_mapa <- leaflet::renderLeaflet({
    md <- hhi_metric_by_year(); req(nrow(md) > 0)
    
    if (is.null(input$hhi_depto) || input$hhi_depto == "Todos") {
      shp <- depto_sf |> dplyr::left_join(md, by = c("DEPARTAMENTO_D" = "grupo"))
    } else {
      shp <- mpios_sf |>
        dplyr::filter(DEPARTAMENTO_D == input$hhi_depto) |>
        dplyr::left_join(md, by = c("MUNICIPIO_D" = "grupo"))
    }
    
    if (input$hhi_metric == "nivel") {
      pal <- leaflet::colorNumeric(colorRampPalette(c("#e0f3ff","#a6c8ff","#3b7ddd","#0b3d91"))(7),
                                   domain = shp$valor, na.color = "#f0f0f0")
      leg_title <- "HHI (0–1)"
      fmt <- function(x) sprintf("%.3f", x)
    } else {
      pal <- leaflet::colorNumeric(colorRampPalette(c("#b2182b","#ef8a62","#f7f7f7","#67a9cf","#2166ac"))(11),
                                   domain = shp$valor, na.color = "#f0f0f0")
      leg_title <- "Crec. HHI (%)"
      fmt <- function(x) sprintf("%.1f%%", x)
    }
    
    leaflet::leaflet(shp) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addPolygons(
        fillColor = ~pal(valor),
        color = BORDER_COL, weight = 0.4, fillOpacity = 0.9,
        label = ~htmltools::HTML(paste0("<b>",
                                        if (is.null(input$hhi_depto) || input$hhi_depto == "Todos") DEPARTAMENTO_D else MUNICIPIO_D,
                                        "</b><br>", leg_title, ": ", fmt(valor)
        )),
        highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) |>
      leaflet::addLegend(position = "bottomright", pal = pal, values = ~valor, title = leg_title)
  })
  
  # =========================
  # === DESCARGAS ===========
  # =========================
  tabla_export <- reactive({
    df <- datos_filtrados()
    if (is_yield()) {
      df <- df |>
        dplyr::group_by(DEPARTAMENTO_D, MUNICIPIO_D, cultivo, !!rlang::sym(year_col())) |>
        dplyr::summarise(
          prod_sum  = sum(prod_num,  na.rm = TRUE),
          area_sum  = sum(area_cnum,  na.rm = TRUE),
          valor_calc = dplyr::if_else(area_sum > 0, prod_sum/area_sum, NA_real_),
          .groups = "drop"
        )
      names(df)[names(df) == year_col()] <- "anio"
    } else {
      df <- df |>
        dplyr::transmute(
          DEPARTAMENTO_D, MUNICIPIO_D, cultivo,
          anio = .data[[year_col()]],
          produccion_t = prod_num,
          area_cosechada_ha = area_cnum,
          area_sembrada_ha  = area_snum,
          valor = valor
        )
    }
    df
  })
  
  # CSV — Tab Explorador
  output$dl_csv_expl <- downloadHandler(
    filename = function() paste0("EVA_", input$f_indicador, "_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(tabla_export(), file, na = "")
  )
  # CSV — Tab Clusters
  output$dl_csv_clus <- downloadHandler(
    filename = function() paste0("EVA_", input$clus_indicador, "_", Sys.Date(), "_clusters.csv"),
    content = function(file) readr::write_csv(tabla_export(), file, na = "")
  )
  
  # PNG — Serie
  output$dl_png_series <- downloadHandler(
    filename = function() paste0("EVA_serie_", input$f_indicador, "_", Sys.Date(), ".png"),
    content = function(file) {
      g <- g_series()
      ggsave(filename = file, plot = g, device = ragg::agg_png, width = 10, height = 5, dpi = 200, units = "in")
    }
  )
  # PNG — Ranking
  output$dl_png_ranking <- downloadHandler(
    filename = function() paste0("EVA_ranking_", input$f_indicador, "_", Sys.Date(), ".png"),
    content = function(file) {
      g <- g_ranking()
      ggsave(filename = file, plot = g, device = ragg::agg_png, width = 10, height = 6, dpi = 200, units = "in")
    }
  )
  # PNG — Mapa explorador (simple)
  output$dl_png_mapa <- downloadHandler(
    filename = function() paste0("EVA_mapa_", input$f_indicador, "_", Sys.Date(), ".png"),
    content = function(file) {
      widget <- map_widget_simple()
      tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget, tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, file = file, vwidth = 1200, vheight = 800, zoom = 2)
    }
  )
  # PNG — Mapa de Clusters (simple)
  output$dl_png_clusters <- downloadHandler(
    filename = function() paste0("EVA_clusters_", input$clus_indicador, "_", Sys.Date(), ".png"),
    content = function(file) {
      widget <- map_clusters_simple()
      tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget, tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, file = file, vwidth = 1200, vheight = 800, zoom = 2)
    }
  )
  
  # PDF — Tab Explorador (Rmd)
  output$dl_pdf_expl <- downloadHandler(
    filename = function() paste0("EVA_informe_", input$f_indicador, "_", Sys.Date(), ".pdf"),
    content  = function(file) {
      params <- list(
        indicador_label = as.character(isolate(indicador_label())),
        depto    = if (is.null(isolate(input$f_depto))) "Todos" else as.character(isolate(input$f_depto)),
        mpio     = if (is.null(isolate(input$f_mpio)))  "Todos" else as.character(isolate(input$f_mpio)),
        cultivo  = if (is.null(isolate(input$f_cultivo))) "Todos" else as.character(isolate(input$f_cultivo)),
        anio_sel = if (is.null(isolate(input$f_anio))) "" else as.character(isolate(input$f_anio)),
        serie    = isolate(series_data()),
        ranking  = isolate(ranking_data()),
        tabla    = isolate(tabla_export())
      )
      out_pdf <- file.path(tempdir(), "informe_expl.pdf")
      rmarkdown::render(
        input         = RMD_PATH,
        output_file   = basename(out_pdf),
        output_dir    = dirname(out_pdf),
        output_format = rmarkdown::pdf_document(
          toc = TRUE, number_sections = TRUE, df_print = "kable",
          latex_engine = "xelatex"
        ),
        params        = params,
        envir         = new.env(parent = globalenv()),
        clean         = TRUE,
        knit_root_dir = dirname(RMD_PATH)
      )
      file.copy(out_pdf, file, overwrite = TRUE) 
    }
  )
  
  # PDF — Tab Clusters
  output$dl_pdf_clus <- downloadHandler(
    filename = function() paste0("EVA_informe_", input$clus_indicador, "_", Sys.Date(), "_clusters.pdf"),
    content  = function(file) {
      params <- list(
        indicador_label = as.character(isolate(clus_indicador_label())),
        depto    = as.character(isolate(input$clus_depto)),
        mpio     = "Todos",
        cultivo  = if (is.null(isolate(input$clus_cultivo))) "Todos" else as.character(isolate(input$clus_cultivo)),
        anio_sel = as.character(isolate(input$clus_anio)),
        serie    = isolate(series_data()),
        ranking  = isolate(ranking_data()),
        tabla    = isolate(tabla_export())
      )
      out_pdf <- file.path(tempdir(), "informe_clus.pdf")
      rmarkdown::render(
        input         = RMD_PATH,
        output_file   = basename(out_pdf),
        output_dir    = dirname(out_pdf),
        output_format = rmarkdown::pdf_document(
          toc = TRUE, number_sections = TRUE, df_print = "kable",
          latex_engine = "xelatex"
        ),
        params        = params,
        envir         = new.env(parent = globalenv()),
        clean         = TRUE,
        knit_root_dir = dirname(RMD_PATH)
      )
      file.copy(out_pdf, file, overwrite = TRUE)
    }
  )
}

# ------------------------------
# 6) Lanzar App
# ------------------------------
shinyApp(ui = ui, server = server)
