# =========================================================
# Shiny App: EVA — Explorador territorial + Clusters espaciales (municipal)
# (CORREGIDA)
# + Un solo botón PDF en el primer tab
# + Renderiza DIRECTAMENTE Informe_descargable.Rmd
# + PDF estable vía archivo temporal + copia final
# + logo leído desde Descargas/LOGO_PLATEA.png
# + exportación robusta de PNG para informe
# + retry especial para mapa de clusters
# =========================================================

# ------------------------------
# 1) Paquetes
# ------------------------------
paquetes <- c(
  "tidyverse","ggplot2","readxl","tidyr","dplyr","data.table",
  "scales","zoo","janitor","lubridate","openxlsx",
  "shiny","shinydashboard","plotly","bsicons","bslib","DT",
  "shinyWidgets","httr","jsonlite","tinytex",
  "sf","leaflet","stringi","spdep","htmltools",
  "rmarkdown","knitr","ragg",
  "webshot2","htmlwidgets","mapview",
  "glue"
)

paquetes <- as.character(paquetes)
paquetes <- paquetes[!is.na(paquetes) & nzchar(paquetes)]

suppressWarnings(invisible(lapply(paquetes, function(p) {
  suppressPackageStartupMessages(require(p, character.only = TRUE))
})))

options(stringsAsFactors = FALSE)
options(OutDec = ",")
sf::sf_use_s2(FALSE)

# ------------------------------
# 1.1) Helpers globales básicos
# ------------------------------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

comma_es <- function(x, accuracy = NULL) {
  scales::comma(
    x,
    accuracy     = accuracy,
    big.mark     = ".",
    decimal.mark = ","
  )
}

safe_chr <- function(x) {
  if (is.null(x)) "" else as.character(x)
}

sanitize_filename <- function(x){
  x <- as.character(x)
  x <- gsub("[/\\\\:*?\"<>|]", "_", x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("__+", "_", x)
  trimws(x)
}

plot_vacio_gg <- function(txt = "Sin datos para la selección actual") {
  ggplot() +
    annotate("text", x = 1, y = 1, label = txt, size = 5) +
    xlim(0, 2) + ylim(0, 2) +
    theme_void()
}

BORDER_COL <- "#a1d99b"
github_url <- "https://github.com/tu_usuario/tu_repo"

# =========================================================
# EXPORTACIÓN TIPO IDM / RMD
# =========================================================
get_app_root <- function(){
  normalizePath(shiny::getShinyOption("appDir") %||% getwd(), winslash = "/", mustWork = FALSE)
}

app_root      <- get_app_root()
EXPORT_DIR    <- file.path(app_root, "Descargas")
dir.create(EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)

ruta_rmd_root <- file.path(app_root, "Informe_descargable.Rmd")
ruta_rmd_data <- file.path(app_root, "data", "Informe_descargable.Rmd")
ruta_rmd      <- if (file.exists(ruta_rmd_root)) ruta_rmd_root else ruta_rmd_data

IMG_TAB1_MAP   <- file.path(EXPORT_DIR, "eva_tab1_mapa.png")
IMG_TAB1_SER   <- file.path(EXPORT_DIR, "eva_tab1_serie.png")
IMG_TAB1_RANK  <- file.path(EXPORT_DIR, "eva_tab1_ranking.png")
IMG_TAB2_MAP   <- file.path(EXPORT_DIR, "eva_tab2_clusters.png")
IMG_TAB3_BAR   <- file.path(EXPORT_DIR, "eva_tab3_barras.png")
IMG_TAB3_SER   <- file.path(EXPORT_DIR, "eva_tab3_serie.png")

save_widget_png <- function(widget, out_png, vwidth, vheight, delay = 1.2){
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  
  tmp_dir  <- tempfile("wshot_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  lib_dir  <- file.path(tmp_dir, "lib")
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
  tmp_html <- file.path(tmp_dir, "widget.html")
  
  htmlwidgets::saveWidget(widget, file = tmp_html, selfcontained = FALSE, libdir = lib_dir)
  
  webshot2::webshot(
    url     = tmp_html,
    file    = out_png,
    vwidth  = vwidth,
    vheight = vheight,
    delay   = delay
  )
  
  file.exists(out_png) && is.finite(file.info(out_png)$size) && file.info(out_png)$size > 0
}

save_widget_png_retry <- function(widget, out_png, vwidth, vheight, delay_base = 1.2){
  delays <- c(delay_base, delay_base + 1.5, delay_base + 3)
  for (d in delays) {
    ok <- tryCatch(
      save_widget_png(widget, out_png, vwidth = vwidth, vheight = vheight, delay = d),
      error = function(e) FALSE
    )
    if (isTRUE(ok)) return(TRUE)
  }
  FALSE
}

save_gg_png <- function(plot_obj, out_png, width = 1800, height = 1100, res = 150){
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  ragg::agg_png(out_png, width = width, height = height, res = res)
  print(plot_obj)
  grDevices::dev.off()
  file.exists(out_png) && is.finite(file.info(out_png)$size) && file.info(out_png)$size > 0
}

save_leaflet_png <- function(widget, file, vwidth = 1800, vheight = 1200, zoom = 2, delay = 1.8) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  
  tmp_dir  <- tempfile("leafshot_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  tmp_html <- file.path(tmp_dir, "widget.html")
  
  if (is.null(widget$elementId) || !nzchar(widget$elementId)) {
    widget$elementId <- paste0("leaflet_", as.integer(stats::runif(1, 1, 1e9)))
  }
  
  htmlwidgets::saveWidget(
    widget = widget,
    file   = tmp_html,
    selfcontained = TRUE,
    title = "mapa_exportado"
  )
  
  if (file.exists(file)) unlink(file, force = TRUE)
  
  webshot2::webshot(
    url     = tmp_html,
    file    = file,
    vwidth  = vwidth,
    vheight = vheight,
    zoom    = zoom,
    delay   = delay
  )
  
  Sys.sleep(1.2)
  
  isTRUE(file.exists(file)) &&
    is.finite(file.info(file)$size) &&
    file.info(file)$size > 0
}

save_leaflet_png_retry <- function(widget, file, vwidth = 2200, vheight = 1500, zoom = 2, delay_base = 2) {
  delays <- c(delay_base, delay_base + 2, delay_base + 4)
  
  for (d in delays) {
    ok <- tryCatch(
      save_leaflet_png(
        widget  = widget,
        file    = file,
        vwidth  = vwidth,
        vheight = vheight,
        zoom    = zoom,
        delay   = d
      ),
      error = function(e) FALSE
    )
    
    Sys.sleep(1)
    
    if (isTRUE(ok) &&
        file.exists(file) &&
        !is.na(file.info(file)$size) &&
        file.info(file)$size > 0) {
      return(TRUE)
    }
  }
  
  FALSE
}

# ------------------------------
# 2) Datos: EVA + Shapefiles
# ------------------------------
eva_df <- readRDS("data/011_UPRA_EVA-A.rds")

eva_df <- eva_df %>%
  dplyr::filter(DEPARTAMENTO_D == "ATLÁNTICO") %>%
  dplyr::mutate(
    cultivo = dplyr::case_when(
      cultivo == "Caña" ~ "Caña de Azúcar",
      TRUE              ~ cultivo
    )
  ) %>%
  dplyr::filter(
    cultivo %in% c(
      "Cacao","Café","Caña de Azúcar","Fique","Iraca","Olivo",
      "Otras oleaginosas","Otros cultivos tropicales tradicionales",
      "Palma de aceite","Sacha inchi"
    )
  )

ruta_shp_mpios <- "data/shp/MGN_ANM_MPIOS.shp"
ruta_shp_dptos <- "data/shp/MGN_ANM_DPTOS.shp"

mpios_sf_raw <- sf::st_read(ruta_shp_mpios, quiet = TRUE)
depto_sf_raw <- sf::st_read(ruta_shp_dptos, quiet = TRUE)

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

mpios_sf <- sf::st_transform(mpios_sf, 4326) |>
  sf::st_make_valid() |>
  sf::st_zm(drop = TRUE, what = "ZM")

depto_sf <- sf::st_transform(depto_sf_raw, 4326) |>
  sf::st_make_valid() |>
  sf::st_zm(drop = TRUE, what = "ZM") |>
  dplyr::mutate(DEPARTAMENTO_D = .data[[depto_name_col]])

norm_txt <- function(x) stringi::stri_trim_both(as.character(x))

mpios_sf <- mpios_sf |>
  dplyr::mutate(
    MUNICIPIO_D    = norm_txt(MUNICIPIO_D),
    DEPARTAMENTO_D = toupper(norm_txt(DEPARTAMENTO_D))
  )

depto_sf <- depto_sf |>
  dplyr::mutate(DEPARTAMENTO_D = toupper(norm_txt(DEPARTAMENTO_D)))

eva_df <- eva_df |>
  dplyr::mutate(
    MUNICIPIO_D    = norm_txt(MUNICIPIO_D),
    DEPARTAMENTO_D = toupper(norm_txt(DEPARTAMENTO_D))
  )

title_case_es <- function(x) {
  if (is.null(x)) return(x)
  
  lower_words <- c(
    "a","ante","bajo","cabe","con","contra","de","del","desde",
    "en","entre","hacia","hasta","para","por","según","sin","so",
    "sobre","tras","el","la","los","las","un","una","unos","unas",
    "y","e","o","u","ni","al"
  )
  
  vapply(as.character(x), function(s) {
    if (is.na(s)) return(NA_character_)
    s <- trimws(s)
    if (s == "") return(s)
    s_low  <- tolower(s)
    parts  <- unlist(strsplit(s_low, "\\s+"))
    parts_tc <- stringi::stri_trans_totitle(parts, locale = "es")
    
    if (length(parts_tc) > 1) {
      for (i in 2:length(parts_tc)) {
        w_low <- tolower(parts_tc[i])
        if (w_low %in% lower_words) parts_tc[i] <- w_low
      }
    }
    paste(parts_tc, collapse = " ")
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

dptos_vec    <- sort(unique(eva_df$DEPARTAMENTO_D))
mpios_vec    <- sort(unique(eva_df$MUNICIPIO_D))
cultivos_vec <- sort(unique(eva_df$cultivo))

# ------------------------------
# 3) UI
# ------------------------------
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"
    ),
    tags$style(HTML(sprintf("
      body{
        background:#ffffff;
        font-family: 'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }
      h2#app-title {
        text-align:center;
        margin-top:10px;
        margin-bottom:10px;
        font-weight:800;
      }
      .eva-wrap{
        max-width:1310px;
        margin:0 auto;
        padding:16px 20px 28px;
      }
      .left-pane  { height: 838px; }
      .right-pane { height: 410px; margin-bottom: 20px; }
      .card {
        background:#ffffff;
        border:1px solid #e6e6e6;
        border-radius:12px;
        padding:12px;
        box-shadow:0 1px 6px rgba(0,0,0,0.05);
      }
      .card-title {
        font-size: 16px;
        font-weight: 700;
        color: #111827;
        margin-bottom: 8px;
      }
      .filter-label,
      .filters-card .control-label,
      .card .control-label {
        font-size:14px;
        font-weight:500;
        margin-bottom:4px;
      }
      .top-filters .col-sm-2, .top-filters .col-sm-7 { margin-bottom:10px; }
      .filters-card { margin-bottom:18px; }
      .dl-footer { margin-top:10px; text-align:right; }
      .dl-under  { margin-top:8px; text-align:right; }
      .btn, .btn-default {
        font-size:12px;
        padding:6px 10px;
        border-radius:8px;
      }
      .btn + .btn { margin-left:6px; }
      .viz-card {
        border:1.5px solid %s !important;
        border-radius:12px;
        box-shadow:0 1px 6px rgba(0,0,0,0.05);
      }
      .form-control,
      .form-select,
      .selectize-control .selectize-input,
      .bootstrap-select .dropdown-toggle {
        border-color:%s !important;
        border-width:1.5px;
        border-radius:8px;
      }
      .form-control:focus,
      .form-select:focus,
      .selectize-control .selectize-input.focus,
      .bootstrap-select .dropdown-toggle:focus,
      .bootstrap-select .dropdown-toggle:active {
        border-color:%s !important;
        box-shadow:0 0 0 0.2rem rgba(161,217,155,0.15);
      }
      .nav-tabs .nav-link { font-weight:700; }
      .nav-tabs .nav-link.active { font-weight:800; }
      .viz-card { padding: 12px !important; }
    ", BORDER_COL, BORDER_COL, BORDER_COL)))
  ),
  
  div(
    class = "eva-wrap",
    h2(textOutput("app_title"), id = "app-title"),
    
    bslib::navset_tab(
      id = "tabs",
      
      bslib::nav_panel(
        "Explorador territorial de indicadores productivos",
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "card viz-card filters-card",
              fluidRow(
                class = "top-filters",
                column(
                  width = 2,
                  div(class = "filter-label", "¿Qué año analizamos?"),
                  uiOutput("anio_ui")
                ),
                column(
                  width = 3,
                  div(class = "filter-label", "¿En qué departamento?"),
                  selectInput(
                    "f_depto", NULL,
                    choices = c("Todos" = "Todos", stats::setNames(dptos_vec, title_case_es(dptos_vec))),
                    selected = if (length(dptos_vec)) dptos_vec[1] else "Todos"
                  )
                ),
                column(
                  width = 3,
                  div(class = "filter-label", "¿Algún municipio en particular?"),
                  selectInput(
                    "f_mpio", NULL,
                    choices = c("Todos" = "Todos", stats::setNames(mpios_vec, title_case_es(mpios_vec))),
                    selected = "Todos"
                  )
                ),
                column(
                  width = 2,
                  div(class = "filter-label", "¿Cuál cultivo?"),
                  selectInput(
                    "f_cultivo", NULL,
                    choices = c("Todos" = "Todos", stats::setNames(cultivos_vec, title_case_es(cultivos_vec))),
                    selected = "Todos"
                  )
                ),
                column(
                  width = 2,
                  div(class = "filter-label", "Variable a considerar"),
                  selectInput(
                    "f_indicador", NULL,
                    choices = c(
                      "Área sembrada (Ha)"   = "area_sembrada_ha",
                      "Área cosechada (Ha)"  = "area_cosechada_ha",
                      "Producción (Ton)"     = "produccion_t",
                      "Rendimiento (Ton/Ha)" = "rendimiento_t_ha"
                    ),
                    selected = "area_sembrada_ha"
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            div(
              class = "card viz-card left-pane",
              h5(class = "card-title", textOutput("titulo_mapa")),
              div(
                style = "display:flex; gap:10px; align-items:center; margin-bottom:8px;",
                actionButton("btn_volver", "◀ Volver a Departamentos", class = "btn btn-light"),
                strong(textOutput("nivel_txt", inline = TRUE))
              ),
              leafletOutput("map_eva", height = 780),
              div(class = "dl-under", downloadButton("dl_png_mapa", label = "PNG — Mapa (simple)"))
            )
          ),
          column(
            width = 6,
            div(
              class = "card viz-card right-pane",
              h5(class = "card-title", textOutput("titulo_serie")),
              plotlyOutput("plot_arriba", height = "290px"),
              div(class = "dl-under", downloadButton("dl_png_series", label = "PNG — Serie temporal"))
            ),
            div(
              class = "card viz-card right-pane",
              h5(class = "card-title", textOutput("titulo_ranking")),
              plotlyOutput("ranking_abajo", height = "290px"),
              div(class = "dl-under", downloadButton("dl_png_ranking", label = "PNG — Ranking Top-10"))
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "card viz-card",
              style = "margin-top: 4px;",
              h5(class = "card-title", "Tabla de detalle (según filtros seleccionados)"),
              DT::dataTableOutput("tabla_detalle")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "dl-footer",
              downloadButton("dl_csv_expl", label = "Descargar CSV"),
              downloadButton("dl_reporte_pdf", label = "Descargar informe (PDF)"),
              tags$a(
                href   = github_url,
                target = "_blank",
                class  = "btn btn-dark",
                style  = "color:white;",
                list(bsicons::bs_icon("github"), " GitHub")
              )
            )
          )
        )
      ),
      
      bslib::nav_panel(
        "Análisis de aglomeración y productos representativos",
        
        fluidRow(
          column(
            width = 6,
            div(
              class = "card viz-card",
              h5(class = "card-title", textOutput("clus_titulo_mapa")),
              
              fluidRow(
                column(width = 6, uiOutput("clus_anio_ui")),
                column(
                  width = 6,
                  selectInput(
                    "clus_depto", "¿En qué departamento?",
                    choices  = stats::setNames(sort(unique(eva_df$DEPARTAMENTO_D)), title_case_es(sort(unique(eva_df$DEPARTAMENTO_D)))),
                    selected = sort(unique(eva_df$DEPARTAMENTO_D))[1]
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  selectInput(
                    "clus_cultivo", "¿Cuál cultivo?",
                    choices  = c("Todos" = "Todos", stats::setNames(sort(unique(eva_df$cultivo)), title_case_es(sort(unique(eva_df$cultivo))))),
                    selected = "Todos"
                  )
                ),
                column(
                  width = 6,
                  selectInput(
                    "clus_indicador", "Variable a considerar",
                    choices = c(
                      "Área sembrada (Ha)"   = "area_sembrada_ha",
                      "Área cosechada (Ha)"  = "area_cosechada_ha",
                      "Producción (Ton)"     = "produccion_t",
                      "Rendimiento (Ton/Ha)" = "rendimiento_t_ha"
                    ),
                    selected = "area_sembrada_ha"
                  )
                )
              ),
              
              leafletOutput("map_clusters", height = 620),
              div(class = "dl-under", downloadButton("dl_png_clusters", label = "PNG — Mapa Clusters (simple)"))
            )
          ),
          column(
            width = 6,
            div(
              class = "card viz-card",
              h5(class = "card-title", textOutput("clus_titulo_tabla")),
              DT::dataTableOutput("clus_resumen")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "dl-footer",
              downloadButton("dl_csv_clus", label = "Descargar CSV"),
              tags$a(
                href   = github_url,
                target = "_blank",
                class  = "btn btn-dark",
                style  = "color:white;",
                list(bsicons::bs_icon("github"), " GitHub")
              )
            )
          )
        )
      ),
      
      bslib::nav_panel(
        "Diversificación de cultivos",
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "card viz-card filters-card",
              fluidRow(
                column(
                  3,
                  div(class = "filter-label", "¿Qué año analizamos?"),
                  selectInput("hhi_anio", NULL, choices = c("2020", "2021", "2022", "2023", "2024"), selected = "2024")
                ),
                column(
                  3,
                  div(class = "filter-label", "¿En qué departamento?"),
                  selectInput(
                    "hhi_depto", NULL,
                    choices = c("Todos" = "Todos", stats::setNames(dptos_vec, title_case_es(dptos_vec))),
                    selected = if (length(dptos_vec)) dptos_vec[1] else "Todos"
                  )
                ),
                column(
                  3,
                  div(class = "filter-label", "¿Algún municipio en particular?"),
                  selectInput("hhi_mpio", NULL, choices = c("Todos" = "Todos"), selected = "Todos")
                ),
                column(
                  3,
                  selectInput(
                    "hhi_base", "Variable a considerar",
                    choices = c(
                      "Producción (Ton)"    = "produccion_t",
                      "Área cosechada (Ha)" = "area_cosechada_ha",
                      "Área sembrada (Ha)"  = "area_sembrada_ha"
                    ),
                    selected = "produccion_t"
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          style = "margin-top: 5px;",
          column(
            width = 6,
            style = "padding-right: 10px;",
            div(
              class = "card viz-card",
              style = "margin-bottom: 15px; height: 800px;",
              h5(class = "card-title", textOutput("hhi_titulo_barras")),
              plotlyOutput("hhi_barras", height = "680px"),
              div(class = "dl-under", downloadButton("dl_png_hhi_barras", label = "PNG — Barras"))
            )
          ),
          column(
            width = 6,
            style = "padding-left: 10px;",
            div(
              class = "card viz-card",
              style = "margin-bottom: 15px; height: 500px;",
              h5(class = "card-title", textOutput("hhi_titulo_serie")),
              plotlyOutput("hhi_serie", height = "380px"),
              div(class = "dl-under", downloadButton("dl_png_hhi_serie", label = "PNG — Serie"))
            ),
            div(
              class = "card viz-card",
              style = "height: 285px;",
              h5(class = "card-title", "Índice de Diversificación de Cultivos en los territorios seleccionados"),
              div(
                style = "margin-top: 20px; margin-bottom: 10px; display:flex; align-items:baseline; gap:10px;",
                span(textOutput("hhi_kpi_valor"), style = "font-size: 24px; font-weight: 800; color:#08519c;"),
                span("Diversificación productiva (1 - HHI, 0–1)", style = "font-size: 14px; color:#666;")
              ),
              p("Este indicador se calcula como 1 menos el índice Herfindahl-Hirschman (HHI) de la distribución de cultivos en el ámbito seleccionado (país, departamento o municipio), para el año y la base de referencia elegidos (producción, área cosechada o área sembrada).", style = "font-size: 13px; color:#555; margin-top: 5px;"),
              p("En términos prácticos, valores cercanos a 1 indican una alta diversificación productiva: la base seleccionada está repartida entre varios cultivos con pesos relativamente similares. Valores cercanos a 0 indican una alta concentración, es decir, uno o pocos cultivos dominan la estructura productiva del territorio.", style = "font-size: 13px; color:#555;")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "dl-footer",
              style = "margin-top: 15px;",
              downloadButton("dl_csv_hhi", label = "Descargar CSV"),
              tags$a(
                href   = github_url,
                target = "_blank",
                class  = "btn btn-dark",
                style  = "color:white;",
                list(bsicons::bs_icon("github"), " GitHub")
              )
            )
          )
        )
      )
    )
  )
)

# ------------------------------
# 4) Helpers globales
# ------------------------------
bbox_to_fitbounds <- function(map, sfobj) {
  bb <- sf::st_bbox(sfobj)
  leaflet::fitBounds(
    map,
    lng1 = unname(bb["xmin"]),
    lat1 = unname(bb["ymin"]),
    lng2 = unname(bb["xmax"]),
    lat2 = unname(bb["ymax"])
  )
}

calc_lisa_and_class <- function(sf_obj, value_col, p_thr = 0.05) {
  sf_obj <- sf::st_make_valid(sf_obj)
  sf_obj <- sf::st_cast(sf_obj, "MULTIPOLYGON", warn = FALSE)
  v <- as.numeric(sf_obj[[value_col]])
  v[is.na(v)] <- 0
  sf_obj$.__valor__ <- v
  
  if (nrow(sf_obj) < 3) {
    sf_obj$Ii <- NA_real_
    sf_obj$pvalue <- NA_real_
    sf_obj$cluster <- "No significativo"
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

mix_hex <- function(c1, c2, t) {
  r1 <- grDevices::col2rgb(c1)[,1] / 255
  r2 <- grDevices::col2rgb(c2)[,1] / 255
  m  <- (1 - t) * r1 + t * r2
  grDevices::rgb(m[1], m[2], m[3])
}

shades4_from_base <- function(base_col) {
  ts <- c(0.10, 0.40, 0.70, 1.00)
  vapply(ts, function(tt) mix_hex("#FFFFFF", base_col, tt), character(1))
}

make_bins4 <- function(values) {
  v <- as.numeric(values)
  v <- v[is.finite(v)]
  if (length(v) == 0) return(seq(0, 4))
  qs <- quantile(v, probs = seq(0, 1, length.out = 5), na.rm = TRUE, type = 7)
  qs <- as.numeric(unique(qs))
  if (length(qs) < 5) {
    r <- range(v, na.rm = TRUE)
    if (r[1] == r[2]) r <- c(0, max(1, r[2]))
    qs <- pretty(r, n = 4)
  }
  qs <- sort(unique(qs))
  if (length(qs) < 5) qs <- seq(min(qs), max(qs), length.out = 5)
  qs
}

fmt_bin <- function(x) comma_es(x, accuracy = 1)

build_bins_labels4_indicator <- function(values, base_col){
  v <- as.numeric(values)
  v <- v[is.finite(v)]
  if (!length(v)) v <- c(0, 1)
  
  bins <- make_bins4(v)
  pals <- shades4_from_base(base_col)
  
  pal <- leaflet::colorBin(
    palette  = pals,
    bins     = bins,
    domain   = v,
    na.color = "#f0f0f0",
    right    = FALSE
  )
  
  labs <- vapply(
    seq_len(length(bins) - 1),
    function(i){
      a  <- bins[i]
      b  <- bins[i + 1]
      sa <- fmt_bin(a)
      sb <- fmt_bin(b)
      if (i == 1) sprintf("%s – %s", sa, sb) else sprintf("> %s – %s", sa, sb)
    },
    character(1)
  )
  
  mids  <- (bins[-length(bins)] + bins[-1]) / 2
  cols  <- pal(mids)
  
  list(bins = bins, pal = pal, labels = labs, colors = cols)
}

# ------------------------------
# 5) SERVER
# ------------------------------
server <- function(input, output, session) {
  
  output$app_title <- renderText({ "" })
  
  is_yield      <- reactive({ identical(input$f_indicador,  "rendimiento_t_ha") })
  clus_is_yield <- reactive({ identical(input$clus_indicador,"rendimiento_t_ha") })
  
  build_titulo_tab1 <- function(tipo = c("mapa", "serie", "ranking"),
                                nivel = c("depto", "mpio"),
                                ind,
                                cultivo = NULL,
                                lbl_ind = NULL) {
    tipo  <- match.arg(tipo)
    nivel <- match.arg(nivel)
    
    cult_txt <- if (is.null(cultivo) || cultivo == "Todos" || is.na(cultivo) || cultivo == "") "" else paste0(" de ", title_case_es(cultivo))
    lbl_ind  <- if (is.null(lbl_ind)) ind else lbl_ind
    
    if (ind == "area_sembrada_ha") {
      if (tipo == "mapa")    return(if (nivel == "depto") paste0("¿En qué departamentos hay una mayor cantidad de hectáreas sembradas", cult_txt, "?") else paste0("¿En qué municipios hay una mayor cantidad de hectáreas sembradas", cult_txt, "?"))
      if (tipo == "serie")   return(paste0("¿Cómo ha evolucionado en el tiempo la cantidad de hectáreas sembradas", cult_txt, "?"))
      if (tipo == "ranking") return(paste0("¿Qué municipios tienen una mayor cantidad de hectáreas sembradas", cult_txt, "?"))
    }
    
    if (ind == "area_cosechada_ha") {
      if (tipo == "mapa")    return(if (nivel == "depto") paste0("¿En qué departamentos hay una mayor cantidad de hectáreas cosechadas", cult_txt, "?") else paste0("¿En qué municipios hay una mayor cantidad de hectáreas cosechadas", cult_txt, "?"))
      if (tipo == "serie")   return(paste0("¿Cómo ha evolucionado en el tiempo la cantidad de hectáreas cosechadas", cult_txt, "?"))
      if (tipo == "ranking") return(paste0("¿Qué municipios tienen una mayor cantidad de hectáreas cosechadas", cult_txt, "?"))
    }
    
    if (ind == "produccion_t") {
      if (tipo == "mapa")    return(if (nivel == "depto") paste0("¿En qué departamentos se concentra el mayor volumen de producción agrícola (toneladas)", cult_txt, "?") else paste0("¿En qué municipios se concentra el mayor volumen de producción agrícola (toneladas)", cult_txt, "?"))
      if (tipo == "serie")   return(paste0("¿Cómo ha evolucionado en el tiempo el volumen de producción agrícola (toneladas)", cult_txt, "?"))
      if (tipo == "ranking") return(paste0("¿Qué municipios concentran el mayor volumen de producción agrícola (toneladas)", cult_txt, "?"))
    }
    
    if (ind == "rendimiento_t_ha") {
      if (tipo == "mapa")    return(if (nivel == "depto") paste0("¿En qué departamentos se observan los mayores niveles de rendimiento (ton/ha)", cult_txt, "?") else paste0("¿En qué municipios se observan los mayores niveles de rendimiento (ton/ha)", cult_txt, "?"))
      if (tipo == "serie")   return(paste0("¿Cómo ha evolucionado en el tiempo el rendimiento promedio (ton/ha)", cult_txt, "?"))
      if (tipo == "ranking") return(paste0("¿Qué municipios presentan los mayores niveles de rendimiento (ton/ha)", cult_txt, "?"))
    }
    
    if (tipo == "mapa") {
      if (nivel == "depto") return(paste0("¿En qué departamentos se concentra el indicador ", lbl_ind, "?"))
      return(paste0("¿En qué municipios se concentra el indicador ", lbl_ind, "?"))
    }
    if (tipo == "serie")   return(paste0("¿Cómo ha evolucionado en el tiempo el indicador ", lbl_ind, "?"))
    if (tipo == "ranking") return(paste0("¿Qué municipios lideran el indicador ", lbl_ind, "?"))
  }
  
  indic_color <- reactive({
    switch(
      input$f_indicador,
      "area_sembrada_ha"  = "#007A3D",
      "area_cosechada_ha" = "#FBC02D",
      "produccion_t"      = "#F57C00",
      "rendimiento_t_ha"  = "#0099cc",
      "#7f7f7f"
    )
  })
  
  observeEvent(input$f_depto, ignoreInit = TRUE, {
    if (is.null(input$f_depto) || input$f_depto == "Todos") {
      munis <- sort(unique(eva_df$MUNICIPIO_D))
    } else {
      munis <- sort(unique(eva_df$MUNICIPIO_D[eva_df$DEPARTAMENTO_D == input$f_depto]))
    }
    updateSelectInput(
      session, "f_mpio",
      choices  = c("Todos" = "Todos", stats::setNames(munis, title_case_es(munis))),
      selected = "Todos"
    )
  })
  
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
  
  datos_filtrados <- reactive({
    df <- eva_df
    if (!is.null(input$f_depto)   && input$f_depto   != "Todos") df <- df |> dplyr::filter(DEPARTAMENTO_D == input$f_depto)
    if (!is.null(input$f_mpio)    && input$f_mpio    != "Todos") df <- df |> dplyr::filter(MUNICIPIO_D == input$f_mpio)
    if (!is.null(input$f_cultivo) && input$f_cultivo != "Todos") df <- df |> dplyr::filter(cultivo == input$f_cultivo)
    if (!is.null(input$f_anio)) df <- df |> dplyr::filter(.data[[year_col()]] == input$f_anio)
    
    df <- df |>
      dplyr::mutate(
        prod_num  = suppressWarnings(as.numeric(produccion_t)),
        area_cnum = suppressWarnings(as.numeric(area_cosechada_ha)),
        area_snum = suppressWarnings(as.numeric(area_sembrada_ha))
      )
    
    ind <- input$f_indicador
    req(ind)
    
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
  
  nivel_mapa <- reactiveVal("depto")
  depto_sel  <- reactiveVal(NULL)
  
  output$nivel_txt <- renderText({
    if (nivel_mapa() == "depto") "Nivel: Departamentos"
    else paste0("Nivel: Municipios — ", title_case_es(depto_sel()))
  })
  
  output$titulo_mapa <- renderText({
    ind  <- input$f_indicador %||% "area_sembrada_ha"
    lbl  <- as.character(indicador_label())
    cult <- input$f_cultivo %||% "Todos"
    build_titulo_tab1(tipo = "mapa", nivel = nivel_mapa(), ind = ind, cultivo = cult, lbl_ind = lbl)
  })
  
  output$titulo_serie <- renderText({
    ind  <- input$f_indicador %||% "area_sembrada_ha"
    lbl  <- as.character(indicador_label())
    cult <- input$f_cultivo %||% "Todos"
    build_titulo_tab1(tipo = "serie", nivel = "depto", ind = ind, cultivo = cult, lbl_ind = lbl)
  })
  
  output$titulo_ranking <- renderText({
    ind  <- input$f_indicador %||% "area_sembrada_ha"
    lbl  <- as.character(indicador_label())
    cult <- input$f_cultivo %||% "Todos"
    build_titulo_tab1(tipo = "ranking", nivel = "mpio", ind = ind, cultivo = cult, lbl_ind = lbl)
  })
  
  agg_depto <- reactive({
    df <- datos_filtrados()
    req(nrow(df) > 0)
    
    if (is_yield()) {
      df |>
        dplyr::group_by(DEPARTAMENTO_D) |>
        dplyr::summarise(
          prod  = sum(prod_num,  na.rm = TRUE),
          area  = sum(area_cnum, na.rm = TRUE),
          valor = dplyr::if_else(area > 0, prod/area, NA_real_),
          .groups = "drop"
        ) |>
        dplyr::select(DEPARTAMENTO_D, valor)
    } else {
      df |>
        dplyr::group_by(DEPARTAMENTO_D) |>
        dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
    }
  })
  
  agg_mpio <- reactive({
    df <- datos_filtrados()
    req(nrow(df) > 0)
    if (!is.null(depto_sel())) df <- df |> dplyr::filter(DEPARTAMENTO_D == depto_sel())
    
    if (is_yield()) {
      df |>
        dplyr::group_by(MUNICIPIO_D) |>
        dplyr::summarise(
          prod  = sum(prod_num,  na.rm = TRUE),
          area  = sum(area_cnum, na.rm = TRUE),
          valor = dplyr::if_else(area > 0, prod/area, NA_real_),
          .groups = "drop"
        ) |>
        dplyr::select(MUNICIPIO_D, valor)
    } else {
      df |>
        dplyr::group_by(MUNICIPIO_D) |>
        dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
    }
  })
  
  output$map_eva <- leaflet::renderLeaflet({
    mdat <- depto_sf |>
      dplyr::left_join(agg_depto(), by = "DEPARTAMENTO_D") |>
      dplyr::mutate(
        valor            = ifelse(is.na(valor), 0, valor),
        DEPARTAMENTO_LBL = title_case_es(DEPARTAMENTO_D)
      )
    
    pal_info <- build_bins_labels4_indicator(mdat$valor, indic_color())
    pal      <- pal_info$pal
    
    leaflet::leaflet(mdat) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addPolygons(
        layerId = ~DEPARTAMENTO_D,
        fillColor = ~pal(valor),
        weight = 0.7, color = BORDER_COL, fillOpacity = 0.9,
        label = ~DEPARTAMENTO_LBL,
        labelOptions = leaflet::labelOptions(direction = "auto", textsize = "12px", sticky = TRUE, opacity = 0.9, style = list("font-weight" = "600")),
        highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) |>
      leaflet::addLegend(position = "bottomright", colors = pal_info$colors, labels = pal_info$labels, title = indicador_label(), opacity = 1)
  })
  
  dibujar_deptos <- function() {
    mdat <- depto_sf |>
      dplyr::left_join(agg_depto(), by = "DEPARTAMENTO_D") |>
      dplyr::mutate(
        valor            = ifelse(is.na(valor), 0, valor),
        DEPARTAMENTO_LBL = title_case_es(DEPARTAMENTO_D)
      )
    
    pal_info <- build_bins_labels4_indicator(mdat$valor, indic_color())
    pal      <- pal_info$pal
    
    leaflet::leafletProxy("map_eva", data = mdat) |>
      leaflet::clearPopups() |>
      leaflet::clearShapes() |>
      leaflet::clearControls() |>
      leaflet::addPolygons(
        layerId = ~DEPARTAMENTO_D,
        fillColor = ~pal(valor),
        weight = 0.7, color = BORDER_COL, fillOpacity = 0.9,
        label = ~DEPARTAMENTO_LBL,
        labelOptions = leaflet::labelOptions(direction = "auto", textsize = "12px", sticky = TRUE, opacity = 0.9, style = list("font-weight" = "600")),
        highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) |>
      leaflet::addLegend(position = "bottomright", colors = pal_info$colors, labels = pal_info$labels, title = indicador_label(), opacity = 1)
  }
  
  dibujar_mpios <- function(dep) {
    sel_mpio <- input$f_mpio
    
    mdat <- mpios_sf |>
      dplyr::filter(DEPARTAMENTO_D == dep) |>
      dplyr::left_join(agg_mpio(), by = "MUNICIPIO_D") |>
      dplyr::mutate(
        valor         = ifelse(is.na(valor), 0, valor),
        MUNICIPIO_LBL = title_case_es(MUNICIPIO_D)
      )
    
    pal_info <- build_bins_labels4_indicator(mdat$valor, indic_color())
    pal      <- pal_info$pal
    
    mdat <- mdat |>
      dplyr::mutate(
        es_sel   = !is.null(sel_mpio) & sel_mpio != "Todos" & MUNICIPIO_D == sel_mpio,
        fill_col = dplyr::if_else(es_sel, "#252525", pal(valor))
      )
    
    leaflet::leafletProxy("map_eva", data = mdat) |>
      leaflet::clearPopups() |>
      leaflet::clearShapes() |>
      leaflet::clearControls() |>
      leaflet::addPolygons(
        layerId = ~MUNICIPIO_D,
        fillColor = ~fill_col,
        weight = 0.4, color = BORDER_COL, fillOpacity = 0.9,
        label = ~MUNICIPIO_LBL,
        labelOptions = leaflet::labelOptions(direction = "auto", textsize = "11px", sticky = TRUE, opacity = 0.9, style = list("font-weight" = "600")),
        highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        colors   = pal_info$colors,
        labels   = pal_info$labels,
        title    = paste0(indicador_label(), " — ", title_case_es(dep)),
        opacity  = 1
      )
  }
  
  observe({
    dep <- input$f_depto
    input$f_mpio
    input$f_indicador
    input$f_anio
    input$f_cultivo
    
    if (is.null(dep) || dep == "Todos") {
      nivel_mapa("depto")
      depto_sel(NULL)
      dibujar_deptos()
    } else {
      nivel_mapa("mpio")
      depto_sel(dep)
      dibujar_mpios(dep)
    }
  })
  
  observeEvent(input$btn_volver, {
    updateSelectInput(session, "f_depto", selected = "Todos")
    updateSelectInput(session, "f_mpio",  selected = "Todos")
    nivel_mapa("depto")
    depto_sel(NULL)
    dibujar_deptos()
  })
  
  series_data <- reactive({
    base <- eva_df
    if (!is.null(input$f_depto)   && input$f_depto   != "Todos") base <- base |> dplyr::filter(DEPARTAMENTO_D == input$f_depto)
    if (!is.null(input$f_mpio)    && input$f_mpio    != "Todos") base <- base |> dplyr::filter(MUNICIPIO_D == input$f_mpio)
    if (!is.null(input$f_cultivo) && input$f_cultivo != "Todos") base <- base |> dplyr::filter(cultivo == input$f_cultivo)
    
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
          prod        = sum(prod_num,  na.rm = TRUE),
          area        = sum(area_cnum, na.rm = TRUE),
          valor_total = dplyr::if_else(area > 0, prod/area, NA_real_),
          .groups     = "drop"
        ) |>
        dplyr::rename(anio = !!ycol)
    } else {
      ind <- input$f_indicador
      base <- base |>
        dplyr::mutate(
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
  
  output$plot_arriba <- plotly::renderPlotly({
    df_year <- series_data()
    col     <- indic_color()
    
    if (nrow(df_year) == 0) {
      return(plotly::plot_ly() |>
               plotly::layout(annotations = list(text = "Sin datos para la selección actual", x = 0.5, y = 0.5, showarrow = FALSE)) |>
               plotly::config(locale = "es"))
    }
    
    plotly::plot_ly(
      data = df_year, x = ~anio, y = ~valor_total,
      type = "scatter", mode = "lines+markers",
      line   = list(color = col, width = 2),
      marker = list(color = col, size = 6),
      hovertemplate = paste0("<b>Año: %{x}</b><br>", as.character(indicador_label()), ": %{y:.2f}<extra></extra>")
    ) |>
      plotly::layout(
        xaxis = list(title = if (year_col() == "ano_sembrado") "Año de sembrado" else "Año de cosechado", tickmode = "linear", dtick = 1),
        yaxis = list(title = as.character(indicador_label())),
        hovermode = "x unified",
        margin = list(l = 60, r = 20, t = 40, b = 50)
      ) |>
      plotly::config(locale = "es")
  })
  
  ranking_data <- reactive({
    df <- datos_filtrados()
    if (nrow(df) == 0) return(data.frame())
    
    if (is_yield()) {
      df |>
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) |>
        dplyr::summarise(
          prod = sum(prod_num,  na.rm = TRUE),
          area = sum(area_cnum, na.rm = TRUE),
          valor_total = dplyr::if_else(area > 0, prod/area, NA_real_),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(valor_total)) |>
        dplyr::slice_head(n = 10)
    } else {
      df |>
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) |>
        dplyr::summarise(valor_total = sum(valor, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(valor_total)) |>
        dplyr::slice_head(n = 10)
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
  
  output$ranking_abajo <- plotly::renderPlotly({
    plot_df <- ranking_data()
    sc  <- ranking_scale()
    col <- indic_color()
    
    if (nrow(plot_df) == 0) {
      return(plotly::plot_ly() |>
               plotly::layout(annotations = list(text = "Sin datos para el ranking", x = 0.5, y = 0.5, showarrow = FALSE)) |>
               plotly::config(locale = "es"))
    }
    
    plot_df <- plot_df |>
      dplyr::mutate(
        MUNICIPIO_LBL    = title_case_es(MUNICIPIO_D),
        DEPARTAMENTO_LBL = title_case_es(DEPARTAMENTO_D),
        valor_scaled     = valor_total / sc$factor,
        label_txt        = comma_es(round(valor_total, 0))
      ) |>
      dplyr::arrange(dplyr::desc(valor_scaled))
    
    plot_df$MUNICIPIO_LBL <- factor(plot_df$MUNICIPIO_LBL, levels = rev(plot_df$MUNICIPIO_LBL))
    
    plotly::plot_ly(
      data = plot_df,
      x    = ~valor_scaled,
      y    = ~MUNICIPIO_LBL,
      type = "bar",
      orientation = "h",
      marker = list(color = col),
      text  = ~label_txt,
      textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(color = "white"),
      hovertemplate = paste0(
        "<b>Municipio:</b> %{customdata[0]}",
        "<br><b>Departamento:</b> %{customdata[1]}",
        "<br><b>", as.character(indicador_label()), " (escala eje X):</b> %{x:.2f} ", sc$unit_short,
        "<br><b>", as.character(indicador_label()), " (sin escala):</b> %{customdata[2]}",
        "<extra></extra>"
      ),
      customdata = cbind(as.character(plot_df$MUNICIPIO_LBL), plot_df$DEPARTAMENTO_LBL, plot_df$label_txt),
      showlegend = FALSE
    ) |>
      plotly::layout(
        xaxis = list(title = sc$axis_label, gridcolor = "#e6e6e6"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 110, r = 40, t = 20, b = 40)
      ) |>
      plotly::config(locale = "es")
  })
  
  tabla_detalle_raw <- reactive({
    df <- eva_df
    if (!is.null(input$f_depto)   && input$f_depto   != "Todos") df <- df |> dplyr::filter(DEPARTAMENTO_D == input$f_depto)
    if (!is.null(input$f_mpio)    && input$f_mpio    != "Todos") df <- df |> dplyr::filter(MUNICIPIO_D == input$f_mpio)
    if (!is.null(input$f_cultivo) && input$f_cultivo != "Todos") df <- df |> dplyr::filter(cultivo == input$f_cultivo)
    if (!is.null(input$f_anio)) df <- df |> dplyr::filter(.data[[year_col()]] == input$f_anio)
    if (nrow(df) == 0) return(data.frame())
    
    df <- df |>
      dplyr::mutate(
        area_s = suppressWarnings(as.numeric(area_sembrada_ha)),
        area_c = suppressWarnings(as.numeric(area_cosechada_ha)),
        prod   = suppressWarnings(as.numeric(produccion_t))
      )
    
    df |>
      dplyr::group_by(ano_cosechado, DEPARTAMENTO_D, MUNICIPIO_D, cultivo) |>
      dplyr::summarise(
        `Hectáreas sembradas`  = sum(area_s, na.rm = TRUE),
        `Hectáreas cosechadas` = sum(area_c, na.rm = TRUE),
        `Producción`           = sum(prod,   na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::rename(
        `Año`        = ano_cosechado,
        Departamento = DEPARTAMENTO_D,
        Municipio    = MUNICIPIO_D,
        Cultivo      = cultivo
      ) |>
      dplyr::mutate(
        Departamento = title_case_es(Departamento),
        Municipio    = title_case_es(Municipio),
        Cultivo      = title_case_es(Cultivo)
      ) |>
      dplyr::arrange(Departamento, Municipio, Cultivo, `Año`)
  })
  
  output$tabla_detalle <- DT::renderDataTable({
    tb <- tabla_detalle_raw()
    
    if (nrow(tb) == 0) {
      return(DT::datatable(data.frame(Mensaje = "Sin datos para la selección actual."), rownames = FALSE, options = list(dom = "t")))
    }
    
    tb_show <- tb |>
      dplyr::mutate(
        `Hectáreas sembradas`  = comma_es(`Hectáreas sembradas`,  accuracy = 1),
        `Hectáreas cosechadas` = comma_es(`Hectáreas cosechadas`, accuracy = 1),
        `Producción`           = comma_es(`Producción`, accuracy = 1)
      )
    
    DT::datatable(tb_show, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE, dom = "tip"), escape = TRUE)
  })
  
  output$dl_csv_expl <- downloadHandler(
    filename = function() paste0("eva_tabla_detalle_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(tabla_detalle_raw(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
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
  
  output$clus_titulo_mapa  <- renderText({ glue::glue("¿Cómo se agrupan los municipios según sus niveles de {tolower(as.character(clus_indicador_label()))} y los de sus vecinos?") })
  output$clus_titulo_tabla <- renderText({ "¿Qué municipios y valores corresponden a cada tipo de clúster?" })
  
  datos_cluster <- reactive({
    req(input$clus_depto, input$clus_indicador, input$clus_anio)
    df <- eva_df %>% dplyr::filter(DEPARTAMENTO_D == input$clus_depto)
    if (!is.null(input$clus_cultivo) && input$clus_cultivo != "Todos") {
      df <- df %>% dplyr::filter(cultivo == input$clus_cultivo)
    }
    df <- df %>% dplyr::filter(.data[[clus_year_col()]] == input$clus_anio)
    
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
    df <- datos_cluster()
    req(nrow(df) > 0)
    
    if (clus_is_yield()) {
      agg_mun <- df %>%
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) %>%
        dplyr::summarise(
          prod  = sum(prod_num, na.rm = TRUE),
          area  = sum(area_cnum, na.rm = TRUE),
          valor = dplyr::if_else(area > 0, prod / area, NA_real_),
          .groups = "drop"
        ) %>%
        dplyr::select(MUNICIPIO_D, DEPARTAMENTO_D, valor)
    } else {
      agg_mun <- df %>%
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) %>%
        dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
    }
    
    mmun <- mpios_sf %>%
      dplyr::filter(DEPARTAMENTO_D == input$clus_depto) %>%
      dplyr::left_join(agg_mun, by = c("MUNICIPIO_D","DEPARTAMENTO_D")) %>%
      dplyr::mutate(
        valor          = ifelse(is.na(valor), 0, valor),
        MUNICIPIO_D    = as.character(MUNICIPIO_D),
        DEPARTAMENTO_D = as.character(DEPARTAMENTO_D)
      )
    
    mmun <- sf::st_make_valid(mmun)
    mmun <- sf::st_zm(mmun, drop = TRUE, what = "ZM")
    mmun <- calc_lisa_and_class(mmun, "valor", p_thr = 0.50)
    
    niveles <- c("Alto-Alto","Bajo-Bajo","Alto-Bajo","Bajo-Alto","No significativo")
    mmun$cluster <- factor(as.character(mmun$cluster), levels = niveles)
    
    pal <- leaflet::colorFactor(
      c("#1B7837", "#762A83", "#A6DBA0", "#C2A5CF", "#D9D9D9"),
      levels = niveles
    )
    
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::addPolygons(
        data = mmun,
        fillColor = ~pal(cluster),
        color = BORDER_COL,
        weight = 0.4,
        fillOpacity = 0.85,
        highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      )
  })
  
  output$clus_resumen <- DT::renderDataTable({
    df <- datos_cluster()
    req(nrow(df) > 0)
    
    if (clus_is_yield()) {
      agg_mun <- df %>%
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) %>%
        dplyr::summarise(
          prod  = sum(prod_num, na.rm = TRUE),
          area  = sum(area_cnum, na.rm = TRUE),
          valor = dplyr::if_else(area > 0, prod / area, NA_real_),
          .groups = "drop"
        ) %>%
        dplyr::select(MUNICIPIO_D, DEPARTAMENTO_D, valor)
    } else {
      agg_mun <- df %>%
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) %>%
        dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
    }
    
    mmun <- mpios_sf %>%
      dplyr::filter(DEPARTAMENTO_D == input$clus_depto) %>%
      dplyr::left_join(agg_mun, by = c("MUNICIPIO_D","DEPARTAMENTO_D")) %>%
      dplyr::mutate(
        valor          = ifelse(is.na(valor), 0, valor),
        MUNICIPIO_D    = as.character(MUNICIPIO_D),
        DEPARTAMENTO_D = as.character(DEPARTAMENTO_D)
      )
    
    req(nrow(mmun) >= 3)
    
    total_depto <- sum(mmun$valor, na.rm = TRUE)
    valor_num   <- as.numeric(mmun$valor)
    
    out <- data.frame(
      Municipio               = title_case_es(mmun$MUNICIPIO_D),
      Departamento            = title_case_es(mmun$DEPARTAMENTO_D),
      Valor                   = comma_es(round(valor_num, 2)),
      `Participación dpto`    = if (total_depto > 0) paste0(round(100 * valor_num / total_depto, 1), "%") else "0,0%",
      `Ranking departamental` = rank(-valor_num, ties.method = "min"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    out <- out[order(out$`Ranking departamental`), ]
    DT::datatable(out, rownames = FALSE, options = list(pageLength = 14, scrollX = TRUE, dom = "tip"), escape = TRUE)
  })
  
  output$dl_csv_clus <- downloadHandler(
    filename = function() paste0("eva_clusters_resumen_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(datos_cluster(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  map_widget_simple <- reactive({
    if (nivel_mapa() == "depto") {
      mdat <- depto_sf |>
        dplyr::left_join(agg_depto(), by = "DEPARTAMENTO_D") |>
        dplyr::mutate(valor = ifelse(is.na(valor), 0, valor), DEPARTAMENTO_LBL = title_case_es(DEPARTAMENTO_D))
      
      pal_info <- build_bins_labels4_indicator(mdat$valor, indic_color())
      pal <- pal_info$pal
      
      m <- leaflet::leaflet(mdat, options = leaflet::leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(fillColor = ~pal(valor), weight = 0.9, color = BORDER_COL, fillOpacity = 0.95, smoothFactor = 0.2, label = ~DEPARTAMENTO_LBL) |>
        leaflet::addLegend(position = "bottomright", colors = pal_info$colors, labels = pal_info$labels, title = as.character(indicador_label()), opacity = 1)
      
      bbox_to_fitbounds(m, mdat)
    } else {
      dep      <- depto_sel()
      sel_mpio <- input$f_mpio
      
      mdat <- mpios_sf |>
        dplyr::filter(DEPARTAMENTO_D == dep) |>
        dplyr::left_join(agg_mpio(), by = "MUNICIPIO_D") |>
        dplyr::mutate(valor = ifelse(is.na(valor), 0, valor), MUNICIPIO_LBL = title_case_es(MUNICIPIO_D))
      
      pal_info <- build_bins_labels4_indicator(mdat$valor, indic_color())
      pal <- pal_info$pal
      
      mdat <- mdat |>
        dplyr::mutate(
          es_sel   = !is.null(sel_mpio) & sel_mpio != "Todos" & MUNICIPIO_D == sel_mpio,
          fill_col = dplyr::if_else(es_sel, "#252525", pal(valor))
        )
      
      m <- leaflet::leaflet(mdat, options = leaflet::leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(fillColor = ~fill_col, weight = 0.7, color = BORDER_COL, fillOpacity = 0.95, smoothFactor = 0.2, label = ~MUNICIPIO_LBL) |>
        leaflet::addLegend(position = "bottomright", colors = pal_info$colors, labels = pal_info$labels, title = paste0(as.character(indicador_label()), " — ", title_case_es(dep)), opacity = 1)
      
      bbox_to_fitbounds(m, mdat)
    }
  })
  
  map_clusters_simple <- reactive({
    df <- datos_cluster()
    req(nrow(df) > 0)
    
    if (clus_is_yield()) {
      agg_mun <- df %>%
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) %>%
        dplyr::summarise(
          prod  = sum(prod_num, na.rm = TRUE),
          area  = sum(area_cnum, na.rm = TRUE),
          valor = dplyr::if_else(area > 0, prod / area, NA_real_),
          .groups = "drop"
        ) %>%
        dplyr::select(MUNICIPIO_D, DEPARTAMENTO_D, valor)
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
    pal <- leaflet::colorFactor(c("#1B7837", "#762A83", "#A6DBA0", "#C2A5CF", "#D9D9D9"), levels = niveles)
    
    m <- leaflet::leaflet(mmun, options = leaflet::leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::addPolygons(fillColor = ~pal(cluster), color = BORDER_COL, weight = 0.6, fillOpacity = 0.9, smoothFactor = 0.2) %>%
      leaflet::addLegend(position = "bottomright", pal = pal, values = ~cluster, title = "Clúster LISA", opacity = 1)
    
    bbox_to_fitbounds(m, mmun)
  })
  
  output$dl_png_mapa <- downloadHandler(
    filename = function() paste0("mapa_con_leyenda_", Sys.Date(), ".png"),
    content = function(file) {
      save_leaflet_png(widget = map_widget_simple(), file = file, vwidth = 2200, vheight = 1500, zoom = 2.2)
    }
  )
  
  g_series_gg <- reactive({
    df  <- series_data()
    col <- indic_color()
    if (nrow(df) == 0) return(plot_vacio_gg())
    
    ggplot(df, aes(x = anio, y = valor_total)) +
      geom_line(linewidth = 0.9, color = col) +
      geom_point(size = 2.2, color = col) +
      scale_x_continuous(breaks = unique(df$anio)) +
      labs(
        x = if (year_col() == "ano_sembrado") "Año de sembrado" else "Año de cosechado",
        y = indicador_label(),
        title = paste0("Evolución de ", indicador_label())
      ) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
  })
  
  g_ranking_gg <- reactive({
    plot_df <- ranking_data()
    sc      <- ranking_scale()
    if (nrow(plot_df) == 0) return(plot_vacio_gg("Sin datos para el ranking"))
    
    plot_df <- plot_df |>
      dplyr::mutate(
        valor_scaled  = valor_total / sc$factor,
        MUNICIPIO_LBL = title_case_es(MUNICIPIO_D)
      )
    
    ggplot(plot_df, aes(x = valor_scaled, y = reorder(MUNICIPIO_LBL, valor_scaled))) +
      geom_col(fill = indic_color()) +
      scale_x_continuous(labels = function(x) comma_es(x), expand = expansion(mult = c(0, 0.10))) +
      labs(x = sc$axis_label, y = NULL, title = "Top 10") +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.y        = element_text(size = 9),
        plot.margin        = margin(r = 30),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(color = "#e6e6e6")
      )
  })
  
  output$dl_png_series <- downloadHandler(
    filename = function() paste0("serie_", Sys.Date(), ".png"),
    content = function(file) {
      ragg::agg_png(file, width = 1400, height = 900, res = 150)
      print(g_series_gg())
      grDevices::dev.off()
    }
  )
  
  output$dl_png_ranking <- downloadHandler(
    filename = function() paste0("ranking_top10_", Sys.Date(), ".png"),
    content = function(file) {
      ragg::agg_png(file, width = 1400, height = 900, res = 150)
      print(g_ranking_gg())
      grDevices::dev.off()
    }
  )
  
  output$dl_png_clusters <- downloadHandler(
    filename = function() paste0("clusters_con_leyenda_", Sys.Date(), ".png"),
    content = function(file) {
      ok <- save_leaflet_png_retry(
        widget     = map_clusters_simple(),
        file       = file,
        vwidth     = 2600,
        vheight    = 1800,
        zoom       = 1.8,
        delay_base = 4
      )
      
      if (!isTRUE(ok)) {
        showNotification("No se pudo exportar el mapa de clusters.", type = "error", duration = NULL)
        stop("No se pudo exportar el mapa de clusters.")
      }
    }
  )
  
  hhi_year_col <- reactive({
    b <- input$hhi_base
    if (is.null(b)) return("ano_cosechado")
    if (b == "area_sembrada_ha") "ano_sembrado" else "ano_cosechado"
  })
  
  hhi_years_available <- reactive({
    req(input$hhi_base)
    ycol <- hhi_year_col()
    df <- eva_df
    if (!is.null(input$hhi_depto) && input$hhi_depto != "Todos") df <- df %>% dplyr::filter(DEPARTAMENTO_D == input$hhi_depto)
    if (!is.null(input$hhi_mpio)  && input$hhi_mpio  != "Todos") df <- df %>% dplyr::filter(MUNICIPIO_D == input$hhi_mpio)
    sort(unique(na.omit(df[[ycol]])))
  })
  
  observeEvent(list(input$hhi_base, input$hhi_depto, input$hhi_mpio), {
    yrs <- hhi_years_available()
    if (!length(yrs)) return()
    current  <- isolate(input$hhi_anio)
    selected <- if (!is.null(current) && current %in% yrs) current else max(yrs)
    updateSelectInput(session, "hhi_anio", choices = yrs, selected = selected)
  }, ignoreInit = TRUE)
  
  observeEvent(input$hhi_depto, {
    dep <- input$hhi_depto
    if (is.null(dep) || dep == "Todos") {
      munis <- sort(unique(eva_df$MUNICIPIO_D))
    } else {
      munis <- sort(unique(eva_df$MUNICIPIO_D[eva_df$DEPARTAMENTO_D == dep]))
    }
    updateSelectInput(session, "hhi_mpio", choices  = c("Todos" = "Todos", stats::setNames(munis, title_case_es(munis))), selected = "Todos")
  }, ignoreInit = FALSE)
  
  output$hhi_titulo_serie  <- renderText({ "¿Cómo ha evolucionado la diversificación productiva a lo largo del tiempo?" })
  output$hhi_titulo_barras <- renderText({ "¿Qué territorios presentan mayor diversificación productiva agrícola?" })
  
  hhi_series_raw <- reactive({
    req(input$hhi_base)
    ycol <- hhi_year_col()
    
    base_col <- switch(
      input$hhi_base,
      "produccion_t"      = "prod_num",
      "area_cosechada_ha" = "area_cnum",
      "area_sembrada_ha"  = "area_snum",
      "prod_num"
    )
    
    df <- eva_df %>%
      dplyr::mutate(
        prod_num  = suppressWarnings(as.numeric(produccion_t)),
        area_cnum = suppressWarnings(as.numeric(area_cosechada_ha)),
        area_snum = suppressWarnings(as.numeric(area_sembrada_ha)),
        anio_calc = .data[[ycol]]
      )
    df$base_val <- df[[base_col]]
    
    dep <- input$hhi_depto
    mp  <- input$hhi_mpio
    
    if (!is.null(mp) && mp != "Todos") {
      df <- df %>% dplyr::filter(MUNICIPIO_D == mp)
    } else if (!is.null(dep) && dep != "Todos") {
      df <- df %>% dplyr::filter(DEPARTAMENTO_D == dep)
    }
    
    if (nrow(df) == 0) return(data.frame(anio = numeric(0), HHI = numeric(0), grupo = character(0)))
    
    out <- df %>%
      dplyr::group_by(anio_calc, cultivo) %>%
      dplyr::summarise(v = sum(base_val, na.rm = TRUE), .groups = "drop") %>%
      dplyr::group_by(anio_calc) %>%
      dplyr::mutate(tot = sum(v, na.rm = TRUE), s = dplyr::if_else(tot > 0, v / tot, NA_real_)) %>%
      dplyr::summarise(HHI = sum(s^2, na.rm = TRUE), .groups = "drop") %>%
      dplyr::rename(anio = anio_calc)
    
    grupo_label <- if (!is.null(mp) && mp != "Todos") {
      title_case_es(mp)
    } else if (!is.null(dep) && dep != "Todos") {
      paste0("Departamento — ", title_case_es(dep))
    } else {
      "Nacional"
    }
    
    out$grupo <- grupo_label
    out
  })
  
  output$hhi_serie <- plotly::renderPlotly({
    sr <- hhi_series_raw()
    if (nrow(sr) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(annotations = list(text = "Sin datos para la combinación seleccionada", x = 0.5, y = 0.5, showarrow = FALSE)) %>%
               plotly::config(locale = "es"))
    }
    
    sr <- sr %>% dplyr::mutate(Diversificacion = pmin(pmax(1 - HHI, 0), 1))
    
    plotly::plot_ly(
      data = sr, x = ~anio, y = ~Diversificacion,
      type = "scatter", mode = "lines+markers",
      line = list(width = 2.5, color = "#3182bd"),
      marker = list(size = 8, color = "#08519c"),
      hovertemplate = "<b>Año %{x}</b><br>Diversificación (1 - HHI): %{y:.3f}<extra></extra>"
    ) %>%
      plotly::layout(
        yaxis = list(title = "Diversificación productiva (1 - HHI)", range = c(0, 1), gridcolor = "#e6e6e6", showgrid = TRUE),
        xaxis = list(title = "", tickmode = "linear", dtick = 1, gridcolor = "transparent", showgrid = FALSE),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        margin = list(l = 60, r = 20, t = 10, b = 40)
      ) %>%
      plotly::config(locale = "es")
  })
  
  eva_hhi <- reactive({
    req(input$hhi_base)
    ycol <- hhi_year_col()
    
    base_col <- switch(
      input$hhi_base,
      "produccion_t"      = "prod_num",
      "area_cosechada_ha" = "area_cnum",
      "area_sembrada_ha"  = "area_snum",
      "prod_num"
    )
    
    df <- eva_df %>%
      dplyr::mutate(
        prod_num  = suppressWarnings(as.numeric(produccion_t)),
        area_cnum = suppressWarnings(as.numeric(area_cosechada_ha)),
        area_snum = suppressWarnings(as.numeric(area_sembrada_ha)),
        anio_calc = .data[[ycol]]
      )
    df$base_val <- df[[base_col]]
    
    if (!is.null(input$hhi_depto) && input$hhi_depto != "Todos") df <- df %>% dplyr::filter(DEPARTAMENTO_D == input$hhi_depto)
    if (!is.null(input$hhi_mpio)  && input$hhi_mpio  != "Todos") df <- df %>% dplyr::filter(MUNICIPIO_D == input$hhi_mpio)
    
    df
  })
  
  hhi_by_year <- reactive({
    if (is.null(input$hhi_anio)) return(data.frame(grupo = character(0), valor = numeric(0)))
    df <- eva_hhi()
    if (nrow(df) == 0) return(data.frame(grupo = character(0), valor = numeric(0)))
    
    dep <- input$hhi_depto
    mp  <- input$hhi_mpio
    
    df <- df %>% dplyr::filter(anio_calc == input$hhi_anio)
    if (nrow(df) == 0) return(data.frame(grupo = character(0), valor = numeric(0)))
    
    if (is.null(dep) || dep == "Todos") {
      sums <- df %>%
        dplyr::group_by(DEPARTAMENTO_D, cultivo) %>%
        dplyr::summarise(v = sum(base_val, na.rm = TRUE), .groups = "drop")
      
      hhi <- sums %>%
        dplyr::group_by(DEPARTAMENTO_D) %>%
        dplyr::mutate(tot = sum(v, na.rm = TRUE), s = dplyr::if_else(tot > 0, v / tot, NA_real_)) %>%
        dplyr::summarise(HHI = sum(s^2, na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(grupo = DEPARTAMENTO_D)
      
      return(hhi %>% dplyr::select(grupo, valor = HHI))
    }
    
    if (!is.null(mp) && mp != "Todos") df <- df %>% dplyr::filter(MUNICIPIO_D == mp)
    
    sums <- df %>%
      dplyr::group_by(MUNICIPIO_D, cultivo) %>%
      dplyr::summarise(v = sum(base_val, na.rm = TRUE), .groups = "drop")
    if (nrow(sums) == 0) return(data.frame(grupo = character(0), valor = numeric(0)))
    
    hhi <- sums %>%
      dplyr::group_by(MUNICIPIO_D) %>%
      dplyr::mutate(tot = sum(v, na.rm = TRUE), s = dplyr::if_else(tot > 0, v / tot, NA_real_)) %>%
      dplyr::summarise(HHI = sum(s^2, na.rm = TRUE), .groups = "drop") %>%
      dplyr::rename(grupo = MUNICIPIO_D)
    
    hhi %>% dplyr::select(grupo, valor = HHI)
  })
  
  output$hhi_barras <- plotly::renderPlotly({
    md <- hhi_by_year()
    if (nrow(md) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(annotations = list(text = "Sin datos para la combinación seleccionada", x = 0.5, y = 0.5, showarrow = FALSE)) %>%
               plotly::config(locale = "es"))
    }
    
    md <- md %>%
      dplyr::mutate(div_valor = pmin(pmax(1 - valor, 0), 1), grupo_lbl = title_case_es(grupo)) %>%
      dplyr::arrange(dplyr::desc(div_valor)) %>%
      dplyr::mutate(
        grupo_ord   = factor(grupo_lbl, levels = rev(grupo_lbl)),
        texto_valor = sprintf("%.3f", div_valor),
        color_val   = dplyr::case_when(
          div_valor > 0.7 ~ "#08306b",
          div_valor > 0.5 ~ "#08519c",
          div_valor > 0.3 ~ "#3182bd",
          div_valor > 0.1 ~ "#6baed6",
          TRUE            ~ "#9ecae1"
        )
      )
    
    plotly::plot_ly(
      data = md,
      x = ~div_valor,
      y = ~grupo_ord,
      type = "bar",
      orientation = "h",
      marker = list(color = ~color_val),
      text = ~texto_valor,
      textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(color = "white", size = 12, family = "Inter"),
      hovertemplate = "<b>%{y}</b><br>Diversificación (1 - HHI): %{x:.3f}<extra></extra>",
      showlegend = FALSE
    ) %>%
      plotly::layout(
        xaxis = list(title = "Diversificación (1 - HHI)", range = c(0, 1), gridcolor = "#e6e6e6", showgrid = TRUE, zeroline = FALSE),
        yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 11, family = "Inter"), showgrid = FALSE, zeroline = FALSE),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        margin = list(l = if (is.null(input$hhi_depto) || input$hhi_depto == "Todos") 150 else 180, r = 40, t = 20, b = 40),
        uniformtext = list(minsize = 10, mode = "hide")
      ) %>%
      plotly::config(locale = "es")
  })
  
  g_hhi_serie_gg <- reactive({
    sr <- hhi_series_raw()
    if (nrow(sr) == 0) return(plot_vacio_gg())
    
    sr <- sr %>% dplyr::mutate(Diversificacion = pmin(pmax(1 - HHI, 0), 1))
    
    ggplot(sr, aes(x = anio, y = Diversificacion)) +
      geom_line(linewidth = 1, color = "#3182bd") +
      geom_point(size = 2.5, color = "#08519c") +
      scale_x_continuous(breaks = unique(sr$anio)) +
      scale_y_continuous(limits = c(0, 1), labels = function(x) sprintf("%.2f", x)) +
      labs(x = NULL, y = "Diversificación productiva (1 - HHI)", title = "Evolución de la diversificación productiva") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
  })
  
  g_hhi_barras_gg <- reactive({
    md <- hhi_by_year()
    if (nrow(md) == 0) return(plot_vacio_gg())
    
    md <- md %>%
      dplyr::mutate(div_valor = pmin(pmax(1 - valor, 0), 1), grupo_lbl = title_case_es(grupo)) %>%
      dplyr::arrange(dplyr::desc(div_valor))
    
    ggplot(md, aes(x = div_valor, y = reorder(grupo_lbl, div_valor))) +
      geom_col(fill = "#3182bd") +
      scale_x_continuous(limits = c(0, 1), labels = function(x) sprintf("%.2f", x), expand = expansion(mult = c(0, 0.02))) +
      labs(x = "Diversificación (1 - HHI)", y = NULL, title = paste0("Diversificación productiva — ", input$hhi_anio)) +
      theme_minimal(base_size = 12) +
      theme(axis.text.y = element_text(size = 9), panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())
  })
  
  output$dl_png_hhi_serie <- downloadHandler(
    filename = function() paste0("hhi_serie_", Sys.Date(), ".png"),
    content = function(file) {
      ragg::agg_png(file, width = 1400, height = 900, res = 150)
      print(g_hhi_serie_gg())
      grDevices::dev.off()
    }
  )
  
  output$dl_png_hhi_barras <- downloadHandler(
    filename = function() paste0("hhi_barras_", Sys.Date(), ".png"),
    content = function(file) {
      ragg::agg_png(file, width = 1400, height = 1100, res = 150)
      print(g_hhi_barras_gg())
      grDevices::dev.off()
    }
  )
  
  filtros_informe <- reactive({
    data.frame(
      Parametro = c(
        "Tab 1 - Año","Tab 1 - Departamento","Tab 1 - Municipio","Tab 1 - Cultivo","Tab 1 - Indicador",
        "Tab 2 - Año","Tab 2 - Departamento","Tab 2 - Cultivo","Tab 2 - Indicador",
        "Tab 3 - Año","Tab 3 - Departamento","Tab 3 - Municipio","Tab 3 - Base"
      ),
      Valor = c(
        input$f_anio %||% "",
        ifelse(is.null(input$f_depto), "", ifelse(input$f_depto == "Todos", "Todos", title_case_es(input$f_depto))),
        ifelse(is.null(input$f_mpio), "", ifelse(input$f_mpio == "Todos", "Todos", title_case_es(input$f_mpio))),
        ifelse(is.null(input$f_cultivo), "", ifelse(input$f_cultivo == "Todos", "Todos", title_case_es(input$f_cultivo))),
        indicador_label() %||% "",
        input$clus_anio %||% "",
        ifelse(is.null(input$clus_depto), "", title_case_es(input$clus_depto)),
        ifelse(is.null(input$clus_cultivo), "", ifelse(input$clus_cultivo == "Todos", "Todos", title_case_es(input$clus_cultivo))),
        clus_indicador_label() %||% "",
        input$hhi_anio %||% "",
        ifelse(is.null(input$hhi_depto), "", ifelse(input$hhi_depto == "Todos", "Todos", title_case_es(input$hhi_depto))),
        ifelse(is.null(input$hhi_mpio), "", ifelse(input$hhi_mpio == "Todos", "Todos", title_case_es(input$hhi_mpio))),
        input$hhi_base %||% ""
      ),
      stringsAsFactors = FALSE
    )
  })
  
  render_informe_pdf <- function(file) {
    log_file <- file.path(EXPORT_DIR, "debug_pdf_eva.txt")
    
    write_log <- function(...) {
      cat(..., "\n", file = log_file, append = TRUE)
    }
    
    write_log("====================================")
    write_log("Inicio render PDF:", as.character(Sys.time()))
    write_log("ruta_rmd:", ruta_rmd)
    write_log("archivo destino:", file)
    
    if (!file.exists(ruta_rmd)) {
      write_log("ERROR: No existe Informe_descargable.Rmd")
      stop("No encuentro Informe_descargable.Rmd en la raíz del proyecto ni en data/.")
    }
    
    filtros_tbl <- filtros_informe()
    
    write_log("Generando PNG tab1 mapa")
    ok1 <- save_leaflet_png_retry(
      widget     = isolate(map_widget_simple()),
      file       = IMG_TAB1_MAP,
      vwidth     = 2200,
      vheight    = 1500,
      zoom       = 2.2,
      delay_base = 2.5
    )
    if (!isTRUE(ok1)) stop("No se pudo generar Descargas/eva_tab1_mapa.png")
    
    write_log("Generando PNG tab1 serie")
    ok2 <- save_gg_png(
      isolate(g_series_gg()),
      IMG_TAB1_SER,
      width = 1800,
      height = 1000,
      res = 150
    )
    if (!isTRUE(ok2)) stop("No se pudo generar Descargas/eva_tab1_serie.png")
    
    write_log("Generando PNG tab1 ranking")
    ok3 <- save_gg_png(
      isolate(g_ranking_gg()),
      IMG_TAB1_RANK,
      width = 1800,
      height = 1000,
      res = 150
    )
    if (!isTRUE(ok3)) stop("No se pudo generar Descargas/eva_tab1_ranking.png")
    
    write_log("Generando PNG tab2 clusters")
    ok4 <- save_leaflet_png_retry(
      widget     = isolate(map_clusters_simple()),
      file       = IMG_TAB2_MAP,
      vwidth     = 2600,
      vheight    = 1800,
      zoom       = 1.8,
      delay_base = 4
    )
    
    if (!isTRUE(ok4)) {
      write_log("No se pudo generar tab2 clusters en lote; intentando reutilizar archivo existente")
      if (file.exists(IMG_TAB2_MAP) &&
          !is.na(file.info(IMG_TAB2_MAP)$size) &&
          file.info(IMG_TAB2_MAP)$size > 0) {
        write_log("Se reutiliza eva_tab2_clusters.png existente")
      } else {
        stop("No se pudo generar Descargas/eva_tab2_clusters.png")
      }
    }
    
    write_log("Generando PNG tab3 barras")
    ok5 <- save_gg_png(
      isolate(g_hhi_barras_gg()),
      IMG_TAB3_BAR,
      width = 1800,
      height = 1200,
      res = 150
    )
    if (!isTRUE(ok5)) stop("No se pudo generar Descargas/eva_tab3_barras.png")
    
    write_log("Generando PNG tab3 serie")
    ok6 <- save_gg_png(
      isolate(g_hhi_serie_gg()),
      IMG_TAB3_SER,
      width = 1800,
      height = 1000,
      res = 150
    )
    if (!isTRUE(ok6)) stop("No se pudo generar Descargas/eva_tab3_serie.png")
    
    logo_src <- file.path(app_root, "www", "LOGO_PLATEA.png")
    if (!file.exists(logo_src)) {
      logo_src2 <- file.path(app_root, "WWW", "LOGO_PLATEA.png")
      logo_src  <- if (file.exists(logo_src2)) logo_src2 else NA_character_
    }
    
    logo_dst <- file.path(EXPORT_DIR, "LOGO_PLATEA.png")
    if (!is.na(logo_src) && file.exists(logo_src)) {
      file.copy(logo_src, logo_dst, overwrite = TRUE)
    } else {
      stop("No encuentro el logo en www/LOGO_PLATEA.png")
    }
    
    rmd_to_render <- ruta_rmd
    write_log("rmd_to_render:", rmd_to_render)
    
    tryCatch({
      write_log("Iniciando rmarkdown::render()")
      
      tmp_pdf <- tempfile(fileext = ".pdf")
      
      rmarkdown::render(
        input         = rmd_to_render,
        output_format = "pdf_document",
        output_file   = basename(tmp_pdf),
        output_dir    = dirname(tmp_pdf),
        quiet         = TRUE,
        params        = list(
          app_root      = app_root,
          export_dir    = "Descargas",
          filtros       = filtros_tbl,
          img_tab1_map  = basename(IMG_TAB1_MAP),
          img_tab1_ser  = basename(IMG_TAB1_SER),
          img_tab1_rank = basename(IMG_TAB1_RANK),
          img_tab2_map  = basename(IMG_TAB2_MAP),
          img_tab3_bar  = basename(IMG_TAB3_BAR),
          img_tab3_ser  = basename(IMG_TAB3_SER)
        ),
        knit_root_dir = app_root,
        envir         = new.env(parent = globalenv())
      )
      
      if (!file.exists(tmp_pdf) || is.na(file.info(tmp_pdf)$size) || file.info(tmp_pdf)$size <= 0) {
        write_log("ERROR: el PDF temporal no se generó correctamente")
        stop("El PDF temporal no se generó correctamente.")
      }
      
      ok_copy <- file.copy(tmp_pdf, file, overwrite = TRUE)
      
      if (!isTRUE(ok_copy) || !file.exists(file) || is.na(file.info(file)$size) || file.info(file)$size <= 0) {
        write_log("ERROR: no se pudo copiar el PDF final")
        stop("No se pudo copiar el PDF final al archivo de descarga.")
      }
      
      write_log("Render terminado y copiado OK")
      
    }, error = function(e) {
      write_log("ERROR EN RENDER:", conditionMessage(e))
      stop(e)
    })
    
    if (!file.exists(file) || is.na(file.info(file)$size) || file.info(file)$size <= 0) {
      write_log("ERROR: el archivo PDF final no existe o quedó vacío")
      stop("El archivo final PDF no se generó correctamente.")
    }
    
    write_log("PDF final OK:", file)
  }
  
  output$dl_reporte_pdf <- downloadHandler(
    filename = function() {
      anio_tag <- input$f_anio %||% Sys.Date()
      dep_tag  <- if (is.null(input$f_depto) || input$f_depto == "Todos") {
        "Atlantico"
      } else {
        sanitize_filename(title_case_es(input$f_depto))
      }
      mpio_tag <- if (is.null(input$f_mpio) || input$f_mpio == "Todos") {
        "Todos"
      } else {
        sanitize_filename(title_case_es(input$f_mpio))
      }
      
      paste0("Informe_EVA_", dep_tag, "_", mpio_tag, "_", anio_tag, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tryCatch(
        render_informe_pdf(file),
        error = function(e) {
          showNotification(
            paste("Error al generar PDF:", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
          stop(e)
        }
      )
    },
    contentType = "application/pdf"
  )
  
  hhi_kpi_val <- reactive({
    req(input$hhi_anio, input$hhi_base)
    
    ycol <- hhi_year_col()
    base_col <- switch(
      input$hhi_base,
      "produccion_t"      = "prod_num",
      "area_cosechada_ha" = "area_cnum",
      "area_sembrada_ha"  = "area_snum",
      "prod_num"
    )
    
    df <- eva_df %>%
      dplyr::mutate(
        prod_num  = suppressWarnings(as.numeric(produccion_t)),
        area_cnum = suppressWarnings(as.numeric(area_cosechada_ha)),
        area_snum = suppressWarnings(as.numeric(area_sembrada_ha)),
        anio_calc = .data[[ycol]]
      )
    df$base_val <- df[[base_col]]
    
    df <- df %>% dplyr::filter(anio_calc == input$hhi_anio)
    if (nrow(df) == 0) return(NA_real_)
    
    dep <- input$hhi_depto
    mp  <- input$hhi_mpio
    
    compute_hhi <- function(df_scope) {
      if (nrow(df_scope) == 0) return(NA_real_)
      sums <- df_scope %>% dplyr::group_by(cultivo) %>% dplyr::summarise(v = sum(base_val, na.rm = TRUE), .groups = "drop")
      if (nrow(sums) == 0) return(NA_real_)
      tot <- sum(sums$v, na.rm = TRUE)
      if (!is.finite(tot) || tot <= 0) return(NA_real_)
      s <- sums$v / tot
      v <- sum(s^2, na.rm = TRUE)
      if (!is.finite(v)) NA_real_ else v
    }
    
    if (!is.null(mp) && mp != "Todos") return(compute_hhi(df %>% dplyr::filter(MUNICIPIO_D == mp)))
    if (!is.null(dep) && dep != "Todos") return(compute_hhi(df %>% dplyr::filter(DEPARTAMENTO_D == dep)))
    compute_hhi(df)
  })
  
  output$hhi_kpi_valor <- renderText({
    v_hhi <- hhi_kpi_val()
    if (is.na(v_hhi) || !is.finite(v_hhi)) return("—")
    sprintf("%.3f", pmin(pmax(1 - v_hhi, 0), 1))
  })
  
  output$dl_csv_hhi <- downloadHandler(
    filename = function() paste0("eva_hhi_base_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(eva_hhi(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
}

# ------------------------------
# 6) Lanzar App
# ------------------------------
shinyApp(ui = ui, server = server)