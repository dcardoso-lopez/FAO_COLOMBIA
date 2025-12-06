# =========================================================
# Shiny App: EVA — Explorador territorial + Clusters espaciales (municipal)
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
options(OutDec = ",")

# Helper numérico en español (miles = ".", decimales = ",")
comma_es <- function(x, accuracy = NULL) {
  scales::comma(
    x,
    accuracy     = accuracy,
    big.mark     = ".",
    decimal.mark = ","
  )
}

sf::sf_use_s2(FALSE)

# Color de borde global
BORDER_COL <- "#a1d99b"

# URL de tu repositorio (cámbiala)
github_url <- "https://github.com/tu_usuario/tu_repo"

# === Ruta fija de la app y del Rmd ===
APP_DIR  <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/FAO_COLOMBIA/01_Atlántico/03_SNINNY_APP/EVA_A"
RMD_PATH <- file.path(APP_DIR, "informe_eva.Rmd")
if (!file.exists(RMD_PATH)) {
  stop(sprintf("No encuentro el Rmd en: %s", RMD_PATH))
}

# ------------------------------
# 2) Datos: EVA + Shapefiles
# ------------------------------
eva_df <- readRDS(
  "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/FAO_COLOMBIA/01_Atlántico/03_SNINNY_APP/EVA_A/data/011_UPRA_EVA-A.rds"
)
eva_df <- eva_df %>% dplyr::filter(DEPARTAMENTO_D == "ATLÁNTICO")

ruta_shp_mpios <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/FAO_COLOMBIA/01_Atlántico/03_SNINNY_APP/EVA_A/data/shp/MGN_ANM_MPIOS.shp"
ruta_shp_dptos <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/FAO_COLOMBIA/01_Atlántico/03_SNINNY_APP/EVA_A/data/shp/MGN_ANM_DPTOS.shp"

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

mpios_sf <- sf::st_transform(mpios_sf, 4326) |>
  sf::st_make_valid() |>
  sf::st_zm(drop = TRUE, what = "ZM")

depto_sf <- sf::st_transform(depto_sf_raw, 4326) |>
  sf::st_make_valid() |>
  sf::st_zm(drop = TRUE, what = "ZM") |>
  dplyr::mutate(DEPARTAMENTO_D = .data[[depto_name_col]])

# Reemplaza TU definición actual de norm_txt por esta
norm_txt <- function(x) {
  # Quita espacios al inicio/final pero respeta acentos y ñ
  stringi::stri_trim_both(as.character(x))
}

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
# 2.3) Title Case en español (global)
# ------------------------------
title_case_es <- function(x) {
  if (is.null(x)) return(x)
  
  # Palabras que queremos en minúscula cuando NO son la primera
  lower_words <- c(
    "a","ante","bajo","cabe","con","contra","de","del","desde",
    "en","entre","hacia","hasta","para","por","según","sin","so",
    "sobre","tras",
    "el","la","los","las","un","una","unos","unas",
    "y","e","o","u","ni",
    "al"
  )
  
  vapply(as.character(x), function(s) {
    if (is.na(s)) return(NA_character_)
    s <- trimws(s)
    if (s == "") return(s)
    
    # Todo en minúscula primero (mantiene tildes y caracteres especiales)
    s_low  <- tolower(s)
    parts  <- unlist(strsplit(s_low, "\\s+"))
    # Title Case palabra a palabra, respetando locale es
    parts_tc <- stringi::stri_trans_totitle(parts, locale = "es")
    
    if (length(parts_tc) > 1) {
      for (i in 2:length(parts_tc)) {
        w_low <- tolower(parts_tc[i])
        if (w_low %in% lower_words) {
          parts_tc[i] <- w_low
        }
      }
    }
    
    paste(parts_tc, collapse = " ")
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

# ------------------------------
# 2.4) Vectores base para combos
# ------------------------------
dptos_vec <- sort(unique(eva_df$DEPARTAMENTO_D))
mpios_vec <- sort(unique(eva_df$MUNICIPIO_D))

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tags$head(
    # Fuente Inter
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"
    ),
    
    tags$style(HTML(sprintf("
      /* ====== Layout general ====== */
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
      
      .bslib-card {
        margin-bottom: 14px !important;
      }
      .bslib-grid {
        gap: 18px !important;
      }

      /* Contenedor central más angosto */
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
        font-family: 'Inter Tight', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-size: 16px;
        font-weight: 700;        /* bold */
        color: #111827;
        margin-bottom: 8px;
      }

      /* ===== TÍTULOS DE LOS FILTROS ===== */
      .filter-label,
      .filters-card .control-label,
      .card .control-label {
        font-family: 'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-size:14px;
        font-weight:500; /* medium */
        margin-bottom:4px;
      }

      .top-filters .col-sm-2, .top-filters .col-sm-7 { margin-bottom:10px; }

      .filters-card {
        margin-bottom:18px;
      }

      .btn-group .btn { margin-right:6px; margin-bottom:6px; }
      .dl-footer { margin-top:10px; text-align:right; }
      .dl-under  { margin-top:8px; text-align:right; }

      .btn, .btn-default {
        font-size:12px;
        padding:6px 10px;
        border-radius:8px;
      }
      .btn + .btn { margin-left:6px; }

      /* Tarjetas visuales con borde verde */
      .viz-card {
        border:1.5px solid %s !important;
        border-radius:12px;
        box-shadow:0 1px 6px rgba(0,0,0,0.05);
      }

      /* ====== Bordes/acentos #a1d99b para filtros ====== */
      .form-control,
      .form-select {
        border-color:%s !important;
        border-width:1.5px;
        border-radius:8px;
      }
      .form-control:focus,
      .form-select:focus {
        border-color:%s !important;
        box-shadow:0 0 0 0.2rem rgba(161,217,155,0.15);
      }

      .selectize-control .selectize-input {
        border-color:%s !important;
        border-width:1.5px;
        border-radius:8px;
      }
      .selectize-control .selectize-input.focus {
        border-color:%s !important;
        box-shadow:0 0 0 0.2rem rgba(161,217,155,0.15);
      }

      .bootstrap-select .dropdown-toggle {
        border-color:%s !important;
        border-width:1.5px;
        border-radius:8px;
      }
      .bootstrap-select .dropdown-toggle:focus,
      .bootstrap-select .dropdown-toggle:active {
        border-color:%s !important;
        box-shadow:0 0 0 0.2rem rgba(161,217,155,0.15);
      }

      /* Slider (ionRangeSlider) */
      .irs--shiny .irs-line { background:#a1d99b22; border-color:%s; }
      .irs--shiny .irs-bar  { background:%s; border-color:%s; }
      .irs--shiny .irs-handle > i:first-child { background-color:%s; border-color:%s; }
      .irs--shiny .irs-single,
      .irs--shiny .irs-from,
      .irs--shiny .irs-to {
        background:%s;
        border-color:%s;
      }

      /* Checks / radios */
      .form-check-input:checked {
        background-color:%s;
        border-color:%s;
      }
      .form-check-input:focus {
        border-color:%s;
        box-shadow:0 0 0 0.2rem rgba(161,217,155,0.15);
      }

      /* Tabs en negrilla */
      .nav-tabs .nav-link { font-weight:700; }
      .nav-tabs .nav-link.active { font-weight:800; }
      
      /* Estilos para mejor espaciado */
      .nav-panel {
        padding-top: 5px;
      }
      .viz-card {
        padding: 12px !important;
      }

    ",
                            BORDER_COL,  # .viz-card
                            BORDER_COL, BORDER_COL,  # form-control / select focus
                            BORDER_COL, BORDER_COL,  # selectize
                            BORDER_COL, BORDER_COL,  # bootstrap-select
                            BORDER_COL,              # slider line
                            BORDER_COL, BORDER_COL,  # slider bar
                            BORDER_COL, BORDER_COL,  # slider handle
                            BORDER_COL, BORDER_COL,  # slider labels
                            BORDER_COL, BORDER_COL,  # checks
                            BORDER_COL               # check focus
    )))
  ),
  
  div(
    class = "eva-wrap",
    
    h2("Explorador territorial de indicadores agrícolas (EVA)", id = "app-title"),
    
    bslib::navset_tab(
      id = "tabs",
      
      # =========================
      # === Tab 1: Explorador ===
      # =========================
      bslib::nav_panel(
        "Explorador territorial de indicadores productivos",
        
        # Filtros superiores
        fluidRow(
          column(
            width = 12,
            div(
              class = "card viz-card filters-card",
              fluidRow(
                class = "top-filters",
                # 1) Año
                column(
                  width = 2,
                  div(class = "filter-label", "¿Qué año analizamos?"),
                  uiOutput("anio_ui")
                ),
                # 2) Depto
                column(
                  width = 3,
                  div(class = "filter-label", "¿En qué departamento?"),
                  selectInput(
                    "f_depto", NULL,
                    choices = c(
                      "Todos" = "Todos",
                      stats::setNames(dptos_vec, title_case_es(dptos_vec))
                    ),
                    selected = "ATLÁNTICO"
                  )
                ),
                # 3) Municipio
                column(
                  width = 3,
                  div(class = "filter-label", "¿Algún municipio en particular?"),
                  selectInput(
                    "f_mpio", NULL,
                    choices = c(
                      "Todos" = "Todos",
                      stats::setNames(mpios_vec, title_case_es(mpios_vec))
                    ),
                    selected = "Todos"
                  )
                ),
                # 4) Cultivo
                column(
                  width = 2,
                  div(class = "filter-label", "¿Cuál cultivo?"),
                  selectInput(
                    "f_cultivo", NULL,
                    choices = c(
                      "Todos" = "Todos",
                      stats::setNames(
                        sort(unique(eva_df$cultivo)),
                        title_case_es(sort(unique(eva_df$cultivo)))
                      )
                    ),
                    selected = "Todos"
                  )
                ),
                # 5) Variable (último filtro)
                column(
                  width = 2,
                  div(class = "filter-label", "Variable a considerar"),
                  selectInput(
                    "f_indicador", NULL,
                    choices = c(
                      "Área sembrada (Ha)"     = "area_sembrada_ha",
                      "Área cosechada (Ha)"    = "area_cosechada_ha",
                      "Producción (Ton)"       = "produccion_t",
                      "Rendimiento (Ton/Ha)"   = "rendimiento_t_ha"
                    ),
                    selected = "area_sembrada_ha"
                  )
                )
              )
            )
          )
        ),
        
        # Layout: mapa + dos gráficos
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
              div(
                class = "dl-under",
                downloadButton("dl_png_mapa", label = "PNG — Mapa (simple)")
              )
            )
          ),
          column(
            width = 6,
            div(
              class = "card viz-card right-pane",
              h5(class = "card-title", textOutput("titulo_serie")),
              plotlyOutput("plot_arriba", height = "290px"),
              div(
                class = "dl-under",
                downloadButton("dl_png_series", label = "PNG — Serie temporal")
              )
            ),
            div(
              class = "card viz-card right-pane",
              h5(class = "card-title", textOutput("titulo_ranking")),
              plotlyOutput("ranking_abajo", height = "290px"),
              div(
                class = "dl-under",
                downloadButton("dl_png_ranking", label = "PNG — Ranking Top-10")
              )
            )
          )
        ),
        
        # Pie Tab 1
        fluidRow(
          column(
            width = 12,
            div(
              class = "dl-footer",
              downloadButton("dl_csv_expl", label = "Descargar CSV"),
              downloadButton("dl_pdf_expl", label = "Informe PDF (Rmd aparte)"),
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
      
      # =========================
      # === Tab 2: CLUSTERS   ===
      # =========================
      bslib::nav_panel(
        "Análisis de aglomeración y productos representativos",
        
        fluidRow(
          column(
            width = 6,
            div(
              class = "card viz-card",
              h5(class = "card-title", textOutput("clus_titulo_mapa")),
              
              # === Filtros en 2 filas x 2 columnas ===
              fluidRow(
                # Fila 1: Año + Depto
                column(
                  width = 6,
                  uiOutput("clus_anio_ui")
                ),
                column(
                  width = 6,
                  selectInput(
                    "clus_depto", "¿En qué departamento?",
                    choices  = stats::setNames(
                      sort(unique(eva_df$DEPARTAMENTO_D)),
                      title_case_es(sort(unique(eva_df$DEPARTAMENTO_D)))
                    ),
                    selected = sort(unique(eva_df$DEPARTAMENTO_D))[1]
                  )
                )
              ),
              fluidRow(
                # Fila 2: Cultivo + Variable
                column(
                  width = 6,
                  selectInput(
                    "clus_cultivo", "¿Cuál cultivo?",
                    choices  = c(
                      "Todos" = "Todos",
                      stats::setNames(
                        sort(unique(eva_df$cultivo)),
                        title_case_es(sort(unique(eva_df$cultivo)))
                      )
                    ),
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
              div(
                class = "dl-under",
                downloadButton("dl_png_clusters", label = "PNG — Mapa Clusters (simple)")
              )
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
        
        # Pie Tab 2
        fluidRow(
          column(
            width = 12,
            div(
              class = "dl-footer",
              downloadButton("dl_csv_clus", label = "Descargar CSV"),
              downloadButton("dl_pdf_clus", label = "Informe PDF (Rmd aparte)"),
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
      
      # =========================
      # === Tab 3: HHI / DIV  ===
      # =========================
      bslib::nav_panel(
        "Diversificación de cultivos",
        
        # Controles en tarjeta de filtros
        fluidRow(
          column(
            width = 12,
            div(
              class = "card viz-card filters-card",
              fluidRow(
                # 1) Año
                column(
                  3,
                  div(class = "filter-label", "¿Qué año analizamos?"),
                  selectInput(
                    "hhi_anio", NULL,
                    choices = c("2020", "2021", "2022", "2023", "2024"),
                    selected = "2024"
                  )
                ),
                # 2) Departamento
                column(
                  3,
                  div(class = "filter-label", "¿En qué departamento?"),
                  selectInput(
                    "hhi_depto", NULL,
                    choices  = c(
                      "Todos" = "Todos",
                      stats::setNames(dptos_vec, title_case_es(dptos_vec))
                    ),
                    selected = "ATLÁNTICO"
                  )
                ),
                # 3) Municipio
                column(
                  3,
                  div(class = "filter-label", "¿Algún municipio en particular?"),
                  selectInput(
                    "hhi_mpio", NULL,
                    choices = c("Todos" = "Todos"),
                    selected = "Todos"
                  )
                ),
                # 4) Variable a considerar (último filtro)
                column(
                  3,
                  selectInput(
                    "hhi_base", "Variable a considerar",
                    choices = c(
                      "Producción (Ton)"      = "produccion_t",
                      "Área cosechada (Ha)"   = "area_cosechada_ha",
                      "Área sembrada (Ha)"    = "area_sembrada_ha"
                    ),
                    selected = "produccion_t"
                  )
                )
              )
            )
          )
        ),
        
        # Layout: barras (izq) + serie (der) + KPI debajo
        fluidRow(
          style = "margin-top: 5px;",
          
          # Columna izquierda - Barras
          column(
            width = 6,
            style = "padding-right: 10px;",
            div(
              class = "card viz-card",
              style = "margin-bottom: 15px; height: 800px;",
              h5(class = "card-title", textOutput("hhi_titulo_barras")),
              plotlyOutput("hhi_barras", height = "720px")
            )
          ),
          
          # Columna derecha - Serie temporal + KPI
          column(
            width = 6,
            style = "padding-left: 10px;",
            
            # --- Serie temporal ---
            div(
              class = "card viz-card",
              style = "margin-bottom: 15px; height: 500px;",
              h5(class = "card-title", textOutput("hhi_titulo_serie")),
              plotlyOutput("hhi_serie", height = "420px")
            ),
            
            # --- KPI de diversificación ---
            div(
              class = "card viz-card",
              style = "height: 285px;",
              h5(class = "card-title", "Índice de Diversificación de Cultivos en los territorios seleccionados"),
              
              div(
                style = "margin-top: 20px; margin-bottom: 10px;
                         display:flex; align-items:baseline; gap:10px;",
                span(
                  textOutput("hhi_kpi_valor"),
                  style = "font-size: 24px; font-weight: 800; color:#08519c;"
                ),
                span(
                  "Diversificación productiva (1 - HHI, 0–1)",
                  style = "font-size: 14px; color:#666;"
                )
              ),
              
              p(
                "Este indicador se calcula como 1 menos el índice Herfindahl-Hirschman (HHI) de la distribución de cultivos en el ámbito seleccionado (país, departamento o municipio), para el año y la base de referencia elegidos (producción, área cosechada o área sembrada).",
                style = "font-size: 13px; color:#555; margin-top: 5px;"
              ),
              p(
                "En términos prácticos, valores cercanos a 1 indican una alta diversificación productiva: la base seleccionada está repartida entre varios cultivos con pesos relativamente similares. Valores cercanos a 0 indican una alta concentración, es decir, uno o pocos cultivos dominan la estructura productiva del territorio.",
                style = "font-size: 13px; color:#555;"
              )
            )
          )
        ),
        
        # Pie Tab 3
        fluidRow(
          column(
            width = 12,
            div(
              class = "dl-footer",
              style = "margin-top: 15px;",
              downloadButton("dl_csv_hhi", label = "Descargar CSV"),
              downloadButton("dl_pdf_hhi", label = "Informe PDF (Rmd aparte)"),
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

# ======== PALETA DINÁMICA POR INDICADOR (4 rangos = cuartiles) ========

# Mezcla de 2 colores en hex
mix_hex <- function(c1, c2, t) {
  r1 <- grDevices::col2rgb(c1)[,1] / 255
  r2 <- grDevices::col2rgb(c2)[,1] / 255
  m  <- (1 - t) * r1 + t * r2
  grDevices::rgb(m[1], m[2], m[3])
}

# Paleta de 4 tonos desde blanco hasta el color base
shades4_from_base <- function(base_col) {
  ts <- c(0.10, 0.40, 0.70, 1.00)
  vapply(ts, function(tt) mix_hex("#FFFFFF", base_col, tt), character(1))
}

# Bins para cuartiles (5 cortes → 4 rangos) de min a max
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

# Formato numérico para etiquetas de rangos (es-CO)
fmt_bin <- function(x){
  comma_es(x, accuracy = 1)
}

# Construye paleta + etiquetas estilo:
#  1er tramo: "a – b"
#  2–4:       "> a – b"
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
      if (i == 1) {
        sprintf("%s – %s", sa, sb)
      } else {
        sprintf("> %s – %s", sa, sb)
      }
    },
    character(1)
  )
  
  mids  <- (bins[-length(bins)] + bins[-1]) / 2
  cols  <- pal(mids)
  
  list(
    bins   = bins,
    pal    = pal,
    labels = labs,
    colors = cols
  )
}

# ======== PALETA 5 RANGOS (si la quieres seguir usando en otros módulos/PNGs) ========
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
  leaflet::colorBin(
    palette  = pals,
    bins     = bins,
    domain   = values,
    na.color = "#f0f0f0",
    right    = FALSE
  )
}


server <- function(input, output, session) {
  
  # ===== Helpers indicador rendimiento calculado =====
  is_yield     <- reactive({ identical(input$f_indicador,  "rendimiento_t_ha") })
  clus_is_yield<- reactive({ identical(input$clus_indicador,"rendimiento_t_ha") })
  
  # ===== Helper para títulos storytelling (Tab 1) =====
  build_titulo_tab1 <- function(tipo = c("mapa", "serie", "ranking"),
                                nivel = c("depto", "mpio"),
                                ind,
                                cultivo = NULL,
                                lbl_ind = NULL) {
    tipo  <- match.arg(tipo)
    nivel <- match.arg(nivel)
    
    # Texto de cultivo (opcional)
    if (is.null(cultivo) || cultivo == "Todos" || is.na(cultivo) || cultivo == "") {
      cult_txt <- ""
    } else {
      cult_txt <- paste0(" de ", title_case_es(cultivo))
    }
    
    # Etiqueta genérica si llega algo raro
    lbl_ind <- if (is.null(lbl_ind)) ind else lbl_ind
    
    # --------- Casos por indicador ---------
    if (ind == "area_sembrada_ha") {
      if (tipo == "mapa") {
        if (nivel == "depto") {
          return(paste0(
            "¿En qué departamentos hay una mayor cantidad de hectáreas sembradas",
            cult_txt, "?"
          ))
        } else {
          return(paste0(
            "¿En qué municipios hay una mayor cantidad de hectáreas sembradas",
            cult_txt, "?"
          ))
        }
      }
      if (tipo == "serie") {
        return(paste0(
          "¿Cómo ha evolucionado en el tiempo la cantidad de hectáreas sembradas",
          cult_txt, "?"
        ))
      }
      if (tipo == "ranking") {
        return(paste0(
          "¿Qué municipios tienen una mayor cantidad de hectáreas sembradas",
          cult_txt, "?"
        ))
      }
    }
    
    if (ind == "area_cosechada_ha") {
      if (tipo == "mapa") {
        if (nivel == "depto") {
          return(paste0(
            "¿En qué departamentos hay una mayor cantidad de hectáreas cosechadas",
            cult_txt, "?"
          ))
        } else {
          return(paste0(
            "¿En qué municipios hay una mayor cantidad de hectáreas cosechadas",
            cult_txt, "?"
          ))
        }
      }
      if (tipo == "serie") {
        return(paste0(
          "¿Cómo ha evolucionado en el tiempo la cantidad de hectáreas cosechadas",
          cult_txt, "?"
        ))
      }
      if (tipo == "ranking") {
        return(paste0(
          "¿Qué municipios tienen una mayor cantidad de hectáreas cosechadas",
          cult_txt, "?"
        ))
      }
    }
    
    if (ind == "produccion_t") {
      if (tipo == "mapa") {
        if (nivel == "depto") {
          return(paste0(
            "¿En qué departamentos se concentra el mayor volumen de producción agrícola (toneladas)",
            cult_txt, "?"
          ))
        } else {
          return(paste0(
            "¿En qué municipios se concentra el mayor volumen de producción agrícola (toneladas)",
            cult_txt, "?"
          ))
        }
      }
      if (tipo == "serie") {
        return(paste0(
          "¿Cómo ha evolucionado en el tiempo el volumen de producción agrícola (toneladas)",
          cult_txt, "?"
        ))
      }
      if (tipo == "ranking") {
        return(paste0(
          "¿Qué municipios concentran el mayor volumen de producción agrícola (toneladas)",
          cult_txt, "?"
        ))
      }
    }
    
    if (ind == "rendimiento_t_ha") {
      if (tipo == "mapa") {
        if (nivel == "depto") {
          return(paste0(
            "¿En qué departamentos se observan los mayores niveles de rendimiento (ton/ha)",
            cult_txt, "?"
          ))
        } else {
          return(paste0(
            "¿En qué municipios se observan los mayores niveles de rendimiento (ton/ha)",
            cult_txt, "?"
          ))
        }
      }
      if (tipo == "serie") {
        return(paste0(
          "¿Cómo ha evolucionado en el tiempo el rendimiento promedio (ton/ha)",
          cult_txt, "?"
        ))
      }
      if (tipo == "ranking") {
        return(paste0(
          "¿Qué municipios presentan los mayores niveles de rendimiento (ton/ha)",
          cult_txt, "?"
        ))
      }
    }
    
    # --------- Fallback genérico ---------
    if (tipo == "mapa") {
      if (nivel == "depto") {
        return(paste0("¿En qué departamentos se concentra el indicador ", lbl_ind, "?"))
      } else {
        return(paste0("¿En qué municipios se concentra el indicador ", lbl_ind, "?"))
      }
    }
    if (tipo == "serie") {
      return(paste0("¿Cómo ha evolucionado en el tiempo el indicador ", lbl_ind, "?"))
    }
    if (tipo == "ranking") {
      return(paste0("¿Qué municipios lideran el indicador ", lbl_ind, "?"))
    }
  }
  
  # ===== TÍTULO DINÁMICO (p/ compatibilidad, aunque ya no lo usamos directo) =====
  indicador_titulo <- reactive({
    ind <- input$f_indicador
    if (is.null(ind) || is.na(ind)) ind <- "area_sembrada_ha"
    switch(
      ind,
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
      if (is.null(dep) || dep == "Todos" || is.na(dep)) {
        "en el país"
      } else {
        paste("en", title_case_es(dep))
      }
    }
  })
  
  anio_frase <- reactive({
    a <- input$f_anio
    if (is.null(a) || is.na(a)) "" else paste0(" (", a, ")")
  })
  
  # Color único por indicador (útil para ranking y serie)
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
  
  # ===== TAB 1 =====
  # Cascada dpto -> mpio con etiquetas en Title Case
  observeEvent(input$f_depto, ignoreInit = TRUE, {
    if (is.null(input$f_depto) || input$f_depto == "Todos") {
      munis <- sort(unique(eva_df$MUNICIPIO_D))
    } else {
      munis <- sort(unique(eva_df$MUNICIPIO_D[eva_df$DEPARTAMENTO_D == input$f_depto]))
    }
    updateSelectInput(
      session, "f_mpio",
      choices  = c("Todos" = "Todos",
                   stats::setNames(munis, title_case_es(munis))),
      selected = "Todos"
    )
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
    if (!is.null(input$f_depto)   && input$f_depto   != "Todos") df <- df |> dplyr::filter(.data$DEPARTAMENTO_D == input$f_depto)
    if (!is.null(input$f_mpio)    && input$f_mpio    != "Todos") df <- df |> dplyr::filter(.data$MUNICIPIO_D    == input$f_mpio)
    if (!is.null(input$f_cultivo) && input$f_cultivo != "Todos") df <- df |> dplyr::filter(.data$cultivo       == input$f_cultivo)
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
  
  # Estado de nivel del mapa
  nivel_mapa <- reactiveVal("depto")
  depto_sel  <- reactiveVal(NULL)
  
  output$nivel_txt <- renderText({
    if (nivel_mapa() == "depto") {
      "Nivel: Departamentos"
    } else {
      paste0("Nivel: Municipios — ", title_case_es(depto_sel()))
    }
  })
  
  # --- Títulos Tab 1 en modo storytelling ---
  output$titulo_mapa <- renderText({
    ind  <- if (is.null(input$f_indicador) || is.na(input$f_indicador)) "area_sembrada_ha" else input$f_indicador
    lbl  <- as.character(indicador_label())
    cult <- if (is.null(input$f_cultivo)) "Todos" else input$f_cultivo
    niv  <- nivel_mapa()
    
    build_titulo_tab1(
      tipo    = "mapa",
      nivel   = niv,
      ind     = ind,
      cultivo = cult,
      lbl_ind = lbl
    )
  })
  
  output$titulo_serie <- renderText({
    ind  <- if (is.null(input$f_indicador) || is.na(input$f_indicador)) "area_sembrada_ha" else input$f_indicador
    lbl  <- as.character(indicador_label())
    cult <- if (is.null(input$f_cultivo)) "Todos" else input$f_cultivo
    
    build_titulo_tab1(
      tipo    = "serie",
      nivel   = "depto",   # da igual aquí
      ind     = ind,
      cultivo = cult,
      lbl_ind = lbl
    )
  })
  
  output$titulo_ranking <- renderText({
    ind  <- if (is.null(input$f_indicador) || is.na(input$f_indicador)) "area_sembrada_ha" else input$f_indicador
    lbl  <- as.character(indicador_label())
    cult <- if (is.null(input$f_cultivo)) "Todos" else input$f_cultivo
    
    build_titulo_tab1(
      tipo    = "ranking",
      nivel   = "mpio",    # ranking es a nivel municipal
      ind     = ind,
      cultivo = cult,
      lbl_ind = lbl
    )
  })
  
  agg_depto <- reactive({
    df <- datos_filtrados(); req(nrow(df) > 0)
    if (is_yield()) {
      df |>
        dplyr::group_by(DEPARTAMENTO_D) |>
        dplyr::summarise(
          prod  = sum(prod_num,  na.rm = TRUE),
          area  = sum(area_cnum,  na.rm = TRUE),
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
    df <- datos_filtrados(); req(nrow(df) > 0)
    if (!is.null(depto_sel())) df <- df |> dplyr::filter(DEPARTAMENTO_D == depto_sel())
    if (is_yield()) {
      df |>
        dplyr::group_by(MUNICIPIO_D) |>
        dplyr::summarise(
          prod  = sum(prod_num,  na.rm = TRUE),
          area  = sum(area_cnum,  na.rm = TRUE),
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
  
  # ========== MAPA Tab 1 ==========
  output$map_eva <- leaflet::renderLeaflet({
    mdat <- depto_sf |>
      dplyr::left_join(agg_depto(), by = "DEPARTAMENTO_D") |>
      dplyr::mutate(
        valor           = ifelse(is.na(valor), 0, valor),
        DEPARTAMENTO_LBL= title_case_es(DEPARTAMENTO_D)
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
        labelOptions = leaflet::labelOptions(
          direction = "auto", textsize = "12px", sticky = TRUE,
          opacity = 0.9, style = list("font-weight" = "600")
        ),
        highlightOptions = leaflet::highlightOptions(
          color = "black", weight = 2, bringToFront = TRUE
        )
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        colors   = pal_info$colors,
        labels   = pal_info$labels,
        title    = indicador_label(),
        opacity  = 1
      )
  })
  
  dibujar_deptos <- function() {
    mdat <- depto_sf |>
      dplyr::left_join(agg_depto(), by = "DEPARTAMENTO_D") |>
      dplyr::mutate(
        valor           = ifelse(is.na(valor), 0, valor),
        DEPARTAMENTO_LBL= title_case_es(DEPARTAMENTO_D)
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
        labelOptions = leaflet::labelOptions(
          direction = "auto", textsize = "12px", sticky = TRUE,
          opacity = 0.9, style = list("font-weight" = "600")
        ),
        highlightOptions = leaflet::highlightOptions(
          color = "black", weight = 2, bringToFront = TRUE
        )
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        colors   = pal_info$colors,
        labels   = pal_info$labels,
        title    = indicador_label(),
        opacity  = 1
      )
  }
  
  # Resaltar municipio seleccionado
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
        labelOptions = leaflet::labelOptions(
          direction = "auto", textsize = "11px", sticky = TRUE,
          opacity = 0.9, style = list("font-weight" = "600")
        ),
        highlightOptions = leaflet::highlightOptions(
          color = "black", weight = 2, bringToFront = TRUE
        )
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        colors   = pal_info$colors,
        labels   = pal_info$labels,
        title    = paste0(indicador_label(), " — ", title_case_es(dep)),
        opacity  = 1
      )
  }
  
  # >>>>>> NUEVO: lógica unificada para el nivel del mapa <<<<<<
  # Si f_depto = "Todos"  → mapa departamental
  # Si f_depto ≠ "Todos"  → mapa municipal del dpto seleccionado
  observe({
    dep  <- input$f_depto
    mp   <- input$f_mpio
    ind  <- input$f_indicador
    anio <- input$f_anio
    cult <- input$f_cultivo
    
    # solo para declarar dependencias
    dummy <- list(mp, ind, anio, cult)
    
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
  # >>>>>> FIN bloque nuevo <<<<<<
  
  observeEvent(input$f_mpio, {
    dep <- depto_sel()
    mp  <- input$f_mpio
    
    if (is.null(dep) && !is.null(input$f_depto) && input$f_depto != "Todos") {
      dep <- input$f_depto
      depto_sel(dep)
      nivel_mapa("mpio")
    }
    
    if (!is.null(dep)) {
      dibujar_mpios(dep)
    }
    
    if (is.null(mp) || mp == "Todos" || is.null(dep)) {
      return()
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
      sprintf("Rendimiento agg.: %s", comma_es(round(total_val, 2)))
    } else {
      base_m |>
        dplyr::group_by(cultivo) |>
        dplyr::summarise(v = sum(valor, na.rm = TRUE), .groups="drop") |>
        dplyr::arrange(dplyr::desc(v)) |>
        dplyr::slice_head(n = 5) |>
        dplyr::mutate(linea = sprintf("%s: %s", cultivo, comma_es(round(v, 2)))) |>
        dplyr::pull(linea) |>
        paste(collapse="<br/>")
    }
    cult_html <- if (length(cult_txt) == 0) "<i>Sin detalles en el filtro</i>" else paste(cult_txt, collapse = "<br/>")
    html <- sprintf(
      "<b>%s</b><br/>%s (total): %s<br/><hr style='margin:6px 0;'>%s",
      title_case_es(mp), indicador_label(), comma_es(round(total_val, 2)), cult_html
    )
    mdat <- mpios_sf |> dplyr::filter(DEPARTAMENTO_D == dep, MUNICIPIO_D == mp)
    if (nrow(mdat) == 1) {
      xy <- xy_from_poly(mdat)
      if (all(is.finite(xy))) {
        leaflet::leafletProxy("map_eva") |>
          leaflet::clearPopups() |>
          leaflet::addPopups(
            lng = xy[1], lat = xy[2], popup = html,
            options = leaflet::popupOptions(closeOnClick = TRUE)
          ) |>
          leaflet::setView(lng = xy[1], lat = xy[2], zoom = 8)
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$map_eva_shape_click, {
    click <- input$map_eva_shape_click
    if (is.null(click$id)) return()
    if (nivel_mapa() == "depto") {
      depto_sel(click$id); nivel_mapa("mpio"); dibujar_mpios(click$id)
    } else {
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
        sprintf("Rendimiento agg.: %s", comma_es(round(total_val, 2)))
      } else {
        base_m |>
          dplyr::group_by(cultivo) |>
          dplyr::summarise(v = sum(valor, na.rm = TRUE), .groups="drop") |>
          dplyr::arrange(dplyr::desc(v)) |>
          dplyr::slice_head(n = 5) |>
          dplyr::mutate(linea = sprintf("%s: %s", cultivo, comma_es(round(v, 2)))) |>
          dplyr::pull(linea) |>
          paste(collapse="<br/>")
      }
      cult_html <- if (length(cult_txt) == 0) "<i>Sin detalles en el filtro</i>" else paste(cult_txt, collapse = "<br/>")
      html <- sprintf(
        "<b>%s</b><br/>%s (total): %s<br/><hr style='margin:6px 0;'>%s",
        title_case_es(muni_id), indicador_label(), comma_es(round(total_val, 2)), cult_html
      )
      mdat <- mpios_sf |> dplyr::filter(DEPARTAMENTO_D == depto_sel(), MUNICIPIO_D == muni_id)
      if (nrow(mdat) == 1) {
        xy <- xy_from_poly(mdat)
        if (all(is.finite(xy))) {
          leaflet::leafletProxy("map_eva") |>
            leaflet::clearPopups() |>
            leaflet::addPopups(
              lng = xy[1], lat = xy[2], popup = html,
              options = leaflet::popupOptions(closeOnClick = TRUE)
            )
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
    if (!is.null(input$f_depto)   && input$f_depto   != "Todos") base <- base |> dplyr::filter(.data$DEPARTAMENTO_D == input$f_depto)
    if (!is.null(input$f_mpio)    && input$f_mpio    != "Todos") base <- base |> dplyr::filter(.data$MUNICIPIO_D    == input$f_mpio)
    if (!is.null(input$f_cultivo) && input$f_cultivo != "Todos") base <- base |> dplyr::filter(.data$cultivo       == input$f_cultivo)
    
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
          area        = sum(area_cnum,  na.rm = TRUE),
          valor_total = dplyr::if_else(area > 0, prod/area, NA_real_),
          .groups     = "drop"
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
    df  <- series_data()
    col <- indic_color()
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
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
  })
  
  output$plot_arriba <- plotly::renderPlotly({
    df_year <- series_data()
    ycol    <- "anio"
    col     <- indic_color()
    plotly::plot_ly(
      data = df_year, x = ~.data[[ycol]], y = ~valor_total,
      type = "scatter", mode = "lines+markers",
      line   = list(color = col, width = 2),
      marker = list(color = col, size = 6),
      hovertemplate = paste0(
        "<b>Año: %{x}</b><br>",
        as.character(indicador_label()),
        ": %{y:.2f}<extra></extra>"
      )
    ) |>
      plotly::layout(
        xaxis = list(
          title = if (year_col() == "ano_sembrado") "Año de sembrado" else "Año de cosechado",
          tickmode = "linear", dtick = 1
        ),
        yaxis = list(title = as.character(indicador_label())),
        hovermode = "x unified",
        margin = list(l = 60, r = 20, t = 40, b = 50)
      ) |>
      plotly::config(locale = "es")
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
      list(
        factor     = 1e6,
        axis_label = paste0(as.character(indicador_label()), " (millones)"),
        unit_short = "millones"
      )
    } else if (mx > 1e5) {
      list(
        factor     = 1e3,
        axis_label = paste0(as.character(indicador_label()), " (miles)"),
        unit_short = "miles"
      )
    } else {
      list(
        factor     = 1,
        axis_label = as.character(indicador_label()),
        unit_short = ""
      )
    }
  })
  
  g_ranking <- reactive({
    plot_df <- ranking_data()
    sc      <- ranking_scale()
    plot_df <- plot_df |>
      dplyr::mutate(
        valor_scaled  = valor_total / sc$factor,
        MUNICIPIO_LBL = title_case_es(MUNICIPIO_D)
      )
    
    ggplot(
      plot_df,
      aes(
        x = valor_scaled,
        y = reorder(MUNICIPIO_LBL, valor_scaled)
      )
    ) +
      geom_col(fill = indic_color()) +
      scale_x_continuous(
        labels = function(x) comma_es(x),
        expand = expansion(mult = c(0, 0.10))
      ) +
      labs(
        x = sc$axis_label,
        y = NULL,
        title = ""
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.y        = element_text(size = 9),
        plot.margin        = margin(r = 30),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(color = "#e6e6e6")
      )
  })
  
  output$ranking_abajo <- plotly::renderPlotly({
    plot_df <- ranking_data()
    sc  <- ranking_scale()
    col <- indic_color()
    
    if (nrow(plot_df) == 0) {
      return(
        plotly::plot_ly() |>
          plotly::layout(
            annotations = list(
              text = "Sin datos para el ranking",
              x = 0.5, y = 0.5, showarrow = FALSE
            )
          ) |>
          plotly::config(locale = "es")
      )
    }
    
    plot_df <- plot_df |>
      dplyr::mutate(
        MUNICIPIO_LBL    = title_case_es(MUNICIPIO_D),
        DEPARTAMENTO_LBL = title_case_es(DEPARTAMENTO_D),
        valor_scaled     = valor_total / sc$factor,
        label_txt        = comma_es(round(valor_total, 0))
      ) |>
      dplyr::arrange(dplyr::desc(valor_scaled))
    
    plot_df$MUNICIPIO_LBL <- factor(
      plot_df$MUNICIPIO_LBL,
      levels = rev(plot_df$MUNICIPIO_LBL)
    )
    
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
      customdata = cbind(
        plot_df$MUNICIPIO_LBL,
        plot_df$DEPARTAMENTO_LBL,
        plot_df$label_txt
      ),
      showlegend = FALSE
    ) |>
      plotly::layout(
        xaxis = list(
          title = sc$axis_label,
          gridcolor = "#e6e6e6"
        ),
        yaxis = list(
          title = "",
          automargin = TRUE
        ),
        margin = list(l = 110, r = 40, t = 20, b = 40)
      ) |>
      plotly::config(locale = "es")
  })
  
  # ========== CLUSTERS (Tab 2) ==========
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
    glue::glue(
      "¿Cómo se agrupan los municipios según sus niveles de {tolower(ind_lab)} y los de sus vecinos?"
    )
  })
  
  output$clus_titulo_tabla <- renderText({
    glue::glue("¿Qué municipios y valores corresponden a cada tipo de clúster?")
  })
  
  datos_cluster <- reactive({
    req(input$clus_depto, input$clus_indicador, input$clus_anio)
    df <- eva_df %>%
      dplyr::filter(DEPARTAMENTO_D == input$clus_depto)
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
      title_case_es(mmun$MUNICIPIO_D),
      as.character(mmun$cluster),
      ind_lab, anio_lab, comma_es(round(as.numeric(mmun$valor), 2)),
      exp_map[as.character(mmun$cluster)],
      accion_map[as.character(mmun$cluster)],
      format(p_thr, nsmall = 2)
    )
    mmun$popup_txt <- as.character(mmun$popup_txt)
    
    pal <- leaflet::colorFactor(
      c("#762A83", "#1B7837", "#C2A5CF", "#A6DBA0", "#D9D9D9"),
      levels = niveles
    )
    
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::addPolygons(
        data = mmun,
        fillColor = ~pal(cluster),
        color = BORDER_COL, weight = 0.4, fillOpacity = 0.85,
        popup = ~as.character(popup_txt),
        highlightOptions = leaflet::highlightOptions(
          color = "black", weight = 2, bringToFront = TRUE
        )
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
        ) %>%
        dplyr::select(MUNICIPIO_D, DEPARTAMENTO_D, valor)
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
        valor          = ifelse(is.na(valor), 0, valor),
        MUNICIPIO_D    = as.character(MUNICIPIO_D),
        DEPARTAMENTO_D = as.character(DEPARTAMENTO_D)
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
      Municipio               = title_case_es(mmun$MUNICIPIO_D),
      Departamento            = title_case_es(mmun$DEPARTAMENTO_D),
      Valor                   = comma_es(round(valor_num, 2)),
      `Participación dpto`    = if (total_depto > 0) paste0(round(100 * valor_num / total_depto, 1), "%") else "0,0%",
      `Ranking departamental` = rank(-valor_num, ties.method = "min"),
      `Acción sugerida`       = unname(accion_map[clus_chr]),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    out <- out[order(out$`Ranking departamental`), ]
    
    DT::datatable(
      out,
      rownames = FALSE,
      options  = list(pageLength = 14, scrollX = TRUE, dom = "tip"),
      escape   = TRUE
    )
  })
  
  # =========================
  # === MAPAS SIMPLE (PNG) ==
  # =========================
  map_widget_simple <- reactive({
    if (nivel_mapa() == "depto") {
      mdat <- depto_sf |>
        dplyr::left_join(agg_depto(), by = "DEPARTAMENTO_D") |>
        dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
      
      pal_info <- build_bins_labels4_indicator(mdat$valor, indic_color())
      pal      <- pal_info$pal
      
      leaflet::leaflet(mdat, options = leaflet::leafletOptions(zoomControl = FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          fillColor = ~pal(valor),
          weight = 0.5, color = BORDER_COL, fillOpacity = 0.9
        ) |>
        leaflet::addControl(
          html = htmltools::HTML(
            sprintf(
              "<div style='font-weight:600;font-size:14px;background:#fff;padding:6px 8px;border-radius:8px;border:1px solid %s'>
                %s por departamento — %s
              </div>",
              BORDER_COL,
              as.character(indicador_label()), safe_chr(input$f_anio)
            )
          ),
          position = "topleft"
        )
    } else {
      dep  <- depto_sel()
      mdat <- mpios_sf |>
        dplyr::filter(DEPARTAMENTO_D == dep) |>
        dplyr::left_join(agg_mpio(), by = "MUNICIPIO_D") |>
        dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
      
      pal_info <- build_bins_labels4_indicator(mdat$valor, indic_color())
      pal      <- pal_info$pal
      
      leaflet::leaflet(mdat, options = leaflet::leafletOptions(zoomControl = FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          fillColor = ~pal(valor),
          weight = 0.4, color = BORDER_COL, fillOpacity = 0.9
        ) |>
        leaflet::addControl(
          html = htmltools::HTML(
            sprintf(
              "<div style='font-weight:600;font-size:14px;background:#fff;padding:6px 8px;border-radius:8px;border:1px solid %s'>
                %s por municipios — %s
              </div>",
              BORDER_COL,
              as.character(indicador_label()), safe_chr(input$f_anio)
            )
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
    pal <- leaflet::colorFactor(
      c("#762A83", "#1B7837", "#C2A5CF", "#A6DBA0", "#D9D9D9"),
      levels = niveles
    )
    
    leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::addPolygons(
        data = mmun,
        fillColor = ~pal(cluster),
        color = BORDER_COL, weight = 0.4, fillOpacity = 0.85
      ) %>%
      leaflet::addControl(
        html = htmltools::HTML(
          sprintf(
            "<div style='font-weight:600;font-size:14px;background:#fff;padding:6px 8px;border-radius:8px;border:1px solid %s'>
              Clusters LISA — %s (%s)
            </div>",
            BORDER_COL,
            as.character(clus_indicador_label()), safe_chr(input$clus_anio)
          )
        ),
        position = "topleft"
      )
  })
  
  # =========================
  # === HHI / DIVERSIFIC. ===
  # =========================
  
  hhi_year_col <- reactive({
    b <- input$hhi_base
    if (is.null(b)) return("ano_cosechado")
    if (b == "area_sembrada_ha") "ano_sembrado" else "ano_cosechado"
  })
  
  # --- Años disponibles dado base + dpto + mpio ---
  hhi_years_available <- reactive({
    req(input$hhi_base)
    ycol <- hhi_year_col()
    
    df <- eva_df
    
    if (!is.null(input$hhi_depto) && input$hhi_depto != "Todos") {
      df <- df %>% dplyr::filter(DEPARTAMENTO_D == input$hhi_depto)
    }
    if (!is.null(input$hhi_mpio) && input$hhi_mpio != "Todos") {
      df <- df %>% dplyr::filter(MUNICIPIO_D == input$hhi_mpio)
    }
    
    sort(unique(na.omit(df[[ycol]])))
  })
  
  # Actualizar años cuando cambian base / dpto / mpio
  observeEvent(
    list(input$hhi_base, input$hhi_depto, input$hhi_mpio),
    {
      yrs <- hhi_years_available()
      if (!length(yrs)) return()
      current  <- isolate(input$hhi_anio)
      selected <- if (!is.null(current) && current %in% yrs) current else max(yrs)
      updateSelectInput(session, "hhi_anio", choices = yrs, selected = selected)
    },
    ignoreInit = TRUE
  )
  
  # Cascada dpto -> mpio SOLO para HHI
  observeEvent(input$hhi_depto, {
    dep <- input$hhi_depto
    if (is.null(dep) || dep == "Todos") {
      munis <- sort(unique(eva_df$MUNICIPIO_D))
    } else {
      munis <- sort(unique(eva_df$MUNICIPIO_D[eva_df$DEPARTAMENTO_D == dep]))
    }
    updateSelectInput(
      session, "hhi_mpio",
      choices  = c("Todos" = "Todos",
                   stats::setNames(munis, title_case_es(munis))),
      selected = "Todos"
    )
  }, ignoreInit = FALSE)
  
  # Títulos fijos (ya en clave de diversificación)
  output$hhi_titulo_serie <- renderText({
    "¿Cómo ha evolucionado la diversificación productiva a lo largo del tiempo?"
  })
  output$hhi_titulo_barras <- renderText({
    "¿Qué territorios presentan mayor diversificación productiva agrícola?"
  })
  output$hhi_titulo_mapa <- renderText({
    "¿Cómo se distribuye geográficamente la diversificación productiva agrícola?"
  })
  
  # === Serie HHI por año (país / dpto / mpio) ===
  #     -> Aquí HHI se calcula "puro"; la transformación a 1-HHI se hace en el plot
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
    
    # Base numérica sin filtrar por año (queremos serie completa)
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
    
    # MISMAS REGLAS QUE EL KPI:
    # 1) Si hay municipio → ámbito = municipio
    # 2) Si no hay municipio pero sí departamento → ámbito = departamento
    # 3) Si no hay nada → ámbito = país completo
    if (!is.null(mp) && mp != "Todos") {
      df <- df %>% dplyr::filter(MUNICIPIO_D == mp)
    } else if (!is.null(dep) && dep != "Todos") {
      df <- df %>% dplyr::filter(DEPARTAMENTO_D == dep)
    }
    
    if (nrow(df) == 0) {
      return(data.frame(anio = numeric(0), HHI = numeric(0), grupo = character(0)))
    }
    
    # HHI por año dentro del ámbito elegido
    out <- df %>%
      dplyr::group_by(anio_calc, cultivo) %>%
      dplyr::summarise(v = sum(base_val, na.rm = TRUE), .groups = "drop") %>%
      dplyr::group_by(anio_calc) %>%
      dplyr::mutate(
        tot = sum(v, na.rm = TRUE),
        s   = dplyr::if_else(tot > 0, v / tot, NA_real_)
      ) %>%
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
  
  # Serie temporal de DIVERSIFICACIÓN = 1 - HHI
  output$hhi_serie <- plotly::renderPlotly({
    sr <- hhi_series_raw()
    if (nrow(sr) == 0) {
      return(
        plotly::plot_ly() %>%
          plotly::layout(
            annotations = list(
              text = "Sin datos para la combinación seleccionada",
              x = 0.5, y = 0.5, showarrow = FALSE
            )
          ) %>%
          plotly::config(locale = "es")
      )
    }
    
    sr <- sr %>%
      dplyr::mutate(
        Diversificacion = pmin(pmax(1 - HHI, 0), 1)
      )
    
    plotly::plot_ly(
      data = sr, x = ~anio, y = ~Diversificacion,
      type = "scatter", mode = "lines+markers",
      line = list(width = 2.5, color = "#3182bd"),
      marker = list(size = 8, color = "#08519c"),
      hovertemplate = "<b>Año %{x}</b><br>Diversificación (1 - HHI): %{y:.3f}<extra></extra>"
    ) %>%
      plotly::layout(
        yaxis = list(
          title = "Diversificación productiva (1 - HHI)",
          range = c(0, 1),
          gridcolor = "#e6e6e6",
          showgrid = TRUE
        ),
        xaxis = list(
          title = "",
          tickmode = "linear",
          dtick = 1,
          gridcolor = "transparent",
          showgrid = FALSE
        ),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        margin = list(l = 60, r = 20, t = 10, b = 40)
      ) %>%
      plotly::config(locale = "es")
  })
  
  # === Base numérica común para HHI (para barras y KPI) ===
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
    
    if (!is.null(input$hhi_depto) && input$hhi_depto != "Todos") {
      df <- df %>% dplyr::filter(DEPARTAMENTO_D == input$hhi_depto)
    }
    if (!is.null(input$hhi_mpio) && input$hhi_mpio != "Todos") {
      df <- df %>% dplyr::filter(MUNICIPIO_D == input$hhi_mpio)
    }
    
    df
  })
  
  # --------- HHI por AÑO (para barras) ----------
  hhi_by_year <- reactive({
    if (is.null(input$hhi_anio)) {
      return(data.frame(grupo = character(0), valor = numeric(0)))
    }
    df <- eva_hhi()
    if (nrow(df) == 0) {
      return(data.frame(grupo = character(0), valor = numeric(0)))
    }
    
    dep <- input$hhi_depto
    mp  <- input$hhi_mpio
    
    df <- df %>% dplyr::filter(anio_calc == input$hhi_anio)
    if (nrow(df) == 0) {
      return(data.frame(grupo = character(0), valor = numeric(0)))
    }
    
    # País: grupo = departamento
    if (is.null(dep) || dep == "Todos") {
      sums <- df %>%
        dplyr::group_by(DEPARTAMENTO_D, cultivo) %>%
        dplyr::summarise(v = sum(base_val, na.rm = TRUE), .groups = "drop")
      
      hhi <- sums %>%
        dplyr::group_by(DEPARTAMENTO_D) %>%
        dplyr::mutate(
          tot = sum(v, na.rm = TRUE),
          s   = dplyr::if_else(tot > 0, v / tot, NA_real_)
        ) %>%
        dplyr::summarise(HHI = sum(s^2, na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(grupo = DEPARTAMENTO_D)
      
      return(hhi %>% dplyr::select(grupo, valor = HHI))
    }
    
    # Departamento: grupo = municipio (mpio individual si mp != Todos)
    if (!is.null(mp) && mp != "Todos") {
      df <- df %>% dplyr::filter(MUNICIPIO_D == mp)
    }
    
    sums <- df %>%
      dplyr::group_by(MUNICIPIO_D, cultivo) %>%
      dplyr::summarise(v = sum(base_val, na.rm = TRUE), .groups = "drop")
    
    if (nrow(sums) == 0) {
      return(data.frame(grupo = character(0), valor = numeric(0)))
    }
    
    hhi <- sums %>%
      dplyr::group_by(MUNICIPIO_D) %>%
      dplyr::mutate(
        tot = sum(v, na.rm = TRUE),
        s   = dplyr::if_else(tot > 0, v / tot, NA_real_)
      ) %>%
      dplyr::summarise(HHI = sum(s^2, na.rm = TRUE), .groups = "drop") %>%
      dplyr::rename(grupo = MUNICIPIO_D)
    
    hhi %>% dplyr::select(grupo, valor = HHI)
  })
  
  # Barras de DIVERSIFICACIÓN = 1 - HHI por año
  output$hhi_barras <- plotly::renderPlotly({
    md <- hhi_by_year()
    if (nrow(md) == 0) {
      return(
        plotly::plot_ly() %>%
          plotly::layout(
            annotations = list(
              text = "Sin datos para la combinación seleccionada",
              x = 0.5, y = 0.5, showarrow = FALSE
            )
          ) %>%
          plotly::config(locale = "es")
      )
    }
    
    md <- md %>%
      dplyr::mutate(
        div_valor   = pmin(pmax(1 - valor, 0), 1),
        grupo_lbl   = title_case_es(grupo)
      ) %>%
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
        xaxis = list(
          title = "Diversificación (1 - HHI)",
          range = c(0, 1),
          separatethousands = TRUE,
          gridcolor = "#e6e6e6",
          showgrid = TRUE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = "",
          automargin = TRUE,
          tickfont = list(size = 11, family = "Inter"),
          showgrid = FALSE,
          zeroline = FALSE,
          showline = FALSE
        ),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        margin = list(
          l = if (is.null(input$hhi_depto) || input$hhi_depto == "Todos") 150 else 180,
          r = 40, t = 20, b = 40
        ),
        uniformtext = list(minsize = 10, mode = "hide")
      ) %>%
      plotly::config(locale = "es")
  })
  
  # ==== KPI de DIVERSIFICACIÓN (Tab 3, debajo de la serie) ====
  # - Municipio seleccionado: HHI del municipio -> se pasa a 1 - HHI.
  # - Departamento seleccionado (mpio = "Todos"): HHI del departamento -> 1 - HHI.
  # - Dpto = "Todos" y mpio = "Todos": HHI nacional -> 1 - HHI.
  hhi_kpi_val <- reactive({
    req(input$hhi_anio, input$hhi_base)
    
    # Base numérica desde eva_df
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
    
    # Filtro por año
    df <- df %>% dplyr::filter(anio_calc == input$hhi_anio)
    if (nrow(df) == 0) return(NA_real_)
    
    dep <- input$hhi_depto
    mp  <- input$hhi_mpio
    
    compute_hhi <- function(df_scope) {
      if (nrow(df_scope) == 0) return(NA_real_)
      sums <- df_scope %>%
        dplyr::group_by(cultivo) %>%
        dplyr::summarise(v = sum(base_val, na.rm = TRUE), .groups = "drop")
      if (nrow(sums) == 0) return(NA_real_)
      tot <- sum(sums$v, na.rm = TRUE)
      if (!is.finite(tot) || tot <= 0) return(NA_real_)
      s <- sums$v / tot
      v <- sum(s^2, na.rm = TRUE)
      if (!is.finite(v)) NA_real_ else v
    }
    
    # 1) Municipio seleccionado → HHI del municipio
    if (!is.null(mp) && mp != "Todos") {
      df_mpio <- df %>% dplyr::filter(MUNICIPIO_D == mp)
      return(compute_hhi(df_mpio))
    }
    
    # 2) Departamento seleccionado, municipio = "Todos" → HHI del departamento
    if (!is.null(dep) && dep != "Todos") {
      df_dep <- df %>% dplyr::filter(DEPARTAMENTO_D == dep)
      return(compute_hhi(df_dep))
    }
    
    # 3) País completo (dep = "Todos" y mpio = "Todos") → HHI nacional
    compute_hhi(df)
  })
  
  output$hhi_kpi_valor <- renderText({
    v_hhi <- hhi_kpi_val()
    if (is.na(v_hhi) || !is.finite(v_hhi)) return("—")
    v_div <- pmin(pmax(1 - v_hhi, 0), 1)
    sprintf("%.3f", v_div)
  })
  
}


# ------------------------------
# 6) Lanzar App
# ------------------------------
shinyApp(ui = ui, server = server)
