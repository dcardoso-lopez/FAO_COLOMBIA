# app_apadt.R — UPRA APADT
# ✅ Dep. por defecto: Atlántico (solo al primer render; luego respeta selección)
# ✅ Zoom consistente (VISUALIZACIÓN + DESCARGA PNG):
#    - Vista Colombia: setView con zoom mayor
#    - Vista Depto: setView con centro+zoom calculado desde bbox (sin fitBoundsOptions)
# ✅ Descarga PNG mapa (con leyenda) + Top10 + CSV + Informe PDF
# ✅ FIX .xts_chob: usar SIEMPRE leaflet::addLegend

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(leaflet); library(sf); library(dplyr); library(tidyr)
  library(scales); library(htmltools); library(plotly)
  library(stringi); library(readr); library(tibble)
  library(shinyjs)
  library(htmlwidgets)
  library(webshot2)
  library(rmarkdown)
})

options(stringsAsFactors = FALSE)
options(scipen = 999)
sf::sf_use_s2(FALSE)

# ---------- Rutas ----------
local_data_dir <- "data"
app_root     <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
rel_data_dir <- file.path(app_root, "data")
data_dir     <- if (dir.exists(rel_data_dir)) rel_data_dir else local_data_dir

ruta_apadt   <- file.path(data_dir, "014_UPRA_APADT.rds")
ruta_pob     <- file.path(data_dir, "051_DANE_Proyecciones_P_total.rds")
ruta_shp_mun <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")
ruta_shp_dep <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")

# ✅ RMD (puede estar en raíz o en data)
ruta_rmd_root <- file.path(app_root, "Informe_descargable.Rmd")
ruta_rmd_data <- file.path(data_dir,  "Informe_descargable.Rmd")
ruta_rmd      <- if (file.exists(ruta_rmd_root)) ruta_rmd_root else ruta_rmd_data

must_exist <- c(ruta_apadt, ruta_pob, ruta_shp_mun, ruta_shp_dep)
miss <- must_exist[!file.exists(must_exist)]
if (length(miss)) stop("Faltan archivos. data_dir usado: ", data_dir, "\n", paste("-", miss, collapse = "\n"))

check_shp_parts <- function(shp){
  b <- sub("\\.shp$", "", shp)
  req <- paste0(b, c(".shp",".dbf",".shx",".prj"))
  req[!file.exists(req)]
}
miss_shp <- c(check_shp_parts(ruta_shp_mun), check_shp_parts(ruta_shp_dep))
if (length(miss_shp)) stop("Faltan componentes de shapefile:\n", paste("-", miss_shp, collapse = "\n"))

# ---------- Export ----------
EXPORT_DIR <- file.path(app_root, "Descargas")
dir.create(EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)

IMG_MAP_AUTO <- file.path(EXPORT_DIR, "APADT_mapa.png")
IMG_TOP_AUTO <- file.path(EXPORT_DIR, "APADT_top10.png")

# ✅ Compatibilidad: si tu Rmd ya esperaba estos nombres (UPRA)
IMG_MAP_COMPAT <- file.path(EXPORT_DIR, "UPRA_mapa.png")
IMG_TOP_COMPAT <- file.path(EXPORT_DIR, "UPRA_top10.png")

PNG_VWIDTH  <- 980
PNG_VHEIGHT <- 720
PNG_DELAY   <- 1.2

save_widget_png <- function(widget, out_png, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay = PNG_DELAY){
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
  
  file.exists(out_png) && file.info(out_png)$size > 0
}

# ---------- Utils ----------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
norm_txt <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
NUP      <- function(x) toupper(norm_txt(x))

safe_chr <- function(x){
  if (is.null(x) || length(x) == 0) return("")
  if (all(is.na(x))) return("")
  as.character(x)[1]
}

num_or_na <- function(x){
  if (is.numeric(x)) return(as.numeric(x))
  x0 <- trimws(as.character(x))
  x0[x0 %in% c("", "NA", "NaN", "Inf", "-Inf")] <- NA_character_
  
  if (any(grepl(",", x0, fixed = TRUE), na.rm = TRUE)) {
    return(suppressWarnings(readr::parse_number(
      x0, locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
    )))
  }
  suppressWarnings(readr::parse_number(
    x0, locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
  ))
}

pick_col <- function(df, primary, pattern){
  nms <- names(df)
  if (primary %in% nms) return(primary)
  alt <- nms[grepl(pattern, nms, ignore.case = TRUE)]
  if (length(alt)) alt[1] else NA_character_
}
safe_pull <- function(df, col) if (!is.na(col) && col %in% names(df)) df[[col]] else NA

title_case_es <- function(x){
  stopw <- c("de","del","la","las","los","y","e","o","u","en","a","al","por","para",
             "con","sin","sobre","entre","hasta","desde","contra","ante","tras",
             "que","el","su","un","una","unos","unas")
  vapply(x, function(s){
    if (is.na(s) || !nzchar(s)) return(s)
    toks <- strsplit(trimws(as.character(s)), "\\s+", perl = TRUE)[[1]]
    toks_out <- mapply(function(tok, i){
      parts <- stringi::stri_split_regex(tok, "([-/])", omit_empty = FALSE, tokens_only = FALSE)[[1]]
      parts_out <- mapply(function(p, j){
        if (p %in% c("-", "/")) return(p)
        base <- tolower(p)
        prev_sep <- if (j>1) parts[j-1] %in% c("-", "/") else FALSE
        if (i==1 || prev_sep || !(base %in% stopw)) stringi::stri_trans_totitle(base, locale="es") else base
      }, parts, seq_along(parts), USE.NAMES=FALSE)
      paste0(parts_out, collapse = "")
    }, toks, seq_along(toks), USE.NAMES=FALSE)
    paste(toks_out, collapse = " ")
  }, character(1))
}

fmt_pct <- function(x, acc = 0.1){
  ifelse(is.na(x), "NA", scales::percent(x, accuracy = acc, decimal.mark = ","))
}
fmt_num <- function(x, digs = 1){
  ifelse(is.na(x), "NA", scales::number(x, accuracy = digs, big.mark = ".", decimal.mark = ","))
}
fmt_int <- function(x){
  ifelse(is.na(x), "NA", scales::number(x, accuracy = 1, big.mark = ".", decimal.mark = ","))
}

compute_breaks_quartiles <- function(values){
  vals <- suppressWarnings(as.numeric(values))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(c(0, 1))
  rng <- range(vals, na.rm = TRUE)
  if (rng[1] == rng[2]) {
    v <- rng[1]; if (!is.finite(v)) v <- 0
    return(c(v - 0.001, v + 0.001))
  }
  qs <- stats::quantile(vals, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  brks <- sort(unique(as.numeric(qs)))
  if (length(brks) < 2) brks <- seq(rng[1], rng[2], length.out = 5)
  brks <- sort(unique(as.numeric(brks)))
  if (length(brks) < 2) brks <- c(rng[1], rng[2])
  brks
}
format_interval_label <- function(a, b, as_percent = FALSE, is_first = TRUE){
  if (as_percent) {
    fa <- scales::percent(a, accuracy = 0.1, decimal.mark = ",")
    fb <- scales::percent(b, accuracy = 0.1, decimal.mark = ",")
  } else {
    fa <- scales::number(a, accuracy = 1, big.mark = ".", decimal.mark = ",")
    fb <- scales::number(b, accuracy = 1, big.mark = ".", decimal.mark = ",")
  }
  if (is_first) sprintf("%s – %s", fa, fb) else sprintf(">%s – %s", fa, fb)
}
build_interval_labels <- function(breaks, as_percent = FALSE){
  if (length(breaks) < 2) return(character(0))
  vapply(
    seq_len(length(breaks) - 1),
    function(i){
      format_interval_label(breaks[i], breaks[i + 1], as_percent = as_percent, is_first = (i == 1))
    },
    character(1)
  )
}

# ---------- Colores ----------
PALETA_AZUL <- c("#e0f3fa","#99d5ec","#4bb5e1","#005b88")
COLOR_RANK  <- "#009edb"
COLOR_BORDE <- "#99d5ec"

BTN_BG <- "#ffffff"
BTN_BD <- "#2563eb"
BTN_TX <- "#6b7280"

# ---------- ZOOM: constantes + helper bbox->zoom ----------
CO_LNG  <- -74.3
CO_LAT  <-  5.0
CO_ZOOM <-  6.6     # ✅ MÁS zoom Colombia (antes 6.1)

# ✅ Zoom extra para deptos desde bbox (sin fitBoundsOptions)
zoom_from_bbox <- function(bb){
  dx <- as.numeric(bb["xmax"] - bb["xmin"])
  dy <- as.numeric(bb["ymax"] - bb["ymin"])
  d  <- max(dx, dy, na.rm = TRUE)
  
  if (!is.finite(d)) return(7.5)
  if (d > 10)  return(6.2)
  if (d >  6)  return(6.8)
  if (d >  3)  return(7.4)
  if (d >  1.8) return(8.1)
  if (d >  1.0) return(8.9)
  if (d >  0.6) return(9.6)
  return(10.2)
}

bbox_center <- function(bb){
  cx <- (as.numeric(bb["xmin"]) + as.numeric(bb["xmax"])) / 2
  cy <- (as.numeric(bb["ymin"]) + as.numeric(bb["ymax"])) / 2
  c(lng = cx, lat = cy)
}

# ---------- Cargar APADT + población ----------
apadt_raw <- readRDS(ruta_apadt)
pob_raw   <- readRDS(ruta_pob)

col_ano       <- pick_col(apadt_raw, "ano", "^a(n|ñ)o$|year")
col_mes       <- pick_col(apadt_raw, "mes", "mes")
col_dep_cod   <- pick_col(apadt_raw, "COD_DANE_DPTO_D", "DPTO|DEPTO|DANE.*DEP|COD.*DEP|DEPART")
col_dep_nom   <- pick_col(apadt_raw, "DEPARTAMENTO_D", "DEPARTA")
col_mun_cod   <- pick_col(apadt_raw, "COD_DANE_MUNIC_D", "MUNI.*COD|COD.*MUNI|DANE.*MUNI|COD_MUN5|MPIO")
col_mun_nom   <- pick_col(apadt_raw, "MUNICIPIO_D", "MUNICIP")
col_area_mpio <- pick_col(apadt_raw, "area_mpio_ha", "area.*mpio.*ha|mpio.*ha|area.*municip.*ha")
col_area_apadt<- pick_col(apadt_raw, "area_APADT_ha", "area.*(apadt|apt|apto).*ha|apadt_ha|ha_apadt|aprovech.*ha|area_APADT_ha")
col_prop_apadt<- pick_col(apadt_raw, "prop_APADT", "prop.*(apadt|apt|apto)|porc.*(apadt|apt)|prop_APADT")

if (is.na(col_area_apadt) && is.na(col_prop_apadt)) {
  stop("No se encontraron columnas de indicador (área/proporción) en 014_UPRA_APADT.rds")
}

DEPARTAMENTO_RAW <- safe_pull(apadt_raw, col_dep_nom)
MUNICIPIO_RAW    <- safe_pull(apadt_raw, col_mun_nom)

apadt <- tibble(
  ano             = suppressWarnings(as.integer(safe_pull(apadt_raw, col_ano))),
  mes             = suppressWarnings(as.integer(safe_pull(apadt_raw, col_mes))),
  COD_DANE_DPTO   = sprintf("%02s", gsub("\\D","", safe_pull(apadt_raw, col_dep_cod))),
  DEPARTAMENTO    = norm_txt(DEPARTAMENTO_RAW),
  DEPARTAMENTO_TC = title_case_es(DEPARTAMENTO_RAW),
  COD_DANE_MUNI   = sprintf("%05s", gsub("\\D","", safe_pull(apadt_raw, col_mun_cod))),
  MUNICIPIO_NORM  = norm_txt(MUNICIPIO_RAW),
  MUNICIPIO_TC    = title_case_es(MUNICIPIO_RAW),
  area_mpio_ha    = num_or_na(safe_pull(apadt_raw, col_area_mpio)),
  area_apadt_ha   = num_or_na(safe_pull(apadt_raw, col_area_apadt)),
  prop_apadt      = num_or_na(safe_pull(apadt_raw, col_prop_apadt))
)

mxp <- suppressWarnings(max(apadt$prop_apadt, na.rm = TRUE))
if (is.finite(mxp) && mxp > 1.5) apadt$prop_apadt <- apadt$prop_apadt / 100

# Población
col_ano_p   <- pick_col(pob_raw, "ano", "^a(n|ñ)o$|year")
col_mun_p   <- pick_col(pob_raw, "COD_DANE_MUNIC_D", "MUNI.*COD|COD.*MUNI|DANE.*MUNI|MPIO|CODMUN|COD_MUN5")
col_pob_tot <- pick_col(pob_raw, "poblacion", "poblaci(o|ó)n|total|p_total|POB|pob$")

pob <- tibble(
  ano           = suppressWarnings(as.integer(safe_pull(pob_raw, col_ano_p))),
  COD_DANE_MUNI = sprintf("%05s", gsub("\\D", "", safe_pull(pob_raw, col_mun_p))),
  pob_total     = num_or_na(safe_pull(pob_raw, col_pob_tot))
) %>%
  group_by(ano, COD_DANE_MUNI) %>%
  summarise(pob_total = sum(pob_total, na.rm=TRUE), .groups="drop")

base_apadt <- apadt %>%
  left_join(pob, by = c("ano","COD_DANE_MUNI")) %>%
  mutate(
    prop_apadt = dplyr::coalesce(prop_apadt, if_else(area_mpio_ha > 0, area_apadt_ha/area_mpio_ha, NA_real_))
  )

# ---------- Shapes ----------
mun_raw <- sf::st_read(ruta_shp_mun, quiet = TRUE)
dep_raw <- sf::st_read(ruta_shp_dep, quiet = TRUE)

mun_sf <- mun_raw %>%
  mutate(
    COD_MUN5 = if ("MPIO_CDPMP" %in% names(.)) sprintf("%05d", as.integer(MPIO_CDPMP))
    else if ("COD_MPIO" %in% names(.)) sprintf("%05d", as.integer(COD_MPIO))
    else stop("Shp municipios: falta MPIO_CDPMP/COD_MPIO"),
    COD_DPTO2   = substr(COD_MUN5, 1, 2),
    MUNICIPIO_N = if ("MPIO_CNMBR" %in% names(.)) as.character(MPIO_CNMBR)
    else if ("NOMBRE_MPIO" %in% names(.)) as.character(NOMBRE_MPIO)
    else "MUNICIPIO"
  ) %>% st_transform(4326) %>% st_make_valid()

dep_sf <- dep_raw %>%
  mutate(
    COD_DPTO2 = if ("DPTO_CCDGO" %in% names(.)) sprintf("%02d", as.integer(DPTO_CCDGO))
    else if ("COD_DEPTO" %in% names(.)) sprintf("%02d", as.integer(COD_DEPTO))
    else stop("Shp deptos: falta DPTO_CCDGO/COD_DEPTO"),
    DEPARTAMENTO_N = if ("DEPARTAMENTO_D" %in% names(.)) as.character(DEPARTAMENTO_D)
    else if ("DPTO_CNMBR" %in% names(.)) as.character(DPTO_CNMBR)
    else if ("NOMBRE_DEPTO" %in% names(.)) as.character(NOMBRE_DEPTO)
    else COD_DPTO2,
    DEPARTAMENTO_TC = title_case_es(DEPARTAMENTO_N)
  ) %>% st_transform(4326) %>% st_make_valid()

dep_lookup <- base_apadt %>%
  transmute(
    COD_DANE_DPTO, DEPARTAMENTO, DEPARTAMENTO_TC,
    COD_DPTO2 = sprintf("%02d", as.integer(COD_DANE_DPTO)),
    DEP_NORM  = NUP(DEPARTAMENTO)
  ) %>% distinct()

dep_sf$DEP_NORM_SHP <- NUP(dep_sf$DEPARTAMENTO_N)

# ---------- UI ----------
ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = bs_theme(
    version = 5, primary = BTN_BD,
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius"   = "0.9rem",
    "font-size-base"  = "0.98rem"
  ),
  tags$head(
    tags$style(HTML(paste0("
      :root{
        --accent-border:#99d5ec;
        --gap:12px;
        --viz-row-top:380px;
        --viz-row-bottom:300px;
        --kpi-h: 110px;
        --btn-bg:", BTN_BG, ";
        --btn-bd:", BTN_BD, ";
        --btn-tx:", BTN_TX, ";
      }
      .wrap{ max-width:1360px; margin:0 auto; padding:16px 20px 32px; }
      .filters, .card{
        background:#fff;
        border:1px solid var(--accent-border) !important;
        border-radius:16px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
        padding:12px;
        margin-bottom:12px;
      }
      .filters{padding:6px 12px 8px;}
      .filters-grid{
        display:grid;
        grid-template-columns:repeat(6,minmax(180px,1fr));
        gap:12px;
        align-items:stretch;
      }
      .filter{ display:flex; flex-direction:column; }
      .filter-label{
        font-size:14px; font-weight:500; color:#111827;
        margin-bottom:6px; min-height:36px;
        display:flex; align-items:flex-end;
      }
      .filters-grid .shiny-input-container{ margin:0 !important; }
      .filters-grid .selectize-input,
      .filters-grid .form-control,
      .filters-grid .form-select{
        height:60px !important; min-height:60px;
        padding-top:10px; padding-bottom:10px;
        border-radius:10px;
        border:1px solid var(--accent-border) !important;
      }
      .card-title{ font-weight:700; font-size:16px; margin-bottom:8px; color:#111827; }
      .content-grid{
        display:grid;
        grid-template-columns: 1.05fr 1fr;
        grid-template-rows: var(--viz-row-top) var(--viz-row-bottom);
        gap: var(--gap);
      }
      .viz-card{ display:flex; flex-direction:column; height:100%; margin:0; }
      .viz-body{ flex:1 1 auto; min-height:0; }
      .viz-map{ grid-row: span 2; }
      .viz-body .leaflet,
      .viz-body .plotly.html-widget{ height:100% !important; }

      .leaflet-control, .leaflet-control .legend, .leaflet-control .info{
        border:1px solid var(--accent-border) !important;
        border-radius:12px;
      }
      .leaflet-control .legend{
        background: rgba(255,255,255,0.92) !important;
        padding: 8px 10px !important;
        border-radius: 12px !important;
      }

      .kpi-grid{
        display:grid;
        grid-template-columns: repeat(2, 1fr);
        grid-auto-rows: var(--kpi-h);
        gap: var(--gap);
        height: 100%;
      }
      .kpi{
        border:1px solid var(--accent-border);
        border-radius:14px;
        padding:12px 14px;
        display:flex;
        flex-direction:column;
        justify-content:center;
        background:#fff;
      }
      .kpi .kpi-title{ font-size:12px; color:#6b7280; margin-bottom:6px; font-weight:600; }
      .kpi .kpi-value{ font-size:22px; font-weight:800; color:#0d3b66; line-height:1; }
      .kpi .kpi-sub{ font-size:12px; color:#4b5563; margin-top:2px; }
      .kpi.muted{ opacity:.25 }

      .btn-unified{
        background: #ffffff !important;
        border: 1px solid var(--btn-bd) !important;
        color: var(--btn-tx) !important;
        font-weight: 700 !important;
        border-radius: 12px !important;
        padding: 8px 12px !important;
      }
      .btn-unified:hover,
      .btn-unified:focus,
      .btn-unified:active{
        background: #ffffff !important;
        border-color: var(--btn-bd) !important;
        color: var(--btn-tx) !important;
        box-shadow: 0 0 0 .2rem rgba(37,99,235,.15) !important;
      }
      .footer-actions{
        margin-top: 14px;
        display:flex;
        justify-content:flex-end;
        gap: 10px;
        padding: 10px 6px 0;
        flex-wrap: wrap;
      }
    ")))
  ),
  div(class="wrap",
      div(class="filters",
          div(class="filters-grid",
              div(class="filter",
                  div(class="filter-label","¿Qué año analizamos?"),
                  uiOutput("anio_ui")
              ),
              div(class="filter",
                  div(class="filter-label","¿En qué departamento?"),
                  selectInput("f_dep", NULL, choices = "Todos", selected = "ATLÁNTICO")
              ),
              div(class="filter",
                  div(class="filter-label","¿Algún municipio en particular?"),
                  selectInput("f_mun", NULL, choices = "Todos", selected = "Todos")
              ),
              div(class="filter",
                  div(class="filter-label","¿Cuál indicador?"),
                  selectInput(
                    "f_ind", NULL,
                    choices = c(
                      "Área Potencial"                 = "area_apadt_ha",
                      "Porcentaje del Área Potencial"  = "prop_apadt"
                    ),
                    selected = "area_apadt_ha"
                  )
              ),
              div(class="filter",
                  div(class="filter-label", id="lbl_prop", "Filtro por porcentaje"),
                  uiOutput("ui_slider_prop")
              ),
              div(class="filter",
                  div(class="filter-label","Acción"),
                  tagList(
                    actionLink("btn_reset","← Limpiar filtros"),
                    br(),
                    actionLink("btn_back_co","⤺ Volver a Colombia")
                  )
              )
          )
      ),
      
      div(class="content-grid",
          div(class="card viz-card viz-map",
              div(class="card-title d-flex justify-content-between align-items-center",
                  span(textOutput("map_title")),
                  downloadButton("dl_map_png", "Descargar PNG", class = "btn-unified")
              ),
              div(class="viz-body",
                  leafletOutput("map_apadt", height = "100%")
              )
          ),
          div(class="card viz-card",
              div(class="card-title d-flex justify-content-between align-items-center",
                  span(textOutput("title_top")),
                  downloadButton("dl_bar_png", "Descargar PNG", class = "btn-unified")
              ),
              div(class="viz-body",
                  plotlyOutput("bar_top", height = "100%")
              )
          ),
          div(class="card viz-card",
              div(class="card-title","Información del Área Potencial de Adecuación de Tierras"),
              div(class="viz-body", uiOutput("kpi_boxes"))
          )
      ),
      
      div(class="footer-actions",
          downloadButton("dl_data_csv", "Descargar CSV", class = "btn-unified"),
          downloadButton("dl_report_pdf", "Descargar Informe (PDF)", class = "btn-unified")
      )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session){
  
  first_run <- reactiveVal(TRUE)
  
  output$anio_ui <- renderUI({
    yrs <- sort(unique(na.omit(base_apadt$ano)))
    selectInput("anio", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  output$ui_slider_prop <- renderUI({
    if (!is.null(input$f_ind) && input$f_ind == "area_apadt_ha") return(NULL)
    shinyWidgets::sliderTextInput(
      inputId = "f_prop", label = NULL,
      choices = c("0%", "50%", "100%"),
      selected = c("0%", "100%"),
      grid = TRUE, dragRange = TRUE
    )
  })
  
  observeEvent(input$f_ind, {
    if (identical(input$f_ind, "area_apadt_ha")) shinyjs::hide(id = "lbl_prop") else shinyjs::show(id = "lbl_prop")
  }, ignoreInit = TRUE)
  
  prop_range <- reactive({
    if (!is.null(input$f_ind) && input$f_ind == "area_apadt_ha") return(c(0,1))
    req(input$f_prop)
    v <- as.numeric(gsub("%","", input$f_prop))
    c(v[1], v[2]) / 100
  })
  
  safe_choices <- function(x) { x <- unique(as.character(x)); x[!is.na(x) & nzchar(x)] }
  update_select_clean <- function(inputId, choices, selected = NULL) {
    ch <- safe_choices(choices); if (length(ch) == 0) ch <- character(0)
    ch_full <- c("Todos", ch)
    sel <- if (!is.null(selected) && selected %in% ch_full) selected else "Todos"
    updateSelectInput(session, inputId, choices = ch_full, selected = sel)
  }
  
  # ✅ Dep. por defecto: Atlántico (solo primer render)
  observeEvent(input$anio, {
    df <- base_apadt %>% filter(ano == input$anio)
    deps_tc <- df %>% pull(DEPARTAMENTO_TC) %>% safe_choices() %>% sort()
    
    if (first_run()) {
      if ("Atlántico" %in% deps_tc) sel_dep <- "Atlántico"
      else if ("Atlantico" %in% deps_tc) sel_dep <- "Atlantico"
      else if (length(deps_tc) > 0) sel_dep <- deps_tc[1]
      else sel_dep <- "Todos"
      first_run(FALSE)
    } else {
      if (!is.null(input$f_dep) && input$f_dep %in% c("Todos", deps_tc)) sel_dep <- input$f_dep
      else if (length(deps_tc) > 0) sel_dep <- deps_tc[1]
      else sel_dep <- "Todos"
    }
    
    update_select_clean("f_dep", deps_tc, selected = sel_dep)
    update_select_clean("f_mun", character(0), selected = "Todos")
  }, ignoreInit = FALSE)
  
  observeEvent(input$f_dep, {
    df <- base_apadt %>% filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") {
      mpios_tc <- df %>% filter(DEPARTAMENTO_TC == input$f_dep) %>%
        pull(MUNICIPIO_TC) %>% safe_choices() %>% sort()
      update_select_clean("f_mun", mpios_tc, selected = "Todos")
    } else {
      update_select_clean("f_mun", character(0), selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_reset, {
    yrs <- sort(unique(na.omit(base_apadt$ano)))
    y0  <- max(yrs, na.rm = TRUE)
    updateSelectInput(session, "anio", selected = y0)
    
    df <- base_apadt %>% filter(ano == y0)
    deps_tc <- df %>% pull(DEPARTAMENTO_TC) %>% safe_choices() %>% sort()
    sel_dep <- if ("Atlántico" %in% deps_tc) "Atlántico" else if ("Atlantico" %in% deps_tc) "Atlantico" else if (length(deps_tc)) deps_tc[1] else "Todos"
    
    update_select_clean("f_dep", deps_tc, selected = sel_dep)
    update_select_clean("f_mun", character(0), selected = "Todos")
    updateSelectInput(session, "f_ind", selected = "area_apadt_ha")
    if (!is.null(input$f_prop)) shinyWidgets::updateSliderTextInput(session, "f_prop", selected = c("0%","100%"))
  })
  
  observeEvent(input$btn_back_co, { updateSelectInput(session, "f_dep", selected = "Todos") })
  
  # ✅ Base filtrada: filtro % solo si indicador es prop_apadt
  base_filtrada <- reactive({
    req(input$anio)
    d <- base_apadt %>% filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") d <- d %>% filter(DEPARTAMENTO_TC == input$f_dep)
    if (!is.null(input$f_mun) && input$f_mun != "Todos") d <- d %>% filter(MUNICIPIO_TC == input$f_mun)
    
    if (is.null(input$f_ind) || input$f_ind == "area_apadt_ha") return(d)
    
    rng <- prop_range()
    d %>% filter(!is.na(prop_apadt), prop_apadt >= rng[1], prop_apadt <= rng[2])
  })
  
  # -------- CSV --------
  output$dl_data_csv <- downloadHandler(
    filename = function(){
      dep_tag <- if (is.null(input$f_dep) || input$f_dep=="Todos") "Colombia" else gsub("\\s+","_", input$f_dep)
      mun_tag <- if (is.null(input$f_mun) || input$f_mun=="Todos") "Todos" else gsub("\\s+","_", input$f_mun)
      paste0("base_apadt_filtrada_", dep_tag, "_", mun_tag, "_", input$anio, ".csv")
    },
    content = function(file){
      d <- base_filtrada()
      if (is.null(d)) d <- tibble()
      d_out <- d %>%
        select(
          ano, mes,
          COD_DANE_DPTO, DEPARTAMENTO, DEPARTAMENTO_TC,
          COD_DANE_MUNI, MUNICIPIO_NORM, MUNICIPIO_TC,
          area_mpio_ha, area_apadt_ha, prop_apadt,
          pob_total
        ) %>%
        arrange(DEPARTAMENTO_TC, MUNICIPIO_TC)
      readr::write_csv2(d_out, file)
    }
  )
  
  # -------- KPIs --------
  kpi_dep <- reactive({
    req(input$anio)
    d <- base_apadt %>% filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") d <- d %>% filter(DEPARTAMENTO_TC == input$f_dep)
    tibble(
      area_apadt = sum(d$area_apadt_ha, na.rm = TRUE),
      area_tot   = sum(d$area_mpio_ha,   na.rm = TRUE),
      prop       = ifelse(area_tot > 0, area_apadt/area_tot, NA_real_)
    )
  })
  
  kpi_mun <- reactive({
    req(input$anio)
    if (is.null(input$f_dep) || input$f_dep == "Todos" ||
        is.null(input$f_mun) || input$f_mun == "Todos") return(NULL)
    d <- base_apadt %>% filter(ano == input$anio,
                               DEPARTAMENTO_TC == input$f_dep,
                               MUNICIPIO_TC   == input$f_mun)
    tibble(
      area_apadt = sum(d$area_apadt_ha, na.rm = TRUE),
      area_tot   = sum(d$area_mpio_ha,   na.rm = TRUE),
      prop       = ifelse(area_tot > 0, area_apadt/area_tot, NA_real_)
    )
  })
  
  kpi_card <- function(title, value, sub = "", muted = FALSE){
    cls <- if (muted) "kpi muted" else "kpi"
    htmltools::div(class = cls,
                   div(class="kpi-title", title),
                   div(class="kpi-value", value),
                   if (nzchar(sub)) div(class="kpi-sub", sub))
  }
  
  output$kpi_boxes <- renderUI({
    dep <- kpi_dep()
    mun <- kpi_mun()
    
    dep_pct <- fmt_pct(dep$prop, acc = 0.1)
    dep_ha  <- fmt_num(dep$area_apadt, digs = 1)
    dep_sub <- paste0("del territorio (", fmt_num(dep$area_tot, digs = 1), " ha)")
    
    if (is.null(mun)) {
      htmltools::div(class="kpi-grid",
                     kpi_card("Área departamental APADT / Área departamental — % del Área", dep_pct, dep_sub),
                     kpi_card("Área departamental — hectáreas", dep_ha, "del Área Potencial de Adecuación de Tierras"),
                     kpi_card("A nivel municipal — % del Área Potencial", "—", "selecciona un municipio", muted = TRUE),
                     kpi_card("A nivel municipal — Área Potencial (ha)", "—", "selecciona un municipio", muted = TRUE)
      )
    } else {
      mun_pct <- fmt_pct(mun$prop, acc = 0.1)
      mun_ha  <- fmt_num(mun$area_apadt, digs = 1)
      mun_sub <- paste0("del municipio (", fmt_num(mun$area_tot, digs = 1), " ha)")
      htmltools::div(class="kpi-grid",
                     kpi_card("Área departamental APADT / Área departamental — % del Área", dep_pct, dep_sub),
                     kpi_card("Área departamental — hectáreas", dep_ha, "del Área Potencial de Adecuación de Tierras"),
                     kpi_card("A nivel municipal — % del Área Potencial", mun_pct, mun_sub),
                     kpi_card("A nivel municipal — hectáreas", mun_ha, mun_sub)
      )
    }
  })
  
  output$map_title <- renderText({
    ind <- input$f_ind
    dep <- input$f_dep %||% "Todos"
    dep_todos <- is.null(dep) || dep == "Todos"
    if (ind == "prop_apadt") {
      if (dep_todos) "¿Qué departamentos tienen mayor porcentaje del Área Potencial de Adecuación de Tierras?"
      else           "¿Qué municipios tienen mayor porcentaje del Área Potencial de Adecuación de Tierras?"
    } else {
      if (dep_todos) "¿Qué departamentos concentran más hectáreas del Área Potencial de Adecuación de Tierras?"
      else           "¿Qué municipios concentran más hectáreas del Área Potencial de Adecuación de Tierras?"
    }
  })
  
  output$title_top <- renderText({
    if (input$f_ind == "prop_apadt")
      "Top 10 municipios con mayor porcentaje del Área Potencial de Adecuación de Tierras"
    else
      "Top 10 municipios con más hectáreas del Área Potencial de Adecuación de Tierras"
  })
  
  # ---------- Mapa base (más zoom desde el arranque) ----------
  output$map_apadt <- renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 4, maxZoom = 12, zoomSnap = 0.25)) %>%
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.Positron,
        options = leaflet::providerTileOptions(crossOrigin = TRUE)
      ) %>%
      leaflet::setView(CO_LNG, CO_LAT, CO_ZOOM) %>%
      htmlwidgets::onRender("function(el,x){ this.zoomControl.setPosition('topright'); }")
  })
  
  # ---------- Helpers ----------
  get_cod_from_dep_name <- function(dep_name){
    if (is.null(dep_name) || dep_name == "Todos") return(NA_character_)
    dep_norm <- NUP(dep_name)
    i <- which(dep_sf$DEP_NORM_SHP == dep_norm)[1]
    if (is.finite(i)) return(dep_sf$COD_DPTO2[i])
    j <- which(dep_lookup$DEP_NORM == dep_norm)[1]
    if (is.finite(j)) return(dep_lookup$COD_DPTO2[j])
    NA_character_
  }
  
  # ---------- Widget Leaflet para PNG (incluye leyenda + zoom consistente) ----------
  map_widget <- reactive({
    d <- base_filtrada()
    ind <- input$f_ind %||% "area_apadt_ha"
    label_txt_global <- if (ind == "prop_apadt") "Porcentaje" else "Hectáreas"
    
    tiles <- function(mp){
      mp %>% leaflet::addProviderTiles(
        leaflet::providers$CartoDB.Positron,
        options = leaflet::providerTileOptions(crossOrigin = TRUE)
      )
    }
    
    base_map <- tiles(leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 4, maxZoom = 12, zoomSnap = 0.25)))
    
    if (is.null(d) || nrow(d) == 0) {
      return(base_map %>% leaflet::setView(CO_LNG, CO_LAT, CO_ZOOM))
    }
    
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      
      dd <- d %>%
        mutate(COD_DPTO2 = sprintf("%02d", as.integer(substr(COD_DANE_MUNI, 1, 2)))) %>%
        group_by(COD_DPTO2) %>%
        summarise(
          valor = if (ind == "area_apadt_ha") sum(area_apadt_ha, na.rm = TRUE)
          else sum(area_apadt_ha, na.rm = TRUE) / sum(area_mpio_ha, na.rm = TRUE),
          .groups = "drop"
        )
      
      shp <- dep_sf %>%
        left_join(dd, by = "COD_DPTO2") %>%
        mutate(
          nombre = coalesce(DEPARTAMENTO_TC, title_case_es(DEPARTAMENTO_N)),
          valor  = as.numeric(valor)
        )
      
      brks <- compute_breaks_quartiles(shp$valor)
      pal  <- leaflet::colorBin(PALETA_AZUL, domain = shp$valor, bins = brks, na.color = "#f0f0f0")
      
      labels_legend <- build_interval_labels(brks, as_percent = (ind == "prop_apadt"))
      mids  <- (brks[-length(brks)] + brks[-1]) / 2
      cols_legend <- pal(mids)
      
      base_map %>%
        leaflet::setView(CO_LNG, CO_LAT, CO_ZOOM) %>%   # ✅ MÁS ZOOM Colombia (PNG)
        leaflet::addPolygons(
          data = shp,
          layerId = ~COD_DPTO2,
          fillColor = ~pal(valor),
          color = COLOR_BORDE, weight = 0.7, fillOpacity = 0.9,
          label = ~lapply(
            paste0(
              "<b>", nombre, "</b><br>", label_txt_global, ": ",
              ifelse(is.na(valor), "NA",
                     if (ind == "prop_apadt") fmt_pct(valor, acc = 0.1) else fmt_int(valor))
            ),
            htmltools::HTML
          ),
          labelOptions = leaflet::labelOptions(sticky = FALSE, textsize = "12px", direction = "auto"),
          highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          colors   = cols_legend,
          labels   = labels_legend,
          opacity  = 0.9,
          title    = if (ind == "prop_apadt") "Porcentaje" else "Hectáreas"
        )
      
    } else {
      
      sel_cod <- get_cod_from_dep_name(input$f_dep)
      if (is.na(sel_cod) || !nzchar(sel_cod)) {
        sel_cod <- d$COD_DANE_DPTO %>% unique() %>% sprintf("%02d", as.integer(.)) %>% .[1]
      }
      
      dd <- d %>%
        mutate(COD_MUN5 = sprintf("%05d", as.integer(COD_DANE_MUNI))) %>%
        group_by(COD_DANE_MUNI, MUNICIPIO_TC, DEPARTAMENTO_TC) %>%
        summarise(
          valor = if (ind == "area_apadt_ha") sum(area_apadt_ha, na.rm = TRUE)
          else sum(area_apadt_ha, na.rm = TRUE) / sum(area_mpio_ha, na.rm = TRUE),
          area_apadt_ha = sum(area_apadt_ha, na.rm = TRUE),
          prop_apadt    = sum(area_apadt_ha, na.rm = TRUE) / sum(area_mpio_ha, na.rm = TRUE),
          pob_total     = mean(pob_total, na.rm = TRUE),
          .groups = "drop"
        )
      
      shp <- mun_sf %>% filter(COD_DPTO2 == sel_cod)
      idx <- match(shp$COD_MUN5, sprintf("%05d", as.integer(dd$COD_DANE_MUNI)))
      
      shp$valor         <- dd$valor[idx]
      shp$MUNICIPIO_TC  <- dd$MUNICIPIO_TC[idx]
      shp$area_apadt_ha <- dd$area_apadt_ha[idx]
      shp$prop_apadt    <- dd$prop_apadt[idx]
      shp$pob_total     <- dd$pob_total[idx]
      shp$DEP_TC        <- dd$DEPARTAMENTO_TC[idx]
      
      shp <- shp %>% mutate(
        MUNICIPIO_MOSTRAR = coalesce(MUNICIPIO_TC, title_case_es(MUNICIPIO_N)),
        DEP_MOSTRAR       = coalesce(DEP_TC, "")
      )
      
      brks <- compute_breaks_quartiles(shp$valor)
      pal  <- leaflet::colorBin(PALETA_AZUL, domain = shp$valor, bins = brks, na.color = "#f0f0f0")
      
      labels_legend <- build_interval_labels(brks, as_percent = (ind == "prop_apadt"))
      mids  <- (brks[-length(brks)] + brks[-1]) / 2
      cols_legend <- pal(mids)
      
      bb <- sf::st_bbox(shp)
      ctr <- bbox_center(bb)
      z   <- zoom_from_bbox(bb)
      
      base_map %>%
        leaflet::setView(lng = ctr["lng"], lat = ctr["lat"], zoom = z) %>%  # ✅ MÁS ZOOM depto (PNG)
        leaflet::addPolygons(
          data = shp,
          layerId = ~COD_MUN5,
          fillColor = ~pal(valor),
          color = COLOR_BORDE, weight = 0.4, fillOpacity = 0.9,
          label = ~lapply(
            paste0(
              "<b>", MUNICIPIO_MOSTRAR, " (", DEP_MOSTRAR, ")</b><br>",
              label_txt_global, ": ",
              ifelse(is.na(valor), "NA",
                     if (ind == "prop_apadt") fmt_pct(valor, acc = 0.1) else fmt_int(valor)),
              "<br>Área (ha): ", fmt_int(area_apadt_ha),
              "<br>Porcentaje: ", fmt_pct(prop_apadt, acc = 0.1),
              "<br>Población: ", fmt_int(pob_total)
            ),
            htmltools::HTML
          ),
          labelOptions = leaflet::labelOptions(sticky = FALSE, textsize = "12px", direction = "auto"),
          highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          colors   = cols_legend,
          labels   = labels_legend,
          opacity  = 0.9,
          title    = if (ind == "prop_apadt") "Porcentaje" else "Hectáreas"
        )
    }
  })
  
  # ---------- Actualización interactiva del mapa (leafletProxy + zoom consistente) ----------
  observe({
    d <- base_filtrada()
    
    if (is.null(d) || nrow(d) == 0) {
      leaflet::leafletProxy("map_apadt") %>%
        leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::setView(CO_LNG, CO_LAT, CO_ZOOM)
      return(invisible(NULL))
    }
    
    ind <- input$f_ind
    label_txt_global <- if (ind == "prop_apadt") "Porcentaje" else "Hectáreas"
    
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      
      dd <- d %>%
        mutate(COD_DPTO2 = sprintf("%02d", as.integer(substr(COD_DANE_MUNI, 1, 2)))) %>%
        group_by(COD_DPTO2) %>%
        summarise(
          valor = if (ind == "area_apadt_ha") sum(area_apadt_ha, na.rm = TRUE)
          else sum(area_apadt_ha, na.rm = TRUE) / sum(area_mpio_ha, na.rm = TRUE),
          .groups = "drop"
        )
      
      shp <- dep_sf %>%
        left_join(dd, by = "COD_DPTO2") %>%
        mutate(
          nombre = coalesce(DEPARTAMENTO_TC, title_case_es(DEPARTAMENTO_N)),
          valor  = as.numeric(valor)
        )
      
      brks <- compute_breaks_quartiles(shp$valor)
      pal  <- leaflet::colorBin(PALETA_AZUL, domain = shp$valor, bins = brks, na.color = "#f0f0f0")
      
      labels_legend <- build_interval_labels(brks, as_percent = (ind == "prop_apadt"))
      mids  <- (brks[-length(brks)] + brks[-1]) / 2
      cols_legend <- pal(mids)
      
      leaflet::leafletProxy("map_apadt", data = shp) %>%
        leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::setView(CO_LNG, CO_LAT, CO_ZOOM) %>%  # ✅ MÁS ZOOM Colombia (visualización)
        leaflet::addPolygons(
          layerId = ~COD_DPTO2,
          fillColor = ~pal(valor),
          color = COLOR_BORDE, weight = 0.7, fillOpacity = 0.9,
          label = ~lapply(
            paste0(
              "<b>", nombre, "</b><br>", label_txt_global, ": ",
              ifelse(is.na(valor), "NA",
                     if (ind == "prop_apadt") fmt_pct(valor, acc = 0.1) else fmt_int(valor))
            ),
            htmltools::HTML
          ),
          labelOptions = leaflet::labelOptions(sticky = FALSE, textsize = "12px", direction = "auto"),
          highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          colors   = cols_legend,
          labels   = labels_legend,
          opacity  = 0.9,
          title    = if (ind == "prop_apadt") "Porcentaje" else "Hectáreas"
        )
      
    } else {
      
      sel_cod <- get_cod_from_dep_name(input$f_dep)
      if (is.na(sel_cod) || !nzchar(sel_cod)) {
        sel_cod <- d$COD_DANE_DPTO %>% unique() %>% sprintf("%02d", as.integer(.)) %>% .[1]
      }
      
      dd <- d %>%
        mutate(COD_MUN5 = sprintf("%05d", as.integer(COD_DANE_MUNI))) %>%
        group_by(COD_DANE_MUNI, MUNICIPIO_TC, DEPARTAMENTO_TC) %>%
        summarise(
          valor = if (ind == "area_apadt_ha") sum(area_apadt_ha, na.rm = TRUE)
          else sum(area_apadt_ha, na.rm = TRUE) / sum(area_mpio_ha, na.rm = TRUE),
          area_apadt_ha = sum(area_apadt_ha, na.rm = TRUE),
          prop_apadt    = sum(area_apadt_ha, na.rm = TRUE) / sum(area_mpio_ha, na.rm = TRUE),
          pob_total     = mean(pob_total, na.rm = TRUE),
          .groups = "drop"
        )
      
      shp <- mun_sf %>% filter(COD_DPTO2 == sel_cod)
      idx <- match(shp$COD_MUN5, sprintf("%05d", as.integer(dd$COD_DANE_MUNI)))
      
      shp$valor         <- dd$valor[idx]
      shp$MUNICIPIO_TC  <- dd$MUNICIPIO_TC[idx]
      shp$area_apadt_ha <- dd$area_apadt_ha[idx]
      shp$prop_apadt    <- dd$prop_apadt[idx]
      shp$pob_total     <- dd$pob_total[idx]
      shp$DEP_TC        <- dd$DEPARTAMENTO_TC[idx]
      
      shp <- shp %>% mutate(
        MUNICIPIO_MOSTRAR = coalesce(MUNICIPIO_TC, title_case_es(MUNICIPIO_N)),
        DEP_MOSTRAR       = coalesce(DEP_TC, "")
      )
      
      brks <- compute_breaks_quartiles(shp$valor)
      pal  <- leaflet::colorBin(PALETA_AZUL, domain = shp$valor, bins = brks, na.color = "#f0f0f0")
      
      labels_legend <- build_interval_labels(brks, as_percent = (ind == "prop_apadt"))
      mids  <- (brks[-length(brks)] + brks[-1]) / 2
      cols_legend <- pal(mids)
      
      bb  <- sf::st_bbox(shp)
      ctr <- bbox_center(bb)
      z   <- zoom_from_bbox(bb)
      
      leaflet::leafletProxy("map_apadt", data = shp) %>%
        leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::setView(lng = ctr["lng"], lat = ctr["lat"], zoom = z) %>%  # ✅ MÁS ZOOM depto (visualización)
        leaflet::addPolygons(
          layerId = ~COD_MUN5,
          fillColor = ~pal(valor),
          color = COLOR_BORDE, weight = 0.4, fillOpacity = 0.9,
          label = ~lapply(
            paste0(
              "<b>", MUNICIPIO_MOSTRAR, " (", DEP_MOSTRAR, ")</b><br>",
              label_txt_global, ": ",
              ifelse(is.na(valor), "NA",
                     if (ind == "prop_apadt") fmt_pct(valor, acc = 0.1) else fmt_int(valor)),
              "<br>Área (ha): ", fmt_int(area_apadt_ha),
              "<br>Porcentaje: ", fmt_pct(prop_apadt, acc = 0.1),
              "<br>Población: ", fmt_int(pob_total)
            ),
            htmltools::HTML
          ),
          labelOptions = leaflet::labelOptions(sticky = FALSE, textsize = "12px", direction = "auto"),
          highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          colors   = cols_legend,
          labels   = labels_legend,
          opacity  = 0.9,
          title    = if (ind == "prop_apadt") "Porcentaje" else "Hectáreas"
        )
    }
  })
  
  # ---- BARRAS TOP-10 ----
  bar_plot_obj <- reactive({
    d <- base_filtrada()
    if (is.null(d) || NROW(d) == 0) return(plotly::plotly_empty())
    
    ind <- input$f_ind
    
    d1 <- d %>%
      group_by(DEPARTAMENTO_TC, MUNICIPIO_TC) %>%
      summarise(
        val = if (ind == "prop_apadt")
          sum(area_apadt_ha, na.rm = TRUE) / sum(area_mpio_ha, na.rm = TRUE)
        else
          sum(area_apadt_ha, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(val)) %>%
      slice_head(n = 10) %>%
      mutate(y_id = paste0(MUNICIPIO_TC, " — ", DEPARTAMENTO_TC),
             y_lab = MUNICIPIO_TC)
    
    if (!NROW(d1) || all(!is.finite(d1$val))) return(plotly::plotly_empty())
    
    ord   <- order(d1$val)
    d1    <- d1[ord, ]
    yvals <- d1$y_id
    ytexts<- d1$y_lab
    
    max_val <- max(d1$val, na.rm = TRUE)
    ticks   <- pretty(c(0, max_val), n = 5)
    
    if (ind == "prop_apadt") {
      txt_vals   <- fmt_pct(d1$val, acc = 0.1)
      hover_tpl  <- "%{y}<br>Porcentaje: %{x:.1%}<extra></extra>"
      xaxis_conf <- list(title="Porcentaje", tickmode="array", tickvals=ticks, ticktext=fmt_pct(ticks, acc=0.1))
    } else {
      txt_vals   <- fmt_num(d1$val, digs = 1)
      hover_tpl  <- "%{y}<br>Hectáreas: %{text} ha<extra></extra>"
      xaxis_conf <- list(title="Hectáreas", tickmode="array", tickvals=ticks, ticktext=fmt_num(ticks, digs=1))
    }
    
    rel      <- d1$val / max(d1$val, na.rm = TRUE)
    text_pos <- ifelse(rel < 0.08, "outside", "inside")
    
    plotly::plot_ly(
      d1, x = ~val, y = ~factor(y_id, levels = yvals),
      type = "bar", orientation = "h",
      marker = list(color = COLOR_RANK),
      text = txt_vals,
      hovertemplate = hover_tpl,
      textposition = text_pos,
      texttemplate = "%{text}",
      cliponaxis = FALSE
    ) %>%
      plotly::layout(
        xaxis = xaxis_conf,
        yaxis = list(title="", automargin=TRUE, tickmode="array", tickvals=yvals, ticktext=ytexts),
        margin=list(l=10,r=10,b=10,t=10)
      ) %>%
      plotly::config(locale = "es")
  })
  
  output$bar_top <- renderPlotly({ bar_plot_obj() })
  
  # -------- Descarga PNG (MAPA con leyenda) --------
  output$dl_map_png <- downloadHandler(
    filename = function(){
      dep_tag <- if (is.null(input$f_dep) || input$f_dep=="Todos") "Colombia" else gsub("\\s+","_", input$f_dep)
      paste0("mapa_apadt_", dep_tag, "_", input$anio, "_", input$f_ind, ".png")
    },
    content = function(file){
      ok <- save_widget_png(map_widget(), file, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay = PNG_DELAY)
      if (!ok) stop("No se pudo generar el PNG del mapa. Revisa webshot2/Chromium y acceso a tiles.")
    }
  )
  
  # -------- Descarga PNG (TOP10) --------
  output$dl_bar_png <- downloadHandler(
    filename = function(){
      paste0("top10_apadt_", input$anio, "_", input$f_ind, ".png")
    },
    content = function(file){
      ok <- save_widget_png(bar_plot_obj(), file, vwidth = 980, vheight = 720, delay = 0.8)
      if (!ok) stop("No se pudo generar el PNG del top10. Revisa webshot2/Chromium.")
    }
  )
  
  # -------- Informe RMarkdown (PDF) — genera PNG antes de render --------
  output$dl_report_pdf <- downloadHandler(
    filename = function() {
      dep_tag <- if (is.null(input$f_dep) || input$f_dep=="Todos") "Colombia" else gsub("\\s+","_", input$f_dep)
      mun_tag <- if (is.null(input$f_mun) || input$f_mun=="Todos") "Todos" else gsub("\\s+","_", input$f_mun)
      ind_tag <- if (is.null(input$f_ind)) "indicador" else input$f_ind
      paste0("Informe_APADT_", dep_tag, "_", mun_tag, "_", input$anio, "_", ind_tag, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      
      anio_now <- input$anio
      dep_now  <- input$f_dep %||% "Todos"
      mpio_now <- input$f_mun %||% "Todos"
      ind_now  <- input$f_ind %||% "area_apadt_ha"
      
      prop_min_now <- NA_real_
      prop_max_now <- NA_real_
      prop_txt_now <- "—"
      if (!is.null(ind_now) && ind_now == "prop_apadt") {
        rng <- prop_range()
        prop_min_now <- rng[1]
        prop_max_now <- rng[2]
        if (!is.null(input$f_prop) && length(input$f_prop) == 2) {
          prop_txt_now <- paste0(input$f_prop[1], " — ", input$f_prop[2])
        } else {
          prop_txt_now <- paste0(scales::percent(prop_min_now, accuracy=0.1, decimal.mark=","), " — ",
                                 scales::percent(prop_max_now, accuracy=0.1, decimal.mark=","))
        }
      }
      
      if (!file.exists(ruta_rmd)) {
        stop("No encuentro Informe_descargable.Rmd en la raíz del proyecto ni en data/.")
      }
      
      dir.create(EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)
      
      ok_map <- save_widget_png(map_widget(), IMG_MAP_AUTO, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay = PNG_DELAY)
      ok_top <- save_widget_png(bar_plot_obj(), IMG_TOP_AUTO, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay = 0.8)
      
      if (!ok_map) stop("No se pudo generar Descargas/APADT_mapa.png para el informe.")
      if (!ok_top) stop("No se pudo generar Descargas/APADT_top10.png para el informe.")
      
      suppressWarnings(file.copy(IMG_MAP_AUTO, IMG_MAP_COMPAT, overwrite = TRUE))
      suppressWarnings(file.copy(IMG_TOP_AUTO, IMG_TOP_COMPAT, overwrite = TRUE))
      
      ind_txt <- if (ind_now == "area_apadt_ha") "Área Potencial (ha)" else "Porcentaje del Área Potencial"
      
      filtros_tbl <- data.frame(
        Parametro = c("Año", "Departamento", "Municipio", "Indicador", "Filtro porcentaje"),
        Valor = c(
          safe_chr(anio_now),
          if (is.null(dep_now)  || dep_now=="Todos")  "Todos" else safe_chr(dep_now),
          if (is.null(mpio_now) || mpio_now=="Todos") "Todos" else safe_chr(mpio_now),
          ind_txt,
          prop_txt_now
        ),
        stringsAsFactors = FALSE
      )
      
      tmp_out <- tempfile(fileext = ".pdf")
      rmarkdown::render(
        input         = ruta_rmd,
        output_file   = tmp_out,
        params        = list(
          app_root   = app_root,
          data_dir   = data_dir,
          export_dir = "Descargas",
          filtros    = filtros_tbl,
          anio       = anio_now,
          dep        = dep_now,
          mpio       = mpio_now,
          ind        = ind_now,
          prop_min   = prop_min_now,
          prop_max   = prop_max_now,
          img_map    = basename(IMG_MAP_AUTO),
          img_top10  = basename(IMG_TOP_AUTO)
        ),
        knit_root_dir = app_root,
        envir         = new.env(parent = globalenv()),
        quiet         = TRUE
      )
      
      file.copy(tmp_out, file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
}

shinyApp(ui, server)



