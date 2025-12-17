# app_apadt.R — UPRA (estilo FA) SIN pestañas, layout uniforme + 4 KPIs (policy)

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(leaflet); library(sf); library(dplyr); library(tidyr)
  library(scales); library(htmltools); library(plotly)
  library(stringi); library(readr); library(tibble)
  library(shinyjs)
})

options(stringsAsFactors = FALSE)
options(scipen = 999)      # evita notación científica al imprimir (NO cambia valores)
sf::sf_use_s2(FALSE)

# ---------- Rutas ----------
local_data_dir <- "data"
app_root     <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
rel_data_dir <- file.path(app_root, "data")
data_dir     <- if (dir.exists(rel_data_dir)) rel_data_dir else local_data_dir

ruta_apadt    <- file.path(data_dir, "014_UPRA_APADT.rds")
ruta_pob      <- file.path(data_dir, "051_DANE_Proyecciones_P_total.rds")
ruta_shp_mun  <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")
ruta_shp_dep  <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")

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

# ---------- Utils ----------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
norm_txt <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
NUP      <- function(x) toupper(norm_txt(x))

# ✅ NUM: robusto (NO re-parsea si ya es numérico; soporta "," o "." como decimal)
num_or_na <- function(x){
  if (is.numeric(x)) return(as.numeric(x))
  
  x0 <- trimws(as.character(x))
  x0[x0 %in% c("", "NA", "NaN", "Inf", "-Inf")] <- NA_character_
  
  if (any(grepl(",", x0, fixed = TRUE), na.rm = TRUE)) {
    return(suppressWarnings(readr::parse_number(
      x0,
      locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
    )))
  }
  
  suppressWarnings(readr::parse_number(
    x0,
    locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
  ))
}

pick_col <- function(df, primary, pattern){
  nms <- names(df)
  if (primary %in% nms) return(primary)
  alt <- nms[grepl(pattern, nms, ignore.case = TRUE)]
  if (length(alt)) alt[1] else NA_character_
}
safe_pull <- function(df, col) if (!is.na(col) && col %in% names(df)) df[[col]] else NA

# Title Case (ES) respetando tildes/ñ
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

# Formato numérico ES (para mostrar)
fmt_pct <- function(x, acc = 0.1){
  ifelse(is.na(x), "NA",
         scales::percent(x, accuracy = acc, decimal.mark = ","))
}
fmt_num <- function(x, digs = 1){
  ifelse(is.na(x), "NA",
         scales::number(x, accuracy = digs, big.mark = ".", decimal.mark = ","))
}
fmt_int <- function(x){
  ifelse(is.na(x), "NA",
         scales::number(x, accuracy = 1, big.mark = ".", decimal.mark = ","))
}

# --- Cuartiles para el mapa ---
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
      format_interval_label(
        a          = breaks[i],
        b          = breaks[i + 1],
        as_percent = as_percent,
        is_first   = (i == 1)
      )
    },
    character(1)
  )
}

# ---------- Colores ----------
PALETA_AZUL <- c("#e0f3fa","#99d5ec","#4bb5e1","#005b88")
COLOR_RANK  <- "#009edb"
COLOR_BORDE <- "#99d5ec"

# ---------- Cargar APADT + población ----------
apadt_raw <- readRDS(ruta_apadt)
pob_raw   <- readRDS(ruta_pob)

# Mapeo de columnas
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

# Si prop viene en % (48,0) → 48, dividir por 100
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
    version = 5, primary = "#2563eb",
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius"   = "0.9rem",
    "font-size-base"  = "0.98rem"
  ),
  tags$head(
    tags$style(HTML("
      :root{
        --accent-border:#99d5ec;
        --gap:12px;
        --viz-row-top:380px;
        --viz-row-bottom:300px;
        --kpi-h: 110px;
      }
      .wrap{ max-width:1360px; margin:0 auto; padding:16px 20px 32px; }
      h3{ font-weight:700; letter-spacing:.2px; margin-bottom:8px; }
      .data-note{ font-size:13px; color:#6b7280; margin:0 0 16px; }

      .filters{
        background:#fff;
        border:1px solid var(--accent-border) !important;
        border-radius:16px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
        padding:6px 12px 8px;
        margin-bottom:12px;
      }
      .card{
        background:#fff;
        border:1px solid var(--accent-border) !important;
        border-radius:16px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
        padding:12px;
        margin-bottom:12px;
      }

      .filters-grid{
        display:grid;
        grid-template-columns:repeat(6,minmax(180px,1fr));
        gap:12px;
        align-items:stretch;
      }
      .filter{ display:flex; flex-direction:column; justify-content:flex-start; }
      .filter-label{
        font-family:'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-size:14px; font-weight:500; letter-spacing:.2px; color:#111827;
        margin-bottom:6px; line-height:1.2; min-height:36px;
        display:flex; align-items:flex-end;
      }
      .filters-grid .shiny-input-container{ margin:0 !important; }
      .filters-grid .selectize-control{ margin:0 !important; }
      .filters-grid .selectize-input,
      .filters-grid .form-control,
      .filters-grid .form-select{
        height:60px !important; min-height:60px;
        padding-top:10px; padding-bottom:10px;
        border-radius:10px;
        border:1px solid var(--accent-border) !important;
      }
      .filters-grid .selectize-input:focus,
      .filters-grid .form-control:focus,
      .filters-grid .form-select:focus{
        border-color:var(--accent-border) !important;
        box-shadow:0 0 0 .2rem rgba(153,213,236,.25) !important;
      }

      .card-title{ font-weight:700; font-size:16px; margin-bottom:8px; color:#111827; }

      .leaflet-control,
      .leaflet-control .legend,
      .leaflet-control .info{
        border:1px solid var(--accent-border) !important;
        border-radius:12px;
      }
      .leaflet-top .leaflet-control { margin-top: 6px; }
      .leaflet-left .leaflet-control { margin-left: 6px; }

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

      .quantiles-note{
        margin-top:6px;
        font-size:12px;
        color:#4b5563;
        border-top:1px dashed #d1d5db;
        padding-top:6px;
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
      .kpi.muted{opacity:.25}
    "))
  ),
  div(class="wrap",
      h3(""),
      div(class="data-note",
          HTML("")
      ),
      div(class="filters",
          div(class="filters-grid",
              div(class="filter",
                  div(class="filter-label","¿Qué año analizamos?"),
                  uiOutput("anio_ui")
              ),
              div(class="filter",
                  div(class="filter-label","¿En qué departamento?"),
                  selectInput("f_dep", NULL, choices = "Todos", selected = "Todos")
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
                      "Área Potencial"                  = "area_apadt_ha",
                      "Porcentaje del Área Potencial"   = "prop_apadt"
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
              div(class="card-title", textOutput("map_title")),
              div(class="viz-body",
                  leafletOutput("map_apadt", height = "100%"),
                  div(class="quantiles-note",
                      HTML("Nota: la segmentación en el mapa muestra los cuartiles de segmentación debajo del mapa.")
                  )
              )
          ),
          div(class="card viz-card",
              div(class="card-title", textOutput("title_top")),
              div(class="viz-body",
                  plotlyOutput("bar_top", height = "80%")
              )
          ),
          div(class="card viz-card",
              div(class="card-title","Información del Área Potencial de Adecuación de Tierras"),
              div(class="viz-body",
                  uiOutput("kpi_boxes")
              )
          )
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
  
  observeEvent(input$anio, {
    df <- base_apadt %>% filter(ano == input$anio)
    deps_tc <- df %>% pull(DEPARTAMENTO_TC) %>% safe_choices() %>% sort()
    
    if (first_run()) {
      sel_dep <- if ("Santander" %in% deps_tc) "Santander" else "Todos"
      first_run(FALSE)
    } else {
      sel_dep <- if (!is.null(input$f_dep) && input$f_dep %in% c("Todos", deps_tc)) input$f_dep else "Todos"
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
    } else update_select_clean("f_mun", character(0), selected = "Todos")
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_reset, {
    yrs <- sort(unique(na.omit(base_apadt$ano)))
    updateSelectInput(session, "anio", selected = max(yrs, na.rm = TRUE))
    update_select_clean("f_dep", character(0), selected = "Todos")
    update_select_clean("f_mun", character(0), selected = "Todos")
    updateSelectInput(session, "f_ind", selected = "prop_apadt")
    if (!is.null(input$f_prop)) shinyWidgets::updateSliderTextInput(session, "f_prop", selected = c("0%","100%"))
  })
  
  observeEvent(input$btn_back_co, {
    updateSelectInput(session, "f_dep", selected = "Todos")
  })
  
  # =========================================================
  # ✅ CORRECCIÓN 1: SOLO filtrar por porcentaje si el indicador es prop_apadt
  # =========================================================
  base_filtrada <- reactive({
    req(input$anio)
    
    d <- base_apadt %>% filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") d <- d %>% filter(DEPARTAMENTO_TC == input$f_dep)
    if (!is.null(input$f_mun) && input$f_mun != "Todos") d <- d %>% filter(MUNICIPIO_TC == input$f_mun)
    
    if (is.null(input$f_ind) || input$f_ind == "area_apadt_ha") return(d)
    
    rng <- prop_range()
    d %>% filter(!is.na(prop_apadt), prop_apadt >= rng[1], prop_apadt <= rng[2])
  })
  
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
    d <- base_apadt %>% filter(
      ano == input$anio,
      DEPARTAMENTO_TC == input$f_dep,
      MUNICIPIO_TC   == input$f_mun
    )
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
                   if (nzchar(sub)) div(class="kpi-sub", sub)
    )
  }
  
  output$kpi_boxes <- renderUI({
    dep <- kpi_dep()
    mun <- kpi_mun()
    
    dep_pct <- fmt_pct(dep$prop, acc = 0.1)
    dep_ha  <- fmt_num(dep$area_apadt, digs = 1)
    dep_sub <- paste0("del territorio (", fmt_num(dep$area_tot, digs = 1), " ha)")
    
    if (is.null(mun)) {
      htmltools::div(class="kpi-grid",
                     kpi_card("Área departamental Potencial de Adecuación de Tierras / Área departamental — % del Área", dep_pct, dep_sub),
                     kpi_card("Área departamental — hectáreas", dep_ha, "del Área Potencial de Adecuación de Tierras"),
                     kpi_card("A nivel municipal  — % de Área Potencial de Adecuación de Tierras", "—", "selecciona un municipio", muted = TRUE),
                     kpi_card("A nivel municipal  — Área Potencial de Adecuación de Tierras (ha)", "—", "selecciona un municipio", muted = TRUE)
      )
    } else {
      mun_pct <- fmt_pct(mun$prop, acc = 0.1)
      mun_ha  <- fmt_num(mun$area_apadt, digs = 1)
      mun_sub <- paste0("del municipio (", fmt_num(mun$area_tot, digs = 1), " ha)")
      
      htmltools::div(class="kpi-grid",
                     kpi_card("Área departamental Potencial de Adecuación de Tierras / Área departamental — % del Área", dep_pct, dep_sub),
                     kpi_card("Área departamental — hectáreas", dep_ha, "del Área Potencial de Adecuación de Tierras"),
                     kpi_card("A nivel municipal  — % del Área Potencial de Adecuación de Tierras", mun_pct, mun_sub),
                     kpi_card("A nivel municipal  — hectáreas", mun_ha, mun_sub)
      )
    }
  })
  
  output$map_title <- renderText({
    ind <- input$f_ind
    dep <- input$f_dep %||% "Todos"
    dep_todos <- is.null(dep) || dep == "Todos"
    if (ind == "prop_apadt") {
      if (dep_todos)
        "¿Qué departamentos tienen mayor porcentaje del Área Potencial de Adecuación de Tierras?"
      else
        "¿Qué municipios tienen mayor porcentaje del Área Potencial de Adecuación de Tierras?"
    } else {
      if (dep_todos)
        "¿Qué departamentos concentran más hectáreas del Área Potencial de Adecuación de Tierras?"
      else
        "¿Qué municipios concentran más hectáreas del Área Potencial de Adecuación de Tierras?"
    }
  })
  
  output$title_top <- renderText({
    if (input$f_ind == "prop_apadt")
      "Top 10 municipios con mayor porcentaje del Área Potencial de Adecuación de Tierras"
    else
      "Top 10 municipios con más hectáreas del Área Potencial de Adecuación de Tierras"
  })
  
  output$map_apadt <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-74.3, 5, 5.45) %>%
      htmlwidgets::onRender("function(el,x){ this.zoomControl.setPosition('topright'); }")
  })
  
  get_cod_from_dep_name <- function(dep_name){
    if (is.null(dep_name) || dep_name == "Todos") return(NA_character_)
    dep_norm <- NUP(dep_name)
    i <- which(dep_sf$DEP_NORM_SHP == dep_norm)[1]
    if (is.finite(i)) return(dep_sf$COD_DPTO2[i])
    j <- which(dep_lookup$DEP_NORM == dep_norm)[1]
    if (is.finite(j)) return(dep_lookup$COD_DPTO2[j])
    NA_character_
  }
  
  observe({
    d <- base_filtrada()
    if (is.null(d) || nrow(d) == 0) {
      leafletProxy("map_apadt") %>% clearShapes() %>% clearControls()
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
      
      leafletProxy("map_apadt", data = shp) %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(
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
          labelOptions = labelOptions(sticky = FALSE, textsize = "12px", direction = "auto"),
          highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
        ) %>%
        addLegend(
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
      
      leafletProxy("map_apadt", data = shp) %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(
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
          labelOptions = labelOptions(sticky = FALSE, textsize = "12px", direction = "auto"),
          highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
        ) %>%
        addLegend(
          position = "bottomright",
          colors   = cols_legend,
          labels   = labels_legend,
          opacity  = 0.9,
          title    = if (ind == "prop_apadt") "Porcentaje" else "Hectáreas"
        ) %>%
        fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  
  # ---- BARRAS TOP-10 ----
  output$bar_top <- renderPlotly({
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
      { if (ind == "prop_apadt") dplyr::filter(., val >= 0.20) else . } %>%
      arrange(desc(val)) %>%
      slice_head(n = 10) %>%
      mutate(
        y_id  = paste0(MUNICIPIO_TC, " — ", DEPARTAMENTO_TC),
        y_lab = MUNICIPIO_TC
      )
    
    if (!NROW(d1) || all(!is.finite(d1$val))) return(plotly::plotly_empty())
    
    ord   <- order(d1$val)
    d1    <- d1[ord, ]
    yvals <- d1$y_id
    ytexts<- d1$y_lab
    
    max_val <- max(d1$val, na.rm = TRUE)
    ticks   <- pretty(c(0, max_val), n = 5)
    
    if (ind == "prop_apadt") {
      txt_vals   <- fmt_pct(d1$val, acc = 0.1)
      hover_tpl  <- "%{y}<br>Porcentaje: %{text}<extra></extra>"
      xaxis_conf <- list(
        title    = "Porcentaje",
        tickmode = "array",
        tickvals = ticks,
        ticktext = fmt_pct(ticks, acc = 0.1)
      )
    } else {
      txt_vals   <- fmt_num(d1$val, digs = 1)
      hover_tpl  <- "%{y}<br>Hectáreas: %{text} ha<extra></extra>"
      xaxis_conf <- list(
        title    = "Hectáreas",
        tickmode = "array",
        tickvals = ticks,
        ticktext = fmt_num(ticks, digs = 1)
      )
    }
    
    rel      <- d1$val / max(d1$val, na.rm = TRUE)
    text_pos <- ifelse(rel < 0.08, "outside", "inside")
    
    plotly::plot_ly(
      d1,
      x = ~val,
      y = ~factor(y_id, levels = yvals),
      type = "bar",
      orientation = "h",
      marker = list(color = COLOR_RANK),
      text = txt_vals,
      hovertemplate = hover_tpl,
      textposition = text_pos,
      texttemplate = "%{text}",
      insidetextanchor = "right",
      insidetextfont = list(
        family = "Inter Black, Inter, Arial, sans-serif",
        size   = 12,
        color  = "white"
      ),
      outsidetextfont = list(
        family = "Inter SemiBold, Inter, Arial, sans-serif",
        size   = 12,
        color  = "#111827"
      ),
      cliponaxis = FALSE
    ) %>%
      plotly::layout(
        xaxis  = xaxis_conf,
        yaxis  = list(
          title      = "",
          automargin = TRUE,
          tickmode   = "array",
          tickvals   = yvals,
          ticktext   = ytexts
        ),
        margin = list(l = 10, r = 10, b = 10, t = 10)
      ) %>%
      plotly::config(locale = "es")
  })
}

shinyApp(ui, server)


