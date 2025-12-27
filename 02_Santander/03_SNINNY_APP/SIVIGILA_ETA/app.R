# app.R
# =========================================================
# ETA — Dashboard (app exclusiva) - SANTANDER (CORREGIDA)
# Vista única: Exploración ETA
# - FIX municipios: se actualizan bien por DEPTO (desde el inicio)
# - FIX arranque: no revienta por inputs NULL
# - FIX archivos: NO pide NDA, SOLO ETA (022_...)
# - Paleta: GAMAS DE NARANJAS
# =========================================================

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(leaflet); library(sf); library(dplyr); library(tidyr)
  library(scales); library(htmltools); library(plotly)
  library(stringi)
})

options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)
options(shiny.maxRequestSize = 100*1024^2)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---------- Rutas ----------
local_data_dir <- "data"
app_root     <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
rel_data_dir <- file.path(app_root, "data")
data_dir <- if (dir.exists(rel_data_dir)) rel_data_dir else local_data_dir

eta_path       <- file.path(data_dir, "022_INS_SIVIGILA-ETA.rds")  # <- SOLO ETA
ruta_pob       <- file.path(data_dir, "051_DANE_Proyecciones_P_total.rds")
ruta_shp_mpios <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")
ruta_shp_dptos <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")

must_exist <- c(eta_path, ruta_pob, ruta_shp_mpios, ruta_shp_dptos)
miss <- must_exist[!file.exists(must_exist)]
if (length(miss)) stop("Faltan archivos. data_dir usado: ", data_dir, "\n", paste("-", miss, collapse = "\n"))

check_shp_parts <- function(shp){
  base <- sub("\\.shp$", "", shp)
  req  <- paste0(base, c(".shp",".dbf",".shx",".prj"))
  req[!file.exists(req)]
}
miss_shp <- c(check_shp_parts(ruta_shp_mpios), check_shp_parts(ruta_shp_dptos))
if (length(miss_shp)) stop("Faltan componentes de shapefile:\n", paste("-", miss_shp, collapse = "\n"))

# ---------- Helper: Title Case español ----------
title_case_es <- function(x){
  x <- trimws(as.character(x))
  is_na <- is.na(x) | x == ""
  if (all(is_na)) return(x)
  
  small <- c("de","del","la","las","los","y","o","u","en","el","por","para","a","e","con")
  x_ok <- x[!is_na]
  x_ok <- stringi::stri_trans_tolower(x_ok, locale = "es")
  x_ok <- stringi::stri_trans_totitle(x_ok, locale = "es")
  
  x_ok <- vapply(x_ok, function(s){
    parts <- unlist(strsplit(s, " ", fixed = TRUE))
    if (!length(parts)) return(s)
    for (i in seq_along(parts)) {
      w <- tolower(parts[i])
      if (i != 1 && w %in% small) parts[i] <- w
    }
    paste(parts, collapse = " ")
  }, FUN.VALUE = character(1))
  
  x[!is_na] <- x_ok
  x
}

get_col <- function(df, opts, stop_msg){
  nm <- opts[opts %in% names(df)][1]
  if (is.na(nm) || !nzchar(nm)) stop(stop_msg) else nm
}
get_col_opt <- function(df, opts){
  nm <- opts[opts %in% names(df)][1]
  if (is.na(nm) || !nzchar(nm)) NA_character_ else nm
}

# =========================================================
# 1) Leer ETA y normalizar
# =========================================================
eta_raw <- readRDS(eta_path)

year_col     <- get_col(eta_raw, c("ano","ANO","year","YEAR"), "ETA: no encuentro columna de año (ano/ANO/year)")
mun_code_col <- get_col(eta_raw, c("COD_DANE_MUNIC_D","COD_MUN5","MPIO_CDPMP","COD_MPIO"), "ETA: no encuentro código municipal (COD_DANE_MUNIC_D/COD_MUN5/...)")
dep_name_col <- get_col(eta_raw, c("DEPARTAMENTO_D","DEPARTMENTO_D","DEPARTAMENTO","DEPARTMENTO"), "ETA: no encuentro nombre de depto")
mun_name_col <- get_col(eta_raw, c("MUNICIPIO_D","MUNICIPIO","MUNICIPIO_NOMBRE"), "ETA: no encuentro nombre de municipio")

tenf_col <- get_col_opt(eta_raw, c("TOTAL_ENF","total_enf","TOTALENF","Total_enf","total_enfermos","TOTAL_ENFERMOS"))
texp_col <- get_col_opt(eta_raw, c("TOTAL_EXP","total_exp","TOTALEXP","Total_exp","total_expuestos","TOTAL_EXPUES"))
hom_col  <- get_col_opt(eta_raw, c("total_hom","TOTAL_HOM","HOMBRES","hom"))
muj_col  <- get_col_opt(eta_raw, c("total_muj","TOTAL_MUJ","MUJERES","muj"))

origen_cols <- intersect(c("agua","alimentos","pers_pers","cont_ambie","otro","desconocid"), names(eta_raw))

eta <- eta_raw %>%
  dplyr::transmute(
    ano       = suppressWarnings(as.integer(.data[[year_col]])),
    COD_MUN5  = sprintf("%05d", suppressWarnings(as.integer(.data[[mun_code_col]]))),
    COD_DPTO2 = substr(COD_MUN5, 1, 2),
    DEP_N     = title_case_es(trimws(as.character(.data[[dep_name_col]]))),
    MUN_N     = title_case_es(trimws(as.character(.data[[mun_name_col]]))),
    TOTAL_ENF = if (!is.na(tenf_col)) suppressWarnings(as.numeric(.data[[tenf_col]])) else NA_real_,
    TOTAL_EXP = if (!is.na(texp_col)) suppressWarnings(as.numeric(.data[[texp_col]])) else NA_real_,
    TOTAL_HOM = if (!is.na(hom_col))  suppressWarnings(as.numeric(.data[[hom_col]])) else NA_real_,
    TOTAL_MUJ = if (!is.na(muj_col))  suppressWarnings(as.numeric(.data[[muj_col]])) else NA_real_
  ) %>%
  dplyr::mutate(
    TOTAL_NR = pmax(coalesce(TOTAL_ENF, 0) - (coalesce(TOTAL_HOM, 0) + coalesce(TOTAL_MUJ, 0)), 0),
    COD_DPTO2 = sprintf("%02d", as.integer(COD_DPTO2)),
    COD_MUN5  = sprintf("%05d", as.integer(COD_MUN5))
  ) %>%
  dplyr::filter(!is.na(ano), !is.na(COD_MUN5), !is.na(COD_DPTO2))

if (length(origen_cols) > 0) {
  eta <- dplyr::bind_cols(eta, eta_raw %>% dplyr::select(dplyr::all_of(origen_cols))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(origen_cols), ~ ifelse(as.integer(.x) == 1, 1L, 0L)))
}

# =========================================================
# 1b) Población
# =========================================================
pob_raw <- readRDS(ruta_pob)

pob_year_col <- get_col(pob_raw, c("ano","ANO","year","YEAR"), "POB: no encuentro columna año")
pob_mun_code_col <- get_col(pob_raw, c("COD_MUN5","COD_DANE_MUNIC_D","COD_MPIO","MPIO_CDPMP"), "POB: no encuentro código municipal")
pob_val_col <- get_col(pob_raw, c("P_TOTAL","POBLACION","Poblacion","total","poblacion"), "POB: no encuentro columna población total")

pob_norm <- pob_raw %>%
  dplyr::transmute(
    ano       = suppressWarnings(as.integer(.data[[pob_year_col]])),
    COD_MUN5  = sprintf("%05d", suppressWarnings(as.integer(.data[[pob_mun_code_col]]))),
    COD_DPTO2 = substr(COD_MUN5, 1, 2),
    POB       = suppressWarnings(as.numeric(.data[[pob_val_col]]))
  ) %>%
  dplyr::filter(!is.na(ano), !is.na(COD_MUN5), !is.na(COD_DPTO2), is.finite(POB))

pob_mpio  <- pob_norm %>%
  dplyr::group_by(ano, COD_MUN5, COD_DPTO2) %>%
  dplyr::summarise(POB = sum(POB, na.rm = TRUE), .groups = "drop")

pob_depto <- pob_norm %>%
  dplyr::group_by(ano, COD_DPTO2) %>%
  dplyr::summarise(POB = sum(POB, na.rm = TRUE), .groups = "drop")

# =========================================================
# 2) Shapes
# =========================================================
mpios_raw <- sf::st_read(ruta_shp_mpios, quiet = TRUE)
dptos_raw <- sf::st_read(ruta_shp_dptos, quiet = TRUE)

mpios_sf <- mpios_raw %>%
  dplyr::mutate(
    COD_MUN5 = if ("MPIO_CDPMP" %in% names(.)) sprintf("%05d", as.integer(MPIO_CDPMP))
    else if ("COD_MPIO" %in% names(.)) sprintf("%05d", as.integer(COD_MPIO))
    else stop("Shp municipios: falta MPIO_CDPMP/COD_MPIO"),
    COD_DPTO2   = substr(COD_MUN5, 1, 2),
    MUNICIPIO_N = if ("MPIO_CNMBR" %in% names(.)) as.character(MPIO_CNMBR)
    else if ("NOMBRE_MPIO" %in% names(.)) as.character(NOMBRE_MPIO)
    else "MUNICIPIO"
  ) %>%
  dplyr::mutate(MUNICIPIO_N = title_case_es(MUNICIPIO_N)) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid()

dptos_sf <- dptos_raw %>%
  dplyr::mutate(
    COD_DPTO2 = if ("DPTO_CCDGO" %in% names(.)) sprintf("%02d", as.integer(DPTO_CCDGO))
    else if ("COD_DEPTO" %in% names(.)) sprintf("%02d", as.integer(COD_DEPTO))
    else stop("Shp deptos: falta DPTO_CCDGO/COD_DEPTO"),
    DEPARTAMENTO_N = if ("DEPARTAMENTO_D" %in% names(.)) as.character(DEPARTAMENTO_D)
    else if ("DPTO_CNMBR" %in% names(.)) as.character(DPTO_CNMBR)
    else if ("NOMBRE_DEPTO" %in% names(.)) as.character(NOMBRE_DEPTO)
    else COD_DPTO2
  ) %>%
  dplyr::mutate(DEPARTAMENTO_N = title_case_es(DEPARTAMENTO_N)) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid()

# =========================================================
# Lookups
# =========================================================
dpt_lookup_eta <- eta %>%
  dplyr::select(COD_DPTO2, DEP_N) %>%
  dplyr::mutate(
    COD_DPTO2 = sprintf("%02d", as.integer(COD_DPTO2)),
    DEP_N     = title_case_es(trimws(DEP_N))
  ) %>%
  dplyr::distinct() %>%
  dplyr::arrange(DEP_N)

mun_lookup_eta <- eta %>%
  dplyr::select(COD_DPTO2, COD_MUN5, MUN_N) %>%
  dplyr::mutate(
    COD_DPTO2 = sprintf("%02d", as.integer(COD_DPTO2)),
    COD_MUN5  = sprintf("%05d", as.integer(COD_MUN5)),
    MUN_N_TC  = title_case_es(trimws(as.character(MUN_N)))
  ) %>%
  dplyr::distinct()

dept_choices <- c("Todos", stats::setNames(dpt_lookup_eta$COD_DPTO2, dpt_lookup_eta$DEP_N))

# --- Santander por defecto (ARREGLADO: no Atlántico) ---
SANTANDER_CODE <- {
  idx <- which(toupper(dpt_lookup_eta$DEP_N) == "SANTANDER")
  if (length(idx) > 0) dpt_lookup_eta$COD_DPTO2[idx[1]]
  else if (nrow(dpt_lookup_eta) > 0) dpt_lookup_eta$COD_DPTO2[1]
  else "Todos"
}

# =========================================================
# Colores (NARANJAS)
# =========================================================
MAP_COLORS <- c("#fff1e6", "#ffd4b3", "#ffb366", "#ff8c1a", "#cc5200")
BAR_COLOR  <- "#ff8c1a"
BORDER_COL <- "#f57c00"
GRID_COLOR <- "rgba(148,163,184,0.35)"

# =========================================================
# Helpers cuartiles
# =========================================================
compute_breaks_quartiles <- function(values){
  vals <- suppressWarnings(as.numeric(values))
  vals <- vals[is.finite(vals)]
  if (!length(vals) || all(vals == 0, na.rm = TRUE)) return(c(0, 1))
  
  pos <- vals[vals > 0]
  if (!length(pos)) return(c(0, 1))
  
  qs <- stats::quantile(pos, probs = c(0.25, 0.50, 0.75, 1), na.rm = TRUE)
  brks <- c(0, as.numeric(qs))
  sort(unique(brks))
}

build_interval_labels <- function(breaks){
  if (length(breaks) < 2) return(character(0))
  vapply(seq_len(length(breaks) - 1), function(i){
    a <- breaks[i]; b <- breaks[i + 1]
    fa <- scales::number(a, accuracy = 1, big.mark=".", decimal.mark=",")
    fb <- scales::number(b, accuracy = 1, big.mark=".", decimal.mark=",")
    if (i == 1) sprintf("%s – %s", fa, fb) else sprintf(">%s – %s", fa, fb)
  }, character(1))
}

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    primary = "#2563eb",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius" = "0.9rem",
    "font-size-base" = "0.98rem"
  ),
  tags$head(tags$style(HTML(sprintf("
    :root{ --border-col:%s; --accent:%s; }
    .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
    .filters{
      background:#fff;border:1.5px solid var(--border-col);
      border-radius:16px;padding:14px 16px;margin-bottom:16px;
      box-shadow:0 2px 10px rgba(0,0,0,.04);
    }
    .filters-grid{display:grid;grid-template-columns:repeat(4,minmax(220px,1fr));gap:12px}
    .filter-label{
      font-size:14px;font-weight:500;letter-spacing:.2px;color:#111827;margin-bottom:6px;
      font-family:'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
    }
    .selectize-input,.form-control{
      min-height:42px;border-radius:10px;border:1.5px solid var(--border-col);box-shadow:none !important;
    }
    .selectize-input:focus,.form-control:focus{
      border-color:var(--border-col) !important; outline:0 !important;
      box-shadow:0 0 0 .15rem rgba(245,124,0,.35) !important;
    }
    .card{
      background:#fff;border:1.5px solid var(--border-col);
      border-radius:16px;padding:12px;box-shadow:0 2px 10px rgba(0,0,0,.05);margin-bottom:12px
    }
    .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827;display:flex;align-items:center;gap:6px;}
    .map-note{margin-top:6px;font-size:12px;color:#6b7280;}
  ", BORDER_COL, BAR_COLOR)))),
  div(class="wrap",
      h3(""),
      br(),
      div(class="filters",
          div(class="filters-grid",
              div(class="filter",
                  div(class="filter-label","¿Qué año analizamos?"),
                  uiOutput("anio_e1_ui")
              ),
              div(class="filter",
                  div(class="filter-label","¿En qué departamento?"),
                  selectInput("f_depto_e1", NULL, choices = dept_choices, selected = SANTANDER_CODE)
              ),
              div(class="filter",
                  div(class="filter-label","¿Algún municipio en particular?"),
                  selectInput("f_mpio_e1", NULL, choices = "Todos", selected = "Todos")
              ),
              div(class="filter",
                  div(class="filter-label","Variable a considerar"),
                  selectInput("f_indic_e1", NULL,
                              choices = c("Total de enfermos"="total_enf","Incidencia (x100k)"="incid"),
                              selected = "total_enf"
                  )
              )
          )
      ),
      fluidRow(
        column(6,
               div(class="card",
                   div(class="card-title",
                       span(textOutput("ttl_mapa_e1")),
                       span(style="margin-left:auto;",
                            actionLink("btn_reset_e1","← Volver a Santander")
                       )
                   ),
                   leafletOutput("map_eta_e1", height = 660),
                   div(class="map-note", textOutput("nota_mapa_e1"))
               )
        ),
        column(6,
               div(class="card",
                   div(class="card-title", textOutput("ttl_origen_e1")),
                   plotlyOutput("plot_origen_e1", height = 315)
               ),
               div(class="card",
                   div(class="card-title", textOutput("ttl_top_e1")),
                   plotlyOutput("top_mpios_e1", height = 318)
               )
        )
      )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  
  scope_txt <- reactive({
    dep_code <- input$f_depto_e1 %||% "Todos"
    mun_code <- input$f_mpio_e1  %||% "Todos"
    
    if (dep_code == "Todos") return("Colombia")
    
    dep_nom <- dpt_lookup_eta$DEP_N[dpt_lookup_eta$COD_DPTO2 == dep_code][1]
    if (is.na(dep_nom) || !nzchar(dep_nom)) dep_nom <- dep_code
    
    if (!is.null(mun_code) && mun_code != "Todos") {
      mun_nom <- mun_lookup_eta$MUN_N_TC[mun_lookup_eta$COD_MUN5 == mun_code][1]
      if (!is.na(mun_nom) && nzchar(mun_nom)) return(paste0(mun_nom, ", ", dep_nom))
    }
    dep_nom
  })
  
  indic_lbl_e1 <- reactive({
    if ((input$f_indic_e1 %||% "total_enf") == "total_enf") "Enfermos" else "Incidencia (x100k)"
  })
  
  output$anio_e1_ui <- renderUI({
    yrs <- sort(unique(eta$ano))
    selectInput("f_anio_e1", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  # ===== FIX MUNICIPIOS: actualiza desde el inicio y cada cambio de DEPTO =====
  observeEvent(input$f_depto_e1, {
    dep <- input$f_depto_e1 %||% "Todos"
    if (dep == "Todos") {
      updateSelectInput(session, "f_mpio_e1", choices = "Todos", selected = "Todos")
    } else {
      mm <- mun_lookup_eta %>%
        dplyr::filter(COD_DPTO2 == dep) %>%
        dplyr::arrange(MUN_N_TC)
      updateSelectInput(
        session, "f_mpio_e1",
        choices  = c("Todos", stats::setNames(mm$COD_MUN5, mm$MUN_N_TC)),
        selected = "Todos"
      )
    }
  }, ignoreInit = FALSE)
  
  nivel_e1 <- reactive({
    if (is.null(input$f_depto_e1) || input$f_depto_e1 == "Todos") "deptos" else "mpios"
  })
  
  base_e1 <- reactive({
    req(input$f_anio_e1)
    df <- eta %>% dplyr::filter(ano == input$f_anio_e1)
    
    if (!is.null(input$f_depto_e1) && input$f_depto_e1 != "Todos")
      df <- df %>% dplyr::filter(COD_DPTO2 == input$f_depto_e1)
    
    if (!is.null(input$f_mpio_e1) && input$f_mpio_e1 != "Todos")
      df <- df %>% dplyr::filter(COD_MUN5 == input$f_mpio_e1)
    
    df
  })
  
  agg_depto_e1 <- reactive({
    df <- base_e1()
    if ((input$f_indic_e1 %||% "total_enf") == "total_enf") {
      df %>% dplyr::group_by(COD_DPTO2) %>% dplyr::summarise(valor = sum(TOTAL_ENF, na.rm=TRUE), .groups="drop")
    } else {
      sum_enf <- df %>% dplyr::group_by(COD_DPTO2) %>% dplyr::summarise(total_enf = sum(TOTAL_ENF, na.rm=TRUE), .groups="drop")
      sum_enf %>%
        dplyr::left_join(pob_depto %>% dplyr::filter(ano == input$f_anio_e1), by="COD_DPTO2") %>%
        dplyr::mutate(valor = ifelse(POB > 0, (total_enf/POB)*1e5, NA_real_)) %>%
        dplyr::select(COD_DPTO2, valor)
    }
  })
  
  agg_mpio_e1 <- reactive({
    df <- base_e1()
    if ((input$f_indic_e1 %||% "total_enf") == "total_enf") {
      df %>%
        dplyr::group_by(COD_DPTO2, COD_MUN5, MUN_N) %>%
        dplyr::summarise(valor = sum(TOTAL_ENF, na.rm=TRUE), .groups="drop")
    } else {
      sum_enf <- df %>%
        dplyr::group_by(COD_DPTO2, COD_MUN5, MUN_N) %>%
        dplyr::summarise(total_enf = sum(TOTAL_ENF, na.rm=TRUE), .groups="drop")
      sum_enf %>%
        dplyr::left_join(pob_mpio %>% dplyr::filter(ano == input$f_anio_e1) %>% dplyr::select(COD_MUN5, POB), by="COD_MUN5") %>%
        dplyr::mutate(valor = ifelse(POB > 0, (total_enf/POB)*1e5, NA_real_)) %>%
        dplyr::select(COD_DPTO2, COD_MUN5, MUN_N, valor)
    }
  })
  
  output$ttl_mapa_e1 <- renderText({
    paste0("¿En qué territorios de ", scope_txt(), " está la mayor cantidad de ", tolower(indic_lbl_e1()), "?")
  })
  output$ttl_origen_e1 <- renderText({
    "¿Cuál fue la principal fuente de origen de Enfermedades Transmitidas por Alimentos (ETA)?"
  })
  output$ttl_top_e1 <- renderText({
    paste0("¿Qué municipios tienen mayor cantidad de ", tolower(indic_lbl_e1()), "?")
  })
  
  output$map_eta_e1 <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::setView(lng = -73.2, lat = 7.1, zoom = 7) # Santander approx
  })
  
  output$nota_mapa_e1 <- renderText({
    "Nota: Los colores del mapa representan cuartiles del indicador seleccionado (rangos redondeados)."
  })
  
  observe({
    req(input$f_anio_e1)
    titulo <- indic_lbl_e1()
    
    fmt_val <- function(x){
      if ((input$f_indic_e1 %||% "total_enf") == "incid") {
        scales::number(x, big.mark=".", decimal.mark=",", accuracy=0.1)
      } else {
        scales::number(x, big.mark=".", decimal.mark=",", accuracy=1)
      }
    }
    
    if (nivel_e1() == "deptos") {
      shp <- dptos_sf %>%
        dplyr::left_join(agg_depto_e1(), by="COD_DPTO2") %>%
        dplyr::left_join(dpt_lookup_eta, by="COD_DPTO2") %>%
        dplyr::mutate(
          valor = tidyr::replace_na(valor, 0),
          DEP_N = dplyr::coalesce(DEP_N, DEPARTAMENTO_N, COD_DPTO2),
          etq   = paste0("<b>", DEP_N, "</b><br>", titulo, ": ", fmt_val(valor))
        )
      
      vals <- shp$valor
      brks <- compute_breaks_quartiles(vals)
      pal  <- leaflet::colorBin(MAP_COLORS, domain = vals, bins = brks, na.color = "#f0f0f0")
      labels_legend <- build_interval_labels(brks)
      mids          <- (brks[-length(brks)] + brks[-1]) / 2
      cols_legend   <- pal(mids)
      
      leaflet::leafletProxy("map_eta_e1", data=shp) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addPolygons(
          layerId  = ~COD_DPTO2,
          fillColor= ~pal(valor),
          color    = BORDER_COL, weight = 0.7, fillOpacity = 0.9,
          label    = ~lapply(etq, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(color=BORDER_COL, weight=2, bringToFront=TRUE)
        ) %>%
        leaflet::addLegend("bottomright", colors = cols_legend, labels = labels_legend, opacity = 0.9, title = titulo)
      
    } else {
      sel_dep <- input$f_depto_e1
      req(!is.na(sel_dep), nzchar(sel_dep))
      
      shp <- mpios_sf %>%
        dplyr::filter(COD_DPTO2 == sel_dep) %>%
        dplyr::left_join(agg_mpio_e1() %>% dplyr::select(COD_MUN5, valor), by="COD_MUN5") %>%
        dplyr::left_join(mun_lookup_eta %>% dplyr::select(COD_MUN5, MUN_N_TC), by="COD_MUN5") %>%
        dplyr::mutate(
          valor   = tidyr::replace_na(valor, 0),
          MUN_LBL = dplyr::coalesce(MUN_N_TC, MUNICIPIO_N, COD_MUN5),
          etq     = paste0("<b>", MUN_LBL, "</b><br>", titulo, ": ", fmt_val(valor))
        )
      
      vals <- shp$valor
      brks <- compute_breaks_quartiles(vals)
      pal  <- leaflet::colorBin(MAP_COLORS, domain = vals, bins = brks, na.color = "#f0f0f0")
      labels_legend <- build_interval_labels(brks)
      mids          <- (brks[-length(brks)] + brks[-1]) / 2
      cols_legend   <- pal(mids)
      bb  <- sf::st_bbox(shp)
      
      leaflet::leafletProxy("map_eta_e1", data=shp) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addPolygons(
          layerId  = ~COD_MUN5,
          fillColor= ~pal(valor),
          color    = BORDER_COL, weight = 0.4, fillOpacity = 0.9,
          label    = ~lapply(etq, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(color=BORDER_COL, weight=2, bringToFront=TRUE)
        ) %>%
        leaflet::addLegend("bottomright", colors = cols_legend, labels = labels_legend, opacity = 0.9, title = titulo) %>%
        leaflet::fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  
  observeEvent(input$map_eta_e1_shape_click, {
    click <- input$map_eta_e1_shape_click
    req(click$id)
    if (nivel_e1() == "deptos") {
      cod <- sprintf("%02d", as.integer(click$id))
      updateSelectInput(session, "f_depto_e1", selected = cod)
      updateSelectInput(session, "f_mpio_e1",  selected = "Todos")
    } else {
      cod_mun <- sprintf("%05d", as.integer(click$id))
      updateSelectInput(session, "f_mpio_e1", selected = cod_mun)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_reset_e1, {
    updateSelectInput(session, "f_depto_e1", selected = SANTANDER_CODE)
    updateSelectInput(session, "f_mpio_e1",  selected = "Todos")
  })
  
  output$plot_origen_e1 <- renderPlotly({
    if (!length(origen_cols)) return(NULL)
    
    d <- base_e1() %>%
      dplyr::select(dplyr::all_of(origen_cols), TOTAL_ENF) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(origen_cols), names_to = "Origen", values_to = "flag") %>%
      dplyr::filter(flag == 1) %>%
      dplyr::group_by(Origen) %>%
      dplyr::summarise(Enfermos = sum(TOTAL_ENF, na.rm=TRUE), Casos = dplyr::n(), .groups="drop") %>%
      dplyr::mutate(
        Origen = dplyr::recode(
          Origen,
          "agua"       = "Agua",
          "alimentos"  = "Alimentos",
          "pers_pers"  = "Persona a persona",
          "cont_ambie" = "Contaminación ambiental",
          "otro"       = "Otro",
          "desconocid" = "Desconocido",
          .default     = Origen
        ),
        Enfermos_fmt = scales::number(Enfermos, big.mark=".", decimal.mark=",", accuracy = 1),
        hover_lab    = paste0(Origen, "<br>Enfermos: ", Enfermos_fmt, "<br>Casos: ", Casos)
      )
    
    plot_ly(
      d, x = ~Enfermos, y = ~reorder(Origen, Enfermos),
      type="bar", orientation="h",
      marker=list(color = BAR_COLOR),
      text=~Enfermos_fmt, textposition="inside",
      insidetextanchor="middle", textfont=list(color="white"),
      hovertext=~hover_lab, hoverinfo="text"
    ) %>%
      layout(
        xaxis=list(title="Número de enfermos"),
        yaxis=list(title=""),
        margin=list(l=10,r=40,b=40,t=10)
      )
  })
  
  output$top_mpios_e1 <- renderPlotly({
    titulo <- indic_lbl_e1()
    acc    <- if ((input$f_indic_e1 %||% "total_enf") == "incid") 0.1 else 1
    
    df <- agg_mpio_e1() %>%
      dplyr::mutate(MUN_LBL = title_case_es(as.character(MUN_N))) %>%
      dplyr::arrange(dplyr::desc(valor)) %>%
      dplyr::slice_head(n = 12) %>%
      dplyr::mutate(
        valor_fmt = scales::number(valor, big.mark=".", decimal.mark=",", accuracy = acc),
        hover_lab = paste0(MUN_LBL, "<br>", titulo, ": ", valor_fmt)
      )
    
    axis_title <- if ((input$f_indic_e1 %||% "total_enf") == "incid") {
      "Incidencia (casos por 100.000 habitantes)"
    } else "Número de enfermos"
    
    plot_ly(
      df, x = ~valor, y = ~reorder(MUN_LBL, valor),
      type="bar", orientation="h",
      marker=list(color = BAR_COLOR),
      text=~valor_fmt, textposition="inside",
      insidetextanchor="middle", textfont=list(color="white"),
      hovertext=~hover_lab, hoverinfo="text"
    ) %>%
      layout(
        xaxis=list(title=axis_title),
        yaxis=list(title=""),
        margin=list(l=10,r=40,b=40,t=10)
      )
  })
}

shinyApp(ui, server)

