# app_upra.R — UPRA FA (Total municipal) estilo BPAN (sin per cápita)
suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(leaflet); library(sf); library(dplyr); library(tidyr)
  library(scales); library(htmltools); library(plotly)
  library(stringi); library(readr); library(tibble)
})
options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

# ---------- Rutas ----------
local_data_dir <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/UPRA_FA/data"
app_root     <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
rel_data_dir <- file.path(app_root, "data")
data_dir <- if (dir.exists(rel_data_dir)) rel_data_dir else local_data_dir

ruta_upra      <- file.path(data_dir, "013_UPRA_FA_Proporcion FA_Total municipal.rds")
ruta_pob       <- file.path(data_dir, "051_DANE_Proyecciones_P_total.rds")
ruta_shp_mpios <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")
ruta_shp_dptos <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")

must_exist <- c(ruta_upra, ruta_pob, ruta_shp_mpios, ruta_shp_dptos)
miss <- must_exist[!file.exists(must_exist)]
if (length(miss)) stop("Faltan archivos. data_dir usado: ", data_dir, "\n", paste("-", miss, collapse = "\n"))
check_shp_parts <- function(shp){ b <- sub("\\.shp$", "", shp); req <- paste0(b, c(".shp",".dbf",".shx",".prj")); req[!file.exists(req)] }
miss_shp <- c(check_shp_parts(ruta_shp_mpios), check_shp_parts(ruta_shp_dptos))
if (length(miss_shp)) stop("Faltan componentes de shapefile:\n", paste("-", miss_shp, collapse = "\n"))

# ---------- Utils ----------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
norm_txt <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
NUP <- function(x) toupper(norm_txt(x))  # normaliza para empates sin tildes

# Title Case “a la española” con tildes
title_case_es <- function(x) {
  stopw <- c("de","del","la","las","los","y","e","o","u","en","a","al","por","para",
             "con","sin","sobre","entre","hasta","desde","contra","ante","tras",
             "que","el","su","un","una","unos","unas")
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(s)) return(s)
    toks <- strsplit(trimws(as.character(s)), "\\s+", perl = TRUE)[[1]]
    toks_out <- mapply(function(tok, idx_tok) {
      parts <- stringi::stri_split_regex(tok, "([-/])", omit_empty = FALSE, tokens_only = FALSE)[[1]]
      parts_out <- mapply(function(p, idx_part) {
        if (p %in% c("-", "/")) return(p)
        base <- tolower(p)
        prev_sep <- if (idx_part > 1) parts[idx_part - 1] %in% c("-", "/") else FALSE
        if (idx_tok == 1 || prev_sep || !(base %in% stopw)) {
          return(stringi::stri_trans_totitle(base, locale = "es"))
        } else {
          return(base)
        }
      }, parts, seq_along(parts), USE.NAMES = FALSE)
      paste0(parts_out, collapse = "")
    }, toks, seq_along(toks), USE.NAMES = FALSE)
    paste(toks_out, collapse = " ")
  }, character(1))
}

num_or_na <- function(x) suppressWarnings(readr::parse_number(as.character(x)))
pick_col <- function(df, primary, pattern){
  nms <- names(df); if (primary %in% nms) return(primary)
  alt <- nms[grepl(pattern, nms, ignore.case = TRUE)]; if (length(alt)) alt[1] else NA_character_
}
safe_pull <- function(df, col) if (!is.na(col) && col %in% names(df)) df[[col]] else NA

make_pal_bin <- function(values, palette = "Blues", n_bins = 6){
  vals <- suppressWarnings(as.numeric(values)); vals <- vals[is.finite(vals)]
  if (!length(vals)) vals <- 0
  qs <- stats::quantile(vals, probs = seq(0, 1, length.out = n_bins), na.rm = TRUE)
  qs <- unique(as.numeric(qs)); if (length(qs) < 3) qs <- pretty(vals, n = n_bins)
  bins <- sort(unique(c(min(vals, na.rm = TRUE), qs, max(vals, na.rm = TRUE))))
  leaflet::colorBin(palette, domain = vals, bins = bins, na.color = "#f0f0f0")
}
fmt_pct <- function(x) ifelse(is.na(x), "NA", scales::percent(x, accuracy = 0.1))
fmt_num <- function(x, digs = 1) ifelse(is.na(x), "NA", scales::comma(x, big.mark = ","))

# Paletas
PALETA_VERDE <- c("#e5f5e0","#a1d99b","#74c476","#31a354","#006d2c")
COLOR_RANK   <- "#007a3d"
COLOR_BORDE  <- "#99d5ec"

# ---------- Cargar UPRA + población ----------
upra_raw <- readRDS(ruta_upra)
pob_raw  <- readRDS(ruta_pob)

col_fecha_u   <- pick_col(upra_raw, "fecha_completa", "fecha|date")
col_ano_u     <- pick_col(upra_raw, "ano", "^a(n|ñ)o$")
col_mes_u     <- pick_col(upra_raw, "mes", "mes")
col_dep_cod_u <- pick_col(upra_raw, "COD_DANE_DPTO_D", "DPTO|DEPTO|DANE.*DEP|COD.*DEP|DEPART")
col_dep_nom_u <- pick_col(upra_raw, "DEPARTAMENTO_D", "DEPARTA")
col_mun_cod_u <- pick_col(upra_raw, "COD_DANE_MUNIC_D", "MUNI.*COD|COD.*MUNI|DANE.*MUNI")
col_mun_nom_u <- pick_col(upra_raw, "MUNICIPIO_D", "MUNICIP")
col_area_mpio <- pick_col(upra_raw, "area_mpio_ha", "area.*mpio|mpio.*ha|area.*municip")
col_area_fa   <- pick_col(upra_raw, "area_fa_ha", "area.*fa.*ha|fa.*ha")
col_prop_fa   <- pick_col(upra_raw, "prop_fa", "prop.*fa|porc.*fa|particip.*fa")

DEPARTAMENTO_RAW <- safe_pull(upra_raw, col_dep_nom_u)
MUNICIPIO_RAW    <- safe_pull(upra_raw, col_mun_nom_u)

upra <- tibble(
  fecha_completa = safe_pull(upra_raw, col_fecha_u),
  ano            = suppressWarnings(as.integer(safe_pull(upra_raw, col_ano_u))),
  mes            = suppressWarnings(as.integer(safe_pull(upra_raw, col_mes_u))),
  COD_DANE_DPTO  = norm_txt(safe_pull(upra_raw, col_dep_cod_u)),
  DEPARTAMENTO   = norm_txt(DEPARTAMENTO_RAW),           # normalizado sin tildes
  DEPARTAMENTO_TC= title_case_es(DEPARTAMENTO_RAW),      # para mostrar
  COD_DANE_MUNI  = norm_txt(safe_pull(upra_raw, col_mun_cod_u)),
  MUNICIPIO_NORM = norm_txt(MUNICIPIO_RAW),
  MUNICIPIO_TC   = title_case_es(MUNICIPIO_RAW),
  area_mpio_ha   = num_or_na(safe_pull(upra_raw, col_area_mpio)),
  area_fa_ha     = num_or_na(safe_pull(upra_raw, col_area_fa)),
  prop_fa        = suppressWarnings(as.numeric(safe_pull(upra_raw, col_prop_fa)))
)
if (max(upra$prop_fa, na.rm = TRUE) > 1.5) upra$prop_fa <- upra$prop_fa / 100
upra$COD_DANE_MUNI <- sprintf("%05s", gsub("\\D", "", upra$COD_DANE_MUNI))

col_ano_p   <- pick_col(pob_raw, "ano", "^a(n|ñ)o$|year")
col_mun_p   <- pick_col(pob_raw, "COD_DANE_MUNIC_D", "MUNI.*COD|COD.*MUNI|DANE.*MUNI|MPIO|CODMUN|COD_MUN5")
col_pob_tot <- pick_col(pob_raw, "poblacion", "poblaci(o|ó)n|total|p_total|POB|pob$")

pob <- tibble(
  ano           = suppressWarnings(as.integer(safe_pull(pob_raw, col_ano_p))),
  COD_DANE_MUNI = sprintf("%05s", gsub("\\D", "", norm_txt(safe_pull(pob_raw, col_mun_p)))),
  pob_total     = suppressWarnings(as.numeric(safe_pull(pob_raw, col_pob_tot)))
) %>% group_by(ano, COD_DANE_MUNI) %>%
  summarise(pob_total = sum(pob_total, na.rm = TRUE), .groups = "drop")

base_upra <- upra %>%
  left_join(pob, by = c("ano", "COD_DANE_MUNI")) %>%
  mutate(
    DEPARTAMENTO_TC = ifelse(is.na(DEPARTAMENTO_TC) | !nzchar(DEPARTAMENTO_TC),
                             title_case_es(DEPARTAMENTO_RAW %||% DEPARTAMENTO), DEPARTAMENTO_TC),
    MUNICIPIO_TC = ifelse(is.na(MUNICIPIO_TC) | !nzchar(MUNICIPIO_TC),
                          title_case_es(MUNICIPIO_RAW %||% MUNICIPIO_NORM), MUNICIPIO_TC)
  )

# ---------- Shapes ----------
mpios_raw <- sf::st_read(ruta_shp_mpios, quiet = TRUE)
dptos_raw <- sf::st_read(ruta_shp_dptos, quiet = TRUE)

mpios_sf <- mpios_raw %>%
  mutate(
    COD_MUN5 = if ("MPIO_CDPMP" %in% names(.)) sprintf("%05d", as.integer(MPIO_CDPMP))
    else if ("COD_MPIO" %in% names(.)) sprintf("%05d", as.integer(COD_MPIO))
    else stop("Shp municipios: falta MPIO_CDPMP/COD_MPIO"),
    COD_DPTO2   = substr(COD_MUN5, 1, 2),
    MUNICIPIO_N = if ("MPIO_CNMBR" %in% names(.)) as.character(MPIO_CNMBR)
    else if ("NOMBRE_MPIO" %in% names(.)) as.character(NOMBRE_MPIO)
    else "MUNICIPIO"
  ) %>% st_transform(4326) %>% st_make_valid()

dptos_sf <- dptos_raw %>%
  mutate(
    COD_DPTO2 = if ("DPTO_CCDGO" %in% names(.)) sprintf("%02d", as.integer(DPTO_CCDGO))
    else if ("COD_DEPTO" %in% names(.)) sprintf("%02d", as.integer(COD_DEPTO))
    else stop("Shp deptos: falta DPTO_CCDGO/COD_DEPTO"),
    DEPARTAMENTO_N  = if ("DEPARTAMENTO_D" %in% names(.)) as.character(DEPARTAMENTO_D)
    else if ("DPTO_CNMBR" %in% names(.)) as.character(DPTO_CNMBR)
    else if ("NOMBRE_DEPTO" %in% names(.)) as.character(NOMBRE_DEPTO)
    else COD_DPTO2,
    DEPARTAMENTO_TC = title_case_es(DEPARTAMENTO_N)
  ) %>% st_transform(4326) %>% st_make_valid()

# Lookups
dpt_lookup <- base_upra %>%
  select(COD_DANE_DPTO, DEPARTAMENTO, DEPARTAMENTO_TC) %>%
  mutate(
    COD_DPTO2 = sprintf("%02d", as.integer(COD_DANE_DPTO)),
    DEP_NORM  = NUP(DEPARTAMENTO),
    DEP_TC    = DEPARTAMENTO_TC
  ) %>% distinct()

dptos_sf$DEP_NORM_SHP <- NUP(dptos_sf$DEPARTAMENTO_N)

# ---------- UI ----------
ui <- fluidPage(
  theme = bs_theme(
    version = 5, primary = "#2563eb",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius" = "0.9rem", "font-size-base" = "0.98rem"
  ),
  tags$head(tags$style(HTML("
    :root{ --accent-border:#99d5ec; }
    .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
    h3{font-weight:700;letter-spacing:.2px;margin-bottom:8px}
    .data-note{font-size:13px;color:#6b7280;margin:0 0 16px}
    .filters,.card{
      background:#fff;border:1px solid var(--accent-border) !important;
      border-radius:16px;box-shadow:0 2px 10px rgba(0,0,0,.05);
      padding:12px;margin-bottom:12px;
    }
    .filters-grid{display:grid;grid-template-columns:repeat(6,minmax(180px,1fr));gap:12px}
    .filter-label{font-size:12px;font-weight:600;letter-spacing:.4px;text-transform:uppercase;color:#6b7280;margin-bottom:6px}
    .selectize-input,.form-control,.form-select{
      min-height:42px;border-radius:10px;border:1px solid var(--accent-border) !important;
    }
    .selectize-input:focus,.form-control:focus,.form-select:focus{
      border-color:var(--accent-border) !important; box-shadow:0 0 0 .2rem rgba(153,213,236,.25) !important;
    }
    .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}
    .leaflet-control, .leaflet-control .legend, .leaflet-control .info{ border:1px solid var(--accent-border) !important; border-radius:12px; }
  "))),
  div(class="wrap",
      h3("UPRA — Frontera Agropecuaria"),
      div(class="data-note", HTML("Exploración de porcentaje y área de FA a nivel municipal / departamental.")),
      tabsetPanel(
        id = "tabs_upra", type = "tabs",
        tabPanel(
          "Exploración FA", br(),
          div(class="filters",
              div(class="filters-grid",
                  div(class="filter", div(class="filter-label","Seleccione Año:"), uiOutput("anio_ui")),
                  div(class="filter", div(class="filter-label","Seleccione Departamento:"),
                      selectInput("f_dep", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Seleccione Municipio:"),
                      selectInput("f_mun", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Seleccione Indicador:"),
                      selectInput("f_ind", NULL, choices = c("Porcentaje FA (%)" = "prop_fa",
                                                             "Área FA (ha)"     = "area_fa_ha"),
                                  selected = "prop_fa")),
                  div(class="filter", div(class="filter-label","Filtro por porcentaje FA"),
                      shinyWidgets::sliderTextInput(
                        inputId = "f_prop",
                        label   = NULL,
                        choices = sprintf("%.1f%%", seq(0, 100, by = 0.1)),
                        selected = c("0.0%", "100.0%"),
                        grid = TRUE,
                        dragRange = TRUE
                      )
                  ),
                  div(class="filter", div(class="filter-label","Acción"),
                      tagList(
                        actionLink("btn_reset","← Limpiar filtros"), br(),
                        actionLink("btn_back_co","⤺ Volver a Colombia")
                      ))
              )
          ),
          fluidRow(
            column(6,
                   div(class="card",
                       div(class="card-title", textOutput("map_title")),
                       leafletOutput("map_upra", height = 700)
                   )
            ),
            column(6,
                   div(class="card", div(class="card-title", textOutput("title_top")),
                       plotlyOutput("bar_top", height = 310)),
                   div(class="card", div(class="card-title", textOutput("title_depto")),
                       plotlyOutput("bar_depto", height = 310))
            )
          )
        )
      )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session){
  
  # Helpers de selects
  safe_choices <- function(x) {
    x <- unique(as.character(x))
    x[!is.na(x) & nzchar(x)]
  }
  update_select_clean <- function(inputId, choices, selected = NULL) {
    ch <- safe_choices(choices)
    if (length(ch) == 0) ch <- character(0)
    ch_full <- c("Todos", ch)
    sel <- if (!is.null(selected) && selected %in% ch_full) selected else "Todos"
    updateSelectInput(session, inputId, choices = ch_full, selected = sel)
  }
  
  # Convierte el slider de "% con 1 decimal" a rango [0,1]
  prop_range <- reactive({
    req(input$f_prop)
    v <- as.numeric(gsub("%", "", input$f_prop))
    c(v[1], v[2]) / 100
  })
  
  empty_plot <- function(texto = "Sin datos para los filtros seleccionados.") {
    plotly::plotly_empty(type = "scatter", mode = "markers") %>%
      plotly::layout(
        annotations = list(x = 0.5, y = 0.5, text = texto, showarrow = FALSE,
                           xref = "paper", yref = "paper", font = list(size = 14)),
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        margin = list(l=10, r=10, b=10, t=10)
      )
  }
  
  # Año UI
  output$anio_ui <- renderUI({
    yrs <- sort(unique(na.omit(base_upra$ano)))
    selectInput("anio", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  # Títulos reactivos
  ind_label <- reactive({
    switch(input$f_ind,
           "prop_fa"    = "Porcentaje FA",
           "area_fa_ha" = "Área FA (ha)")
  })
  output$map_title <- renderText({
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      "¿Qué departamentos tienen más tierra disponible para la agricultura?"
    } else {
      paste0("¿Qué municipios de ", input$f_dep, " tienen más tierra disponible para la agricultura?")
    }
  })
  output$title_top <- renderText({
    if (is.null(input$f_dep) || input$f_dep == "Todos")
      "Top-10 municipios por área de FA (ha) — Colombia"
    else
      paste0("Top-10 municipios por área de FA (ha) — ", input$f_dep)
  })
  output$title_depto <- renderText({
    if (input$f_ind == "prop_fa") {
      "Porcentaje de FA por departamento"
    } else {
      "Promedio de Área FA (ha) por departamento"
    }
  })
  
  # Combos dependientes (usar *_TC para mostrar con tildes)
  observeEvent(input$anio, {
    df <- base_upra %>% dplyr::filter(ano == input$anio)
    deps_tc <- df %>% dplyr::pull(DEPARTAMENTO_TC) %>% safe_choices() %>% sort()
    sel_dep <- if (!is.null(input$f_dep) && input$f_dep %in% c("Todos", deps_tc)) input$f_dep else "Todos"
    update_select_clean("f_dep", deps_tc, selected = sel_dep)
    update_select_clean("f_mun", character(0), selected = "Todos")
  }, ignoreInit = FALSE)
  
  observeEvent(input$f_dep, {
    df <- base_upra %>% dplyr::filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") {
      mpios_tc <- df %>%
        dplyr::filter(DEPARTAMENTO_TC == input$f_dep) %>%
        dplyr::pull(MUNICIPIO_TC) %>%
        safe_choices() %>% sort()
      update_select_clean("f_mun", mpios_tc, selected = "Todos")
    } else {
      update_select_clean("f_mun", character(0), selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_reset, {
    yrs <- sort(unique(na.omit(base_upra$ano)))
    updateSelectInput(session, "anio", selected = max(yrs, na.rm = TRUE))
    update_select_clean("f_dep", character(0), selected = "Todos")
    update_select_clean("f_mun", character(0), selected = "Todos")
    updateSelectInput(session, "f_ind", selected = "prop_fa")
    shinyWidgets::updateSliderTextInput(session, "f_prop", selected = c("0.0%", "100.0%"))
  })
  observeEvent(input$btn_back_co, { updateSelectInput(session, "f_dep", selected = "Todos") })
  
  # Base filtrada
  base_filtrada <- reactive({
    req(input$anio)
    d <- base_upra %>% dplyr::filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") d <- d %>% dplyr::filter(DEPARTAMENTO_TC == input$f_dep)
    if (!is.null(input$f_mun) && input$f_mun != "Todos") d <- d %>% dplyr::filter(MUNICIPIO_TC == input$f_mun)
    rng <- prop_range()
    d %>% dplyr::filter(prop_fa >= rng[1], prop_fa <= rng[2])
  })
  
  # Mapa base
  output$map_upra <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(-74.3, 5, 5.45)
  })
  
  # Helpers para mapa
  get_cod_from_dep_name <- function(dep_name){
    if (is.null(dep_name) || dep_name == "Todos") return(NA_character_)
    dep_norm <- NUP(dep_name)
    i <- which(dptos_sf$DEP_NORM_SHP == dep_norm)[1]
    if (is.finite(i)) return(dptos_sf$COD_DPTO2[i])
    j <- which(dpt_lookup$DEP_NORM == dep_norm)[1]
    if (is.finite(j)) return(dpt_lookup$COD_DPTO2[j])
    NA_character_
  }
  map_shp_dep_to_base <- function(nom_shp) {
    dep_norm <- NUP(nom_shp)
    dep_tc <- dpt_lookup$DEP_TC[match(dep_norm, dpt_lookup$DEP_NORM)]
    ifelse(is.na(dep_tc) | !nzchar(dep_tc), title_case_es(nom_shp), dep_tc)
  }
  
  # Mapa (deptos -> mpios)
  observe({
    d <- base_filtrada()
    if (is.null(d) || nrow(d) == 0) {
      leafletProxy("map_upra") %>% clearShapes() %>% clearControls()
      return(invisible(NULL))
    }
    ind <- input$f_ind
    label <- if (ind == "prop_fa") "Porcentaje FA" else "Área FA (ha)"
    
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      dd <- d %>% dplyr::group_by(COD_DANE_DPTO, DEPARTAMENTO_TC) %>%
        dplyr::summarise(
          valor = if (ind == "area_fa_ha") sum(.data[[ind]], na.rm=TRUE) else mean(.data[[ind]], na.rm=TRUE),
          .groups="drop"
        ) %>% dplyr::mutate(COD_DPTO2 = sprintf("%02d", as.integer(COD_DANE_DPTO)))
      
      shp <- dptos_sf
      idx <- match(shp$COD_DPTO2, dd$COD_DPTO2)
      shp$valor <- dd$valor[idx]
      shp$DEPARTAMENTO_TC <- dd$DEPARTAMENTO_TC[idx]
      shp$nombre <- dplyr::coalesce(shp$DEPARTAMENTO_TC, title_case_es(shp$DEPARTAMENTO_N))
      shp$etq <- paste0(
        "<b>", shp$nombre, "</b><br>", label, ": ",
        ifelse(ind=="prop_fa", fmt_pct(shp$valor), fmt_num(shp$valor, 1))
      )
      
      pal <- make_pal_bin(shp$valor, palette = PALETA_VERDE, n_bins = length(PALETA_VERDE))
      leafletProxy("map_upra", data = shp) %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(layerId=~COD_DPTO2, fillColor=~pal(valor), color=COLOR_BORDE, weight=0.7, fillOpacity=0.9,
                    label=~lapply(etq, HTML),
                    highlightOptions = highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        addLegend("bottomright", pal = pal, values = ~valor, title = label)
      
    } else {
      sel_cod <- get_cod_from_dep_name(input$f_dep)
      if (is.na(sel_cod) || !nzchar(sel_cod)) {
        sel_cod <- d$COD_DANE_DPTO %>% unique() %>% sprintf("%02d", as.integer(.)) %>% .[1]
      }
      dd <- d %>%
        dplyr::mutate(COD_MUN5 = sprintf("%05d", as.integer(COD_DANE_MUNI))) %>%
        dplyr::group_by(COD_DANE_MUNI, MUNICIPIO_TC, DEPARTAMENTO_TC) %>%
        dplyr::summarise(
          valor = if (ind == "area_fa_ha") sum(.data[[ind]], na.rm=TRUE) else mean(.data[[ind]], na.rm=TRUE),
          area_fa_ha = sum(area_fa_ha, na.rm = TRUE),
          prop_fa = mean(prop_fa, na.rm = TRUE),
          pob_total = mean(pob_total, na.rm = TRUE),
          .groups = "drop"
        )
      
      shp <- mpios_sf %>% dplyr::filter(COD_DPTO2 == sel_cod)
      idx <- match(shp$COD_MUN5, sprintf("%05d", as.integer(dd$COD_DANE_MUNI)))
      shp$valor <- dd$valor[idx]
      shp$MUNICIPIO_TC <- dd$MUNICIPIO_TC[idx]
      shp$area_fa_ha <- dd$area_fa_ha[idx]
      shp$prop_fa <- dd$prop_fa[idx]
      shp$pob_total <- dd$pob_total[idx]
      shp$DEP_TC <- dd$DEPARTAMENTO_TC[idx]
      
      shp$MUNICIPIO_MOSTRAR <- dplyr::coalesce(shp$MUNICIPIO_TC, title_case_es(shp$MUNICIPIO_N))
      shp$DEP_MOSTRAR <- dplyr::coalesce(shp$DEP_TC, "")
      
      shp$etq <- paste0(
        "<b>", shp$MUNICIPIO_MOSTRAR, " (", shp$DEP_MOSTRAR, ")</b><br>", label, ": ",
        ifelse(ind=="prop_fa", fmt_pct(shp$valor), fmt_num(shp$valor, 1)),
        "<br>Área FA (ha): ", fmt_num(shp$area_fa_ha, 1),
        "<br>Porcentaje FA: ", fmt_pct(shp$prop_fa),
        "<br>Población: ", fmt_num(shp$pob_total, 1)
      )
      
      pal <- make_pal_bin(shp$valor, palette = PALETA_VERDE, n_bins = length(PALETA_VERDE))
      bb <- sf::st_bbox(shp)
      
      leafletProxy("map_upra", data = shp) %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(layerId=~COD_MUN5, fillColor=~pal(valor), color=COLOR_BORDE, weight=0.4, fillOpacity=0.9,
                    label=~lapply(etq, HTML),
                    highlightOptions = highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        addLegend("bottomright", pal = pal, values = ~valor, title = label) %>%
        fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  
  # Drill-down por click (mantener Title Case en el combo)
  observeEvent(input$map_upra_shape_click, {
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      click <- input$map_upra_shape_click; req(click$id)
      cod <- sprintf("%02d", as.integer(click$id))
      nom_shp <- dptos_sf$DEPARTAMENTO_N[match(cod, dptos_sf$COD_DPTO2)]
      nom_combo_tc <- map_shp_dep_to_base(nom_shp)
      if (!is.na(nom_combo_tc) && nzchar(nom_combo_tc)) updateSelectInput(session, "f_dep", selected = nom_combo_tc)
    }
  }, ignoreInit = TRUE)
  
  # Top-10 municipios — área FA (ha) (se mantiene en ha)
  output$bar_top <- renderPlotly({
    d <- base_filtrada()
    if (is.null(d) || NROW(d) == 0) return(empty_plot())
    
    d1 <- d %>%
      dplyr::group_by(DEPARTAMENTO_TC, MUNICIPIO_TC) %>%
      dplyr::summarise(val = sum(area_fa_ha, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(val)) %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::mutate(lbl = paste0(MUNICIPIO_TC, " (", DEPARTAMENTO_TC, ")"))
    
    if (!NROW(d1) || all(!is.finite(d1$val))) return(empty_plot())
    
    plotly::plot_ly(d1 %>% dplyr::arrange(val),
                    x = ~val,
                    y = ~factor(lbl, levels = d1$lbl[order(d1$val)]),
                    type = "bar", orientation = "h",
                    marker = list(color = COLOR_RANK),
                    hovertemplate = "%{y}<br>Área FA: %{x:,.0f} ha<extra></extra>"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Área FA (ha)"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l=10, r=10, b=10, t=10)
      )
  })
  
  # Promedio por dpto — si es porcentaje, ejes/hover en %
  output$bar_depto <- renderPlotly({
    d <- base_filtrada()
    if (is.null(d) || NROW(d) == 0) return(empty_plot())
    
    ind <- input$f_ind
    d2 <- d %>%
      dplyr::group_by(DEPARTAMENTO_TC) %>%
      dplyr::summarise(val = mean(.data[[ind]], na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(val)) %>%
      dplyr::slice_head(n = 15)
    
    if (!NROW(d2) || all(!is.finite(d2$val))) return(empty_plot())
    
    hover_tpl <- if (ind == "prop_fa") {
      "%{y}<br>Porcentaje: %{x:.1%}<extra></extra>"
    } else {
      "%{y}<br>Promedio: %{x:,.6f}<extra></extra>"
    }
    x_title <- if (ind == "prop_fa") "Porcentaje" else "Promedio del indicador"
    x_tickformat <- if (ind == "prop_fa") ".1%" else NULL
    
    plotly::plot_ly(d2 %>% dplyr::arrange(val),
                    x = ~val,
                    y = ~factor(DEPARTAMENTO_TC, levels = d2$DEPARTAMENTO_TC[order(d2$val)]),
                    type = "bar", orientation = "h",
                    marker = list(color = COLOR_RANK),
                    hovertemplate = hover_tpl
    ) %>%
      plotly::layout(
        xaxis = list(title = x_title, tickformat = x_tickformat),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l=10, r=10, b=10, t=10)
      )
  })
}

shinyApp(ui, server)


