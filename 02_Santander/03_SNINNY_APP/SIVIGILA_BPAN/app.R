# app_bpan.R
# =========================================================
# BPAN — Dashboard (app exclusiva) - SANTANDER
# BOTONES TIPO ICA: PNG + CSV + PDF via Rmarkdown
# CORREGIDO: descarga robusta del mapa PNG
# =========================================================

# ---------- Paquetes ----------
pkgs <- c(
  "shiny","bslib","shinyWidgets",
  "leaflet","sf","dplyr","tidyr","scales","htmltools","DT","plotly",
  "stringi","htmlwidgets","webshot2","rmarkdown","readr","ggplot2"
)

pkgs <- as.character(pkgs)
pkgs <- pkgs[!is.na(pkgs) & nzchar(pkgs)]
stopifnot(is.character(pkgs), length(pkgs) > 0)

suppressWarnings(invisible(lapply(pkgs, function(p) {
  suppressPackageStartupMessages(require(p, character.only = TRUE))
})))

options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)
options(shiny.maxRequestSize = 100 * 1024^2)

validate <- shiny::validate
need     <- shiny::need

# ---------- Colores globales ----------
MAP_COLORS <- c("#fff9db", "#ffe082", "#ffd54f", "#fbc02d", "#c49000")
BAR_COLOR  <- "#ffd54f"
BORDER_UI  <- "#ffb366"

# ---------- Utils ----------
norm_txt <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
NUP      <- function(x) toupper(norm_txt(x))

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

safe_utf8 <- function(x){
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x <- stringi::stri_enc_toutf8(x, is_unknown_8bit = TRUE)
  x
}

title_case_es <- function(x){
  x <- safe_utf8(x)
  x <- trimws(as.character(x))
  is_na <- is.na(x) | x == ""
  if (all(is_na)) return(x)

  x_ok <- x[!is_na]
  x_ok <- stringi::stri_trans_totitle(x_ok, locale = "es")

  conectores <- c(
    " De ", " Del ", " La ", " Las ", " Los ", " Y ", " E ", " O ", " U ",
    " En ", " Por ", " Para ", " Con ", " Sin ", " A ", " Al ", " El "
  )
  conectores_low <- tolower(conectores)

  for (i in seq_along(conectores)) {
    x_ok <- gsub(conectores[i], conectores_low[i], x_ok, fixed = TRUE)
  }

  x[!is_na] <- x_ok
  x
}

get_col <- function(df, opts, stop_msg){
  nm <- opts[opts %in% names(df)][1]
  if (is.na(nm) || !nzchar(nm)) stop(stop_msg) else nm
}

fmt_num <- function(x, accuracy = 1){
  scales::number(x, accuracy = accuracy, big.mark = ".", decimal.mark = ",")
}

fmt_km <- function(x){
  if (is.na(x) || !is.finite(x)) return(NA_character_)
  ax <- abs(x)
  if (ax >= 1e6) {
    paste0(fmt_num(x / 1e6, 0.1), " M")
  } else if (ax >= 1e3) {
    paste0(fmt_num(x / 1e3, 0.1), " K")
  } else {
    fmt_num(x, 1)
  }
}

zoom_from_bbox <- function(bb){
  w <- abs(as.numeric(bb["xmax"] - bb["xmin"]))
  h <- abs(as.numeric(bb["ymax"] - bb["ymin"]))
  span <- max(w, h)
  if (!is.finite(span)) return(7)
  if (span < 0.10) return(12)
  if (span < 0.20) return(11)
  if (span < 0.35) return(10)
  if (span < 0.80) return(9)
  if (span < 1.50) return(8)
  if (span < 3.00) return(7)
  6
}

get_app_root <- function(){
  normalizePath(shiny::getShinyOption("appDir") %||% getwd(), winslash = "/", mustWork = FALSE)
}

# ---------- Export ----------
app_root   <- get_app_root()
EXPORT_DIR <- file.path(app_root, "Descargas")
dir.create(EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)

ruta_rmd <- file.path(app_root, "Informe_descargable.Rmd")

PNG_VWIDTH    <- 3200
PNG_VHEIGHT   <- 2400
PNG_DELAY_CO  <- 3.0
PNG_DELAY_MUN <- 4.5

IMG_MAP <- file.path(EXPORT_DIR, "bpan_mapa.png")
IMG_SER <- file.path(EXPORT_DIR, "bpan_serie.png")
IMG_TOP <- file.path(EXPORT_DIR, "bpan_top10.png")

save_widget_png <- function(widget, out_png, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay = PNG_DELAY_CO){
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)

  tmp_dir  <- tempfile("wshot_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  tmp_html <- file.path(tmp_dir, "widget.html")

  htmlwidgets::saveWidget(
    widget,
    file = tmp_html,
    selfcontained = TRUE,
    background = "white"
  )

  html_url <- paste0(
    "file:///",
    gsub("\\\\", "/", normalizePath(tmp_html, winslash = "/", mustWork = TRUE))
  )

  if (file.exists(out_png)) unlink(out_png, force = TRUE)

  webshot2::webshot(
    url     = html_url,
    file    = out_png,
    vwidth  = vwidth,
    vheight = vheight,
    delay   = delay
  )

  for (i in 1:15) {
    if (file.exists(out_png)) {
      info <- file.info(out_png)
      if (is.finite(info$size) && info$size > 0) return(TRUE)
    }
    Sys.sleep(0.4)
  }

  FALSE
}

save_widget_png_retry <- function(widget, out_png, vwidth, vheight, delay_base){
  delays <- c(delay_base, delay_base + 2, delay_base + 4, delay_base + 6)
  for (d in delays){
    ok <- tryCatch(
      save_widget_png(widget, out_png, vwidth = vwidth, vheight = vheight, delay = d),
      error = function(e) {
        message("Error en save_widget_png con delay=", d, ": ", conditionMessage(e))
        FALSE
      }
    )
    if (isTRUE(ok)) return(TRUE)
  }
  FALSE
}

# ---------- Rutas ----------
local_data_dir <- "data"
rel_data_dir   <- file.path(app_root, "data")
data_dir       <- if (dir.exists(rel_data_dir)) rel_data_dir else local_data_dir

bpan_path      <- file.path(data_dir, "021_INS_SIVIGILA-BPAN.rds")
ruta_shp_mpios <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")
ruta_shp_dptos <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")

must_exist <- c(bpan_path, ruta_shp_mpios, ruta_shp_dptos)
miss <- must_exist[!file.exists(must_exist)]
if (length(miss)) stop("Faltan archivos. data_dir usado: ", data_dir, "\n", paste("-", miss, collapse = "\n"))

check_shp_parts <- function(shp){
  base <- sub("\\.shp$", "", shp)
  req  <- paste0(base, c(".shp",".dbf",".shx",".prj"))
  req[!file.exists(req)]
}
miss_shp <- c(check_shp_parts(ruta_shp_mpios), check_shp_parts(ruta_shp_dptos))
if (length(miss_shp)) stop("Faltan componentes de shapefile:\n", paste("-", miss_shp, collapse = "\n"))

# ---------- 1) Leer BPAN ----------
bpan_raw <- readRDS(bpan_path)

bpan_raw <- bpan_raw %>%
  dplyr::filter(toupper(trimws(as.character(DEPARTAMENTO_O))) == "SANTANDER")

b_year_col <- get_col(bpan_raw, c("ano","ANO","year","YEAR"), "BPAN: no 'ano'/'ANO'")

b_mun_code_col <- get_col(
  bpan_raw,
  c("COD_DANE_MUNIC_O","COD_DANE_MUNIC_D","COD_MUN5","COD_MPIO","MPIO_CDPMP"),
  "BPAN: no 'COD_DANE_MUNIC_O' (ni columnas equivalentes) para ocurrencia"
)

b_dep_origen_col <- get_col(bpan_raw, c("DEPARTAMENTO_O","DEPARTAMENTO"), "BPAN: no 'DEPARTAMENTO_O'")
b_mun_origen_col <- get_col(bpan_raw, c("MUNICIPIO_O","MUNICIPIO"), "BPAN: no 'MUNICIPIO_O'")
b_dep_code_o_col <- get_col(bpan_raw, c("COD_DANE_DPTO_O","COD_DPTO"), "BPAN: no 'COD_DANE_DPTO_O'")

edad_col    <- c("edad","EDAD")[c("edad","EDAD") %in% names(bpan_raw)][1]
pac_hos_col <- c("pac_hos","PAC_HOS")[c("pac_hos","PAC_HOS") %in% names(bpan_raw)][1]

bpan <- bpan_raw %>%
  dplyr::transmute(
    ano       = suppressWarnings(as.integer(.data[[b_year_col]])),
    COD_MUN5  = sprintf("%05d", suppressWarnings(as.integer(.data[[b_mun_code_col]]))),
    COD_DPTO2 = if (!is.na(b_dep_code_o_col)) {
      sprintf("%02d", suppressWarnings(as.integer(.data[[b_dep_code_o_col]])))
    } else {
      substr(sprintf("%05d", suppressWarnings(as.integer(.data[[b_mun_code_col]]))), 1, 2)
    },
    DEP     = title_case_es(.data[[b_dep_origen_col]]),
    MUN     = title_case_es(.data[[b_mun_origen_col]]),
    edad    = if (!is.na(edad_col)) suppressWarnings(as.numeric(.data[[edad_col]])) else NA_real_,
    PAC_HOS = if (!is.na(pac_hos_col)) suppressWarnings(as.integer(.data[[pac_hos_col]])) else NA_integer_
  ) %>%
  dplyr::filter(
    !is.na(ano),
    !is.na(COD_MUN5),
    !is.na(COD_DPTO2),
    !is.na(MUN),
    !is.na(DEP)
  )

bpan <- bpan %>%
  dplyr::mutate(DEP = ifelse(toupper(DEP) %in% c("SANTANDER"), "Santander", DEP)) %>%
  dplyr::filter(DEP == "Santander")

DEPS_ALL <- "Santander"
SANTANDER_DEFAULT <- "Santander"

# ---------- 2) Shapes ----------
mpios_raw <- sf::st_read(ruta_shp_mpios, quiet = TRUE)
dptos_raw <- sf::st_read(ruta_shp_dptos, quiet = TRUE)

mpios_sf <- mpios_raw %>%
  dplyr::mutate(
    COD_MUN5 = if ("MPIO_CDPMP" %in% names(.)) sprintf("%05d", as.integer(MPIO_CDPMP))
    else if ("COD_MPIO" %in% names(.)) sprintf("%05d", as.integer(COD_MPIO))
    else stop("Shp municipios: falta MPIO_CDPMP/COD_MPIO"),
    COD_DPTO2 = substr(COD_MUN5, 1, 2),
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

# ---------- Helper paleta con cuartiles ----------
make_quartile_pal <- function(values) {
  vals_pos <- values[values > 0 & is.finite(values)]

  if (length(vals_pos) < 4) {
    if (length(vals_pos) == 0) {
      bins <- c(0, 0.5, 1)
      colors <- MAP_COLORS[3:5]
    } else if (length(vals_pos) == 1) {
      bins <- c(0, vals_pos[1], vals_pos[1] + 0.5)
      colors <- MAP_COLORS[3:5]
    } else if (length(vals_pos) == 2) {
      bins <- c(0, min(vals_pos), max(vals_pos), max(vals_pos) + 1)
      colors <- MAP_COLORS[2:5]
    } else if (length(vals_pos) == 3) {
      bins <- c(0, sort(vals_pos), max(vals_pos) + 1)
      colors <- MAP_COLORS[2:5]
    }
  } else {
    q1 <- quantile(vals_pos, 0.25, na.rm = TRUE)
    q2 <- quantile(vals_pos, 0.50, na.rm = TRUE)
    q3 <- quantile(vals_pos, 0.75, na.rm = TRUE)
    max_val <- max(vals_pos, na.rm = TRUE)
    bins <- c(0, q1, q2, q3, max_val)
    bins <- unique(bins)

    if (length(bins) == 2) {
      bins <- c(0, max_val / 2, max_val)
      colors <- MAP_COLORS[3:5]
    } else if (length(bins) == 3) {
      colors <- MAP_COLORS[2:4]
    } else if (length(bins) == 4) {
      colors <- MAP_COLORS[2:5]
    } else {
      colors <- MAP_COLORS
    }
  }

  bins <- sort(bins)

  leaflet::colorBin(
    palette  = colors,
    domain   = values,
    bins     = bins,
    na.color = "#f0f0f0"
  )
}

quartile_lab_format <- function(type, cuts) {
  if (type != "bin") return(cuts)
  n <- length(cuts)
  if (n < 2) return(cuts)
  labels <- character(n - 1)
  for (i in 1:(n - 1)) {
    lower <- cuts[i]
    upper <- cuts[i + 1]
    if (i == 1 && lower <= 0) {
      labels[i] <- paste0("0 – ", scales::comma(upper, accuracy = 1))
    } else {
      labels[i] <- paste0(">", scales::comma(lower, accuracy = 1), " – ", scales::comma(upper, accuracy = 1))
    }
  }
  labels
}

# ---------- 3) UI ----------
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    primary = "#2563eb",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius" = "0.9rem",
    "font-size-base" = "0.98rem"
  ),
  tags$head(
    tags$style(HTML(sprintf("
      :root{ --border-col:%s; }

      .wrap{
        max-width:1360px;
        margin:0 auto;
        padding:16px 20px 32px;
      }
      h3{
        font-weight:700;
        letter-spacing:.2px;
        margin-bottom:8px;
      }
      .data-note{
        font-size:13px;
        color:#6b7280;
        margin:0 0 16px;
      }

      .filters{
        background:#fff;
        border:1.5px solid var(--border-col);
        border-radius:16px;
        padding:14px 16px;
        margin-bottom:16px;
        box-shadow:0 2px 10px rgba(0,0,0,.04);
      }

      .filters-grid{
        display:grid;
        grid-template-columns: repeat(auto-fit, minmax(180px,1fr));
        gap:12px;
      }

      .filter{
        display:flex;
        flex-direction:column;
      }

      .filter-label{
        font-family: 'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-size:14px;
        font-weight:500;
        letter-spacing:.2px;
        color:#000000;
        margin-bottom:6px;
        min-height:32px;
        display:flex;
        align-items:flex-end;
      }

      .selectize-input,.form-control{
        min-height:42px;
        border-radius:10px;
        border:1.5px solid var(--border-col);
        box-shadow:none !important;
      }
      .selectize-input:focus,.form-control:focus{
        border-color:var(--border-col)!important;
        outline:0 !important;
        box-shadow:0 0 0 .15rem rgba(255,179,102,.35)!important;
      }
      .selectize-dropdown{
        border-color:var(--border-col)!important;
      }

      .card{
        background:#fff;
        border:1.5px solid var(--border-col);
        border-radius:16px;
        padding:12px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
        margin-bottom:12px;
      }
      .card-title {
        font-family: 'Inter Tight', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-size: 16px;
        font-weight: 700;
        color: #111827;
        margin-bottom: 8px;
      }

      .series-plot-container {
        height: 340px;
        width: 100%%;
      }

      .map-note{
        font-size:12px;
        color:#6b7280;
        margin-top:6px;
      }

      .btn-unified{
        background:#ffffff !important;
        border:1px solid var(--border-col) !important;
        color:#374151 !important;
        font-weight:700 !important;
        border-radius:12px !important;
        padding:6px 10px !important;
        font-size:12px !important;
      }

      .footer-actions{
        margin-top:10px;
        display:flex;
        justify-content:flex-end;
        gap:8px;
        padding:6px 6px 0;
        flex-wrap:wrap;
      }
    ", BORDER_UI)))
  ),
  div(
    class = "wrap",
    h3(""),
    div(class = "data-note", HTML("")),
    div(
      class = "filters",
      div(
        class = "filters-grid",
        div(
          class = "filter",
          div(class = "filter-label", "¿Qué año analizamos?"),
          uiOutput("anio_bpan_ui")
        ),
        div(
          class = "filter",
          div(class = "filter-label", "¿En qué departamento?"),
          selectInput(
            "f_depto", NULL,
            choices  = DEPS_ALL,
            selected = SANTANDER_DEFAULT
          )
        ),
        div(
          class = "filter",
          div(class = "filter-label", "¿Algún municipio en particular?"),
          selectInput("f_mpio", NULL, choices = "Todos", selected = "Todos")
        ),
        div(
          class = "filter",
          div(class = "filter-label", "Acción"),
          actionLink("btn_reset_bpan","← Limpiar filtros")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        div(
          class = "card",
          div(
            class = "card-title d-flex justify-content-between align-items-center",
            span(textOutput("ttl_map_tab1", inline = TRUE)),
            downloadButton("dl_png_mapa_bpan","Descargar PNG", class = "btn-unified")
          ),
          leafletOutput("map_bpan", height = 720),
          div(
            class = "map-note",
            "Nota: El mapa clasifica los valores del indicador en cuartiles (cuatro grupos con igual número de observaciones)."
          )
        )
      ),
      column(
        width = 6,
        div(
          class = "card",
          div(
            class = "card-title d-flex justify-content-between align-items-center",
            span("¿Cómo ha evolucionado los casos de bajo peso al nacer (BPAN) por año?"),
            downloadButton("dl_png_serie_bpan","Descargar PNG", class = "btn-unified")
          ),
          div(class = "series-plot-container",
              plotlyOutput("serie_temporal", height = 330))
        ),
        div(
          class = "card",
          div(
            class = "card-title d-flex justify-content-between align-items-center",
            span(textOutput("ttl_top10_tab1", inline = TRUE)),
            downloadButton("dl_png_top_bpan","Descargar PNG", class = "btn-unified")
          ),
          plotlyOutput("bar_bpan", height = 350)
        )
      )
    ),
    div(
      class = "footer-actions",
      downloadButton("dl_csv_expl_bpan","Descargar CSV", class = "btn-unified"),
      downloadButton("dl_reporte_pdf_bpan","Descargar informe (PDF)", class = "btn-unified")
    )
  )
)

# ---------- 4) SERVER ----------
server <- function(input, output, session){

  output$anio_bpan_ui <- renderUI({
    yrs <- sort(unique(bpan$ano))
    selectInput("f_anio_bpan", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })

  update_mpio_choices <- function(selected = NULL) {
    req(input$f_anio_bpan, input$f_depto)

    df <- bpan %>%
      dplyr::filter(ano == input$f_anio_bpan, DEP == input$f_depto)

    mpios_o <- df %>%
      dplyr::distinct(MUN) %>%
      dplyr::arrange(MUN) %>%
      dplyr::pull(MUN)

    sel <- selected %||% input$f_mpio %||% "Todos"
    if (!sel %in% mpios_o) sel <- "Todos"

    updateSelectInput(
      session, "f_mpio",
      choices  = c("Todos", mpios_o),
      selected = sel
    )
  }

  observeEvent(input$btn_reset_bpan, {
    yrs <- sort(unique(bpan$ano))
    ysel <- max(yrs, na.rm = TRUE)

    updateSelectInput(session, "f_anio_bpan", selected = ysel)
    updateSelectInput(session, "f_depto", selected = SANTANDER_DEFAULT)

    isolate({
      update_mpio_choices(selected = "Todos")
    })
  })

  observeEvent(input$f_anio_bpan, {
    req(input$f_anio_bpan)

    df_year <- bpan %>% dplyr::filter(ano == input$f_anio_bpan)

    deps_o <- df_year %>%
      dplyr::distinct(DEP) %>%
      dplyr::arrange(DEP) %>%
      dplyr::pull(DEP)

    sel_dep <- if (!is.null(input$f_depto) && input$f_depto %in% deps_o) input$f_depto else deps_o[1]

    updateSelectInput(session, "f_depto", choices = deps_o, selected = sel_dep)

    isolate({
      update_mpio_choices(selected = "Todos")
    })
  }, ignoreInit = FALSE)

  observeEvent(input$f_depto, {
    req(input$f_anio_bpan, input$f_depto)
    update_mpio_choices()
  }, ignoreInit = FALSE)

  bpan_base <- reactive({
    req(input$f_anio_bpan)
    df <- bpan %>% dplyr::filter(ano == input$f_anio_bpan)
    if (!is.null(input$f_depto))
      df <- df %>% dplyr::filter(DEP == input$f_depto)
    if (!is.null(input$f_mpio) && input$f_mpio != "Todos")
      df <- df %>% dplyr::filter(MUN == input$f_mpio)
    validate(need(nrow(df) > 0, "Sin datos para los filtros actuales"))
    df
  })

  bpan_serie_completa <- reactive({
    df <- bpan
    if (!is.null(input$f_depto))
      df <- df %>% dplyr::filter(DEP == input$f_depto)
    if (!is.null(input$f_mpio) && input$f_mpio != "Todos")
      df <- df %>% dplyr::filter(MUN == input$f_mpio)
    validate(need(nrow(df) > 0, "Sin datos para los filtros actuales"))
    df
  })

  sel_cod_dep <- reactive({ "68" })

  output$ttl_map_tab1 <- renderText({
    if (is.null(input$f_anio_bpan)) return("")
    paste0("¿Dónde se concentran los casos de bajo peso al nacer (BPAN) en Santander en ", input$f_anio_bpan, "?")
  })

  output$ttl_top10_tab1 <- renderText({
    if (is.null(input$f_anio_bpan)) return("")
    paste0("Top 10 municipios con más casos de bajo peso al nacer (BPAN) en ", input$f_anio_bpan)
  })

  bpan_agg_mpio <- reactive({
    bpan_base() %>%
      dplyr::group_by(COD_DPTO2, COD_MUN5, MUN) %>%
      dplyr::summarise(valor = dplyr::n(), .groups = "drop")
  })

  bbox_actual <- reactive({
    sel_cod <- sel_cod_dep()

    if (!is.null(input$f_mpio) && input$f_mpio != "Todos") {
      cod_sel <- bpan %>%
        dplyr::filter(DEP == input$f_depto, MUN == input$f_mpio) %>%
        dplyr::distinct(COD_MUN5) %>%
        dplyr::pull(COD_MUN5)

      geom <- mpios_sf %>% dplyr::filter(COD_MUN5 %in% cod_sel)
      if (nrow(geom) > 0) return(sf::st_bbox(geom))
    }

    shp <- mpios_sf %>% dplyr::filter(COD_DPTO2 == sel_cod)
    sf::st_bbox(shp)
  })

  output$map_bpan <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::setView(lng = -73.12, lat = 7.12, zoom = 8)
  })

  observe({
    titulo  <- "Casos"
    fmt_val <- function(x) scales::comma(x)

    sel_cod <- sel_cod_dep()

    shp <- mpios_sf %>%
      dplyr::filter(COD_DPTO2 == sel_cod) %>%
      dplyr::left_join(bpan_agg_mpio(), by = c("COD_DPTO2","COD_MUN5")) %>%
      dplyr::mutate(
        valor  = tidyr::replace_na(valor, 0),
        nombre = dplyr::coalesce(MUN, MUNICIPIO_N),
        etq    = paste0("<b>", nombre, "</b><br>", titulo, ": ", fmt_val(valor))
      )

    pal <- make_quartile_pal(shp$valor)
    bb  <- bbox_actual()

    leaflet::leafletProxy("map_bpan", data = shp) %>%
      leaflet::clearShapes() %>%
      leaflet::clearControls() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::addPolygons(
        layerId = ~COD_MUN5,
        fillColor = ~pal(valor),
        color = BORDER_UI, weight = 0.4, fillOpacity = 0.9,
        label = ~lapply(etq, htmltools::HTML),
        highlightOptions = leaflet::highlightOptions(
          color = BORDER_UI, weight = 2, bringToFront = TRUE
        )
      ) %>%
      leaflet::addLegend(
        position  = "bottomright",
        pal       = pal,
        values    = ~valor,
        title     = titulo,
        labFormat = quartile_lab_format
      ) %>%
      leaflet::fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
  })

  observeEvent(input$map_bpan_shape_click, {
    click <- input$map_bpan_shape_click
    req(click$id)
    codm <- sprintf("%05d", as.integer(click$id))
    nom_mpio <- mpios_sf$MUNICIPIO_N[match(codm, mpios_sf$COD_MUN5)]

    mpios_year <- bpan_base() %>% dplyr::distinct(MUN) %>% dplyr::pull(MUN)

    if (!is.na(nom_mpio) && nzchar(nom_mpio) && nom_mpio %in% mpios_year) {
      updateSelectInput(session, "f_mpio", selected = nom_mpio)
    }
  }, ignoreInit = TRUE)

  top10_df <- reactive({
    bpan_base() %>%
      dplyr::group_by(COD_MUN5, MUN) %>%
      dplyr::summarise(valor = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(nombre = dplyr::if_else(!is.na(MUN) & nzchar(MUN), MUN, COD_MUN5)) %>%
      dplyr::arrange(dplyr::desc(valor)) %>%
      dplyr::slice_head(n = 10)
  })

  build_top_plotly_bpan <- function(){
    df <- top10_df()
    validate(need(nrow(df) > 0, "No hay datos para el Top-10 con los filtros actuales."))

    df2 <- df %>%
      dplyr::arrange(valor) %>%
      dplyr::mutate(
        nombre = factor(nombre, levels = nombre),
        valor_fmt = vapply(valor, fmt_km, character(1))
      )

    plotly::plot_ly(
      data = df2,
      x = ~valor, y = ~nombre,
      type = "bar", orientation = "h",
      marker = list(color = BAR_COLOR),
      text = ~valor_fmt,
      textposition = "inside",
      textfont = list(color = "white"),
      insidetextanchor = "middle",
      hovertemplate = "%{y}<br>Casos: %{x:,}<extra></extra>"
    ) %>%
      plotly::layout(
        xaxis = list(
          title = "Casos",
          showgrid = TRUE,
          gridcolor = "#f0f0f0",
          zeroline = FALSE
        ),
        yaxis = list(
          title = "",
          automargin = TRUE,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        margin = list(l = 10, r = 10, t = 10, b = 10),
        showlegend = FALSE,
        paper_bgcolor = "#ffffff",
        plot_bgcolor  = "#ffffff"
      )
  }

  output$bar_bpan <- renderPlotly({
    build_top_plotly_bpan()
  })

  build_serie_plotly_bpan <- function(){
    df <- bpan_serie_completa() %>%
      dplyr::filter(!is.na(ano)) %>%
      dplyr::group_by(ano) %>%
      dplyr::summarise(Casos = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(ano)

    validate(need(nrow(df) > 0, "No hay datos para la serie temporal con los filtros actuales."))

    ano_seleccionado <- if (!is.null(input$f_anio_bpan)) as.numeric(input$f_anio_bpan) else NULL

    plotly::plot_ly(
      data = df,
      x = ~ano,
      y = ~Casos,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = BAR_COLOR, width = 3),
      marker = list(
        size = 10,
        color = ifelse(df$ano == ano_seleccionado, "#fbc02d", BAR_COLOR),
        line = list(color = "white", width = 2)
      ),
      text = ~paste0("Año: ", ano, "<br>Casos: ", scales::comma(Casos)),
      hoverinfo = "text",
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      plotly::layout(
        xaxis = list(
          title = "",
          showgrid = FALSE,
          tickmode = "linear",
          dtick = 1
        ),
        yaxis = list(
          title = "Casos",
          showgrid = TRUE,
          gridcolor = "#f0f0f0",
          zeroline = FALSE
        ),
        margin = list(l = 60, r = 30, t = 20, b = 60),
        showlegend = FALSE,
        hovermode = "closest",
        paper_bgcolor = "#ffffff",
        plot_bgcolor  = "#ffffff"
      )
  }

  output$serie_temporal <- renderPlotly({
    build_serie_plotly_bpan()
  })

  map_widget_export_bpan <- reactive({
    req(input$f_anio_bpan)

    titulo  <- "Casos"
    fmt_val <- function(x) scales::comma(x)
    sel_cod <- sel_cod_dep()

    shp <- mpios_sf %>%
      dplyr::filter(COD_DPTO2 == sel_cod) %>%
      dplyr::left_join(bpan_agg_mpio(), by = c("COD_DPTO2","COD_MUN5")) %>%
      dplyr::mutate(
        valor  = tidyr::replace_na(valor, 0),
        nombre = dplyr::coalesce(MUN, MUNICIPIO_N),
        etq    = paste0("<b>", nombre, "</b><br>", titulo, ": ", fmt_val(valor))
      )

    bb  <- bbox_actual()
    pal <- make_quartile_pal(shp$valor)

    leaflet::leaflet(
      options = leaflet::leafletOptions(
        zoomControl = TRUE,
        zoomSnap = 0.25
      )
    ) %>%
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.Positron,
        options = leaflet::providerTileOptions(crossOrigin = TRUE)
      ) %>%
      leaflet::addPolygons(
        data = shp,
        layerId = ~COD_MUN5,
        fillColor = ~pal(valor),
        color = BORDER_UI,
        weight = 0.4,
        fillOpacity = 0.9,
        label = ~lapply(etq, htmltools::HTML),
        highlightOptions = leaflet::highlightOptions(
          color = BORDER_UI,
          weight = 2,
          bringToFront = TRUE
        )
      ) %>%
      leaflet::addLegend(
        position  = "bottomright",
        pal       = pal,
        values    = shp$valor,
        title     = titulo,
        labFormat = quartile_lab_format
      ) %>%
      leaflet::fitBounds(
        lng1 = as.numeric(bb["xmin"]),
        lat1 = as.numeric(bb["ymin"]),
        lng2 = as.numeric(bb["xmax"]),
        lat2 = as.numeric(bb["ymax"])
      ) %>%
      htmlwidgets::onRender("
        function(el, x) {
          this.zoomControl.setPosition('topright');
        }
      ")
  })

  tabla_export_bpan <- reactive({
    bpan_base() %>%
      dplyr::transmute(
        anio         = ano,
        departamento = DEP,
        municipio    = MUN,
        cod_dpto     = COD_DPTO2,
        cod_mpio     = COD_MUN5,
        edad         = edad,
        pac_hos      = PAC_HOS
      )
  })

  output$dl_png_mapa_bpan <- downloadHandler(
    filename = function(){
      mun_tag <- if (is.null(input$f_mpio) || input$f_mpio == "Todos") {
        "Todos"
      } else {
        gsub("[^[:alnum:]_]+", "_", input$f_mpio)
      }

      paste0(
        "BPAN_mapa_Santander_",
        mun_tag, "_",
        input$f_anio_bpan %||% "NA", "_",
        Sys.Date(), ".png"
      )
    },
    content = function(file){
      dly <- if (!is.null(input$f_mpio) && input$f_mpio != "Todos") PNG_DELAY_MUN else PNG_DELAY_CO

      ok <- save_widget_png_retry(
        widget     = map_widget_export_bpan(),
        out_png    = file,
        vwidth     = PNG_VWIDTH,
        vheight    = PNG_VHEIGHT,
        delay_base = dly
      )

      if (!ok) stop("No se pudo generar el PNG del mapa.")
    }
  )

  output$dl_png_serie_bpan <- downloadHandler(
    filename = function(){
      mun_tag <- if (is.null(input$f_mpio) || input$f_mpio == "Todos") "Todos" else gsub("[^[:alnum:]_]+", "_", input$f_mpio)
      paste0("BPAN_serie_Santander_", mun_tag, "_", Sys.Date(), ".png")
    },
    content = function(file){
      ok <- save_widget_png_retry(build_serie_plotly_bpan(), file, vwidth = 1800, vheight = 900, delay_base = 0.9)
      if (!ok) stop("No se pudo generar el PNG de la serie.")
    }
  )

  output$dl_png_top_bpan <- downloadHandler(
    filename = function(){
      mun_tag <- if (is.null(input$f_mpio) || input$f_mpio == "Todos") "Todos" else gsub("[^[:alnum:]_]+", "_", input$f_mpio)
      paste0("BPAN_top10_Santander_", mun_tag, "_", input$f_anio_bpan %||% "NA", "_", Sys.Date(), ".png")
    },
    content = function(file){
      ok <- save_widget_png_retry(build_top_plotly_bpan(), file, vwidth = 1800, vheight = 1000, delay_base = 0.9)
      if (!ok) stop("No se pudo generar el PNG del Top-10.")
    }
  )

  output$dl_csv_expl_bpan <- downloadHandler(
    filename = function(){
      mun_tag <- if (is.null(input$f_mpio) || input$f_mpio == "Todos") "Todos" else gsub("[^[:alnum:]_]+", "_", input$f_mpio)
      paste0("BPAN_base_filtrada_Santander_", mun_tag, "_", input$f_anio_bpan %||% "NA", "_", Sys.Date(), ".csv")
    },
    content = function(file){
      readr::write_csv(tabla_export_bpan(), file, na = "")
    }
  )

  output$dl_reporte_pdf_bpan <- downloadHandler(
    filename = function(){
      mun_tag <- if (is.null(input$f_mpio) || input$f_mpio == "Todos") "Todos" else gsub("[^[:alnum:]_]+", "_", input$f_mpio)
      paste0("Informe_descargable_BPAN_Santander_", mun_tag, "_", input$f_anio_bpan %||% "NA", "_", Sys.Date(), ".pdf")
    },
    content = function(file){

      if (!file.exists(ruta_rmd)) stop("No encuentro Informe_descargable.Rmd en la raíz del proyecto.")

      anio_now <- input$f_anio_bpan
      dep_now  <- input$f_depto %||% "Santander"
      mun_now  <- input$f_mpio %||% "Todos"

      dly_map <- if (!is.null(mun_now) && mun_now != "Todos") PNG_DELAY_MUN else PNG_DELAY_CO

      ok_map <- save_widget_png_retry(map_widget_export_bpan(), IMG_MAP, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay_base = dly_map)
      ok_ser <- save_widget_png_retry(build_serie_plotly_bpan(), IMG_SER, vwidth = 1800, vheight = 900, delay_base = 0.9)
      ok_top <- save_widget_png_retry(build_top_plotly_bpan(), IMG_TOP, vwidth = 1800, vheight = 1000, delay_base = 0.9)

      if (!ok_map) stop("No se pudo generar Descargas/bpan_mapa.png para el informe.")
      if (!ok_ser) stop("No se pudo generar Descargas/bpan_serie.png para el informe.")
      if (!ok_top) stop("No se pudo generar Descargas/bpan_top10.png para el informe.")

      filtros_tbl <- data.frame(
        Parametro = c("Año", "Departamento", "Municipio"),
        Valor     = c(as.character(anio_now), as.character(dep_now), as.character(mun_now)),
        stringsAsFactors = FALSE
      )

      logo_src <- file.path(app_root, "www", "LOGO_PLATEA.png")
      if (!file.exists(logo_src)) {
        logo_src2 <- file.path(app_root, "WWW", "LOGO_PLATEA.png")
        logo_src  <- if (file.exists(logo_src2)) logo_src2 else NA_character_
      }
      logo_dst <- file.path(EXPORT_DIR, "LOGO_PLATEA.png")
      if (!is.na(logo_src) && file.exists(logo_src)) file.copy(logo_src, logo_dst, overwrite = TRUE)
      logo_tex <- gsub("\\\\", "/", normalizePath(logo_dst, winslash = "/", mustWork = FALSE))

      td <- tempfile("rmd_bpan_")
      dir.create(td, recursive = TRUE, showWarnings = FALSE)

      rmd_to_render <- ruta_rmd
      rmd_lines <- readLines(ruta_rmd, warn = FALSE, encoding = "UTF-8")
      if (any(grepl("__LOGO_PLATEA_PATH__", rmd_lines, fixed = TRUE))) {
        rmd_tmp <- file.path(td, "Informe_descargable_BPAN_render.Rmd")
        rmd_lines <- gsub("__LOGO_PLATEA_PATH__", logo_tex, rmd_lines, fixed = TRUE)
        writeLines(rmd_lines, rmd_tmp, useBytes = TRUE)
        rmd_to_render <- rmd_tmp
      }

      rmarkdown::render(
        input         = rmd_to_render,
        output_format = "pdf_document",
        output_file   = basename(file),
        output_dir    = dirname(file),
        quiet         = TRUE,
        params        = list(
          app_root     = app_root,
          export_dir   = "Descargas",
          filtros      = filtros_tbl,
          anio         = anio_now,
          especie      = "BPAN",
          departamento = dep_now,
          municipio    = mun_now,
          ind          = "bpan",
          img_map      = basename(IMG_MAP),
          img_serie    = basename(IMG_SER),
          img_ranking  = basename(IMG_TOP),
          csv_filtrado = NULL
        ),
        knit_root_dir = app_root,
        envir         = new.env(parent = globalenv())
      )
    },
    contentType = "application/pdf"
  )
}

shinyApp(ui, server)