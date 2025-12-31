# app_bpan.R
# =========================================================
# BPAN — Dashboard (app exclusiva) - ATLÁNTICO
# =========================================================

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(leaflet); library(sf); library(dplyr); library(tidyr)
  library(scales); library(htmltools); library(DT); library(plotly)
  library(stringi)
})

options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)
options(shiny.maxRequestSize = 100*1024^2)

validate <- shiny::validate
need     <- shiny::need

# ---------- Colores globales ----------
MAP_COLORS <- c("#fff9db", "#ffe082", "#ffd54f", "#fbc02d", "#c49000")

BAR_COLOR  <- "#ffd54f"
BORDER_UI  <- "#ffb366"

# ---------- Utils ----------
norm_txt <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
NUP      <- function(x) toupper(norm_txt(x))

# Title Case en español, optimizado (mucho más rápido)
title_case_es <- function(x){
  x <- trimws(as.character(x))
  is_na <- is.na(x) | x == ""
  if (all(is_na)) return(x)
  
  x_ok <- x[!is_na]
  
  # 1) Pasar todo a Title Case
  x_ok <- stringi::stri_trans_totitle(x_ok, locale = "es")
  
  # 2) Bajar conectores al medio de la frase (no toco el primero)
  conectores      <- c(" De ", " Del ", " La ", " Las ", " Los ", " Y ", " E ", " O ", " U ",
                       " En ", " Por ", " Para ", " Con ", " Sin ", " A ", " Al ", " El ")
  conectores_low  <- tolower(conectores)
  
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

# ---------- Helper por si acaso ----------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---------- Rutas ----------
local_data_dir <- "data"
app_root     <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
rel_data_dir <- file.path(app_root, "data")
data_dir <- if (dir.exists(rel_data_dir)) rel_data_dir else local_data_dir

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

# ---------- 1) Leer BPAN (casos = filas) ----------
bpan_raw <- readRDS(bpan_path)

# CAMBIO: Filtrar solo Atlántico
bpan_raw <- bpan_raw %>%
  dplyr::filter(DEPARTAMENTO_O == "ATLÁNTICO" | DEPARTAMENTO_O == "ATLANTICO")

# SOLO año, eliminado mes
b_year_col <- get_col(bpan_raw, c("ano","ANO","year","YEAR"), "BPAN: no 'ano'/'ANO'")

# Códigos para MAPA desde OCURRENCIA (_O)
b_mun_code_col <- get_col(
  bpan_raw,
  c("COD_DANE_MUNIC_O","COD_DANE_MUNIC_D","COD_MUN5","COD_MPIO","MPIO_CDPMP"),
  "BPAN: no 'COD_DANE_MUNIC_O' (ni columnas equivalentes) para ocurrencia"
)

# Códigos y nombres de ORIGEN (ocurrencia)
b_dep_origen_col <- get_col(bpan_raw, c("DEPARTAMENTO_O","DEPARTAMENTO"), "BPAN: no 'DEPARTAMENTO_O'")
b_mun_origen_col <- get_col(bpan_raw, c("MUNICIPIO_O","MUNICIPIO"), "BPAN: no 'MUNICIPIO_O'")
b_dep_code_o_col <- get_col(bpan_raw, c("COD_DANE_DPTO_O","COD_DPTO"), "BPAN: no 'COD_DANE_DPTO_O'")

bpan <- bpan_raw %>%
  dplyr::transmute(
    ano       = suppressWarnings(as.integer(.data[[b_year_col]])),
    COD_MUN5  = sprintf("%05d", suppressWarnings(as.integer(.data[[b_mun_code_col]]))),
    COD_DPTO2 = if (!is.na(b_dep_code_o_col)) {
      sprintf("%02d", suppressWarnings(as.integer(.data[[b_dep_code_o_col]])))
    } else {
      substr(COD_MUN5, 1, 2)
    },
    DEP     = title_case_es(.data[[b_dep_origen_col]]),
    MUN     = title_case_es(.data[[b_mun_origen_col]]),
    edad      = suppressWarnings(as.numeric(edad)),     # edad materna (si existe en la base)
    PAC_HOS   = suppressWarnings(as.integer(pac_hos))   # 1 = vivo, 2 = fallecido (si existe en la base)
  ) %>%
  dplyr::filter(
    !is.na(ano),
    !is.na(COD_MUN5),
    !is.na(COD_DPTO2),
    !is.na(MUN),
    !is.na(DEP)
  )

# Filtrar solo Atlántico (normalizando el nombre)
bpan <- bpan %>%
  dplyr::mutate(DEP = ifelse(toupper(DEP) %in% c("ATLÁNTICO", "ATLANTICO"), "Atlántico", DEP)) %>%
  dplyr::filter(DEP == "Atlántico")

# *** Lookup de departamentos - solo Atlántico ***
DEPS_ALL <- "Atlántico"
ATLANTICO_DEFAULT <- "Atlántico"

# ---------- 2) Shapes ----------
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

# ---------- Helper paleta con CUARTILES EXACTOS ----------
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
      bins <- c(0, max_val/2, max_val)
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
      labels[i] <- paste0(">", scales::comma(lower, accuracy = 1),
                          " – ", scales::comma(upper, accuracy = 1))
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

      .nav-tabs .nav-link.active{
        border-color:var(--border-col) var(--border-col) #fff !important;
      }
      .nav-tabs{
        border-bottom:1.5px solid var(--border-col);
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
    ", BORDER_UI)))
  ),
  div(
    class = "wrap",
    h3("Bajo Peso al Nacer (BPAN) - Atlántico"),
    div(class = "data-note",
        HTML("Datos de ocurrencia de bajo peso al nacer en el departamento del Atlántico")),
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
            selected = ATLANTICO_DEFAULT
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
            class = "card-title d-flex align-items-center",
            textOutput("ttl_map_tab1")
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
          div(class = "card-title", "¿Cómo ha evolucionado los casos de bajo peso al nacer (BPAN) por año?"),
          div(class = "series-plot-container",
              plotlyOutput("serie_temporal", height = 330))
        ),
        div(
          class = "card",
          div(class = "card-title", textOutput("ttl_top10_tab1")),
          plotlyOutput("bar_bpan", height = 350)
        )
      )
    )
  )
)

# ---------- 4) SERVER ----------
server <- function(input, output, session){
  
  # ================= Exploración =================
  output$anio_bpan_ui <- renderUI({
    yrs <- sort(unique(bpan$ano))
    selectInput("f_anio_bpan", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  # --- Helper central: cargar municipios según año + depto (arregla tu problema) ---
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
  
  # Reset
  observeEvent(input$btn_reset_bpan, {
    yrs <- sort(unique(bpan$ano))
    ysel <- max(yrs, na.rm = TRUE)
    
    updateSelectInput(session, "f_anio_bpan", selected = ysel)
    updateSelectInput(session, "f_depto", selected = ATLANTICO_DEFAULT)
    
    # Recargar municipios con esos filtros (año+depto)
    isolate({
      update_mpio_choices(selected = "Todos")
    })
  })
  
  # Al cambiar año: actualizar depto y (CLAVE) cargar municipios
  observeEvent(input$f_anio_bpan, {
    req(input$f_anio_bpan)
    
    df_year <- bpan %>% dplyr::filter(ano == input$f_anio_bpan)
    
    deps_o <- df_year %>%
      dplyr::distinct(DEP) %>%
      dplyr::arrange(DEP) %>%
      dplyr::pull(DEP)
    
    sel_dep <- if (!is.null(input$f_depto) && input$f_depto %in% deps_o) input$f_depto else deps_o[1]
    
    updateSelectInput(session, "f_depto", choices = deps_o, selected = sel_dep)
    
    # IMPORTANTÍSIMO: cargar municipios (choices) del año+depto
    isolate({
      update_mpio_choices(selected = "Todos")
    })
  }, ignoreInit = FALSE)
  
  # Al cambiar depto: recargar municipios (choices)
  observeEvent(input$f_depto, {
    req(input$f_anio_bpan, input$f_depto)
    update_mpio_choices()
  }, ignoreInit = FALSE)
  
  # Base de datos filtrada (para mapa/top10)
  bpan_base <- reactive({
    req(input$f_anio_bpan)
    df <- bpan %>% dplyr::filter(ano == input$f_anio_bpan)
    if (!is.null(input$f_depto))
      df <- df %>% dplyr::filter(DEP == input$f_depto)
    if (!is.null(input$f_mpio) && input$f_mpio != "Todos")
      df <- df %>% dplyr::filter(MUN == input$f_mpio)
    df
  })
  
  # Base completa para serie temporal (sin filtrar por año)
  bpan_serie_completa <- reactive({
    df <- bpan
    if (!is.null(input$f_depto))
      df <- df %>% dplyr::filter(DEP == input$f_depto)
    if (!is.null(input$f_mpio) && input$f_mpio != "Todos")
      df <- df %>% dplyr::filter(MUN == input$f_mpio)
    df
  })
  
  # Siempre municipios (Atlántico)
  sel_cod_dep <- reactive({ "08" })
  
  # Títulos
  output$ttl_map_tab1 <- renderText({
    if (is.null(input$f_anio_bpan)) return("")
    paste0("¿Dónde se concentran los casos de bajo peso al nacer (BPAN) en Atlántico en ", input$f_anio_bpan, "?")
  })
  
  output$ttl_top10_tab1 <- renderText({
    if (is.null(input$f_anio_bpan)) return("")
    paste0("Top 10 municipios con más casos de bajo peso al nacer (BPAN) en ", input$f_anio_bpan)
  })
  
  # Agregación municipio
  bpan_agg_mpio <- reactive({
    bpan_base() %>%
      dplyr::group_by(COD_DPTO2, COD_MUN5, MUN) %>%
      dplyr::summarise(valor = dplyr::n(), .groups = "drop")
  })
  
  # Mapa base
  output$map_bpan <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::setView(lng = -74.9, lat = 10.8, zoom = 9)
  })
  
  # Actualizar mapa
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
    bb  <- sf::st_bbox(shp)
    
    leaflet::leafletProxy("map_bpan", data = shp) %>%
      leaflet::clearShapes() %>%
      leaflet::clearControls() %>%
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
  
  # Click en mapa => seleccionar municipio
  observeEvent(input$map_bpan_shape_click, {
    click <- input$map_bpan_shape_click; req(click$id)
    codm <- sprintf("%05d", as.integer(click$id))
    nom_mpio <- mpios_sf$MUNICIPIO_N[match(codm, mpios_sf$COD_MUN5)]
    
    mpios_year <- bpan_base() %>% dplyr::distinct(MUN) %>% dplyr::pull(MUN)
    
    if (!is.na(nom_mpio) && nzchar(nom_mpio) && nom_mpio %in% mpios_year) {
      updateSelectInput(session, "f_mpio", selected = nom_mpio)
    }
  }, ignoreInit = TRUE)
  
  # Top-10 municipios
  top10_df <- reactive({
    bpan_base() %>%
      dplyr::group_by(COD_MUN5, MUN) %>%
      dplyr::summarise(valor = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(nombre = dplyr::if_else(!is.na(MUN) & nzchar(MUN), MUN, COD_MUN5)) %>%
      dplyr::arrange(dplyr::desc(valor)) %>%
      dplyr::slice_head(n = 10)
  })
  
  output$bar_bpan <- renderPlotly({
    df <- top10_df()
    validate(need(nrow(df) > 0, "No hay datos para el Top-10 con los filtros actuales."))
    
    df2 <- df %>%
      dplyr::arrange(valor) %>%
      dplyr::mutate(nombre = factor(nombre, levels = nombre))
    
    plotly::plot_ly(
      data = df2,
      x = ~valor, y = ~nombre,
      type = "bar", orientation = "h",
      marker = list(color = BAR_COLOR),
      text = ~scales::comma(valor),
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
        showlegend = FALSE
      )
  })
  
  # Serie temporal
  output$serie_temporal <- renderPlotly({
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
        color = ~ifelse(!is.null(ano_seleccionado) & ano == ano_seleccionado, "#ffd54f", BAR_COLOR),
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
        hovermode = "closest"
      )
  })
}

shinyApp(ui, server)
