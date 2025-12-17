# app_bpan.R
# =========================================================
# BPAN — Dashboard (app exclusiva)
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
  
  # 1) Pasar todo a Title Case con una sola llamada vectorizada
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
bpan_raw <- bpan_raw %>% dplyr::filter(DEPARTAMENTO_O=="SANTANDER")  # <--- solo Santander

b_month_col    <- get_col(bpan_raw, c("mes","MES"), "BPAN: no 'mes'/'MES'")
b_year_col     <- get_col(bpan_raw, c("ano","ANO"), "BPAN: no 'ano'/'ANO'")

# AHORA: códigos para el MAPA desde OCURRENCIA (_O), no desde _D
b_mun_code_col <- get_col(
  bpan_raw,
  c("COD_DANE_MUNIC_O","COD_DANE_MUNIC_D","COD_MUN5","COD_MPIO","MPIO_CDPMP"),
  "BPAN: no 'COD_DANE_MUNIC_O' (ni columnas equivalentes) para ocurrencia"
)

# Nombre "destino" sigue estando disponible si lo quieres luego en otra pestaña
b_dep_name_col <- get_col(bpan_raw, c("DEPARTAMENTO_D","DEPARTMENTO_D"), "BPAN: no 'DEPARTAMENTO_D'")
b_mun_name_col <- get_col(bpan_raw, c("MUNICIPIO_D","NOMBRE_MPIO"), "BPAN: no 'MUNICIPIO_D'")

# Códigos y nombres de ORIGEN (ocurrencia)
b_dep_origen_col  <- if ("DEPARTAMENTO_O"   %in% names(bpan_raw)) "DEPARTAMENTO_O"   else NA_character_
b_mun_origen_col  <- if ("MUNICIPIO_O"      %in% names(bpan_raw)) "MUNICIPIO_O"      else NA_character_
b_dep_code_o_col  <- if ("COD_DANE_DPTO_O"  %in% names(bpan_raw)) "COD_DANE_DPTO_O"  else NA_character_

bpan <- bpan_raw %>%
  dplyr::transmute(
    mes       = suppressWarnings(as.integer(.data[[b_month_col]])),
    ano       = suppressWarnings(as.integer(.data[[b_year_col]])),
    # CÓDIGO MUNICIPAL AHORA ES DE OCURRENCIA
    COD_MUN5  = sprintf("%05d", suppressWarnings(as.integer(.data[[b_mun_code_col]]))),
    # CÓDIGO DEPARTAMENTAL TAMBIÉN, SI ESTÁ; SI NO, SE DERIVA DEL MUNICIPIO
    COD_DPTO2 = if (!is.na(b_dep_code_o_col)) {
      sprintf("%02d", suppressWarnings(as.integer(.data[[b_dep_code_o_col]])))
    } else {
      substr(COD_MUN5, 1, 2)
    },
    # Nombres de DESTINO (por si se usan después)
    DEP_D     = title_case_es(.data[[b_dep_name_col]]),
    MUN_D     = title_case_es(.data[[b_mun_name_col]]),
    # Nombres de OCURRENCIA (los que realmente usamos en filtros/mapa)
    DEP_O     = if (!is.na(b_dep_origen_col))  title_case_es(.data[[b_dep_origen_col]])  else NA_character_,
    MUN_O     = if (!is.na(b_mun_origen_col))  title_case_es(.data[[b_mun_origen_col]])  else NA_character_,
    edad      = suppressWarnings(as.numeric(edad)),     # edad materna
    PAC_HOS   = suppressWarnings(as.integer(pac_hos))   # 1 = vivo, 2 = fallecido
  ) %>%
  dplyr::filter(
    !is.na(ano),
    !is.na(COD_MUN5),
    !is.na(COD_DPTO2),
    !is.na(MUN_D)
  )

# Solo filas con ORIGEN válido (ocurrencia)
bpan_valid <- bpan %>%
  dplyr::filter(
    !is.na(DEP_O), DEP_O != "",
    !is.na(MUN_O), MUN_O != ""
  )

# *** Lookup de departamentos y valor por defecto Santander ***
DEPS_ALL <- sort(unique(bpan_valid$DEP_O))
SANTANDER_DEFAULT <- if ("Santander" %in% DEPS_ALL) "Santander" else (DEPS_ALL[1] %||% "Todos")

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

# ---------- Lookups ----------
dpt_lookup_bpan <- bpan_valid %>%
  dplyr::select(COD_DPTO2, DEP_O) %>%
  dplyr::mutate(
    COD_DPTO2 = sprintf("%02d", as.integer(COD_DPTO2)),
    DEP_O     = trimws(DEP_O)
  ) %>%
  dplyr::distinct() %>%
  dplyr::arrange(DEP_O)

mun_lookup_bpan <- bpan_valid %>%
  dplyr::select(COD_DPTO2, COD_MUN5, MUN_O) %>%
  dplyr::mutate(
    COD_DPTO2 = sprintf("%02d", as.integer(COD_DPTO2)),
    COD_MUN5  = sprintf("%05d", as.integer(COD_MUN5)),
    MUN_O     = trimws(MUN_O)
  ) %>%
  dplyr::distinct()

# Helper para operador por si acaso lo usamos antes
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

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
  
  pal <- leaflet::colorBin(
    palette = colors,
    domain  = values,
    bins    = bins,
    na.color = "#f0f0f0"
  )
  
  pal
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
    h3(""),
    div(class = "data-note",
        HTML("")),
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
            "f_depto_ocurr", NULL,
            choices  = c("Todos", DEPS_ALL),
            selected = SANTANDER_DEFAULT   # <-- por defecto Santander
          )
        ),
        div(
          class = "filter",
          div(class = "filter-label", "¿Algún municipio en particular?"),
          selectInput("f_mpio_ocurr", NULL, choices = "Todos", selected = "Todos")
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
            "Nota: el mapa está clasificado en 4 cuartiles según los casos observados; ",
            "la leyenda muestra los rangos de cada cuartil con valores redondeados."
          )
        )
      ),
      column(
        width = 6,
        div(
          class = "card",
          div(class = "card-title", "¿Como ha evolucionado los casos de bajo peso al nacer (BPAN)?"),
          div(class = "series-plot-container",
              plotlyOutput("serie_temporal", height = 330))),
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
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  
  # ================= Exploración =================
  output$anio_bpan_ui <- renderUI({
    yrs <- sort(unique(bpan_valid$ano))
    selectInput("f_anio_bpan", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  observeEvent(input$btn_reset_bpan, {
    yrs <- sort(unique(bpan_valid$ano))
    updateSelectInput(session, "f_anio_bpan",  selected = max(yrs, na.rm = TRUE))
    # reset dejando Santander seleccionado
    updateSelectInput(session, "f_depto_ocurr",
                      choices  = c("Todos", DEPS_ALL),
                      selected = SANTANDER_DEFAULT)
    updateSelectInput(session, "f_mpio_ocurr",  choices = "Todos", selected = "Todos")
  })
  
  observeEvent(input$f_anio_bpan, {
    df_year <- bpan_valid %>% dplyr::filter(ano == input$f_anio_bpan)
    deps_o  <- df_year %>% dplyr::distinct(DEP_O) %>% dplyr::arrange(DEP_O) %>% dplyr::pull(DEP_O)
    
    # si lo que estaba seleccionado sigue siendo válido, lo mantenemos;
    # si no, tratamos de dejar Santander; si tampoco está, "Todos"
    sel_dep <- if (!is.null(input$f_depto_ocurr) && input$f_depto_ocurr %in% c("Todos", deps_o)) {
      input$f_depto_ocurr
    } else if (SANTANDER_DEFAULT %in% deps_o) {
      SANTANDER_DEFAULT
    } else {
      "Todos"
    }
    updateSelectInput(session, "f_depto_ocurr", choices = c("Todos", deps_o), selected = sel_dep)
    updateSelectInput(session, "f_mpio_ocurr",  choices = "Todos", selected = "Todos")
  }, ignoreInit = FALSE)
  
  observeEvent(input$f_depto_ocurr, {
    df <- bpan_valid %>% dplyr::filter(ano == input$f_anio_bpan)
    if (!is.null(input$f_depto_ocurr) && input$f_depto_ocurr != "Todos") {
      mpios_o <- df %>%
        dplyr::filter(DEP_O == input$f_depto_ocurr) %>%
        dplyr::distinct(MUN_O) %>%
        dplyr::arrange(MUN_O) %>%
        dplyr::pull(MUN_O)
      updateSelectInput(session, "f_mpio_ocurr", choices = c("Todos", mpios_o), selected = "Todos")
    } else {
      updateSelectInput(session, "f_mpio_ocurr", choices = "Todos", selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  # Base de datos filtrada por ocurrencia
  bpan_base <- reactive({
    req(input$f_anio_bpan)
    df <- bpan_valid %>% dplyr::filter(ano == input$f_anio_bpan)
    if (!is.null(input$f_depto_ocurr) && input$f_depto_ocurr != "Todos")
      df <- df %>% dplyr::filter(DEP_O == input$f_depto_ocurr)
    if (!is.null(input$f_mpio_ocurr)  && input$f_mpio_ocurr  != "Todos")
      df <- df %>% dplyr::filter(MUN_O == input$f_mpio_ocurr)
    df
  })
  
  # Base completa para serie temporal (sin filtrar por año)
  bpan_serie_completa <- reactive({
    df <- bpan_valid
    if (!is.null(input$f_depto_ocurr) && input$f_depto_ocurr != "Todos")
      df <- df %>% dplyr::filter(DEP_O == input$f_depto_ocurr)
    if (!is.null(input$f_mpio_ocurr)  && input$f_mpio_ocurr  != "Todos")
      df <- df %>% dplyr::filter(MUN_O == input$f_mpio_ocurr)
    df
  })
  
  # Determinar nivel del mapa
  map_nivel <- reactive({
    if (!is.null(input$f_depto_ocurr) && input$f_depto_ocurr != "Todos") "mpios" else "deptos"
  })
  
  sel_cod_dep <- reactive({
    if (map_nivel() == "mpios") {
      cod <- dptos_sf %>%
        dplyr::filter(DEPARTAMENTO_N == input$f_depto_ocurr) %>%
        dplyr::pull(COD_DPTO2) %>%
        .[1]
      if (is.na(cod) || !nzchar(cod)) {
        cod <- bpan_base() %>%
          dplyr::distinct(COD_DPTO2) %>%
          dplyr::pull(COD_DPTO2) %>%
          .[1]
      }
      cod
    } else NA_character_
  })
  
  # Títulos
  output$ttl_map_tab1 <- renderText({
    if (is.null(input$f_anio_bpan)) return("")
    if (map_nivel() == "deptos") {
      paste0("¿Dónde se concentran los casos de bajo peso al nacer (BPAN) a nivel departamental", "?")
    } else {
      paste0("¿Dónde se concentran los casos de bajo peso al nacer (BPAN) a nivel municipal", "?")
    }
  })
  
  output$ttl_top10_tab1 <- renderText({
    if (is.null(input$f_anio_bpan)) return("")
    paste0("Top 10 municipios con más casos de bajo peso al nacer (BPAN)")
  })
  
  # Agregaciones por departamento y municipio (ya con COD_* de ocurrencia)
  bpan_agg_depto <- reactive({
    bpan_base() %>%
      dplyr::group_by(COD_DPTO2, DEP_O) %>%
      dplyr::summarise(valor = dplyr::n(), .groups = "drop")
  })
  
  bpan_agg_mpio  <- reactive({
    bpan_base() %>%
      dplyr::group_by(COD_DPTO2, COD_MUN5, MUN_O) %>%
      dplyr::summarise(valor = dplyr::n(), .groups = "drop")
  })
  
  # Mapa base
  output$map_bpan <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::setView(lng = -74.3, lat = 4.6, zoom = 5)
  })
  
  # Actualizar mapa
  observe({
    titulo  <- "Casos"
    fmt_val <- function(x) scales::comma(x)
    
    if (map_nivel() == "deptos") {
      shp <- dptos_sf %>%
        dplyr::left_join(bpan_agg_depto(), by = "COD_DPTO2") %>%
        dplyr::mutate(
          valor  = tidyr::replace_na(valor, 0),
          nombre = dplyr::coalesce(DEP_O, DEPARTAMENTO_N),
          etq    = paste0("<b>", nombre, "</b><br>", titulo, ": ", fmt_val(valor))
        )
      
      pal <- make_quartile_pal(shp$valor)
      
      leaflet::leafletProxy("map_bpan", data = shp) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addPolygons(
          layerId = ~COD_DPTO2,
          fillColor = ~pal(valor),
          color = BORDER_UI, weight = 0.7, fillOpacity = 0.9,
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
        )
      
    } else {
      sel_cod <- sel_cod_dep(); req(!is.na(sel_cod), nzchar(sel_cod))
      shp <- mpios_sf %>%
        dplyr::filter(COD_DPTO2 == sel_cod) %>%
        dplyr::left_join(bpan_agg_mpio(), by = c("COD_DPTO2","COD_MUN5")) %>%
        dplyr::mutate(
          valor  = tidyr::replace_na(valor, 0),
          nombre = dplyr::coalesce(MUN_O, MUNICIPIO_N),
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
    }
  })
  
  observeEvent(input$map_bpan_shape_click, {
    click <- input$map_bpan_shape_click; req(click$id)
    if (map_nivel()=="deptos") {
      cod <- sprintf("%02d", as.integer(click$id))
      nom_shape <- dptos_sf$DEPARTAMENTO_N[match(cod, dptos_sf$COD_DPTO2)]
      deps_year <- bpan_base() %>% dplyr::distinct(DEP_O) %>% dplyr::pull(DEP_O)
      if (!is.na(nom_shape) && nzchar(nom_shape) && nom_shape %in% deps_year)
        updateSelectInput(session, "f_depto_ocurr", selected = nom_shape)
    } else {
      codm <- sprintf("%05d", as.integer(click$id))
      nom_mpio <- mpios_sf$MUNICIPIO_N[match(codm, mpios_sf$COD_MUN5)]
      if (!is.null(input$f_depto_ocurr) && input$f_depto_ocurr != "Todos") {
        mpios_year_dep <- bpan_base() %>%
          dplyr::distinct(MUN_O) %>% dplyr::pull(MUN_O)
        if (!is.na(nom_mpio) && nzchar(nom_mpio) && nom_mpio %in% mpios_year_dep)
          updateSelectInput(session, "f_mpio_ocurr", selected = nom_mpio)
      }
    }
  }, ignoreInit = TRUE)
  
  # Top-10 MUNICIPIOS (OCURRENCIA)
  top10_df <- reactive({
    bpan_base() %>%
      dplyr::group_by(COD_MUN5, MUN_O) %>%
      dplyr::summarise(valor = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(nombre = dplyr::if_else(!is.na(MUN_O) & nzchar(MUN_O), MUN_O, COD_MUN5)) %>%
      dplyr::arrange(dplyr::desc(valor)) %>%
      dplyr::slice_head(n = 10)
  })
  
  output$bar_bpan <- renderPlotly({
    df <- top10_df()
    validate(need(nrow(df) > 0, "No hay datos para el Top-10 con los filtros actuales."))
    df2 <- df %>%
      dplyr::arrange(valor) %>%
      dplyr::mutate(
        nombre = factor(nombre, levels = nombre)
      )
    
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
