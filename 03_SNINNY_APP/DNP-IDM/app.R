# app_idm.R — Terridata IDM (storytelling, paletas y bordes personalizados)
suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(leaflet); library(sf); library(dplyr)
  library(scales); library(htmltools); library(plotly)
  library(stringi); library(readr)
})
options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

# ---------- Rutas ----------
data_dir <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/DNP-IDM/data"
ruta_idm     <- file.path(data_dir, "071_DNP_Terridata_IDM.rds")
ruta_pob     <- file.path(data_dir, "051_DANE_Proyecciones_P_total.rds")  # opcional
ruta_shp_mun <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")
ruta_shp_dep <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")

# ---------- Utils ----------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
norm_txt <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
NUP      <- function(x) toupper(norm_txt(x))

# paleta para mapas (tu gama)
MAP_PALETTE <- c("#005b88", "#0099cc", "#4bb5e1", "#99d5ec", "#e0f3fa")

make_pal_bin <- function(values, palette = MAP_PALETTE, n_bins = 6){
  vals <- suppressWarnings(as.numeric(values)); vals <- vals[is.finite(vals)]
  if (!length(vals)) vals <- 0
  qs <- stats::quantile(vals, probs = seq(0, 1, length.out = n_bins), na.rm = TRUE)
  qs <- unique(as.numeric(qs)); if (length(qs) < 3) qs <- pretty(vals, n = n_bins)
  bins <- sort(unique(c(min(vals, na.rm = TRUE), qs, max(vals, na.rm = TRUE))))
  leaflet::colorBin(palette, domain = vals, bins = bins, na.color = "#f0f0f0")
}

pick_col <- function(df, primary, pattern){
  nms <- names(df); if (primary %in% nms) return(primary)
  alt <- nms[grepl(pattern, nms, ignore.case = TRUE)]
  if (length(alt)) alt[1] else NA_character_
}
safe_pull <- function(df, col) if (!is.na(col) && col %in% names(df)) df[[col]] else NA

# ---------- Cargar IDM ----------
idm_raw <- readRDS(ruta_idm)

col_ano     <- pick_col(idm_raw, "ano", "^a(n|ñ)o$|year")
col_dep_cod <- pick_col(idm_raw, "COD_DANE_DPTO_D", "DPTO|DEPTO|DANE.*DEP|COD.*DEP|DEPART")
col_dep_nom <- pick_col(idm_raw, "DEPARTAMENTO_D", "DEPARTA")
col_mun_cod <- pick_col(idm_raw, "COD_DANE_MUNIC_D", "MUNI.*COD|COD.*MUNI|DANE.*MUNI|COD_MUN5|MPIO")
col_mun_nom <- pick_col(idm_raw, "MUNICIPIO_D", "MUNICIP")
col_val     <- pick_col(idm_raw, "valor", "valor|indice|idm")

idm <- tibble::tibble(
  ano           = suppressWarnings(as.integer(safe_pull(idm_raw, col_ano))),
  COD_DANE_DPTO = sprintf("%02d", as.integer(gsub("\\D","", safe_pull(idm_raw, col_dep_cod)))),
  DEPARTAMENTO  = norm_txt(safe_pull(idm_raw, col_dep_nom)),
  COD_DANE_MUNI = sprintf("%05d", as.integer(gsub("\\D","", safe_pull(idm_raw, col_mun_cod)))),
  MUNICIPIO     = norm_txt(safe_pull(idm_raw, col_mun_nom)),
  valor         = suppressWarnings(as.numeric(safe_pull(idm_raw, col_val)))
) %>% dplyr::filter(is.finite(ano), nzchar(COD_DANE_MUNI))

# ---------- Shapes (robustos) ----------
mun_raw <- sf::st_read(ruta_shp_mun, quiet = TRUE)
dep_raw <- sf::st_read(ruta_shp_dep, quiet = TRUE)

# Municipios
col_mpio_code <- pick_col(mun_raw, "MPIO_CDPMP", "MPIO_CDPMP|COD_MPIO|CODMPIO|COD_MUN")
col_mpio_name <- pick_col(mun_raw, "MPIO_CNMBR", "MPIO_CNMBR|NOMBRE_MPIO|NOM_MPIO|MUNICIPIO")
stopifnot(!is.na(col_mpio_code))
mun_sf <- mun_raw %>%
  mutate(
    COD_MUN5    = sprintf("%05d", as.integer(.data[[col_mpio_code]])),
    COD_DPTO2   = substr(COD_MUN5, 1, 2),
    MUNICIPIO_N = as.character(.data[[col_mpio_name]] %||% COD_MUN5)
  ) %>% sf::st_transform(4326) %>% sf::st_make_valid()

# Departamentos
col_dep_code <- pick_col(dep_raw, "DPTO_CCDGO", "DPTO_CCDGO|COD_DEPTO|CODDPTO|DPTO_CCES")
col_dep_name <- pick_col(dep_raw, "DEPARTAMENTO_D", "DEPARTAMENTO_D|DPTO_CNMBR|NOMBRE_DEPTO|NOM_DPTO|DEPARTAM")
stopifnot(!is.na(col_dep_code))
dep_sf <- dep_raw %>%
  mutate(
    COD_DPTO2       = sprintf("%02d", as.integer(.data[[col_dep_code]])),
    DEPARTAMENTO_N  = as.character(.data[[col_dep_name]] %||% COD_DPTO2)
  ) %>% sf::st_transform(4326) %>% sf::st_make_valid()

# Lookups
dep_lookup <- idm %>% dplyr::select(COD_DANE_DPTO, DEPARTAMENTO) %>%
  mutate(COD_DPTO2 = sprintf("%02d", as.integer(COD_DANE_DPTO)),
         DEP_NORM  = NUP(DEPARTAMENTO)) %>% distinct()
dep_sf$DEP_NORM_SHP <- NUP(dep_sf$DEPARTAMENTO_N)

# ---------- UI ----------
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5, primary = "#2563eb",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius" = "0.9rem", "font-size-base" = "0.95rem"
  ),
  tags$head(tags$style(HTML("
    .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
    h3{font-weight:700;letter-spacing:.2px;margin-bottom:8px}
    .data-note{font-size:13px;color:#6b7280;margin:0 0 16px}

    /* Bordes personalizados #99d5ec en filtros y cards */
    .filters{
      background:#fff;border:1.5px solid #99d5ec;border-radius:16px;
      padding:14px 16px;margin-bottom:16px;box-shadow:0 4px 14px rgba(0,0,0,.06)
    }
    .card{
      background:#fff;border:1.5px solid #99d5ec;border-radius:16px;padding:12px;
      box-shadow:0 2px 10px rgba(0,0,0,.05);margin-bottom:12px
    }
    .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}

    /* Inputs con el mismo borde */
    .form-control{ border:1.5px solid #99d5ec !important; border-radius:10px; }
    .form-control:focus{ border-color:#99d5ec !important; box-shadow:0 0 0 .2rem rgba(153,213,236,.35); }
    .selectize-input{ border:1.5px solid #99d5ec !important; border-radius:10px; }
    .selectize-input.focus{ border-color:#99d5ec !important; box-shadow:0 0 0 .2rem rgba(153,213,236,.35); }
    .selectize-dropdown{ border:1.5px solid #99d5ec !important; }
  "))),
  div(class="wrap",
      h3("DNP — Índice de Desempeño Municipal (IDM)"),
      div(class="data-note","Exploración del IDM a nivel municipal y departamental."),
      tabsetPanel(
        tabPanel("Exploración IDM", br(),
                 div(class="filters",
                     fluidRow(
                       column(3, selectInput("anio", "Año",
                                             choices = sort(unique(idm$ano)),
                                             selected = max(idm$ano, na.rm = TRUE))),
                       column(3, selectInput("f_dep", "Departamento", choices = "Todos")),
                       column(3, selectInput("f_mun", "Municipio", choices = "Todos")),
                       column(3, actionLink("btn_back_co","⤺ Volver a Colombia"))
                     )),
                 fluidRow(
                   column(6, div(class="card",
                                 div(class="card-title", textOutput("ttl_mapa")),
                                 leafletOutput("map_idm", height=700))),
                   column(6,
                          div(class="card",
                              div(class="card-title", textOutput("ttl_top_mpios")),
                              plotlyOutput("bar_top", height=310)),
                          div(class="card",
                              div(class="card-title", textOutput("ttl_prom_dep")),
                              plotlyOutput("bar_depto", height=310)))
                 )
        )
      )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session){
  
  # ---- Títulos reactivos (storytelling) ----
  output$ttl_mapa <- renderText({
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      paste0("¿Qué puntaje promedio de desempeño municipal tiene cada departamento? — ", input$anio)
    } else if (!is.null(input$f_mun) && input$f_mun != "Todos") {
      paste0("¿Qué puntaje de desempeño tiene ", input$f_mun, " (", input$f_dep, ") en el mapa? — ", input$anio)
    } else {
      paste0("¿Qué puntaje de desempeño tiene cada municipio de ", input$f_dep, "? — ", input$anio)
    }
  })
  output$ttl_top_mpios <- renderText({
    if (is.null(input$f_dep) || input$f_dep == "Todos")
      paste0("¿Cuáles son los 10 municipios que obtienen la calificación más alta en desempeño? — ", input$anio)
    else
      paste0("¿Cuáles son los 10 municipios con mayor desempeño en ", input$f_dep, "? — ", input$anio)
  })
  output$ttl_prom_dep <- renderText({
    paste0("¿Cuál es el puntaje promedio de desempeño municipal por departamento? — ", input$anio)
  })
  
  # ---- Combos dependientes ----
  observe({
    df <- idm %>% dplyr::filter(ano == input$anio)
    deps <- df %>% dplyr::distinct(DEPARTAMENTO) %>% dplyr::arrange(DEPARTAMENTO) %>% dplyr::pull()
    sel <- if (!is.null(input$f_dep) && input$f_dep %in% deps) input$f_dep else "Todos"
    updateSelectInput(session, "f_dep", choices = c("Todos", deps), selected = sel)
    updateSelectInput(session, "f_mun", choices = "Todos")
  })
  
  observeEvent(input$f_dep, {
    df <- idm %>% dplyr::filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") {
      mpios <- df %>% dplyr::filter(DEPARTAMENTO == input$f_dep) %>%
        dplyr::distinct(MUNICIPIO) %>% dplyr::arrange(MUNICIPIO) %>% dplyr::pull()
      updateSelectInput(session, "f_mun", choices = c("Todos", mpios), selected = "Todos")
    } else updateSelectInput(session, "f_mun", choices = "Todos")
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_back_co, {
    updateSelectInput(session, "f_dep", selected = "Todos")
    updateSelectInput(session, "f_mun", selected = "Todos")
  })
  
  # ---- Base filtrada ----
  base_filtrada <- reactive({
    d <- idm %>% dplyr::filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") d <- d %>% dplyr::filter(DEPARTAMENTO == input$f_dep)
    if (!is.null(input$f_mun) && input$f_mun != "Todos") d <- d %>% dplyr::filter(MUNICIPIO == input$f_mun)
    d
  })
  
  # ---- Mapa base ----
  output$map_idm <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(-74.3, 4.6, 5)
  })
  
  # Helper: obtener código depto desde nombre (normalizado)
  get_cod_from_dep_name <- function(dep_name){
    if (is.null(dep_name) || dep_name == "Todos") return(NA_character_)
    dep_norm <- NUP(dep_name)
    i <- which(dep_sf$DEP_NORM_SHP == dep_norm)[1]
    if (is.finite(i)) return(dep_sf$COD_DPTO2[i])
    j <- which(dep_lookup$DEP_NORM == dep_norm)[1]
    if (is.finite(j)) return(dep_lookup$COD_DPTO2[j])
    NA_character_
  }
  
  # ---- Mapa (deptos -> mpios) ----
  observe({
    d <- base_filtrada()
    if (nrow(d) == 0) {
      leafletProxy("map_idm") %>% clearShapes() %>% clearControls()
      return()
    }
    
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      dd <- d %>% dplyr::group_by(COD_DANE_DPTO, DEPARTAMENTO) %>%
        dplyr::summarise(valor = mean(valor, na.rm=TRUE), .groups="drop") %>%
        dplyr::mutate(COD_DPTO2 = sprintf("%02d", as.integer(COD_DANE_DPTO)))
      
      shp <- dep_sf %>% dplyr::left_join(dd, by="COD_DPTO2") %>%
        dplyr::mutate(
          nombre = dplyr::coalesce(DEPARTAMENTO, DEPARTAMENTO_N),
          etq = paste0("<b>", nombre, "</b><br>IDM promedio: ", round(valor,2), "<br>Año: ", input$anio)
        )
      pal <- make_pal_bin(shp$valor, MAP_PALETTE)
      
      leafletProxy("map_idm", data = shp) %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(layerId=~COD_DPTO2, fillColor=~pal(valor), color="#666", weight=0.7,
                    fillOpacity=0.9, label=~lapply(etq, HTML),
                    highlightOptions = highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        addLegend("bottomright", pal = pal, values = ~valor, title = "IDM promedio")
    } else {
      sel_cod <- get_cod_from_dep_name(input$f_dep)
      if (is.na(sel_cod) || !nzchar(sel_cod)) {
        sel_cod <- d$COD_DANE_DPTO %>% unique() %>% sprintf("%02d", as.integer(.)) %>% .[1]
      }
      dd <- d %>% dplyr::group_by(COD_DANE_MUNI, MUNICIPIO) %>%
        dplyr::summarise(valor = mean(valor, na.rm=TRUE), .groups="drop") %>%
        dplyr::mutate(COD_MUN5 = sprintf("%05d", as.integer(COD_DANE_MUNI)))
      
      shp <- mun_sf %>% dplyr::filter(COD_DPTO2 == sel_cod) %>%
        dplyr::left_join(dd, by="COD_MUN5") %>%
        dplyr::mutate(
          MUNICIPIO = dplyr::coalesce(MUNICIPIO, MUNICIPIO_N),
          etq = paste0("<b>", MUNICIPIO, "</b><br>IDM: ", round(valor,2), "<br>Año: ", input$anio)
        )
      pal <- make_pal_bin(shp$valor, MAP_PALETTE); bb <- sf::st_bbox(shp)
      
      leafletProxy("map_idm", data = shp) %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(layerId=~COD_MUN5, fillColor=~pal(valor), color="#666", weight=0.4,
                    fillOpacity=0.9, label=~lapply(etq, HTML),
                    highlightOptions = highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        addLegend("bottomright", pal = pal, values = ~valor, title = "IDM") %>%
        fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  
  # Click depto -> drill-down (robusto a tildes/variantes de nombre)
  observeEvent(input$map_idm_shape_click, {
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      click <- input$map_idm_shape_click; req(click$id)
      cod <- sprintf("%02d", as.integer(click$id))
      
      nom_shp <- dep_sf$DEPARTAMENTO_N[match(cod, dep_sf$COD_DPTO2)]
      req(!is.na(nom_shp), nzchar(nom_shp))
      
      deps_year <- idm %>%
        dplyr::filter(ano == input$anio) %>%
        dplyr::distinct(DEPARTAMENTO) %>%
        dplyr::mutate(DEP_NORM = NUP(DEPARTAMENTO))
      
      nom_norm  <- NUP(nom_shp)
      nom_combo <- deps_year$DEPARTAMENTO[match(nom_norm, deps_year$DEP_NORM)]
      
      if (length(nom_combo) && !is.na(nom_combo) && nzchar(nom_combo)) {
        updateSelectInput(session, "f_dep", selected = nom_combo)
      }
    }
  }, ignoreInit = TRUE)
  
  # ---- Barras (color #009edb) ----
  output$bar_top <- renderPlotly({
    d <- base_filtrada(); req(nrow(d) > 0)
    d1 <- d %>% dplyr::group_by(DEPARTAMENTO, MUNICIPIO) %>%
      dplyr::summarise(val = mean(valor, na.rm=TRUE), .groups="drop") %>%
      dplyr::arrange(dplyr::desc(val)) %>% dplyr::slice_head(n = 10) %>%
      dplyr::mutate(lbl = paste0(MUNICIPIO, " (", DEPARTAMENTO, ")"))
    
    plot_ly(d1 %>% dplyr::arrange(val),
            x = ~val, y = ~factor(lbl, levels = d1$lbl[order(d1$val)]),
            type = "bar", orientation = "h",
            marker = list(color = "#009edb"),
            hovertemplate = "%{y}<br>IDM: %{x:.2f}<extra></extra>") %>%
      layout(xaxis = list(title = "IDM"),
             yaxis = list(title = "", automargin = TRUE),
             margin = list(l=10, r=10, b=10, t=10),
             showlegend = FALSE)
  })
  
  output$bar_depto <- renderPlotly({
    d <- base_filtrada(); req(nrow(d) > 0)
    d2 <- d %>% dplyr::group_by(DEPARTAMENTO) %>%
      dplyr::summarise(val = mean(valor, na.rm=TRUE), .groups="drop") %>%
      dplyr::arrange(dplyr::desc(val))
    
    plot_ly(d2 %>% dplyr::arrange(val),
            x=~val, y=~factor(DEPARTAMENTO, levels = d2$DEPARTAMENTO[order(d2$val)]),
            type="bar", orientation="h",
            marker = list(color = "#009edb"),
            hovertemplate = "%{y}<br>Promedio IDM: %{x:.2f}<extra></extra>") %>%
      layout(xaxis=list(title="Promedio IDM"),
             yaxis=list(title="", automargin = TRUE),
             margin=list(l=10,r=10,b=10,t=10),
             showlegend = FALSE)
  })
}

shinyApp(ui, server)


