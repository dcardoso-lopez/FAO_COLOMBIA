# =========================================================
# Dashboard ICA Pecuaria — Mapa + Serie temporal + Top-10
# =========================================================

# ---- Paquetes ----
pkgs <- c(
  "shiny","bslib","shinyWidgets",
  "tidyverse","data.table","janitor","lubridate","scales",
  "sf","leaflet","htmltools","plotly","stringr","DT"
)
suppressWarnings(invisible(lapply(pkgs, require, character.only = TRUE)))
options(stringsAsFactors = FALSE)

validate <- shiny::validate
need     <- shiny::need

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
safe_first <- function(x, default = "?"){ x <- x[!is.na(x)]; if (length(x)==0) default else x[1] }

# ---- Utilidad: Title Case en español (sin capitalizar conectores) ----
title_case_es <- function(x){
  x <- stringr::str_trim(as.character(x))
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  
  small_words <- c(
    "de","del","la","las","los","y","e","o","u","a",
    "en","el","al","da","do","das","dos"
  )
  
  vapply(x, function(s){
    if (is.na(s) || s == "") return(NA_character_)
    
    words <- strsplit(s, "\\s+")[[1]]
    
    words <- vapply(seq_along(words), function(i){
      w <- words[i]
      if (i > 1 && w %in% small_words) {
        w
      } else {
        stringr::str_to_title(w, locale = "es")
      }
    }, character(1))
    
    paste(words, collapse = " ")
  }, character(1))
}

# ---- Rutas / datos GOLDEN ----
golden_dir <- "data"
ica_bovino  <- readRDS(file.path(golden_dir, "101_ICA_CensoPecuario-Bovino.rds"))
ica_porcino <- readRDS(file.path(golden_dir, "102_ICA_CensoPecuario-Porcino.rds"))
ica_bcoe    <- readRDS(file.path(golden_dir, "103_ICA_CensoPecuario-BCOE.rds"))
ica_aviar   <- readRDS(file.path(golden_dir, "104_ICA_CensoPecuario-Aviar.rds"))

# ---- Rutas shapefiles ----
ruta_shp_mpios <- file.path(golden_dir, "shp/MGN_ANM_MPIOS.shp")
ruta_shp_dptos <- file.path(golden_dir, "shp/MGN_ANM_DPTOS.shp")
mpios_raw <- sf::st_read(ruta_shp_mpios, quiet = TRUE)
dptos_raw <- sf::st_read(ruta_shp_dptos, quiet = TRUE)

# =========================================================
# 1) Normalizadores y shapefiles estandarizados
# =========================================================
norm2 <- function(x) stringr::str_pad(as.character(x), 2, pad="0")
norm5 <- function(x) stringr::str_pad(as.character(x), 5, pad="0")

# --- MUNICIPIOS ---
stopifnot("MPIO_CDPMP" %in% names(mpios_raw))
muni_name_cands <- c("MUNICIPIO_D","MPIO_CNMBR","NOMBRE_MPIO","NOMBRE_MUNICIP","MUNICIPIO","NOMBRE")
muni_name_col <- muni_name_cands[muni_name_cands %in% names(mpios_raw)][1]
stopifnot(!is.na(muni_name_col))
dpto2_cands_mpio <- c("DPTO_CCDGO","COD_DPTO","DPTO","CODIGO_DEPTO","DPTO_COD")
dpto2_mpio_col <- dpto2_cands_mpio[dpto2_cands_mpio %in% names(mpios_raw)][1]

mpios_sf <- mpios_raw %>%
  mutate(
    CODMUN      = norm5(.data[["MPIO_CDPMP"]]),
    DPTO2       = if (!is.na(dpto2_mpio_col)) norm2(.data[[dpto2_mpio_col]]) else substr(CODMUN, 1, 2),
    MUNICIPIO_D = .data[[muni_name_col]]
  ) %>%
  mutate(
    MUNICIPIO_D = title_case_es(MUNICIPIO_D)
  ) %>%
  st_transform(4326) %>%
  st_make_valid()

# --- DEPARTAMENTOS ---
depto_name_cands <- c("DEPARTAMENTO_D","DPTO_CNMBR","NOMBRE_DPT","NOMBRE_DEPTO","DEPARTAMEN","DEPARTAMENTO","NOMBRE")
depto_code_cands <- c("DPTO_CCDGO","COD_DPTO","DPTO","CODIGO_DPTO","DPTO_COD")
depto_name_col <- depto_name_cands[depto_name_cands %in% names(dptos_raw)][1]
depto_code_col <- depto_code_cands[depto_code_cands %in% names(dptos_raw)][1]
stopifnot(!is.na(depto_name_col), !is.na(depto_code_col))
dptos_sf <- dptos_raw %>%
  mutate(
    DPTO2          = norm2(.data[[depto_code_col]]),
    DEPARTAMENTO_D = .data[[depto_name_col]]
  ) %>%
  mutate(
    DEPARTAMENTO_D = title_case_es(DEPARTAMENTO_D)
  ) %>%
  st_transform(4326) %>%
  st_make_valid()

# =========================================================
# 2) GOLDEN — estandarizar
# =========================================================
std_golden <- function(df, valor_col, etiqueta){
  df <- janitor::clean_names(df)
  pick_first <- function(nms, cands) { hit <- cands[cands %in% nms]; if (!length(hit)) NA_character_ else hit[1] }
  mcol <- pick_first(names(df), c("cod_dane_munic_d","cod_dane_mpio","cod_mpio","cod_municipio","codigo_mpio","mpio"))
  ycol <- pick_first(names(df), c("ano","anio","year"))
  dcol <- pick_first(names(df), c("dpto_ccdgo","cod_dpto","dpto","codigo_dpto","dpto_cod"))
  dncol<- pick_first(names(df), c("departamento_d","departamento","nombre_depto","departamen"))
  stopifnot(!is.na(ycol), !is.na(mcol))
  vcol <- tolower(valor_col); stopifnot(vcol %in% names(df))
  df %>% mutate(
    year   = .data[[ycol]],
    CODMUN = norm5(.data[[mcol]]),
    DPTO2  = if (!is.na(dcol)) norm2(.data[[dcol]]) else substr(CODMUN, 1, 2),
    DEPARTAMENTO_D = if (!is.na(dncol)) title_case_es(.data[[dncol]]) else NA_character_,
    valor  = suppressWarnings(as.numeric(.data[[vcol]]))
  ) %>%
    group_by(year, CODMUN, DPTO2, DEPARTAMENTO_D) %>%
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    mutate(especie = etiqueta)
}
need_one <- function(df, opts){ nm <- opts[opts %in% names(df)][1]; if (is.na(nm)) stop(paste("No encuentro:", paste(opts, collapse=", "))); nm }

# Bovinos
if ("total_bovinos" %in% names(ica_bovino)) {
  bov_total_col <- "total_bovinos"
} else {
  cols <- grep("^(hembras|machos|terneros|terneras)_", names(ica_bovino), value = TRUE)
  stopifnot(length(cols) > 0)
  ica_bovino[["tmp_bov_total"]] <- rowSums(sapply(ica_bovino[cols], function(x) suppressWarnings(as.numeric(x))), na.rm = TRUE)
  bov_total_col <- "tmp_bov_total"
}
g_bov  <- std_golden(ica_bovino, bov_total_col, "Bovinos")
# Porcinos
stopifnot("total_porcinos" %in% names(ica_porcino))
g_porc <- std_golden(ica_porcino, "total_porcinos", "Porcinos")
# BCOE
g_buf <- std_golden(ica_bcoe, need_one(ica_bcoe, c("total_bufalos","total_búfalos")), "Búfalos")
g_equ <- std_golden(ica_bcoe, need_one(ica_bcoe, c("total_equinos")), "Equinos")
g_cap <- std_golden(ica_bcoe, need_one(ica_bcoe, c("total_caprinos")), "Caprinos")
g_ovi <- std_golden(ica_bcoe, need_one(ica_bcoe, c("total_ovinos")),  "Ovinos")
# Aviar: ocupada + traspatio
prep_aves_combo <- function(df){
  df <- janitor::clean_names(df)
  pick_first <- function(nms, cands){ hit <- cands[cands %in% nms]; if (!length(hit)) NA_character_ else hit[1] }
  col_ocup <- pick_first(names(df), c("total_aves_capacidad_ocupada","total_aves_ocupada"))
  col_trap <- pick_first(names(df), c("total_aves_traspatio","total_aves_trapatio","total_aves_trapAtio"))
  stopifnot(!is.na(col_ocup), !is.na(col_trap))
  df$aves_ocupada_mas_traspatio <- suppressWarnings(as.numeric(df[[col_ocup]])) +
    suppressWarnings(as.numeric(df[[col_trap]]))
  df
}
ica_aviar_combo <- prep_aves_combo(ica_aviar)
g_aves <- std_golden(ica_aviar_combo, "aves_ocupada_mas_traspatio", "Aves")

# GOLDEN unificada
golden <- bind_rows(g_bov, g_porc, g_buf, g_equ, g_cap, g_ovi, g_aves)
golden <- golden %>% dplyr::filter(DEPARTAMENTO_D=="Santander")

# =========================================================
# 3) Utilidades de formato y cuartiles (tipo NOAA)
# =========================================================

fmt_num <- function(x, accuracy = 1){
  scales::number(
    x,
    accuracy     = accuracy,
    big.mark     = ".",
    decimal.mark = ","
  )
}

pal4_vec <- grDevices::colorRampPalette(
  c("#f0f8f4","#9fd9c0","#2ea56f","#0f7e4f")
)(4)

make_bins4 <- function(values){
  v <- suppressWarnings(as.numeric(values))
  v <- v[is.finite(v)]
  if (!length(v)) return(seq(0,4))
  qs <- quantile(v, probs = seq(0,1,length.out=5), na.rm=TRUE, type=7)
  qs <- sort(unique(as.numeric(qs)))
  if (length(qs) < 5){
    r <- range(v, na.rm=TRUE)
    if (r[1] == r[2]) r <- c(0, max(1, r[2]))
    qs <- pretty(r, n=4)
  }
  if (length(qs) < 5) qs <- seq(min(qs), max(qs), length.out=5)
  qs
}

build_bins_labels <- function(values){
  v <- suppressWarnings(as.numeric(values))
  v <- v[is.finite(v)]
  if (!length(v)) v <- c(0,1)
  
  bins <- make_bins4(v)
  pal  <- leaflet::colorBin(
    palette  = pal4_vec,
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
      sa <- fmt_num(a, accuracy = 1)
      sb <- fmt_num(b, accuracy = 1)
      if (i == 1) {
        sprintf("%s – %s", sa, sb)
      } else {
        sprintf("> %s – %s", sa, sb)
      }
    },
    character(1)
  )
  mids <- (bins[-length(bins)] + bins[-1]) / 2
  cols <- pal(mids)
  
  list(
    bins   = bins,
    pal    = pal,
    labels = labs,
    colors = cols
  )
}

# =========================================================
# 4) UI — Tab único (explorador, 2 columnas)
# =========================================================
especies_all <- sort(unique(golden$especie))

ui <- fluidPage(
  theme = bslib::bs_theme(
    version      = 5,
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight")
  ),
  tags$head(
    tags$style(HTML("
      body{
        background:#ffffff;
      }
      :root{ --brand-border:#a1d99b; }

      .wrap{
        max-width:1360px;
        margin:0 auto;
        padding:16px 20px 32px;
      }

      h2#app-title {
        text-align:center;
        margin-top:10px;
        margin-bottom:5px;
        font-weight:800;
        letter-spacing:.3px;
      }

      .card{
        background:#ffffff;
        border:1px solid var(--brand-border) !important;
        border-radius:16px;
        padding:12px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
        margin-bottom:12px;
      }

      .card-title{
        font-weight:700;
        font-size:16px;
        margin-bottom:8px;
        color:#111827;
      }

      .section-title{ font-weight:800; margin-bottom:8px; }
      
      .filter-label{
        font-family:'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-weight:500;
        font-size:14px;
        margin-bottom:4px;
        color:#000000;
      }

      .filters-grid{
        display:grid;
        grid-template-columns: 1.3fr 1fr 1fr 1fr;
        column-gap:16px;
        row-gap:10px;
        align-items:end;
      }

      .filter-block{
        width:100%;
      }

      .filter-block .form-select,
      .filter-block .bootstrap-select > .dropdown-toggle,
      .filter-block .selectize-input {
        width:100% !important;
      }

      @media (max-width: 992px){
        .filters-grid{
          grid-template-columns: 1fr 1fr;
        }
      }
      @media (max-width: 576px){
        .filters-grid{
          grid-template-columns: 1fr;
        }
      }

      .bootstrap-select{
        border:none !important;
        background-color:transparent !important;
      }

      .form-select,
      .bootstrap-select > .dropdown-toggle,
      .selectize-input {
        border:1px solid var(--brand-border) !important;
        border-radius:10px !important;
        box-shadow:none !important;
        font-family:'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-size:14px;
        font-weight:500;
        color:#000000;
        background-color:#ffffff !important;
      }

      .form-select:focus,
      .bootstrap-select > .dropdown-toggle:focus,
      .selectize-input.focus {
        border:1px solid var(--brand-border) !important;
        box-shadow:0 0 0 .15rem rgba(161,217,155,.25) !important;
      }

      .bootstrap-select>.dropdown-toggle .filter-option-inner-inner{
        font-family:'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-size:14px;
        font-weight:500;
        color:#000000;
      }

      .bootstrap-select .dropdown-menu{
        border:1px solid var(--brand-border);
        max-height:none !important;
        overflow:hidden !important;
      }
      .bootstrap-select .dropdown-menu .inner{
        max-height:230px !important;
        overflow-y:auto !important;
      }
      .bootstrap-select .bs-searchbox .form-control{
        border:1px solid var(--brand-border);
      }

      .card .leaflet-container,
      .card .html-widget{
        border:none !important;
        border-radius:12px;
      }
      .leaflet-control.legend, .info.legend {
        border:none !important;
        border-radius:10px !important;
      }

      .map-note{
        margin-top:8px;
        font-size:11px;
        color:#555555;
        font-style:italic;
      }

      .viz-grid{
        display:grid;
        grid-template-columns: 1.05fr 1fr;
        gap:12px;
        align-items:stretch;
      }

      .viz-right{
        display:grid;
        grid-template-rows: 1fr 1fr;
        gap:12px;
      }

      .card-mapa,
      .card-viz{
        display:flex;
        flex-direction:column;
      }

      .card-mapa .leaflet-container,
      .card-viz .html-widget{
        flex:1;
        min-height:350px;
      }

      @media (max-width: 768px){
        .viz-grid{
          grid-template-columns: 1fr;
        }
        .viz-right{
          grid-template-rows: auto auto;
        }
      }
    "))
  ),
  
  div(
    class = "wrap",
    
    h2("", id = "app-title"),
    
    # ======== Filtros ========
    div(
      class = "card",
      div(
        class = "filters-grid",
        
        # 1. Año
        div(
          class = "filter-block",
          div(class="filter-label","¿Qué año analizamos?"),
          uiOutput("anio_ui")
        ),
        
        # 2. Departamento (default basado en golden: Santander)
        div(
          class = "filter-block",
          div(class="filter-label","¿En qué departamento?"),
          {
            dpts <- dptos_sf |> sf::st_drop_geometry() |>
              dplyr::select(DPTO2, DEPARTAMENTO_D) |>
              dplyr::distinct() |>
              dplyr::arrange(DEPARTAMENTO_D)
            
            # Tomar como default el/los departamentos presentes en golden
            dep_golden <- golden |>
              dplyr::select(DPTO2, DEPARTAMENTO_D) |>
              dplyr::distinct()
            
            default_dep <- if (nrow(dep_golden) == 1) dep_golden$DPTO2[1] else "Todos"
            
            selectInput(
              "depto", NULL,
              choices  = c(
                "Todos" = "Todos",
                stats::setNames(dpts$DPTO2, dpts$DEPARTAMENTO_D)
              ),
              selected = default_dep
            )
          }
        ),
        
        # 3. Municipio
        div(
          class = "filter-block",
          div(class="filter-label","¿Algún municipio en particular?"),
          uiOutput("muni_ui")
        ),
        
        # 4. Tipo de animal
        div(
          class = "filter-block",
          div(class = "filter-label","¿Qué tipo de animal?"),
          shinyWidgets::pickerInput(
            "especie", NULL,
            choices  = especies_all,
            selected = especies_all[1],
            options  = list(`live-search` = TRUE, size = 7)
          )
        )
      )
    ),
    
    # ======== Visualizaciones ========
    div(
      class = "viz-grid",
      
      # Columna 1: MAPA
      div(
        class = "card card-mapa",
        div(class = "card-title", uiOutput("titulo_mapa")),
        div(
          style = "display:flex; gap:10px; align-items:center; margin-bottom:8px;",
          actionButton("volver", "◀ Volver a Departamentos", class = "btn btn-light"),
          strong(textOutput("nivel_txt", inline = TRUE))
        ),
        leafletOutput("mapa", height = "100%"),
        div(
          class = "map-note",
          "Nota: los rangos de color del mapa se construyen con cuartiles (4 clases) del inventario de animales según el subconjunto de datos filtrado."
        )
      ),
      
      # Columna 2: SERIE + TOP10
      div(
        class = "viz-right",
        div(
          class = "card card-viz",
          div(class = "card-title", uiOutput("titulo_serie")),
          plotlyOutput("serie", height = "100%")
        ),
        div(
          class = "card card-viz",
          div(class = "card-title", uiOutput("titulo_top")),
          plotlyOutput("top10", height = "100%")
        )
      )
    )
  )
)

# =========================================================
# 5) SERVER
# =========================================================
server <- function(input, output, session){
  
  pal_vec  <- c("#0f7e4f", "#2ea56f", "#6ac39c", "#9fd9c0", "#cdeee0", "#f0f8f4")
  bar_fill <- "#2ea56f"
  
  fmt_km <- function(x){
    if (is.na(x) || !is.finite(x)) return(NA_character_)
    ax <- abs(x)
    if (ax >= 1e6) {
      base <- x / 1e6
      paste0(fmt_num(base, accuracy = 0.1), " M")
    } else if (ax >= 1e3) {
      base <- x / 1e3
      paste0(fmt_num(base, accuracy = 0.1), " K")
    } else {
      fmt_num(x, accuracy = 1)
    }
  }
  
  # --- UI dinámico ---
  output$anio_ui <- renderUI({
    req(input$especie)
    yy <- golden |> dplyr::filter(especie==input$especie) |>
      dplyr::pull(year) |> unique() |> sort()
    selectInput("anio", label = NULL, choices = yy, selected = max(yy))
  })
  
  output$muni_ui <- renderUI({
    base <- mpios_sf |> sf::st_drop_geometry()
    if (!is.null(input$depto) && input$depto != "Todos") {
      base <- dplyr::filter(base, DPTO2 == input$depto)
    }
    muni <- base |>
      dplyr::transmute(CODMUN, MUNICIPIO_D) |>
      dplyr::arrange(MUNICIPIO_D)
    
    shinyWidgets::pickerInput(
      "muni", label = NULL,
      choices  = c("Todos", stats::setNames(muni$CODMUN, muni$MUNICIPIO_D)),
      selected = "Todos",
      options  = list(`live-search` = TRUE, size = 7)
    )
  })
  
  output$nivel_txt <- renderText({
    if (is.null(input$depto) || input$depto == "Todos") "Nivel: Departamentos" else "Nivel: Municipios"
  })
  
  observeEvent(input$volver, ignoreInit = TRUE, {
    updateSelectInput(session,"depto", selected = "Todos")
    shinyWidgets::updatePickerInput(session,"muni",  selected = "Todos")
  })
  
  # --- Datos base ---
  datos_base <- reactive({
    req(input$especie)
    df <- golden |> dplyr::filter(especie==input$especie)
    if (!is.null(input$anio))  df <- df |> dplyr::filter(year==input$anio)
    if (!is.null(input$depto) && input$depto!="Todos") df <- df |> dplyr::filter(DPTO2==input$depto)
    if (!is.null(input$muni)  && input$muni!="Todos")  df <- df |> dplyr::filter(CODMUN==input$muni)
    validate(need(nrow(df)>0, "Sin datos para los filtros actuales"))
    df
  })
  
  datos_dpto <- reactive({
    df <- datos_base() |>
      dplyr::group_by(DPTO2) |>
      dplyr::summarise(valor = sum(as.numeric(valor), na.rm=TRUE), .groups="drop")
    dptos_sf |>
      dplyr::left_join(df, by="DPTO2") |>
      dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
  })
  
  datos_mpio <- reactive({
    df <- datos_base() |>
      dplyr::group_by(CODMUN) |>
      dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
    
    mpios_sf |>
      dplyr::left_join(df, by = "CODMUN")
  })
  
  # --- Títulos ---
  output$titulo_mapa <- renderUI({
    req(input$especie, input$anio)
    strong("¿En cuáles territorios se concentran la mayor cantidad de los inventarios de animales?")
  })
  
  output$titulo_serie <- renderUI({
    strong("¿Cómo ha evolucionado el inventario de animales a lo largo del tiempo?")
  })
  
  output$titulo_top <- renderUI({
    req(input$especie, input$anio)
    strong("Top 10 de municipios con mayor inventario de animales")
  })
  
  # --- MAPA inicial ---
  output$mapa <- renderLeaflet({
    req(input$especie, input$anio)
    sf_m <- datos_dpto()
    bl   <- build_bins_labels(sf_m$valor)
    pal  <- bl$pal
    leaflet(sf_m) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        layerId = ~DPTO2,
        fillColor = ~pal(valor),
        weight = 0.7, color = "#3a6b57", fillOpacity = 0.9,
        label  = ~DEPARTAMENTO_D,
        labelOptions = leaflet::labelOptions(direction = "auto", textsize = "12px", sticky = TRUE,
                                             opacity = 0.9, style = list("font-weight" = "600")),
        highlightOptions = leaflet::highlightOptions(color = "#1f5d46", weight = 2, bringToFront = TRUE)
      ) %>%
      addLegend(
        position = "bottomright",
        colors   = bl$colors,
        labels   = bl$labels,
        opacity  = 0.9,
        title    = "Cantidad"
      )
  })
  
  # --- Redibujar mapas ---
  dibujar_deptos <- function() {
    sf_m <- datos_dpto()
    bl   <- build_bins_labels(sf_m$valor)
    pal  <- bl$pal
    leafletProxy("mapa", data = sf_m) %>%
      clearPopups() %>% clearShapes() %>% clearControls() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        layerId=~DPTO2, fillColor=~pal(valor),
        weight=0.7, color="#3a6b57", fillOpacity=0.9,
        label=~DEPARTAMENTO_D,
        labelOptions = leaflet::labelOptions(
          direction="auto", textsize="12px", sticky=TRUE,
          opacity=0.9, style=list("font-weight"="600")
        ),
        highlightOptions = leaflet::highlightOptions(
          color="#1f5d46", weight=2, bringToFront=TRUE
        )
      ) %>%
      addLegend(
        "bottomright",
        colors = bl$colors,
        labels = bl$labels,
        opacity = 0.9,
        title   = "Cantidad"
      )
  }
  
  dibujar_mpios <- function() {
    sf_all <- datos_mpio()
    
    if (!is.null(input$depto) && input$depto != "Todos") {
      sf_all <- sf_all |> dplyr::filter(DPTO2 == input$depto)
    }
    
    if (!is.null(input$muni) && input$muni != "Todos") {
      sf_sel <- sf_all |>
        dplyr::filter(CODMUN == input$muni & is.finite(valor))
      
      lp <- leafletProxy("mapa", data = sf_all) %>%
        clearPopups() %>% clearShapes() %>% clearControls() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          data        = sf_all,
          layerId     = ~CODMUN,
          fillColor   = "#e5e7eb",
          fillOpacity = 0.8,
          color       = "#b3b3b3",
          weight      = 0.4,
          label       = ~MUNICIPIO_D,
          labelOptions = leaflet::labelOptions(
            direction = "auto", textsize = "11px", sticky = TRUE,
            opacity = 0.9, style = list("font-weight" = "600")
          ),
          highlightOptions = leaflet::highlightOptions(
            color = "#1f5d46", weight = 2, bringToFront = TRUE
          )
        )
      
      if (nrow(sf_sel) > 0) {
        val_sel <- sf_sel$valor[1]
        col_sel <- pal4_vec[4]
        
        lp <- lp %>%
          addPolygons(
            data        = sf_sel,
            layerId     = ~CODMUN,
            fillColor   = col_sel,
            fillOpacity = 0.9,
            color       = "#3a6b57",
            weight      = 0.8,
            label       = ~MUNICIPIO_D,
            labelOptions = leaflet::labelOptions(
              direction = "auto", textsize = "11px", sticky = TRUE,
              opacity = 0.9, style = list("font-weight" = "600")
            )
          ) %>%
          addLegend(
            "bottomright",
            colors  = col_sel,
            labels  = fmt_km(val_sel),
            opacity = 0.9,
            title   = "Cantidad"
          )
      }
    } else {
      bl  <- build_bins_labels(sf_all$valor)
      pal <- bl$pal
      
      leafletProxy("mapa", data = sf_all) %>%
        clearPopups() %>% clearShapes() %>% clearControls() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          layerId     = ~CODMUN,
          fillColor   = ~pal(valor),
          weight      = 0.4,
          color       = "#3a6b57",
          fillOpacity = 0.9,
          label       = ~MUNICIPIO_D,
          labelOptions = leaflet::labelOptions(
            direction = "auto", textsize = "11px", sticky = TRUE,
            opacity = 0.9, style = list("font-weight" = "600")
          ),
          highlightOptions = leaflet::highlightOptions(
            color = "#1f5d46", weight = 2, bringToFront = TRUE
          )
        ) %>%
        addLegend(
          "bottomright",
          colors  = bl$colors,
          labels  = bl$labels,
          opacity = 0.9,
          title   = "Cantidad"
        )
    }
  }
  
  # --- Reactividad de filtros ---
  observeEvent(list(input$especie, input$anio), {
    if (is.null(input$depto) || input$depto == "Todos") dibujar_deptos() else dibujar_mpios()
  }, ignoreInit = TRUE)
  
  observeEvent(input$depto, {
    req(input$especie, input$anio)
    shinyWidgets::updatePickerInput(session, "muni", selected = "Todos")
    if (is.null(input$depto) || input$depto == "Todos") {
      dibujar_deptos()
    } else {
      dibujar_mpios()
      geom <- dptos_sf |> dplyr::filter(DPTO2 == input$depto)
      if (nrow(geom) == 1) {
        bb <- sf::st_bbox(geom)
        leafletProxy("mapa") |> fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$muni, {
    req(input$especie, input$anio)
    if (!is.null(input$muni) && input$muni != "Todos") {
      dibujar_mpios()
      geom <- mpios_sf |> dplyr::filter(CODMUN == input$muni)
      if (nrow(geom) == 1) {
        bb <- sf::st_bbox(geom)
        leafletProxy("mapa") |> fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
      }
    } else {
      if (!is.null(input$depto) && input$depto != "Todos") dibujar_mpios() else dibujar_deptos()
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$mapa_shape_click, {
    req(input$especie, input$anio)
    cl <- input$mapa_shape_click; req(cl$id)
    en_deptos <- is.null(input$depto) || input$depto == "Todos"
    if (en_deptos) {
      updateSelectInput(session, "depto", selected = cl$id)
    } else {
      if (cl$id %in% (mpios_sf |> sf::st_drop_geometry() |> dplyr::pull(CODMUN))) {
        shinyWidgets::updatePickerInput(session, "muni", selected = cl$id)
      }
    }
  })
  
  # --- Serie temporal ---
  output$serie <- renderPlotly({
    req(input$especie)
    
    df <- golden |> dplyr::filter(especie == input$especie)
    if (!is.null(input$depto) && input$depto != "Todos") df <- df |> dplyr::filter(DPTO2 == input$depto)
    if (!is.null(input$muni)  && input$muni  != "Todos") df <- df |> dplyr::filter(CODMUN == input$muni)
    
    ts <- df %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(valor = sum(as.numeric(valor), na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(
        date      = as.Date(paste0(as.integer(year), "-01-01")),
        valor_fmt = vapply(valor, fmt_km, character(1))
      ) %>%
      dplyr::arrange(date)
    
    validate(need(nrow(ts) > 0, "Sin datos para los filtros actuales"))
    
    rng      <- range(ts$valor, na.rm = TRUE)
    tickvals <- pretty(rng, n = 6)
    ticktext <- vapply(tickvals, fmt_km, character(1))
    
    plot_ly(
      ts,
      x    = ~date,
      y    = ~valor,
      type = "scatter",
      mode = "lines+markers",
      line   = list(color = "#2ea56f", width = 2),
      marker = list(size = 6, color = "#2ea56f"),
      text   = ~paste0(
        "Año: ", format(date, "%Y"),
        "<br>Inventario: ", valor_fmt
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(
          title    = "",
          tickformat = "%Y",
          showgrid = FALSE
        ),
        yaxis = list(
          title    = "Cantidad",
          tickvals = tickvals,
          ticktext = ticktext,
          showgrid = TRUE
        ),
        margin = list(l = 40, r = 20, t = 10, b = 40)
      )
  })
  
  # --- Top-10 ---
  output$top10 <- renderPlotly({
    df <- datos_base() |>
      dplyr::mutate(valor = suppressWarnings(as.numeric(valor))) |>
      dplyr::filter(is.finite(valor)) |>
      dplyr::group_by(CODMUN) |>
      dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups="drop") |>
      dplyr::left_join(
        mpios_sf |> sf::st_drop_geometry() |> dplyr::select(CODMUN, MUNICIPIO_D, DPTO2),
        by = "CODMUN"
      ) |>
      dplyr::left_join(
        dptos_sf |> sf::st_drop_geometry() |> dplyr::select(DPTO2, DEPARTAMENTO_D),
        by = "DPTO2"
      )
    
    top <- df |>
      dplyr::filter(!is.na(valor)) |>
      dplyr::arrange(dplyr::desc(valor)) |>
      dplyr::slice_head(n = 10) |>
      dplyr::mutate(
        MUNICIPIO_D = title_case_es(MUNICIPIO_D),
        lbl         = MUNICIPIO_D,
        valor_fmt   = vapply(valor, fmt_km, character(1))
      )
    
    validate(need(nrow(top) > 0, "Sin datos para el Top-10"))
    
    g <- ggplot(
      top,
      aes(
        x    = valor,
        y    = reorder(lbl, valor),
        text = paste0(lbl," — ", valor_fmt)
      )
    ) +
      geom_col(fill = bar_fill) +
      geom_text(
        aes(
          x     = valor / 2,
          label = valor_fmt
        ),
        color    = "white",
        fontface = "bold",
        size     = 3.8
      ) +
      scale_x_continuous(labels = function(x) vapply(x, fmt_km, character(1))) +
      labs(x = "Cantidad", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(margin = margin(r = -5))
      )
    
    p <- ggplotly(g, tooltip = "text")
    if (is.null(p$x$layout$margin)) p$x$layout$margin <- list()
    p$x$layout$margin$l <- 90
    p$x$layout$margin$r <- 5
    p
  })
}

shinyApp(ui, server)

