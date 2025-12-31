# app_idm.R — Terridata IDM (una sola ventana, zoom top-right, cuantiles, title_case ES, % en mapa y barras)
suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(leaflet); library(sf); library(dplyr)
  library(scales); library(htmltools); library(plotly)
  library(stringi); library(stringr); library(readr); library(tidyr)
})
options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

# ---------- Rutas ----------
data_dir <- "data"
ruta_idm     <- file.path(data_dir, "071_DNP_Terridata_IDM.rds")
ruta_pob     <- file.path(data_dir, "051_DANE_Proyecciones_P_total.rds")  # opcional
ruta_shp_mun <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")
ruta_shp_dep <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")

# ---------- Utils ----------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
norm_txt <- function(x) ifelse(is.na(x), NA_character_, stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII"))
NUP      <- function(x) toupper(norm_txt(x))

# Title Case en español
title_case_es <- function(x){
  if (is.null(x)) return(x)
  small <- c("de","del","la","las","los","y","e","o","u","en","el","al","a","para","por","con","sin",
             "sobre","entre","ante","bajo","cabe","contra","desde","hacia","hasta","segun","según")
  x <- as.character(x)
  vapply(seq_along(x), function(i){
    xi <- x[i]; if (is.na(xi) || !nzchar(xi)) return(xi)
    xi <- stringr::str_to_lower(xi, locale="es")
    w  <- unlist(strsplit(xi, "\\s+"))
    if (!length(w)) return("")
    w  <- ifelse(seq_along(w)==1, stringr::str_to_title(w, locale="es"),
                 ifelse(w %in% small, w, stringr::str_to_title(w, locale="es")))
    paste(w, collapse=" ")
  }, character(1))
}

# Paleta y helper SIEMPRE en CUANTILES
MAP_PALETTE <- c("#e0f3fa", "#99d5ec", "#4bb5e1", "#0099cc", "#005b88")

# Formato ES: % y números
fmt_pct <- function(x, acc = 0.1){
  ifelse(
    is.na(x),
    "NA",
    scales::percent(
      x,
      accuracy     = acc,
      decimal.mark = ","
    )
  )
}
fmt_num <- function(x, digs = 1){
  ifelse(
    is.na(x),
    "NA",
    scales::number(
      x,
      accuracy     = digs,
      big.mark     = ".",
      decimal.mark = ","
    )
  )
}
# Para porcentajes que ya están en escala 0–100
fmt_pct100 <- function(x, acc = 0.1){
  fmt_pct(x/100, acc = acc)
}

make_pal_bin <- function(values, palette = MAP_PALETTE, n_bins = length(MAP_PALETTE)) {
  vals <- suppressWarnings(as.numeric(values))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) vals <- 0
  
  qs   <- stats::quantile(vals, probs = seq(0, 1, length.out = n_bins), na.rm = TRUE, type = 7)
  bins <- sort(unique(as.numeric(qs)))
  
  # Por si todos los valores son iguales
  if (length(bins) < 2) {
    v <- unique(vals)[1]
    bins <- c(v - 1e-9, v + 1e-9)
  }
  leaflet::colorBin(palette, domain = vals, bins = bins, na.color = "#f0f0f0")
}

pick_col  <- function(df, primary, pattern){
  nms <- names(df)
  if (primary %in% nms) return(primary)
  alt <- nms[grepl(pattern, nms, ignore.case = TRUE)]
  if (length(alt)) alt[1] else NA_character_
}
safe_pull <- function(df, col) if (!is.na(col) && col %in% names(df)) df[[col]] else NA

# ===== Tarjeta de cortes tipo UPRA =====
compute_breaks <- function(values, n_bins){
  vals <- suppressWarnings(as.numeric(values)); vals <- vals[is.finite(vals)]
  if (!length(vals)) vals <- 0
  qs <- stats::quantile(vals, probs = seq(0, 1, length.out = n_bins), na.rm = TRUE, type = 7)
  sort(unique(as.numeric(qs)))
}

# Etiqueta de intervalo para porcentajes en escala 0–100, con ">" desde el segundo
format_interval_label_pct <- function(a, b, is_first = TRUE){
  fa <- fmt_pct100(a, acc = 0.1)
  fb <- fmt_pct100(b, acc = 0.1)
  if (is_first) {
    sprintf("%s – %s", fa, fb)
  } else {
    sprintf(">%s – %s", fa, fb)
  }
}

# Para la cajita ℹ️ (lista <li>…)
format_breaks_list <- function(breaks){
  if (length(breaks) < 2) return("<li>Sin información suficiente para segmentar</li>")
  items <- vapply(
    seq_len(length(breaks) - 1),
    function(i){
      lbl <- format_interval_label_pct(breaks[i], breaks[i+1], is_first = (i == 1))
      sprintf("<li>%s</li>", lbl)
    },
    character(1)
  )
  paste(items, collapse = "\n")
}

# Para la leyenda del mapa (vector de etiquetas lisas)
build_interval_labels_pct <- function(breaks){
  if (length(breaks) < 2) return(character(0))
  vapply(
    seq_len(length(breaks) - 1),
    function(i){
      format_interval_label_pct(breaks[i], breaks[i+1], is_first = (i == 1))
    },
    character(1)
  )
}

build_info_html <- function(breaks){
  htmltools::HTML(sprintf(
    '<div class="info-title">ℹ️&nbsp;Cortes (cuantiles)</div>
       <div class="info-text">Los colores se calculan con cuantiles del IDM.</div>
       <ul class="info-list">%s</ul>', format_breaks_list(breaks)))
}

# ---------- Cargar IDM ----------
idm_raw <- readRDS(ruta_idm)

# *** Filtro fijo: solo Atlántico ***
idm_raw <- idm_raw %>% dplyr::filter(DEPARTAMENTO_D == "ATLÁNTICO")

col_ano     <- pick_col(idm_raw, "ano", "^a(n|ñ)o$|year")
col_dep_cod <- pick_col(idm_raw, "COD_DANE_DPTO_D", "DPTO|DEPTO|DANE.*DEP|COD.*DEP|DEPART")
col_dep_nom <- pick_col(idm_raw, "DEPARTAMENTO_D", "DEPARTA")
col_mun_cod <- pick_col(idm_raw, "COD_DANE_MUNIC_D", "MUNI.*COD|COD.*MUNI|DANE.*MUNI|COD_MUN5|MPIO")
col_mun_nom <- pick_col(idm_raw, "MUNICIPIO_D", "MUNICIP")
col_val     <- pick_col(idm_raw, "valor", "valor|indice|idm")

DEP_ORI <- as.character(safe_pull(idm_raw, col_dep_nom))
MUN_ORI <- as.character(safe_pull(idm_raw, col_mun_nom))

idm <- tibble::tibble(
  ano             = suppressWarnings(as.integer(safe_pull(idm_raw, col_ano))),
  COD_DANE_DPTO   = sprintf("%02d", as.integer(gsub("\\D","", safe_pull(idm_raw, col_dep_cod)))),
  COD_DANE_MUNI   = sprintf("%05d", as.integer(gsub("\\D","", safe_pull(idm_raw, col_mun_cod)))),
  valor           = suppressWarnings(as.numeric(safe_pull(idm_raw, col_val))),
  DEPARTAMENTO_NU = norm_txt(DEP_ORI),
  MUNICIPIO_NU    = norm_txt(MUN_ORI),
  DEPARTAMENTO_TC = title_case_es(DEP_ORI),
  MUNICIPIO_TC    = title_case_es(MUN_ORI)
) %>% dplyr::filter(is.finite(ano), nzchar(COD_DANE_MUNI))

# ---------- Shapes ----------
mun_raw <- sf::st_read(ruta_shp_mun, quiet = TRUE)
dep_raw <- sf::st_read(ruta_shp_dep, quiet = TRUE)

# Municipios
col_mpio_code <- pick_col(mun_raw, "MPIO_CDPMP", "MPIO_CDPMP|COD_MPIO|CODMPIO|COD_MUN")
col_mpio_name <- pick_col(mun_raw, "MPIO_CNMBR", "MPIO_CNMBR|NOMBRE_MPIO|NOM_MPIO|MUNICIPIO")
stopifnot(!is.na(col_mpio_code))
mun_sf <- mun_raw %>%
  dplyr::mutate(
    COD_MUN5     = sprintf("%05d", as.integer(.data[[col_mpio_code]])),
    COD_DPTO2    = substr(COD_MUN5, 1, 2),
    MUNICIPIO_SH = as.character(.data[[col_mpio_name]] %||% COD_MUN5),
    MUNICIPIO_TC = title_case_es(MUNICIPIO_SH),
    MUNI_NU      = norm_txt(MUNICIPIO_SH)
  ) %>% sf::st_transform(4326) %>% sf::st_make_valid()

# Departamentos
col_dep_code <- pick_col(dep_raw, "DPTO_CCDGO", "DPTO_CCDGO|COD_DEPTO|CODDPTO|DPTO_CCES")
col_dep_name <- pick_col(dep_raw, "DEPARTAMENTO_D", "DEPARTAMENTO_D|DPTO_CNMBR|NOMBRE_DEPTO|NOM_DPTO|DEPARTAM")
stopifnot(!is.na(col_dep_code))
dep_sf <- dep_raw %>%
  dplyr::mutate(
    COD_DPTO2       = sprintf("%02d", as.integer(.data[[col_dep_code]])),
    DEPARTAMENTO_SH = as.character(.data[[col_dep_name]] %||% COD_DPTO2),
    DEPARTAMENTO_TC = title_case_es(DEPARTAMENTO_SH),
    DEP_NU_SHP      = norm_txt(DEPARTAMENTO_SH)
  ) %>% sf::st_transform(4326) %>% sf::st_make_valid()

# Lookups
dep_lookup <- idm %>%
  dplyr::select(COD_DANE_DPTO, DEPARTAMENTO_NU, DEPARTAMENTO_TC) %>%
  dplyr::mutate(COD_DPTO2 = sprintf("%02d", as.integer(COD_DANE_DPTO))) %>% dplyr::distinct()

# ---------- UI ----------
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5, primary = "#2563eb",
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius"  = "0.9rem",
    "font-size-base" = "0.95rem"
  ),
  tags$head(
    tags$style(HTML("
      :root{
        --accent-border:#99d5ec;
        --gap:12px;
      }
      .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
      h3{font-weight:700;letter-spacing:.2px;margin-bottom:8px}
      .data-note{font-size:13px;color:#6b7280;margin:0 0 16px}
      .filters{
        background:#fff;border:1.5px solid var(--accent-border);border-radius:16px;
        padding:10px 14px 12px;margin-bottom:16px;box-shadow:0 4px 14px rgba(0,0,0,.06)
      }
      .card{
        background:#fff;border:1.5px solid var(--accent-border);border-radius:16px;padding:12px;
        box-shadow:0 2px 8px rgba(0,0,0,.06);margin-bottom:12px
      }
      .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}

      /* ====== GRID DE FILTROS (cajas alineadas) ====== */
      .filters-grid{
        display:grid;
        grid-template-columns:repeat(4,minmax(200px,1fr));
        gap:var(--gap);
        align-items:stretch;
      }
      .filter{
        display:flex;
        flex-direction:column;
        justify-content:flex-start;
      }
      /* Títulos de filtros: Inter, 14px, medium, negro */
      .filter-label{
        font-family:'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-size:14px;
        font-weight:500;
        letter-spacing:.2px;
        color:#111827;
        margin-bottom:6px;
      }
      .filters-grid .shiny-input-container{
        margin:0 !important;
      }
      .filters-grid .selectize-input,
      .filters-grid .form-control,
      .filters-grid .form-select{
        height:60px !important;
        min-height:60px;
        padding-top:10px;
        padding-bottom:10px;
        border-radius:10px;
        border:1.5px solid var(--accent-border) !important;
      }
      .filters-grid .selectize-input:focus,
      .filters-grid .form-control:focus,
      .filters-grid .form-select:focus{
        border-color:var(--accent-border) !important;
        box-shadow:0 0 0 .2rem rgba(153,213,236,.35) !important;
      }

      .form-control{ border:1.5px solid var(--accent-border) !important; border-radius:10px; }
      .form-control:focus{ border-color:var(--accent-border) !important; box-shadow:0 0 0 .2rem rgba(153,213,236,.35); }
      .selectize-input{ border:1.5px solid var(--accent-border) !important; border-radius:10px; }
      .selectize-input.focus{ border-color:var(--accent-border) !important; box-shadow:0 0 0 .2rem rgba(153,213,236,.35); }
      .selectize-dropdown{ border:1.5px solid var(--accent-border) !important; }

      .leaflet-top.leaflet-right { margin-top: 6px; margin-right: 6px; }
      .info-title{font-weight:700;margin-bottom:4px;}
      .info-text{font-size:12px;color:#4b5563;margin-bottom:4px;}
      .info-list{margin:0;padding-left:18px;font-size:12px;}
      .info-quantiles-note{
        margin-top:8px;
        font-size:12px;
        color:#4b5563;
      }
      .info-quantiles-icon{
        margin-left:8px;
        font-size:14px;
        cursor:pointer;
        color:#4b5563;
      }
    ")),
    # Tooltip estático para el Top 10 (línea punteada = promedio departamental)
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        var el = document.getElementById('info_top10');
        if (el && typeof bootstrap !== 'undefined' && bootstrap.Popover) {
          new bootstrap.Popover(el);
        }
      });
    "))
  ),
  div(class="wrap",
      h3(""),
      div(class="data-note",""),
      div(
        class = "filters",
        div(
          class = "filters-grid",
          div(
            class = "filter",
            div(class = "filter-label", "¿Qué año analizamos?"),
            selectInput(
              "anio", NULL,
              choices  = sort(unique(idm$ano)),
              selected = max(idm$ano, na.rm = TRUE)
            )
          ),
          div(
            class = "filter",
            div(class = "filter-label", "¿En qué departamento?"),
            selectInput("f_dep", NULL, choices = "Santander", selected = "Santander")
          ),
          div(
            class = "filter",
            div(class = "filter-label", "¿Algún municipio en particular?"),
            selectInput("f_mun", NULL, choices = "Todos")
          ),
          div(
            class = "filter",
            div(class = "filter-label", "Acción"),
            tagList(
              actionLink("btn_back_co","⤺ Volver a Colombia")
            )
          )
        )
      ),
      fluidRow(
        column(6,
               div(class="card",
                   div(class="card-title d-flex align-items-center",
                       span(textOutput("ttl_mapa"))
                   ),
                   leafletOutput("map_idm", height=760),
                   div(
                     class = "info-quantiles-note",
                     HTML("Nota: El mapa clasifica los valores del indicador en cuartiles (cuatro grupos con igual número de observaciones).")
                   )
               )
        ),
        column(6,
               div(class="card",
                   div(class="card-title d-flex align-items-center",
                       span(textOutput("ttl_top_mpios")),
                       tags$span(
                         id = "info_top10",
                         "ℹ️",
                         class = "info-quantiles-icon",
                         `data-bs-toggle`    = "popover",
                         `data-bs-html`      = "true",
                         `data-bs-placement` = "bottom",
                         `data-bs-offset`    = "0,8",
                         `data-bs-content`   =
                           '<div class=\"info-title\">ℹ️&nbsp;Referencia del promedio departamental</div>
                             <div class=\"info-text\">
                               La línea punteada negra marca el promedio del IDM del departamento seleccionado.
                               Las barras que quedan a la derecha de la línea están por encima del promedio departamental.
                             </div>'
                       )
                   ),
                   plotlyOutput("bar_top", height=340)),
               div(class="card",
                   div(class="card-title", textOutput("ttl_prom_dep")),
                   plotlyOutput("bar_depto", height=345)))
      )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session){
  
  # ---- Títulos ----
  output$ttl_mapa <- renderText({
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      "¿Cuál es el promedio departamental del Índice de Desempeño Municipal?"
    } else if (!is.null(input$f_mun) && input$f_mun != "Todos") {
      "¿Cuál es el nivel del Índice de Desempeño Municipal?"
    } else {
      "¿Cuál es el nivel del Índice de Desempeño Municipal?"
    }
  })
  output$ttl_top_mpios <- renderText({
    "Top 10 municipios con mejor desempeño en el Índice de Desempeño Municipal"
  })
  output$ttl_prom_dep <- renderText({
    "¿Cuál es el valor promedio del Índice de Desempeño Municipal por departamento?"
  })
  
  # ---- Combos dependientes (usar *_TC) ----
  observe({
    df <- idm %>% dplyr::filter(ano == input$anio)
    deps_tc <- df %>% dplyr::distinct(DEPARTAMENTO_TC) %>% dplyr::arrange(DEPARTAMENTO_TC) %>% dplyr::pull()
    sel <- if (!is.null(input$f_dep) && input$f_dep %in% deps_tc) input$f_dep else "Todos"
    updateSelectInput(session, "f_dep", choices = c("Todos", deps_tc), selected = sel)
    updateSelectInput(session, "f_mun", choices = "Todos")
  })
  
  observeEvent(input$f_dep, {
    df <- idm %>% dplyr::filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") {
      mpios_tc <- df %>% dplyr::filter(DEPARTAMENTO_TC == input$f_dep) %>%
        dplyr::distinct(MUNICIPIO_TC) %>% dplyr::arrange(MUNICIPIO_TC) %>% dplyr::pull()
      updateSelectInput(session, "f_mun", choices = c("Todos", mpios_tc), selected = "Todos")
    } else updateSelectInput(session, "f_mun", choices = "Todos")
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_back_co, {
    updateSelectInput(session, "f_dep", selected = "Todos")
    updateSelectInput(session, "f_mun", selected = "Todos")
  })
  
  # ---- Base filtrada ----
  base_filtrada <- reactive({
    d <- idm %>% dplyr::filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") d <- d %>% dplyr::filter(DEPARTAMENTO_TC == input$f_dep)
    if (!is.null(input$f_mun) && input$f_mun != "Todos") d <- d %>% dplyr::filter(MUNICIPIO_TC == input$f_mun)
    d
  })
  
  # Escala a porcentaje (0–1 -> *100)
  scale_to_pct <- function(v){
    mx <- suppressWarnings(max(v, na.rm = TRUE))
    if (!is.finite(mx)) return(v)
    if (mx <= 1.5) v*100 else v
  }
  
  # ---- Límites comunes de eje X cuando hay departamento filtrado ----
  compute_axis_limits <- reactive({
    if (is.null(input$f_dep) || input$f_dep == "Todos") return(NULL)
    
    d_all <- idm %>%
      dplyr::filter(
        ano == input$anio,
        DEPARTAMENTO_TC == input$f_dep
      )
    
    if (nrow(d_all) == 0) return(NULL)
    
    df_mun <- d_all %>%
      dplyr::group_by(DEPARTAMENTO_TC, MUNICIPIO_TC) %>%
      dplyr::summarise(val = mean(valor, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(val_pct = scale_to_pct(val))
    
    max_mun_pct <- suppressWarnings(max(df_mun$val_pct, na.rm = TRUE))
    
    dep_mean_val <- mean(d_all$valor, na.rm = TRUE)
    dep_mean_pct <- scale_to_pct(dep_mean_val)
    
    x_max <- max(max_mun_pct, dep_mean_pct)
    if (!is.finite(x_max)) return(NULL)
    
    list(
      x_max        = x_max * 1.05,
      dep_mean_pct = dep_mean_pct
    )
  })
  
  # ---- Mapa base (zoom control top-right) ----
  output$map_idm <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-74.3, 4.6, 5) %>%
      htmlwidgets::onRender("function(el,x){ this.zoomControl.setPosition('topright'); }")
  })
  
  # Helper: código depto desde nombre TC
  get_cod_from_dep_tc <- function(dep_tc){
    if (is.null(dep_tc) || dep_tc == "Todos") return(NA_character_)
    i <- which(dep_sf$DEPARTAMENTO_TC == dep_tc)[1]
    if (is.finite(i)) return(dep_sf$COD_DPTO2[i])
    j <- which(title_case_es(dep_lookup$DEPARTAMENTO_TC) == dep_tc)[1]
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
      # Departamentos
      dd <- d %>%
        dplyr::group_by(COD_DANE_DPTO) %>%
        dplyr::summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(COD_DPTO2 = sprintf("%02d", as.integer(COD_DANE_DPTO)),
                      valor_pct = scale_to_pct(valor))
      
      shp <- dep_sf %>%
        dplyr::left_join(dd %>% dplyr::select(COD_DPTO2, valor_pct), by = "COD_DPTO2") %>%
        dplyr::mutate(
          nombre = DEPARTAMENTO_TC,
          etq    = paste0("<b>", nombre, "</b><br>IDM promedio: ", fmt_pct100(valor_pct, acc = 0.1))
        )
      
      pal  <- make_pal_bin(shp$valor_pct, MAP_PALETTE)
      bins <- attr(pal, "bins")
      if (is.null(bins)) bins <- compute_breaks(shp$valor_pct, length(MAP_PALETTE))
      
      labels_legend <- build_interval_labels_pct(bins)
      mids          <- (bins[-length(bins)] + bins[-1]) / 2
      cols_legend   <- pal(mids)
      
      leafletProxy("map_idm", data = shp) %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(layerId=~COD_DPTO2, fillColor=~pal(valor_pct), color="#666", weight=0.7,
                    fillOpacity=0.9, label=~lapply(etq, HTML),
                    highlightOptions = highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        addLegend(
          position = "bottomright",
          colors   = cols_legend,
          labels   = labels_legend,
          opacity  = 0.9,
          title    = "Porcentaje"
        )
      
    } else {
      # Municipios
      sel_cod <- get_cod_from_dep_tc(input$f_dep)
      if (is.na(sel_cod) || !nzchar(sel_cod)) {
        sel_cod <- d$COD_DANE_DPTO %>% unique() %>% sprintf("%02d", as.integer(.)) %>% .[1]
      }
      
      dd <- d %>%
        dplyr::group_by(COD_DANE_MUNI) %>%
        dplyr::summarise(valor = mean(valor, na.rm = TRUE), .groups="drop") %>%
        dplyr::mutate(COD_MUN5 = sprintf("%05d", as.integer(COD_DANE_MUNI)),
                      valor_pct = scale_to_pct(valor))
      
      shp <- mun_sf %>%
        dplyr::filter(COD_DPTO2 == sel_cod) %>%
        dplyr::left_join(dd %>% dplyr::select(COD_MUN5, valor_pct), by = "COD_MUN5") %>%
        dplyr::mutate(
          MUNICIPIO_SHOW = dplyr::coalesce(MUNICIPIO_TC, MUNICIPIO_SH),
          etq = paste0("<b>", MUNICIPIO_SHOW, "</b><br>IDM: ", fmt_pct100(valor_pct, acc = 0.1))
        )
      
      pal  <- make_pal_bin(shp$valor_pct, MAP_PALETTE)
      bins <- attr(pal, "bins")
      if (is.null(bins)) bins <- compute_breaks(shp$valor_pct, length(MAP_PALETTE))
      bb <- sf::st_bbox(shp)
      
      labels_legend <- build_interval_labels_pct(bins)
      mids          <- (bins[-length(bins)] + bins[-1]) / 2
      cols_legend   <- pal(mids)
      
      leafletProxy("map_idm", data = shp) %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(layerId=~COD_MUN5, fillColor=~pal(valor_pct), color="#666", weight=0.4,
                    fillOpacity=0.9, label=~lapply(etq, HTML),
                    highlightOptions = highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        addLegend(
          position = "bottomright",
          colors   = cols_legend,
          labels   = labels_legend,
          opacity  = 0.9,
          title    = "Porcentaje"
        ) %>%
        fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  
  # Drill-down
  observeEvent(input$map_idm_shape_click, {
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      click <- input$map_idm_shape_click; req(click$id)
      cod <- sprintf("%02d", as.integer(click$id))
      nom_tc <- dep_sf$DEPARTAMENTO_TC[match(cod, dep_sf$COD_DPTO2)]
      req(!is.na(nom_tc), nzchar(nom_tc))
      deps_year <- idm %>% dplyr::filter(ano == input$anio) %>% dplyr::distinct(DEPARTAMENTO_TC)
      if (nom_tc %in% deps_year$DEPARTAMENTO_TC) updateSelectInput(session, "f_dep", selected = nom_tc)
    }
  }, ignoreInit = TRUE)
  
  # ---- Barras (% con 1 decimal, texto blanco, centrado) ----
  output$bar_top <- renderPlotly({
    d <- base_filtrada(); req(nrow(d) > 0)
    
    d1 <- d %>%
      dplyr::group_by(DEPARTAMENTO_TC, MUNICIPIO_TC) %>%
      dplyr::summarise(val = mean(valor, na.rm=TRUE), .groups="drop") %>%
      dplyr::mutate(val_pct = scale_to_pct(val)) %>%
      dplyr::arrange(dplyr::desc(val_pct)) %>% 
      dplyr::slice_head(n = 10) %>%
      dplyr::mutate(
        lbl = MUNICIPIO_TC,
        txt = fmt_pct100(val_pct, acc = 0.1)
      )
    
    axis_info <- compute_axis_limits()
    
    xaxis_opts <- list(
      title      = "Porcentaje",
      ticksuffix = "%"
    )
    shapes_list <- list()
    
    if (!is.null(axis_info)) {
      xaxis_opts$range <- c(0, axis_info$x_max)
      
      shapes_list <- list(
        list(
          type  = "line",
          xref  = "x",
          yref  = "paper",
          x0    = axis_info$dep_mean_pct,
          x1    = axis_info$dep_mean_pct,
          y0    = 0,
          y1    = 1,
          line  = list(
            color = "rgba(17,24,39,0.85)",
            dash  = "dash",
            width = 1.4
          )
        )
      )
    }
    
    plot_ly(d1 %>% dplyr::arrange(val_pct),
            x = ~val_pct,
            y = ~factor(lbl, levels = d1$lbl[order(d1$val_pct)]),
            type = "bar",
            orientation = "h",
            marker = list(color = "#0099db"),
            text = ~txt,
            textposition = "inside",
            insidetextanchor = "middle",
            textfont = list(color = "white", family = "Inter", size = 12),
            hovertemplate = "%{y}<br>IDM: %{x:.1f}%%<extra></extra>") %>%
      layout(
        xaxis      = xaxis_opts,
        yaxis      = list(title = "", automargin = TRUE),
        margin     = list(l=10, r=10, b=10, t=10),
        showlegend = FALSE,
        uniformtext = list(minsize = 10, mode = "hide"),
        shapes     = shapes_list
      )
  })
  
  output$bar_depto <- renderPlotly({
    d <- base_filtrada(); req(nrow(d) > 0)
    
    d2 <- d %>%
      dplyr::group_by(DEPARTAMENTO_TC) %>%
      dplyr::summarise(val = mean(valor, na.rm=TRUE), .groups="drop") %>%
      dplyr::mutate(
        val_pct = scale_to_pct(val),
        txt     = fmt_pct100(val_pct, acc = 0.1)
      ) %>%
      dplyr::arrange(dplyr::desc(val_pct))
    
    axis_info <- compute_axis_limits()
    
    xaxis_opts <- list(
      title      = "Porcentaje",
      ticksuffix = "%"
    )
    if (!is.null(axis_info)) {
      xaxis_opts$range <- c(0, axis_info$x_max)
    }
    
    plot_ly(d2 %>% dplyr::arrange(val_pct),
            x=~val_pct,
            y=~factor(DEPARTAMENTO_TC, levels = d2$DEPARTAMENTO_TC[order(d2$val_pct)]),
            type="bar",
            orientation="h",
            marker = list(color = "#0099db"),
            text = ~txt,
            textposition = "inside",
            insidetextanchor = "middle",
            textfont = list(color = "white", family = "Inter", size = 12),
            hovertemplate = "%{y}<br>IDM promedio: %{x:.1f}%%<extra></extra>") %>%
      layout(
        xaxis  = xaxis_opts,
        yaxis  = list(title="", automargin = TRUE),
        margin = list(l=10,r=10,b=10,t=10),
        showlegend = FALSE,
        uniformtext = list(minsize = 10, mode = "hide")
      )
  })
}

shinyApp(ui, server)

