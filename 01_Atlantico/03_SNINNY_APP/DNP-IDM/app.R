# app_idm.R — Terridata IDM (PNG mapa con leyenda + zoom municipal robusto + CSV + PDF tipo “población”)
suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(leaflet); library(sf); library(dplyr)
  library(scales); library(htmltools); library(plotly)
  library(stringi); library(stringr); library(readr); library(tidyr)
  library(tibble)
  
  # Export robusto (como UPRA / Población)
  library(htmlwidgets)
  library(webshot2)
  
  # PDF
  library(rmarkdown)
})

options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

# ---------- Rutas ----------
data_dir <- "data"
ruta_idm     <- file.path(data_dir, "071_DNP_Terridata_IDM.rds")
ruta_pob     <- file.path(data_dir, "051_DANE_Proyecciones_P_total.rds")  # opcional
ruta_shp_mun <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")
ruta_shp_dep <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")

# RMD (puede estar en raíz o en data/)
app_root      <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
ruta_rmd_root <- file.path(app_root, "Informe_descargable.Rmd")
ruta_rmd_data <- file.path(app_root, "data", "Informe_descargable.Rmd")
ruta_rmd      <- if (file.exists(ruta_rmd_root)) ruta_rmd_root else ruta_rmd_data

# ---------- Export (robusto con webshot2) ----------
EXPORT_DIR <- file.path(app_root, "Descargas")
dir.create(EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)

PNG_VWIDTH   <- 1600
PNG_VHEIGHT  <- 1000
PNG_DELAY_CO <- 1.2
PNG_DELAY_MUN <- 2.0   # <- más delay cuando es municipal

# Nombres fijos para que el Rmd los encuentre fácil (como el ejemplo población)
IMG_MAP  <- file.path(EXPORT_DIR, "idm_mapa.png")
IMG_TOP  <- file.path(EXPORT_DIR, "idm_top10.png")
IMG_DEP  <- file.path(EXPORT_DIR, "idm_prom_depto.png")
CSV_FILT <- file.path(EXPORT_DIR, "idm_base_filtrada.csv")

save_widget_png <- function(widget, out_png, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay = PNG_DELAY_CO){
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
  
  file.exists(out_png) && is.finite(file.info(out_png)$size) && file.info(out_png)$size > 0
}

# ---------- Utils ----------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
norm_txt <- function(x) ifelse(is.na(x), NA_character_, stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII"))
NUP      <- function(x) toupper(norm_txt(x))

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

sanitize_filename <- function(x){
  x <- as.character(x)
  x <- gsub("[/\\\\:*?\"<>|]", "_", x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("__+", "_", x)
  trimws(x)
}

# Paleta
MAP_PALETTE <- c("#e0f3fa", "#99d5ec", "#4bb5e1", "#0099cc", "#005b88")

# Formato ES
fmt_pct <- function(x, acc = 0.1){
  ifelse(is.na(x), "NA", scales::percent(x, accuracy = acc, decimal.mark = ","))
}
fmt_pct100 <- function(x, acc = 0.1) fmt_pct(x/100, acc = acc)

make_pal_bin <- function(values, palette = MAP_PALETTE, n_bins = length(MAP_PALETTE)) {
  vals <- suppressWarnings(as.numeric(values))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) vals <- 0
  qs   <- stats::quantile(vals, probs = seq(0, 1, length.out = n_bins), na.rm = TRUE, type = 7)
  bins <- sort(unique(as.numeric(qs)))
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

compute_breaks <- function(values, n_bins){
  vals <- suppressWarnings(as.numeric(values)); vals <- vals[is.finite(vals)]
  if (!length(vals)) vals <- 0
  qs <- stats::quantile(vals, probs = seq(0, 1, length.out = n_bins), na.rm = TRUE, type = 7)
  sort(unique(as.numeric(qs)))
}
format_interval_label_pct <- function(a, b, is_first = TRUE){
  fa <- fmt_pct100(a, acc = 0.1)
  fb <- fmt_pct100(b, acc = 0.1)
  if (is_first) sprintf("%s – %s", fa, fb) else sprintf(">%s – %s", fa, fb)
}
build_interval_labels_pct <- function(breaks){
  if (length(breaks) < 2) return(character(0))
  vapply(seq_len(length(breaks) - 1), function(i){
    format_interval_label_pct(breaks[i], breaks[i+1], is_first = (i == 1))
  }, character(1))
}

# Zoom heurístico (como UPRA)
zoom_from_bbox <- function(bb){
  w <- abs(as.numeric(bb["xmax"] - bb["xmin"]))
  h <- abs(as.numeric(bb["ymax"] - bb["ymin"]))
  span <- max(w, h)
  if (!is.finite(span)) return(7)
  if (span < 0.15) return(11)
  if (span < 0.35) return(10)
  if (span < 0.80) return(9)
  if (span < 1.50) return(8)
  if (span < 3.00) return(7)
  6
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

dep_lookup <- idm %>%
  dplyr::select(COD_DANE_DPTO, DEPARTAMENTO_NU, DEPARTAMENTO_TC) %>%
  dplyr::mutate(COD_DPTO2 = sprintf("%02d", as.integer(COD_DANE_DPTO))) %>%
  dplyr::distinct()

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
      :root{ --accent-border:#99d5ec; --gap:12px; }
      .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
      .filters{
        background:#fff;border:1.5px solid var(--accent-border);border-radius:16px;
        padding:10px 14px 12px;margin-bottom:16px;box-shadow:0 4px 14px rgba(0,0,0,.06)
      }
      .card{
        background:#fff;border:1.5px solid var(--accent-border);border-radius:16px;padding:12px;
        box-shadow:0 2px 8px rgba(0,0,0,.06);margin-bottom:12px
      }
      .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}
      .filters-grid{display:grid;grid-template-columns:repeat(4,minmax(200px,1fr));gap:var(--gap);align-items:stretch;}
      .filter{display:flex;flex-direction:column;justify-content:flex-start;}
      .filter-label{font-family:'Inter',system-ui; font-size:14px;font-weight:500;color:#111827;margin-bottom:6px;}
      .filters-grid .shiny-input-container{margin:0 !important;}
      .filters-grid .selectize-input,.filters-grid .form-control,.filters-grid .form-select{
        height:60px !important;min-height:60px;padding-top:10px;padding-bottom:10px;border-radius:10px;
        border:1.5px solid var(--accent-border) !important;
      }
      .leaflet-top.leaflet-right { margin-top: 6px; margin-right: 6px; }

      .btn-unified{
        background:#ffffff !important;border:1px solid #2563eb !important;color:#6b7280 !important;
        font-weight:700 !important;border-radius:12px !important;padding:6px 6px !important;
      }
      .btn-unified:hover,.btn-unified:focus,.btn-unified:active{
        background:#ffffff !important;border-color:#2563eb !important;color:#6b7280 !important;
        box-shadow:0 0 0 .2rem rgba(37,99,235,.15) !important;
      }
      .footer-actions{
        margin-top: -4px;
        padding-top: 0px;
        display:flex;
        justify-content:flex-end;
        gap: 6px;
        padding-left: 6px;
        padding-right: 6px;
        flex-wrap: wrap;
      }
    ")),
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
      div(class="filters",
          div(class="filters-grid",
              div(class="filter",
                  div(class="filter-label","¿Qué año analizamos?"),
                  selectInput("anio", NULL, choices = sort(unique(idm$ano)), selected = max(idm$ano, na.rm = TRUE))
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
                  div(class="filter-label","Acción"),
                  tagList(actionLink("btn_back_co","⤺ Volver a Colombia"))
              )
          )
      ),
      fluidRow(
        column(6,
               div(class="card",
                   div(class="card-title d-flex align-items-center justify-content-between",
                       span(textOutput("ttl_mapa")),
                       downloadButton("dl_map_png", "Descargar PNG", class="btn-unified")
                   ),
                   leafletOutput("map_idm", height=760)
               )
        ),
        column(6,
               div(class="card",
                   div(class="card-title d-flex align-items-center justify-content-between",
                       div(class="d-flex align-items-center",
                           span(textOutput("ttl_top_mpios")),
                           tags$span(
                             id = "info_top10", "ℹ️", class = "info-quantiles-icon",
                             `data-bs-toggle`="popover", `data-bs-html`="true", `data-bs-placement`="bottom",
                             `data-bs-offset`="0,8",
                             `data-bs-content`=
                               '<div style=\"font-weight:700;margin-bottom:4px;\">ℹ️&nbsp;Referencia del promedio departamental</div>
                                <div style=\"font-size:12px;color:#4b5563;\">
                                  La línea punteada negra marca el promedio del IDM del departamento seleccionado.
                                  Las barras a la derecha están por encima del promedio.
                                </div>'
                           )
                       ),
                       downloadButton("dl_top_png", "Descargar PNG", class="btn-unified")
                   ),
                   plotlyOutput("bar_top", height=340)
               ),
               div(class="card",
                   div(class="card-title d-flex align-items-center justify-content-between",
                       span(textOutput("ttl_prom_dep")),
                       downloadButton("dl_dep_png", "Descargar PNG", class="btn-unified")
                   ),
                   plotlyOutput("bar_depto", height=345)
               )
        )
      ),
      
      # ===== Footer: CSV + PDF =====
      div(
        class = "footer-actions",
        downloadButton("dl_csv", "Descargar CSV", class="btn-unified"),
        downloadButton("dl_reporte_pdf", "Descargar informe (PDF)", class="btn-unified")
      )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session){
  
  output$ttl_mapa <- renderText({
    if (is.null(input$f_dep) || input$f_dep == "Todos")
      "¿Cuál es el promedio departamental del Índice de Desempeño Municipal?"
    else
      "¿Cuál es el nivel del Índice de Desempeño Municipal?"
  })
  output$ttl_top_mpios <- renderText("Top 10 municipios con mejor desempeño en el Índice de Desempeño Municipal")
  output$ttl_prom_dep  <- renderText("¿Cuál es el valor promedio del Índice de Desempeño Municipal por departamento?")
  
  observe({
    df <- idm %>% dplyr::filter(ano == input$anio)
    deps_tc <- df %>% dplyr::distinct(DEPARTAMENTO_TC) %>% dplyr::arrange(DEPARTAMENTO_TC) %>% dplyr::pull()
    sel <- if (!is.null(input$f_dep) && input$f_dep %in% deps_tc) input$f_dep else "Todos"
    updateSelectInput(session, "f_dep", choices = c("Todos", deps_tc), selected = sel)
    updateSelectInput(session, "f_mun", choices = "Todos", selected = "Todos")
  })
  
  observeEvent(input$f_dep, {
    df <- idm %>% dplyr::filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") {
      mpios_tc <- df %>%
        dplyr::filter(DEPARTAMENTO_TC == input$f_dep) %>%
        dplyr::distinct(MUNICIPIO_TC) %>%
        dplyr::arrange(MUNICIPIO_TC) %>%
        dplyr::pull()
      updateSelectInput(session, "f_mun", choices = c("Todos", mpios_tc), selected = "Todos")
    } else updateSelectInput(session, "f_mun", choices = "Todos", selected = "Todos")
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_back_co, {
    updateSelectInput(session, "f_dep", selected = "Todos")
    updateSelectInput(session, "f_mun", selected = "Todos")
  })
  
  base_filtrada <- reactive({
    d <- idm %>% dplyr::filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") d <- d %>% dplyr::filter(DEPARTAMENTO_TC == input$f_dep)
    if (!is.null(input$f_mun) && input$f_mun != "Todos") d <- d %>% dplyr::filter(MUNICIPIO_TC == input$f_mun)
    d
  })
  
  scale_to_pct <- function(v){
    mx <- suppressWarnings(max(v, na.rm = TRUE))
    if (!is.finite(mx)) return(v)
    if (mx <= 1.5) v*100 else v
  }
  
  compute_axis_limits <- reactive({
    if (is.null(input$f_dep) || input$f_dep == "Todos") return(NULL)
    d_all <- idm %>% dplyr::filter(ano == input$anio, DEPARTAMENTO_TC == input$f_dep)
    if (nrow(d_all) == 0) return(NULL)
    
    df_mun <- d_all %>%
      dplyr::group_by(DEPARTAMENTO_TC, MUNICIPIO_TC) %>%
      dplyr::summarise(val = mean(valor, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(val_pct = scale_to_pct(val))
    
    max_mun_pct  <- suppressWarnings(max(df_mun$val_pct, na.rm = TRUE))
    dep_mean_pct <- scale_to_pct(mean(d_all$valor, na.rm = TRUE))
    x_max <- max(max_mun_pct, dep_mean_pct)
    if (!is.finite(x_max)) return(NULL)
    
    list(x_max = x_max * 1.05, dep_mean_pct = dep_mean_pct)
  })
  
  # ---- Map base view ----
  output$map_idm <- renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 4, maxZoom = 13, zoomSnap = 0.25)) %>%
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.Positron,
        options = leaflet::providerTileOptions(crossOrigin = TRUE)
      ) %>%
      leaflet::setView(-74.3, 4.6, 5) %>%
      htmlwidgets::onRender("function(el,x){ this.zoomControl.setPosition('topright'); }")
  })
  
  get_cod_from_dep_tc <- function(dep_tc){
    if (is.null(dep_tc) || dep_tc == "Todos") return(NA_character_)
    i <- which(dep_sf$DEPARTAMENTO_TC == dep_tc)[1]
    if (is.finite(i)) return(dep_sf$COD_DPTO2[i])
    j <- which(title_case_es(dep_lookup$DEPARTAMENTO_TC) == dep_tc)[1]
    if (is.finite(j)) return(dep_lookup$COD_DPTO2[j])
    NA_character_
  }
  
  # ---- Vista interactiva (proxy) ----
  observe({
    d <- base_filtrada()
    if (nrow(d) == 0) {
      leafletProxy("map_idm") %>% clearShapes() %>% clearControls()
      return()
    }
    
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
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
      bins <- attr(pal, "bins"); if (is.null(bins)) bins <- compute_breaks(shp$valor_pct, length(MAP_PALETTE))
      labels_legend <- build_interval_labels_pct(bins)
      mids          <- (bins[-length(bins)] + bins[-1]) / 2
      cols_legend   <- pal(mids)
      
      leafletProxy("map_idm", data = shp) %>%
        clearShapes() %>% clearControls() %>%
        setView(-74.3, 4.6, 5) %>%
        addPolygons(layerId=~COD_DPTO2, fillColor=~pal(valor_pct), color="#666", weight=0.7,
                    fillOpacity=0.9, label=~lapply(etq, HTML),
                    highlightOptions = highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        leaflet::addLegend(position="bottomright", colors=cols_legend, labels=labels_legend, opacity=0.9, title="Porcentaje")
      
    } else {
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
      bins <- attr(pal, "bins"); if (is.null(bins)) bins <- compute_breaks(shp$valor_pct, length(MAP_PALETTE))
      labels_legend <- build_interval_labels_pct(bins)
      mids          <- (bins[-length(bins)] + bins[-1]) / 2
      cols_legend   <- pal(mids)
      
      bb  <- sf::st_bbox(shp)
      lng <- mean(c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"])))
      lat <- mean(c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"])))
      z   <- zoom_from_bbox(bb)
      
      leafletProxy("map_idm", data = shp) %>%
        clearShapes() %>% clearControls() %>%
        setView(lng, lat, zoom = z) %>%
        addPolygons(layerId=~COD_MUN5, fillColor=~pal(valor_pct), color="#666", weight=0.4,
                    fillOpacity=0.9, label=~lapply(etq, HTML),
                    highlightOptions = highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        leaflet::addLegend(position="bottomright", colors=cols_legend, labels=labels_legend, opacity=0.9, title="Porcentaje")
    }
  })
  
  # ===== Mapa widget para DESCARGA PNG (municipal correcto) =====
  map_widget <- reactive({
    d <- base_filtrada()
    req(nrow(d) > 0)
    
    base_map <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 4, maxZoom = 13, zoomSnap = 0.25)) %>%
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.Positron,
        options = leaflet::providerTileOptions(crossOrigin = TRUE)
      ) %>%
      htmlwidgets::onRender("function(el,x){ this.zoomControl.setPosition('topright'); }")
    
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
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
      
      brks <- compute_breaks(shp$valor_pct, n_bins = length(MAP_PALETTE))
      pal  <- leaflet::colorBin(MAP_PALETTE, domain = shp$valor_pct, bins = brks, na.color="#f0f0f0")
      labels_legend <- build_interval_labels_pct(brks)
      mids          <- (brks[-length(brks)] + brks[-1]) / 2
      cols_legend   <- pal(mids)
      
      base_map %>%
        leaflet::setView(-74.3, 4.6, 5) %>%
        leaflet::addPolygons(
          data = shp, layerId = ~COD_DPTO2,
          fillColor = ~pal(valor_pct),
          color="#666", weight=0.7, fillOpacity=0.9,
          label = ~lapply(etq, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
        ) %>%
        leaflet::addLegend(position="bottomright", colors=cols_legend, labels=labels_legend, opacity=0.9, title="Porcentaje")
      
    } else {
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
      
      brks <- compute_breaks(shp$valor_pct, n_bins = length(MAP_PALETTE))
      pal  <- leaflet::colorBin(MAP_PALETTE, domain = shp$valor_pct, bins = brks, na.color="#f0f0f0")
      labels_legend <- build_interval_labels_pct(brks)
      mids          <- (brks[-length(brks)] + brks[-1]) / 2
      cols_legend   <- pal(mids)
      
      bb  <- sf::st_bbox(shp)
      lng <- mean(c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"])))
      lat <- mean(c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"])))
      z   <- zoom_from_bbox(bb)
      
      base_map %>%
        leaflet::setView(lng, lat, zoom = z) %>%
        leaflet::addPolygons(
          data = shp, layerId = ~COD_MUN5,
          fillColor = ~pal(valor_pct),
          color="#666", weight=0.4, fillOpacity=0.9,
          label = ~lapply(etq, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
        ) %>%
        leaflet::addLegend(position="bottomright", colors=cols_legend, labels=labels_legend, opacity=0.9, title="Porcentaje")
    }
  })
  
  # ---- Barras (plotly widgets) ----
  build_plot_top <- reactive({
    d <- base_filtrada(); req(nrow(d) > 0)
    
    d1 <- d %>%
      dplyr::group_by(DEPARTAMENTO_TC, MUNICIPIO_TC) %>%
      dplyr::summarise(val = mean(valor, na.rm=TRUE), .groups="drop") %>%
      dplyr::mutate(val_pct = scale_to_pct(val)) %>%
      dplyr::arrange(dplyr::desc(val_pct)) %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::mutate(lbl = MUNICIPIO_TC, txt = fmt_pct100(val_pct, acc = 0.1))
    
    axis_info <- compute_axis_limits()
    xaxis_opts <- list(title="Porcentaje", ticksuffix="%")
    shapes_list <- list()
    
    if (!is.null(axis_info)) {
      xaxis_opts$range <- c(0, axis_info$x_max)
      shapes_list <- list(
        list(type="line", xref="x", yref="paper",
             x0=axis_info$dep_mean_pct, x1=axis_info$dep_mean_pct, y0=0, y1=1,
             line=list(color="rgba(17,24,39,0.85)", dash="dash", width=1.4))
      )
    }
    
    plot_ly(d1 %>% dplyr::arrange(val_pct),
            x = ~val_pct,
            y = ~factor(lbl, levels = d1$lbl[order(d1$val_pct)]),
            type="bar", orientation="h",
            marker=list(color="#0099db"),
            text=~txt, textposition="inside",
            insidetextanchor="middle",
            textfont=list(color="white", family="Inter", size=12),
            hovertemplate="%{y}<br>IDM: %{x:.1f}%%<extra></extra>") %>%
      layout(
        xaxis=xaxis_opts,
        yaxis=list(title="", automargin=TRUE),
        margin=list(l=10,r=10,b=10,t=10),
        showlegend=FALSE,
        uniformtext=list(minsize=10, mode="hide"),
        shapes=shapes_list
      )
  })
  
  build_plot_depto <- reactive({
    d <- base_filtrada(); req(nrow(d) > 0)
    
    d2 <- d %>%
      dplyr::group_by(DEPARTAMENTO_TC) %>%
      dplyr::summarise(val = mean(valor, na.rm=TRUE), .groups="drop") %>%
      dplyr::mutate(val_pct = scale_to_pct(val), txt = fmt_pct100(val_pct, acc=0.1)) %>%
      dplyr::arrange(dplyr::desc(val_pct))
    
    axis_info <- compute_axis_limits()
    xaxis_opts <- list(title="Porcentaje", ticksuffix="%")
    if (!is.null(axis_info)) xaxis_opts$range <- c(0, axis_info$x_max)
    
    plot_ly(d2 %>% dplyr::arrange(val_pct),
            x=~val_pct,
            y=~factor(DEPARTAMENTO_TC, levels = d2$DEPARTAMENTO_TC[order(d2$val_pct)]),
            type="bar", orientation="h",
            marker=list(color="#0099db"),
            text=~txt, textposition="inside",
            insidetextanchor="middle",
            textfont=list(color="white", family="Inter", size=12),
            hovertemplate="%{y}<br>IDM promedio: %{x:.1f}%%<extra></extra>") %>%
      layout(
        xaxis=xaxis_opts,
        yaxis=list(title="", automargin=TRUE),
        margin=list(l=10,r=10,b=10,t=10),
        showlegend=FALSE,
        uniformtext=list(minsize=10, mode="hide")
      )
  })
  
  output$bar_top   <- renderPlotly({ build_plot_top() })
  output$bar_depto <- renderPlotly({ build_plot_depto() })
  
  # ---------- Descargas PNG ----------
  output$dl_map_png <- downloadHandler(
    filename = function(){
      dep_tag <- if (is.null(input$f_dep) || input$f_dep=="Todos") "Colombia" else gsub("\\s+","_", input$f_dep)
      mun_tag <- if (is.null(input$f_mun) || input$f_mun=="Todos") "Todos"   else gsub("\\s+","_", input$f_mun)
      paste0("IDM_mapa_", dep_tag, "_", mun_tag, "_", input$anio, "_", Sys.Date(), ".png")
    },
    content = function(file){
      dly <- if (!is.null(input$f_dep) && input$f_dep != "Todos") PNG_DELAY_MUN else PNG_DELAY_CO
      ok <- save_widget_png(map_widget(), file, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay = dly)
      if (!ok) stop("No se pudo generar el PNG del mapa. Revisa webshot2/Chromium (y tiles).")
    }
  )
  
  output$dl_top_png <- downloadHandler(
    filename = function(){ paste0("IDM_top10_", input$anio, "_", Sys.Date(), ".png") },
    content = function(file){
      ok <- save_widget_png(build_plot_top(), file, vwidth = 1400, vheight = 900, delay = 0.8)
      if (!ok) stop("No se pudo generar el PNG del Top 10.")
    }
  )
  
  output$dl_dep_png <- downloadHandler(
    filename = function(){ paste0("IDM_prom_depto_", input$anio, "_", Sys.Date(), ".png") },
    content = function(file){
      ok <- save_widget_png(build_plot_depto(), file, vwidth = 1400, vheight = 900, delay = 0.8)
      if (!ok) stop("No se pudo generar el PNG de Promedio por dpto.")
    }
  )
  
  # ---------- Descarga CSV ----------
  output$dl_csv <- downloadHandler(
    filename = function(){
      dep_tag <- if (is.null(input$f_dep) || input$f_dep=="Todos") "Colombia" else gsub("\\s+","_", input$f_dep)
      mun_tag <- if (is.null(input$f_mun) || input$f_mun=="Todos") "Todos"   else gsub("\\s+","_", input$f_mun)
      paste0("IDM_datos_", dep_tag, "_", mun_tag, "_", input$anio, "_", Sys.Date(), ".csv")
    },
    content = function(file){
      d <- base_filtrada()
      d_out <- d %>% dplyr::mutate(valor_pct = scale_to_pct(valor)) %>%
        dplyr::select(ano, COD_DANE_DPTO, DEPARTAMENTO_TC, COD_DANE_MUNI, MUNICIPIO_TC, valor, valor_pct)
      readr::write_csv(d_out, file)
    }
  )
  
  # =========================================================
  # PDF: corregido (ENTREGA PDF real y NO .htm)
  #   - genera PNGs en ./Descargas
  #   - genera CSV filtrado en ./Descargas
  #   - si el Rmd tiene __LOGO_PLATEA_PATH__ lo reemplaza
  #   - renderiza DIRECTO al archivo que Shiny descarga
  # =========================================================
  output$dl_reporte_pdf <- downloadHandler(
    filename = function(){
      dep_tag <- if (is.null(input$f_dep) || input$f_dep=="Todos") "Colombia" else sanitize_filename(input$f_dep)
      mun_tag <- if (is.null(input$f_mun) || input$f_mun=="Todos") "Todos"   else sanitize_filename(input$f_mun)
      paste0("Informe_IDM_", dep_tag, "_", mun_tag, "_", input$anio, "_", Sys.Date(), ".pdf")
    },
    content = function(file){
      
      if (!file.exists(ruta_rmd)) {
        stop("No encuentro Informe_descargable.Rmd en la raíz del proyecto ni en data/.")
      }
      
      # snapshot inputs
      anio_now <- input$anio
      dep_now  <- input$f_dep %||% "Todos"
      mun_now  <- input$f_mun %||% "Todos"
      
      # 1) PNGs en ./Descargas
      dly_map <- if (!is.null(dep_now) && dep_now != "Todos") PNG_DELAY_MUN else PNG_DELAY_CO
      
      ok_map <- save_widget_png(map_widget(),       IMG_MAP, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay = dly_map)
      ok_top <- save_widget_png(build_plot_top(),   IMG_TOP, vwidth = 1400,       vheight = 900,        delay = 0.9)
      ok_dep <- save_widget_png(build_plot_depto(), IMG_DEP, vwidth = 1400,       vheight = 900,        delay = 0.9)
      
      if (!ok_map) stop("No se pudo generar Descargas/idm_mapa.png para el informe.")
      if (!ok_top) stop("No se pudo generar Descargas/idm_top10.png para el informe.")
      if (!ok_dep) stop("No se pudo generar Descargas/idm_prom_depto.png para el informe.")
      
      # 2) CSV filtrado en ./Descargas
      d <- base_filtrada()
      d_out <- d %>% dplyr::mutate(valor_pct = scale_to_pct(valor)) %>%
        dplyr::select(ano, COD_DANE_DPTO, DEPARTAMENTO_TC, COD_DANE_MUNI, MUNICIPIO_TC, valor, valor_pct)
      utils::write.csv2(d_out, CSV_FILT, row.names = FALSE, fileEncoding = "UTF-8")
      
      # 3) Tabla de filtros
      filtros_tbl <- data.frame(
        Parametro = c("Año", "Departamento", "Municipio"),
        Valor     = c(as.character(anio_now), as.character(dep_now), as.character(mun_now)),
        stringsAsFactors = FALSE
      )
      
      # 4) Logo: copiar a Descargas y preparar ruta LaTeX
      logo_src <- file.path(app_root, "www", "LOGO_PLATEA.png")
      if (!file.exists(logo_src)) {
        logo_src2 <- file.path(app_root, "WWW", "LOGO_PLATEA.png")
        logo_src  <- if (file.exists(logo_src2)) logo_src2 else NA_character_
      }
      logo_dst <- file.path(EXPORT_DIR, "LOGO_PLATEA.png")
      if (!is.na(logo_src) && file.exists(logo_src)) {
        file.copy(logo_src, logo_dst, overwrite = TRUE)
      }
      logo_tex <- gsub("\\\\", "/", normalizePath(logo_dst, winslash = "/", mustWork = FALSE))
      
      # 5) Si el Rmd tiene placeholder del header, crear Rmd temporal reemplazado
      td <- tempfile("rmd_idm_")
      dir.create(td, recursive = TRUE, showWarnings = FALSE)
      rmd_to_render <- ruta_rmd
      
      rmd_lines <- readLines(ruta_rmd, warn = FALSE, encoding = "UTF-8")
      if (any(grepl("__LOGO_PLATEA_PATH__", rmd_lines, fixed = TRUE))) {
        rmd_tmp <- file.path(td, "Informe_descargable_IDM_render.Rmd")
        rmd_lines <- gsub("__LOGO_PLATEA_PATH__", logo_tex, rmd_lines, fixed = TRUE)
        writeLines(rmd_lines, rmd_tmp, useBytes = TRUE)
        rmd_to_render <- rmd_tmp
      }
      
      # 6) Render DIRECTO al archivo temporal que Shiny entrega (esto evita .htm)
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
          dep          = dep_now,
          mpio         = mun_now,
          ind          = "idm",
          
          img_map      = basename(IMG_MAP),
          img_top10    = basename(IMG_TOP),
          img_dep      = basename(IMG_DEP),
          
          csv_filtrado = CSV_FILT
        ),
        knit_root_dir = app_root,
        envir         = new.env(parent = globalenv())
      )
    },
    contentType = "application/pdf"
  )
}

shinyApp(ui, server)