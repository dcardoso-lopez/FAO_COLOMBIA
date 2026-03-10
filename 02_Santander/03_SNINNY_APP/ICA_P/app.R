# =========================================================
# Dashboard ICA Pecuaria — Santander — Mapa + Serie temporal + Top-10
# (BOTONES TIPO IDM + PNG + CSV + PDF via Rmarkdown)
# MOD: PDF SIN GENERAR/EXPORTAR CSV (csv_filtrado = NULL)
# =========================================================

# ---- Paquetes ----
pkgs <- c(
  "shiny","bslib","shinyWidgets",
  "tidyverse","data.table","janitor","lubridate","scales",
  "sf","leaflet","htmltools","plotly","stringr","DT",
  "htmlwidgets","webshot2","rmarkdown","readr","stringi","ggplot2"
)

# FIX robusto: evita símbolos/elementos vacíos que rompen .getNamespace()
pkgs <- as.character(pkgs)
pkgs <- pkgs[!is.na(pkgs) & nzchar(pkgs)]
stopifnot(is.character(pkgs), length(pkgs) > 0)

suppressWarnings(invisible(lapply(pkgs, function(p) {
  suppressPackageStartupMessages(require(p, character.only = TRUE))
})))

options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

validate <- shiny::validate
need     <- shiny::need

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
safe_first <- function(x, default = "?"){ x <- x[!is.na(x)]; if (length(x)==0) default else x[1] }

# =========================================================
# FIX UTF-8 (robusto)
# =========================================================
safe_utf8 <- function(x){
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  stringi::stri_enc_toutf8(x, is_unknown_8bit = TRUE)
}

# ---- Utilidad: Title Case en español (sin capitalizar conectores) ----
title_case_es <- function(x){
  x <- safe_utf8(x)
  x <- stringr::str_trim(as.character(x))
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  
  small_words <- c("de","del","la","las","los","y","e","o","u","a","en","el","al","da","do","das","dos")
  
  vapply(x, function(s){
    if (is.na(s) || s == "") return(NA_character_)
    words <- strsplit(s, "\\s+")[[1]]
    words <- vapply(seq_along(words), function(i){
      w <- words[i]
      if (i > 1 && w %in% small_words) w else stringr::str_to_title(w, locale = "es")
    }, character(1))
    paste(words, collapse = " ")
  }, character(1))
}

# =========================================================
# ZOOM heurístico (IDM-style)
# =========================================================
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

# =========================================================
# APP ROOT + EXPORT (IDM-style)
# =========================================================
get_app_root <- function(){
  normalizePath(shiny::getShinyOption("appDir") %||% getwd(), winslash = "/", mustWork = FALSE)
}

app_root   <- get_app_root()
EXPORT_DIR <- file.path(app_root, "Descargas")
dir.create(EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)

# Rmd fijo en raíz (nombre real)
ruta_rmd <- file.path(app_root, "Informe_descargable.Rmd")

# Viewport PNG
PNG_VWIDTH    <- 3000
PNG_VHEIGHT   <- 2300
PNG_DELAY_CO  <- 1.4
PNG_DELAY_MUN <- 2.6

# Nombres fijos para PDF
IMG_MAP <- file.path(EXPORT_DIR, "ica_mapa.png")
IMG_SER <- file.path(EXPORT_DIR, "ica_serie.png")
IMG_TOP <- file.path(EXPORT_DIR, "ica_top10.png")

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

save_widget_png_retry <- function(widget, out_png, vwidth, vheight, delay_base){
  delays <- c(delay_base, delay_base + 2.0, delay_base + 4.0)
  for (d in delays){
    ok <- tryCatch(
      save_widget_png(widget, out_png, vwidth = vwidth, vheight = vheight, delay = d),
      error = function(e) FALSE
    )
    if (isTRUE(ok)) return(TRUE)
  }
  FALSE
}

# =========================================================
# Datos GOLDEN + SHP
# =========================================================
golden_dir <- "data"
ica_bovino  <- readRDS(file.path(golden_dir, "101_ICA_CensoPecuario-Bovino.rds"))
ica_porcino <- readRDS(file.path(golden_dir, "102_ICA_CensoPecuario-Porcino.rds"))
ica_bcoe    <- readRDS(file.path(golden_dir, "103_ICA_CensoPecuario-BCOE.rds"))
ica_aviar   <- readRDS(file.path(golden_dir, "104_ICA_CensoPecuario-Aviar.rds"))

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
  mutate(MUNICIPIO_D = title_case_es(MUNICIPIO_D)) %>%
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
  mutate(DEPARTAMENTO_D = title_case_es(DEPARTAMENTO_D)) %>%
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

golden <- bind_rows(g_bov, g_porc, g_buf, g_equ, g_cap, g_ovi, g_aves)

# Filtro fijo Santander
golden <- golden %>% dplyr::filter(DEPARTAMENTO_D == "Santander")

# =========================================================
# 3) Formato y cuartiles
# =========================================================
fmt_num <- function(x, accuracy = 1){
  scales::number(x, accuracy = accuracy, big.mark=".", decimal.mark=",")
}

pal4_vec <- grDevices::colorRampPalette(c("#f0f8f4","#9fd9c0","#2ea56f","#0f7e4f"))(4)

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
  
  labs <- vapply(seq_len(length(bins) - 1), function(i){
    a  <- bins[i]
    b  <- bins[i + 1]
    sa <- fmt_num(a, accuracy = 1)
    sb <- fmt_num(b, accuracy = 1)
    if (i == 1) sprintf("%s – %s", sa, sb) else sprintf("> %s – %s", sa, sb)
  }, character(1))
  
  mids <- (bins[-length(bins)] + bins[-1]) / 2
  cols <- pal(mids)
  
  list(bins=bins, pal=pal, labels=labs, colors=cols)
}

# =========================================================
# 4) UI
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
      body{ background:#ffffff; }
      :root{ --brand-border:#a1d99b; }

      .wrap{ max-width:1360px; margin:0 auto; padding:16px 20px 32px; }
      h2#app-title { text-align:center; margin-top:10px; margin-bottom:5px; font-weight:800; letter-spacing:.3px; }

      .card{
        background:#ffffff;
        border:1px solid var(--brand-border) !important;
        border-radius:16px;
        padding:12px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
        margin-bottom:12px;
      }

      .card-title{ font-weight:700; font-size:16px; margin-bottom:8px; color:#111827; }
      .filter-label{ font-weight:500; font-size:14px; margin-bottom:4px; color:#000000; }

      .filters-grid{
        display:grid;
        grid-template-columns: 1.3fr 1fr 1fr 1fr;
        column-gap:16px;
        row-gap:10px;
        align-items:end;
      }
      @media (max-width: 992px){ .filters-grid{ grid-template-columns: 1fr 1fr; } }
      @media (max-width: 576px){ .filters-grid{ grid-template-columns: 1fr; } }

      .form-select,
      .bootstrap-select > .dropdown-toggle,
      .selectize-input {
        border:1px solid var(--brand-border) !important;
        border-radius:10px !important;
        box-shadow:none !important;
        font-family:'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-size:14px; font-weight:500; color:#000000;
        background-color:#ffffff !important;
      }

      .viz-grid{ display:grid; grid-template-columns: 1.05fr 1fr; gap:12px; align-items:stretch; }
      .viz-right{ display:grid; grid-template-rows: 1fr 1fr; gap:12px; }

      .card-mapa, .card-viz{ display:flex; flex-direction:column; }
      .card-mapa .leaflet-container, .card-viz .html-widget{ flex:1; min-height:350px; }

      @media (max-width: 768px){
        .viz-grid{ grid-template-columns: 1fr; }
        .viz-right{ grid-template-rows: auto auto; }
      }

      .map-note{ margin-top:8px; font-size:11px; color:#555555; font-style:italic; }

      /* botones tipo IDM */
      .btn-unified{
        background:#ffffff !important;
        border:1px solid var(--brand-border) !important;
        color:#374151 !important;
        font-weight:700 !important;
        border-radius:12px !important;
        padding:6px 10px !important;
        font-size:12px !important;
      }

      .footer-actions{
        margin-top: 10px;
        display:flex;
        justify-content:flex-end;
        gap: 8px;
        padding: 6px 6px 0;
        flex-wrap: wrap;
      }

      .leaflet-top.leaflet-right { margin-top: 6px; margin-right: 6px; }
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
        
        div(class="filter-block",
            div(class="filter-label","¿Qué año analizamos?"),
            uiOutput("anio_ui")
        ),
        
        div(class="filter-block",
            div(class="filter-label","¿En qué departamento?"),
            {
              dpts <- dptos_sf |> sf::st_drop_geometry() |>
                dplyr::select(DPTO2, DEPARTAMENTO_D) |>
                dplyr::distinct() |>
                dplyr::arrange(DEPARTAMENTO_D)
              
              dep_golden <- golden |>
                dplyr::select(DPTO2, DEPARTAMENTO_D) |>
                dplyr::distinct()
              
              default_dep <- if (nrow(dep_golden) == 1) dep_golden$DPTO2[1] else "Todos"
              
              selectInput(
                "depto", NULL,
                choices  = c("Todos" = "Todos", stats::setNames(dpts$DPTO2, dpts$DEPARTAMENTO_D)),
                selected = default_dep
              )
            }
        ),
        
        div(class="filter-block",
            div(class="filter-label","¿Algún municipio en particular?"),
            uiOutput("muni_ui")
        ),
        
        div(class="filter-block",
            div(class="filter-label","¿Qué tipo de animal?"),
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
      
      # MAPA
      div(
        class = "card card-mapa",
        div(class="card-title d-flex justify-content-between align-items-center",
            uiOutput("titulo_mapa"),
            downloadButton("dl_png_mapa","Descargar PNG", class="btn-unified")
        ),
        div(
          style = "display:flex; gap:10px; align-items:center; margin-bottom:8px;",
          actionButton("volver", "◀ Volver a Departamentos", class = "btn btn-light"),
          strong(textOutput("nivel_txt", inline = TRUE))
        ),
        leafletOutput("mapa", height = "100%"),
        div(class="map-note",
            "Nota: los rangos de color del mapa se construyen con cuartiles (4 clases) del inventario de animales según el subconjunto de datos filtrado."
        )
      ),
      
      # SERIE + TOP10
      div(
        class = "viz-right",
        div(
          class = "card card-viz",
          div(class="card-title d-flex justify-content-between align-items-center",
              uiOutput("titulo_serie"),
              downloadButton("dl_png_serie","Descargar PNG", class="btn-unified")
          ),
          plotlyOutput("serie", height = "100%")
        ),
        div(
          class = "card card-viz",
          div(class="card-title d-flex justify-content-between align-items-center",
              uiOutput("titulo_top"),
              downloadButton("dl_png_top","Descargar PNG", class="btn-unified")
          ),
          plotlyOutput("top10", height = "100%")
        )
      )
    ),
    
    # ===== Footer (CSV + PDF) =====
    div(
      class = "footer-actions",
      downloadButton("dl_csv_expl","Descargar CSV", class="btn-unified"),
      downloadButton("dl_reporte_pdf","Descargar informe (PDF)", class="btn-unified")
    )
  )
)

# =========================================================
# 5) SERVER
# =========================================================
server <- function(input, output, session){
  
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
    yy <- golden |> dplyr::filter(especie == input$especie) |>
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
    shinyWidgets::updatePickerInput(session,"muni", selected = "Todos")
  })
  
  # --- Datos base ---
  datos_base <- reactive({
    req(input$especie)
    df <- golden |> dplyr::filter(especie == input$especie)
    if (!is.null(input$anio))  df <- df |> dplyr::filter(year == input$anio)
    if (!is.null(input$depto) && input$depto != "Todos") df <- df |> dplyr::filter(DPTO2 == input$depto)
    if (!is.null(input$muni)  && input$muni  != "Todos") df <- df |> dplyr::filter(CODMUN == input$muni)
    validate(need(nrow(df) > 0, "Sin datos para los filtros actuales"))
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
      dplyr::summarise(valor = sum(as.numeric(valor), na.rm = TRUE), .groups = "drop")
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
  
  # ---- bbox para export ----
  bbox_actual <- reactive({
    req(input$especie, input$anio)
    
    if (!is.null(input$muni) && input$muni != "Todos") {
      g <- mpios_sf |> dplyr::filter(CODMUN == input$muni)
      if (nrow(g) > 0) return(sf::st_bbox(g))
    }
    
    if (!is.null(input$depto) && input$depto != "Todos") {
      g <- dptos_sf |> dplyr::filter(DPTO2 == input$depto)
      if (nrow(g) > 0) return(sf::st_bbox(g))
    }
    
    sf::st_bbox(datos_dpto())
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
      addLegend(position="bottomright", colors=bl$colors, labels=bl$labels, opacity=0.9, title="Cantidad") %>%
      htmlwidgets::onRender("function(el,x){ this.zoomControl.setPosition('topright'); }")
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
        labelOptions = leaflet::labelOptions(direction="auto", textsize="12px", sticky=TRUE,
                                             opacity=0.9, style=list("font-weight"="600")),
        highlightOptions = leaflet::highlightOptions(color="#1f5d46", weight=2, bringToFront=TRUE)
      ) %>%
      addLegend("bottomright", colors=bl$colors, labels=bl$labels, opacity=0.9, title="Cantidad")
  }
  
  dibujar_mpios <- function() {
    sf_all <- datos_mpio()
    
    if (!is.null(input$depto) && input$depto != "Todos") {
      sf_all <- sf_all |> dplyr::filter(DPTO2 == input$depto)
    }
    
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
        labelOptions = leaflet::labelOptions(direction="auto", textsize="11px", sticky=TRUE,
                                             opacity=0.9, style=list("font-weight"="600")),
        highlightOptions = leaflet::highlightOptions(color="#1f5d46", weight=2, bringToFront=TRUE)
      ) %>%
      addLegend("bottomright", colors=bl$colors, labels=bl$labels, opacity=0.9, title="Cantidad")
  }
  
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
  
  # ---- Serie (builder para export) ----
  build_serie_plotly <- function(){
    req(input$especie)
    
    df <- golden |> dplyr::filter(especie == input$especie)
    if (!is.null(input$depto) && input$depto != "Todos") df <- df |> dplyr::filter(DPTO2 == input$depto)
    if (!is.null(input$muni)  && input$muni  != "Todos") df <- df |> dplyr::filter(CODMUN == input$muni)
    
    ts <- df %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(valor = sum(as.numeric(valor), na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(year)
    
    validate(need(nrow(ts) > 0, "Sin datos para los filtros actuales"))
    
    rng      <- range(ts$valor, na.rm = TRUE)
    tickvals <- pretty(rng, n = 6)
    ticktext <- vapply(tickvals, fmt_km, character(1))
    
    plot_ly(
      ts,
      x    = ~year,
      y    = ~valor,
      type = "scatter",
      mode = "lines+markers",
      line   = list(color = "#2ea56f", width = 2),
      marker = list(size = 6, color = "#2ea56f"),
      text   = ~paste0("Año: ", year, "<br>Inventario: ", vapply(valor, fmt_km, character(1))),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title="", tickmode="linear", dtick=2, showgrid=FALSE),
        yaxis = list(title="Cantidad", tickvals=tickvals, ticktext=ticktext, showgrid=TRUE),
        margin = list(l=40, r=20, t=10, b=40),
        paper_bgcolor="#ffffff",
        plot_bgcolor ="#ffffff"
      )
  }
  
  output$serie <- renderPlotly({ build_serie_plotly() })
  
  # ---- Top10 (builder para export) ----
  build_top_plotly <- function(){
    df <- datos_base() |>
      dplyr::mutate(valor = suppressWarnings(as.numeric(valor))) |>
      dplyr::filter(is.finite(valor)) |>
      dplyr::group_by(CODMUN) |>
      dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups="drop") |>
      dplyr::left_join(
        mpios_sf |> sf::st_drop_geometry() |> dplyr::select(CODMUN, MUNICIPIO_D),
        by="CODMUN"
      )
    
    top <- df |>
      dplyr::arrange(dplyr::desc(valor)) |>
      dplyr::slice_head(n = 10) |>
      dplyr::mutate(
        MUNICIPIO_D = title_case_es(MUNICIPIO_D),
        valor_fmt   = vapply(valor, fmt_km, character(1))
      )
    
    validate(need(nrow(top) > 0, "Sin datos para el Top-10"))
    
    g <- ggplot(
      top,
      aes(
        x    = valor,
        y    = reorder(MUNICIPIO_D, valor),
        text = paste0(MUNICIPIO_D, " — ", valor_fmt)
      )
    ) +
      geom_col(fill = bar_fill) +
      geom_text(aes(x = valor/2, label = valor_fmt), color="white", fontface="bold", size=3.8) +
      scale_x_continuous(labels = function(x) vapply(x, fmt_km, character(1))) +
      labs(x = "Cantidad", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background  = element_rect(fill="white", color=NA),
        panel.background = element_rect(fill="white", color=NA)
      )
    
    p <- ggplotly(g, tooltip="text") %>% layout(paper_bgcolor="#ffffff", plot_bgcolor="#ffffff")
    if (is.null(p$x$layout$margin)) p$x$layout$margin <- list()
    p$x$layout$margin$l <- 90
    p$x$layout$margin$r <- 5
    p
  }
  
  output$top10 <- renderPlotly({ build_top_plotly() })
  
  # =========================================================
  # MAPA EXPORT (widget dedicado, leyenda + zoom IDM)
  # =========================================================
  map_widget_export <- reactive({
    req(input$especie, input$anio)
    
    bb  <- bbox_actual()
    lng <- mean(c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"])))
    lat <- mean(c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"])))
    z   <- zoom_from_bbox(bb)
    
    en_deptos <- is.null(input$depto) || input$depto == "Todos"
    
    base_map <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 10, maxZoom = 10, zoomSnap = 0.25)) %>%
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.Positron,
        options = leaflet::providerTileOptions(crossOrigin = TRUE)
      ) %>%
      htmlwidgets::onRender("function(el,x){ this.zoomControl.setPosition('topright'); }") %>%
      leaflet::setView(lng, lat, zoom = z)
    
    if (en_deptos) {
      sf_m <- datos_dpto()
      bl   <- build_bins_labels(sf_m$valor)
      pal  <- bl$pal
      
      base_map %>%
        leaflet::addPolygons(
          data = sf_m,
          layerId = ~DPTO2,
          fillColor = ~pal(valor),
          color="#3a6b57", weight=0.7, fillOpacity=0.9
        ) %>%
        leaflet::addLegend(position="bottomright", colors=bl$colors, labels=bl$labels, opacity=0.9, title="Cantidad")
    } else {
      sf_all <- datos_mpio()
      if (!is.null(input$depto) && input$depto != "Todos") sf_all <- sf_all |> dplyr::filter(DPTO2 == input$depto)
      
      bl  <- build_bins_labels(sf_all$valor)
      pal <- bl$pal
      
      base_map %>%
        leaflet::addPolygons(
          data = sf_all,
          layerId = ~CODMUN,
          fillColor = ~pal(valor),
          color="#3a6b57", weight=0.4, fillOpacity=0.9
        ) %>%
        leaflet::addLegend(position="bottomright", colors=bl$colors, labels=bl$labels, opacity=0.9, title="Cantidad")
    }
  })
  
  # =========================================================
  # TABLA EXPORT (CSV) — solo para botón CSV
  # =========================================================
  tabla_export <- reactive({
    df <- datos_base() |>
      dplyr::mutate(valor = suppressWarnings(as.numeric(valor))) |>
      dplyr::filter(is.finite(valor)) |>
      dplyr::left_join(
        mpios_sf |> sf::st_drop_geometry() |> dplyr::select(CODMUN, MUNICIPIO_D),
        by="CODMUN"
      ) |>
      dplyr::left_join(
        dptos_sf |> sf::st_drop_geometry() |> dplyr::select(DPTO2, DEPARTAMENTO_D),
        by="DPTO2"
      ) |>
      dplyr::transmute(
        anio = year,
        especie,
        DPTO2,
        DEPARTAMENTO = DEPARTAMENTO_D,
        CODMUN,
        MUNICIPIO = MUNICIPIO_D,
        valor
      )
    df
  })
  
  # =========================================================
  # DESCARGAS PNG
  # =========================================================
  output$dl_png_mapa <- downloadHandler(
    filename = function(){
      dep_tag <- if (is.null(input$depto) || input$depto=="Todos") "Santander" else input$depto
      mun_tag <- if (is.null(input$muni)  || input$muni =="Todos") "Todos" else input$muni
      paste0("ICA_Santander_mapa_", input$especie %||% "NA", "_", dep_tag, "_", mun_tag, "_", input$anio %||% "NA", "_", Sys.Date(), ".png")
    },
    content = function(file){
      dly <- if (!is.null(input$depto) && input$depto != "Todos") PNG_DELAY_MUN else PNG_DELAY_CO
      ok <- save_widget_png_retry(map_widget_export(), file, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay_base = dly)
      if (!ok) stop("No se pudo generar el PNG del mapa. Revisa webshot2/Chromium (y tiles).")
    }
  )
  
  output$dl_png_serie <- downloadHandler(
    filename = function(){ paste0("ICA_Santander_serie_", input$especie %||% "NA", "_", Sys.Date(), ".png") },
    content  = function(file){
      ok <- save_widget_png_retry(build_serie_plotly(), file, vwidth = 1800, vheight = 900, delay_base = 0.9)
      if (!ok) stop("No se pudo generar el PNG de la serie.")
    }
  )
  
  output$dl_png_top <- downloadHandler(
    filename = function(){ paste0("ICA_Santander_top10_", input$especie %||% "NA", "_", input$anio %||% "NA", "_", Sys.Date(), ".png") },
    content  = function(file){
      ok <- save_widget_png_retry(build_top_plotly(), file, vwidth = 1800, vheight = 1000, delay_base = 0.9)
      if (!ok) stop("No se pudo generar el PNG del Top-10.")
    }
  )
  
  # =========================================================
  # CSV (solo botón CSV)
  # =========================================================
  output$dl_csv_expl <- downloadHandler(
    filename = function(){
      dep_tag <- if (is.null(input$depto) || input$depto=="Todos") "Santander" else input$depto
      mun_tag <- if (is.null(input$muni)  || input$muni =="Todos") "Todos" else input$muni
      paste0("ICA_Santander_base_filtrada_", input$especie %||% "NA", "_", dep_tag, "_", mun_tag, "_", input$anio %||% "NA", "_", Sys.Date(), ".csv")
    },
    content = function(file){
      readr::write_csv(tabla_export(), file, na = "")
    }
  )
  
  # =========================================================
  # PDF (SIN CSV)
  # =========================================================
  output$dl_reporte_pdf <- downloadHandler(
    filename = function(){
      dep_tag <- if (is.null(input$depto) || input$depto=="Todos") "Santander" else input$depto
      mun_tag <- if (is.null(input$muni)  || input$muni =="Todos") "Todos" else input$muni
      paste0("Informe_descargable_ICA_Santander_", input$especie %||% "NA", "_", dep_tag, "_", mun_tag, "_", input$anio %||% "NA", "_", Sys.Date(), ".pdf")
    },
    content = function(file){
      
      if (!file.exists(ruta_rmd)) stop("No encuentro Informe_descargable.Rmd en la raíz del proyecto.")
      
      # snapshot inputs
      anio_now <- input$anio
      esp_now  <- input$especie %||% "NA"
      dep_now  <- input$depto %||% "Todos"
      mun_now  <- input$muni  %||% "Todos"
      
      # PNGs fijos en ./Descargas
      dly_map <- if (!is.null(dep_now) && dep_now != "Todos") PNG_DELAY_MUN else PNG_DELAY_CO
      
      ok_map <- save_widget_png_retry(map_widget_export(), IMG_MAP, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay_base = dly_map)
      ok_ser <- save_widget_png_retry(build_serie_plotly(), IMG_SER, vwidth = 1800, vheight = 900,  delay_base = 0.9)
      ok_top <- save_widget_png_retry(build_top_plotly(),   IMG_TOP, vwidth = 1800, vheight = 1000, delay_base = 0.9)
      
      if (!ok_map) stop("No se pudo generar Descargas/ica_mapa.png para el informe.")
      if (!ok_ser) stop("No se pudo generar Descargas/ica_serie.png para el informe.")
      if (!ok_top) stop("No se pudo generar Descargas/ica_top10.png para el informe.")
      
      # Tabla filtros
      dep_disp <- if (is.null(dep_now) || dep_now=="Todos") "Santander" else {
        d <- dptos_sf |> sf::st_drop_geometry() |> dplyr::filter(DPTO2 == dep_now) |> dplyr::pull(DEPARTAMENTO_D)
        if (!length(d) || is.na(d[1])) as.character(dep_now) else d[1]
      }
      mp_disp <- if (is.null(mun_now) || mun_now=="Todos") "Todos" else {
        m <- mpios_sf |> sf::st_drop_geometry() |> dplyr::filter(CODMUN == mun_now) |> dplyr::pull(MUNICIPIO_D)
        if (!length(m) || is.na(m[1])) as.character(mun_now) else m[1]
      }
      
      filtros_tbl <- data.frame(
        Parametro = c("Año", "Especie", "Departamento", "Municipio"),
        Valor     = c(as.character(anio_now), as.character(esp_now), dep_disp, mp_disp),
        stringsAsFactors = FALSE
      )
      
      # Logo (si tu Rmd usa placeholder, esto lo soporta; si no, no estorba)
      logo_src <- file.path(app_root, "www", "LOGO_PLATEA.png")
      if (!file.exists(logo_src)) {
        logo_src2 <- file.path(app_root, "WWW", "LOGO_PLATEA.png")
        logo_src  <- if (file.exists(logo_src2)) logo_src2 else NA_character_
      }
      logo_dst <- file.path(EXPORT_DIR, "LOGO_PLATEA.png")
      if (!is.na(logo_src) && file.exists(logo_src)) file.copy(logo_src, logo_dst, overwrite = TRUE)
      logo_tex <- gsub("\\\\", "/", normalizePath(logo_dst, winslash = "/", mustWork = FALSE))
      
      # Si el Rmd tiene placeholder del header, crear Rmd temporal reemplazado
      td <- tempfile("rmd_ica_")
      dir.create(td, recursive = TRUE, showWarnings = FALSE)
      
      rmd_to_render <- ruta_rmd
      rmd_lines <- readLines(ruta_rmd, warn=FALSE, encoding="UTF-8")
      if (any(grepl("__LOGO_PLATEA_PATH__", rmd_lines, fixed=TRUE))) {
        rmd_tmp <- file.path(td, "Informe_descargable_ICA_render.Rmd")
        rmd_lines <- gsub("__LOGO_PLATEA_PATH__", logo_tex, rmd_lines, fixed = TRUE)
        writeLines(rmd_lines, rmd_tmp, useBytes = TRUE)
        rmd_to_render <- rmd_tmp
      }
      
      # Render directo al archivo de Shiny (evita .htm)
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
          especie      = esp_now,
          departamento = dep_disp,
          municipio    = mp_disp,
          ind          = paste0("ica_", esp_now),
          
          img_map      = basename(IMG_MAP),
          img_serie    = basename(IMG_SER),
          img_ranking  = basename(IMG_TOP),
          
          # MOD: NO CSV EN EL INFORME
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
