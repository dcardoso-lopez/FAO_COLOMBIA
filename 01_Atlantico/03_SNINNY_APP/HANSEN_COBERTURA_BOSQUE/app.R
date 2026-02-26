# =========================================================
# Shiny App — COBERTURA NETA DE BOSQUE (Hansen)
# (BOTONES TIPO IDM + PDF via Rmarkdown)
# - PNG mapa (con leyenda) en título
# - PNG serie / ranking en título
# - Footer: CSV + PDF (renderiza Informe_descargable.Rmd)
# - PDF: genera PNGs + CSV en ./Descargas y renderiza Informe_descargable.Rmd
# =========================================================

# 1) Paquetes
pkgs <- c(
  "shiny","bslib","dplyr","readr","stringi","sf","leaflet",
  "plotly","ggplot2","htmltools","webshot2","htmlwidgets",
  "ragg","glue","scales","tidyr","rmarkdown"
)
suppressWarnings(invisible(sapply(pkgs, require, character.only = TRUE)))
options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

# 2) Rutas (usando el directorio actual del script)
if (interactive()) {
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    APP_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
  } else {
    APP_DIR <- getwd()
  }
} else {
  APP_DIR <- getwd()
}
if (is.null(APP_DIR) || APP_DIR == "") APP_DIR <- getwd()

DATA_RDS <- file.path(APP_DIR, "data/141_HANSEN_COBERTURA_NETA_TOTAL.rds")
SHP_DIR  <- file.path(APP_DIR, "data/shp")

if (!file.exists(DATA_RDS)) stop("No encuentro la base RDS en: ", DATA_RDS)
shp_files <- list.files(SHP_DIR, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
if (!length(shp_files)) stop("No encuentro archivos .shp en: ", SHP_DIR)

# ---------- Export ----------
app_root <- APP_DIR

EXPORT_DIR <- file.path(app_root, "Descargas")
dir.create(EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)

# RMD reporte (en raíz o data/)
ruta_rmd_root <- file.path(app_root, "Informe_descargable.Rmd")
ruta_rmd_data <- file.path(app_root, "data", "Informe_descargable.Rmd")
ruta_rmd      <- if (file.exists(ruta_rmd_root)) ruta_rmd_root else ruta_rmd_data

PNG_VWIDTH  <- 3000
PNG_VHEIGHT <- 2300
PNG_DELAY   <- 1.2

# Nombres fijos para informe
IMG_MAP  <- file.path(EXPORT_DIR, "bosque_mapa.png")
IMG_SER  <- file.path(EXPORT_DIR, "bosque_serie.png")
IMG_RNK  <- file.path(EXPORT_DIR, "bosque_ranking.png")
CSV_FILT <- file.path(EXPORT_DIR, "bosque_base_filtrada.csv")

save_widget_png <- function(widget, out_png, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay = PNG_DELAY){
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

# 3) Helpers básicos
norm_txt   <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
pick_first <- function(nms, candidates){
  cand <- candidates[candidates %in% nms]
  if (!length(cand)) NA_character_ else cand[1]
}
find_shp   <- function(files, key){
  i <- grep(key, basename(files), ignore.case = TRUE)
  if (!length(i)) NA_character_ else files[i[1]]
}
safe_chr <- function(x) if (is.null(x)) "" else as.character(x)

# Title Case (ES)
title_case_es <- function(x){
  stopw <- c(
    "de","del","la","las","los","y","e","o","u","en","a","al","por","para",
    "con","sin","sobre","entre","hasta","desde","contra","ante","tras",
    "que","el","su","un","una","unos","unas"
  )
  vapply(x, function(s){
    if (is.na(s) || !nzchar(s)) return(s)
    s <- tolower(trimws(as.character(s)))
    toks <- strsplit(s, "\\s+", perl = TRUE)[[1]]
    toks_out <- character(length(toks))
    for (i in seq_along(toks)){
      base <- toks[i]
      if (i == 1 || !(base %in% stopw)) toks_out[i] <- stringi::stri_trans_totitle(base, locale = "es") else toks_out[i] <- base
    }
    paste(toks_out, collapse = " ")
  }, character(1))
}

sanitize_filename <- function(x){
  x <- as.character(x)
  x <- gsub("[/\\\\:*?\"<>|]", "_", x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("__+", "_", x)
  trimws(x)
}

# ---------- Formato numérico ES ----------
format_num_es <- function(x, digits = 1){
  scales::number(x, accuracy = 10^-digits, big.mark = ".", decimal.mark = ",")
}
format_int_es <- function(x) format_num_es(x, digits = 0)
format_pct_es <- function(x, digits = 1) paste0(format_num_es(x, digits = digits), "%")

# Formato corto K / M
format_short <- function(x){
  ifelse(
    is.na(x), NA_character_,
    ifelse(
      abs(x) >= 1e6,
      paste0(format_num_es(x / 1e6, digits = 1), "M"),
      ifelse(
        abs(x) >= 1e3,
        paste0(format_num_es(x / 1e3, digits = 1), "K"),
        format_int_es(x)
      )
    )
  )
}

# Filtros: pares únicos + choices
distinct_pairs <- function(df, key_col, disp_col){
  stopifnot(key_col %in% names(df), disp_col %in% names(df))
  df |>
    dplyr::distinct(dplyr::across(all_of(c(key_col, disp_col)))) |>
    dplyr::filter(!is.na(.data[[key_col]]), nzchar(.data[[key_col]])) |>
    dplyr::arrange(.data[[disp_col]])
}
mk_tc_from_pairs <- function(keys, labels_disp){
  stopifnot(length(keys) == length(labels_disp))
  labs_tc <- title_case_es(labels_disp)
  out <- stats::setNames(as.character(keys), labs_tc)
  out <- out[order(names(out), na.last = TRUE)]
  c("Todos" = "Todos", out)
}

# ---------- labelFormat ES para leaflet ----------
labelFormat_es <- function(suffix = "",
                           digits = 1,
                           big.mark = ".",
                           decimal.mark = ",",
                           between = " – ",
                           first_no_symbol = TRUE) {
  force(suffix); force(digits); force(big.mark); force(decimal.mark)
  force(between); force(first_no_symbol)
  
  function(type = "numeric", cuts, p = NULL) {
    if (type %in% c("numeric","bin")) {
      n <- length(cuts)
      if (n <= 1) return(character(0))
      
      left  <- cuts[-n]
      right <- cuts[-1]
      
      fmt <- function(x) {
        format(round(x, digits), big.mark = big.mark, decimal.mark = decimal.mark)
      }
      
      left_chr  <- fmt(left)
      right_chr <- fmt(right)
      
      labs <- character(n - 1)
      for (i in seq_len(n - 1)) {
        if (i == 1 && first_no_symbol) {
          labs[i] <- paste0(left_chr[i], between, right_chr[i], suffix)
        } else if (i == n - 1) {
          labs[i] <- paste0("> ", left_chr[i], suffix)
        } else {
          labs[i] <- paste0("> ", left_chr[i], between, right_chr[i], suffix)
        }
      }
      return(labs)
    }
    
    vals_chr <- format(round(cuts, digits), big.mark = big.mark, decimal.mark = decimal.mark)
    paste0(vals_chr, suffix)
  }
}

# ================== Colores ==================
SERIE_COLOR <- "#2E7D32"
BAR_COLOR   <- "#2E7D32"

pal4_vec <- grDevices::colorRampPalette(
  c("#E8F5E9", "#81C784", "#43A047", "#1B5E20")
)(4)

make_bins4 <- function(values, clamp01 = FALSE){
  v <- as.numeric(values); v <- v[is.finite(v)]
  if (clamp01) v <- pmax(pmin(v, 100), 0)
  if (!length(v)) return(seq(0, 4))
  
  qs <- quantile(v, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE, type = 7)
  qs <- sort(unique(as.numeric(qs)))
  
  if (length(qs) < 5){
    r <- range(v, na.rm=TRUE)
    if (r[1] == r[2]) r <- c(0, max(1, r[2]))
    qs <- pretty(r, n = 4)
  }
  if (length(qs) < 5) qs <- seq(min(qs), max(qs), length.out = 5)
  qs
}

palBin4 <- function(values, clamp01 = FALSE){
  bins <- make_bins4(values, clamp01 = clamp01)
  pal  <- leaflet::colorBin(
    palette  = pal4_vec,
    bins     = bins,
    domain   = values,
    na.color = "#f0f0f0",
    right    = FALSE
  )
  attr(pal, "bins") <- bins
  pal
}

# Zoom heurístico (como IDM/UPRA)
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

# =========================================================
# 4) Shapefiles
# =========================================================
ruta_shp_mpios <- find_shp(shp_files, "MPIO|MUN")
ruta_shp_dptos <- find_shp(shp_files, "DPTO|DEP|DEPT")
if (is.na(ruta_shp_mpios) || is.na(ruta_shp_dptos))
  stop("No pude detectar SHP de mpios/dptos en ", SHP_DIR)

mpios_sf_raw <- sf::st_read(ruta_shp_mpios, quiet = TRUE)
depto_sf_raw <- sf::st_read(ruta_shp_dptos, quiet = TRUE)

muni_name_cands       <- c("MUNICIPIO_D","MPIO_CNMBR","NOMBRE_MPIO","NOMBRE_MUNICIP","NOMBRE","MUNICIPIO")
depto_name_cands      <- c("DEPARTAMENTO_D","DPTO_CNMBR","NOMBRE_DPT","NOMBRE_DEPTO","DEPARTAMEN","DEPARTAMENTO")
depto_code_cands      <- c("DPTO_CCDGO","COD_DPTO","DPTO","CODIGO_DPTO","COD_DEPART","DPTO_COD")
muni_depto_code_cands <- c("DPTO_CCDGO","COD_DPTO","DPTO","CODIGO_DPTO","DPTO_COD")

mpn <- names(mpios_sf_raw); dpn <- names(depto_sf_raw)
muni_name_col  <- pick_first(mpn, muni_name_cands)
muni_dpto_code <- pick_first(mpn, muni_depto_code_cands)
depto_name_col <- pick_first(dpn, depto_name_cands)
depto_code_col <- pick_first(dpn, depto_code_cands)
stopifnot(!is.na(muni_name_col), !is.na(muni_dpto_code),
          !is.na(depto_name_col), !is.na(depto_code_col))

depto_key <- depto_sf_raw |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    dpto_code        = .data[[depto_code_col]],
    DEPARTAMENTO_RAW = as.character(.data[[depto_name_col]]),
    DEPARTAMENTO_D   = toupper(norm_txt(.data[[depto_name_col]]))
  )

mpios_sf <- mpios_sf_raw |>
  dplyr::mutate(
    MUNICIPIO_RAW = as.character(.data[[muni_name_col]]),
    MUNICIPIO_D   = norm_txt(.data[[muni_name_col]]),
    dpto_code     = .data[[muni_dpto_code]]
  ) |>
  dplyr::left_join(depto_key, by = "dpto_code")

mpios_sf <- sf::st_transform(mpios_sf, 4326) |>
  sf::st_make_valid() |>
  sf::st_zm(drop = TRUE, what = "ZM") |>
  dplyr::mutate(
    DEPARTAMENTO_D = DEPARTAMENTO_D,
    MPIO_TC  = title_case_es(MUNICIPIO_RAW),
    DEPTO_TC = title_case_es(DEPARTAMENTO_RAW)
  )

depto_sf <- sf::st_transform(depto_sf_raw, 4326) |>
  sf::st_make_valid() |>
  sf::st_zm(drop = TRUE, what = "ZM") |>
  dplyr::mutate(
    DEPARTAMENTO_RAW = as.character(.data[[depto_name_col]]),
    DEPARTAMENTO_D   = toupper(norm_txt(DEPARTAMENTO_RAW)),
    DEPTO_TC         = title_case_es(DEPARTAMENTO_RAW)
  )

# =========================================================
# 5) Base COBERTURA NETA
# =========================================================
base_raw <- readRDS(DATA_RDS)

# (Tu filtro original; si quieres panorama nacional real, quita esta línea)
base_raw <- base_raw %>% dplyr::filter(DEPARTAMENTO_D=="ATLÁNTICO")

eva_df <- base_raw |>
  dplyr::mutate(
    anio             = as.integer(.data[["ano"]]),
    cobertura_ha     = suppressWarnings(as.numeric(.data[["cobertura_neta_ha"]])),
    cobertura_pct    = suppressWarnings(as.numeric(.data[["cobertura_neta_pct"]])),
    base_ha_2000     = suppressWarnings(as.numeric(.data[["base_ha_2000"]])),
    MUNICIPIO_RAW    = as.character(.data[["MUNICIPIO_D"]]),
    DEPARTAMENTO_RAW = as.character(.data[["DEPARTAMENTO_D"]]),
    MUNICIPIO_D      = norm_txt(MUNICIPIO_RAW),
    DEPARTAMENTO_D   = toupper(norm_txt(DEPARTAMENTO_RAW))
  ) |>
  dplyr::mutate(
    DEPTO_KEY  = DEPARTAMENTO_D,
    MPIO_KEY   = MUNICIPIO_D,
    DEPTO_DISP = DEPARTAMENTO_RAW,
    MPIO_DISP  = MUNICIPIO_RAW,
    DEPTO_TC   = title_case_es(DEPARTAMENTO_RAW),
    MPIO_TC    = title_case_es(MUNICIPIO_RAW)
  )

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    primary = "#2E7D32",
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius"  = "0.9rem",
    "font-size-base" = "0.95rem"
  ),
  tags$head(
    tags$style(HTML("
      :root{
        --accent-border:#2E7D32;
        --gap:12px;
        --viz-row-top:410px;
        --viz-row-bottom:410px;
      }
      .wrap{ max-width:1360px; margin:0 auto; padding:16px 20px 32px; }
      h2#app-title{ font-weight:700; letter-spacing:.2px; margin-top:4px; margin-bottom:6px; }
      .data-note{ font-size:13px; color:#6b7280; margin:0 0 16px; }
      .filters{
        background:#fff;border:1px solid var(--accent-border);border-radius:16px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);padding:6px 12px 8px;margin-bottom:12px;
      }
      .filters-grid{
        display:grid;grid-template-columns:repeat(4,minmax(200px,1fr));gap:12px;align-items:stretch;
      }
      .filter{ display:flex; flex-direction:column; justify-content:flex-start; }
      .filter-label{
        font-family:'Inter', system-ui; font-size:14px; font-weight:500; letter-spacing:.2px;
        color:#111827; margin-bottom:6px;
      }
      .filters-grid .shiny-input-container{ margin:0 !important; }
      .filters-grid .selectize-input, .filters-grid .form-control, .filters-grid .form-select{
        height:60px !important; min-height:60px; padding-top:10px; padding-bottom:10px;
        border-radius:10px; border:1px solid var(--accent-border) !important;
      }
      .filters-grid .selectize-input:focus, .filters-grid .form-control:focus, .filters-grid .form-select:focus{
        border-color:var(--accent-border) !important;
        box-shadow:0 0 0 .2rem rgba(46,125,50,.25) !important;
      }

      .card{
        background:#fff;border:1px solid var(--accent-border);border-radius:16px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);padding:12px;margin-bottom:12px;
      }
      .card-title{ font-weight:700; font-size:16px; margin-bottom:8px; color:#111827; }

      .content-grid{
        display:grid;
        grid-template-columns:1.05fr 1fr;
        grid-template-rows:var(--viz-row-top) var(--viz-row-bottom);
        gap:var(--gap);
      }
      .viz-card{ display:flex; flex-direction:column; height:100%; margin:0; }
      .viz-body{ flex:1 1 auto; min-height:0; }
      .viz-map{ grid-row:1 / span 2; }
      .viz-body .leaflet, .viz-body .plotly.html-widget{ height:100% !important; }

      .leaflet-tooltip.lbl-clean{
        background: rgba(255,255,255,.92);
        border: 1px solid #e6e6e6;
        border-radius: 6px;
        padding: 4px 6px;
        color: #222;
        font-weight: 600;
        box-shadow: 0 1px 4px rgba(0,0,0,.08);
      }
      .map-note{ margin-top:6px; font-size:12px; color:#4b5563; }

      /* BOTONES TIPO IDM */
      .btn-unified{
        background:#ffffff !important;
        border:1px solid var(--accent-border) !important;
        color:#374151 !important;
        font-weight:700 !important;
        border-radius:12px !important;
        padding:6px 10px !important;
        font-size:12px !important;
      }
      .btn-unified:hover,.btn-unified:focus,.btn-unified:active{
        background:#ffffff !important;
        border-color:var(--accent-border) !important;
        color:#111827 !important;
        box-shadow:0 0 0 .2rem rgba(46,125,50,.15) !important;
      }

      .footer-actions{
        margin-top: 10px;
        display:flex;
        justify-content:flex-end;
        gap: 8px;
        padding: 6px 6px 0;
        flex-wrap: wrap;
      }
    "))
  ),
  
  div(
    class = "wrap",
    h2("", id = "app-title"),
    div(class = "data-note", HTML("")),
    
    # ---------- Filtros ----------
    div(
      class = "filters",
      div(
        class = "filters-grid",
        div(
          class="filter",
          div(class="filter-label","¿Qué año analizamos?"),
          uiOutput("anio_ui")
        ),
        div(
          class="filter",
          div(class="filter-label","¿Qué indicador quieres ver?"),
          selectInput(
            "f_metric", NULL,
            choices = c("Cobertura neta (Ha)"="ha", "Cobertura neta (%)"="pct"),
            selected = "pct"
          )
        ),
        div(
          class="filter",
          div(class="filter-label","¿En que departamento?"),
          {
            dep_pairs <- distinct_pairs(eva_df, "DEPTO_KEY", "DEPTO_DISP")
            idx_santander <- which(toupper(norm_txt(dep_pairs$DEPTO_DISP)) == "ATLÁNTICO")
            default_dep <- if (length(idx_santander)) dep_pairs$DEPTO_KEY[idx_santander[1]] else "Todos"
            selectInput(
              "f_depto", NULL,
              choices  = mk_tc_from_pairs(dep_pairs$DEPTO_KEY, dep_pairs$DEPTO_DISP),
              selected = default_dep
            )
          }
        ),
        div(
          class="filter",
          div(class="filter-label","¿Algún municipio en particular?"),
          {
            mpio_pairs <- distinct_pairs(eva_df, "MPIO_KEY", "MPIO_DISP")
            selectInput(
              "f_mpio", NULL,
              choices  = mk_tc_from_pairs(mpio_pairs$MPIO_KEY, mpio_pairs$MPIO_DISP),
              selected = "Todos"
            )
          }
        )
      )
    ),
    
    # ---------- Contenido ----------
    div(
      class = "content-grid",
      
      # --- Mapa (2 filas) ---
      div(
        class = "card viz-card viz-map",
        div(class="card-title d-flex justify-content-between align-items-center",
            span(textOutput("titulo_mapa")),
            downloadButton("dl_png_mapa","Descargar PNG", class="btn-unified")
        ),
        div(
          style="display:flex; gap:10px; align-items:center; margin-bottom:8px;",
          actionButton("btn_volver", "◀ Volver al panorama nacional", class="btn btn-light"),
          strong(textOutput("nivel_txt", inline = TRUE))
        ),
        div(class="viz-body", leafletOutput("map_eva", height = "100%")),
        div(
          class = "map-note",
          HTML("Nota: El mapa clasifica los valores del indicador en cuartiles (cuatro grupos con igual número de observaciones).")
        )
      ),
      
      # --- Serie temporal ---
      div(
        class = "card viz-card",
        div(class="card-title d-flex justify-content-between align-items-center",
            span(textOutput("titulo_serie")),
            downloadButton("dl_png_series","Descargar PNG", class="btn-unified")
        ),
        div(class="viz-body", plotlyOutput("plot_arriba", height = "100%"))
      ),
      
      # --- Ranking Top-10 ---
      div(
        class = "card viz-card",
        div(class="card-title d-flex justify-content-between align-items-center",
            span(textOutput("titulo_ranking")),
            downloadButton("dl_png_ranking","Descargar PNG", class="btn-unified")
        ),
        div(class="viz-body", plotlyOutput("ranking_abajo", height = "100%"))
      )
    ),
    
    # Footer: CSV + PDF
    div(
      class="footer-actions",
      downloadButton("dl_csv_expl","Descargar CSV", class="btn-unified"),
      downloadButton("dl_reporte_pdf","Descargar informe (PDF)", class="btn-unified")
    )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  
  nivel_mapa <- reactiveVal("depto")
  depto_sel  <- reactiveVal(NULL)
  
  output$nivel_txt <- renderText({
    if (nivel_mapa() == "depto") "Nivel: Departamentos"
    else paste0("Nivel: Municipios — ", title_case_es(depto_sel()))
  })
  
  # Años disponibles
  output$anio_ui <- renderUI({
    yrs <- sort(unique(na.omit(as.integer(eva_df$anio))))
    selectInput("f_anio", NULL, choices = yrs, selected = max(yrs))
  })
  
  # Cascada dpto -> mpio
  observeEvent(input$f_depto, ignoreInit = TRUE, {
    if (is.null(input$f_depto) || input$f_depto == "Todos"){
      munis <- distinct_pairs(eva_df, "MPIO_KEY", "MPIO_DISP")
    } else {
      munis <- eva_df |>
        dplyr::filter(DEPTO_KEY == input$f_depto) |>
        distinct_pairs("MPIO_KEY", "MPIO_DISP")
    }
    updateSelectInput(
      session, "f_mpio",
      choices  = mk_tc_from_pairs(munis$MPIO_KEY, munis$MPIO_DISP),
      selected = "Todos"
    )
  })
  
  # Datos filtrados (AÑO + filtros)
  datos_filtrados <- reactive({
    req(input$f_anio)
    df <- eva_df |> dplyr::filter(anio == input$f_anio)
    if (!is.null(input$f_depto) && input$f_depto!="Todos")
      df <- df |> dplyr::filter(DEPTO_KEY == input$f_depto)
    if (!is.null(input$f_mpio)  && input$f_mpio !="Todos")
      df <- df |> dplyr::filter(MPIO_KEY  == input$f_mpio)
    df
  })
  
  # Títulos
  output$titulo_mapa <- renderText({
    if (input$f_metric == "ha") {
      "¿En cuáles territorios está la mayor cantidad de hectáreas en cobertura neta boscosa?"
    } else {
      "¿En cuáles territorios está el mayor porcentaje de cobertura neta boscosa?"
    }
  })
  output$titulo_serie <- renderText({
    if (input$f_metric == "ha") {
      "¿Cómo ha evolucionado la cantidad de cobertura de bosque en el tiempo?"
    } else {
      "¿Cómo ha evolucionado el porcentaje de cobertura de bosque en el tiempo?"
    }
  })
  output$titulo_ranking <- renderText({
    if (input$f_metric == "ha") {
      "Top 10 municipios con mayor cantidad de hectáreas en cobertura neta de bosque"
    } else {
      "Top 10 municipios con mayor porcentaje de cobertura neta de bosque"
    }
  })
  
  # Agregaciones por DEPARTAMENTO (en el año filtrado)
  agg_depto <- reactive({
    df <- datos_filtrados()
    if (input$f_metric == "ha") {
      df |>
        dplyr::group_by(DEPARTAMENTO_D) |>
        dplyr::summarise(valor = sum(cobertura_ha, na.rm = TRUE), .groups = "drop")
    } else {
      df |>
        dplyr::group_by(DEPARTAMENTO_D) |>
        dplyr::summarise(
          base_sum = sum(base_ha_2000, na.rm = TRUE),
          cob_sum  = sum(cobertura_ha,  na.rm = TRUE),
          valor    = dplyr::if_else(base_sum > 0, 100 * cob_sum / base_sum, NA_real_),
          .groups  = "drop"
        ) |>
        dplyr::select(DEPARTAMENTO_D, valor)
    }
  })
  
  # Agregación por MUNICIPIO (para el departamento seleccionado en el mapa)
  agg_mpio <- reactive({
    req(input$f_anio)
    df <- eva_df |> dplyr::filter(anio == input$f_anio)
    if (!is.null(depto_sel()))
      df <- df |> dplyr::filter(DEPARTAMENTO_D == depto_sel())
    
    if (input$f_metric == "ha") {
      df |>
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D, MUNICIPIO_RAW, DEPARTAMENTO_RAW) |>
        dplyr::summarise(valor = sum(cobertura_ha, na.rm = TRUE), .groups = "drop")
    } else {
      df |>
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D, MUNICIPIO_RAW, DEPARTAMENTO_RAW) |>
        dplyr::summarise(
          base_sum = sum(base_ha_2000, na.rm = TRUE),
          cob_sum  = sum(cobertura_ha,  na.rm = TRUE),
          valor    = dplyr::if_else(base_sum > 0, 100 * cob_sum / base_sum, NA_real_),
          .groups = "drop"
        )
    }
  })
  
  badge_filtros <- reactive({
    yr  <- safe_chr(input$f_anio)
    met <- if (input$f_metric=="ha") "Cobertura neta (Ha)" else "Cobertura neta (%)"
    htmltools::HTML(sprintf(
      '<div style="background:#fff;padding:6px 10px;border-radius:8px;
                   box-shadow:0 1px 6px rgba(0,0,0,.15);font-size:12px;line-height:1.3;">
         <b>Indicador:</b> %s<br>
         <b>Año:</b> %s
       </div>', met, yr))
  })
  
  hover_label_opts       <- leaflet::labelOptions(direction="auto", textsize="12px", sticky=TRUE, opacity=0.95, className="lbl-clean")
  hover_label_opts_small <- leaflet::labelOptions(direction="auto", textsize="11px", sticky=TRUE, opacity=0.95, className="lbl-clean")
  
  # -------- Mapa inicial --------
  output$map_eva <- leaflet::renderLeaflet({
    req(input$f_depto, input$f_metric, input$f_anio)
    
    if (is.null(input$f_depto) || input$f_depto == "Todos") {
      nivel_mapa("depto")
      depto_sel(NULL)
      
      mdat <- depto_sf |>
        dplyr::left_join(agg_depto(), by="DEPARTAMENTO_D") |>
        dplyr::mutate(valor = as.numeric(valor))
      
      pal <- palBin4(mdat$valor, clamp01 = (input$f_metric=="pct"))
      ttl <- if (input$f_metric=="ha") "Hectáreas" else "Porcentaje"
      
      leaflet::leaflet(mdat) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          layerId = ~DEPARTAMENTO_D,
          fillColor = ~pal(valor),
          weight = 0.7, color = "#666", fillOpacity = 0.9,
          label = ~sprintf(
            "%s — %s",
            DEPTO_TC,
            if (input$f_metric=="ha") paste0(format_short(valor)," Ha") else format_pct_es(valor, digits = 2)
          ),
          labelOptions = hover_label_opts,
          highlightOptions = leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
        ) |>
        leaflet::addLegend(
          position="bottomright", pal=pal, values=~valor,
          title=ttl,
          labFormat = if (input$f_metric=="ha")
            labelFormat_es(digits = 0, first_no_symbol = TRUE)
          else
            labelFormat_es(suffix = "%", digits = 1, first_no_symbol = TRUE)
        ) |>
        leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
      
    } else {
      nivel_mapa("mpio")
      depto_sel(input$f_depto)
      
      mdat <- mpios_sf |>
        dplyr::filter(DEPARTAMENTO_D == depto_sel()) |>
        dplyr::left_join(agg_mpio(), by=c("MUNICIPIO_D","DEPARTAMENTO_D"))
      
      pal <- palBin4(mdat$valor, clamp01 = (input$f_metric=="pct"))
      ttl <- if (input$f_metric=="ha") "Hectáreas" else "Porcentaje"
      
      leaflet::leaflet(mdat) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          layerId   = ~MUNICIPIO_D,
          fillColor = ~pal(valor),
          weight    = 0.4, color="#666", fillOpacity=0.9,
          label = ~sprintf(
            "%s (%s) — %s",
            MPIO_TC, DEPTO_TC,
            if (input$f_metric=="ha") paste0(format_short(valor)," Ha") else format_pct_es(valor, digits = 2)
          ),
          labelOptions=hover_label_opts_small,
          highlightOptions=leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
        ) |>
        leaflet::addLegend(
          position="bottomright", pal=pal, values=~valor,
          title=ttl,
          labFormat = if (input$f_metric=="ha")
            labelFormat_es(digits = 0, first_no_symbol = TRUE)
          else
            labelFormat_es(suffix = "%", digits = 1, first_no_symbol = TRUE)
        ) |>
        leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
    }
  })
  
  observe({
    leaflet::leafletProxy("map_eva") |>
      leaflet::removeControl("badge_filtros") |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  })
  
  # -------- Redibujar niveles --------
  dibujar_deptos <- function(){
    mdat <- depto_sf |>
      dplyr::left_join(agg_depto(), by="DEPARTAMENTO_D") |>
      dplyr::mutate(valor = as.numeric(valor))
    
    pal  <- palBin4(mdat$valor, clamp01 = (input$f_metric=="pct"))
    ttl <- if (input$f_metric=="ha") "Área de bosque que queda (Ha)" else "Porcentaje de bosque que queda (%)"
    
    leaflet::leafletProxy("map_eva", data=mdat) |>
      leaflet::clearPopups() |> leaflet::clearShapes() |> leaflet::clearMarkers() |> leaflet::clearControls() |>
      leaflet::addPolygons(
        layerId=~DEPARTAMENTO_D, fillColor=~pal(valor),
        weight=0.7, color="#666", fillOpacity=0.9,
        label=~sprintf(
          "%s — %s",
          DEPTO_TC,
          if (input$f_metric=="ha") paste0(format_short(valor)," Ha") else format_pct_es(valor, digits = 2)
        ),
        labelOptions=hover_label_opts,
        highlightOptions=leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
      ) |>
      leaflet::addLegend(
        position="bottomright", pal=pal, values=~valor,
        title=ttl,
        labFormat = if (input$f_metric=="ha")
          labelFormat_es(digits = 0, first_no_symbol = TRUE)
        else
          labelFormat_es(suffix = "%", digits = 1, first_no_symbol = TRUE)
      ) |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  }
  
  dibujar_mpios <- function(dep){
    mdat <- mpios_sf |>
      dplyr::filter(DEPARTAMENTO_D == dep) |>
      dplyr::left_join(agg_mpio(), by=c("MUNICIPIO_D","DEPARTAMENTO_D")) |>
      dplyr::mutate(valor = as.numeric(valor))
    
    pal  <- palBin4(mdat$valor, clamp01 = (input$f_metric=="pct"))
    
    if (!is.null(input$f_mpio) && input$f_mpio != "Todos") {
      sel_key <- input$f_mpio
      mdat <- mdat |>
        dplyr::mutate(valor_plot = dplyr::if_else(MUNICIPIO_D == sel_key, valor, NA_real_))
    } else {
      mdat <- mdat |>
        dplyr::mutate(valor_plot = valor)
    }
    
    ttl <- if (input$f_metric=="ha") "Hectáreas" else "Porcentaje"
    
    bb  <- sf::st_bbox(mdat)
    lng <- mean(c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"])))
    lat <- mean(c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"])))
    z   <- zoom_from_bbox(bb)
    
    leaflet::leafletProxy("map_eva", data=mdat) |>
      leaflet::clearPopups() |> leaflet::clearShapes() |> leaflet::clearMarkers() |> leaflet::clearControls() |>
      leaflet::setView(lng, lat, zoom = z) |>
      leaflet::addPolygons(
        layerId=~MUNICIPIO_D, fillColor=~pal(valor_plot),
        weight=0.4, color="#666", fillOpacity=0.9,
        label=~sprintf(
          "%s (%s) — %s",
          MPIO_TC, DEPTO_TC,
          ifelse(
            is.na(valor_plot),
            "Sin información",
            if (input$f_metric=="ha") paste0(format_short(valor_plot)," Ha") else format_pct_es(valor_plot, digits = 2)
          )
        ),
        labelOptions=hover_label_opts_small,
        highlightOptions=leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
      ) |>
      leaflet::addLegend(
        position="bottomright", pal=pal, values=mdat$valor,
        title=ttl,
        labFormat = if (input$f_metric=="ha")
          labelFormat_es(digits = 0, first_no_symbol = TRUE)
        else
          labelFormat_es(suffix = "%", digits = 1, first_no_symbol = TRUE)
      ) |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  }
  
  observeEvent(input$f_depto, {
    dep <- input$f_depto
    if (is.null(dep) || dep=="Todos") {
      nivel_mapa("depto"); depto_sel(NULL); dibujar_deptos()
    } else {
      nivel_mapa("mpio"); depto_sel(dep); dibujar_mpios(dep)
    }
  })
  
  observeEvent(input$f_mpio, {
    if (nivel_mapa() == "mpio" && !is.null(depto_sel())) dibujar_mpios(depto_sel())
  }, ignoreInit = TRUE)
  
  observeEvent(input$map_eva_shape_click, {
    click <- input$map_eva_shape_click
    if (is.null(click$id)) return()
    if (nivel_mapa()=="depto") {
      depto_sel(click$id); nivel_mapa("mpio"); dibujar_mpios(click$id)
    }
  })
  
  observeEvent(input$btn_volver, {
    updateSelectInput(session, "f_depto", selected="Todos")
    updateSelectInput(session, "f_mpio",  selected="Todos")
    nivel_mapa("depto"); depto_sel(NULL); dibujar_deptos()
  })
  
  # ========= Serie temporal =========
  series_data <- reactive({
    base <- eva_df
    if (!is.null(input$f_depto) && input$f_depto!="Todos")
      base <- base |> dplyr::filter(DEPTO_KEY == input$f_depto)
    if (!is.null(input$f_mpio)  && input$f_mpio !="Todos")
      base <- base |> dplyr::filter(MPIO_KEY  == input$f_mpio)
    
    if (input$f_metric == "ha") {
      base |>
        dplyr::group_by(anio) |>
        dplyr::summarise(valor_total = sum(cobertura_ha, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(anio)
    } else {
      base |>
        dplyr::group_by(anio) |>
        dplyr::summarise(
          base_sum    = sum(base_ha_2000, na.rm = TRUE),
          cob_sum     = sum(cobertura_ha,  na.rm = TRUE),
          valor_total = dplyr::if_else(base_sum > 0, 100 * cob_sum / base_sum, NA_real_),
          .groups     = "drop"
        ) |>
        dplyr::select(anio, valor_total) |>
        dplyr::arrange(anio)
    }
  })
  
  output$plot_arriba <- plotly::renderPlotly({
    df <- series_data()
    if (!nrow(df)) return(plotly::plot_ly())
    
    if (input$f_metric=="ha") {
      ylab        <- "Hectáreas"
      y_breaks    <- pretty(df$valor_total, n = 5); y_breaks <- y_breaks[y_breaks >= 0]
      y_ticktext  <- format_short(y_breaks)
      hover_tmpl  <- "<b>Año:</b> %{x}<br>Hectáreas %{customdata}<extra></extra>"
      custom_vals <- format_short(df$valor_total)
    } else {
      ylab        <- "Porcentaje"
      custom_vals <- format_pct_es(df$valor_total, digits = 2)
      
      min_val <- max(0, floor(min(df$valor_total, na.rm = TRUE) / 5) * 5)
      max_val <- min(100, ceiling(max(df$valor_total, na.rm = TRUE) / 5) * 5)
      if (!is.finite(min_val) || !is.finite(max_val) || min_val >= max_val) { min_val <- 0; max_val <- 100 }
      y_breaks   <- seq(min_val, max_val, by = 5)
      y_ticktext <- format_pct_es(y_breaks, digits = 0)
      
      hover_tmpl <- "<b>Año:</b> %{x}<br>Porcentaje %{customdata}<extra></extra>"
    }
    
    x_min <- suppressWarnings(min(df$anio, na.rm = TRUE))
    if (!is.finite(x_min)) x_min <- 0
    
    plotly::plot_ly(
      data=df, x=~anio, y=~valor_total,
      type="scatter", mode="lines+markers",
      line=list(width=2, color=SERIE_COLOR),
      marker=list(size=6, color=SERIE_COLOR),
      customdata = custom_vals,
      hovertemplate=hover_tmpl
    ) |>
      plotly::layout(
        font = list(family = "Inter"),
        xaxis=list(title="", tickmode="linear", tick0=x_min, dtick=2, tickangle=0, showgrid = FALSE),
        yaxis=list(title=ylab, tickvals = y_breaks, ticktext = y_ticktext, showgrid = FALSE),
        hovermode="x unified",
        margin=list(l=60, r=20, t=30, b=50)
      )
  })
  
  # ========= Ranking Top-10 =========
  ranking_data <- reactive({
    df <- datos_filtrados()
    if (input$f_metric=="ha") {
      df |>
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D, MUNICIPIO_RAW, DEPARTAMENTO_RAW) |>
        dplyr::summarise(valor_total = sum(cobertura_ha, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(valor_total)) |>
        dplyr::slice_head(n = 10) |>
        dplyr::mutate(muni_tc  = title_case_es(MUNICIPIO_RAW),
                      depto_tc = title_case_es(DEPARTAMENTO_RAW))
    } else {
      df |>
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D, MUNICIPIO_RAW, DEPARTAMENTO_RAW) |>
        dplyr::summarise(
          base_sum    = sum(base_ha_2000, na.rm = TRUE),
          cob_sum     = sum(cobertura_ha,  na.rm = TRUE),
          valor_total = dplyr::if_else(base_sum > 0, 100 * cob_sum / base_sum, NA_real_),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(valor_total)) |>
        dplyr::slice_head(n = 10) |>
        dplyr::mutate(muni_tc  = title_case_es(MUNICIPIO_RAW),
                      depto_tc = title_case_es(DEPARTAMENTO_RAW))
    }
  })
  
  output$ranking_abajo <- plotly::renderPlotly({
    plot_df <- ranking_data()
    if (!nrow(plot_df)) {
      return(plotly::plot_ly() |>
               plotly::layout(annotations = list(text="Sin datos para el ranking", x=0.5, y=0.5, showarrow=FALSE)))
    }
    
    plot_df <- plot_df |>
      dplyr::mutate(muni_label = muni_tc) |>
      dplyr::arrange(valor_total)
    
    if (input$f_metric == "ha") {
      val_fmt <- format_short(plot_df$valor_total)
      max_val <- max(plot_df$valor_total, na.rm = TRUE)
      breaks  <- pretty(c(0, max_val), n = 5); breaks <- breaks[breaks >= 0]
      ticktxt <- format_short(breaks)
      
      xaxis_opts <- list(title="Hectáreas", tickvals=breaks, ticktext=ticktxt, showgrid=FALSE)
      hover_tpl <- paste0(
        "<b>Municipio:</b> %{customdata[0]}<br>",
        "<b>Departamento:</b> %{customdata[1]}<br>",
        "<b>Cobertura neta (Ha):</b> %{customdata[2]}<extra></extra>"
      )
    } else {
      val_fmt <- format_pct_es(plot_df$valor_total, digits = 1)
      breaks <- seq(0, 100, 20)
      xaxis_opts <- list(title="Porcentaje", tickvals=breaks, ticktext=format_pct_es(breaks,0), showgrid=FALSE, range=c(0,100))
      hover_tpl <- paste0(
        "<b>Municipio:</b> %{customdata[0]}<br>",
        "<b>Departamento:</b> %{customdata[1]}<br>",
        "<b>Cobertura neta (%):</b> %{customdata[2]}<extra></extra>"
      )
    }
    
    plotly::plot_ly(
      data  = plot_df,
      x     = ~valor_total,
      y     = ~muni_label,
      type  = "bar",
      orientation = "h",
      marker = list(color = BAR_COLOR),
      text  = val_fmt,
      textposition = "inside",
      insidetextanchor = "middle",
      insidetextfont = list(family="Inter SemiBold, Inter, Arial, sans-serif", size=12, color="white"),
      hovertemplate = hover_tpl,
      customdata = cbind(plot_df$muni_tc, plot_df$depto_tc, val_fmt),
      cliponaxis = FALSE
    ) |>
      plotly::layout(
        font = list(family = "Inter"),
        xaxis = xaxis_opts,
        yaxis = list(title="", categoryorder="array", categoryarray=rev(plot_df$muni_label), showgrid=FALSE),
        margin = list(l = 120, r = 40, t = 20, b = 40)
      )
  })
  
  # ========= Tabla export CSV =========
  tabla_export <- reactive({
    df <- datos_filtrados()
    if (input$f_metric=="ha") {
      df |>
        dplyr::transmute(
          DEPARTAMENTO = title_case_es(DEPARTAMENTO_RAW),
          MUNICIPIO    = title_case_es(MUNICIPIO_RAW),
          anio,
          cobertura_neta_ha = cobertura_ha,
          base_ha_2000
        )
    } else {
      df |>
        dplyr::transmute(
          DEPARTAMENTO = title_case_es(DEPARTAMENTO_RAW),
          MUNICIPIO    = title_case_es(MUNICIPIO_RAW),
          anio,
          cobertura_neta_pct = cobertura_pct,
          cobertura_neta_ha  = cobertura_ha,
          base_ha_2000
        )
    }
  })
  
  output$dl_csv_expl <- downloadHandler(
    filename = function(){
      paste0("HANSEN_cobertura_neta_", safe_chr(input$f_metric), "_", safe_chr(input$f_anio), "_", Sys.Date(), ".csv")
    },
    content  = function(file){
      readr::write_csv(tabla_export(), file, na = "")
    }
  )
  
  # ========= WIDGET MAPA EXPORT (CON LEYENDA) =========
  map_widget_export <- reactive({
    req(input$f_anio, input$f_metric)
    
    if (nivel_mapa() == "depto") {
      
      mdat <- depto_sf |>
        dplyr::left_join(agg_depto(), by="DEPARTAMENTO_D") |>
        dplyr::mutate(valor = as.numeric(valor))
      
      pal <- palBin4(mdat$valor, clamp01 = (input$f_metric=="pct"))
      ttl <- if (input$f_metric=="ha") "Hectáreas" else "Porcentaje"
      
      leaflet::leaflet(
        mdat,
        options = leaflet::leafletOptions(minZoom=11, maxZoom=11, zoomSnap=3)
      ) |>
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.Positron,
          options = leaflet::providerTileOptions(crossOrigin = TRUE)
        ) |>
        htmlwidgets::onRender("function(el,x){ this.zoomControl.setPosition('topright'); }") |>
        leaflet::setView(-74.3, 4.6, 6) |>
        leaflet::addPolygons(fillColor=~pal(valor), weight=0.7, color="#666", fillOpacity=0.9) |>
        leaflet::addLegend(
          position="bottomright",
          pal = pal, values = ~valor,
          title = ttl,
          labFormat = if (input$f_metric=="ha")
            labelFormat_es(digits=0, first_no_symbol=TRUE)
          else
            labelFormat_es(suffix="%", digits=1, first_no_symbol=TRUE)
        )
      
    } else {
      
      req(depto_sel())
      dep <- depto_sel()
      
      mdat <- mpios_sf |>
        dplyr::filter(DEPARTAMENTO_D == dep) |>
        dplyr::left_join(agg_mpio(), by=c("MUNICIPIO_D","DEPARTAMENTO_D")) |>
        dplyr::mutate(valor = as.numeric(valor))
      
      pal <- palBin4(mdat$valor, clamp01 = (input$f_metric=="pct"))
      ttl <- if (input$f_metric=="ha") "Hectáreas" else "Porcentaje"
      
      bb  <- sf::st_bbox(mdat)
      lng <- mean(c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"])))
      lat <- mean(c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"])))
      z   <- zoom_from_bbox(bb)
      
      leaflet::leaflet(
        mdat,
        options = leaflet::leafletOptions(minZoom=11, maxZoom=11, zoomSnap=3)
      ) |>
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.Positron,
          options = leaflet::providerTileOptions(crossOrigin = TRUE)
        ) |>
        htmlwidgets::onRender("function(el,x){ this.zoomControl.setPosition('topright'); }") |>
        leaflet::setView(lng, lat, zoom = z) |>
        leaflet::addPolygons(fillColor=~pal(valor), weight=0.4, color="#666", fillOpacity=0.9) |>
        leaflet::addLegend(
          position="bottomright",
          pal = pal, values = ~valor,
          title = ttl,
          labFormat = if (input$f_metric=="ha")
            labelFormat_es(digits=0, first_no_symbol=TRUE)
          else
            labelFormat_es(suffix="%", digits=1, first_no_symbol=TRUE)
        )
    }
  })
  
  # ========= Descarga PNG Mapa (con leyenda) =========
  output$dl_png_mapa <- downloadHandler(
    filename = function(){
      paste0("HANSEN_mapa_", safe_chr(input$f_metric), "_", safe_chr(input$f_anio), "_", Sys.Date(), ".png")
    },
    content  = function(file){
      ok <- save_widget_png(map_widget_export(), file, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay = 1.6)
      if (!ok) stop("No se pudo generar el PNG del mapa. Revisa webshot2/Chromium.")
    }
  )
  
  # ========= Descarga PNG Serie =========
  output$dl_png_series <- downloadHandler(
    filename = function(){
      paste0("HANSEN_cobertura_serie_", safe_chr(input$f_metric), "_", Sys.Date(), ".png")
    },
    content  = function(file){
      df <- series_data()
      if (!nrow(df)) { file.create(file); return() }
      
      ylab <- if (input$f_metric=="ha") "Cobertura neta (Ha)" else "Cobertura neta (%)"
      x_breaks <- seq(min(df$anio, na.rm = TRUE), max(df$anio, na.rm = TRUE), by = 2)
      
      g <- ggplot(df, aes(x=anio, y=valor_total)) +
        geom_line(linewidth=0.9, color=SERIE_COLOR) +
        geom_point(size=2.2, color=SERIE_COLOR) +
        scale_x_continuous(breaks = x_breaks) +
        labs(x="Año", y=ylab, title=paste0("Evolución anual de ", ylab)) +
        theme_minimal(base_size=12) +
        theme(
          text = element_text(family="Inter"),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()
        )
      
      ggsave(filename=file, plot=g, device=ragg::agg_png, width=10, height=5, dpi=200, units="in", bg = "white")
    }
  )
  
  # ========= Descarga PNG Ranking =========
  output$dl_png_ranking <- downloadHandler(
    filename = function(){
      paste0("HANSEN_cobertura_ranking_", safe_chr(input$f_metric), "_", safe_chr(input$f_anio), "_", Sys.Date(), ".png")
    },
    content  = function(file){
      plot_df <- ranking_data()
      if (!nrow(plot_df)) { file.create(file); return() }
      
      plot_df <- plot_df |>
        dplyr::mutate(muni_label = muni_tc) |>
        dplyr::arrange(dplyr::desc(valor_total))
      
      if (input$f_metric == "ha") {
        max_val <- max(plot_df$valor_total, na.rm = TRUE)
        breaks  <- pretty(c(0, max_val), n = 5)
        breaks  <- breaks[breaks >= 0]
        
        g <- ggplot(plot_df, aes(x = valor_total, y = reorder(muni_label, valor_total))) +
          geom_col(fill = BAR_COLOR) +
          geom_text(aes(x = valor_total / 2, label = format_short(valor_total)), color="white", size=3) +
          scale_x_continuous(labels = format_short, breaks = breaks, expand = expansion(mult = c(0, 0.05))) +
          labs(x="Cobertura neta (Ha)", y=NULL, title=paste0("Top-10 municipios por área de bosque (Ha) — ", safe_chr(input$f_anio)))
      } else {
        breaks <- seq(0, 100, 20)
        
        g <- ggplot(plot_df, aes(x = valor_total, y = reorder(muni_label, valor_total))) +
          geom_col(fill = BAR_COLOR) +
          geom_text(aes(x = valor_total / 2, label = format_pct_es(valor_total, digits = 1)), color="white", size=3) +
          scale_x_continuous(labels = function(x) format_pct_es(x, digits = 0), breaks = breaks, limits = c(0, 100),
                             expand = expansion(mult = c(0, 0.05))) +
          labs(x="Porcentaje", y=NULL, title=paste0("Top-10 municipios por porcentaje de bosque (%) — ", safe_chr(input$f_anio)))
      }
      
      g <- g +
        theme_minimal(base_size=12) +
        theme(text = element_text(family="Inter"),
              axis.text.y=element_text(size=9),
              plot.margin=margin(r=30),
              panel.grid.minor=element_blank(),
              panel.grid.major=element_blank())
      
      ggsave(filename=file, plot=g, device=ragg::agg_png, width=10, height=6, dpi=200, units="in", bg = "white")
    }
  )
  
  # =========================================================
  # PDF (Informe_descargable.Rmd -> PDF)
  #   - Genera PNGs + CSV en ./Descargas (según filtros)
  #   - Renderiza Informe_descargable.Rmd (en raíz o data/)
  # =========================================================
  output$dl_reporte_pdf <- downloadHandler(
    filename = function(){
      met <- if (input$f_metric=="ha") "Ha" else "Pct"
      paste0("Informe_descargable_", met, "_", safe_chr(input$f_anio), "_", Sys.Date(), ".pdf")
    },
    content = function(file){
      
      if (!file.exists(ruta_rmd)) stop("No encuentro Informe_descargable.Rmd en raíz o data/.")
      
      # --- 1) PNG mapa (con leyenda; según nivel + filtros) ---
      ok_map <- save_widget_png(map_widget_export(), IMG_MAP, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay = 1.8)
      if (!ok_map) stop("No se pudo generar Descargas/bosque_mapa.png")
      
      # --- 2) PNG serie (según filtros) ---
      df_s <- series_data()
      if (nrow(df_s)) {
        x_breaks <- seq(min(df_s$anio, na.rm=TRUE), max(df_s$anio, na.rm=TRUE), by=2)
        ylab <- if (input$f_metric=="ha") "Cobertura neta (Ha)" else "Cobertura neta (%)"
        
        g_ser <- ggplot(df_s, aes(x=anio, y=valor_total)) +
          geom_line(linewidth=0.9, color=SERIE_COLOR) +
          geom_point(size=2.2, color=SERIE_COLOR) +
          scale_x_continuous(breaks = x_breaks) +
          labs(x="Año", y=ylab, title="Evolución en el tiempo") +
          theme_minimal(base_size=12) +
          theme(text = element_text(family="Inter"),
                axis.text.x = element_text(angle=0, hjust=0.5),
                panel.grid.minor=element_blank(),
                panel.grid.major=element_blank())
        
        ggsave(IMG_SER, plot=g_ser, device=ragg::agg_png, width=10, height=5, dpi=160, units="in", bg = "white")
      } else {
        ragg::agg_png(IMG_SER, width=1200, height=600, res=150)
        plot.new(); text(0.5, 0.5, "Sin datos para la serie", cex=1.2)
        dev.off()
      }
      
      # --- 3) PNG ranking (según filtros) ---
      df_r <- ranking_data()
      if (nrow(df_r)) {
        df_r <- df_r |> dplyr::mutate(muni_label = muni_tc) |> dplyr::arrange(dplyr::desc(valor_total))
        if (input$f_metric == "ha") {
          g_rnk <- ggplot(df_r, aes(x=valor_total, y=reorder(muni_label, valor_total))) +
            geom_col(fill=BAR_COLOR) +
            geom_text(aes(x = valor_total/2, label = format_short(valor_total)), color="white", size=3) +
            theme_minimal(base_size=12) +
            theme(text = element_text(family="Inter"),
                  panel.grid.minor=element_blank(),
                  panel.grid.major=element_blank()) +
            labs(x="Ha", y=NULL, title="Ranking Top 10")
        } else {
          g_rnk <- ggplot(df_r, aes(x=valor_total, y=reorder(muni_label, valor_total))) +
            geom_col(fill=BAR_COLOR) +
            geom_text(aes(x = valor_total/2, label = format_pct_es(valor_total, 1)), color="white", size=3) +
            scale_x_continuous(limits=c(0,100)) +
            theme_minimal(base_size=12) +
            theme(text = element_text(family="Inter"),
                  panel.grid.minor=element_blank(),
                  panel.grid.major=element_blank()) +
            labs(x="%", y=NULL, title="Ranking Top 10")
        }
        ggsave(IMG_RNK, plot=g_rnk, device=ragg::agg_png, width=10, height=6, dpi=160, units="in", bg = "white")
      } else {
        ragg::agg_png(IMG_RNK, width=1200, height=700, res=150)
        plot.new(); text(0.5, 0.5, "Sin datos para el ranking", cex=1.2)
        dev.off()
      }
      
      # --- 4) CSV filtrado para informe ---
      readr::write_csv(tabla_export(), CSV_FILT, na = "")
      
      # --- 5) Tabla filtros (valores “humanos”) ---
      dep_disp <- "Todos"
      if (!is.null(input$f_depto) && input$f_depto != "Todos") {
        dep_disp <- eva_df |>
          dplyr::filter(DEPTO_KEY == input$f_depto) |>
          dplyr::summarise(v = dplyr::first(DEPTO_TC)) |>
          dplyr::pull(v)
        if (!length(dep_disp) || is.na(dep_disp)) dep_disp <- title_case_es(input$f_depto)
      }
      
      mp_disp <- "Todos"
      if (!is.null(input$f_mpio) && input$f_mpio != "Todos") {
        mp_disp <- eva_df |>
          dplyr::filter(MPIO_KEY == input$f_mpio) |>
          dplyr::summarise(v = dplyr::first(MPIO_TC)) |>
          dplyr::pull(v)
        if (!length(mp_disp) || is.na(mp_disp)) mp_disp <- title_case_es(input$f_mpio)
      }
      
      filtros_tbl <- data.frame(
        Parametro = c("Año", "Indicador", "Departamento", "Municipio"),
        Valor     = c(
          as.character(input$f_anio),
          if (input$f_metric=="ha") "Cobertura neta (Ha)" else "Cobertura neta (%)",
          dep_disp,
          mp_disp
        ),
        stringsAsFactors = FALSE
      )
      
      # --- 6) Render DIRECTO al archivo descargado ---
      rmarkdown::render(
        input         = ruta_rmd,
        output_format = "pdf_document",
        output_file   = basename(file),
        output_dir    = dirname(file),
        quiet         = TRUE,
        params        = list(
          app_root     = app_root,
          export_dir   = "Descargas",
          filtros      = filtros_tbl,
          
          anio         = input$f_anio,
          ind          = if (input$f_metric=="ha") "bosque_ha" else "bosque_pct",
          
          img_map      = basename(IMG_MAP),
          img_serie    = basename(IMG_SER),
          img_ranking  = basename(IMG_RNK),
          
          csv_filtrado = CSV_FILT
        ),
        knit_root_dir = app_root,
        envir         = new.env(parent = globalenv())
      )
    },
    contentType = "application/pdf"
  )
}

# RUN
shinyApp(ui = ui, server = server)

