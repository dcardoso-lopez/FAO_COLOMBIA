# app_eta.R
# =========================================================
# ETA — Dashboard (app exclusiva) - SANTANDER
# Vista única: Exploración ETA
# BOTONES TIPO ICA/BPAN: PNG + CSV + PDF via Rmarkdown
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
options(shiny.maxRequestSize = 100*1024^2)

# ---- Alias seguros ----
validate <- shiny::validate
need     <- shiny::need
`%||%`   <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---------- Export / app root ----------
get_app_root <- function(){
  normalizePath(shiny::getShinyOption("appDir") %||% getwd(), winslash = "/", mustWork = FALSE)
}

app_root   <- get_app_root()
EXPORT_DIR <- file.path(app_root, "Descargas")
dir.create(EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)

ruta_rmd <- file.path(app_root, "Informe_descargable.Rmd")

PNG_VWIDTH    <- 3200
PNG_VHEIGHT   <- 2400
PNG_DELAY_CO  <- 3.0
PNG_DELAY_MUN <- 4.5

IMG_MAP <- file.path(EXPORT_DIR, "eta_mapa.png")
IMG_ORI <- file.path(EXPORT_DIR, "eta_origen.png")
IMG_TOP <- file.path(EXPORT_DIR, "eta_top10.png")

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

zoom_from_bbox <- function(bb){
  w <- abs(as.numeric(bb["xmax"] - bb["xmin"]))
  h <- abs(as.numeric(bb["ymax"] - bb["ymin"]))
  span <- max(w, h)
  if (!is.finite(span)) return(10)
  if (span < 0.10) return(15)
  if (span < 0.20) return(14)
  if (span < 0.35) return(13)
  if (span < 0.80) return(12)
  if (span < 1.50) return(11)
  if (span < 3.00) return(10)
  9
}

# ---------- Rutas ----------
local_data_dir <- "data"
rel_data_dir <- file.path(app_root, "data")
data_dir <- if (dir.exists(rel_data_dir)) rel_data_dir else local_data_dir

eta_path       <- file.path(data_dir, "022_INS_SIVIGILA-ETA.rds")
ruta_pob       <- file.path(data_dir, "051_DANE_Proyecciones_P_total.rds")
ruta_shp_mpios <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")
ruta_shp_dptos <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")

must_exist <- c(eta_path, ruta_pob, ruta_shp_mpios, ruta_shp_dptos)
miss <- must_exist[!file.exists(must_exist)]
if (length(miss)) stop("Faltan archivos. data_dir usado: ", data_dir, "\n", paste("-", miss, collapse = "\n"))

check_shp_parts <- function(shp){
  base <- sub("\\.shp$", "", shp)
  req  <- paste0(base, c(".shp",".dbf",".shx",".prj"))
  req[!file.exists(req)]
}
miss_shp <- c(check_shp_parts(ruta_shp_mpios), check_shp_parts(ruta_shp_dptos))
if (length(miss_shp)) stop("Faltan componentes de shapefile:\n", paste("-", miss_shp, collapse = "\n"))

# ---------- Helper: Title Case español ----------
title_case_es <- function(x){
  palabras_minas <- c("de","del","la","las","los","y","o","u","en","el","por","para","a","e")
  vapply(x, function(txt){
    if (is.na(txt)) return(NA_character_)
    txt <- stringi::stri_trim_both(txt)
    if (txt == "") return("")
    txt <- stringi::stri_trans_tolower(txt, locale = "es")
    palabras <- unlist(strsplit(txt, "\\s+"))
    palabras <- palabras[palabras != ""]
    if (!length(palabras)) return("")
    palabras <- mapply(function(w, i){
      if (i > 1 && w %in% palabras_minas) {
        w
      } else {
        first <- stringi::stri_sub(w, 1, 1)
        rest  <- if (stringi::stri_length(w) > 1) stringi::stri_sub(w, 2) else ""
        paste0(stringi::stri_trans_toupper(first, locale = "es"), rest)
      }
    }, palabras, seq_along(palabras), USE.NAMES = FALSE)
    paste(palabras, collapse = " ")
  }, FUN.VALUE = character(1))
}

# ---------- 1) Leer ETA y normalizar ----------
eta_raw <- readRDS(eta_path) %>% dplyr::filter(!is.na(MUNICIPIO_D))

year_col     <- if ("ano" %in% names(eta_raw)) "ano" else if ("ANO" %in% names(eta_raw)) "ANO" else stop("No encuentro 'ano'/'ANO'")
mun_code_col <- if ("COD_DANE_MUNIC_D" %in% names(eta_raw)) "COD_DANE_MUNIC_D" else stop("No encuentro 'COD_DANE_MUNIC_D'")
dep_name_col <- if ("DEPARTAMENTO_D" %in% names(eta_raw)) "DEPARTAMENTO_D" else if ("DEPARTMENTO_D" %in% names(eta_raw)) "DEPARTMENTO_D" else "DEPARTAMENTO_D"
mun_name_col <- if ("MUNICIPIO_D" %in% names(eta_raw)) "MUNICIPIO_D" else "MUNICIPIO_D"

tenf_col_candidates <- c("TOTAL_ENF","total_enf","TOTALENF","Total_enf","total_enfermos","TOTAL_ENFERMOS")
tenf_col <- { x <- tenf_col_candidates[tenf_col_candidates %in% names(eta_raw)]; if(length(x)) x[1] else NA_character_ }

texp_col_candidates <- c("TOTAL_EXP","total_exp","TOTALEXP","Total_exp","total_expuestos","TOTAL_EXPUES")
texp_col <- { x <- texp_col_candidates[texp_col_candidates %in% names(eta_raw)]; if(length(x)) x[1] else NA_character_ }

origen_cols <- intersect(c("agua","alimentos","pers_pers","cont_ambie","otro","desconocid"), names(eta_raw))
hom_col <- if ("total_hom" %in% names(eta_raw)) "total_hom" else NA_character_
muj_col <- if ("total_muj" %in% names(eta_raw)) "total_muj" else NA_character_

eta <- eta_raw %>%
  dplyr::transmute(
    ano       = suppressWarnings(as.integer(.data[[year_col]])),
    COD_MUN5  = sprintf("%05d", suppressWarnings(as.integer(.data[[mun_code_col]]))),
    COD_DPTO2 = substr(COD_MUN5, 1, 2),
    DEP_N     = title_case_es(trimws(as.character(.data[[dep_name_col]]))),
    MUN_N     = title_case_es(trimws(as.character(.data[[mun_name_col]]))),
    TOTAL_ENF = if (!is.na(tenf_col)) suppressWarnings(as.numeric(.data[[tenf_col]])) else NA_real_,
    TOTAL_EXP = if (!is.na(texp_col)) suppressWarnings(as.numeric(.data[[texp_col]])) else NA_real_,
    TOTAL_HOM = if (!is.na(hom_col))  suppressWarnings(as.numeric(.data[[hom_col]])) else NA_real_,
    TOTAL_MUJ = if (!is.na(muj_col))  suppressWarnings(as.numeric(.data[[muj_col]])) else NA_real_
  ) %>%
  dplyr::filter(COD_DPTO2 == "68") %>%   # <- SANTANDER
  dplyr::mutate(
    TOTAL_NR = pmax(coalesce(TOTAL_ENF, 0) - (coalesce(TOTAL_HOM, 0) + coalesce(TOTAL_MUJ, 0)), 0)
  ) %>%
  dplyr::filter(!is.na(ano), !is.na(COD_MUN5), !is.na(COD_DPTO2))

if (length(origen_cols) > 0) {
  eta <- dplyr::bind_cols(eta, eta_raw %>% dplyr::select(dplyr::all_of(origen_cols)) %>% dplyr::slice(match(eta_raw[[mun_code_col]], eta_raw[[mun_code_col]]))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(origen_cols), ~ ifelse(as.integer(.x) == 1, 1L, 0L)))
}

# ---------- 1b) Población ----------
pob_raw <- readRDS(ruta_pob)
pob_year_col <- dplyr::case_when(
  "ano"  %in% names(pob_raw) ~ "ano",
  "ANO"  %in% names(pob_raw) ~ "ANO",
  "year" %in% names(pob_raw) ~ "year",
  TRUE ~ NA_character_
)
stopifnot(!is.na(pob_year_col))

pob_mun_code_col <- dplyr::case_when(
  "COD_MUN5"          %in% names(pob_raw) ~ "COD_MUN5",
  "COD_DANE_MUNIC_D"  %in% names(pob_raw) ~ "COD_DANE_MUNIC_D",
  "COD_MPIO"          %in% names(pob_raw) ~ "COD_MPIO",
  "MPIO_CDPMP"        %in% names(pob_raw) ~ "MPIO_CDPMP",
  TRUE ~ NA_character_
)
stopifnot(!is.na(pob_mun_code_col))

pob_val_col <- dplyr::case_when(
  "P_TOTAL"   %in% names(pob_raw) ~ "P_TOTAL",
  "Poblacion" %in% names(pob_raw) ~ "Poblacion",
  "POBLACION" %in% names(pob_raw) ~ "POBLACION",
  "total"     %in% names(pob_raw) ~ "total",
  "poblacion" %in% names(pob_raw) ~ "poblacion",
  TRUE ~ NA_character_
)
stopifnot(!is.na(pob_val_col))

pob_norm <- pob_raw %>%
  dplyr::transmute(
    ano       = suppressWarnings(as.integer(.data[[pob_year_col]])),
    COD_MUN5  = sprintf("%05d", suppressWarnings(as.integer(.data[[pob_mun_code_col]]))),
    COD_DPTO2 = substr(COD_MUN5, 1, 2),
    POB       = suppressWarnings(as.numeric(.data[[pob_val_col]]))
  ) %>%
  dplyr::filter(COD_DPTO2 == "68") %>%   # <- SANTANDER
  dplyr::filter(!is.na(ano), !is.na(COD_MUN5), !is.na(COD_DPTO2), is.finite(POB))

pob_mpio  <- pob_norm %>%
  dplyr::group_by(ano, COD_MUN5, COD_DPTO2) %>%
  dplyr::summarise(POB = sum(POB, na.rm = TRUE), .groups = "drop")

pob_depto <- pob_norm %>%
  dplyr::group_by(ano, COD_DPTO2) %>%
  dplyr::summarise(POB = sum(POB, na.rm = TRUE), .groups = "drop")

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
  dplyr::filter(COD_DPTO2 == "68") %>%   # <- SANTANDER
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
  dplyr::filter(COD_DPTO2 == "68") %>%   # <- SANTANDER
  dplyr::mutate(DEPARTAMENTO_N = title_case_es(DEPARTAMENTO_N)) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid()

# ---------- Lookups (ETA) ----------
dpt_lookup_eta <- eta %>%
  dplyr::select(COD_DPTO2, DEP_N) %>%
  dplyr::mutate(
    COD_DPTO2 = sprintf("%02d", as.integer(COD_DPTO2)),
    DEP_N     = title_case_es(trimws(DEP_N))
  ) %>%
  dplyr::distinct() %>%
  dplyr::arrange(DEP_N)

mun_lookup_eta <- eta %>%
  dplyr::select(COD_DPTO2, COD_MUN5, MUN_N) %>%
  dplyr::mutate(
    COD_DPTO2  = sprintf("%02d", as.integer(COD_DPTO2)),
    COD_MUN5   = sprintf("%05d", as.integer(COD_MUN5)),
    MUN_N      = trimws(as.character(MUN_N)),
    MUN_N_TC   = title_case_es(MUN_N)
  ) %>%
  dplyr::distinct()

dept_choices <- c(
  "Todos",
  stats::setNames(dpt_lookup_eta$COD_DPTO2, dpt_lookup_eta$DEP_N)
)

SANTANDER_CODE <- {
  idx <- which(toupper(dpt_lookup_eta$DEP_N) %in% c("SANTANDER"))
  if (length(idx) > 0) {
    dpt_lookup_eta$COD_DPTO2[idx[1]]
  } else {
    "68"
  }
}

# ---------- Paletas/colores ----------
MAP_COLORS <- c("#fff4e6","#ffd8a8","#ffa94d","#f76707","#d9480f")
BAR_COLOR  <- "#f76707"
BORDER_COL <- "#f57c00"

# ---------- Helpers de cuartiles ----------
compute_breaks_quartiles <- function(values){
  vals <- suppressWarnings(as.numeric(values))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(c(0, 1))
  if (all(vals == 0, na.rm = TRUE)) return(c(0, 1))
  
  pos <- vals[vals > 0]
  if (!length(pos)) return(c(0, 1))
  
  qs <- stats::quantile(pos, probs = c(0.25, 0.50, 0.75, 1), na.rm = TRUE)
  c(0, as.numeric(qs))
}
format_interval_label <- function(a, b, is_first = TRUE){
  fa <- scales::number(a, accuracy = 1, big.mark = ".", decimal.mark = ",")
  fb <- scales::number(b, accuracy = 1, big.mark = ".", decimal.mark = ",")
  if (is_first) sprintf("%s – %s", fa, fb) else sprintf(">%s – %s", fa, fb)
}
build_interval_labels <- function(breaks){
  if (length(breaks) < 2) return(character(0))
  vapply(
    seq_len(length(breaks) - 1),
    function(i) format_interval_label(breaks[i], breaks[i + 1], is_first = (i == 1)),
    character(1)
  )
}

# =========================================================
# 3) UI — Vista única
# =========================================================
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    primary = "#2563eb",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius" = "0.9rem",
    "font-size-base" = "0.98rem"
  ),
  tags$head(tags$style(HTML(sprintf("
    :root{ --border-col:%s; --accent:%s; }
    .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
    h3{font-weight:700;letter-spacing:.2px;margin-bottom:16px}
    .filters{
      background:#fff;border:1.5px solid var(--border-col);
      border-radius:16px;padding:14px 16px;margin-bottom:16px;
      box-shadow:0 2px 10px rgba(0,0,0,.04);
    }
    .filters-grid{display:grid;grid-template-columns:repeat(4,minmax(220px,1fr));gap:12px}
    .filter-label{
      font-size:14px;font-weight:500;letter-spacing:.2px;color:#111827;margin-bottom:6px;
      font-family:'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
    }
    .selectize-input,.form-control{
      min-height:42px;border-radius:10px;border:1.5px solid var(--border-col);
      box-shadow:none !important;
    }
    .selectize-input:focus,.form-control:focus{
      border-color:var(--border-col) !important; outline:0 !important;
      box-shadow:0 0 0 .15rem rgba(245,124,0,.35) !important;
    }
    .card{
      background:#fff;border:1.5px solid var(--border-col);
      border-radius:16px;padding:12px;box-shadow:0 2px 10px rgba(0,0,0,.05);margin-bottom:12px
    }
    .card-title{
      font-weight:700;font-size:16px;margin-bottom:8px;color:#111827;
      display:flex;align-items:center;justify-content:space-between;gap:8px;
    }
    .map-note{margin-top:6px;font-size:12px;color:#6b7280;}
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
  ", BORDER_COL, BAR_COLOR)))),
  div(class="wrap",
      h3(""),
      br(),
      div(class="filters",
          div(class="filters-grid",
              div(class="filter",
                  div(class="filter-label","¿Qué año analizamos?"),
                  uiOutput("anio_e1_ui")
              ),
              div(class="filter",
                  div(class="filter-label","¿En qué departamento?"),
                  selectInput(
                    "f_depto_e1", NULL,
                    choices  = dept_choices,
                    selected = SANTANDER_CODE
                  )
              ),
              div(class="filter",
                  div(class="filter-label","¿Algún municipio en particular?"),
                  selectInput("f_mpio_e1", NULL, choices = "Todos", selected = "Todos")
              ),
              div(class="filter",
                  div(class="filter-label","Variable a considerar"),
                  selectInput(
                    "f_indic_e1", NULL,
                    choices = c("Total de enfermos" = "total_enf",
                                "Incidencia (x100k)" = "incid"),
                    selected = "total_enf"
                  )
              )
          )
      ),
      fluidRow(
        column(6,
               div(class="card",
                   div(class="card-title",
                       span(textOutput("ttl_mapa_e1", inline = TRUE)),
                       span(
                         downloadButton("dl_png_mapa_eta","Descargar PNG", class="btn-unified")
                       )
                   ),
                   div(style="display:flex; gap:10px; align-items:center; margin-bottom:8px;",
                       actionButton("btn_reset_e1","← Volver a Santander", class="btn btn-light")
                   ),
                   leafletOutput("map_eta_e1", height = 660),
                   div(class = "map-note", textOutput("nota_mapa_e1"))
               )
        ),
        column(6,
               fluidRow(
                 column(12,
                        div(class="card",
                            div(class="card-title",
                                span(textOutput("ttl_origen_e1", inline = TRUE)),
                                span(downloadButton("dl_png_origen_eta","Descargar PNG", class="btn-unified"))
                            ),
                            plotlyOutput("plot_origen_e1", height = 315)
                        )
                 )
               ),
               fluidRow(
                 column(12,
                        div(class="card",
                            div(class="card-title",
                                span(textOutput("ttl_top_e1", inline = TRUE)),
                                span(downloadButton("dl_png_top_eta","Descargar PNG", class="btn-unified"))
                            ),
                            plotlyOutput("top_mpios_e1", height = 318)
                        )
                 )
               )
        )
      ),
      div(
        class = "footer-actions",
        downloadButton("dl_csv_expl_eta","Descargar CSV", class="btn-unified"),
        downloadButton("dl_reporte_pdf_eta","Descargar informe (PDF)", class="btn-unified")
      )
  )
)

# =========================================================
# 4) SERVER
# =========================================================
server <- function(input, output, session){
  
  indic_lbl_e1 <- reactive({
    if (identical(input$f_indic_e1, "incid")) "Incidencia (x100k)" else "Enfermos"
  })
  
  output$anio_e1_ui <- renderUI({
    yrs <- sort(unique(eta$ano))
    selectInput("f_anio_e1", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  scope_txt <- reactive({
    dep_code <- input$f_depto_e1 %||% "Todos"
    mun_code <- input$f_mpio_e1  %||% "Todos"
    
    if (dep_code == "Todos") return("Santander")
    
    dep_nom <- dpt_lookup_eta$DEP_N[dpt_lookup_eta$COD_DPTO2 == dep_code][1] %||% dep_code
    
    if (!is.null(mun_code) && mun_code != "Todos") {
      mun_nom <- mun_lookup_eta$MUN_N_TC[mun_lookup_eta$COD_MUN5 == mun_code][1] %||% mun_code
      return(paste(mun_nom, ",", dep_nom))
    }
    dep_nom
  })
  
  observeEvent(list(input$f_anio_e1, input$f_depto_e1), {
    if (is.null(input$f_anio_e1) || length(input$f_anio_e1) == 0) return()
    
    dep <- input$f_depto_e1 %||% "Todos"
    
    if (is.null(dep) || dep == "Todos") {
      updateSelectInput(session, "f_mpio_e1", choices = "Todos", selected = "Todos")
      return()
    }
    
    mm <- eta %>%
      dplyr::filter(ano == input$f_anio_e1, COD_DPTO2 == dep) %>%
      dplyr::distinct(COD_MUN5, MUN_N) %>%
      dplyr::mutate(MUN_N_TC = title_case_es(MUN_N)) %>%
      dplyr::arrange(MUN_N_TC)
    
    if (nrow(mm) == 0) {
      mm <- mun_lookup_eta %>%
        dplyr::filter(COD_DPTO2 == dep) %>%
        dplyr::arrange(MUN_N_TC) %>%
        dplyr::transmute(COD_MUN5, MUN_N_TC)
    }
    
    updateSelectInput(
      session, "f_mpio_e1",
      choices  = c("Todos", stats::setNames(mm$COD_MUN5, mm$MUN_N_TC)),
      selected = "Todos"
    )
  }, ignoreInit = FALSE)
  
  nivel_e1 <- reactive({
    if (is.null(input$f_depto_e1) || input$f_depto_e1 == "Todos") "deptos" else "mpios"
  })
  
  base_e1 <- reactive({
    req(input$f_anio_e1)
    df <- eta %>% dplyr::filter(ano == input$f_anio_e1)
    
    if (!is.null(input$f_depto_e1) && input$f_depto_e1 != "Todos") {
      df <- df %>% dplyr::filter(COD_DPTO2 == input$f_depto_e1)
    }
    if (!is.null(input$f_mpio_e1) && input$f_mpio_e1 != "Todos") {
      df <- df %>% dplyr::filter(COD_MUN5 == input$f_mpio_e1)
    }
    df
  })
  
  agg_depto_e1 <- reactive({
    df <- base_e1()
    if (identical(input$f_indic_e1, "incid")) {
      sum_enf <- df %>%
        dplyr::group_by(COD_DPTO2) %>%
        dplyr::summarise(total_enf = sum(TOTAL_ENF, na.rm=TRUE), .groups="drop")
      sum_enf %>%
        dplyr::left_join(pob_depto %>% dplyr::filter(ano == input$f_anio_e1), by="COD_DPTO2") %>%
        dplyr::mutate(valor = ifelse(POB > 0, (total_enf/POB)*1e5, NA_real_)) %>%
        dplyr::select(COD_DPTO2, valor)
    } else {
      df %>%
        dplyr::group_by(COD_DPTO2) %>%
        dplyr::summarise(valor = sum(TOTAL_ENF, na.rm=TRUE), .groups="drop")
    }
  })
  
  agg_mpio_e1 <- reactive({
    df <- base_e1()
    if (identical(input$f_indic_e1, "incid")) {
      sum_enf <- df %>%
        dplyr::group_by(COD_DPTO2, COD_MUN5, MUN_N) %>%
        dplyr::summarise(total_enf = sum(TOTAL_ENF, na.rm=TRUE), .groups="drop")
      sum_enf %>%
        dplyr::left_join(
          pob_mpio %>% dplyr::filter(ano == input$f_anio_e1) %>% dplyr::select(COD_MUN5, POB),
          by="COD_MUN5"
        ) %>%
        dplyr::mutate(valor = ifelse(POB > 0, (total_enf/POB)*1e5, NA_real_)) %>%
        dplyr::select(COD_DPTO2, COD_MUN5, MUN_N, valor)
    } else {
      df %>%
        dplyr::group_by(COD_DPTO2, COD_MUN5, MUN_N) %>%
        dplyr::summarise(valor = sum(TOTAL_ENF, na.rm=TRUE), .groups="drop")
    }
  })
  
  output$ttl_mapa_e1 <- renderText({
    amb <- scope_txt()
    ind <- tolower(indic_lbl_e1())
    paste0("¿En qué territorios de ", amb, " está la mayor cantidad de ", ind, "?")
  })
  
  output$ttl_origen_e1 <- renderText({
    "¿Cuál fue la principal fuente de origen de Enfermedades Transmitidas por Alimentos (ETA)?"
  })
  
  output$ttl_top_e1 <- renderText({
    paste0("¿Qué municipios tienen mayor cantidad de ", tolower(indic_lbl_e1()), "?")
  })
  
  build_origen_plotly_eta <- function(){
    validate(need(length(origen_cols) > 0, "No hay variables de origen disponibles."))
    
    d <- base_e1() %>%
      dplyr::select(dplyr::all_of(origen_cols), TOTAL_ENF) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(origen_cols), names_to = "Origen", values_to = "flag") %>%
      dplyr::filter(flag == 1) %>%
      dplyr::group_by(Origen) %>%
      dplyr::summarise(Casos = dplyr::n(), Enfermos = sum(TOTAL_ENF, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(
        Origen = dplyr::recode(
          Origen,
          "agua"       = "Agua",
          "alimentos"  = "Alimentos",
          "pers_pers"  = "Persona a persona",
          "cont_ambie" = "Contaminación ambiental",
          "otro"       = "Otro",
          "desconocid" = "Desconocido",
          .default     = Origen
        ),
        Casos_fmt    = scales::number(Casos,    big.mark=".", decimal.mark=",", accuracy = 1),
        Enfermos_fmt = scales::number(Enfermos, big.mark=".", decimal.mark=",", accuracy = 1),
        hover_lab    = paste0(Origen, "<br>Enfermos: ", Enfermos_fmt, "<br>Casos: ", Casos_fmt)
      )
    
    plot_ly(
      d, x = ~Enfermos, y = ~reorder(Origen, Enfermos),
      type = "bar", orientation = "h",
      marker = list(color = BAR_COLOR),
      text = ~Enfermos_fmt, textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(color = "white"),
      hovertext = ~hover_lab, hoverinfo = "text"
    ) %>%
      layout(
        xaxis  = list(title = "Número de enfermos"),
        yaxis  = list(title = ""),
        margin = list(l=10,r=40,b=40,t=10),
        paper_bgcolor="#ffffff",
        plot_bgcolor ="#ffffff"
      )
  }
  
  output$plot_origen_e1 <- renderPlotly({
    if (!length(origen_cols)) return(NULL)
    build_origen_plotly_eta()
  })
  
  output$map_eta_e1 <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::setView(lng = -73.12, lat = 7.13, zoom = 8)
  })
  
  output$nota_mapa_e1 <- renderText({
    df_vals <- if (nivel_e1() == "deptos") agg_depto_e1() else agg_mpio_e1()
    if (is.null(df_vals) || nrow(df_vals) == 0) {
      "Nota: Sin información suficiente para segmentar el indicador en cuartiles."
    } else {
      "Nota: El mapa clasifica los valores del indicador en cuartiles (cuatro grupos con igual número de observaciones)."
    }
  })
  
  bbox_actual_eta <- reactive({
    if (nivel_e1() == "deptos") {
      if (!is.null(input$f_depto_e1) && input$f_depto_e1 != "Todos") {
        g <- dptos_sf %>% dplyr::filter(COD_DPTO2 == input$f_depto_e1)
        if (nrow(g) > 0) return(sf::st_bbox(g))
      }
      return(sf::st_bbox(dptos_sf))
    }
    
    sel_dep <- input$f_depto_e1
    req(!is.na(sel_dep), nzchar(sel_dep))
    
    if (!is.null(input$f_mpio_e1) && input$f_mpio_e1 != "Todos") {
      g <- mpios_sf %>% dplyr::filter(COD_MUN5 == input$f_mpio_e1)
      if (nrow(g) > 0) return(sf::st_bbox(g))
    }
    
    shp <- mpios_sf %>% dplyr::filter(COD_DPTO2 == sel_dep)
    sf::st_bbox(shp)
  })
  
  observe({
    req(input$f_anio_e1)
    
    titulo <- indic_lbl_e1()
    fmt_val <- function(x){
      if (identical(input$f_indic_e1, "incid")) {
        scales::number(x, big.mark=".", decimal.mark=",", accuracy=0.1)
      } else {
        scales::number(x, big.mark=".", decimal.mark=",", accuracy=1)
      }
    }
    
    if (nivel_e1() == "deptos") {
      shp <- dptos_sf %>%
        dplyr::left_join(agg_depto_e1(), by="COD_DPTO2") %>%
        dplyr::left_join(dpt_lookup_eta, by="COD_DPTO2") %>%
        dplyr::mutate(
          valor = tidyr::replace_na(valor, 0),
          DEP_N = dplyr::coalesce(DEP_N, DEPARTAMENTO_N, COD_DPTO2),
          etq   = paste0("<b>", DEP_N, "</b><br>", titulo, ": ", fmt_val(valor))
        )
      
      vals <- shp$valor
      brks <- compute_breaks_quartiles(vals)
      pal  <- leaflet::colorBin(MAP_COLORS, domain = vals, bins = brks, na.color = "#f0f0f0")
      labels_legend <- build_interval_labels(brks)
      mids          <- (brks[-length(brks)] + brks[-1]) / 2
      cols_legend   <- pal(mids)
      
      leaflet::leafletProxy("map_eta_e1", data=shp) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::addPolygons(
          layerId  = ~COD_DPTO2,
          fillColor= ~pal(valor),
          color    = BORDER_COL, weight = 0.7, fillOpacity = 0.9,
          label    = ~lapply(etq, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(color=BORDER_COL, weight=2, bringToFront=TRUE)
        ) %>%
        leaflet::addLegend(
          "bottomright",
          colors = cols_legend,
          labels = labels_legend,
          opacity = 0.9,
          title   = titulo
        )
    } else {
      sel_dep <- input$f_depto_e1
      req(!is.na(sel_dep), nzchar(sel_dep))
      
      shp <- mpios_sf %>%
        dplyr::filter(COD_DPTO2 == sel_dep) %>%
        dplyr::left_join(agg_mpio_e1() %>% dplyr::select(COD_MUN5, valor), by="COD_MUN5") %>%
        dplyr::left_join(mun_lookup_eta %>% dplyr::select(COD_MUN5, MUN_N_TC), by="COD_MUN5") %>%
        dplyr::mutate(
          valor = tidyr::replace_na(valor, 0),
          MUN_N_TC = dplyr::coalesce(MUN_N_TC, MUNICIPIO_N, COD_MUN5),
          etq   = paste0("<b>", MUN_N_TC, "</b><br>", titulo, ": ", fmt_val(valor))
        )
      
      vals <- shp$valor
      brks <- compute_breaks_quartiles(vals)
      pal  <- leaflet::colorBin(MAP_COLORS, domain = vals, bins = brks, na.color = "#f0f0f0")
      labels_legend <- build_interval_labels(brks)
      mids          <- (brks[-length(brks)] + brks[-1]) / 2
      cols_legend   <- pal(mids)
      bb  <- sf::st_bbox(shp)
      
      leaflet::leafletProxy("map_eta_e1", data=shp) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::addPolygons(
          layerId  = ~COD_MUN5,
          fillColor= ~pal(valor),
          color    = BORDER_COL, weight = 0.4, fillOpacity = 0.9,
          label    = ~lapply(etq, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(color=BORDER_COL, weight=2, bringToFront=TRUE)
        ) %>%
        leaflet::addLegend(
          "bottomright",
          colors = cols_legend,
          labels = labels_legend,
          opacity = 0.9,
          title   = titulo
        ) %>%
        leaflet::fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  
  observeEvent(input$map_eta_e1_shape_click, {
    click <- input$map_eta_e1_shape_click
    req(click$id)
    if (nivel_e1() == "deptos") {
      cod <- sprintf("%02d", as.integer(click$id))
      updateSelectInput(session, "f_depto_e1", selected = cod)
      updateSelectInput(session, "f_mpio_e1",  selected = "Todos")
    } else {
      cod_mun <- sprintf("%05d", as.integer(click$id))
      updateSelectInput(session, "f_mpio_e1", selected = cod_mun)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_reset_e1, {
    updateSelectInput(session, "f_depto_e1", selected = SANTANDER_CODE)
    updateSelectInput(session, "f_mpio_e1",  selected = "Todos")
  })
  
  build_top_plotly_eta <- function(){
    req(input$f_anio_e1)
    
    titulo <- indic_lbl_e1()
    acc    <- if (identical(input$f_indic_e1, "incid")) 0.1 else 1
    
    df <- agg_mpio_e1() %>%
      dplyr::mutate(MUN_N = title_case_es(MUN_N)) %>%
      dplyr::arrange(dplyr::desc(valor)) %>%
      dplyr::slice(1:12) %>%
      dplyr::mutate(
        valor_fmt = scales::number(valor, big.mark=".", decimal.mark=",", accuracy = acc),
        hover_lab = paste0(MUN_N, "<br>", titulo, ": ", valor_fmt)
      )
    
    axis_title <- if (identical(input$f_indic_e1, "incid")) {
      "Incidencia (casos por 100.000 habitantes)"
    } else {
      "Número de enfermos"
    }
    
    plot_ly(
      df, x = ~valor, y = ~reorder(MUN_N, valor),
      type = "bar", orientation = "h",
      marker = list(color = BAR_COLOR),
      text = ~valor_fmt,
      textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(color = "white"),
      hovertext = ~hover_lab,
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis  = list(title = axis_title),
        yaxis  = list(title = ""),
        margin = list(l=10,r=40,b=40,t=10),
        paper_bgcolor="#ffffff",
        plot_bgcolor ="#ffffff"
      )
  }
  
  output$top_mpios_e1 <- renderPlotly({
    build_top_plotly_eta()
  })
  
  map_widget_export_eta <- reactive({
    req(input$f_anio_e1)
    
    titulo <- indic_lbl_e1()
    fmt_val <- function(x){
      if (identical(input$f_indic_e1, "incid")) {
        scales::number(x, big.mark=".", decimal.mark=",", accuracy=0.1)
      } else {
        scales::number(x, big.mark=".", decimal.mark=",", accuracy=1)
      }
    }
    
    bb  <- bbox_actual_eta()
    lng <- mean(c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"])))
    lat <- mean(c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"])))
    z   <- zoom_from_bbox(bb)
    
    m <- leaflet::leaflet(
      options = leaflet::leafletOptions(
        zoomControl = TRUE,
        zoomSnap = 0.25
      )
    ) %>%
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.Positron,
        options = leaflet::providerTileOptions(crossOrigin = TRUE)
      )
    
    if (nivel_e1() == "deptos") {
      shp <- dptos_sf %>%
        dplyr::left_join(agg_depto_e1(), by="COD_DPTO2") %>%
        dplyr::left_join(dpt_lookup_eta, by="COD_DPTO2") %>%
        dplyr::mutate(
          valor = tidyr::replace_na(valor, 0),
          DEP_N = dplyr::coalesce(DEP_N, DEPARTAMENTO_N, COD_DPTO2),
          etq   = paste0("<b>", DEP_N, "</b><br>", titulo, ": ", fmt_val(valor))
        )
      
      vals <- shp$valor
      brks <- compute_breaks_quartiles(vals)
      pal  <- leaflet::colorBin(MAP_COLORS, domain = vals, bins = brks, na.color = "#f0f0f0")
      labels_legend <- build_interval_labels(brks)
      mids          <- (brks[-length(brks)] + brks[-1]) / 2
      cols_legend   <- pal(mids)
      
      m <- m %>%
        leaflet::addPolygons(
          data = shp,
          layerId  = ~COD_DPTO2,
          fillColor= ~pal(valor),
          color    = BORDER_COL, weight = 0.7, fillOpacity = 0.9
        ) %>%
        leaflet::addLegend(
          "bottomright",
          colors = cols_legend,
          labels = labels_legend,
          opacity = 0.9,
          title   = titulo
        )
    } else {
      sel_dep <- input$f_depto_e1
      req(!is.na(sel_dep), nzchar(sel_dep))
      
      shp <- mpios_sf %>%
        dplyr::filter(COD_DPTO2 == sel_dep) %>%
        dplyr::left_join(agg_mpio_e1() %>% dplyr::select(COD_MUN5, valor), by="COD_MUN5") %>%
        dplyr::left_join(mun_lookup_eta %>% dplyr::select(COD_MUN5, MUN_N_TC), by="COD_MUN5") %>%
        dplyr::mutate(
          valor = tidyr::replace_na(valor, 0),
          MUN_N_TC = dplyr::coalesce(MUN_N_TC, MUNICIPIO_N, COD_MUN5),
          etq   = paste0("<b>", MUN_N_TC, "</b><br>", titulo, ": ", fmt_val(valor))
        )
      
      vals <- shp$valor
      brks <- compute_breaks_quartiles(vals)
      pal  <- leaflet::colorBin(MAP_COLORS, domain = vals, bins = brks, na.color = "#f0f0f0")
      labels_legend <- build_interval_labels(brks)
      mids          <- (brks[-length(brks)] + brks[-1]) / 2
      cols_legend   <- pal(mids)
      
      m <- m %>%
        leaflet::addPolygons(
          data = shp,
          layerId  = ~COD_MUN5,
          fillColor= ~pal(valor),
          color    = BORDER_COL, weight = 0.4, fillOpacity = 0.9
        ) %>%
        leaflet::addLegend(
          "bottomright",
          colors = cols_legend,
          labels = labels_legend,
          opacity = 0.9,
          title   = titulo
        )
    }
    
    m %>%
      leaflet::setView(lng = lng, lat = lat, zoom = z) %>%
      htmlwidgets::onRender("
        function(el, x) {
          this.zoomControl.setPosition('topright');
        }
      ")
  })
  
  tabla_export_eta <- reactive({
    req(input$f_anio_e1)
    out <- base_e1() %>%
      dplyr::transmute(
        anio         = ano,
        departamento = DEP_N,
        municipio    = MUN_N,
        cod_dpto     = COD_DPTO2,
        cod_mpio     = COD_MUN5,
        total_enf    = TOTAL_ENF,
        total_exp    = TOTAL_EXP,
        total_hom    = TOTAL_HOM,
        total_muj    = TOTAL_MUJ,
        total_nr     = TOTAL_NR
      )
    
    if (length(origen_cols) > 0) {
      extra <- base_e1() %>% dplyr::select(dplyr::all_of(origen_cols))
      out <- dplyr::bind_cols(out, extra)
    }
    out
  })
  
  output$dl_png_mapa_eta <- downloadHandler(
    filename = function(){
      dep_tag <- if (is.null(input$f_depto_e1) || input$f_depto_e1 == "Todos") "Santander" else "Santander"
      mun_tag <- if (is.null(input$f_mpio_e1) || input$f_mpio_e1 == "Todos") "Todos" else input$f_mpio_e1
      paste0("ETA_mapa_", dep_tag, "_", mun_tag, "_", input$f_anio_e1 %||% "NA", "_", Sys.Date(), ".png")
    },
    content = function(file){
      dly <- if (!is.null(input$f_mpio_e1) && input$f_mpio_e1 != "Todos") PNG_DELAY_MUN else PNG_DELAY_CO
      ok <- save_widget_png_retry(map_widget_export_eta(), file, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay_base = dly)
      if (!ok) stop("No se pudo generar el PNG del mapa.")
    }
  )
  
  output$dl_png_origen_eta <- downloadHandler(
    filename = function(){
      paste0("ETA_origen_Santander_", Sys.Date(), ".png")
    },
    content = function(file){
      ok <- save_widget_png_retry(build_origen_plotly_eta(), file, vwidth = 1800, vheight = 900, delay_base = 0.9)
      if (!ok) stop("No se pudo generar el PNG del gráfico de origen.")
    }
  )
  
  output$dl_png_top_eta <- downloadHandler(
    filename = function(){
      paste0("ETA_top_municipios_Santander_", input$f_anio_e1 %||% "NA", "_", Sys.Date(), ".png")
    },
    content = function(file){
      ok <- save_widget_png_retry(build_top_plotly_eta(), file, vwidth = 1800, vheight = 1000, delay_base = 0.9)
      if (!ok) stop("No se pudo generar el PNG del Top municipios.")
    }
  )
  
  output$dl_csv_expl_eta <- downloadHandler(
    filename = function(){
      dep_tag <- if (is.null(input$f_depto_e1) || input$f_depto_e1 == "Todos") "Santander" else "Santander"
      mun_tag <- if (is.null(input$f_mpio_e1) || input$f_mpio_e1 == "Todos") "Todos" else input$f_mpio_e1
      paste0("ETA_base_filtrada_", dep_tag, "_", mun_tag, "_", input$f_anio_e1 %||% "NA", "_", Sys.Date(), ".csv")
    },
    content = function(file){
      readr::write_csv(tabla_export_eta(), file, na = "")
    }
  )
  
  output$dl_reporte_pdf_eta <- downloadHandler(
    filename = function(){
      dep_tag <- if (is.null(input$f_depto_e1) || input$f_depto_e1 == "Todos") "Santander" else "Santander"
      mun_tag <- if (is.null(input$f_mpio_e1) || input$f_mpio_e1 == "Todos") "Todos" else input$f_mpio_e1
      paste0("Informe_descargable_ETA_", dep_tag, "_", mun_tag, "_", input$f_anio_e1 %||% "NA", "_", Sys.Date(), ".pdf")
    },
    content = function(file){
      
      if (!file.exists(ruta_rmd)) stop("No encuentro Informe_descargable.Rmd en la raíz del proyecto.")
      
      anio_now <- input$f_anio_e1
      dep_now_code <- input$f_depto_e1 %||% SANTANDER_CODE
      mun_now_code <- input$f_mpio_e1 %||% "Todos"
      
      dep_now <- if (is.null(dep_now_code) || dep_now_code == "Todos") {
        "Santander"
      } else {
        dpt_lookup_eta$DEP_N[dpt_lookup_eta$COD_DPTO2 == dep_now_code][1] %||% dep_now_code
      }
      
      mun_now <- if (is.null(mun_now_code) || mun_now_code == "Todos") {
        "Todos"
      } else {
        mun_lookup_eta$MUN_N_TC[mun_lookup_eta$COD_MUN5 == mun_now_code][1] %||% mun_now_code
      }
      
      dly_map <- if (!is.null(mun_now_code) && mun_now_code != "Todos") PNG_DELAY_MUN else PNG_DELAY_CO
      
      ok_map <- save_widget_png_retry(map_widget_export_eta(), IMG_MAP, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay_base = dly_map)
      ok_ori <- save_widget_png_retry(build_origen_plotly_eta(), IMG_ORI, vwidth = 1800, vheight = 900, delay_base = 0.9)
      ok_top <- save_widget_png_retry(build_top_plotly_eta(), IMG_TOP, vwidth = 1800, vheight = 1000, delay_base = 0.9)
      
      if (!ok_map) stop("No se pudo generar Descargas/eta_mapa.png para el informe.")
      if (!ok_ori) stop("No se pudo generar Descargas/eta_origen.png para el informe.")
      if (!ok_top) stop("No se pudo generar Descargas/eta_top10.png para el informe.")
      
      filtros_tbl <- data.frame(
        Parametro = c("Año", "Departamento", "Municipio", "Indicador"),
        Valor     = c(as.character(anio_now), dep_now, mun_now, indic_lbl_e1()),
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
      
      td <- tempfile("rmd_eta_")
      dir.create(td, recursive = TRUE, showWarnings = FALSE)
      
      rmd_to_render <- ruta_rmd
      rmd_lines <- readLines(ruta_rmd, warn = FALSE, encoding = "UTF-8")
      if (any(grepl("__LOGO_PLATEA_PATH__", rmd_lines, fixed = TRUE))) {
        rmd_tmp <- file.path(td, "Informe_descargable_ETA_render.Rmd")
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
          especie      = "ETA",
          departamento = dep_now,
          municipio    = mun_now,
          ind          = paste0("eta_", input$f_indic_e1 %||% "total_enf"),
          img_map      = basename(IMG_MAP),
          img_serie    = basename(IMG_ORI),
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

