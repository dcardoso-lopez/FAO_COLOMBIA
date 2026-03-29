# app.R
# =========================================================
# SIPSA ABASTECIMIENTO — MAPA + (BARRAS GRUPOS) + (SERIE TOTAL) + TABLA (2 pestañas)
# MODIFICACIONES:
# - Botones PNG para TODOS los objetos visuales de ambas pestañas
# - PDF SOLO en Tab 1
# - PDF SOLO se habilita cuando ya se visualizaron las 2 pestañas
# - El PDF incluye imágenes de ambas pestañas
# =========================================================

# ------------------------------
# 1) Paquetes (NO instalar aquí)
# ------------------------------
pkgs <- c(
  "shiny","bslib","shinyWidgets",
  "dplyr","stringr","janitor","scales",
  "readr","DT","plotly",
  "sf","leaflet","stringi","htmltools",
  "webshot2","htmlwidgets","ragg","rmarkdown"
)

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    "Faltan paquetes requeridos (NO los instalo automáticamente):\n- ",
    paste(missing, collapse = "\n- "),
    "\n\nInstálalos manualmente y vuelve a ejecutar."
  )
}

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(dplyr); library(stringr); library(janitor); library(scales)
  library(readr)
  library(DT)
  library(plotly)
  library(sf); library(leaflet)
  library(stringi)
  library(htmltools)
  library(webshot2); library(htmlwidgets)
  library(ragg)
  library(rmarkdown)
})

options(stringsAsFactors = FALSE, scipen = 999)
sf::sf_use_s2(FALSE)

validate <- shiny::validate
need     <- shiny::need
`%||%`   <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# =========================================================
# FOCO
# =========================================================
DPTO_FOCO_COD    <- "08"
DPTO_FOCO_NOMBRE <- "Atlántico"
APP_TITLE <- paste0("")

# =========================================================
# 0) Rutas robustas
# =========================================================
app_root <- tryCatch({
  of <- sys.frame(1)$ofile
  if (!is.null(of)) dirname(normalizePath(of, winslash = "/", mustWork = TRUE))
  else normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}, error = function(e){
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
})

data_dir <- file.path(app_root, "data")
rds_path <- file.path(data_dir, "041_DANE_SIPSA-Abast.rds")
stopifnot(file.exists(rds_path))

export_dir_default <- file.path(app_root, "Descargas")
if (!dir.exists(export_dir_default)) dir.create(export_dir_default, recursive = TRUE, showWarnings = FALSE)

# =========================================================
# 1) Helpers
# =========================================================
title_case_es <- function(x){
  x <- str_trim(as.character(x))
  x <- str_to_lower(x)
  x <- str_replace_all(x, "\\s+", " ")
  small_words <- c("de","del","la","las","los","y","e","o","u","a","en","el","al","da","do","das","dos")
  vapply(x, function(s){
    if (is.na(s) || s == "") return(NA_character_)
    w <- strsplit(s, "\\s+")[[1]]
    w <- vapply(seq_along(w), function(i){
      if (i > 1 && w[i] %in% small_words) w[i] else str_to_title(w[i], locale = "es")
    }, character(1))
    paste(w, collapse = " ")
  }, character(1))
}

parse_num_co <- function(x){
  readr::parse_number(
    as.character(x),
    locale = readr::locale(grouping_mark = ".", decimal_mark = ",")
  )
}

pick_first <- function(nms, cands){
  hit <- cands[cands %in% nms]
  if (!length(hit)) NA_character_ else hit[1]
}

req_col <- function(nms, cands, label){
  hit <- pick_first(nms, cands)
  if (is.na(hit)) {
    stop(paste0(
      "No encuentro columna para: ", label, "\n",
      "Busqué: ", paste(cands, collapse = ", "), "\n",
      "Columnas disponibles: ", paste(nms, collapse = ", ")
    ))
  }
  hit
}

fmt_ton_co <- function(x, digits = 1){
  scales::number(x, big.mark=".", decimal.mark=",", accuracy = 10^(-digits))
}
fmt_pct_co <- function(x, digits = 1){
  paste0(scales::number(100*x, big.mark=".", decimal.mark=",", accuracy = 10^(-digits)), "%")
}
fmt_num <- function(x, accuracy = 1){
  scales::number(x, accuracy = accuracy, big.mark=".", decimal.mark=",")
}
format_short <- function(x){
  ifelse(
    is.na(x), NA_character_,
    ifelse(
      abs(x) >= 1e6,
      paste0(fmt_num(x / 1e6, accuracy = 0.1), "M"),
      ifelse(
        abs(x) >= 1e3,
        paste0(fmt_num(x / 1e3, accuracy = 0.1), "K"),
        fmt_num(x, accuracy = 0.1)
      )
    )
  )
}
legend_lab_es <- function(suffix = "", between = " – "){
  function(type, cuts, p){
    if (length(cuts) <= 1) return(character())
    lows  <- head(cuts, -1)
    highs <- tail(cuts, -1)
    lows_f  <- format_short(lows)
    highs_f <- format_short(highs)
    pref <- c("", rep("> ", length(lows_f) - 1))
    paste0(pref, lows_f, between, highs_f, suffix)
  }
}

pal4_vec <- grDevices::colorRampPalette(
  c("#F6E8C3", "#EBD3A6", "#C9A56A", "#9A7547", "#6B4F2C")
)(4)

make_bins4 <- function(values){
  v <- as.numeric(values)
  v <- v[is.finite(v) & v > 0]
  if (!length(v)) return(c(0, 1, 2, 3, 4))
  
  qs <- quantile(v, probs = seq(0, 1, length.out = 5), na.rm = TRUE, type = 7)
  qs <- sort(unique(as.numeric(qs)))
  
  if (length(qs) < 5){
    r <- range(v, na.rm = TRUE)
    if (r[1] == r[2]) r <- c(0, max(1, r[2]))
    qs <- pretty(r, n = 4)
  }
  if (length(qs) < 5) qs <- seq(min(qs), max(qs), length.out = 5)
  qs
}

palBin4 <- function(values){
  v <- as.numeric(values)
  vpos <- v[is.finite(v) & v > 0]
  bins <- make_bins4(vpos)
  
  pal <- leaflet::colorBin(
    palette  = pal4_vec,
    bins     = bins,
    domain   = vpos,
    na.color = "#bdbdbd",
    right    = FALSE
  )
  attr(pal, "bins") <- bins
  pal
}

pad_dpto <- function(x){
  x <- as.character(x)
  x <- str_replace_all(x, "\\D", "")
  x <- ifelse(nchar(x) == 0, NA_character_, x)
  str_pad(x, width = 2, side = "left", pad = "0")
}
pad_mpio <- function(x){
  x <- as.character(x)
  x <- str_replace_all(x, "\\D", "")
  x <- ifelse(nchar(x) == 0, NA_character_, x)
  str_pad(x, width = 5, side = "left", pad = "0")
}

sel_is_all <- function(x){
  is.null(x) || length(x) == 0 || any(x == "Todos")
}
filter_multi <- function(df, col, sel){
  if (sel_is_all(sel)) return(df)
  df %>% dplyr::filter(.data[[col]] %in% sel)
}

save_widget_png <- function(widget, file, vwidth = 1600, vheight = 1000, delay = 1.5, zoom = 2){
  tmp_html <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(widget, tmp_html, selfcontained = TRUE)
  webshot2::webshot(
    url    = tmp_html,
    file   = file,
    vwidth = vwidth,
    vheight = vheight,
    delay  = delay,
    zoom   = zoom
  )
}

safe_basename <- function(x){
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# =========================================================
# 2) Cargar RDS y mapear columnas reales
# =========================================================
sipsa_raw <- readRDS(rds_path)
sipsa <- janitor::clean_names(sipsa_raw)
nms  <- names(sipsa)

ycol <- req_col(nms, c("ano","anio","year"), "AÑO (ano/anio/year)")
mcol <- req_col(nms, c("mes","month"), "MES (mes/month)")
gcol <- req_col(nms, c("grupo","grupo_alimento","grupo_alimentos","grupo_de_alimento"), "GRUPO")
pcol <- req_col(nms, c("alimento","producto","item","articulo","artículo"), "ALIMENTO/PRODUCTO")
qcol <- req_col(nms, c("cantkg_total","cant_kg_total","cantidad_kg","cantidadkg","cantkg","kg_total","cant_total_kg"), "CANTIDAD KG")

dep_d_col <- pick_first(nms, c("departamento_d","depto_d","departamento_destino","depto_destino"))
mun_d_col <- pick_first(nms, c("municipio_d","mpio_d","municipio_destino","mpio_destino"))

dep_o_col <- pick_first(nms, c("departamento_o","depto_o","departamento_origen","depto_origen"))
mun_o_col <- pick_first(nms, c("municipio_o","mpio_o","municipio_origen","mpio_origen"))

dpto_code_d_col <- req_col(
  nms,
  c("cod_dane_dpto_d","dane_cod_dpto_d","dane_cod_dpto","cod_dane_dpto","cod_dpto_d","cod_dpto"),
  "DANE_COD_DPTO_D (código dpto destino)"
)
mpio_code_d_col <- pick_first(nms, c("cod_dane_munic_d","dane_cod_munic_d","cod_dane_mpio_d","cod_dane_mpio","cod_mpio_d"))

dpto_code_o_col <- req_col(
  nms,
  c("cod_dane_dpto_o","dane_cod_dpto_o","cod_dpto_o","cod_dpto_origen","cod_dane_dpto_origen","dane_cod_dpto_origen"),
  "COD_DANE_DPTO_O (código dpto origen)"
)
mpio_code_o_col <- pick_first(nms, c("cod_dane_munic_o","dane_cod_munic_o","cod_dane_mpio_o","cod_mpio_o","cod_mpio_origen"))

# =========================================================
# 3) Base estándar
# =========================================================
base_sipsa <- sipsa %>%
  transmute(
    anio = suppressWarnings(as.integer(.data[[ycol]])),
    mes  = suppressWarnings(as.integer(.data[[mcol]])),
    
    cod_dpto_d = pad_dpto(.data[[dpto_code_d_col]]),
    cod_mpio_d = if (!is.na(mpio_code_d_col)) pad_mpio(.data[[mpio_code_d_col]]) else NA_character_,
    departamento_d = if (!is.na(dep_d_col)) title_case_es(.data[[dep_d_col]]) else NA_character_,
    municipio_d    = if (!is.na(mun_d_col)) title_case_es(.data[[mun_d_col]]) else NA_character_,
    
    cod_dpto_o = pad_dpto(.data[[dpto_code_o_col]]),
    cod_mpio_o = if (!is.na(mpio_code_o_col)) pad_mpio(.data[[mpio_code_o_col]]) else NA_character_,
    departamento_o = if (!is.na(dep_o_col)) title_case_es(.data[[dep_o_col]]) else NA_character_,
    municipio_o    = if (!is.na(mun_o_col)) title_case_es(.data[[mun_o_col]]) else NA_character_,
    
    grupo    = title_case_es(.data[[gcol]]),
    alimento = title_case_es(.data[[pcol]]),
    kg       = parse_num_co(.data[[qcol]])
  ) %>%
  filter(is.finite(anio), anio >= 2018) %>%
  filter(!is.na(departamento_o), str_trim(departamento_o) != "") %>%
  filter(is.finite(kg), kg > 0) %>%
  mutate(
    fecha = suppressWarnings(as.Date(sprintf("%04d-%02d-01", anio, mes))),
    ton   = kg/1000
  )

base_t1 <- base_sipsa %>% filter(cod_dpto_o == DPTO_FOCO_COD)
base_t2 <- base_sipsa %>% filter(cod_dpto_d == DPTO_FOCO_COD)

dept_names_lut <- dplyr::bind_rows(
  base_sipsa %>% dplyr::distinct(cod_dpto = cod_dpto_d, dept_name = departamento_d),
  base_sipsa %>% dplyr::distinct(cod_dpto = cod_dpto_o, dept_name = departamento_o)
) %>%
  dplyr::filter(!is.na(cod_dpto), !is.na(dept_name), stringr::str_trim(dept_name) != "") %>%
  dplyr::group_by(cod_dpto) %>%
  dplyr::summarise(dept_name = dplyr::first(dept_name), .groups = "drop")

# =========================================================
# 3.3) Shapefile Departamentos
# =========================================================
load_dept_sf_only <- function(data_dir){
  shp_dir <- file.path(data_dir, "shp")
  if (!dir.exists(shp_dir)) stop("No existe el directorio: ", shp_dir)
  shp_files <- list.files(shp_dir, pattern="\\.shp$", full.names=TRUE, recursive=TRUE)
  if (!length(shp_files)) stop("No encontré archivos .shp dentro de: ", shp_dir)
  
  ruta_dptos <- shp_files[grep("DPTOS|DEPTO|DEPART", basename(shp_files), ignore.case = TRUE)][1]
  if (is.na(ruta_dptos)) ruta_dptos <- shp_files[1]
  
  obj <- sf::st_read(ruta_dptos, quiet=TRUE)
  if (!inherits(obj, "sf") || nrow(obj) == 0) stop("No pude leer el shapefile: ", ruta_dptos)
  
  obj <- janitor::clean_names(obj)
  
  ccol <- req_col(
    names(obj),
    c("dpto_ccdgo","cod_depto","cod_dpto","codigo_depto","dpto_cod"),
    "DPTO_CCDGO (código dpto en shapefile)"
  )
  
  ncol_name <- pick_first(
    names(obj),
    c("departamento_d","departamento","dpto_cnmb","nom_dpto","name_1","name","dpto")
  )
  
  guess_crs_epsg <- function(sfobj){
    bb <- sf::st_bbox(sfobj)
    xs <- c(bb[["xmin"]], bb[["xmax"]])
    ys <- c(bb[["ymin"]], bb[["ymax"]])
    if (all(is.finite(xs), is.finite(ys)) &&
        max(abs(xs), na.rm=TRUE) <= 180 &&
        max(abs(ys), na.rm=TRUE) <= 90) return(4326)
    if (max(abs(xs), na.rm=TRUE) > 2e6) return(3857)
    3116
  }
  
  if (is.na(sf::st_crs(obj))) {
    epsg_guess <- guess_crs_epsg(obj)
    message("[MAPA] CRS faltante (.prj). Asumiendo EPSG:", epsg_guess)
    sf::st_crs(obj) <- epsg_guess
  }
  
  out <- obj %>%
    transmute(
      cod_dpto = pad_dpto(.data[[ccol]]),
      departamento_d = if (!is.na(ncol_name)) title_case_es(.data[[ncol_name]]) else pad_dpto(.data[[ccol]]),
      geometry = geometry
    ) %>%
    sf::st_transform(4326) %>%
    sf::st_make_valid() %>%
    sf::st_zm(drop = TRUE, what = "ZM")
  
  out <- out %>%
    group_by(cod_dpto) %>%
    summarise(
      departamento_d = dplyr::first(departamento_d),
      geometry = sf::st_union(geometry),
      .groups = "drop"
    )
  
  out
}

dept_sf <- tryCatch(load_dept_sf_only(data_dir), error = function(e){ message("[MAPA] ", e$message); NULL })

if (!is.null(dept_sf) && inherits(dept_sf, "sf") && nrow(dept_sf) > 0) {
  dept_sf <- dept_sf %>%
    dplyr::left_join(dept_names_lut, by = "cod_dpto") %>%
    dplyr::mutate(departamento_d = dplyr::coalesce(dept_name, departamento_d)) %>%
    dplyr::select(-dept_name)
}

# =========================================================
# 4) UI
# =========================================================
ui <- fluidPage(
  theme = bslib::bs_theme(
    version      = 5,
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
        --map-h: 640px;
      }
      body{ background:#ffffff; }
      .wrap{ max-width:1360px; margin:0 auto; padding:16px 20px 32px; }
      h2#app-title{ text-align:center; margin-top:10px; margin-bottom:10px; font-weight:800; letter-spacing:.3px; }

      .filters{
        background:#fff; border:1px solid var(--accent-border); border-radius:16px;
        padding:14px 16px; margin-bottom:16px; box-shadow:0 4px 14px rgba(0,0,0,.06);
        width:100%;
        display:block;
        overflow: visible;
        position: relative;
        z-index: 20;
      }

      .filters-grid{
        width:100%;
        display:grid;
        grid-template-columns: repeat(5, minmax(170px, 1fr));
        column-gap:16px;
        row-gap:10px;
        align-items:end;
      }
      .filters-grid .filter{ min-width:0; }

      @media (max-width: 1100px){ .filters-grid{ grid-template-columns: repeat(3, minmax(170px, 1fr)); } }
      @media (max-width: 650px){ .filters-grid{ grid-template-columns: 1fr; } }

      .filter-label{
        font-weight:500; font-size:14px; margin-bottom:6px; color:#000;
        white-space: normal;
        line-height: 1.1;
        min-height: 32px;
      }

      .form-select, .bootstrap-select > .dropdown-toggle, .selectize-input{
        border:1px solid var(--accent-border) !important;
        border-radius:10px !important;
        box-shadow:none !important;
        font-size:14px; font-weight:500; color:#000;
        background-color:#fff !important;
        min-height:42px;
        width:100% !important;
      }

      .bootstrap-select,
      .bootstrap-select > .dropdown-toggle,
      .selectize-control,
      .selectize-input{
        width:100% !important;
      }

      .bootstrap-select .dropdown-menu{ z-index: 99999 !important; }
      .selectize-dropdown{ z-index: 99999 !important; }
      .leaflet-container{ z-index: 1 !important; }

      .card{
        background:#fff; border:1px solid var(--accent-border) !important;
        border-radius:16px; padding:12px;
        box-shadow:0 2px 10px rgba(0,0,0,.05); margin-bottom:12px;
      }
      .card-title{ font-weight:700; font-size:16px; margin-bottom:8px; color:#111827; }
      .header-row{ display:flex; gap:10px; align-items:center; justify-content:space-between; }

      .content-grid{
        display:flex;
        gap:var(--gap);
        align-items:flex-start;
      }
      .left-col{ flex:1.05; min-width:0; }

      .right-col{
        flex:1;
        min-width:0;
        display:flex;
        flex-direction:column;
        gap:var(--gap);
        height: var(--map-h);
      }

      .viz-map{ height: var(--map-h); }

      .right-col .viz-card{
        flex: 1 1 0;
        height: auto !important;
        min-height: 0;
        margin:0;
      }

      .viz-card{ display:flex; flex-direction:column; }
      .viz-body{ flex:1 1 auto; min-height:0; }

      .viz-body .leaflet{ height:100% !important; }
      .viz-body .plotly,
      .viz-body .html-widget,
      .viz-body .plot-container{
        height:100% !important;
      }

      .leaflet-tooltip.lbl-clean{
        background: rgba(255,255,255,.92); border: 1px solid #e6e6e6; border-radius: 6px;
        padding: 4px 6px; color: #222; font-weight: 600; box-shadow: 0 1px 4px rgba(0,0,0,.08);
      }
      .map-note{ font-size:12px; color:#4b5563; margin-top:6px; }
      .dl-under{
        margin-top:8px;
        display:flex;
        gap:8px;
        flex-wrap:wrap;
        justify-content:flex-end;
      }

      .report-box{
        background:#fff; border:1px solid #99d5ec; border-radius:16px;
        padding:14px; box-shadow:0 2px 10px rgba(0,0,0,.05); margin-top:8px;
      }

      table.dataTable tbody td{ padding:6px 8px; }
      table.dataTable thead th{ padding:8px; }
      .nav-tabs .nav-link{ font-weight:700; }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('leaflet-resize', function(id){
        setTimeout(function(){
          try{
            var w = HTMLWidgets.find('#' + id);
            if(w && w.getMap){
              var m = w.getMap();
              if(m){ m.invalidateSize(true); }
            }
          } catch(e){}
        }, 250);
      });
    "))
  ),
  
  div(
    class = "wrap",
    h2(id = "app-title", APP_TITLE),
    
    tabsetPanel(
      id   = "tabs_sipsa",
      type = "tabs",
      
      tabPanel(
        "Remisión de alimentos desde Atlántico", br(),
        
        div(
          class = "filters",
          div(
            class = "filters-grid",
            div(class="filter", div(class="filter-label","¿Qué año analizamos?"), uiOutput("anio_ui_t1")),
            div(class="filter", div(class="filter-label","¿En qué mes?"), uiOutput("mes_ui_t1")),
            div(class="filter", div(class="filter-label","¿En qué departamento de destino?"), uiOutput("territorio_ui_t1")),
            div(class="filter", div(class="filter-label","¿En qué grupo alimenticio?"), uiOutput("grupos_ui_t1")),
            div(class="filter", div(class="filter-label","¿En qué alimento?"), uiOutput("alimentos_ui_t1"))
          )
        ),
        
        div(
          class = "content-grid",
          
          div(
            class = "left-col",
            div(
              class = "card viz-card viz-map",
              div(class="card-title", strong("¿En qué departamentos se concentra la mayor cantidad de alimentos remitidos de la central mayorista priorizada?")),
              div(class="viz-body", leafletOutput("map_t1")),
              div(class="map-note","Nota: El mapa clasifica los valores del indicador en cuartiles (cuatro grupos con igual número de observaciones)."),
              div(
                class="dl-under",
                downloadButton("dl_png_map_t1", "PNG — Mapa")
              )
            )
          ),
          
          div(
            class = "right-col",
            div(
              class="card viz-card",
              div(class="card-title", strong("¿Qué grupos de alimentos tienen mayor participación?")),
              div(class="viz-body", plotlyOutput("bar_grupos_t1", height="100%")),
              div(class="dl-under", downloadButton("dl_png_bar_t1", "PNG — Barras"))
            ),
            div(
              class="card viz-card",
              div(class="card-title", strong("¿Cómo es la evolución de abastecimiento de alimentos?")),
              div(class="viz-body", plotlyOutput("serie_total_t1", height="100%")),
              div(class="dl-under", downloadButton("dl_png_serie_t1", "PNG — Serie"))
            )
          )
        ),
        
        div(
          class="card",
          div(class="header-row",
              div(class="card-title",
                  strong("¿Cuál es la composición de alimentos remitidos desde el departamento?")),
              div(style="font-size:12px;color:#6b7280;",
                  "El % se calcula sobre el total de toneladas del departamento (destino), por año, con los filtros del Tab 1")
          ),
          DTOutput("tbl_detalle_t1")
        ),
        
        div(
          class = "report-box",
          h4("Informe descargable"),
          p("El PDF solo se habilita cuando ya se hayan visualizado las dos pestañas del tablero."),
          uiOutput("pdf_ui_t1")
        )
      ),
      
      tabPanel(
        "Recepción de alimentos en Atlántico", br(),
        
        div(
          class = "filters",
          div(
            class = "filters-grid",
            div(class="filter", div(class="filter-label","¿Qué año analizamos?"), uiOutput("anio_ui_t2")),
            div(class="filter", div(class="filter-label","¿En qué mes?"), uiOutput("mes_ui_t2")),
            div(class="filter", div(class="filter-label","¿En qué departamento de origen?"), uiOutput("territorio_ui_t2")),
            div(class="filter", div(class="filter-label","¿En qué grupo alimenticio?"), uiOutput("grupos_ui_t2")),
            div(class="filter", div(class="filter-label","¿En qué alimento?"), uiOutput("alimentos_ui_t2"))
          )
        ),
        
        div(
          class = "content-grid",
          
          div(
            class = "left-col",
            div(
              class = "card viz-card viz-map",
              div(class="card-title", strong("¿En qué departamentos se concentra la mayor cantidad de alimentos recibidos de la central mayorista priorizada?")),
              div(class="viz-body", leafletOutput("map_t2")),
              div(class="map-note","Nota: El mapa clasifica los valores del indicador en cuartiles (cuatro grupos con igual número de observaciones)."),
              div(
                class="dl-under",
                downloadButton("dl_png_map_t2", "PNG — Mapa")
              )
            )
          ),
          
          div(
            class = "right-col",
            div(
              class="card viz-card",
              div(class="card-title", strong("¿Qué grupos de alimentos tienen mayor participación?")),
              div(class="viz-body", plotlyOutput("bar_grupos_t2", height="100%")),
              div(class="dl-under", downloadButton("dl_png_bar_t2", "PNG — Barras"))
            ),
            div(
              class="card viz-card",
              div(class="card-title", strong("¿Cómo es la evolución de abastecimiento de alimentos?")),
              div(class="viz-body", plotlyOutput("serie_total_t2", height="100%")),
              div(class="dl-under", downloadButton("dl_png_serie_t2", "PNG — Serie"))
            )
          )
        ),
        
        div(
          class="card",
          div(class="header-row",
              div(class="card-title",
                  strong("¿Cuál es la composición de alimentos recibidos en el departamento?")),
              div(style="font-size:12px;color:#6b7280;",
                  "El % se calcula sobre el total de toneladas del departamento (origen), por año, con los filtros del Tab 2")
          ),
          DTOutput("tbl_detalle_t2")
        )
      )
    )
  )
)

# =========================================================
# 5) SERVER
# =========================================================
server <- function(input, output, session){
  
  years  <- sort(unique(base_sipsa$anio[is.finite(base_sipsa$anio)]))
  months <- sort(unique(base_sipsa$mes[is.finite(base_sipsa$mes)]))
  
  mes_nombre <- c(
    "Enero","Febrero","Marzo","Abril","Mayo","Junio",
    "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"
  )
  month_choices <- c("Todos" = "Todos", stats::setNames(as.character(months), mes_nombre[months]))
  
  hover_label_opts <- leaflet::labelOptions(
    direction="auto", textsize="12px", sticky=TRUE, opacity=0.95, className="lbl-clean"
  )
  
  rv <- reactiveValues(
    tab1_seen = TRUE,   # arranca en la primera pestaña
    tab2_seen = FALSE
  )
  
  observeEvent(input$tabs_sipsa, {
    if (identical(input$tabs_sipsa, "Remisión de alimentos desde Atlántico")) rv$tab1_seen <- TRUE
    if (identical(input$tabs_sipsa, "Recepción de alimentos en Atlántico"))   rv$tab2_seen <- TRUE
  }, ignoreInit = FALSE)
  
  both_tabs_seen <- reactive({
    isTRUE(rv$tab1_seen) && isTRUE(rv$tab2_seen)
  })
  
  output$pdf_ui_t1 <- renderUI({
    if (both_tabs_seen()) {
      tagList(
        tags$p(style="color:#065f46;font-weight:600;", "Listo: ya se visualizaron las dos pestañas."),
        downloadButton("dl_pdf_report", "Descargar informe PDF")
      )
    } else {
      faltan <- c()
      if (!isTRUE(rv$tab1_seen)) faltan <- c(faltan, "Remisión")
      if (!isTRUE(rv$tab2_seen)) faltan <- c(faltan, "Recepción")
      
      tagList(
        tags$p(
          style="color:#92400e;font-weight:600;",
          paste0("Aún no se habilita el PDF. Falta visualizar: ", paste(faltan, collapse = " y "), ".")
        ),
        tags$button(
          type = "button",
          class = "btn btn-secondary",
          disabled = NA,
          "Descargar informe PDF"
        )
      )
    }
  })
  
  # =========================================================
  # TAB 1 — Inputs
  # =========================================================
  output$anio_ui_t1 <- renderUI({
    selectInput("anio_t1", NULL, choices = c("Todos"="Todos", years),
                selected = if (length(years)) max(years) else "Todos")
  })
  output$mes_ui_t1 <- renderUI({
    selectInput("mes_t1", NULL, choices = month_choices, selected = "Todos")
  })
  output$territorio_ui_t1 <- renderUI({
    opts <- sort(unique(na.omit(base_t1$departamento_d)))
    pickerInput(
      "territorio_t1", NULL,
      choices  = c("Todos", opts),
      selected = "Todos",
      options  = list(`live-search`=TRUE, size=7)
    )
  })
  
  # =========================================================
  # TAB 2 — Inputs
  # =========================================================
  output$anio_ui_t2 <- renderUI({
    selectInput("anio_t2", NULL, choices = c("Todos"="Todos", years),
                selected = if (length(years)) max(years) else "Todos")
  })
  output$mes_ui_t2 <- renderUI({
    selectInput("mes_t2", NULL, choices = month_choices, selected = "Todos")
  })
  output$territorio_ui_t2 <- renderUI({
    opts <- sort(unique(na.omit(base_t2$departamento_o)))
    pickerInput(
      "territorio_t2", NULL,
      choices  = c("Todos", opts),
      selected = "Todos",
      options  = list(`live-search`=TRUE, size=7)
    )
  })
  
  # =========================================================
  # TAB 1 — Contexto
  # =========================================================
  ctx_t1 <- reactive({
    df <- base_t1
    if (!is.null(input$anio_t1) && input$anio_t1 != "Todos") df <- df %>% filter(anio == as.integer(input$anio_t1))
    if (!is.null(input$mes_t1)  && input$mes_t1  != "Todos") df <- df %>% filter(mes  == as.integer(input$mes_t1))
    
    df <- df %>% mutate(territorio_lbl = departamento_d)
    
    if (!is.null(input$territorio_t1) && input$territorio_t1 != "Todos") {
      df <- df %>% filter(territorio_lbl == input$territorio_t1)
    }
    df
  })
  
  output$grupos_ui_t1 <- renderUI({
    df <- ctx_t1()
    grupos <- sort(unique(na.omit(df$grupo)))
    pickerInput(
      "grupos_t1", NULL,
      choices  = c("Todos", grupos),
      selected = "Todos",
      multiple = TRUE,
      options  = list(`live-search`=TRUE, `actions-box`=TRUE, size=8)
    )
  })
  
  output$alimentos_ui_t1 <- renderUI({
    df <- ctx_t1()
    df <- filter_multi(df, "grupo", input$grupos_t1)
    alimentos <- sort(unique(na.omit(df$alimento)))
    pickerInput(
      "alimentos_t1", NULL,
      choices  = c("Todos", alimentos),
      selected = "Todos",
      multiple = TRUE,
      options  = list(`live-search`=TRUE, `actions-box`=TRUE, size=8)
    )
  })
  
  # =========================================================
  # TAB 2 — Contexto
  # =========================================================
  ctx_t2 <- reactive({
    df <- base_t2
    if (!is.null(input$anio_t2) && input$anio_t2 != "Todos") df <- df %>% filter(anio == as.integer(input$anio_t2))
    if (!is.null(input$mes_t2)  && input$mes_t2  != "Todos") df <- df %>% filter(mes  == as.integer(input$mes_t2))
    
    df <- df %>% mutate(territorio_lbl = departamento_o)
    
    if (!is.null(input$territorio_t2) && input$territorio_t2 != "Todos") {
      df <- df %>% filter(territorio_lbl == input$territorio_t2)
    }
    df
  })
  
  output$grupos_ui_t2 <- renderUI({
    df <- ctx_t2()
    grupos <- sort(unique(na.omit(df$grupo)))
    pickerInput(
      "grupos_t2", NULL,
      choices  = c("Todos", grupos),
      selected = "Todos",
      multiple = TRUE,
      options  = list(`live-search`=TRUE, `actions-box`=TRUE, size=8)
    )
  })
  
  output$alimentos_ui_t2 <- renderUI({
    df <- ctx_t2()
    df <- filter_multi(df, "grupo", input$grupos_t2)
    alimentos <- sort(unique(na.omit(df$alimento)))
    pickerInput(
      "alimentos_t2", NULL,
      choices  = c("Todos", alimentos),
      selected = "Todos",
      multiple = TRUE,
      options  = list(`live-search`=TRUE, `actions-box`=TRUE, size=8)
    )
  })
  
  # =========================================================
  # DATOS FILTRADOS
  # =========================================================
  datos_t1 <- reactive({
    df <- ctx_t1()
    df <- filter_multi(df, "grupo",    input$grupos_t1)
    df <- filter_multi(df, "alimento", input$alimentos_t1)
    validate(need(nrow(df) > 0, "Sin datos con los filtros seleccionados (Tab 1)"))
    df
  })
  
  datos_t2 <- reactive({
    df <- ctx_t2()
    df <- filter_multi(df, "grupo",    input$grupos_t2)
    df <- filter_multi(df, "alimento", input$alimentos_t2)
    validate(need(nrow(df) > 0, "Sin datos con los filtros seleccionados (Tab 2)"))
    df
  })
  
  # =========================================================
  # BADGES
  # =========================================================
  badge_t1 <- reactive({
    yr <- if (!is.null(input$anio_t1) && input$anio_t1 != "Todos") as.character(input$anio_t1) else "Todos"
    ms <- if (!is.null(input$mes_t1)  && input$mes_t1  != "Todos") mes_nombre[as.integer(input$mes_t1)] else "Todos"
    tr <- input$territorio_t1 %||% "Todos"
    htmltools::HTML(sprintf(
      '<div style="background:#fff;padding:6px 10px;border-radius:8px;
                   box-shadow:0 1px 6px rgba(0,0,0,.15);font-size:12px;line-height:1.3;">
         <b>Remisión:</b> desde %s<br>
         <b>Año:</b> %s &nbsp; <b>Mes:</b> %s<br><b>Destino:</b> %s
       </div>', DPTO_FOCO_NOMBRE, yr, ms, htmltools::htmlEscape(tr)
    ))
  })
  
  badge_t2 <- reactive({
    yr <- if (!is.null(input$anio_t2) && input$anio_t2 != "Todos") as.character(input$anio_t2) else "Todos"
    ms <- if (!is.null(input$mes_t2)  && input$mes_t2  != "Todos") mes_nombre[as.integer(input$mes_t2)] else "Todos"
    tr <- input$territorio_t2 %||% "Todos"
    htmltools::HTML(sprintf(
      '<div style="background:#fff;padding:6px 10px;border-radius:8px;
                   box-shadow:0 1px 6px rgba(0,0,0,.15);font-size:12px;line-height:1.3;">
         <b>Recepción:</b> en %s<br>
         <b>Año:</b> %s &nbsp; <b>Mes:</b> %s<br><b>Origen:</b> %s
       </div>', DPTO_FOCO_NOMBRE, yr, ms, htmltools::htmlEscape(tr)
    ))
  })
  
  # =========================================================
  # MAPAS
  # =========================================================
  output$map_t1 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -74.0, lat = 4.6, zoom = 5)
  })
  outputOptions(output, "map_t1", suspendWhenHidden = FALSE)
  
  mapa_t1_sf <- reactive({
    if (is.null(dept_sf) || !inherits(dept_sf, "sf") || nrow(dept_sf) == 0) return(NULL)
    
    agg <- datos_t1() %>%
      mutate(cod_dpto = pad_dpto(cod_dpto_d)) %>%
      group_by(cod_dpto) %>%
      summarise(ton = sum(ton, na.rm=TRUE), .groups="drop")
    
    dept_sf %>%
      mutate(cod_dpto = pad_dpto(cod_dpto)) %>%
      left_join(agg, by="cod_dpto") %>%
      mutate(ton = as.numeric(ton))
  })
  
  build_map_widget_t1 <- function(){
    shp <- tryCatch(mapa_t1_sf(), error = function(e) NULL)
    
    if (is.null(shp) || !inherits(shp, "sf") || nrow(shp) == 0) {
      return(
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(-74, 4.6, 5)
      )
    }
    
    shp <- shp %>% sf::st_make_valid() %>% sf::st_zm(drop = TRUE, what = "ZM")
    shp <- tryCatch(sf::st_cast(shp, "MULTIPOLYGON", warn = FALSE), error = function(e) shp)
    shp <- shp[!sf::st_is_empty(shp$geometry), , drop = FALSE]
    pal <- palBin4(shp$ton)
    
    leaflet(shp, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~ifelse(is.na(ton) | ton <= 0, "#bdbdbd", pal(ton)),
        weight = 0.7, color = "#666", fillOpacity = 0.9,
        label = ~ifelse(
          is.na(ton) | ton <= 0,
          sprintf("%s — Sin información", departamento_d),
          sprintf("%s — %s Ton", departamento_d, fmt_ton_co(ton, 1))
        ),
        labelOptions = hover_label_opts
      ) %>%
      addLegend(
        position="bottomright",
        pal=pal,
        values=~ifelse(is.na(ton)|ton<=0, NA, ton),
        title="Toneladas",
        labFormat=legend_lab_es(suffix=" Ton"),
        na.label="Sin información"
      ) %>%
      addControl(badge_t1(), position="topright")
  }
  
  draw_map_t1 <- function(){
    mdat <- tryCatch(mapa_t1_sf(), error = function(e) NULL)
    if (is.null(mdat) || !inherits(mdat,"sf") || nrow(mdat)==0) return(invisible(NULL))
    
    mdat <- mdat %>% sf::st_make_valid() %>% sf::st_zm(drop=TRUE, what="ZM")
    mdat <- tryCatch(sf::st_cast(mdat, "MULTIPOLYGON", warn=FALSE), error=function(e) mdat)
    mdat <- mdat[!sf::st_is_empty(mdat$geometry), , drop=FALSE]
    if (nrow(mdat)==0) return(invisible(NULL))
    
    pal <- palBin4(mdat$ton)
    bb  <- sf::st_bbox(mdat)
    
    leafletProxy("map_t1", data=mdat) %>%
      clearGroup("poly") %>%
      clearControls() %>%
      addPolygons(
        group="poly",
        layerId = ~departamento_d,
        fillColor = ~ifelse(is.na(ton) | ton <= 0, "#bdbdbd", pal(ton)),
        weight=0.7, color="#666", fillOpacity=0.9,
        label = ~ifelse(
          is.na(ton) | ton <= 0,
          sprintf("%s — Sin información", departamento_d),
          sprintf("%s — %s Ton", departamento_d, fmt_ton_co(ton, 1))
        ),
        labelOptions = hover_label_opts,
        highlightOptions = leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
      ) %>%
      addLegend(
        position="bottomright",
        pal=pal,
        values=~ifelse(is.na(ton)|ton<=0, NA, ton),
        title="Toneladas",
        labFormat=legend_lab_es(suffix=" Ton"),
        na.label="Sin información"
      ) %>%
      addControl(badge_t1(), position="topright") %>%
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  }
  
  session$onFlushed(function(){ draw_map_t1() }, once=TRUE)
  
  observeEvent(list(input$anio_t1, input$mes_t1, input$territorio_t1, input$grupos_t1, input$alimentos_t1), {
    draw_map_t1()
  }, ignoreInit=FALSE)
  
  observeEvent(input$tabs_sipsa, {
    if (!is.null(input$tabs_sipsa) && input$tabs_sipsa == "Remisión de alimentos desde Atlántico") {
      session$sendCustomMessage("leaflet-resize", "map_t1")
      draw_map_t1()
    }
  }, ignoreInit=TRUE)
  
  output$map_t2 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -74.0, lat = 4.6, zoom = 5)
  })
  outputOptions(output, "map_t2", suspendWhenHidden = FALSE)
  
  mapa_t2_sf <- reactive({
    if (is.null(dept_sf) || !inherits(dept_sf, "sf") || nrow(dept_sf) == 0) return(NULL)
    
    agg <- datos_t2() %>%
      mutate(cod_dpto = pad_dpto(cod_dpto_o)) %>%
      group_by(cod_dpto) %>%
      summarise(ton = sum(ton, na.rm=TRUE), .groups="drop")
    
    dept_sf %>%
      mutate(cod_dpto = pad_dpto(cod_dpto)) %>%
      left_join(agg, by="cod_dpto") %>%
      mutate(ton = as.numeric(ton))
  })
  
  build_map_widget_t2 <- function(){
    shp <- tryCatch(mapa_t2_sf(), error = function(e) NULL)
    
    if (is.null(shp) || !inherits(shp, "sf") || nrow(shp) == 0) {
      return(
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(-74, 4.6, 5)
      )
    }
    
    shp <- shp %>% sf::st_make_valid() %>% sf::st_zm(drop = TRUE, what = "ZM")
    shp <- tryCatch(sf::st_cast(shp, "MULTIPOLYGON", warn = FALSE), error = function(e) shp)
    shp <- shp[!sf::st_is_empty(shp$geometry), , drop = FALSE]
    pal <- palBin4(shp$ton)
    
    leaflet(shp, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~ifelse(is.na(ton) | ton <= 0, "#bdbdbd", pal(ton)),
        weight = 0.7, color = "#666", fillOpacity = 0.9,
        label = ~ifelse(
          is.na(ton) | ton <= 0,
          sprintf("%s — Sin información", departamento_d),
          sprintf("%s — %s Ton", departamento_d, fmt_ton_co(ton, 1))
        ),
        labelOptions = hover_label_opts
      ) %>%
      addLegend(
        position="bottomright",
        pal=pal,
        values=~ifelse(is.na(ton)|ton<=0, NA, ton),
        title="Toneladas",
        labFormat=legend_lab_es(suffix=" Ton"),
        na.label="Sin información"
      ) %>%
      addControl(badge_t2(), position="topright")
  }
  
  draw_map_t2 <- function(){
    mdat <- tryCatch(mapa_t2_sf(), error = function(e) NULL)
    if (is.null(mdat) || !inherits(mdat,"sf") || nrow(mdat)==0) return(invisible(NULL))
    
    mdat <- mdat %>% sf::st_make_valid() %>% sf::st_zm(drop=TRUE, what="ZM")
    mdat <- tryCatch(sf::st_cast(mdat, "MULTIPOLYGON", warn=FALSE), error=function(e) mdat)
    mdat <- mdat[!sf::st_is_empty(mdat$geometry), , drop=FALSE]
    if (nrow(mdat)==0) return(invisible(NULL))
    
    pal <- palBin4(mdat$ton)
    bb  <- sf::st_bbox(mdat)
    
    leafletProxy("map_t2", data=mdat) %>%
      clearGroup("poly") %>%
      clearControls() %>%
      addPolygons(
        group="poly",
        layerId = ~departamento_d,
        fillColor = ~ifelse(is.na(ton) | ton <= 0, "#bdbdbd", pal(ton)),
        weight=0.7, color="#666", fillOpacity=0.9,
        label = ~ifelse(
          is.na(ton) | ton <= 0,
          sprintf("%s — Sin información", departamento_d),
          sprintf("%s — %s Ton", departamento_d, fmt_ton_co(ton, 1))
        ),
        labelOptions = hover_label_opts,
        highlightOptions = leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
      ) %>%
      addLegend(
        position="bottomright",
        pal=pal,
        values=~ifelse(is.na(ton)|ton<=0, NA, ton),
        title="Toneladas",
        labFormat=legend_lab_es(suffix=" Ton"),
        na.label="Sin información"
      ) %>%
      addControl(badge_t2(), position="topright") %>%
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  }
  
  observeEvent(list(input$anio_t2, input$mes_t2, input$territorio_t2, input$grupos_t2, input$alimentos_t2), {
    draw_map_t2()
  }, ignoreInit=FALSE)
  
  observeEvent(input$tabs_sipsa, {
    if (!is.null(input$tabs_sipsa) && input$tabs_sipsa == "Recepción de alimentos en Atlántico") {
      session$sendCustomMessage("leaflet-resize", "map_t2")
      draw_map_t2()
    }
  }, ignoreInit=TRUE)
  
  # =========================================================
  # BARRAS Y SERIES — construir widgets reutilizables
  # =========================================================
  bar_grupos_t1_df <- reactive({
    datos_t1() %>%
      group_by(grupo) %>%
      summarise(ton=sum(ton, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(ton)) %>%
      head(10)
  })
  
  build_bar_widget_t1 <- function(){
    dd <- bar_grupos_t1_df()
    validate(need(nrow(dd) > 0, "Sin datos para barras (Tab 1)"))
    dd$grupo <- factor(dd$grupo, levels = rev(dd$grupo))
    
    plot_ly(
      dd, x=~ton, y=~grupo, type="bar", orientation="h",
      hovertemplate="<b>%{y}</b><br>Ton: %{customdata}<extra></extra>",
      customdata=~fmt_ton_co(ton, 1)
    ) %>%
      layout(
        xaxis=list(title="Toneladas", zeroline=FALSE),
        yaxis=list(title="", automargin=TRUE),
        margin=list(l=140, r=20, b=40, t=10)
      ) %>%
      config(displayModeBar = FALSE)
  }
  
  output$bar_grupos_t1 <- renderPlotly({
    build_bar_widget_t1()
  })
  
  bar_grupos_t2_df <- reactive({
    datos_t2() %>%
      group_by(grupo) %>%
      summarise(ton=sum(ton, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(ton)) %>%
      head(10)
  })
  
  build_bar_widget_t2 <- function(){
    dd <- bar_grupos_t2_df()
    validate(need(nrow(dd) > 0, "Sin datos para barras (Tab 2)"))
    dd$grupo <- factor(dd$grupo, levels = rev(dd$grupo))
    
    plot_ly(
      dd, x=~ton, y=~grupo, type="bar", orientation="h",
      hovertemplate="<b>%{y}</b><br>Ton: %{customdata}<extra></extra>",
      customdata=~fmt_ton_co(ton, 1)
    ) %>%
      layout(
        xaxis=list(title="Toneladas", zeroline=FALSE),
        yaxis=list(title="", automargin=TRUE),
        margin=list(l=140, r=20, b=40, t=10)
      ) %>%
      config(displayModeBar = FALSE)
  }
  
  output$bar_grupos_t2 <- renderPlotly({
    build_bar_widget_t2()
  })
  
  serie_total_t1_df <- reactive({
    df <- base_t1
    if (!is.null(input$anio_t1) && input$anio_t1 != "Todos") df <- df %>% filter(anio == as.integer(input$anio_t1))
    
    df <- df %>% mutate(territorio_lbl = departamento_d)
    if (!is.null(input$territorio_t1) && input$territorio_t1 != "Todos") df <- df %>% filter(territorio_lbl == input$territorio_t1)
    
    df <- filter_multi(df, "grupo",    input$grupos_t1)
    df <- filter_multi(df, "alimento", input$alimentos_t1)
    
    out <- df %>%
      group_by(fecha) %>%
      summarise(ton=sum(ton, na.rm=TRUE), .groups="drop") %>%
      arrange(fecha)
    
    validate(need(nrow(out) > 0, "Sin datos para serie total (Tab 1)"))
    out
  })
  
  build_serie_widget_t1 <- function(){
    ts <- serie_total_t1_df()
    plot_ly(
      ts, x=~fecha, y=~ton, type="scatter", mode="lines+markers",
      hovertemplate="Fecha: %{x|%Y-%m}<br>Ton: %{customdata}<extra></extra>",
      customdata=~fmt_ton_co(ton, 1)
    ) %>%
      layout(
        xaxis=list(title="", tickformat="%Y-%m"),
        yaxis=list(title="Toneladas"),
        margin=list(l=60, r=20, b=40, t=10)
      ) %>%
      config(displayModeBar = FALSE)
  }
  
  output$serie_total_t1 <- renderPlotly({
    build_serie_widget_t1()
  })
  
  serie_total_t2_df <- reactive({
    df <- base_t2
    if (!is.null(input$anio_t2) && input$anio_t2 != "Todos") df <- df %>% filter(anio == as.integer(input$anio_t2))
    
    df <- df %>% mutate(territorio_lbl = departamento_o)
    if (!is.null(input$territorio_t2) && input$territorio_t2 != "Todos") df <- df %>% filter(territorio_lbl == input$territorio_t2)
    
    df <- filter_multi(df, "grupo",    input$grupos_t2)
    df <- filter_multi(df, "alimento", input$alimentos_t2)
    
    out <- df %>%
      group_by(fecha) %>%
      summarise(ton=sum(ton, na.rm=TRUE), .groups="drop") %>%
      arrange(fecha)
    
    validate(need(nrow(out) > 0, "Sin datos para serie total (Tab 2)"))
    out
  })
  
  build_serie_widget_t2 <- function(){
    ts <- serie_total_t2_df()
    plot_ly(
      ts, x=~fecha, y=~ton, type="scatter", mode="lines+markers",
      hovertemplate="Fecha: %{x|%Y-%m}<br>Ton: %{customdata}<extra></extra>",
      customdata=~fmt_ton_co(ton, 1)
    ) %>%
      layout(
        xaxis=list(title="", tickformat="%Y-%m"),
        yaxis=list(title="Toneladas"),
        margin=list(l=60, r=20, b=40, t=10)
      ) %>%
      config(displayModeBar = FALSE)
  }
  
  output$serie_total_t2 <- renderPlotly({
    build_serie_widget_t2()
  })
  
  # =========================================================
  # TABLAS
  # =========================================================
  detalle_t1 <- reactive({
    df <- datos_t1()
    
    det <- df %>%
      group_by(anio, Departamento = departamento_d, grupo, alimento) %>%
      summarise(ton=sum(ton, na.rm=TRUE), .groups="drop")
    validate(need(nrow(det) > 0, "Sin filas para tabla (Tab 1)"))
    
    dep_rank <- df %>%
      group_by(Departamento = departamento_d) %>%
      summarise(dep_total_ton=sum(ton, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(dep_total_ton)) %>%
      mutate(dep_order = row_number())
    
    den <- det %>%
      group_by(anio, Departamento) %>%
      summarise(den_ton=sum(ton, na.rm=TRUE), .groups="drop")
    
    det %>%
      left_join(den, by=c("anio","Departamento")) %>%
      mutate(pct = ifelse(den_ton > 0, ton/den_ton, NA_real_)) %>%
      select(-den_ton) %>%
      left_join(dep_rank, by="Departamento") %>%
      mutate(dep_order = ifelse(is.na(dep_order), 999999L, dep_order)) %>%
      arrange(dep_order, desc(ton), anio, grupo, alimento)
  })
  
  output$tbl_detalle_t1 <- renderDT({
    det <- detalle_t1() %>%
      mutate(Toneladas = fmt_ton_co(ton, 1), Porc = fmt_pct_co(pct, 1))
    
    show <- det %>% transmute(
      Año = anio,
      Departamento = Departamento,
      Grupo = grupo,
      Alimento = alimento,
      Toneladas = Toneladas,
      `% del departamento` = Porc,
      dep_order = dep_order,
      ton_num = ton
    )
    
    datatable(
      show,
      rownames = FALSE,
      options = list(
        pageLength = 15, lengthChange = FALSE,
        scrollX = TRUE, scrollY = "420px", deferRender = TRUE,
        order = list(list(6, "asc"), list(7, "desc"), list(0, "asc")),
        columnDefs = list(list(visible = FALSE, targets = c(6, 7)))
      )
    )
  })
  
  detalle_t2 <- reactive({
    df <- datos_t2()
    
    det <- df %>%
      group_by(anio, Departamento = departamento_o, grupo, alimento) %>%
      summarise(ton=sum(ton, na.rm=TRUE), .groups="drop")
    validate(need(nrow(det) > 0, "Sin filas para tabla (Tab 2)"))
    
    dep_rank <- df %>%
      group_by(Departamento = departamento_o) %>%
      summarise(dep_total_ton=sum(ton, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(dep_total_ton)) %>%
      mutate(dep_order = row_number())
    
    den <- det %>%
      group_by(anio, Departamento) %>%
      summarise(den_ton=sum(ton, na.rm=TRUE), .groups="drop")
    
    det %>%
      left_join(den, by=c("anio","Departamento")) %>%
      mutate(pct = ifelse(den_ton > 0, ton/den_ton, NA_real_)) %>%
      select(-den_ton) %>%
      left_join(dep_rank, by="Departamento") %>%
      mutate(dep_order = ifelse(is.na(dep_order), 999999L, dep_order)) %>%
      arrange(dep_order, desc(ton), anio, grupo, alimento)
  })
  
  output$tbl_detalle_t2 <- renderDT({
    det <- detalle_t2() %>%
      mutate(Toneladas = fmt_ton_co(ton, 1), Porc = fmt_pct_co(pct, 1))
    
    show <- det %>% transmute(
      Año = anio,
      Departamento = Departamento,
      Grupo = grupo,
      Alimento = alimento,
      Toneladas = Toneladas,
      `% del departamento` = Porc,
      dep_order = dep_order,
      ton_num = ton
    )
    
    datatable(
      show,
      rownames = FALSE,
      options = list(
        pageLength = 15, lengthChange = FALSE,
        scrollX = TRUE, scrollY = "420px", deferRender = TRUE,
        order = list(list(6, "asc"), list(7, "desc"), list(0, "asc")),
        columnDefs = list(list(visible = FALSE, targets = c(6, 7)))
      )
    )
  })
  
  # =========================================================
  # DOWNLOAD PNG — TAB 1
  # =========================================================
  output$dl_png_map_t1 <- downloadHandler(
    filename = function() paste0("SIPSA_tab1_mapa_", Sys.Date(), ".png"),
    content  = function(file){
      widget <- build_map_widget_t1()
      save_widget_png(widget, file, vwidth = 1600, vheight = 1100, delay = 2, zoom = 2)
    }
  )
  
  output$dl_png_bar_t1 <- downloadHandler(
    filename = function() paste0("SIPSA_tab1_barras_", Sys.Date(), ".png"),
    content  = function(file){
      widget <- build_bar_widget_t1()
      save_widget_png(widget, file, vwidth = 1600, vheight = 900, delay = 1.2, zoom = 2)
    }
  )
  
  output$dl_png_serie_t1 <- downloadHandler(
    filename = function() paste0("SIPSA_tab1_serie_", Sys.Date(), ".png"),
    content  = function(file){
      widget <- build_serie_widget_t1()
      save_widget_png(widget, file, vwidth = 1600, vheight = 900, delay = 1.2, zoom = 2)
    }
  )
  
  # =========================================================
  # DOWNLOAD PNG — TAB 2
  # =========================================================
  output$dl_png_map_t2 <- downloadHandler(
    filename = function() paste0("SIPSA_tab2_mapa_", Sys.Date(), ".png"),
    content  = function(file){
      widget <- build_map_widget_t2()
      save_widget_png(widget, file, vwidth = 1600, vheight = 1100, delay = 2, zoom = 2)
    }
  )
  
  output$dl_png_bar_t2 <- downloadHandler(
    filename = function() paste0("SIPSA_tab2_barras_", Sys.Date(), ".png"),
    content  = function(file){
      widget <- build_bar_widget_t2()
      save_widget_png(widget, file, vwidth = 1600, vheight = 900, delay = 1.2, zoom = 2)
    }
  )
  
  output$dl_png_serie_t2 <- downloadHandler(
    filename = function() paste0("SIPSA_tab2_serie_", Sys.Date(), ".png"),
    content  = function(file){
      widget <- build_serie_widget_t2()
      save_widget_png(widget, file, vwidth = 1600, vheight = 900, delay = 1.2, zoom = 2)
    }
  )
  
  # =========================================================
  # HELPERS PDF
  # =========================================================
  filtros_tab1_df <- reactive({
    data.frame(
      Parametro = c("Pestaña", "Departamento foco", "Año", "Mes", "Departamento de destino", "Grupo alimenticio", "Alimento"),
      Valor = c(
        "Remisión desde Atlántico",
        DPTO_FOCO_NOMBRE,
        input$anio_t1 %||% "Todos",
        if (!is.null(input$mes_t1) && input$mes_t1 != "Todos") mes_nombre[as.integer(input$mes_t1)] else "Todos",
        paste(input$territorio_t1 %||% "Todos", collapse = ", "),
        paste(input$grupos_t1 %||% "Todos", collapse = ", "),
        paste(input$alimentos_t1 %||% "Todos", collapse = ", ")
      ),
      stringsAsFactors = FALSE
    )
  })
  
  filtros_tab2_df <- reactive({
    data.frame(
      Parametro = c("Pestaña", "Departamento foco", "Año", "Mes", "Departamento de origen", "Grupo alimenticio", "Alimento"),
      Valor = c(
        "Recepción en Atlántico",
        DPTO_FOCO_NOMBRE,
        input$anio_t2 %||% "Todos",
        if (!is.null(input$mes_t2) && input$mes_t2 != "Todos") mes_nombre[as.integer(input$mes_t2)] else "Todos",
        paste(input$territorio_t2 %||% "Todos", collapse = ", "),
        paste(input$grupos_t2 %||% "Todos", collapse = ", "),
        paste(input$alimentos_t2 %||% "Todos", collapse = ", ")
      ),
      stringsAsFactors = FALSE
    )
  })
  
  generar_insumos_pdf <- function(export_dir){
    dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
    
    img_t1_map   <- file.path(export_dir, "sipsa_tab1_mapa.png")
    img_t1_bar   <- file.path(export_dir, "sipsa_tab1_barras.png")
    img_t1_serie <- file.path(export_dir, "sipsa_tab1_serie.png")
    img_t2_map   <- file.path(export_dir, "sipsa_tab2_mapa.png")
    img_t2_bar   <- file.path(export_dir, "sipsa_tab2_barras.png")
    img_t2_serie <- file.path(export_dir, "sipsa_tab2_serie.png")
    
    save_widget_png(build_map_widget_t1(),   img_t1_map,   vwidth = 1600, vheight = 1100, delay = 2.0, zoom = 2)
    save_widget_png(build_bar_widget_t1(),   img_t1_bar,   vwidth = 1600, vheight = 900,  delay = 1.2, zoom = 2)
    save_widget_png(build_serie_widget_t1(), img_t1_serie, vwidth = 1600, vheight = 900,  delay = 1.2, zoom = 2)
    
    save_widget_png(build_map_widget_t2(),   img_t2_map,   vwidth = 1600, vheight = 1100, delay = 2.0, zoom = 2)
    save_widget_png(build_bar_widget_t2(),   img_t2_bar,   vwidth = 1600, vheight = 900,  delay = 1.2, zoom = 2)
    save_widget_png(build_serie_widget_t2(), img_t2_serie, vwidth = 1600, vheight = 900,  delay = 1.2, zoom = 2)
    
    csv_t1 <- file.path(export_dir, "sipsa_tab1_base_filtrada.csv")
    csv_t2 <- file.path(export_dir, "sipsa_tab2_base_filtrada.csv")
    readr::write_csv(datos_t1(), csv_t1)
    readr::write_csv(datos_t2(), csv_t2)
    
    list(
      img_t1_map = img_t1_map,
      img_t1_bar = img_t1_bar,
      img_t1_serie = img_t1_serie,
      img_t2_map = img_t2_map,
      img_t2_bar = img_t2_bar,
      img_t2_serie = img_t2_serie,
      csv_t1 = csv_t1,
      csv_t2 = csv_t2
    )
  }
  
  output$dl_pdf_report <- downloadHandler(
    filename = function(){
      paste0("Informe_SIPSA_Abastecimiento_", Sys.Date(), ".pdf")
    },
    content = function(file){
      validate(need(both_tabs_seen(), "Debes haber visualizado las dos pestañas antes de descargar el informe PDF."))
      
      rmd_candidates <- c(
        file.path(app_root, "Informe_descargable.Rmd"),
        file.path(data_dir, "Informe_descargable.Rmd")
      )
      rmd_path <- rmd_candidates[file.exists(rmd_candidates)][1]
      
      if (is.na(rmd_path) || !nzchar(rmd_path)) {
        stop("No encontré 'Informe_descargable.Rmd' ni en la raíz de la app ni en la carpeta data/.")
      }
      
      tmp_dir <- tempfile("sipsa_pdf_")
      dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
      
      insumos <- generar_insumos_pdf(tmp_dir)
      
      params_list <- list(
        app_root     = app_root,
        export_dir   = tmp_dir,
        filtros_tab1 = filtros_tab1_df(),
        filtros_tab2 = filtros_tab2_df(),
        img_t1_map   = basename(insumos$img_t1_map),
        img_t1_bar   = basename(insumos$img_t1_bar),
        img_t1_serie = basename(insumos$img_t1_serie),
        img_t2_map   = basename(insumos$img_t2_map),
        img_t2_bar   = basename(insumos$img_t2_bar),
        img_t2_serie = basename(insumos$img_t2_serie)
      )
      
      out_file <- rmarkdown::render(
        input = rmd_path,
        output_file = basename(file),
        output_dir  = dirname(file),
        params = params_list,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
      
      invisible(out_file)
    }
  )
}

shinyApp(ui, server)