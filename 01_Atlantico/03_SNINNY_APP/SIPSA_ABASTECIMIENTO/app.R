# app.R
# =========================================================
# SIPSA ABASTECIMIENTO — MAPA + (BARRAS GRUPOS) + (SERIE TOTAL) + TABLA (2 pestañas)
# (MOD: dropdowns salen de la caja verde + meses con nombres + ✅ SIN filtro de NIVEL)
# (MOD NUEVO: Tab 2 (Recepción) barras + serie se calculan SOLO con recepción en Atlántico)
# (MOD NUEVO: ✅ En mapas SOLO etiqueta al pasar el cursor (hover) y mostrando NOMBRE del dpto)
# =========================================================

# ------------------------------
# 1) Paquetes (NO instalar aquí)
# ------------------------------
pkgs <- c(
  "shiny","bslib","shinyWidgets",
  "dplyr","stringr","janitor","scales",
  "readr","DT","plotly",
  "sf","leaflet","stringi","htmltools",
  "webshot2","htmlwidgets","ragg"
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

# --- Paleta 4 clases tipo HANSEN ---
pal4_vec <- grDevices::colorRampPalette(
  c("#F6E8C3", "#EBD3A6", "#C9A56A", "#9A7547", "#6B4F2C")
)(4)

# ✅ BINS: SOLO con valores > 0 (excluye 0/NA)
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

# ✅ Paleta: dominio SOLO con >0 y NA pintado gris
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

# nombres DESTINO (si existen)
dep_d_col <- pick_first(nms, c("departamento_d","depto_d","departamento_destino","depto_destino"))
mun_d_col <- pick_first(nms, c("municipio_d","mpio_d","municipio_destino","mpio_destino"))

# nombres ORIGEN (si existen)
dep_o_col <- pick_first(nms, c("departamento_o","depto_o","departamento_origen","depto_origen"))
mun_o_col <- pick_first(nms, c("municipio_o","mpio_o","municipio_origen","mpio_origen"))

# códigos DESTINO
dpto_code_d_col <- req_col(
  nms,
  c("cod_dane_dpto_d","dane_cod_dpto_d","dane_cod_dpto","cod_dane_dpto","cod_dpto_d","cod_dpto"),
  "DANE_COD_DPTO_D (código dpto destino)"
)
mpio_code_d_col <- pick_first(nms, c("cod_dane_munic_d","dane_cod_munic_d","cod_dane_mpio_d","cod_dane_mpio","cod_mpio_d"))

# códigos ORIGEN
dpto_code_o_col <- req_col(
  nms,
  c("cod_dane_dpto_o","dane_cod_dpto_o","cod_dpto_o","cod_dpto_origen","cod_dane_dpto_origen","dane_cod_dpto_origen"),
  "COD_DANE_DPTO_O (código dpto origen)"
)
mpio_code_o_col <- pick_first(nms, c("cod_dane_munic_o","dane_cod_munic_o","cod_dane_mpio_o","cod_mpio_o","cod_mpio_origen"))

# =========================================================
# 3) Base estándar (kg interno, UI solo muestra ton)
#   ✅ Filtra 2018+
#   ✅ Elimina filas con DEPARTAMENTO_O en blanco
# =========================================================
base_sipsa <- sipsa %>%
  transmute(
    anio = suppressWarnings(as.integer(.data[[ycol]])),
    mes  = suppressWarnings(as.integer(.data[[mcol]])),
    
    # DESTINO
    cod_dpto_d = pad_dpto(.data[[dpto_code_d_col]]),
    cod_mpio_d = if (!is.na(mpio_code_d_col)) pad_mpio(.data[[mpio_code_d_col]]) else NA_character_,
    departamento_d = if (!is.na(dep_d_col)) title_case_es(.data[[dep_d_col]]) else NA_character_,
    municipio_d    = if (!is.na(mun_d_col)) title_case_es(.data[[mun_d_col]]) else NA_character_,
    
    # ORIGEN
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

# =========================================================
# 3.2) Separar bases por pestaña (FOCO Atlántico)
# =========================================================
# TAB 1: Remisión desde Atlántico (ORIGEN = Atlántico)
base_t1 <- base_sipsa %>% filter(cod_dpto_o == DPTO_FOCO_COD)

# TAB 2: Recepción en Atlántico (DESTINO = Atlántico)
base_t2 <- base_sipsa %>% filter(cod_dpto_d == DPTO_FOCO_COD)

# =========================================================
# 3.4) (NUEVO) Diccionario COD_DPTO -> NOMBRE (desde la base)
#   para asegurar que el hover muestre NOMBRE y no código
# =========================================================
dept_names_lut <- dplyr::bind_rows(
  base_sipsa %>% dplyr::distinct(cod_dpto = cod_dpto_d, dept_name = departamento_d),
  base_sipsa %>% dplyr::distinct(cod_dpto = cod_dpto_o, dept_name = departamento_o)
) %>%
  dplyr::filter(!is.na(cod_dpto), !is.na(dept_name), stringr::str_trim(dept_name) != "") %>%
  dplyr::group_by(cod_dpto) %>%
  dplyr::summarise(dept_name = dplyr::first(dept_name), .groups = "drop")

# =========================================================
# 3.3) Shapefile Departamentos — SOLO desde ./data/shp
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

# ✅ Forzar NOMBRE del departamento usando el diccionario de la base
if (!is.null(dept_sf) && inherits(dept_sf, "sf") && nrow(dept_sf) > 0) {
  dept_sf <- dept_sf %>%
    dplyr::left_join(dept_names_lut, by = "cod_dpto") %>%
    dplyr::mutate(departamento_d = dplyr::coalesce(dept_name, departamento_d)) %>%
    dplyr::select(-dept_name)
}

# =========================================================
# 4) UI  ✅ (CORREGIDO: h2 + tabsetPanel bien cerrados)
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

      /* ✅ Caja verde: permitir que dropdowns salgan */
      .filters{
        background:#fff; border:1px solid var(--accent-border); border-radius:16px;
        padding:14px 16px; margin-bottom:16px; box-shadow:0 4px 14px rgba(0,0,0,.06);
        width:100%;
        display:block;
        overflow: visible;
        position: relative;
        z-index: 20;
      }

      /* ✅ SIN NIVEL: ahora 5 columnas */
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
      .dl-under{ margin-top:8px; text-align:right; }

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
      
      # =====================================================
      # TAB 1 — REMISIÓN DESDE ATLÁNTICO (destinos)
      # =====================================================
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
              div(class="card-title", strong("¿En que departamentos se concentra la mayor cantidad de alimentos remitidos de la central mayorista priorizada?")),
              div(class="viz-body", leafletOutput("map_t1")),
              div(class="map-note","Nota: cuartiles (4 clases) con valores > 0. Dptos sin información en gris."),
              div(class="dl-under", downloadButton("dl_png_map_t1", "PNG — Mapa (simple)"))
            )
          ),
          
          div(
            class = "right-col",
            div(
              class="card viz-card",
              div(class="card-title", strong("¿Qué grupos de alimentos tienen mayor participación?")),
              div(class="viz-body", plotlyOutput("bar_grupos_t1", height="100%"))
            ),
            div(
              class="card viz-card",
              div(class="card-title", strong("¿Cómo es la evolución de abastecimiento de alimentos?")),
              div(class="viz-body", plotlyOutput("serie_total_t1", height="100%"))
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
        )
      ),
      
      # =====================================================
      # TAB 2 — RECEPCIÓN EN ATLÁNTICO (orígenes)
      # =====================================================
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
              div(class="card-title", strong("¿En que departamentos se concentra la mayor cantidad de alimentos recibidos de la central mayorista priorizada?")),
              div(class="viz-body", leafletOutput("map_t2")),
              div(class="map-note","Nota: cuartiles (4 clases) con valores > 0. Dptos sin información en gris."),
              div(class="dl-under", downloadButton("dl_png_map_t2", "PNG — Mapa (simple)"))
            )
          ),
          
          div(
            class = "right-col",
            div(
              class="card viz-card",
              div(class="card-title", strong("¿Qué grupos de alimentos tienen mayor participación?")),
              div(class="viz-body", plotlyOutput("bar_grupos_t2", height="100%"))
            ),
            div(
              class="card viz-card",
              div(class="card-title", strong("¿Cómo es la evolución de abastecimiento de alimentos?")),
              div(class="viz-body", plotlyOutput("serie_total_t2", height="100%"))
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
  
  # ✅ Meses en español (UI), pero valores siguen siendo 1..12
  mes_nombre <- c(
    "Enero","Febrero","Marzo","Abril","Mayo","Junio",
    "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"
  )
  month_choices <- c("Todos" = "Todos", stats::setNames(as.character(months), mes_nombre[months]))
  
  hover_label_opts <- leaflet::labelOptions(
    direction="auto", textsize="12px", sticky=TRUE, opacity=0.95, className="lbl-clean"
  )
  
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
    opts <- sort(unique(na.omit(base_t1$departamento_d)))  # ✅ destinos desde Atlántico
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
    opts <- sort(unique(na.omit(base_t2$departamento_o)))  # ✅ orígenes hacia Atlántico
    pickerInput(
      "territorio_t2", NULL,
      choices  = c("Todos", opts),
      selected = "Todos",
      options  = list(`live-search`=TRUE, size=7)
    )
  })
  
  # =========================================================
  # TAB 1 — Contexto (choices grupo/alimento) ✅ Remisión desde Atlántico
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
  # TAB 2 — Contexto (choices grupo/alimento) ✅ Recepción en Atlántico
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
  # TAB 1 — Datos (DESTINO)
  # =========================================================
  datos_t1 <- reactive({
    df <- ctx_t1()
    df <- filter_multi(df, "grupo",    input$grupos_t1)
    df <- filter_multi(df, "alimento", input$alimentos_t1)
    validate(need(nrow(df) > 0, "Sin datos con los filtros seleccionados (Tab 1)"))
    df
  })
  
  # =========================================================
  # TAB 2 — Datos (ORIGEN)
  # =========================================================
  datos_t2 <- reactive({
    df <- ctx_t2()
    df <- filter_multi(df, "grupo",    input$grupos_t2)
    df <- filter_multi(df, "alimento", input$alimentos_t2)
    validate(need(nrow(df) > 0, "Sin datos con los filtros seleccionados (Tab 2)"))
    df
  })
  
  # =========================================================
  # TAB 1 — MAPA (Destinos) — ✅ SOLO HOVER (sin etiquetas fijas)
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
  
  # =========================================================
  # TAB 2 — MAPA (Orígenes hacia Atlántico) — ✅ SOLO HOVER
  # =========================================================
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
  # TAB 1 — Barras (Grupos) (remisión desde Atlántico)
  # =========================================================
  output$bar_grupos_t1 <- renderPlotly({
    dd <- datos_t1() %>%
      group_by(grupo) %>%
      summarise(ton=sum(ton, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(ton)) %>%
      head(10)
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
      )
  })
  
  # =========================================================
  # TAB 2 — Barras (Grupos) (✅ recepción en Atlántico)
  # =========================================================
  output$bar_grupos_t2 <- renderPlotly({
    dd <- datos_t2() %>%
      group_by(grupo) %>%
      summarise(ton=sum(ton, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(ton)) %>%
      head(10)
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
      )
  })
  
  # =========================================================
  # TAB 1 — Serie Total (ignora filtro de mes) (remisión desde Atlántico)
  # =========================================================
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
  
  output$serie_total_t1 <- renderPlotly({
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
      )
  })
  
  # =========================================================
  # TAB 2 — Serie Total (ignora filtro de mes) (✅ recepción en Atlántico)
  # =========================================================
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
  
  output$serie_total_t2 <- renderPlotly({
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
      )
  })
  
  # =========================================================
  # TAB 1 — Tabla detalle (DESTINO) — ✅ SOLO DEPARTAMENTO
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
  
  # =========================================================
  # TAB 2 — Tabla detalle (ORIGEN) — ✅ SOLO DEPARTAMENTO
  # =========================================================
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
  # PNG — Mapas simples (sin etiquetas fijas; solo polígonos)
  # =========================================================
  output$dl_png_map_t1 <- downloadHandler(
    filename = function() paste0("SIPSA_mapa_tab1_destino_", Sys.Date(), ".png"),
    content  = function(file){
      shp <- mapa_t1_sf()
      widget <- if (is.null(shp)) {
        leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(-74, 4.6, 5)
      } else {
        pal <- palBin4(shp$ton)
        leaflet(shp, options = leafletOptions(zoomControl=FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons(
            fillColor=~ifelse(is.na(ton)|ton<=0,"#bdbdbd",pal(ton)),
            weight=0.7,color="#666",fillOpacity=0.9,
            label=~ifelse(is.na(ton)|ton<=0,
                          sprintf("%s — Sin información", departamento_d),
                          sprintf("%s — %s Ton", departamento_d, fmt_ton_co(ton,1))),
            labelOptions=hover_label_opts
          ) %>%
          addLegend(position="bottomright", pal=pal,
                    values=~ifelse(is.na(ton)|ton<=0, NA, ton),
                    title="Toneladas", labFormat=legend_lab_es(suffix=" Ton"),
                    na.label="Sin información") %>%
          addControl(badge_t1(), position="topright")
      }
      tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget, tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, file = file, vwidth = 1200, vheight = 800, zoom = 2)
    }
  )
  
  output$dl_png_map_t2 <- downloadHandler(
    filename = function() paste0("SIPSA_mapa_tab2_origen_", Sys.Date(), ".png"),
    content  = function(file){
      shp <- mapa_t2_sf()
      widget <- if (is.null(shp)) {
        leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(-74, 4.6, 5)
      } else {
        pal <- palBin4(shp$ton)
        leaflet(shp, options = leafletOptions(zoomControl=FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons(
            fillColor=~ifelse(is.na(ton)|ton<=0,"#bdbdbd",pal(ton)),
            weight=0.7,color="#666",fillOpacity=0.9,
            label=~ifelse(is.na(ton)|ton<=0,
                          sprintf("%s — Sin información", departamento_d),
                          sprintf("%s — %s Ton", departamento_d, fmt_ton_co(ton,1))),
            labelOptions=hover_label_opts
          ) %>%
          addLegend(position="bottomright", pal=pal,
                    values=~ifelse(is.na(ton)|ton<=0, NA, ton),
                    title="Toneladas", labFormat=legend_lab_es(suffix=" Ton"),
                    na.label="Sin información") %>%
          addControl(badge_t2(), position="topright")
      }
      tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget, tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, file = file, vwidth = 1200, vheight = 800, zoom = 2)
    }
  )
}

shinyApp(ui, server)
