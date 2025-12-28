# app.R
# =========================================================
# SIPSA_ABASTECIMIENTO_IMPORTANCIA — 2 pestañas (Storytelling)
# ✅ Cada pestaña contiene: filtros + objetos visuales (sin IDs duplicados)
# =========================================================

DPTO_FOCO_NOMBRE <- "Atlántico"
DPTO_FOCO_COD    <- "08"
APP_TITLE <- ""

# ------------------------------
# Paquetes (NO instalar aquí)
# ------------------------------
pkgs <- c(
  "shiny","bslib","shinyWidgets",
  "dplyr","stringr","janitor","scales",
  "readr","sf","leaflet","stringi","htmltools",
  "webshot2","htmlwidgets"
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
  library(readr); library(sf); library(leaflet); library(stringi); library(htmltools)
  library(webshot2); library(htmlwidgets)
})

options(stringsAsFactors = FALSE, scipen = 999)
sf::sf_use_s2(FALSE)

validate <- shiny::validate
need     <- shiny::need
`%||%`   <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ✅ Blindaje dplyr
select    <- dplyr::select
mutate    <- dplyr::mutate
filter    <- dplyr::filter
summarise <- dplyr::summarise
arrange   <- dplyr::arrange
left_join <- dplyr::left_join

# =========================================================
# Rutas robustas (runApp / source)
# =========================================================
app_root <- tryCatch({
  of <- sys.frame(1)$ofile
  if (!is.null(of)) dirname(normalizePath(of, winslash = "/", mustWork = TRUE))
  else normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}, error = function(e){
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
})

data_dir <- file.path(app_root, "data")

# =========================================================
# Helpers
# =========================================================
title_case_es <- function(x){
  x <- stringr::str_trim(as.character(x))
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  small_words <- c("de","del","la","las","los","y","e","o","u","a","en","el","al","da","do","das","dos")
  vapply(x, function(s){
    if (is.na(s) || s == "") return(NA_character_)
    w <- strsplit(s, "\\s+")[[1]]
    w <- vapply(seq_along(w), function(i){
      if (i > 1 && w[i] %in% small_words) w[i] else stringr::str_to_title(w[i], locale = "es")
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

pad_dpto <- function(x){
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\D", "")
  x <- ifelse(nchar(x) == 0, NA_character_, x)
  stringr::str_pad(x, width = 2, side = "left", pad = "0")
}

sel_is_all <- function(x){
  is.null(x) || length(x) == 0 || any(x == "Todos")
}

filter_multi <- function(df, col, sel){
  if (sel_is_all(sel)) return(df)
  df %>% dplyr::filter(.data[[col]] %in% sel)
}

fmt_ton_co <- function(x, digits = 1){
  scales::number(x, big.mark=".", decimal.mark=",", accuracy = 10^(-digits))
}
fmt_pct_co <- function(x, digits = 1){
  paste0(scales::number(x, big.mark=".", decimal.mark=",", accuracy = 10^(-digits)), "%")
}
format_short <- function(x){
  scales::number(x, accuracy = 0.1, big.mark=".", decimal.mark=",")
}

# =========================================================
# Paletas / bins
# =========================================================
pal4_vec <- grDevices::colorRampPalette(
  c("#F6E8C3", "#EBD3A6", "#C9A56A", "#9A7547", "#6B4F2C")
)(4)

make_bins4 <- function(values){
  v <- as.numeric(values)
  v <- v[is.finite(v) & v > 0]
  if (!length(v)) return(c(0, 1, 2, 3, 4))
  qs <- stats::quantile(v, probs = seq(0, 1, length.out = 5), na.rm = TRUE, type = 7)
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

legend_lab_range <- function(suffix = ""){
  function(type, cuts, p){
    if (length(cuts) <= 1) return(character())
    lows  <- head(cuts, -1)
    highs <- tail(cuts, -1)
    pref <- c("", rep("> ", length(lows) - 1))
    paste0(pref, format_short(lows), suffix, " – ", format_short(highs), suffix)
  }
}

# =========================================================
# ✅ FIX GEOM
# =========================================================
get_geom_col <- function(x){
  g <- attr(x, "sf_column")
  if (!is.null(g) && g %in% names(x) && inherits(x[[g]], "sfc")) return(g)
  g2 <- names(x)[vapply(x, inherits, logical(1), "sfc")]
  if (length(g2)) return(g2[1])
  NA_character_
}

rename_geom_to_geometry <- function(x){
  if (!inherits(x, "sf")) stop("rename_geom_to_geometry: objeto no es sf.")
  geom_nm <- get_geom_col(x)
  if (is.na(geom_nm)) stop("No encontré columna sfc (geometría) en el objeto sf.")
  if (geom_nm == "geometry") {
    sf::st_geometry(x) <- "geometry"
    return(x)
  }
  y <- x
  names(y)[names(y) == geom_nm] <- "geometry"
  sf::st_geometry(y) <- "geometry"
  y
}

# =========================================================
# Cargar RDS base agregada
# =========================================================
rds_candidates <- c(
  file.path(data_dir, "041_DANE_SIPSA-Abast.rds"),
  file.path(data_dir, "datos_agregados.rds"),
  file.path(data_dir, "041_DANE_SIPSA-Abast_volumen.rds"),
  file.path(data_dir, "base_importancia_atlantico.rds")
)
rds_path <- rds_candidates[file.exists(rds_candidates)][1]
if (is.na(rds_path)) {
  stop(
    "No encontré el archivo .rds en /data. Busqué:\n- ",
    paste(basename(rds_candidates), collapse = "\n- "),
    "\n\nColoca tu base (.rds) en: ", data_dir
  )
}

raw <- readRDS(rds_path)
df0 <- janitor::clean_names(raw)
nms <- names(df0)

ycol <- req_col(nms, c("ano","anio","year"), "AÑO")
gcol <- req_col(nms, c("grupo","grupo_alimento","grupo_alimentos"), "GRUPO")

cod_o_col <- req_col(
  nms,
  c("cod_dane_dpto_o","dane_cod_dpto_o","cod_dpto_o","cod_depto_o"),
  "COD_DANE_DPTO_O (ORIGEN)"
)

dep_o_col <- pick_first(nms, c("departamento_o","depto_o","departamento_origen","depto_origen"))

kg_tot_col <- req_col(
  nms,
  c("kg_total_origen","kg_total","kg_origen_total","kg_total_envia","kg_total_salida"),
  "kg_total_origen"
)

kg_atl_col <- req_col(
  nms,
  c("kg_a_atlantico","kg_atlantico","kg_hacia_atlantico","kg_to_atlantico"),
  "kg_a_atlantico"
)

pct_col <- pick_first(nms, c("pct_importancia","porc_importancia","pct","porcentaje"))

base_sipsa <- df0 %>%
  dplyr::transmute(
    anio = suppressWarnings(as.integer(.data[[ycol]])),
    grupo = title_case_es(.data[[gcol]]),
    cod_dpto_o = pad_dpto(.data[[cod_o_col]]),
    departamento_o = if (!is.na(dep_o_col)) title_case_es(.data[[dep_o_col]]) else NA_character_,
    kg_total_origen = parse_num_co(.data[[kg_tot_col]]),
    kg_a_atlantico  = parse_num_co(.data[[kg_atl_col]]),
    pct_importancia = if (!is.na(pct_col)) suppressWarnings(as.numeric(.data[[pct_col]])) else NA_real_
  ) %>%
  dplyr::filter(is.finite(anio), anio >= 2018) %>%
  dplyr::filter(!is.na(cod_dpto_o), cod_dpto_o != "") %>%
  dplyr::filter(is.finite(kg_total_origen), kg_total_origen > 0) %>%
  dplyr::mutate(
    kg_a_atlantico = dplyr::coalesce(kg_a_atlantico, 0),
    pct = ifelse(kg_total_origen > 0, (kg_a_atlantico / kg_total_origen) * 100, NA_real_),
    ton_den = kg_total_origen / 1000,
    ton_num = kg_a_atlantico  / 1000
  ) %>%
  dplyr::group_by(anio, grupo, cod_dpto_o) %>%
  dplyr::summarise(
    departamento_o = dplyr::first(na.omit(departamento_o)) %||% NA_character_,
    ton_den = sum(ton_den, na.rm = TRUE),
    ton_num = sum(ton_num, na.rm = TRUE),
    pct = ifelse(ton_den > 0, (ton_num / ton_den) * 100, NA_real_),
    .groups = "drop"
  )

# =========================================================
# Cargar shapefile de departamentos
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
  obj <- rename_geom_to_geometry(obj)
  
  ccol <- req_col(
    names(obj),
    c("dpto_ccgeo","dpto_ccdgo","cod_depto","cod_dpto","codigo_depto","dpto_cod"),
    "DPTO_CCGEO (o DPTO_CCDGO)"
  )
  ncol <- pick_first(names(obj), c("dpto_cnmbr","departamento","nom_dpto","name_1","name","dpto"))
  
  if (is.na(sf::st_crs(obj))) sf::st_crs(obj) <- 4326
  
  out <- obj %>%
    dplyr::transmute(
      cod_dpto = pad_dpto(.data[[ccol]]),
      dpto_nm  = if (!is.na(ncol)) title_case_es(.data[[ncol]]) else pad_dpto(.data[[ccol]]),
      geometry = geometry
    ) %>%
    sf::st_make_valid() %>%
    sf::st_zm(drop = TRUE, what = "ZM") %>%
    sf::st_transform(3116) %>%
    dplyr::group_by(cod_dpto) %>%
    dplyr::summarise(dpto_nm = dplyr::first(dpto_nm), geometry = sf::st_union(geometry), .groups="drop") %>%
    sf::st_transform(4326)
  
  out
}
dept_sf <- load_dept_sf_only(data_dir)

# =========================================================
# TAB 2 — localizar .gpkg(s) y leer capas
# =========================================================
list_gpkgs <- function(data_dir){
  cand_dirs <- unique(c(file.path(data_dir, "shp"), data_dir))
  gp <- unlist(lapply(cand_dirs, function(dd){
    if (!dir.exists(dd)) return(character(0))
    list.files(dd, pattern="\\.gpkg$", full.names=TRUE, ignore.case=TRUE)
  }))
  gp <- gp[file.exists(gp)]
  gp
}

gpkg_files <- list_gpkgs(data_dir)
prefer_name <- "rutas_ORS_barranquilla_capitales_con_contexto.gpkg"
gpkg_default <- gpkg_files[basename(gpkg_files) == prefer_name][1]
if (is.na(gpkg_default) && length(gpkg_files)) gpkg_default <- gpkg_files[1]

read_gpkg_layers <- function(gpkg_path){
  if (is.null(gpkg_path) || is.na(gpkg_path) || !file.exists(gpkg_path)) return(list(
    rutas=NULL, dptos=NULL, caps=NULL, layers=character(0), path=gpkg_path
  ))
  
  lyr <- sf::st_layers(gpkg_path)$name
  
  get_layer <- function(name){
    if (!(name %in% lyr)) return(NULL)
    x <- sf::st_read(gpkg_path, layer = name, quiet = TRUE)
    janitor::clean_names(x)
  }
  
  rutas <- get_layer("rutas_baq_a_capitales")
  dptos <- get_layer("departamentos")
  caps  <- get_layer("capitales")
  
  list(rutas=rutas, dptos=dptos, caps=caps, layers=lyr, path=gpkg_path)
}

normalize_routes <- function(rutas){
  if (is.null(rutas) || !inherits(rutas,"sf") || nrow(rutas) == 0) return(NULL)
  
  rutas <- janitor::clean_names(rutas)
  rutas <- rename_geom_to_geometry(rutas)
  
  rid <- pick_first(names(rutas), c("dpto_id","depto_id","cod_dpto","dpto","dpto_ccdgo","dpto_ccgeo"))
  if (is.na(rid)) rid <- NA_character_
  
  if (is.na(sf::st_crs(rutas))) sf::st_crs(rutas) <- 4326
  crs0 <- sf::st_crs(rutas)
  
  out <- rutas %>%
    dplyr::mutate(dpto_id = if (!is.na(rid)) pad_dpto(.data[[rid]]) else NA_character_) %>%
    dplyr::select(dpto_id, geometry)
  
  sf::st_geometry(out) <- "geometry"
  sf::st_crs(out) <- crs0
  
  out %>%
    sf::st_make_valid() %>%
    sf::st_zm(drop=TRUE, what="ZM") %>%
    { suppressWarnings(sf::st_cast(., "MULTILINESTRING", warn = FALSE)) } %>%
    sf::st_transform(4326)
}

normalize_dptos <- function(dptos){
  if (is.null(dptos) || !inherits(dptos,"sf") || nrow(dptos) == 0) return(NULL)
  
  dptos <- janitor::clean_names(dptos)
  dptos <- rename_geom_to_geometry(dptos)
  
  ccol <- pick_first(names(dptos), c("depto_id","cod_dpto","dpto_id","dpto_ccdgo","dpto_ccgeo"))
  ncol <- pick_first(names(dptos), c("depto","dpto_nm","departamento","dpto_cnmbr","nombre"))
  if (is.na(ccol)) return(NULL)
  
  if (is.na(sf::st_crs(dptos))) sf::st_crs(dptos) <- 4326
  crs0 <- sf::st_crs(dptos)
  
  out <- dptos %>%
    dplyr::transmute(
      cod_dpto = pad_dpto(.data[[ccol]]),
      dpto_nm  = if (!is.na(ncol)) title_case_es(.data[[ncol]]) else NA_character_,
      geometry = geometry
    )
  
  sf::st_geometry(out) <- "geometry"
  sf::st_crs(out) <- crs0
  
  out %>%
    sf::st_make_valid() %>%
    sf::st_zm(drop = TRUE, what = "ZM") %>%
    sf::st_transform(3116) %>%
    dplyr::group_by(cod_dpto) %>%
    dplyr::summarise(dpto_nm = dplyr::first(dpto_nm), geometry = sf::st_union(geometry), .groups="drop") %>%
    sf::st_transform(4326)
}

normalize_caps <- function(caps){
  if (is.null(caps) || !inherits(caps,"sf") || nrow(caps) == 0) return(NULL)
  
  caps <- janitor::clean_names(caps)
  caps <- rename_geom_to_geometry(caps)
  
  ccol <- pick_first(names(caps), c("depto_id","cod_dpto","dpto_id","dpto_ccdgo","dpto_ccgeo"))
  ncol <- pick_first(names(caps), c("depto","departamento","dpto_nm","dpto_cnmbr","name"))
  mcol <- pick_first(names(caps), c("mpio","municipio","mpio_cnmbr","nombre"))
  
  if (is.na(sf::st_crs(caps))) sf::st_crs(caps) <- 4326
  crs0 <- sf::st_crs(caps)
  
  out <- caps %>%
    dplyr::transmute(
      cod_dpto = if (!is.na(ccol)) pad_dpto(.data[[ccol]]) else NA_character_,
      dpto_nm  = if (!is.na(ncol)) title_case_es(.data[[ncol]]) else NA_character_,
      mpio_nm  = if (!is.na(mcol)) title_case_es(.data[[mcol]]) else NA_character_,
      geometry = geometry
    )
  
  sf::st_geometry(out) <- "geometry"
  sf::st_crs(out) <- crs0
  
  out %>%
    sf::st_make_valid() %>%
    sf::st_zm(drop=TRUE, what="ZM") %>%
    sf::st_transform(4326)
}

# =========================================================
# UI
# =========================================================
filters_box <- function(tag){
  # tag: "t1" o "t2" -> IDs únicos por pestaña
  id_anio   <- paste0("anio_ui_", tag)
  id_grupos <- paste0("grupos_ui_", tag)
  id_dptos  <- paste0("dptos_ui_", tag)
  
  div(
    class="filters",
    div(
      class="filters-grid",
      div(class="filter", div(class="filter-label","¿Qué año analizamos?"), uiOutput(id_anio)),
      div(class="filter", div(class="filter-label","¿Qué grupo alimenticio estás mirando?"), uiOutput(id_grupos)),
      div(class="filter", div(class="filter-label","¿En qué departamento?"), uiOutput(id_dptos))
    )
  )
}

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
      :root{ --accent-border:#99d5ec; --map-h: 600px; }
      body{ background:#ffffff; }
      .wrap{ max-width:1360px; margin:0 auto; padding:16px 20px 32px; }
      h2#app-title{ text-align:center; margin-top:10px; margin-bottom:10px; font-weight:800; letter-spacing:.3px; }

      .filters{
        background:#fff; border:1px solid var(--accent-border); border-radius:16px;
        padding:14px 16px; margin-bottom:12px; box-shadow:0 4px 14px rgba(0,0,0,.06);
        width:100%; overflow: visible; position: relative; z-index: 20;
      }
      .filters-grid{
        width:100%;
        display:grid;
        grid-template-columns: repeat(3, minmax(240px, 1fr));
        column-gap:16px; row-gap:10px; align-items:end;
      }
      @media (max-width: 900px){ .filters-grid{ grid-template-columns: repeat(2, minmax(240px, 1fr)); } }
      @media (max-width: 650px){ .filters-grid{ grid-template-columns: 1fr; } }

      .filter-label{
        font-weight:800; font-size:13px; margin-bottom:6px; color:#111827;
        white-space: normal; line-height: 1.15; min-height: 28px;
      }

      .form-select, .bootstrap-select > .dropdown-toggle, .selectize-input{
        border:1px solid var(--accent-border) !important;
        border-radius:10px !important;
        box-shadow:none !important;
        font-size:14px; font-weight:600; color:#000;
        background-color:#fff !important;
        min-height:42px;
        width:100% !important;
      }
      .bootstrap-select .dropdown-menu{ z-index: 99999 !important; }
      .selectize-dropdown{ z-index: 99999 !important; }
      .leaflet-container{ z-index: 1 !important; }

      .card{
        background:#fff; border:1px solid var(--accent-border) !important;
        border-radius:16px; padding:12px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
      }
      .card-title{ font-weight:900; font-size:16px; margin-bottom:8px; color:#111827; }

      #map_imp{ height: var(--map-h) !important; }
      #map_routes{ height: var(--map-h) !important; }

      .leaflet-tooltip.lbl-clean{
        background: rgba(255,255,255,.92); border: 1px solid #e6e6e6; border-radius: 6px;
        padding: 4px 6px; color: #222; font-weight: 800; box-shadow: 0 1px 4px rgba(0,0,0,.08);
      }
      .dl-under{ margin-top:8px; text-align:right; }

      .sel-row{ display:flex; gap:10px; flex-wrap:wrap; align-items:center; justify-content:space-between; margin:6px 0 2px; }
      .sel-chip{
        display:inline-block; padding:6px 10px; border-radius:999px;
        border:1px solid #e5e7eb; background:#fff; font-size:12px; color:#111827; font-weight:800;
      }
      .btn-clear{ border-radius:10px; }
    "))
  ),
  
  div(
    class="wrap",
    h2(APP_TITLE, id="app-title"),
    
    tabsetPanel(
      type = "tabs",
      id   = "tabs",
      
      tabPanel(
        "¿Cuánto de lo que envía cada departamento llega a Atlántico?",
        # ✅ filtros dentro de la pestaña
        filters_box("t1"),
        div(
          class="card",
          div(class="card-title",
              strong("¿Cuáles departamentos envían la mayor cantidad de alimentos al departamento priorizado?")),
          leafletOutput("map_imp"),
          div(class="dl-under", downloadButton("dl_png_map_imp", "PNG — Mapa (simple)"))
        )
      ),
      
      tabPanel(
        "¿Cuáles son corredores donde se mueve el abastecimiento hacia Atlántico?",
        # ✅ filtros dentro de la pestaña
        filters_box("t2"),
        div(
          class="card",
          div(class="card-title",
              strong("¿Cuáles son los corredores de volúmenes de envío de alimentos al departamento?")),
          uiOutput("sel_routes_ui"),
          leafletOutput("map_routes"),
          div(class="dl-under", downloadButton("dl_png_map_routes", "PNG — Mapa (rutas)"))
        )
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  
  hover_label_opts <- leaflet::labelOptions(
    direction="auto", textsize="12px", sticky=TRUE, opacity=0.95, className="lbl-clean"
  )
  
  years <- sort(unique(base_sipsa$anio[is.finite(base_sipsa$anio)]))
  
  # ------------------------
  # UI filtros TAB 1 (IDs únicos)
  # ------------------------
  output$anio_ui_t1 <- renderUI({
    selectInput("anio_t1", NULL, choices = c("Todos"="Todos", years),
                selected = if (length(years)) max(years) else "Todos")
  })
  
  base_all_t1 <- reactive({
    df <- base_sipsa
    if (!is.null(input$anio_t1) && input$anio_t1 != "Todos") {
      df <- df %>% dplyr::filter(anio == as.integer(input$anio_t1))
    }
    df
  })
  
  output$grupos_ui_t1 <- renderUI({
    grupos <- sort(unique(na.omit(base_all_t1()$grupo)))
    pickerInput(
      "grupos_t1", NULL,
      choices  = c("Todos", grupos),
      selected = "Todos",
      multiple = TRUE,
      options  = list(`live-search`=TRUE, `actions-box`=TRUE, size=8)
    )
  })
  
  output$dptos_ui_t1 <- renderUI({
    dpts <- dept_sf %>% sf::st_drop_geometry() %>% dplyr::arrange(dpto_nm)
    choices <- c("Todos" = "Todos")
    choices <- c(choices, stats::setNames(dpts$cod_dpto, dpts$dpto_nm))
    
    pickerInput(
      "dptos_t1", NULL,
      choices  = choices,
      selected = "Todos",
      multiple = TRUE,
      options  = list(`live-search`=TRUE, `actions-box`=TRUE, size=10)
    )
  })
  
  datos_filtrados_t1 <- reactive({
    df <- base_all_t1()
    df <- filter_multi(df, "grupo", input$grupos_t1)
    
    if (!sel_is_all(input$dptos_t1)) {
      sel_codes <- unique(as.character(input$dptos_t1))
      sel_codes <- sel_codes[sel_codes != "Todos"]
      df <- df %>% dplyr::filter(cod_dpto_o %in% sel_codes)
    }
    
    validate(need(nrow(df) > 0, "Sin datos con los filtros seleccionados."))
    df
  })
  
  badge_t1 <- reactive({
    yr <- if (!is.null(input$anio_t1) && input$anio_t1 != "Todos") as.character(input$anio_t1) else "Todos"
    htmltools::HTML(sprintf(
      '<div style="background:#fff;padding:6px 10px;border-radius:8px;
                   box-shadow:0 1px 6px rgba(0,0,0,.15);font-size:12px;line-height:1.3;">
         <b>Destino:</b> %s<br>
         <b>Año:</b> %s
       </div>', htmltools::htmlEscape(DPTO_FOCO_NOMBRE), yr
    ))
  })
  
  # ---------------- TAB 1: Mapa importancia ----------------
  mapa_imp_sf <- reactive({
    agg <- datos_filtrados_t1() %>%
      dplyr::group_by(cod_dpto_o) %>%
      dplyr::summarise(
        ton_den = sum(ton_den, na.rm = TRUE),
        ton_num = sum(ton_num, na.rm = TRUE),
        pct = ifelse(ton_den > 0, (ton_num / ton_den) * 100, NA_real_),
        .groups = "drop"
      ) %>%
      dplyr::filter(cod_dpto_o %in% dept_sf$cod_dpto)
    
    out <- dept_sf %>%
      dplyr::left_join(agg, by = c("cod_dpto" = "cod_dpto_o")) %>%
      dplyr::mutate(
        ton_num = as.numeric(ton_num),
        ton_den = as.numeric(ton_den),
        pct     = as.numeric(pct)
      )
    
    out$pct[out$cod_dpto == DPTO_FOCO_COD] <- NA_real_
    out$ton_num[out$cod_dpto == DPTO_FOCO_COD] <- NA_real_
    out$ton_den[out$cod_dpto == DPTO_FOCO_COD] <- NA_real_
    out
  })
  
  output$map_imp <- renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::setView(lng = -74.0, lat = 4.6, zoom = 5)
  })
  outputOptions(output, "map_imp", suspendWhenHidden = FALSE)
  
  draw_map_imp <- function(){
    mdat <- tryCatch(mapa_imp_sf(), error = function(e) NULL)
    if (is.null(mdat) || !inherits(mdat,"sf") || nrow(mdat) == 0) return(invisible(NULL))
    
    mdat <- mdat %>% sf::st_make_valid() %>% sf::st_zm(drop=TRUE, what="ZM")
    mdat <- tryCatch(sf::st_cast(mdat, "MULTIPOLYGON", warn=FALSE), error=function(e) mdat)
    mdat <- mdat[!sf::st_is_empty(mdat$geometry), , drop=FALSE]
    if (nrow(mdat) == 0) return(invisible(NULL))
    
    mdat$pct_plot <- ifelse(is.na(mdat$pct) | mdat$pct <= 0, NA_real_, as.numeric(mdat$pct))
    pal <- palBin4(mdat$pct_plot)
    bb  <- sf::st_bbox(mdat)
    
    leaflet::leafletProxy("map_imp", data = mdat) %>%
      leaflet::clearGroup("poly") %>%
      leaflet::clearControls() %>%
      leaflet::addPolygons(
        group="poly",
        fillColor = ~ifelse(is.na(pct_plot), "#bdbdbd", pal(pct_plot)),
        fillOpacity = 0.90,
        color = "#666",
        weight = 0.7,
        label = ~ifelse(
          is.na(pct_plot),
          sprintf("%s — 0%% / NA", dpto_nm),
          sprintf("%s — %s", dpto_nm, fmt_pct_co(pct_plot, 1))
        ),
        popup = ~paste0(
          "<strong>Departamento (origen): </strong>", dpto_nm,
          ifelse(is.na(pct_plot), "", paste0("<br><strong>% hacia ", DPTO_FOCO_NOMBRE, ":</strong> ", fmt_pct_co(pct_plot, 1))),
          ifelse(is.na(ton_num), "", paste0("<br><strong>Ton hacia ", DPTO_FOCO_NOMBRE, ":</strong> ", fmt_ton_co(ton_num, 1))),
          ifelse(is.na(ton_den), "", paste0("<br><strong>Total enviado por el origen:</strong> ", fmt_ton_co(ton_den, 1)))
        ),
        highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
        labelOptions = hover_label_opts
      ) %>%
      leaflet::addLegend(
        position="bottomright",
        pal=pal,
        values=mdat$pct_plot,
        title=paste0("% de envíos a ", DPTO_FOCO_NOMBRE),
        labFormat=legend_lab_range("%"),
        na.label="0% / NA"
      ) %>%
      leaflet::addControl(badge_t1(), position="topright") %>%
      leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  }
  
  observeEvent(list(input$anio_t1, input$grupos_t1, input$dptos_t1), { draw_map_imp() }, ignoreInit = FALSE)
  
  output$dl_png_map_imp <- downloadHandler(
    filename = function() paste0("SIPSA_importancia_", DPTO_FOCO_NOMBRE, "_", Sys.Date(), ".png"),
    content  = function(file){
      shp <- isolate(mapa_imp_sf())
      bdg <- isolate(badge_t1())
      
      if (is.null(shp) || !inherits(shp,"sf") || nrow(shp) == 0) {
        widget <- leaflet::leaflet() %>%
          leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
          leaflet::setView(-74.0, 4.6, 5)
      } else {
        shp$pct_plot <- ifelse(is.na(shp$pct) | shp$pct <= 0, NA_real_, as.numeric(shp$pct))
        pal <- palBin4(shp$pct_plot)
        
        widget <- leaflet::leaflet(shp, options = leaflet::leafletOptions(zoomControl=FALSE)) %>%
          leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
          leaflet::addPolygons(
            fillColor = ~ifelse(is.na(pct_plot), "#bdbdbd", pal(pct_plot)),
            fillOpacity = 0.90,
            color = "#666",
            weight = 0.7
          ) %>%
          leaflet::addLegend(
            position="bottomright",
            pal=pal,
            values=shp$pct_plot,
            title=paste0("% de envíos a ", DPTO_FOCO_NOMBRE),
            labFormat=legend_lab_range("%"),
            na.label="0% / NA"
          ) %>%
          leaflet::addControl(bdg, position="topright")
      }
      
      tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget, tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, file = file, vwidth = 1200, vheight = 820, zoom = 2)
    }
  )
  
  # ------------------------
  # UI filtros TAB 2 (IDs únicos)
  # ------------------------
  output$anio_ui_t2 <- renderUI({
    selectInput("anio_t2", NULL, choices = c("Todos"="Todos", years),
                selected = if (length(years)) max(years) else "Todos")
  })
  
  base_all_t2 <- reactive({
    df <- base_sipsa
    if (!is.null(input$anio_t2) && input$anio_t2 != "Todos") {
      df <- df %>% dplyr::filter(anio == as.integer(input$anio_t2))
    }
    df
  })
  
  output$grupos_ui_t2 <- renderUI({
    grupos <- sort(unique(na.omit(base_all_t2()$grupo)))
    pickerInput(
      "grupos_t2", NULL,
      choices  = c("Todos", grupos),
      selected = "Todos",
      multiple = TRUE,
      options  = list(`live-search`=TRUE, `actions-box`=TRUE, size=8)
    )
  })
  
  output$dptos_ui_t2 <- renderUI({
    dpts <- dept_sf %>% sf::st_drop_geometry() %>% dplyr::arrange(dpto_nm)
    choices <- c("Todos" = "Todos")
    choices <- c(choices, stats::setNames(dpts$cod_dpto, dpts$dpto_nm))
    
    pickerInput(
      "dptos_t2", NULL,
      choices  = choices,
      selected = "Todos",
      multiple = TRUE,
      options  = list(`live-search`=TRUE, `actions-box`=TRUE, size=10)
    )
  })
  
  datos_filtrados_t2 <- reactive({
    df <- base_all_t2()
    df <- filter_multi(df, "grupo", input$grupos_t2)
    
    if (!sel_is_all(input$dptos_t2)) {
      sel_codes <- unique(as.character(input$dptos_t2))
      sel_codes <- sel_codes[sel_codes != "Todos"]
      df <- df %>% dplyr::filter(cod_dpto_o %in% sel_codes)
    }
    
    validate(need(nrow(df) > 0, "Sin datos con los filtros seleccionados."))
    df
  })
  
  badge_t2 <- reactive({
    yr <- if (!is.null(input$anio_t2) && input$anio_t2 != "Todos") as.character(input$anio_t2) else "Todos"
    htmltools::HTML(sprintf(
      '<div style="background:#fff;padding:6px 10px;border-radius:8px;
                   box-shadow:0 1px 6px rgba(0,0,0,.15);font-size:12px;line-height:1.3;">
         <b>Destino:</b> %s<br>
         <b>Año:</b> %s
       </div>', htmltools::htmlEscape(DPTO_FOCO_NOMBRE), yr
    ))
  })
  
  # ---------------- TAB 2: rutas ----------------
  gpkg_path_reactive <- reactive({
    if (length(gpkg_files) == 0) return(NA_character_)
    if (!is.na(gpkg_default) && file.exists(gpkg_default)) return(gpkg_default)
    gpkg_files[1]
  })
  
  gpkg_ctx <- reactive({ read_gpkg_layers(gpkg_path_reactive()) })
  rutas_sf <- reactive({ normalize_routes(gpkg_ctx()$rutas) })
  
  dptos_ctx_sf <- reactive({
    dpt <- normalize_dptos(gpkg_ctx()$dptos)
    if (is.null(dpt) || !inherits(dpt,"sf") || nrow(dpt) == 0) dept_sf else dpt
  })
  
  caps_sf <- reactive({ normalize_caps(gpkg_ctx()$caps) })
  
  ton_by_dpto_t2 <- reactive({
    df <- datos_filtrados_t2() %>%
      dplyr::group_by(cod_dpto_o) %>%
      dplyr::summarise(
        ton_to_atl = sum(ton_num, na.rm = TRUE),
        ton_total  = sum(ton_den, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(cod_dpto_o = pad_dpto(cod_dpto_o))
    
    df$ton_to_atl[df$cod_dpto_o == DPTO_FOCO_COD] <- NA_real_
    df$ton_total[df$cod_dpto_o == DPTO_FOCO_COD]  <- NA_real_
    df
  })
  
  dept_names_tbl <- reactive({
    dept_sf %>% sf::st_drop_geometry() %>%
      dplyr::select(cod_dpto, dpto_nm) %>%
      dplyr::distinct()
  })
  
  rutas_con_ton <- reactive({
    rts <- rutas_sf()
    validate(need(!is.null(rts) && inherits(rts,"sf") && nrow(rts) > 0,
                  "No hay capa 'rutas_baq_a_capitales' en el .gpkg o está vacía."))
    
    out <- rts %>%
      dplyr::mutate(.row_id = dplyr::row_number()) %>%
      dplyr::left_join(ton_by_dpto_t2(),     by = c("dpto_id" = "cod_dpto_o")) %>%
      dplyr::left_join(dept_names_tbl(),     by = c("dpto_id" = "cod_dpto")) %>%
      dplyr::mutate(
        ton_to_atl = as.numeric(ton_to_atl),
        ton_total  = as.numeric(ton_total),
        ton_route  = ifelse(is.na(ton_to_atl) | ton_to_atl < 0, 0, ton_to_atl),
        dpto_show  = dplyr::if_else(is.na(dpto_nm) | dpto_nm == "",
                                    paste0("Dpto ", dpto_id),
                                    dpto_nm),
        route_id = paste0(dpto_id, "_", .row_id)
      ) %>%
      dplyr::select(route_id, dpto_id, dpto_show, ton_route, ton_total, geometry)
    
    if (!sel_is_all(input$dptos_t2)) {
      sel_codes <- unique(as.character(input$dptos_t2))
      sel_codes <- sel_codes[sel_codes != "Todos"]
      out <- out %>% dplyr::filter(dpto_id %in% sel_codes)
    }
    
    out
  })
  
  selected_routes <- reactiveVal(character(0))
  
  observe({
    rts <- tryCatch(rutas_con_ton(), error = function(e) NULL)
    if (is.null(rts) || nrow(rts) == 0) {
      selected_routes(character(0))
    } else {
      cur <- selected_routes()
      keep <- intersect(cur, rts$route_id)
      if (!identical(cur, keep)) selected_routes(keep)
    }
  })
  
  observeEvent(input$map_routes_shape_click, {
    click <- input$map_routes_shape_click
    req(click$id)
    cur <- selected_routes()
    id  <- as.character(click$id)
    if (id %in% cur) selected_routes(setdiff(cur, id)) else selected_routes(c(cur, id))
  })
  
  observeEvent(input$clear_routes, { selected_routes(character(0)) })
  
  output$sel_routes_ui <- renderUI({
    if (length(gpkg_files) == 0) return(NULL)
    n <- length(selected_routes())
    div(
      class="sel-row",
      div(class="sel-chip",
          if (n == 0) "Selección de rutas: ninguna (mostrando todas)"
          else paste0("Rutas seleccionadas: ", n, " (clic para quitar/agregar)")),
      actionButton("clear_routes", "Limpiar selección", class="btn btn-outline-secondary btn-clear")
    )
  })
  
  output$map_routes <- renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::setView(lng = -74.0, lat = 4.6, zoom = 5)
  })
  outputOptions(output, "map_routes", suspendWhenHidden = FALSE)
  
  draw_map_routes <- function(){
    if (length(gpkg_files) == 0) return(invisible(NULL))
    
    dpt <- tryCatch(dptos_ctx_sf(), error = function(e) NULL)
    rts <- tryCatch(rutas_con_ton(), error = function(e) NULL)
    cps <- tryCatch(caps_sf(), error = function(e) NULL)
    
    if (is.null(dpt) || !inherits(dpt,"sf") || nrow(dpt) == 0) return(invisible(NULL))
    
    dpt <- dpt %>% sf::st_make_valid() %>% sf::st_zm(drop=TRUE, what="ZM")
    dpt <- tryCatch(sf::st_cast(dpt, "MULTIPOLYGON", warn=FALSE), error=function(e) dpt)
    
    name_map <- dept_sf %>% sf::st_drop_geometry() %>%
      dplyr::select(cod_dpto, dpto_nm_shp = dpto_nm) %>% dplyr::distinct()
    
    dpt2 <- dpt %>%
      dplyr::left_join(name_map, by = "cod_dpto") %>%
      dplyr::mutate(
        dpto_nm_lbl = dplyr::coalesce(dpto_nm_shp, dpto_nm),
        dpto_nm_lbl = title_case_es(dpto_nm_lbl),
        dpto_nm_lbl = ifelse(is.na(dpto_nm_lbl) | dpto_nm_lbl == "", paste0("Dpto ", cod_dpto), dpto_nm_lbl)
      )
    
    if (!sel_is_all(input$dptos_t2)) {
      sel_codes <- unique(as.character(input$dptos_t2))
      sel_codes <- sel_codes[sel_codes != "Todos"]
      dpt2 <- dpt2 %>% dplyr::filter(cod_dpto %in% sel_codes | cod_dpto == DPTO_FOCO_COD)
      if (nrow(dpt2) == 0) dpt2 <- dpt
    }
    
    bb <- sf::st_bbox(dpt2)
    
    proxy <- leaflet::leafletProxy("map_routes") %>%
      leaflet::clearGroup("dpt") %>%
      leaflet::clearGroup("rts") %>%
      leaflet::clearGroup("caps") %>%
      leaflet::clearControls() %>%
      leaflet::addPolygons(
        data = dpt2,
        group="dpt",
        fillColor   = "#d1d5db",
        fillOpacity = 0.45,
        color       = "#9ca3af",
        weight      = 0.8,
        label       = ~dpto_nm_lbl,
        labelOptions = hover_label_opts
      ) %>%
      leaflet::addControl(badge_t2(), position="topright")
    
    if (is.null(rts) || !inherits(rts,"sf") || nrow(rts) == 0) {
      proxy %>% leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      return(invisible(NULL))
    }
    
    rts <- rts %>% sf::st_make_valid() %>% sf::st_zm(drop=TRUE, what="ZM")
    rts <- tryCatch(sf::st_cast(rts, "MULTILINESTRING", warn=FALSE), error=function(e) rts)
    rts <- rts[!sf::st_is_empty(rts$geometry), , drop=FALSE]
    if (nrow(rts) == 0) {
      proxy %>% leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      return(invisible(NULL))
    }
    
    w <- rts$ton_route
    w <- ifelse(!is.finite(w) | w <= 0, NA_real_, w)
    if (all(is.na(w))) {
      rts$w <- 2
    } else {
      rts$w <- scales::rescale(w, to = c(1.2, 7), from = range(w, na.rm = TRUE))
      rts$w[is.na(rts$w)] <- 1.2
    }
    
    sel <- selected_routes()
    rts$is_sel <- if (length(sel) == 0) TRUE else (rts$route_id %in% sel)
    any_sel <- length(sel) > 0
    rts$line_op <- ifelse(rts$is_sel, 0.90, ifelse(any_sel, 0.18, 0.70))
    rts$line_w  <- ifelse(rts$is_sel, rts$w * 1.25, ifelse(any_sel, pmax(1.2, rts$w * 0.75), rts$w))
    
    pal_gy <- grDevices::colorRampPalette(
      c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
    )(4)
    
    vals <- rts$ton_route
    vals <- vals[is.finite(vals) & vals > 0]
    bins <- make_bins4(vals)
    
    pal_routes <- leaflet::colorBin(
      palette  = pal_gy,
      bins     = bins,
      domain   = vals,
      na.color = "#6b7280",
      right    = FALSE
    )
    
    proxy %>%
      leaflet::addPolylines(
        data = rts,
        group="rts",
        layerId = ~route_id,
        color   = ~ifelse(is.na(ton_route) | ton_route <= 0, "#6b7280", pal_routes(ton_route)),
        weight  = ~line_w,
        opacity = ~line_op,
        label = ~paste0(dpto_show, " — ", fmt_ton_co(ton_route, 1), " Ton"),
        labelOptions = hover_label_opts,
        popup = ~paste0(
          "<strong>Ruta BAQ → capital (Departamento):</strong> ", dpto_show,
          "<br><strong>Ton hacia ", DPTO_FOCO_NOMBRE, " (asignada):</strong> ", fmt_ton_co(ton_route, 1),
          ifelse(is.na(ton_total), "", paste0("<br><strong>Total enviado por el dpto (den):</strong> ", fmt_ton_co(ton_total, 1))),
          "<br><small>Tip: clic para seleccionar/deseleccionar.</small>"
        )
      ) %>%
      leaflet::addLegend(
        position="bottomright",
        pal=pal_routes,
        values=rts$ton_route,
        title=paste0("Ton por ruta (→ ", DPTO_FOCO_NOMBRE, ")"),
        labFormat=legend_lab_range(""),
        na.label="0 / NA"
      ) %>%
      leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    
    if (!is.null(cps) && inherits(cps,"sf") && nrow(cps) > 0) {
      cps <- cps %>% sf::st_make_valid() %>% sf::st_zm(drop=TRUE, what="ZM")
      leaflet::leafletProxy("map_routes") %>%
        leaflet::addCircleMarkers(
          data = cps,
          group="caps",
          radius = 3,
          stroke = TRUE,
          weight = 1,
          color = "#0f172a",
          fillOpacity = 0.9,
          label = ~ifelse(is.na(mpio_nm) | mpio_nm == "", "Capital", mpio_nm),
          labelOptions = hover_label_opts,
          popup = ~paste0(
            "<strong>Capital:</strong> ", mpio_nm,
            ifelse(is.na(dpto_nm), "", paste0("<br><strong>Departamento:</strong> ", dpto_nm))
          )
        )
    }
    
    invisible(NULL)
  }
  
  observeEvent(list(input$anio_t2, input$grupos_t2, input$dptos_t2, selected_routes()), {
    draw_map_routes()
  }, ignoreInit = FALSE)
  
  output$dl_png_map_routes <- downloadHandler(
    filename = function() paste0("SIPSA_rutas_corredor_", DPTO_FOCO_NOMBRE, "_", Sys.Date(), ".png"),
    content  = function(file){
      if (length(gpkg_files) == 0) {
        widget <- leaflet::leaflet() %>%
          leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
          leaflet::setView(-74.0, 4.6, 5)
      } else {
        dpt <- isolate(dptos_ctx_sf())
        rts <- isolate(rutas_con_ton())
        bdg <- isolate(badge_t2())
        sel <- isolate(selected_routes())
        if (length(sel) > 0) rts <- rts %>% dplyr::filter(route_id %in% sel)
        
        if (is.null(dpt) || !inherits(dpt,"sf") || nrow(dpt) == 0 ||
            is.null(rts) || !inherits(rts,"sf") || nrow(rts) == 0) {
          widget <- leaflet::leaflet() %>%
            leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
            leaflet::setView(-74.0, 4.6, 5)
        } else {
          dpt <- dpt %>% sf::st_make_valid() %>% sf::st_zm(drop=TRUE, what="ZM")
          dpt <- tryCatch(sf::st_cast(dpt, "MULTIPOLYGON", warn=FALSE), error=function(e) dpt)
          
          rts$ton_route <- ifelse(is.na(rts$ton_route) | rts$ton_route < 0, 0, rts$ton_route)
          
          pal_gy <- grDevices::colorRampPalette(
            c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
          )(4)
          
          vals <- rts$ton_route
          vals <- vals[is.finite(vals) & vals > 0]
          bins <- make_bins4(vals)
          
          pal_routes <- leaflet::colorBin(
            palette  = pal_gy,
            bins     = bins,
            domain   = vals,
            na.color = "#6b7280",
            right    = FALSE
          )
          
          widget <- leaflet::leaflet(dpt, options = leaflet::leafletOptions(zoomControl=FALSE)) %>%
            leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
            leaflet::addPolygons(
              fillColor   = "#d1d5db",
              fillOpacity = 0.45,
              color = "#9ca3af",
              weight = 0.8
            ) %>%
            leaflet::addPolylines(
              data = rts,
              color   = ~ifelse(is.na(ton_route) | ton_route <= 0, "#6b7280", pal_routes(ton_route)),
              weight  = 2,
              opacity = 0.75
            ) %>%
            leaflet::addLegend(
              position="bottomright",
              pal=pal_routes,
              values=rts$ton_route,
              title=paste0("Ton por ruta (→ ", DPTO_FOCO_NOMBRE, ")"),
              labFormat=legend_lab_range(""),
              na.label="0 / NA"
            ) %>%
            leaflet::addControl(bdg, position="topright")
        }
      }
      
      tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget, tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, file = file, vwidth = 1200, vheight = 820, zoom = 2)
    }
  )
}

shinyApp(ui, server)

