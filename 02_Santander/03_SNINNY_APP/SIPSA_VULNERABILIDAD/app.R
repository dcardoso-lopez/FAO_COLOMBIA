# app.R
# =========================================================
# SIPSA_ABASTECIMIENTO_HHI — 4 pestañas (Storytelling)
# (FOCO = SANTANDER)  ✅
# =========================================================

DPTO_FOCO_NOMBRE <- "Santander"  # ✅ CAMBIO
DPTO_FOCO_COD    <- "68"         # ✅ CAMBIO
APP_TITLE <- paste0(" ")

# ------------------------------
# Paquetes (NO instalar aquí)
# ------------------------------
pkgs <- c(
  "shiny","bslib","shinyWidgets",
  "dplyr","stringr","janitor","scales",
  "readr","stringi","htmltools",
  "lubridate",
  "plotly","ggplot2",
  "DT",
  "sf","leaflet","webshot2","htmlwidgets"
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
  library(readr); library(stringi); library(htmltools)
  library(lubridate)
  library(plotly); library(ggplot2)
  library(DT)
  library(sf); library(leaflet); library(webshot2); library(htmlwidgets)
})

options(stringsAsFactors = FALSE, scipen = 999)
sf::sf_use_s2(FALSE)

# ✅ Blindaje contra funciones enmascaradas
select    <- dplyr::select
mutate    <- dplyr::mutate
filter    <- dplyr::filter
summarise <- dplyr::summarise
arrange   <- dplyr::arrange
left_join <- dplyr::left_join

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
validate <- shiny::validate
need     <- shiny::need

# =========================================================
# Rutas robustas (runApp / source) + DATA DIR RELATIVO
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

fmt_num_co <- function(x, digits = 0){
  scales::number(x, big.mark=".", decimal.mark=",", accuracy = 10^(-digits))
}
fmt_pct_co <- function(x, digits = 1){
  ifelse(is.finite(x),
         paste0(scales::number(x, big.mark=".", decimal.mark=",", accuracy = 10^(-digits)), "%"),
         "NA")
}
fmt_ton_co <- function(x, digits = 1){
  scales::number(x, big.mark=".", decimal.mark=",", accuracy = 10^(-digits))
}

is_all <- function(x){
  is.null(x) || length(x) == 0 || identical(x, "Todos")
}
kg_to_ton <- function(x) x/1000

parse_num_co <- function(x){
  readr::parse_number(
    as.character(x),
    locale = readr::locale(grouping_mark = ".", decimal_mark = ",")
  )
}

# =========================================================
# ✅ Clasificación territorial (Región natural) por dpto origen
# =========================================================
dpto_region_map <- data.frame(
  cod_dpto = c(
    "05","08","11","13","15","17","18","19","20","23","25","27","41","44","47","50",
    "52","54","63","66","68","70","73","76","81","85","86","88","91","94","95","97","99"
  ),
  region = c(
    "Andina","Caribe","Andina","Caribe","Andina","Andina","Amazonía","Pacífica","Caribe","Caribe","Andina","Pacífica","Andina","Caribe","Caribe","Orinoquía",
    "Pacífica","Andina","Andina","Andina","Andina","Caribe","Andina","Pacífica","Orinoquía","Orinoquía","Amazonía","Insular","Amazonía","Amazonía","Amazonía","Amazonía","Orinoquía"
  ),
  stringsAsFactors = FALSE
)

region_label <- function(region){
  ifelse(is.na(region) | region == "", "Sin clasificar", region)
}

# ✅ Paleta fija por región (corredor)
region_palette_map <- c(
  "Andina"         = "#1F77B4",
  "Caribe"         = "#FF7F0E",
  "Pacífica"       = "#2CA02C",
  "Orinoquía"      = "#9467BD",
  "Amazonía"       = "#8C564B",
  "Insular"        = "#E377C2",
  "Sin clasificar" = "#6B7280"
)

# =========================================================
# DT (DataTable) — idioma + opciones
# =========================================================
dt_lang_es <- list(
  processing = "Procesando...",
  search = "Buscar:",
  lengthMenu = "Mostrar _MENU_",
  info = "Mostrando _START_ a _END_ de _TOTAL_",
  infoEmpty = "Mostrando 0 a 0 de 0",
  infoFiltered = "(filtrado de _MAX_ en total)",
  loadingRecords = "Cargando...",
  zeroRecords = "No se encontraron resultados",
  emptyTable = "Sin datos",
  paginate = list(first="Primero", previous="Anterior", `next`="Siguiente", last="Último")
)

dt_opts <- function(pageLength = 10){
  list(
    pageLength = pageLength,
    lengthMenu = c(5,10,15,25,50,100),
    scrollX = TRUE,
    autoWidth = TRUE,
    stateSave = TRUE,
    deferRender = TRUE
  )
}
dt_opts_lang <- function(pageLength = 10){
  opts <- dt_opts(pageLength)
  opts$language <- dt_lang_es
  opts
}

# =========================================================
# Finder genérico de RDS (LECTURA RELATIVA)
# =========================================================
rds_candidates <- function(stem){
  c(
    file.path(data_dir, paste0(stem, ".rds")),
    file.path(data_dir, paste0(stem, ".RDS")),
    file.path(app_root, paste0(stem, ".rds")),
    file.path(app_root, paste0(stem, ".RDS")),
    file.path("data", paste0(stem, ".rds")),
    file.path("data", paste0(stem, ".RDS")),
    paste0(stem, ".rds"),
    paste0(stem, ".RDS")
  )
}

find_rds <- function(paths){
  for (p in paths) {
    if (!is.na(p) && file.exists(p) && !dir.exists(p) && grepl("\\.rds$", tolower(p))) {
      return(normalizePath(p, winslash = "/", mustWork = TRUE))
    }
  }
  NA_character_
}

# =========================================================
# 1) BASE PRINCIPAL (para TAB 1)
# =========================================================
rds_path <- find_rds(rds_candidates("041_DANE_SIPSA-Abast"))
if (is.na(rds_path)) {
  stop(
    "No encontré el archivo ./data/041_DANE_SIPSA-Abast.rds\n\n",
    "Solución rápida:\n",
    "1) Copia el .rds a: ", data_dir, "\n",
    "2) y nómbralo exactamente: 041_DANE_SIPSA-Abast.rds"
  )
}

raw <- readRDS(rds_path)
df  <- janitor::clean_names(raw)
nms <- names(df)

ycol  <- req_col(nms, c("ano","anio","year"), "AÑO")
gcol  <- req_col(nms, c("grupo","grupo_alimento","grupo_alimentos"), "GRUPO")
acol  <- req_col(nms, c("alimento","producto","articulo"), "ALIMENTO")
qcol  <- req_col(nms, c("cantkg_total","cant_kg_total","cantidad_kg","cantidad","kg","cantkg","cantkg_total_kg"), "CANTIDAD (Kg)")

dpto_d_c <- pick_first(nms, c("cod_dane_dpto_d","dane_cod_dpto_d","cod_dpto_d","cod_depto_d","cod_dane_depto_d"))
dpto_o_c <- pick_first(nms, c("cod_dane_dpto_o","dane_cod_dpto_o","cod_dpto_o","cod_depto_o","cod_dane_depto_o"))

abast_all <- df %>%
  dplyr::transmute(
    anio = suppressWarnings(as.integer(.data[[ycol]])),
    grupo    = title_case_es(.data[[gcol]]),
    alimento = title_case_es(.data[[acol]]),
    cant_kg  = suppressWarnings(as.numeric(.data[[qcol]])),
    cod_dpto_d = if (!is.na(dpto_d_c)) pad_dpto(.data[[dpto_d_c]]) else NA_character_,
    cod_dpto_o = if (!is.na(dpto_o_c)) pad_dpto(.data[[dpto_o_c]]) else NA_character_
  ) %>%
  dplyr::filter(
    is.finite(anio), anio >= 2018,
    !is.na(grupo), grupo != "",
    !is.na(alimento), alimento != "",
    is.finite(cant_kg), cant_kg > 0
  )

# Base TAB 1: hacia FOCO si existe cod destino
abast_t1 <- abast_all
if (!all(is.na(abast_t1$cod_dpto_d))) {
  abast_t1 <- abast_t1 %>% dplyr::filter(cod_dpto_d == DPTO_FOCO_COD)
}

# =========================================================
# 2) TAB 2 — Cargar RDS 041_DANE_SIPSA-Abast_2.rds
# =========================================================
rds_path2 <- find_rds(rds_candidates("041_DANE_SIPSA-Abast_2"))
abast2 <- NULL

if (!is.na(rds_path2)) {
  raw2 <- readRDS(rds_path2)
  df2  <- janitor::clean_names(raw2)
  nms2 <- names(df2)
  
  y2 <- req_col(nms2, c("ano","anio","year"), "AÑO (base 2)")
  q2 <- req_col(nms2, c("cantkg_total","cant_kg_total","cantidad_kg","cantidad","kg","cantkg","cantkg_total_kg"), "CANTIDAD (Kg) (base 2)")
  
  dpto_d2_c <- pick_first(nms2, c("cod_dane_dpto_d","dane_cod_dpto_d","cod_dpto_d","cod_depto_d","cod_dane_depto_d"))
  dpto_o2_c <- pick_first(nms2, c("cod_dane_dpto_o","dane_cod_dpto_o","cod_dpto_o","cod_depto_o","cod_dane_depto_o"))
  dpto_d2_n <- pick_first(nms2, c("departamento_d","depto_d","departamento_destino","depto_destino"))
  dpto_o2_n <- pick_first(nms2, c("departamento_o","depto_o","departamento_origen","depto_origen"))
  
  g2 <- pick_first(nms2, c("grupo","grupo_alimento","grupo_alimentos"))
  a2 <- pick_first(nms2, c("alimento","producto","articulo"))
  
  abast2 <- df2 %>%
    dplyr::transmute(
      anio    = suppressWarnings(as.integer(.data[[y2]])),
      cant_kg = suppressWarnings(as.numeric(.data[[q2]])),
      cod_dpto_d = if (!is.na(dpto_d2_c)) pad_dpto(.data[[dpto_d2_c]]) else NA_character_,
      cod_dpto_o = if (!is.na(dpto_o2_c)) pad_dpto(.data[[dpto_o2_c]]) else NA_character_,
      dpto_d = if (!is.na(dpto_d2_n)) title_case_es(.data[[dpto_d2_n]]) else NA_character_,
      dpto_o = if (!is.na(dpto_o2_n)) title_case_es(.data[[dpto_o2_n]]) else NA_character_,
      grupo    = if (!is.na(g2)) title_case_es(.data[[g2]]) else NA_character_,
      alimento = if (!is.na(a2)) title_case_es(.data[[a2]]) else NA_character_
    ) %>%
    dplyr::filter(is.finite(anio), anio >= 2018, is.finite(cant_kg), cant_kg > 0)
  
  if (!all(is.na(abast2$dpto_o))) abast2 <- abast2 %>% dplyr::filter(!is.na(dpto_o), dpto_o != "")
}

# =========================================================
# 3) TAB 3 — Cargar RDS 041_DANE_SIPSA-Abast_3.rds
# =========================================================
rds_path3 <- find_rds(rds_candidates("041_DANE_SIPSA-Abast_3"))
abast3 <- NULL

if (!is.na(rds_path3)) {
  raw3 <- readRDS(rds_path3)
  df3  <- janitor::clean_names(raw3)
  nms3 <- names(df3)
  
  y3 <- req_col(nms3, c("ano","anio","year"), "AÑO (base 3)")
  q3 <- req_col(nms3, c("cantkg_total","cant_kg_total","cantidad_kg","cantidad","kg","cantkg","cantkg_total_kg"), "CANTIDAD (Kg) (base 3)")
  
  dpto_d3_c <- pick_first(nms3, c("cod_dane_dpto_d","dane_cod_dpto_d","cod_dpto_d","cod_depto_d","cod_dane_depto_d"))
  dpto_o3_c <- pick_first(nms3, c("cod_dane_dpto_o","dane_cod_dpto_o","cod_dpto_o","cod_depto_o","cod_dane_depto_o"))
  dpto_d3_n <- pick_first(nms3, c("departamento_d","depto_d","departamento_destino","depto_destino"))
  dpto_o3_n <- pick_first(nms3, c("departamento_o","depto_o","departamento_origen","depto_origen"))
  
  g3 <- pick_first(nms3, c("grupo","grupo_alimento","grupo_alimentos"))
  a3 <- pick_first(nms3, c("alimento","producto","articulo"))
  
  abast3 <- df3 %>%
    dplyr::transmute(
      anio    = suppressWarnings(as.integer(.data[[y3]])),
      cant_kg = suppressWarnings(as.numeric(.data[[q3]])),
      cod_dpto_d = if (!is.na(dpto_d3_c)) pad_dpto(.data[[dpto_d3_c]]) else NA_character_,
      cod_dpto_o = if (!is.na(dpto_o3_c)) pad_dpto(.data[[dpto_o3_c]]) else NA_character_,
      dpto_d = if (!is.na(dpto_d3_n)) title_case_es(.data[[dpto_d3_n]]) else NA_character_,
      dpto_o = if (!is.na(dpto_o3_n)) title_case_es(.data[[dpto_o3_n]]) else NA_character_,
      grupo    = if (!is.na(g3)) title_case_es(.data[[g3]]) else NA_character_,
      alimento = if (!is.na(a3)) title_case_es(.data[[a3]]) else NA_character_
    ) %>%
    dplyr::filter(is.finite(anio), anio >= 2018, is.finite(cant_kg), cant_kg > 0)
  
  if (!all(is.na(abast3$dpto_o))) abast3 <- abast3 %>% dplyr::filter(!is.na(dpto_o), dpto_o != "")
  if (!all(is.na(abast3$dpto_d))) abast3 <- abast3 %>% dplyr::filter(!is.na(dpto_d), dpto_d != "")
}

# =========================================================
# 4) TAB 4 — Cargar RDS 041_DANE_SIPSA-Abast_4.rds  ✅ (base pre-calculada)
# =========================================================
rds_path4 <- find_rds(rds_candidates("041_DANE_SIPSA-Abast_4"))
abast4 <- NULL

if (!is.na(rds_path4)) {
  raw4 <- readRDS(rds_path4)
  df4  <- janitor::clean_names(raw4)
  nms4 <- names(df4)
  
  y4  <- req_col(nms4, c("ano","anio","year"), "AÑO (base 4)")
  g4  <- req_col(nms4, c("grupo","grupo_alimento","grupo_alimentos"), "GRUPO (base 4)")
  a4  <- req_col(nms4, c("alimento","producto","articulo"), "ALIMENTO (base 4)")
  
  cod_o4 <- req_col(nms4, c("cod_dane_dpto_o","cod_dpto_o","cod_depto_o","dane_cod_dpto_o"), "COD_ORIGEN (base 4)")
  nom_o4 <- pick_first(nms4, c("departamento_o","depto_o","departamento_origen","depto_origen"))
  
  kg_den4 <- req_col(nms4, c("kg_total_origen","kg_total","kg_origen_total","kg_total_envia","kg_total_origen_dpto"), "KG_TOTAL_ORIGEN (base 4)")
  kg_num4 <- req_col(nms4, c("kg_a_atlantico","kg_atlantico","kg_hacia_atlantico","kg_a_foco","kg_hacia_foco","kg_a_santander","kg_hacia_santander"), "KG_A_FOCO (base 4)")
  pct4    <- pick_first(nms4, c("pct_importancia","porc_importancia","pct","participacion","participacion_pct"))
  
  abast4 <- df4 %>%
    dplyr::transmute(
      anio = suppressWarnings(as.integer(.data[[y4]])),
      grupo    = title_case_es(.data[[g4]]),
      alimento = title_case_es(.data[[a4]]),
      cod_dpto_o = pad_dpto(.data[[cod_o4]]),
      dpto_o = if (!is.na(nom_o4)) title_case_es(.data[[nom_o4]]) else NA_character_,
      kg_total_origen = suppressWarnings(as.numeric(.data[[kg_den4]])),
      kg_a_atlantico  = suppressWarnings(as.numeric(.data[[kg_num4]])),  # (nombre interno, pero es “a FOCO”)
      pct_importancia = if (!is.na(pct4)) suppressWarnings(as.numeric(.data[[pct4]])) else NA_real_
    ) %>%
    dplyr::filter(
      is.finite(anio), anio >= 2018,
      !is.na(grupo), grupo != "",
      !is.na(alimento), alimento != "",
      !is.na(cod_dpto_o), cod_dpto_o != "",
      is.finite(kg_total_origen), kg_total_origen > 0
    ) %>%
    dplyr::mutate(
      ton_total_origen = kg_to_ton(kg_total_origen),
      ton_a_atlantico  = kg_to_ton(pmax(kg_a_atlantico, 0)),
      pct_importancia  = dplyr::if_else(
        is.finite(pct_importancia),
        pct_importancia,
        dplyr::if_else(ton_total_origen > 0, (ton_a_atlantico/ton_total_origen)*100, NA_real_)
      )
    ) %>%
    dplyr::left_join(dpto_region_map, by = c("cod_dpto_o" = "cod_dpto")) %>%
    dplyr::mutate(
      region   = region_label(region),
      corredor = paste0("Corredor ", region),
      via_nombre = paste0("Corredor ", region)
    )
}

# =========================================================
# ✅ Colores fijos por grupo (TAB 1)
# =========================================================
col_palette <- c(
  "#007CC3", "#456ABB","#1A4922", "#2E7730", "#0D8D38", "#85A728", "#AEBF22", "#F2E203",
  "#F1B709", "#F39F06", "#BE7E11", "#08384D", "#094B5C", "#00596C", "#006A75", "#007A71",
  "#00909C", "#0088BB", "#007CC3", "#456ABB"
)

group_levels_all <- sort(unique(na.omit(abast_t1$grupo)))
group_colors_map <- setNames(rep(col_palette, length.out = length(group_levels_all)), group_levels_all)

# =========================================================
# UI helpers
# =========================================================
filters_box_t1 <- function(){
  div(
    class="filters",
    div(
      class="filters-grid-3",
      div(class="filter", div(class="filter-label","¿Qué año analizamos?"), uiOutput("anio_ui_t1")),
      div(class="filter", div(class="filter-label","¿Quieres enfocar un grupo?"), uiOutput("grupo_ui_t1")),
      div(class="filter", div(class="filter-label",""), tags$div(style="height:42px;"))
    )
  )
}

filters_box_t2 <- function(){
  div(
    class="filters",
    div(
      class="filters-grid-3",
      div(class="filter", div(class="filter-label","¿Qué año analizamos?"), uiOutput("anio_ui_t2")),
      div(class="filter", div(class="filter-label","¿Qué grupo alimenticio?"), uiOutput("grupo_ui_t2")),
      div(class="filter", div(class="filter-label","¿Qué alimento?"), uiOutput("alim_ui_t2"))
    )
  )
}

filters_box_blank <- function(tag){
  id_anio  <- paste0("anio_ui_", tag)
  id_grupo <- paste0("grupo_ui_", tag)
  id_alim  <- paste0("alim_ui_", tag)
  
  div(
    class="filters",
    div(
      class="filters-grid-3",
      div(class="filter", div(class="filter-label","¿Qué año analizamos?"), uiOutput(id_anio)),
      div(class="filter", div(class="filter-label","¿Qué grupo alimenticio?"), uiOutput(id_grupo)),
      div(class="filter", div(class="filter-label","¿Qué alimento?"), uiOutput(id_alim))
    )
  )
}

# =========================================================
# TAB 4 — Helpers Leaflet / SF (rutas)
# =========================================================
format_short <- function(x){
  scales::number(x, accuracy = 0.1, big.mark=".", decimal.mark=",")
}

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

load_dept_sf_only_safe <- function(data_dir){
  tryCatch({
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
  }, error = function(e){
    NULL
  })
}

list_gpkgs <- function(data_dir){
  cand_dirs <- unique(c(file.path(data_dir, "shp"), data_dir))
  gp <- unlist(lapply(cand_dirs, function(dd){
    if (!dir.exists(dd)) return(character(0))
    list.files(dd, pattern="\\.gpkg$", full.names=TRUE, ignore.case=TRUE)
  }))
  gp <- gp[file.exists(gp)]
  gp
}

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

dept_sf <- load_dept_sf_only_safe(data_dir)
gpkg_files <- list_gpkgs(data_dir)
prefer_name <- "rutas_ORS_barranquilla_capitales_con_contexto.gpkg"
gpkg_default <- gpkg_files[basename(gpkg_files) == prefer_name][1]
if (is.na(gpkg_default) && length(gpkg_files)) gpkg_default <- gpkg_files[1]

# =========================================================
# UI (incluye TAB 4)
# =========================================================
filters_box_t4 <- function(){
  div(
    class="filters",
    div(
      class="filters-grid-4",
      div(class="filter", div(class="filter-label","¿Qué año analizamos?"), uiOutput("anio_ui_t4")),
      div(class="filter", div(class="filter-label","¿Qué grupo alimenticio?"), uiOutput("grupo_ui_t4")),
      div(class="filter", div(class="filter-label","¿Qué alimento?"), uiOutput("alim_ui_t4")),
      div(class="filter", div(class="filter-label","¿Qué corredor (región)?"), uiOutput("cor_ui_t4"))
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
      :root{ --accent-border:#ffb366; }
      body{ background:#ffffff; }
      .wrap{ max-width:1360px; margin:0 auto; padding:16px 20px 32px; }
      h2#app-title{ text-align:center; margin-top:10px; margin-bottom:10px; font-weight:800; letter-spacing:.3px; }

      .tabs-box{
        background:#fff; border:1px solid var(--accent-border) !important;
        border-radius:16px; padding:12px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
      }

      .filters{
        background:#fff; border:1px solid var(--accent-border); border-radius:16px;
        padding:14px 16px; margin-bottom:12px; box-shadow:0 4px 14px rgba(0,0,0,.06);
        width:100%; overflow: visible; position: relative; z-index: 20;
      }

      .filters-grid-3{
        width:100%;
        display:grid;
        grid-template-columns: repeat(3, minmax(240px, 1fr));
        column-gap:16px; row-gap:10px; align-items:end;
      }
      @media (max-width: 1100px){ .filters-grid-3{ grid-template-columns: repeat(2, minmax(240px, 1fr)); } }
      @media (max-width: 650px){ .filters-grid-3{ grid-template-columns: 1fr; } }

      .filters-grid-4{
        width:100%;
        display:grid;
        grid-template-columns: repeat(4, minmax(220px, 1fr));
        column-gap:16px; row-gap:10px; align-items:end;
      }
      @media (max-width: 1200px){ .filters-grid-4{ grid-template-columns: repeat(2, minmax(240px, 1fr)); } }
      @media (max-width: 650px){ .filters-grid-4{ grid-template-columns: 1fr; } }

      .filter-label{
        font-weight:800; font-size:13px; margin-bottom:6px; color:#111827;
        white-space: normal; line-height: 1.15; min-height: 28px;
      }

      .form-select, .selectize-input, .bootstrap-select > .dropdown-toggle{
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

      .blocks-grid{
        display:grid;
        grid-template-columns: repeat(2, minmax(420px, 1fr));
        gap:12px;
        grid-auto-rows: minmax(380px, auto);
      }
      @media (max-width: 980px){
        .blocks-grid{ grid-template-columns: 1fr; grid-auto-rows: auto; }
        .span-2rows{ grid-row: auto; }
      }
      .span-2rows{ grid-row: span 2; }

      #blk_plot_1, #blk_plot_3 { height: 380px !important; }
      #blk_plot_2 { height: 772px !important; }

      .blocks-grid-2{
        display:grid;
        grid-template-columns: repeat(2, minmax(420px, 1fr));
        gap:12px;
        grid-auto-rows: minmax(420px, auto);
      }
      @media (max-width: 980px){ .blocks-grid-2{ grid-template-columns: 1fr; } }

      #t2_plot_a, #t2_plot_b, #t3_plot_b, #t3_plot_e { height: 420px !important; }

      #t4_map_routes{ height: 600px !important; }

      .leaflet-tooltip.lbl-clean{
        background: rgba(255,255,255,.92); border: 1px solid #e6e6e6; border-radius: 6px;
        padding: 4px 6px; color: #222; font-weight: 800; box-shadow: 0 1px 4px rgba(0,0,0,.08);
      }

      .sel-row{ display:flex; gap:10px; flex-wrap:wrap; align-items:center; justify-content:space-between; margin:6px 0 2px; }
      .sel-chip{
        display:inline-block; padding:6px 10px; border-radius:999px;
        border:1px solid #e5e7eb; background:#fff; font-size:12px; color:#111827; font-weight:800;
      }
      .btn-clear{ border-radius:10px; }

      .dataTables_wrapper{ width:100% !important; }

      .t4-grid{
        display:grid;
        grid-template-columns: 1.45fr 0.55fr;
        gap:12px;
        align-items:start;
      }
      @media (max-width: 1100px){
        .t4-grid{ grid-template-columns: 1fr; }
      }
      .kpi-box{
        border:1px solid var(--accent-border);
        border-radius:16px;
        padding:14px 14px;
        background:#fff;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
      }
      .kpi-title{
        font-weight:900; font-size:13px; color:#111827;
        margin-bottom:6px; line-height:1.2;
      }
      .kpi-value{
        font-weight:950; font-size:28px; color:#0f172a;
        margin-bottom:2px;
      }
      .kpi-sub{
        font-weight:700; font-size:12px; color:#6b7280;
      }
      .kpi-divider{ height:10px; }
    "))
  ),
  
  div(
    class="wrap",
    h2(APP_TITLE, id="app-title"),
    
    div(
      class="tabs-box",
      tabsetPanel(
        type = "tabs",
        
        tabPanel(
          "Concentración de la canasta por grupo de alimentos (IHH)",
          filters_box_t1(),
          
          div(
            class="blocks-grid",
            
            div(class="card",
                div(class="card-title", strong("Concentración del abastecimiento por grupo de alimentos (IHH)")),
                plotlyOutput("blk_plot_1")
            ),
            
            div(class="card span-2rows",
                div(class="card-title", strong("Top 15 alimentos por volumen abastecido (toneladas)")),
                plotlyOutput("blk_plot_2")
            ),
            
            div(class="card",
                div(class="card-title", strong("Evolución temporal de la concentración por grupo de alimentos (IHH)")),
                plotlyOutput("blk_plot_3")
            )
          ),
          
          div(class="card", style="margin-top:12px;",
              div(class="card-title", strong("Ranking y métricas de concentración por grupo de alimentos (IHH)")),
              DTOutput("hhi_group_table")
          )
        ),
        
        tabPanel(
          paste0("Concentración de abastecedores hacia ", DPTO_FOCO_NOMBRE, " por origen (IHH)"),
          filters_box_t2(),
          
          div(
            class="blocks-grid-2",
            
            div(class="card",
                div(class="card-title", strong(paste0("Top 15 de departamentos abastecedores hacia ", DPTO_FOCO_NOMBRE))),
                plotlyOutput("t2_plot_a")
            ),
            
            div(class="card",
                div(class="card-title", strong("Evolución temporal de la concentración de abastecedores")),
                plotlyOutput("t2_plot_b")
            )
          ),
          
          div(class="card", style="margin-top:12px;",
              div(class="card-title", strong("Participación y volumen por departamento de origen")),
              DTOutput("t2_table")
          )
        ),
        
        tabPanel(
          paste0("Diversificación de destinos desde ", DPTO_FOCO_NOMBRE, " (IHH por destino)"),
          filters_box_blank("t3"),
          
          div(
            class="blocks-grid-2",
            
            div(class="card",
                div(class="card-title", strong("Destino dominante por alimento (conteo de alimentos)")),
                plotlyOutput("t3_plot_e")
            ),
            
            div(class="card",
                div(class="card-title", strong(paste0("Evolución temporal de la concentración de destinos (IHH, ", DPTO_FOCO_NOMBRE, " → destinos)"))),
                plotlyOutput("t3_plot_b")
            )
          ),
          
          div(class="card", style="margin-top:12px;",
              div(class="card-title", strong(paste0("IHH por alimento y destino dominante (origen: ", DPTO_FOCO_NOMBRE, ")"))),
              DTOutput("t3_table")
          )
        ),
        
        tabPanel(
          paste0("Corredores del abastecimiento hacia ", DPTO_FOCO_NOMBRE),
          filters_box_t4(),
          
          div(
            class="t4-grid",
            
            div(
              class="card",
              div(class="card-title", strong(paste0("Corredores (rutas) de volúmenes de envío hacia ", DPTO_FOCO_NOMBRE))),
              uiOutput("sel_routes_ui_t4"),
              leafletOutput("t4_map_routes")
            ),
            
            div(
              class="kpi-box",
              uiOutput("t4_kpis_ui")
            )
          )
        )
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  
  safe_outputOptions <- function(name, ...) {
    tryCatch(shiny::outputOptions(output, name, ...), error = function(e) NULL)
  }
  session$onFlushed(function() {
    safe_outputOptions("hhi_group_table", suspendWhenHidden = FALSE)
    safe_outputOptions("t2_table",        suspendWhenHidden = FALSE)
    safe_outputOptions("t3_table",        suspendWhenHidden = FALSE)
    safe_outputOptions("t4_map_routes",   suspendWhenHidden = FALSE)
  }, once = TRUE)
  
  # =========================================================
  # TAB 1 — filtros
  # =========================================================
  years1 <- sort(unique(abast_t1$anio[is.finite(abast_t1$anio)]), decreasing = TRUE)
  
  output$anio_ui_t1 <- renderUI({
    selectInput("anio_t1", NULL, choices = c("Todos"="Todos", years1), selected = "Todos")
  })
  
  base_all_t1 <- reactive({
    df <- abast_t1
    if (!is_all(input$anio_t1)) df <- df %>% dplyr::filter(anio == as.integer(input$anio_t1))
    validate(need(nrow(df) > 0, "Sin datos con los filtros seleccionados."))
    df
  })
  
  output$grupo_ui_t1 <- renderUI({
    grupos <- sort(unique(na.omit(base_all_t1()$grupo)))
    selectInput("grupo_t1", NULL, choices = c("Todos"="Todos", grupos), selected = "Todos")
  })
  
  ga_t1 <- reactive({
    df <- base_all_t1()
    
    ga <- df %>%
      dplyr::group_by(grupo, alimento) %>%
      dplyr::summarise(kg = sum(cant_kg, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(is.finite(kg), kg > 0)
    
    validate(need(nrow(ga) > 0, "No se pudo construir Grupo → Alimento."))
    
    gtot <- ga %>%
      dplyr::group_by(grupo) %>%
      dplyr::summarise(
        total_kg = sum(kg),
        n_alimentos = dplyr::n_distinct(alimento),
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(total_kg), total_kg > 0)
    
    ga %>%
      dplyr::left_join(gtot, by="grupo") %>%
      dplyr::mutate(share = kg / total_kg) %>%
      dplyr::filter(is.finite(share), share > 0)
  })
  
  hhi_by_group <- reactive({
    ga <- ga_t1()
    
    top_food <- ga %>%
      dplyr::arrange(grupo, dplyr::desc(share)) %>%
      dplyr::group_by(grupo) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(grupo, top_alimento = alimento, top_share = share, top_share_pct = share*100)
    
    out <- ga %>%
      dplyr::group_by(grupo) %>%
      dplyr::summarise(
        total_kg = dplyr::first(total_kg),
        n_alimentos = dplyr::first(n_alimentos),
        hhi01 = sum(share^2, na.rm = TRUE),
        hhi10000 = sum((share*100)^2, na.rm = TRUE),
        nequiv = ifelse(is.finite(hhi01) && hhi01 > 0, 1/hhi01, NA_real_),
        .groups = "drop"
      ) %>%
      dplyr::left_join(top_food, by="grupo") %>%
      dplyr::mutate(
        total_ton = kg_to_ton(total_kg),
        total_ton_lbl = fmt_num_co(total_ton, 1),
        hhi01_lbl = ifelse(is.finite(hhi01), fmt_num_co(hhi01, 3), "NA"),
        hhi10000_lbl = ifelse(is.finite(hhi10000), fmt_num_co(hhi10000, 0), "NA"),
        nequiv_lbl = ifelse(is.finite(nequiv), fmt_num_co(nequiv, 1), "NA"),
        top_share_lbl = fmt_pct_co(top_share_pct, 1),
        tooltip_hhi = paste0(
          "<b>", grupo, "</b>",
          "<br>IHH (0–1): ", hhi01_lbl,
          "<br>IHH (0–10.000): ", hhi10000_lbl,
          "<br># alimentos: ", n_alimentos,
          "<br>N efectivo: ", nequiv_lbl,
          "<br>Top alimento: ", top_alimento, " (", top_share_lbl, ")",
          "<br>Total (Ton): ", total_ton_lbl,
          "<extra></extra>"
        )
      )
    
    validate(need(nrow(out) >= 1, "No hay grupos suficientes para HHI."))
    out
  })
  
  hhi_by_group_year <- reactive({
    df <- base_all_t1()
    
    ga_y <- df %>%
      dplyr::group_by(anio, grupo, alimento) %>%
      dplyr::summarise(kg = sum(cant_kg, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(is.finite(kg), kg > 0)
    
    validate(need(nrow(ga_y) > 0, "No se pudo construir serie anual Grupo → Alimento."))
    
    gtot_y <- ga_y %>%
      dplyr::group_by(anio, grupo) %>%
      dplyr::summarise(
        total_kg = sum(kg),
        n_alimentos = dplyr::n_distinct(alimento),
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(total_kg), total_kg > 0)
    
    hhi_y <- ga_y %>%
      dplyr::left_join(gtot_y, by=c("anio","grupo")) %>%
      dplyr::mutate(share = kg / total_kg) %>%
      dplyr::group_by(anio, grupo) %>%
      dplyr::summarise(
        hhi01 = sum(share^2, na.rm = TRUE),
        hhi01_lbl = ifelse(is.finite(hhi01), fmt_num_co(hhi01, 3), "NA"),
        total_kg = dplyr::first(total_kg),
        n_alimentos = dplyr::first(n_alimentos),
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(hhi01)) %>%
      dplyr::mutate(
        total_ton = kg_to_ton(total_kg),
        tooltip = paste0(
          "<b>", grupo, "</b>",
          "<br>Año: ", anio,
          "<br>IHH (0–1): ", hhi01_lbl,
          "<br># alimentos: ", n_alimentos,
          "<br>Total (Ton): ", fmt_num_co(total_ton, 1),
          "<extra></extra>"
        )
      )
    
    if (!is_all(input$grupo_t1)) hhi_y <- hhi_y %>% dplyr::filter(grupo == input$grupo_t1)
    
    validate(need(nrow(hhi_y) > 0, "No hay datos para la serie temporal con esos filtros."))
    hhi_y %>% dplyr::mutate(grupo = factor(grupo, levels = group_levels_all))
  })
  
  output$blk_plot_1 <- renderPlotly({
    df <- hhi_by_group() %>%
      dplyr::filter(is.finite(hhi01)) %>%
      dplyr::arrange(dplyr::desc(hhi01)) %>%
      dplyr::mutate(grupo_ord = factor(grupo, levels = rev(grupo)))
    
    validate(need(nrow(df) > 0, "Sin datos para graficar HHI por grupo."))
    
    plotly::plot_ly(
      data = df,
      x = ~hhi01,
      y = ~grupo_ord,
      type = "bar",
      orientation = "h",
      text = ~hhi01_lbl,
      textposition = "auto",
      textangle = 0,
      hovertext = ~tooltip_hhi,
      hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = "IHH (0–1)", rangemode = "tozero"),
        yaxis = list(title = ""),
        margin = list(l=150, r=20, t=10, b=50)
      )
  })
  
  output$blk_plot_2 <- renderPlotly({
    df <- base_all_t1()
    if (!is_all(input$grupo_t1)) df <- df %>% dplyr::filter(grupo == input$grupo_t1)
    
    top15 <- df %>%
      dplyr::group_by(alimento) %>%
      dplyr::summarise(kg = sum(cant_kg, na.rm = TRUE), .groups="drop") %>%
      dplyr::filter(is.finite(kg), kg > 0) %>%
      dplyr::mutate(ton = kg_to_ton(kg)) %>%
      dplyr::arrange(dplyr::desc(ton)) %>%
      dplyr::slice_head(n = 15) %>%
      dplyr::mutate(
        alimento_ord = factor(alimento, levels = rev(alimento)),
        ton_lbl = fmt_num_co(ton, 1),
        tooltip = paste0(
          "<b>", alimento, "</b>",
          if (!is_all(input$grupo_t1)) paste0("<br>Grupo: ", input$grupo_t1) else "<br>Grupo: (todos)",
          "<br>Total: ", ton_lbl, " Ton",
          "<extra></extra>"
        )
      )
    
    validate(need(nrow(top15) > 0, "Sin datos para construir el Top 15."))
    
    plotly::plot_ly(
      data = top15,
      x = ~ton,
      y = ~alimento_ord,
      type = "bar",
      orientation = "h",
      text = ~ton_lbl,
      textposition = "auto",
      textangle = 0,
      hovertext = ~tooltip,
      hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Toneladas", rangemode = "tozero"),
        yaxis = list(title = ""),
        margin = list(l=190, r=20, t=10, b=55)
      )
  })
  
  output$blk_plot_3 <- renderPlotly({
    ts <- hhi_by_group_year() %>%
      dplyr::mutate(grupo = factor(as.character(grupo), levels = group_levels_all))
    
    plotly::plot_ly(
      data = ts,
      x = ~anio,
      y = ~hhi01,
      color = ~grupo,
      colors = unname(group_colors_map),
      type = "scatter",
      mode = "lines+markers",
      hovertext = ~tooltip,
      hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Año", tickmode = "linear", dtick = 1),
        yaxis = list(title = "IHH (0–1)", rangemode = "tozero"),
        legend = list(orientation = "h", y = -0.25),
        margin = list(l=60, r=20, t=10, b=80)
      )
  })
  
  output$hhi_group_table <- DT::renderDT({
    df <- hhi_by_group() %>%
      dplyr::arrange(dplyr::desc(hhi01)) %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::transmute(
        `#` = rank,
        Grupo = grupo,
        `IHH (0–1)` = hhi01_lbl,
        `IHH (0–10.000)` = hhi10000_lbl,
        `# alimentos` = n_alimentos,
        `N efectivo` = nequiv_lbl,
        `Top alimento` = top_alimento,
        `Top (%)` = top_share_lbl,
        `Total (Ton)` = total_ton_lbl
      )
    
    DT::datatable(
      df, rownames = FALSE, escape = TRUE,
      options = dt_opts_lang(pageLength = 10),
      class = "stripe hover order-column compact"
    )
  }, server = FALSE)
  
  # =========================================================
  # TAB 2 — filtros
  # =========================================================
  output$anio_ui_t2 <- renderUI({
    validate(need(!is.null(abast2), "No se encontró ./data/041_DANE_SIPSA-Abast_2.rds"))
    years2 <- sort(unique(abast2$anio[is.finite(abast2$anio)]), decreasing = TRUE)
    selectInput("anio_t2", NULL, choices = c("Todos"="Todos", years2), selected = "Todos")
  })
  
  output$grupo_ui_t2 <- renderUI({
    validate(need(!is.null(abast2), "No se encontró ./data/041_DANE_SIPSA-Abast_2.rds"))
    if (all(is.na(abast2$grupo))) return(selectInput("grupo_t2", NULL, choices=c("Todos"="Todos"), selected="Todos"))
    df <- abast2
    if (!is_all(input$anio_t2)) df <- df %>% dplyr::filter(anio == as.integer(input$anio_t2))
    grupos <- sort(unique(na.omit(df$grupo)))
    selectInput("grupo_t2", NULL, choices = c("Todos"="Todos", grupos), selected = "Todos")
  })
  
  output$alim_ui_t2 <- renderUI({
    validate(need(!is.null(abast2), "No se encontró ./data/041_DANE_SIPSA-Abast_2.rds"))
    if (all(is.na(abast2$alimento))) return(selectInput("alim_t2", NULL, choices=c("Todos"="Todos"), selected="Todos"))
    df <- abast2
    if (!is_all(input$anio_t2)) df <- df %>% dplyr::filter(anio == as.integer(input$anio_t2))
    if (!is_all(input$grupo_t2) && !all(is.na(df$grupo))) df <- df %>% dplyr::filter(grupo == input$grupo_t2)
    alims <- sort(unique(na.omit(df$alimento)))
    selectInput("alim_t2", NULL, choices = c("Todos"="Todos", alims), selected = "Todos")
  })
  
  base_t2 <- reactive({
    validate(need(!is.null(abast2), "No se cargó 041_DANE_SIPSA-Abast_2.rds"))
    df <- abast2
    
    if (!is_all(input$anio_t2))  df <- df %>% dplyr::filter(anio == as.integer(input$anio_t2))
    if (!is_all(input$grupo_t2) && !all(is.na(df$grupo)))    df <- df %>% dplyr::filter(grupo == input$grupo_t2)
    if (!is_all(input$alim_t2)  && !all(is.na(df$alimento))) df <- df %>% dplyr::filter(alimento == input$alim_t2)
    
    has_cod_dest <- !all(is.na(df$cod_dpto_d))
    has_nom_dest <- !all(is.na(df$dpto_d))
    if (has_cod_dest) df <- df %>% dplyr::filter(cod_dpto_d == DPTO_FOCO_COD)
    else if (has_nom_dest) df <- df %>% dplyr::filter(dpto_d == DPTO_FOCO_NOMBRE)
    
    if (!all(is.na(df$dpto_o))) df <- df %>% dplyr::filter(!is.na(dpto_o), dpto_o != "")
    
    validate(need(nrow(df) > 0, paste0("Sin datos hacia ", DPTO_FOCO_NOMBRE, " con los filtros seleccionados.")))
    df
  })
  
  orig_shares_t2 <- reactive({
    df <- base_t2()
    if (!all(is.na(df$dpto_o))) df <- df %>% dplyr::mutate(origen = dpto_o)
    else df <- df %>% dplyr::mutate(origen = cod_dpto_o)
    
    agg <- df %>%
      dplyr::group_by(origen) %>%
      dplyr::summarise(kg = sum(cant_kg, na.rm = TRUE), .groups="drop") %>%
      dplyr::filter(is.finite(kg), kg > 0) %>%
      dplyr::mutate(ton = kg_to_ton(kg))
    
    tot <- sum(agg$ton, na.rm = TRUE)
    validate(need(is.finite(tot) && tot > 0, paste0("No hay total positivo (Ton) hacia ", DPTO_FOCO_NOMBRE, ".")))
    
    out <- agg %>%
      dplyr::mutate(
        share = ton / tot,
        share_pct = share * 100,
        ton_lbl = fmt_num_co(ton, 1),
        share_lbl = fmt_pct_co(share_pct, 1)
      ) %>%
      dplyr::arrange(dplyr::desc(ton))
    
    hhi01 <- sum(out$share^2, na.rm = TRUE)
    attr(out, "hhi01_lbl") <- ifelse(is.finite(hhi01), fmt_num_co(hhi01, 3), "NA")
    out
  })
  
  hhi_year_t2 <- reactive({
    df <- base_t2()
    if (!all(is.na(df$dpto_o))) df <- df %>% dplyr::mutate(origen = dpto_o)
    else df <- df %>% dplyr::mutate(origen = cod_dpto_o)
    
    agg_y <- df %>%
      dplyr::group_by(anio, origen) %>%
      dplyr::summarise(kg = sum(cant_kg, na.rm = TRUE), .groups="drop") %>%
      dplyr::filter(is.finite(kg), kg > 0) %>%
      dplyr::mutate(ton = kg_to_ton(kg))
    
    tot_y <- agg_y %>%
      dplyr::group_by(anio) %>%
      dplyr::summarise(total_ton = sum(ton), .groups="drop") %>%
      dplyr::filter(is.finite(total_ton), total_ton > 0)
    
    hhi_y <- agg_y %>%
      dplyr::left_join(tot_y, by="anio") %>%
      dplyr::mutate(share = ton / total_ton) %>%
      dplyr::group_by(anio) %>%
      dplyr::summarise(
        hhi01 = sum(share^2, na.rm = TRUE),
        total_ton = dplyr::first(total_ton),
        .groups="drop"
      ) %>%
      dplyr::filter(is.finite(hhi01)) %>%
      dplyr::mutate(
        hhi01_lbl = fmt_num_co(hhi01, 3),
        total_lbl = fmt_num_co(total_ton, 1),
        tooltip = paste0(
          "<b>", DPTO_FOCO_NOMBRE, "</b>",
          "<br>Año: ", anio,
          "<br>IHH (0–1): ", hhi01_lbl,
          "<br>Total: ", total_lbl, " Ton",
          "<extra></extra>"
        )
      )
    
    validate(need(nrow(hhi_y) > 0, "No hay serie temporal HHI con esos filtros."))
    hhi_y
  })
  
  output$t2_plot_a <- renderPlotly({
    df <- orig_shares_t2()
    hhi_lbl <- attr(df, "hhi01_lbl")
    
    top <- df %>%
      dplyr::slice_head(n = 15) %>%
      dplyr::mutate(
        origen_ord = factor(origen, levels = rev(origen)),
        txt = paste0(ton_lbl, " Ton (", share_lbl, ")"),
        tooltip = paste0("<b>", origen, "</b><br>Ton: ", ton_lbl, "<br>Participación: ", share_lbl, "<extra></extra>")
      )
    
    plotly::plot_ly(
      data = top,
      x = ~ton,
      y = ~origen_ord,
      type = "bar",
      orientation = "h",
      text = ~txt,
      textposition = "auto",
      textangle = 0,
      hovertext = ~tooltip,
      hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = paste0("Toneladas hacia ", DPTO_FOCO_NOMBRE), rangemode = "tozero"),
        yaxis = list(title = ""),
        annotations = list(
          list(
            x = 0, y = 1.10, xref = "paper", yref = "paper",
            text = paste0("<b>HHI (0–1):</b> ", hhi_lbl),
            showarrow = FALSE, align = "left"
          )
        ),
        margin = list(l=210, r=20, t=50, b=55)
      )
  })
  
  output$t2_plot_b <- renderPlotly({
    ts <- hhi_year_t2()
    plotly::plot_ly(
      data = ts,
      x = ~anio, y = ~hhi01,
      type = "scatter", mode = "lines+markers",
      hovertext = ~tooltip, hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Año", tickmode = "linear", dtick = 1),
        yaxis = list(title = "HHI (0–1)", rangemode = "tozero"),
        margin = list(l=60, r=20, t=10, b=60)
      )
  })
  
  # =========================================================
  # ✅ TAB 2 — Tabla (FIX del parse) ✅
  # =========================================================
  output$t2_table <- DT::renderDT({
    df <- orig_shares_t2() %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::transmute(
        `#` = rank,
        `Departamento origen` = origen,
        ton_hacia_foco = ton_lbl,
        `Participación` = share_lbl
      )
    
    # ✅ Renombre dinámico sin romper el parseo
    names(df)[names(df) == "ton_hacia_foco"] <- paste0("Ton hacia ", DPTO_FOCO_NOMBRE)
    
    DT::datatable(
      df, rownames = FALSE, escape = TRUE,
      options = dt_opts_lang(pageLength = 10),
      class = "stripe hover order-column compact"
    )
  }, server = FALSE)
  
  # =========================================================
  # TAB 3 — filtros y outputs (igual que tu lógica)
  # =========================================================
  output$anio_ui_t3 <- renderUI({
    validate(need(!is.null(abast3), "No se encontró ./data/041_DANE_SIPSA-Abast_3.rds"))
    years3 <- sort(unique(abast3$anio[is.finite(abast3$anio)]), decreasing = TRUE)
    selectInput("anio_t3", NULL, choices = c("Todos"="Todos", years3), selected = "Todos")
  })
  
  output$grupo_ui_t3 <- renderUI({
    validate(need(!is.null(abast3), "No se encontró ./data/041_DANE_SIPSA-Abast_3.rds"))
    df <- abast3
    
    has_cod_org <- !all(is.na(df$cod_dpto_o))
    has_nom_org <- !all(is.na(df$dpto_o))
    if (has_cod_org) df <- df %>% dplyr::filter(cod_dpto_o == DPTO_FOCO_COD)
    else if (has_nom_org) df <- df %>% dplyr::filter(dpto_o == DPTO_FOCO_NOMBRE)
    
    if (!is_all(input$anio_t3)) df <- df %>% dplyr::filter(anio == as.integer(input$anio_t3))
    
    if (all(is.na(df$grupo))) return(selectInput("grupo_t3", NULL, choices=c("Todos"="Todos"), selected="Todos"))
    grupos <- sort(unique(na.omit(df$grupo)))
    selectInput("grupo_t3", NULL, choices = c("Todos"="Todos", grupos), selected = "Todos")
  })
  
  output$alim_ui_t3 <- renderUI({
    validate(need(!is.null(abast3), "No se encontró ./data/041_DANE_SIPSA-Abast_3.rds"))
    df <- abast3
    
    has_cod_org <- !all(is.na(df$cod_dpto_o))
    has_nom_org <- !all(is.na(df$dpto_o))
    if (has_cod_org) df <- df %>% dplyr::filter(cod_dpto_o == DPTO_FOCO_COD)
    else if (has_nom_org) df <- df %>% dplyr::filter(dpto_o == DPTO_FOCO_NOMBRE)
    
    if (!is_all(input$anio_t3)) df <- df %>% dplyr::filter(anio == as.integer(input$anio_t3))
    if (!is_all(input$grupo_t3) && !all(is.na(df$grupo))) df <- df %>% dplyr::filter(grupo == input$grupo_t3)
    
    if (all(is.na(df$alimento))) return(selectInput("alim_t3", NULL, choices=c("Todos"="Todos"), selected="Todos"))
    alims <- sort(unique(na.omit(df$alimento)))
    selectInput("alim_t3", NULL, choices = c("Todos"="Todos", alims), selected = "Todos")
  })
  
  base_t3 <- reactive({
    validate(need(!is.null(abast3), "No se cargó 041_DANE_SIPSA-Abast_3.rds"))
    df <- abast3
    
    if (!is_all(input$anio_t3))  df <- df %>% dplyr::filter(anio == as.integer(input$anio_t3))
    if (!is_all(input$grupo_t3) && !all(is.na(df$grupo)))    df <- df %>% dplyr::filter(grupo == input$grupo_t3)
    if (!is_all(input$alim_t3)  && !all(is.na(df$alimento))) df <- df %>% dplyr::filter(alimento == input$alim_t3)
    
    has_cod_org <- !all(is.na(df$cod_dpto_o))
    has_nom_org <- !all(is.na(df$dpto_o))
    if (has_cod_org) df <- df %>% dplyr::filter(cod_dpto_o == DPTO_FOCO_COD)
    else if (has_nom_org) df <- df %>% dplyr::filter(dpto_o == DPTO_FOCO_NOMBRE)
    
    has_cod_dest <- !all(is.na(df$cod_dpto_d))
    has_nom_dest <- !all(is.na(df$dpto_d))
    if (has_cod_dest) df <- df %>% dplyr::filter(!is.na(cod_dpto_d), cod_dpto_d != "", cod_dpto_d != DPTO_FOCO_COD)
    else if (has_nom_dest) df <- df %>% dplyr::filter(!is.na(dpto_d), dpto_d != "", dpto_d != DPTO_FOCO_NOMBRE)
    
    validate(need(nrow(df) > 0, paste0("Sin datos (origen ", DPTO_FOCO_NOMBRE, " → otros destinos) con los filtros seleccionados.")))
    df
  })
  
  hhi_food_t3 <- reactive({
    df <- base_t3()
    
    if (!all(is.na(df$dpto_d))) df <- df %>% dplyr::mutate(destino = dpto_d)
    else df <- df %>% dplyr::mutate(destino = cod_dpto_d)
    
    agg <- df %>%
      dplyr::group_by(grupo, alimento, destino) %>%
      dplyr::summarise(kg = sum(cant_kg, na.rm = TRUE), .groups="drop") %>%
      dplyr::filter(is.finite(kg), kg > 0) %>%
      dplyr::mutate(ton = kg_to_ton(kg))
    
    tot_i <- agg %>%
      dplyr::group_by(grupo, alimento) %>%
      dplyr::summarise(total_ton = sum(ton), .groups="drop") %>%
      dplyr::filter(is.finite(total_ton), total_ton > 0)
    
    shares <- agg %>%
      dplyr::left_join(tot_i, by=c("grupo","alimento")) %>%
      dplyr::mutate(p = ton / total_ton) %>%
      dplyr::filter(is.finite(p), p > 0)
    
    top_dest <- shares %>%
      dplyr::group_by(grupo, alimento) %>%
      dplyr::slice_max(order_by = p, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(grupo, alimento, destino_top = destino, top_p = p, top_p_pct = p*100)
    
    hhi <- shares %>%
      dplyr::group_by(grupo, alimento) %>%
      dplyr::summarise(
        hhi01 = sum(p^2, na.rm = TRUE),
        total_ton = dplyr::first(total_ton),
        ndest = dplyr::n_distinct(destino),
        .groups="drop"
      ) %>%
      dplyr::filter(is.finite(hhi01)) %>%
      dplyr::left_join(top_dest, by=c("grupo","alimento")) %>%
      dplyr::mutate(
        hhi01_lbl = fmt_num_co(hhi01, 3),
        total_lbl = fmt_num_co(total_ton, 1),
        nequiv = ifelse(hhi01 > 0, 1/hhi01, NA_real_),
        nequiv_lbl = ifelse(is.finite(nequiv), fmt_num_co(nequiv, 1), "NA"),
        destino_top = as.character(destino_top)
      ) %>%
      dplyr::arrange(dplyr::desc(hhi01), dplyr::desc(total_ton))
    
    validate(need(nrow(hhi) > 0, "No se pudo calcular HHI por alimento (TAB 3)."))
    hhi
  })
  
  hhi_year_t3 <- reactive({
    df0 <- base_t3()
    
    if (!all(is.na(df0$dpto_d))) df0 <- df0 %>% dplyr::mutate(destino = dpto_d)
    else df0 <- df0 %>% dplyr::mutate(destino = cod_dpto_d)
    
    agg <- df0 %>%
      dplyr::group_by(anio, destino) %>%
      dplyr::summarise(kg = sum(cant_kg, na.rm = TRUE), .groups="drop") %>%
      dplyr::filter(is.finite(kg), kg > 0) %>%
      dplyr::mutate(ton = kg_to_ton(kg))
    
    tot <- agg %>%
      dplyr::group_by(anio) %>%
      dplyr::summarise(total_ton = sum(ton), .groups="drop") %>%
      dplyr::filter(is.finite(total_ton), total_ton > 0)
    
    hhi <- agg %>%
      dplyr::left_join(tot, by="anio") %>%
      dplyr::mutate(p = ton / total_ton) %>%
      dplyr::group_by(anio) %>%
      dplyr::summarise(
        hhi01 = sum(p^2, na.rm = TRUE),
        total_ton = dplyr::first(total_ton),
        .groups="drop"
      ) %>%
      dplyr::filter(is.finite(hhi01)) %>%
      dplyr::mutate(
        hhi01_lbl = fmt_num_co(hhi01, 3),
        total_lbl = fmt_num_co(total_ton, 1),
        tooltip = paste0(
          "<b>", DPTO_FOCO_NOMBRE, " → destinos</b>",
          if (!is_all(input$grupo_t3)) paste0("<br>Grupo: ", input$grupo_t3) else "<br>Grupo: (todos)",
          if (!is_all(input$alim_t3))  paste0("<br>Alimento: ", input$alim_t3) else "",
          "<br>Año: ", anio,
          "<br>HHI destinos (0–1): ", hhi01_lbl,
          "<br>Total: ", total_lbl, " Ton",
          "<extra></extra>"
        )
      )
    
    validate(need(nrow(hhi) > 0, "No hay serie temporal HHI (TAB 3) con esos filtros."))
    hhi
  })
  
  output$t3_plot_b <- renderPlotly({
    ts <- hhi_year_t3()
    plotly::plot_ly(
      data = ts,
      x = ~anio, y = ~hhi01,
      type = "scatter", mode = "lines+markers",
      hovertext = ~tooltip, hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Año", tickmode = "linear", dtick = 1),
        yaxis = list(title = "HHI destinos (0–1)", rangemode = "tozero"),
        margin = list(l=60, r=20, t=10, b=60)
      )
  })
  
  output$t3_plot_e <- renderPlotly({
    df <- hhi_food_t3()
    
    top_dest <- df %>%
      dplyr::group_by(destino_top) %>%
      dplyr::summarise(n_alimentos = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(n_alimentos)) %>%
      dplyr::slice_head(n = 15) %>%
      dplyr::mutate(
        destino_ord = factor(destino_top, levels = rev(destino_top)),
        tooltip = paste0("<b>", destino_top, "</b><br># alimentos: ", n_alimentos, "<extra></extra>")
      )
    
    validate(need(nrow(top_dest) > 0, "Sin datos para Bloque E."))
    
    plotly::plot_ly(
      data = top_dest,
      x = ~n_alimentos,
      y = ~destino_ord,
      type = "bar",
      orientation = "h",
      text = ~n_alimentos,
      textposition = "auto",
      textangle = 0,
      hovertext = ~tooltip,
      hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = "# alimentos (destino principal)", rangemode = "tozero"),
        yaxis = list(title = ""),
        margin = list(l=210, r=20, t=10, b=55)
      )
  })
  
  output$t3_table <- DT::renderDT({
    df <- hhi_food_t3() %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::transmute(
        `#` = rank,
        Grupo = grupo,
        Alimento = alimento,
        `Destino principal` = destino_top,
        `HHI destinos (0–1)` = hhi01_lbl,
        `N destinos` = ndest,
        `N efectivo` = nequiv_lbl,
        `Total (Ton)` = total_lbl
      )
    
    DT::datatable(
      df, rownames = FALSE, escape = TRUE,
      options = dt_opts_lang(pageLength = 10),
      class = "stripe hover order-column compact"
    )
  }, server = FALSE)
  
  # =========================================================
  # TAB 4 — (idéntico a tu lógica; solo hereda DPTO_FOCO_*)
  # =========================================================
  hover_label_opts <- leaflet::labelOptions(
    direction="auto", textsize="12px", sticky=TRUE, opacity=0.95, className="lbl-clean"
  )
  
  years4 <- reactive({
    if (is.null(abast4)) return(integer(0))
    sort(unique(abast4$anio[is.finite(abast4$anio)]), decreasing = FALSE)
  })
  
  output$anio_ui_t4 <- renderUI({
    validate(need(!is.null(abast4), "No se encontró ./data/041_DANE_SIPSA-Abast_4.rds"))
    ys <- years4()
    selectInput("anio_t4", NULL, choices = c("Todos"="Todos", ys),
                selected = if (length(ys)) max(ys) else "Todos")
  })
  
  output$grupo_ui_t4 <- renderUI({
    validate(need(!is.null(abast4), "No se encontró ./data/041_DANE_SIPSA-Abast_4.rds"))
    df <- abast4
    if (!is_all(input$anio_t4)) df <- df %>% dplyr::filter(anio == as.integer(input$anio_t4))
    grupos <- sort(unique(na.omit(df$grupo)))
    selectInput("grupo_t4", NULL, choices = c("Todos"="Todos", grupos), selected = "Todos")
  })
  
  output$alim_ui_t4 <- renderUI({
    validate(need(!is.null(abast4), "No se encontró ./data/041_DANE_SIPSA-Abast_4.rds"))
    df <- abast4
    if (!is_all(input$anio_t4)) df <- df %>% dplyr::filter(anio == as.integer(input$anio_t4))
    if (!is_all(input$grupo_t4)) df <- df %>% dplyr::filter(grupo == input$grupo_t4)
    alims <- sort(unique(na.omit(df$alimento)))
    selectInput("alim_t4", NULL, choices = c("Todos"="Todos", alims), selected = "Todos")
  })
  
  output$cor_ui_t4 <- renderUI({
    validate(need(!is.null(abast4), "No se encontró ./data/041_DANE_SIPSA-Abast_4.rds"))
    df <- abast4
    if (!is_all(input$anio_t4))  df <- df %>% dplyr::filter(anio == as.integer(input$anio_t4))
    if (!is_all(input$grupo_t4)) df <- df %>% dplyr::filter(grupo == input$grupo_t4)
    if (!is_all(input$alim_t4))  df <- df %>% dplyr::filter(alimento == input$alim_t4)
    
    regs <- sort(unique(na.omit(df$region)))
    if (!length(regs)) regs <- "Sin clasificar"
    
    choices <- c("Todos"="Todos", stats::setNames(regs, paste0("Corredor ", regs)))
    selectInput("cor_t4", NULL, choices = choices, selected = "Todos")
  })
  
  datos_filtrados_t4 <- reactive({
    validate(need(!is.null(abast4), "No se cargó 041_DANE_SIPSA-Abast_4.rds"))
    df <- abast4
    if (!is_all(input$anio_t4))  df <- df %>% dplyr::filter(anio == as.integer(input$anio_t4))
    if (!is_all(input$grupo_t4)) df <- df %>% dplyr::filter(grupo == input$grupo_t4)
    if (!is_all(input$alim_t4))  df <- df %>% dplyr::filter(alimento == input$alim_t4)
    if (!is_all(input$cor_t4))   df <- df %>% dplyr::filter(region == input$cor_t4)
    
    df <- df %>% dplyr::filter(!is.na(cod_dpto_o), cod_dpto_o != "", cod_dpto_o != DPTO_FOCO_COD)
    validate(need(nrow(df) > 0, "Sin datos con los filtros seleccionados (TAB 4)."))
    df
  })
  
  ton_by_dpto_t4 <- reactive({
    df <- datos_filtrados_t4() %>%
      dplyr::group_by(cod_dpto_o, region, corredor) %>%
      dplyr::summarise(
        ton_to_atl = sum(ton_a_atlantico, na.rm = TRUE),
        ton_total  = sum(ton_total_origen, na.rm = TRUE),
        pct        = ifelse(ton_total > 0, (ton_to_atl / ton_total) * 100, NA_real_),
        via_nombre = dplyr::first(via_nombre),
        .groups = "drop"
      ) %>%
      dplyr::mutate(cod_dpto_o = pad_dpto(cod_dpto_o))
    
    df
  })
  
  dept_names_tbl <- reactive({
    validate(need(!is.null(dept_sf) && inherits(dept_sf,"sf") && nrow(dept_sf) > 0,
                  "No hay shapefile de departamentos en /data/shp para TAB 4."))
    dept_sf %>% sf::st_drop_geometry() %>%
      dplyr::select(cod_dpto, dpto_nm) %>%
      dplyr::distinct()
  })
  
  gpkg_path_t4 <- reactive({
    if (length(gpkg_files) == 0) return(NA_character_)
    if (!is.na(gpkg_default) && file.exists(gpkg_default)) return(gpkg_default)
    gpkg_files[1]
  })
  
  gpkg_ctx_t4  <- reactive({ read_gpkg_layers(gpkg_path_t4()) })
  rutas_sf_t4  <- reactive({ normalize_routes(gpkg_ctx_t4()$rutas) })
  dptos_ctx_sf_t4 <- reactive({
    dpt <- normalize_dptos(gpkg_ctx_t4()$dptos)
    if (is.null(dpt) || !inherits(dpt,"sf") || nrow(dpt) == 0) dept_sf else dpt
  })
  caps_sf_t4   <- reactive({ normalize_caps(gpkg_ctx_t4()$caps) })
  
  rutas_con_ton_t4 <- reactive({
    rts <- rutas_sf_t4()
    validate(need(!is.null(rts) && inherits(rts,"sf") && nrow(rts) > 0,
                  "No hay capa 'rutas_baq_a_capitales' en el .gpkg o está vacía (TAB 4)."))
    
    out <- rts %>%
      dplyr::mutate(.row_id = dplyr::row_number()) %>%
      dplyr::left_join(ton_by_dpto_t4(), by = c("dpto_id" = "cod_dpto_o")) %>%
      dplyr::left_join(dept_names_tbl(), by = c("dpto_id" = "cod_dpto")) %>%
      dplyr::mutate(
        ton_to_atl = as.numeric(ton_to_atl),
        ton_total  = as.numeric(ton_total),
        pct        = as.numeric(pct),
        ton_route  = ifelse(is.na(ton_to_atl) | ton_to_atl < 0, 0, ton_to_atl),
        dpto_show  = dplyr::if_else(is.na(dpto_nm) | dpto_nm == "",
                                    paste0("Dpto ", dpto_id),
                                    dpto_nm),
        region   = region_label(region),
        corredor = paste0("Corredor ", region_label(region)),
        via_nombre = paste0("Corredor ", region_label(region)),
        region_col = dplyr::coalesce(unname(region_palette_map[region_label(region)]), region_palette_map[["Sin clasificar"]]),
        route_id = paste0(dpto_id, "_", .row_id)
      ) %>%
      dplyr::select(route_id, dpto_id, dpto_show, region, corredor, via_nombre, region_col, ton_route, ton_total, pct, geometry)
    
    out
  })
  
  selected_routes_t4 <- reactiveVal(character(0))
  
  observe({
    rts <- tryCatch(rutas_con_ton_t4(), error = function(e) NULL)
    if (is.null(rts) || nrow(rts) == 0) {
      selected_routes_t4(character(0))
    } else {
      cur <- selected_routes_t4()
      keep <- intersect(cur, rts$route_id)
      if (!identical(cur, keep)) selected_routes_t4(keep)
    }
  })
  
  observeEvent(input$t4_map_routes_shape_click, {
    click <- input$t4_map_routes_shape_click
    req(click$id)
    cur <- selected_routes_t4()
    id  <- as.character(click$id)
    if (id %in% cur) selected_routes_t4(setdiff(cur, id)) else selected_routes_t4(c(cur, id))
  })
  
  observeEvent(input$clear_routes_t4, { selected_routes_t4(character(0)) })
  
  output$sel_routes_ui_t4 <- renderUI({
    if (length(gpkg_files) == 0) return(NULL)
    n <- length(selected_routes_t4())
    div(
      class="sel-row",
      div(class="sel-chip",
          if (n == 0) "Selección de rutas: ninguna (mostrando todas)"
          else paste0("Rutas seleccionadas: ", n, " (clic para quitar/agregar)")),
      actionButton("clear_routes_t4", "Limpiar selección", class="btn btn-outline-secondary btn-clear")
    )
  })
  
  datos_base_t4_sin_cor <- reactive({
    validate(need(!is.null(abast4), "No se cargó 041_DANE_SIPSA-Abast_4.rds"))
    df <- abast4
    if (!is_all(input$anio_t4))  df <- df %>% dplyr::filter(anio == as.integer(input$anio_t4))
    if (!is_all(input$grupo_t4)) df <- df %>% dplyr::filter(grupo == input$grupo_t4)
    if (!is_all(input$alim_t4))  df <- df %>% dplyr::filter(alimento == input$alim_t4)
    
    df <- df %>% dplyr::filter(!is.na(cod_dpto_o), cod_dpto_o != "", cod_dpto_o != DPTO_FOCO_COD)
    validate(need(nrow(df) > 0, "Sin datos con los filtros seleccionados (TAB 4)."))
    df
  })
  
  kpis_t4 <- reactive({
    base <- datos_base_t4_sin_cor()
    
    total_ton_all <- sum(base$ton_a_atlantico, na.rm = TRUE)
    total_ton_all <- ifelse(is.finite(total_ton_all), total_ton_all, NA_real_)
    
    if (is_all(input$cor_t4)) {
      cor_lbl <- "Todos los corredores"
      cor_ton <- total_ton_all
    } else {
      cor_lbl <- paste0("Corredor ", input$cor_t4)
      cor_ton <- sum(base$ton_a_atlantico[base$region == input$cor_t4], na.rm = TRUE)
      cor_ton <- ifelse(is.finite(cor_ton), cor_ton, NA_real_)
    }
    
    lost_pct <- ifelse(is.finite(total_ton_all) && total_ton_all > 0 && is.finite(cor_ton),
                       (cor_ton / total_ton_all) * 100, NA_real_)
    
    list(
      cor_lbl = cor_lbl,
      cor_ton = cor_ton,
      total_ton_all = total_ton_all,
      lost_pct = lost_pct
    )
  })
  
  output$t4_kpis_ui <- renderUI({
    k <- kpis_t4()
    
    ton_lbl  <- ifelse(is.finite(k$cor_ton), fmt_ton_co(k$cor_ton, 1), "NA")
    pct_lbl  <- ifelse(is.finite(k$lost_pct), fmt_pct_co(k$lost_pct, 2), "NA")
    tot_lbl  <- ifelse(is.finite(k$total_ton_all), fmt_ton_co(k$total_ton_all, 1), "NA")
    
    tagList(
      div(class="kpi-title", paste0("Toneladas hacia ", DPTO_FOCO_NOMBRE, " — ", k$cor_lbl)),
      div(class="kpi-value", paste0(ton_lbl, " Ton")),
      div(class="kpi-sub", paste0("Total (todos los corredores): ", tot_lbl, " Ton")),
      
      div(class="kpi-divider"),
      
      div(class="kpi-title", "Si esa vía fallara, se dejaría de percibir:"),
      div(class="kpi-value", pct_lbl),
      div(class="kpi-sub", "% del total de alimentos (Ton) bajo los filtros actuales")
    )
  })
  
  output$t4_map_routes <- renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::setView(lng = -74.0, lat = 4.6, zoom = 5)
  })
  safe_outputOptions("t4_map_routes", suspendWhenHidden = FALSE)
  
  draw_map_routes_t4 <- function(){
    validate(need(!is.null(dept_sf) && inherits(dept_sf,"sf") && nrow(dept_sf) > 0,
                  "No hay shapefile de departamentos para dibujar (TAB 4)."))
    
    dpt <- tryCatch(dptos_ctx_sf_t4(), error = function(e) NULL)
    rts <- tryCatch(rutas_con_ton_t4(), error = function(e) NULL)
    cps <- tryCatch(caps_sf_t4(), error = function(e) NULL)
    
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
    
    bb <- sf::st_bbox(dpt2)
    
    proxy <- leaflet::leafletProxy("t4_map_routes") %>%
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
      )
    
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
    
    sel <- selected_routes_t4()
    rts$is_sel <- if (length(sel) == 0) TRUE else (rts$route_id %in% sel)
    any_sel <- length(sel) > 0
    rts$line_op <- ifelse(rts$is_sel, 0.90, ifelse(any_sel, 0.18, 0.70))
    rts$line_w  <- ifelse(rts$is_sel, rts$w * 1.25, ifelse(any_sel, pmax(1.2, rts$w * 0.75), rts$w))
    
    rts$region_lbl <- region_label(rts$region)
    rts$region_col <- dplyr::coalesce(unname(region_palette_map[rts$region_lbl]), region_palette_map[["Sin clasificar"]])
    
    proxy %>%
      leaflet::addPolylines(
        data = rts,
        group="rts",
        layerId = ~route_id,
        color   = ~region_col,
        weight  = ~line_w,
        opacity = ~line_op,
        label = ~paste0("Corredor ", region_lbl, " — ", fmt_ton_co(ton_route, 1), " Ton"),
        labelOptions = hover_label_opts,
        popup = ~paste0(
          "<strong>Corredor ", region_lbl, "</strong>",
          "<br><strong>Departamento origen:</strong> ", dpto_show,
          "<br><strong>Ton hacia ", DPTO_FOCO_NOMBRE, ":</strong> ", fmt_ton_co(ton_route, 1),
          ifelse(is.na(ton_total), "", paste0("<br><strong>Total enviado por el dpto (den):</strong> ", fmt_ton_co(ton_total, 1))),
          ifelse(is.na(pct), "", paste0("<br><strong>% importancia:</strong> ", fmt_pct_co(pct, 2))),
          "<br><small>Tip: clic para seleccionar/deseleccionar.</small>"
        )
      ) %>%
      leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    
    regs_present <- sort(unique(na.omit(rts$region_lbl)))
    if (length(regs_present)) {
      cols <- unname(region_palette_map[regs_present])
      cols[is.na(cols)] <- region_palette_map[["Sin clasificar"]]
      leaflet::leafletProxy("t4_map_routes") %>%
        leaflet::addLegend(
          position = "bottomright",
          colors   = cols,
          labels   = paste0("Corredor ", regs_present),
          title    = "Corredores (Región)"
        )
    }
    
    if (!is.null(cps) && inherits(cps,"sf") && nrow(cps) > 0) {
      cps <- cps %>% sf::st_make_valid() %>% sf::st_zm(drop=TRUE, what="ZM")
      leaflet::leafletProxy("t4_map_routes") %>%
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
  
  observeEvent(
    list(input$anio_t4, input$grupo_t4, input$alim_t4, input$cor_t4, selected_routes_t4()),
    { draw_map_routes_t4() },
    ignoreInit = FALSE
  )
}

shinyApp(ui, server)

