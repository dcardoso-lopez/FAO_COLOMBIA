# app.R
# =========================================================
# SIPSA_ABASTECIMIENTO_IMPORTANCIA — SOLO MAPA (% IMPORTANCIA)
#
# BASE (agregada como tu screenshot):
#   ano, Grupo, (Alimento puede existir), COD_DANE_DPTO_O, DEPARTAMENTO_O,
#   kg_total_origen, kg_a_atlantico, pct_importancia
#
# ENFOQUE:
# - ✅ Atlántico (foco) destino implícito (ya viene en kg_a_atlantico)
# - ✅ Mapa por ORIGEN:
#     % = kg_a_atlantico / kg_total_origen * 100
# - ✅ Filtros: Año, Grupo
# - ✅ SIN filtro de ALIMENTO
# - ✅ SIN filtro de MES
# - ✅ Join mapa por código: COD_DANE_DPTO_O vs shapefile DPTO_CCGEO/DPTO_CCDGO
# - ✅ FIX error .xts_chob: namespace explícito leaflet:: + values como vector
# =========================================================

DPTO_FOCO_NOMBRE <- "Atlántico"
DPTO_FOCO_COD    <- "08"
APP_TITLE <- paste0("SIPSA — Importancia del abastecimiento hacia ", DPTO_FOCO_NOMBRE)

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

# =========================================================
# Rutas robustas
# =========================================================
app_root <- tryCatch({
  of <- sys.frame(1)$ofile
  if (!is.null(of)) dirname(normalizePath(of, winslash = "/", mustWork = TRUE))
  else normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}, error = function(e){
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
})

data_dir <- file.path(app_root, "data")

# ✅ intenta varios nombres comunes (ajusta si tu .rds se llama distinto)
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

# =========================================================
# Helpers
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

pad_dpto <- function(x){
  x <- as.character(x)
  x <- str_replace_all(x, "\\D", "")
  x <- ifelse(nchar(x) == 0, NA_character_, x)
  str_pad(x, width = 2, side = "left", pad = "0")
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

# =========================================================
# Paleta/bins 4 clases (para %)
# =========================================================
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

format_short <- function(x){
  scales::number(x, accuracy = 0.1, big.mark=".", decimal.mark=",")
}

legend_lab_pct <- function(){
  function(type, cuts, p){
    if (length(cuts) <= 1) return(character())
    lows  <- head(cuts, -1)
    highs <- tail(cuts, -1)
    pref <- c("", rep("> ", length(lows) - 1))
    paste0(pref, format_short(lows), "% – ", format_short(highs), "%")
  }
}

# =========================================================
# Cargar RDS (BASE AGREGADA) y mapear columnas
# =========================================================
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
  transmute(
    anio = suppressWarnings(as.integer(.data[[ycol]])),
    grupo = title_case_es(.data[[gcol]]),
    cod_dpto_o = pad_dpto(.data[[cod_o_col]]),
    departamento_o = if (!is.na(dep_o_col)) title_case_es(.data[[dep_o_col]]) else NA_character_,
    kg_total_origen = parse_num_co(.data[[kg_tot_col]]),
    kg_a_atlantico  = parse_num_co(.data[[kg_atl_col]]),
    pct_importancia = if (!is.na(pct_col)) suppressWarnings(as.numeric(.data[[pct_col]])) else NA_real_
  ) %>%
  filter(is.finite(anio), anio >= 2018) %>%
  filter(!is.na(cod_dpto_o), cod_dpto_o != "") %>%
  filter(is.finite(kg_total_origen), kg_total_origen > 0) %>%
  mutate(
    kg_a_atlantico = dplyr::coalesce(kg_a_atlantico, 0),
    pct = ifelse(kg_total_origen > 0, (kg_a_atlantico / kg_total_origen) * 100, NA_real_),
    ton_den = kg_total_origen / 1000,
    ton_num = kg_a_atlantico  / 1000
  ) %>%
  group_by(anio, grupo, cod_dpto_o) %>%
  summarise(
    departamento_o = dplyr::first(na.omit(departamento_o)) %||% NA_character_,
    ton_den = sum(ton_den, na.rm = TRUE),
    ton_num = sum(ton_num, na.rm = TRUE),
    pct = ifelse(ton_den > 0, (ton_num / ton_den) * 100, NA_real_),
    .groups = "drop"
  )

# =========================================================
# Shapefile Departamentos (DPTO_CCGEO / DPTO_CCDGO)
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
    c("dpto_ccgeo","dpto_ccdgo","cod_depto","cod_dpto","codigo_depto","dpto_cod"),
    "DPTO_CCGEO (o DPTO_CCDGO)"
  )
  
  ncol <- pick_first(names(obj), c("dpto_cnmbr","departamento","nom_dpto","name_1","name","dpto"))
  
  if (is.na(sf::st_crs(obj))) sf::st_crs(obj) <- 4326
  
  out <- obj %>%
    transmute(
      cod_dpto = pad_dpto(.data[[ccol]]),
      dpto_nm  = if (!is.na(ncol)) title_case_es(.data[[ncol]]) else pad_dpto(.data[[ccol]]),
      geometry = geometry
    ) %>%
    sf::st_make_valid() %>%
    sf::st_zm(drop = TRUE, what = "ZM") %>%
    sf::st_transform(3116) %>%
    group_by(cod_dpto) %>%
    summarise(dpto_nm = dplyr::first(dpto_nm), geometry = sf::st_union(geometry), .groups="drop") %>%
    sf::st_transform(4326)
  
  out
}
dept_sf <- load_dept_sf_only(data_dir)

# =========================================================
# UI
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
        --map-h: 600px;
      }
      body{ background:#ffffff; }
      .wrap{ max-width:1360px; margin:0 auto; padding:16px 20px 32px; }
      h2#app-title{ text-align:center; margin-top:10px; margin-bottom:10px; font-weight:800; letter-spacing:.3px; }

      .filters{
        background:#fff; border:1px solid var(--accent-border); border-radius:16px;
        padding:14px 16px; margin-bottom:16px; box-shadow:0 4px 14px rgba(0,0,0,.06);
        width:100%; overflow: visible; position: relative; z-index: 20;
      }
      .filters-grid{
        width:100%;
        display:grid;
        grid-template-columns: repeat(2, minmax(260px, 1fr));
        column-gap:16px; row-gap:10px; align-items:end;
      }
      @media (max-width: 650px){ .filters-grid{ grid-template-columns: 1fr; } }

      .filter-label{
        font-weight:500; font-size:14px; margin-bottom:6px; color:#000;
        white-space: normal; line-height: 1.1; min-height: 28px;
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
      .bootstrap-select .dropdown-menu{ z-index: 99999 !important; }
      .selectize-dropdown{ z-index: 99999 !important; }
      .leaflet-container{ z-index: 1 !important; }

      .card{
        background:#fff; border:1px solid var(--accent-border) !important;
        border-radius:16px; padding:12px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
      }
      .card-title{ font-weight:800; font-size:16px; margin-bottom:8px; color:#111827; }

      #map_imp{ height: var(--map-h) !important; }

      .leaflet-tooltip.lbl-clean{
        background: rgba(255,255,255,.92); border: 1px solid #e6e6e6; border-radius: 6px;
        padding: 4px 6px; color: #222; font-weight: 700; box-shadow: 0 1px 4px rgba(0,0,0,.08);
      }
      .map-note{ font-size:12px; color:#4b5563; margin-top:6px; }
      .dl-under{ margin-top:8px; text-align:right; }
    "))
  ),
  
  div(
    class="wrap",
    h2(APP_TITLE, id="app-title"),
    
    div(
      class="filters",
      div(
        class="filters-grid",
        div(class="filter", div(class="filter-label","¿Qué año analizamos?"), uiOutput("anio_ui")),
        div(class="filter", div(class="filter-label","¿En qué grupo alimenticio?"), uiOutput("grupos_ui"))
      )
    ),
    
    div(
      class="card",
      div(class="card-title",
          strong(paste0("¿Qué porcentaje de lo que envía cada departamento termina en ", DPTO_FOCO_NOMBRE, "?"))),
      leafletOutput("map_imp"),
      div(class="map-note","Nota: cuartiles (4 clases) con valores > 0. Dptos sin información o con 0% en gris. Atlántico (origen) se marca como NA (no aplica)."),
      div(class="dl-under", downloadButton("dl_png_map_imp", "PNG — Mapa (simple)"))
    )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  
  # (Opcional “blindaje” adicional contra funciones enmascaradas)
  addLegend   <- leaflet::addLegend
  addControl  <- leaflet::addControl
  addPolygons <- leaflet::addPolygons
  fitBounds   <- leaflet::fitBounds
  clearGroup  <- leaflet::clearGroup
  clearControls <- leaflet::clearControls
  
  years <- sort(unique(base_sipsa$anio[is.finite(base_sipsa$anio)]))
  
  hover_label_opts <- leaflet::labelOptions(
    direction="auto", textsize="12px", sticky=TRUE, opacity=0.95, className="lbl-clean"
  )
  
  output$anio_ui <- renderUI({
    selectInput("anio", NULL, choices = c("Todos"="Todos", years),
                selected = if (length(years)) max(years) else "Todos")
  })
  
  base_all <- reactive({
    df <- base_sipsa
    if (!is.null(input$anio) && input$anio != "Todos") df <- df %>% filter(anio == as.integer(input$anio))
    df
  })
  
  output$grupos_ui <- renderUI({
    grupos <- sort(unique(na.omit(base_all()$grupo)))
    pickerInput(
      "grupos", NULL,
      choices  = c("Todos", grupos),
      selected = "Todos",
      multiple = TRUE,
      options  = list(`live-search`=TRUE, `actions-box`=TRUE, size=8)
    )
  })
  
  datos_filtrados <- reactive({
    df <- base_all()
    df <- filter_multi(df, "grupo", input$grupos)
    validate(need(nrow(df) > 0, "Sin datos con los filtros seleccionados."))
    df
  })
  
  badge <- reactive({
    yr <- if (!is.null(input$anio) && input$anio != "Todos") as.character(input$anio) else "Todos"
    htmltools::HTML(sprintf(
      '<div style="background:#fff;padding:6px 10px;border-radius:8px;
                   box-shadow:0 1px 6px rgba(0,0,0,.15);font-size:12px;line-height:1.3;">
         <b>Destino:</b> %s<br>
         <b>Año:</b> %s
       </div>', htmltools::htmlEscape(DPTO_FOCO_NOMBRE), yr
    ))
  })
  
  mapa_imp_sf <- reactive({
    # Agrega por ORIGEN (si hay múltiples grupos seleccionados)
    agg <- datos_filtrados() %>%
      group_by(cod_dpto_o) %>%
      summarise(
        ton_den = sum(ton_den, na.rm = TRUE),
        ton_num = sum(ton_num, na.rm = TRUE),
        pct = ifelse(ton_den > 0, (ton_num / ton_den) * 100, NA_real_),
        .groups = "drop"
      ) %>%
      filter(cod_dpto_o %in% dept_sf$cod_dpto)
    
    out <- dept_sf %>%
      left_join(agg, by = c("cod_dpto" = "cod_dpto_o")) %>%
      mutate(
        ton_num = as.numeric(ton_num),
        ton_den = as.numeric(ton_den),
        pct     = as.numeric(pct)
      )
    
    # Atlántico como ORIGEN -> NA (no aplica)
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
  
  # ✅ FIX: leaflet:: + values como vector (no fórmula) para evitar .xts_chob
  draw_map_imp <- function(){
    mdat <- tryCatch(mapa_imp_sf(), error = function(e) NULL)
    if (is.null(mdat) || !inherits(mdat,"sf") || nrow(mdat) == 0) return(invisible(NULL))
    
    mdat <- mdat %>% sf::st_make_valid() %>% sf::st_zm(drop=TRUE, what="ZM")
    mdat <- tryCatch(sf::st_cast(mdat, "MULTIPOLYGON", warn=FALSE), error=function(e) mdat)
    mdat <- mdat[!sf::st_is_empty(mdat$geometry), , drop=FALSE]
    if (nrow(mdat) == 0) return(invisible(NULL))
    
    # vector para leyenda (NA para 0 o NA)
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
          ifelse(is.na(pct_plot), "", paste0("<br><strong>% de sus envíos hacia ", DPTO_FOCO_NOMBRE, ":</strong> ", fmt_pct_co(pct_plot, 1))),
          ifelse(is.na(ton_num), "", paste0("<br><strong>Ton hacia ", DPTO_FOCO_NOMBRE, " (numerador): </strong>", fmt_ton_co(ton_num, 1))),
          ifelse(is.na(ton_den), "", paste0("<br><strong>Total enviado por el origen (denominador): </strong>", fmt_ton_co(ton_den, 1)))
        ),
        highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
        labelOptions = hover_label_opts
      ) %>%
      leaflet::addLegend(
        position="bottomright",
        pal=pal,
        values=mdat$pct_plot,  # <- vector, NO fórmula
        title=paste0("% de envíos a ", DPTO_FOCO_NOMBRE),
        labFormat=legend_lab_pct(),
        na.label="0% / NA"
      ) %>%
      leaflet::addControl(badge(), position="topright") %>%
      leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  }
  
  observeEvent(list(input$anio, input$grupos), {
    draw_map_imp()
  }, ignoreInit = FALSE)
  
  output$dl_png_map_imp <- downloadHandler(
    filename = function() paste0("SIPSA_importancia_", DPTO_FOCO_NOMBRE, "_", Sys.Date(), ".png"),
    content  = function(file){
      shp <- isolate(mapa_imp_sf())
      bdg <- isolate(badge())
      
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
            values=shp$pct_plot,  # <- vector, NO fórmula
            title=paste0("% de envíos a ", DPTO_FOCO_NOMBRE),
            labFormat=legend_lab_pct(),
            na.label="0% / NA"
          ) %>%
          leaflet::addControl(bdg, position="topright")
      }
      
      tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget, tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, file = file, vwidth = 1200, vheight = 820, zoom = 2)
    }
  )
}

shinyApp(ui, server)



