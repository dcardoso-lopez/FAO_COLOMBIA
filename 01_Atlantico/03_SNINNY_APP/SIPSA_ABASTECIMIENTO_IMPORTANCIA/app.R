# =========================================================
# SIPSA_ABASTECIMIENTO_IMPORTANCIA — SOLO MAPA (% IMPORTANCIA)
# (MISMA BASE 041_DANE_SIPSA-Abast.rds)
#
# - ✅ Atlántico (foco)
# - ✅ Flujos HACIA Atlántico (COD_DANE_DPTO_D == 08)
# - ✅ Mapa por ORIGEN: % = Ton_origen / Total_ton_destino * 100
#     (denominador = TOTAL hacia el DEPARTAMENTO_D, no solo mapeable)
# - ✅ Filtros: Año, Mes (nombres), Grupo, Alimento
# - ✅ Join mapa por código: base COD_DANE_DPTO_O vs shapefile DPTO_CCGEO (o DPTO_CCDGO)
# - ✅ Mapa se dibuja (patrón observeEvent)
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
rds_path <- file.path(data_dir, "041_DANE_SIPSA-Abast.rds")
stopifnot(file.exists(rds_path))

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
# Cargar RDS y mapear columnas
# =========================================================
sipsa_raw <- readRDS(rds_path)
sipsa <- janitor::clean_names(sipsa_raw)
nms  <- names(sipsa)

ycol <- req_col(nms, c("ano","anio","year"), "AÑO")
mcol <- req_col(nms, c("mes","month"), "MES")
gcol <- req_col(nms, c("grupo","grupo_alimento","grupo_alimentos","grupo_de_alimento"), "GRUPO")
pcol <- req_col(nms, c("alimento","producto","item","articulo","artículo"), "ALIMENTO/PRODUCTO")
qcol <- req_col(nms, c("cantkg_total","cant_kg_total","cantidad_kg","cantidadkg","cantkg","kg_total","cant_total_kg"), "CANTIDAD KG")

dep_o_col <- pick_first(nms, c("departamento_o","depto_o","departamento_origen","depto_origen"))

dpto_code_d_col <- req_col(
  nms,
  c("cod_dane_dpto_d","dane_cod_dpto_d","dane_cod_dpto","cod_dane_dpto","cod_dpto_d","cod_dpto"),
  "COD_DANE_DPTO_D"
)
dpto_code_o_col <- req_col(
  nms,
  c("cod_dane_dpto_o","dane_cod_dpto_o","cod_dpto_o","cod_dpto_origen","cod_dane_dpto_origen","dane_cod_dpto_origen"),
  "COD_DANE_DPTO_O"
)

base_sipsa <- sipsa %>%
  transmute(
    anio = suppressWarnings(as.integer(.data[[ycol]])),
    mes  = suppressWarnings(as.integer(.data[[mcol]])),
    cod_dpto_d = pad_dpto(.data[[dpto_code_d_col]]),
    cod_dpto_o = pad_dpto(.data[[dpto_code_o_col]]),
    departamento_o = if (!is.na(dep_o_col)) title_case_es(.data[[dep_o_col]]) else NA_character_,
    grupo    = title_case_es(.data[[gcol]]),
    alimento = title_case_es(.data[[pcol]]),
    kg       = parse_num_co(.data[[qcol]])
  ) %>%
  filter(is.finite(anio), anio >= 2018) %>%
  filter(!is.na(departamento_o), str_trim(departamento_o) != "") %>%
  filter(is.finite(kg), kg > 0) %>%
  mutate(ton = kg/1000)

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
        grid-template-columns: repeat(4, minmax(220px, 1fr));
        column-gap:16px; row-gap:10px; align-items:end;
      }
      @media (max-width: 1000px){ .filters-grid{ grid-template-columns: 1fr 1fr; } }
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
        div(class="filter", div(class="filter-label","¿En qué mes?"), uiOutput("mes_ui")),
        div(class="filter", div(class="filter-label","¿En qué grupo alimenticio?"), uiOutput("grupos_ui")),
        div(class="filter", div(class="filter-label","¿En qué alimento?"), uiOutput("alimentos_ui"))
      )
    ),
    
    div(
      class="card",
      div(class="card-title",
          strong(paste0("¿Qué porcentaje del abastecimiento que llega a ", DPTO_FOCO_NOMBRE, " proviene de cada departamento?"))),
      leafletOutput("map_imp"),
      div(class="map-note","Nota: cuartiles (4 clases) con valores > 0. Dptos sin información en gris."),
      div(class="dl-under", downloadButton("dl_png_map_imp", "PNG — Mapa (simple)"))
    )
  )
)

# =========================================================
# SERVER
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
  
  output$anio_ui <- renderUI({
    selectInput("anio", NULL, choices = c("Todos"="Todos", years),
                selected = if (length(years)) max(years) else "Todos")
  })
  output$mes_ui <- renderUI({
    selectInput("mes", NULL, choices = month_choices, selected = "Todos")
  })
  
  base_foco <- reactive({
    df <- base_sipsa %>% filter(cod_dpto_d == DPTO_FOCO_COD)
    if (!is.null(input$anio) && input$anio != "Todos") df <- df %>% filter(anio == as.integer(input$anio))
    if (!is.null(input$mes)  && input$mes  != "Todos") df <- df %>% filter(mes  == as.integer(input$mes))
    df
  })
  
  output$grupos_ui <- renderUI({
    grupos <- sort(unique(na.omit(base_foco()$grupo)))
    pickerInput(
      "grupos", NULL,
      choices  = c("Todos", grupos),
      selected = "Todos",
      multiple = TRUE,
      options  = list(`live-search`=TRUE, `actions-box`=TRUE, size=8)
    )
  })
  
  output$alimentos_ui <- renderUI({
    df <- base_foco()
    df <- filter_multi(df, "grupo", input$grupos)
    alimentos <- sort(unique(na.omit(df$alimento)))
    pickerInput(
      "alimentos", NULL,
      choices  = c("Todos", alimentos),
      selected = "Todos",
      multiple = TRUE,
      options  = list(`live-search`=TRUE, `actions-box`=TRUE, size=8)
    )
  })
  
  datos_imp <- reactive({
    df <- base_foco()
    df <- filter_multi(df, "grupo",    input$grupos)
    df <- filter_multi(df, "alimento", input$alimentos)
    validate(need(nrow(df) > 0, "Sin datos con los filtros seleccionados."))
    df
  })
  
  badge <- reactive({
    yr <- if (!is.null(input$anio) && input$anio != "Todos") as.character(input$anio) else "Todos"
    ms <- if (!is.null(input$mes)  && input$mes  != "Todos") mes_nombre[as.integer(input$mes)] else "Todos"
    htmltools::HTML(sprintf(
      '<div style="background:#fff;padding:6px 10px;border-radius:8px;
                   box-shadow:0 1px 6px rgba(0,0,0,.15);font-size:12px;line-height:1.3;">
         <b>Destino:</b> %s<br>
         <b>Año:</b> %s &nbsp; <b>Mes:</b> %s
       </div>', htmltools::htmlEscape(DPTO_FOCO_NOMBRE), yr, ms
    ))
  })
  
  # =========================================================
  # ✅ FIX: % con base en el TOTAL hacia el DEPARTAMENTO_D (destino)
  #     (denominador NO se restringe a "mapeable"; solo el numerador se mapea)
  # =========================================================
  mapa_imp_sf <- reactive({
    df <- datos_imp()
    
    # Normaliza código de origen y elimina NA (sin código no se puede asignar a dpto)
    df2 <- df %>%
      mutate(cod_dpto = pad_dpto(cod_dpto_o)) %>%
      filter(!is.na(cod_dpto))
    
    # ✅ Denominador: TOTAL que llega al destino (Atlántico) con los filtros activos
    #    (NO solo mapeable)
    total_destino <- sum(df2$ton, na.rm = TRUE)
    
    # Numerador (solo lo mapeable para poder dibujarlo)
    agg_map <- df2 %>%
      filter(cod_dpto %in% dept_sf$cod_dpto) %>%
      group_by(cod_dpto) %>%
      summarise(ton = sum(ton, na.rm = TRUE), .groups = "drop") %>%
      mutate(pct = ifelse(total_destino > 0, ton / total_destino * 100, NA_real_))
    
    dept_sf %>%
      left_join(agg_map, by = "cod_dpto") %>%
      mutate(ton = as.numeric(ton), pct = as.numeric(pct))
  })
  
  output$map_imp <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -74.0, lat = 4.6, zoom = 5)
  })
  outputOptions(output, "map_imp", suspendWhenHidden = FALSE)
  
  draw_map_imp <- function(){
    mdat <- tryCatch(mapa_imp_sf(), error = function(e) NULL)
    if (is.null(mdat) || !inherits(mdat,"sf") || nrow(mdat) == 0) return(invisible(NULL))
    
    mdat <- mdat %>% sf::st_make_valid() %>% sf::st_zm(drop=TRUE, what="ZM")
    mdat <- tryCatch(sf::st_cast(mdat, "MULTIPOLYGON", warn=FALSE), error=function(e) mdat)
    mdat <- mdat[!sf::st_is_empty(mdat$geometry), , drop=FALSE]
    if (nrow(mdat) == 0) return(invisible(NULL))
    
    pal <- palBin4(mdat$pct)
    bb  <- sf::st_bbox(mdat)
    
    leafletProxy("map_imp", data = mdat) %>%
      clearGroup("poly") %>% clearControls() %>%
      addPolygons(
        group="poly",
        fillColor = ~ifelse(is.na(pct) | pct <= 0, "#bdbdbd", pal(pct)),
        fillOpacity = 0.90,
        color = "#666",
        weight = 0.7,
        label = ~ifelse(
          is.na(pct) | pct <= 0,
          sprintf("%s — Sin información", dpto_nm),
          sprintf("%s — %s", dpto_nm, fmt_pct_co(pct, 1))
        ),
        popup = ~paste0(
          "<strong>Departamento (origen): </strong>", dpto_nm,
          ifelse(is.na(pct), "", paste0("<br><strong>% hacia ", DPTO_FOCO_NOMBRE, ":</strong> ", fmt_pct_co(pct, 1))),
          ifelse(is.na(ton), "", paste0("<br><strong>Toneladas: </strong>", fmt_ton_co(ton, 1)))
        ),
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
        labelOptions = hover_label_opts
      ) %>%
      addLegend(
        position="bottomright",
        pal=pal,
        values=~ifelse(is.na(pct)|pct<=0, NA, pct),
        title="% de abastecimiento",
        labFormat=legend_lab_pct(),
        na.label="Sin información"
      ) %>%
      addControl(badge(), position="topright") %>%
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  }
  
  observeEvent(list(input$anio, input$mes, input$grupos, input$alimentos), {
    draw_map_imp()
  }, ignoreInit = FALSE)
  
  output$dl_png_map_imp <- downloadHandler(
    filename = function() paste0("SIPSA_importancia_", DPTO_FOCO_NOMBRE, "_", Sys.Date(), ".png"),
    content  = function(file){
      shp <- isolate(mapa_imp_sf())
      bdg <- isolate(badge())
      
      if (is.null(shp) || !inherits(shp,"sf") || nrow(shp) == 0) {
        widget <- leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(-74.0, 4.6, 5)
      } else {
        pal <- palBin4(shp$pct)
        widget <- leaflet(shp, options = leafletOptions(zoomControl=FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons(
            fillColor = ~ifelse(is.na(pct) | pct <= 0, "#bdbdbd", pal(pct)),
            fillOpacity = 0.90,
            color = "#666",
            weight = 0.7
          ) %>%
          addLegend(
            position="bottomright",
            pal=pal,
            values=~ifelse(is.na(pct)|pct<=0, NA, pct),
            title="% de abastecimiento",
            labFormat=legend_lab_pct(),
            na.label="Sin información"
          ) %>%
          addControl(bdg, position="topright")
      }
      
      tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget, tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, file = file, vwidth = 1200, vheight = 820, zoom = 2)
    }
  )
}

shinyApp(ui, server)



