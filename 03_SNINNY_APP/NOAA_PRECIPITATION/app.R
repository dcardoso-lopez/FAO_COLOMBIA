# =========================================================
# Shiny App — NOAA Precipitación (Anual / Mensual)
# Mapa (dpto/municipio) + Serie (SIN MM) + Ranking Top-10
# + Anomalía (velas de MM de la anomalía, todo el periodo, base = 3 primeros años)
# Etiquetas con tildes correctas (NARIÑO, CAQUETÁ, etc.)
# =========================================================

# 1) Paquetes
pkgs <- c("shiny","bslib","dplyr","readr","stringi","sf","leaflet",
          "plotly","ggplot2","htmltools","webshot2","htmlwidgets",
          "ragg","glue","scales","tibble","zoo","lubridate")
suppressPackageStartupMessages(invisible(sapply(pkgs, require, character.only = TRUE)))
options(stringsAsFactors = FALSE, scipen = 999)
sf::sf_use_s2(FALSE)
try(Sys.setlocale("LC_CTYPE","es_ES.UTF-8"), silent = TRUE)

# 2) Rutas (ajusta si difieren)
APP_ROOT      <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP"
NOAA_DIR      <- file.path(APP_ROOT, "NOAA_PRECIPITATION")
NOAA_DATA_DIR <- file.path(NOAA_DIR, "data")
SHP_DIR       <- file.path(APP_ROOT, "NOAA_PRECIPITATION/data/shp")  # Re-uso de shapes

# Autodetección del RDS NOAA (usa el nombre exacto y fallback si no existe)
DATA_RDS <- file.path(NOAA_DATA_DIR, "131_NOAA_Precipitación.rds")
if (!file.exists(DATA_RDS)) {
  rds_cands <- list.files(NOAA_DATA_DIR, pattern = "\\.rds$", full.names = TRUE, recursive = FALSE)
  if (length(rds_cands)) DATA_RDS <- rds_cands[1]
}
if (!file.exists(DATA_RDS)) stop("No encuentro RDS de NOAA en: ", NOAA_DATA_DIR)

# ---------- Helpers ----------
up_es <- function(x){
  x <- trimws(as.character(x))
  x <- iconv(x, from = "", to = "UTF-8")
  toupper(x)
}
safe_chr  <- function(x) if (is.null(x)) "" else as.character(x)
find_shp  <- function(files, key){ i <- grep(key, basename(files), ignore.case = TRUE); if (!length(i)) NA_character_ else files[i[1]] }
nombre_mes <- function(m) c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")[as.integer(m)]
pal5_vec <- grDevices::colorRampPalette(c("#e5f5f9","#99d8c9","#66c2a4","#2ca25f","#006d2c"))(5)
make_bins5 <- function(values){
  v <- suppressWarnings(as.numeric(values)); v <- v[is.finite(v)]
  if (!length(v)) return(seq(0,5))
  qs <- quantile(v, probs = seq(0,1,length.out=6), na.rm=TRUE, type=7)
  qs <- sort(unique(as.numeric(qs)))
  if (length(qs) < 6){
    r <- range(v, na.rm=TRUE); if (r[1]==r[2]) r <- c(0, max(1, r[2])); qs <- pretty(r, n=5)
  }
  if (length(qs) < 6) qs <- seq(min(qs), max(qs), length.out=6)
  qs
}
palBin5 <- function(values){
  bins <- make_bins5(values)
  leaflet::colorBin(palette = pal5_vec, bins = bins, domain = values, na.color = "#f0f0f0", right = FALSE)
}

# --- Colores de los gráficos (serie temporal y ranking) ---
SERIES_CLR  <- "#006d2c"
RANKING_CLR <- "#006d2c"

# ---------- Shapefiles ----------
shp_files <- list.files(SHP_DIR, pattern="\\.shp$", full.names=TRUE, recursive=TRUE)
if (!length(shp_files)) stop("No encuentro .shp en: ", SHP_DIR)
ruta_shp_mpios <- find_shp(shp_files, "MPIO|MUN")
ruta_shp_dptos <- find_shp(shp_files, "DPTO|DEP|DEPT")
if (is.na(ruta_shp_mpios) || is.na(ruta_shp_dptos)) stop("No pude detectar SHP de mpios/dptos en ", SHP_DIR)

mpios_sf_raw <- sf::st_read(ruta_shp_mpios, quiet = TRUE)
depto_sf_raw <- sf::st_read(ruta_shp_dptos, quiet = TRUE)

pick_first <- function(nms, candidates){ cand <- candidates[candidates %in% nms]; if (!length(cand)) NA_character_ else cand[1] }
muni_name_cands       <- c("MUNICIPIO_D","MPIO_CNMBR","NOMBRE_MPIO","NOMBRE_MUNICIP","NOMBRE","MUNICIPIO")
depto_name_cands      <- c("DEPARTAMENTO_D","DPTO_CNMBR","NOMBRE_DPT","NOMBRE_DEPTO","DEPARTAMEN","DEPARTAMENTO")
depto_code_cands      <- c("DPTO_CCDGO","COD_DPTO","DPTO","CODIGO_DPTO","COD_DEPART","DPTO_COD")
muni_depto_code_cands <- c("DPTO_CCDGO","COD_DPTO","DPTO","CODIGO_DPTO","DPTO_COD")

mpn <- names(mpios_sf_raw); dpn <- names(depto_sf_raw)
muni_name_col  <- pick_first(mpn, muni_name_cands)
muni_dpto_code <- pick_first(mpn, muni_depto_code_cands)
depto_name_col <- pick_first(dpn, depto_name_cands)
depto_code_col <- pick_first(dpn, depto_code_cands)
stopifnot(!is.na(muni_name_col), !is.na(muni_dpto_code), !is.na(depto_name_col), !is.na(depto_code_col))

depto_key <- depto_sf_raw |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    dpto_code      = .data[[depto_code_col]],
    DEPARTAMENTO_D = up_es(.data[[depto_name_col]])
  )

mpios_sf <- mpios_sf_raw |>
  dplyr::mutate(
    MUNICIPIO_D    = up_es(.data[[muni_name_col]]),
    dpto_code      = .data[[muni_dpto_code]]
  ) |>
  dplyr::left_join(depto_key, by = "dpto_code") |>
  sf::st_transform(4326) |> sf::st_make_valid() |> sf::st_zm(drop = TRUE, what = "ZM")

depto_sf <- depto_sf_raw |>
  sf::st_transform(4326) |> sf::st_make_valid() |> sf::st_zm(drop = TRUE, what = "ZM") |>
  dplyr::mutate(DEPARTAMENTO_D = up_es(.data[[depto_name_col]]))

# ---------- Base NOAA ----------
base_raw <- readRDS(DATA_RDS)
required_cols <- c("fecha_completa","ano","mes","DEPARTAMENTO_D","MUNICIPIO_D","precip_mm")
if (!all(required_cols %in% names(base_raw))) {
  faltan <- setdiff(required_cols, names(base_raw))
  stop("Faltan columnas en el RDS NOAA: ", paste(faltan, collapse=", "))
}
eva_df <- tibble::tibble(
  anio           = as.integer(base_raw[["ano"]]),
  mes            = as.integer(base_raw[["mes"]]),
  valor          = suppressWarnings(as.numeric(base_raw[["precip_mm"]])),
  MUNICIPIO_D    = up_es(base_raw[["MUNICIPIO_D"]]),
  DEPARTAMENTO_D = up_es(base_raw[["DEPARTAMENTO_D"]])
) |>
  dplyr::filter(!is.na(anio), !is.na(mes)) |>
  dplyr::mutate(
    mes   = pmax(pmin(mes, 12L), 1L),
    valor = dplyr::if_else(is.finite(valor), valor, NA_real_)
  )
stopifnot(nrow(eva_df) > 0)

# ========================= UI =========================
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tags$head(tags$style(HTML("
    /* Que el contenedor ocupe la altura de la ventana */
    html, body, .container-fluid { height: 100%; }

    h2#app-title { text-align:center; margin-top:10px; margin-bottom:10px; }

    /* >>> CAMBIO 1: quitar altura fija y padding de la tarjeta del mapa */
    .left-pane  { padding: 0; height: auto; }

    /* Tarjetas generales */
    .card { background:#fff; border:1px solid #e6e6e6; border-radius:12px; padding:12px; box-shadow:0 1px 6px rgba(0,0,0,.05); }

    .right-pane { height: 310px; margin-bottom: 20px; }
    .filter-label { font-weight:600; margin-bottom:4px; }
    .btn, .btn-default { font-size:12px; padding:6px 10px; border-radius:8px; }
    .dl-under { margin-top:8px; text-align:right; }
    .dl-footer { margin-top:10px; text-align:right; }

    /* Tooltip limpio en el mapa */
    .leaflet-tooltip.lbl-clean {
      background: rgba(255,255,255,.92);
      border: 1px solid #e6e6e6;
      border-radius: 6px;
      padding: 4px 6px;
      color: #222;
      font-weight: 600;
      box-shadow: 0 1px 4px rgba(0,0,0,.08);
    }

    /* >>> CAMBIO 2: alto dinámico del contenedor del mapa (ajusta 260px si quieres) */
    #map_container { height: calc(100vh - 260px); }
    
    /* === Bordes en color #F57C00 (filtros y cajas) === */
  .card {
    border-color: #ffb366 !important;
  }

  /* Inputs de texto, numéricos, date, etc. */
  .form-control {
    border-color: #ffb366 !important;
  }
  .form-control:focus {
    border-color: #ffb366 !important;
    box-shadow: 0 0 0 0.2rem rgba(245,124,0,0.25);
  }

  /* Selectize (selectInput por defecto en Shiny) */
  .selectize-input {
    border-color: #ffb366 !important;
  }
  .selectize-input.focus {
    border-color: #ffb366 !important;
    box-shadow: 0 0 0 0.2rem rgba(245,124,0,0.25);
  }

  /* Radio/checkbox */
  input[type='radio'],
  input[type='checkbox'] {
    accent-color: #ffb366;
  }

  /* (Opcional) botones a juego */
  .btn, .btn-default {
    border-color: #ffb366 !important;
  "))),
  h2("Explorador territorial — Precipitación (NOAA)", id = "app-title"),
  fluidRow(
    column(2, div(class="filter-label","Frecuencia"),
           radioButtons("freq", NULL, choices=c("Anual","Mensual"), selected="Anual", inline=TRUE)),
    column(2, div(class="filter-label","Año"), uiOutput("anio_ui")),
    column(2, div(class="filter-label","Mes"),
           conditionalPanel("input.freq == 'Mensual'",
                            selectInput("f_mes", NULL,
                                        choices = setNames(1:12, c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")),
                                        selected = 1)
           )
    ),
    column(3, div(class="filter-label","Departamento"),
           selectInput("f_depto", NULL, choices = c("Todos", sort(unique(na.omit(eva_df$DEPARTAMENTO_D)))),"Todos")),
    column(3, div(class="filter-label","Municipio"),
           selectInput("f_mpio", NULL, choices = c("Todos", sort(unique(na.omit(eva_df$MUNICIPIO_D)))),"Todos"))
  ),
  fluidRow(
    column(
      5,
      div(class="card left-pane",
          h5(textOutput("titulo_mapa")),
          div(style="display:flex; gap:10px; align-items:center; margin-bottom:8px;",
              actionButton("btn_volver", "◀ Volver a Departamentos", class="btn btn-light"),
              strong(textOutput("nivel_txt", inline = TRUE))
          ),
          # >>> CAMBIO 3: envoltura con alto dinámico + mapa al 100%
          div(id = "map_container",
              leafletOutput("map_eva", height = "100%")
          ),
          # <<< fin del cambio
          div(class="dl-under", downloadButton("dl_png_mapa","PNG — Mapa (simple)"))
      )
    ),
    column(
      7,
      div(class="card right-pane",
          h5(textOutput("titulo_serie")),
          plotlyOutput("plot_arriba", height = "240px"),
          div(class="dl-under", downloadButton("dl_png_series","PNG — Serie temporal"))
      ),
      div(class="card right-pane",
          h5(textOutput("titulo_ranking")),
          plotlyOutput("ranking_abajo", height = "300px"),
          div(class="dl-under", downloadButton("dl_png_ranking","PNG — Ranking Top-10"))
      ),
      # --- Tercer bloque: Anomalía en VELAS de MM (todo el periodo, base=3 primeros años) ---
      div(class="card right-pane",
          h5(textOutput("titulo_anomalia")),
          div(style="display:flex; gap:10px; align-items:baseline; justify-content:space-between; margin-bottom:6px;",
              div(
                htmltools::tags$div(id="anom_head",
                                    htmltools::tags$span(textOutput("anom_resumen"), style="font-weight:600; font-size:14px;"),
                                    htmltools::tags$br(),
                                    htmltools::tags$span(textOutput("anom_detalle"), style="color:#555; font-size:12px;")
                )
              ),
              div(class="dl-under", downloadButton("dl_png_anom","PNG — Anomalía (velas MM)"))
          ),
          plotlyOutput("anom_plot", height = "240px")
      )
    )
  ),
  fluidRow(column(12, div(class="dl-footer", downloadButton("dl_csv_expl","Descargar CSV (filtro actual)"))))
)

# ======================= SERVER =======================
server <- function(input, output, session){
  
  # Estado del mapa
  nivel_mapa <- reactiveVal("depto")
  depto_sel  <- reactiveVal(NULL)
  output$nivel_txt <- renderText({ if (nivel_mapa()=="depto") "Nivel: Departamentos" else paste0("Nivel: Municipios — ", depto_sel()) })
  
  # Años disponibles (para vista Serie/Mapa)
  output$anio_ui <- renderUI({
    yrs <- sort(unique(na.omit(as.integer(eva_df$anio))))
    selectInput("f_anio", NULL, choices = yrs, selected = max(yrs))
  })
  
  # Meses por año (cuando es Mensual)
  observeEvent(input$f_anio, ignoreInit = TRUE, {
    req(input$freq == "Mensual")
    meses <- eva_df |> dplyr::filter(anio == input$f_anio) |>
      dplyr::distinct(mes) |> dplyr::arrange(mes) |> dplyr::pull(mes)
    if (length(meses)) {
      updateSelectInput(session, "f_mes",
                        choices = setNames(meses, nombre_mes(meses)),
                        selected = min(meses, na.rm=TRUE))
    }
  })
  
  # Cascada dpto->mpio
  observeEvent(input$f_depto, ignoreInit = TRUE, {
    munis <- if (is.null(input$f_depto) || input$f_depto=="Todos")
      sort(unique(na.omit(eva_df$MUNICIPIO_D))) else
        sort(unique(na.omit(eva_df$MUNICIPIO_D[eva_df$DEPARTAMENTO_D == input$f_depto])))
    updateSelectInput(session, "f_mpio", choices = c("Todos", munis), selected = "Todos")
  })
  
  # ------------------ Datos filtrados (para mapa/series/ranking) ------------------
  datos_filtrados <- reactive({
    df <- eva_df
    if (!is.null(input$f_depto) && input$f_depto!="Todos") df <- df |> dplyr::filter(DEPARTAMENTO_D == input$f_depto)
    if (!is.null(input$f_mpio)  && input$f_mpio !="Todos") df <- df |> dplyr::filter(MUNICIPIO_D    == input$f_mpio)
    if (!is.null(input$f_anio)) df <- df |> dplyr::filter(anio == input$f_anio)
    if (!is.null(input$freq) && input$freq == "Mensual" && !is.null(input$f_mes)) {
      df <- df |> dplyr::filter(mes == as.integer(input$f_mes))
    }
    df |> dplyr::mutate(valor = suppressWarnings(as.numeric(valor)))
  })
  
  # Títulos (mapa/serie/ranking)
  output$titulo_mapa <- renderText({
    if (input$freq == "Mensual")
      paste0("¿Dónde se concentra la precipitación (mm) — ", nombre_mes(input$f_mes), " ", input$f_anio, "?")
    else
      paste0("¿Dónde se concentra la precipitación (mm) en ", input$f_anio, "?")
  })
  output$titulo_serie <- renderText({
    if (input$freq == "Mensual")
      paste0("Evolución mensual de la precipitación (mm) en ", input$f_anio)
    else
      "¿Cómo ha evolucionado la precipitación (mm) en el tiempo?"
  })
  output$titulo_ranking <- renderText({
    if (input$freq == "Mensual")
      paste0("Top-10 municipios con mayor precipitación (mm) — ", nombre_mes(input$f_mes), " ", input$f_anio)
    else
      paste0("Top-10 municipios con mayor precipitación (mm) en ", input$f_anio)
  })
  
  # Agregaciones para mapa/ranking
  agg_depto <- reactive({
    datos_filtrados() |>
      dplyr::group_by(DEPARTAMENTO_D) |>
      dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
  })
  agg_mpio <- reactive({
    df <- datos_filtrados()
    if (!is.null(depto_sel())) df <- df |> dplyr::filter(DEPARTAMENTO_D == depto_sel())
    df |>
      dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) |>
      dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
  })
  
  # Badge (con tildes en labels)
  badge_filtros <- reactive({
    if (input$freq == "Mensual") {
      htmltools::HTML(sprintf(
        '<div style="background:#fff;padding:6px 10px;border-radius:8px;box-shadow:0 1px 6px rgba(0,0,0,.15);font-size:12px;line-height:1.3;">
           <b>Indicador:</b> Precipitación (mm)<br>
           <b>Periodo:</b> %s %s
         </div>', nombre_mes(input$f_mes), input$f_anio))
    } else {
      htmltools::HTML(sprintf(
        '<div style="background:#fff;padding:6px 10px;border-radius:8px;box-shadow:0 1px 6px rgba(0,0,0,.15);font-size:12px;line-height:1.3;">
           <b>Indicador:</b> Precipitación (mm)<br>
           <b>Año:</b> %s
         </div>', input$f_anio))
    }
  })
  
  # Leaflet
  hover_label_opts <- leaflet::labelOptions(direction="auto", textsize="12px", sticky=TRUE, opacity=0.95, className="lbl-clean")
  hover_label_opts_small <- leaflet::labelOptions(direction="auto", textsize="11px", sticky=TRUE, opacity=0.95, className="lbl-clean")
  
  output$map_eva <- leaflet::renderLeaflet({
    mdat <- depto_sf |>
      dplyr::left_join(agg_depto(), by="DEPARTAMENTO_D") |>
      dplyr::mutate(valor = dplyr::coalesce(valor, 0))
    pal  <- palBin5(mdat$valor)
    leaflet::leaflet(mdat) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addPolygons(
        layerId = ~DEPARTAMENTO_D,
        fillColor = ~pal(valor), weight = 0.7, color = "#666", fillOpacity = 0.9,
        label = ~sprintf("%s — %s mm", DEPARTAMENTO_D, scales::comma(round(valor,0))),
        labelOptions = hover_label_opts,
        highlightOptions = leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
      ) |>
      leaflet::addLegend(position="bottomright", pal=pal, values=~valor, title="Precipitación (mm)",
                         labFormat = leaflet::labelFormat(big.mark=",")) |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  })
  observe({
    leaflet::leafletProxy("map_eva") |>
      leaflet::removeControl("badge_filtros") |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  })
  dibujar_deptos <- function(){
    mdat <- depto_sf |>
      dplyr::left_join(agg_depto(), by="DEPARTAMENTO_D") |>
      dplyr::mutate(valor = dplyr::coalesce(valor, 0))
    pal  <- palBin5(mdat$valor)
    leaflet::leafletProxy("map_eva", data=mdat) |>
      leaflet::clearPopups() |> leaflet::clearShapes() |> leaflet::clearMarkers() |> leaflet::clearControls() |>
      leaflet::addPolygons(layerId=~DEPARTAMENTO_D, fillColor=~pal(valor),
                           weight=0.7, color="#666", fillOpacity=0.9,
                           label=~sprintf("%s — %s mm", DEPARTAMENTO_D, scales::comma(round(valor,0))),
                           labelOptions=hover_label_opts,
                           highlightOptions=leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)) |>
      leaflet::addLegend(position="bottomright", pal=pal, values=~valor, title="Precipitación (mm)",
                         labFormat = leaflet::labelFormat(big.mark=",")) |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  }
  dibujar_mpios <- function(dep_lbl){
    mdat <- mpios_sf |> dplyr::filter(DEPARTAMENTO_D==dep_lbl) |>
      dplyr::left_join(agg_mpio(), by=c("MUNICIPIO_D","DEPARTAMENTO_D")) |>
      dplyr::mutate(valor = dplyr::coalesce(valor, 0))
    pal  <- palBin5(mdat$valor)
    leaflet::leafletProxy("map_eva", data=mdat) |>
      leaflet::clearPopups() |> leaflet::clearShapes() |> leaflet::clearMarkers() |> leaflet::clearControls() |>
      leaflet::addPolygons(layerId=~MUNICIPIO_D, fillColor=~pal(valor),
                           weight=0.4, color="#666", fillOpacity=0.9,
                           label=~sprintf("%s (%s) — %s mm", MUNICIPIO_D, DEPARTAMENTO_D, scales::comma(round(valor,0))),
                           labelOptions=hover_label_opts_small,
                           highlightOptions=leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)) |>
      leaflet::addLegend(position="bottomright", pal=pal, values=mdat$valor, title=paste0("Precipitación (mm) — ", dep_lbl),
                         labFormat = leaflet::labelFormat(big.mark=",")) |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  }
  observeEvent(input$f_depto, {
    dep <- input$f_depto
    if (is.null(dep) || dep=="Todos") { nivel_mapa("depto"); depto_sel(NULL); dibujar_deptos() }
    else { nivel_mapa("mpio"); depto_sel(dep); dibujar_mpios(dep) }
  }, ignoreInit = TRUE)
  observeEvent(input$map_eva_shape_click, {
    click <- input$map_eva_shape_click
    if (is.null(click$id)) return()
    if (nivel_mapa()=="depto") { depto_sel(click$id); nivel_mapa("mpio"); dibujar_mpios(click$id) }
  })
  observeEvent(input$btn_volver, {
    updateSelectInput(session, "f_depto", selected="Todos")
    updateSelectInput(session, "f_mpio",  selected="Todos")
    nivel_mapa("depto"); depto_sel(NULL); dibujar_deptos()
  })
  
  # ------------------ Serie temporal (SIN Media Móvil) ------------------
  series_data <- reactive({
    base <- eva_df
    if (!is.null(input$f_depto) && input$f_depto!="Todos") base <- base |> dplyr::filter(DEPARTAMENTO_D == input$f_depto)
    if (!is.null(input$f_mpio)  && input$f_mpio !="Todos") base <- base |> dplyr::filter(MUNICIPIO_D    == input$f_mpio)
    
    if (!is.null(input$freq) && input$freq == "Mensual") {
      req(input$f_anio)
      base <- base |> dplyr::filter(anio == input$f_anio)
      base |> dplyr::group_by(mes) |>
        dplyr::summarise(valor_total = sum(as.numeric(valor), na.rm = TRUE), .groups="drop") |>
        dplyr::arrange(mes)
    } else {
      base |> dplyr::group_by(anio) |>
        dplyr::summarise(valor_total = sum(as.numeric(valor), na.rm = TRUE), .groups="drop") |>
        dplyr::arrange(anio)
    }
  })
  output$plot_arriba <- plotly::renderPlotly({
    df <- series_data()
    if (!nrow(df)) return(plotly::plot_ly())
    
    if (!is.null(input$freq) && input$freq == "Mensual") {
      plotly::plot_ly(df, x=~mes, y=~valor_total, type="scatter", mode="lines+markers",
                      line=list(width=2, color=SERIES_CLR),     # color aplicado
                      marker=list(size=6, color=SERIES_CLR),     # color aplicado
                      name="Serie",
                      hovertemplate="<b>Mes:</b> %{x}<br>Precipitación (mm): %{y:,}<extra></extra>") |>
        plotly::layout(
          xaxis=list(title="Mes", tickmode="array", tickvals=1:12, ticktext=month.abb),
          yaxis=list(title="Precipitación (mm)", separatethousands=TRUE),
          hovermode="x unified", margin=list(l=60,r=20,t=40,b=50), legend=list(orientation="h")
        )
    } else {
      plotly::plot_ly(df, x=~anio, y=~valor_total, type="scatter", mode="lines+markers",
                      line=list(width=2, color=SERIES_CLR),     # color aplicado
                      marker=list(size=6, color=SERIES_CLR),     # color aplicado
                      name="Serie",
                      hovertemplate="<b>Año:</b> %{x}<br>Precipitación (mm): %{y:,}<extra></extra>") |>
        plotly::layout(
          xaxis=list(title="Año", tickmode="linear", dtick=1),
          yaxis=list(title="Precipitación (mm)", separatethousands=TRUE),
          hovermode="x unified", margin=list(l=60,r=20,t=40,b=50), legend=list(orientation="h")
        )
    }
  })
  
  # ------------------ Ranking ------------------
  ranking_data <- reactive({
    datos_filtrados() |>
      dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) |>
      dplyr::summarise(valor_total = sum(valor, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(valor_total)) |>
      dplyr::slice_head(n = 10)
  })
  output$ranking_abajo <- plotly::renderPlotly({
    plot_df <- ranking_data()
    if (!nrow(plot_df)) {
      return(plotly::plot_ly() |> plotly::layout(annotations = list(
        text="Sin datos para el ranking", x=0.5, y=0.5, showarrow=FALSE)))
    }
    plot_df <- plot_df |> dplyr::mutate(etiqueta = paste0(MUNICIPIO_D, " (", DEPARTAMENTO_D, ")"))
    plotly::plot_ly(
      data = plot_df, x = ~valor_total, y = ~etiqueta,
      type = "bar", orientation = "h",
      marker = list(color = RANKING_CLR),              # color aplicado
      text = ~scales::comma(round(valor_total, 0)), textposition = "outside",
      hovertemplate = "<b>Municipio:</b> %{customdata[0]}<br><b>Departamento:</b> %{customdata[1]}<br><b>Precipitación (mm):</b> %{x:,}<extra></extra>",
      customdata = cbind(plot_df$MUNICIPIO_D, plot_df$DEPARTAMENTO_D)
    ) |>
      plotly::layout(
        xaxis = list(title = "Precipitación (mm)", separatethousands = TRUE, gridcolor = "#e6e6e6"),
        yaxis = list(title = "", categoryorder = "array", categoryarray = rev(plot_df$etiqueta)),
        margin = list(l = 160, r = 40, t = 20, b = 40)
      )
  })
  
  # ================== ANOMALÍA: solo filtros dpto/mun, todo periodo, base=3 primeros años ==================
  mensual_full_by_geo <- reactive({
    df <- eva_df
    if (!is.null(input$f_depto) && input$f_depto!="Todos") df <- df |> dplyr::filter(DEPARTAMENTO_D == input$f_depto)
    if (!is.null(input$f_mpio)  && input$f_mpio !="Todos") df <- df |> dplyr::filter(MUNICIPIO_D    == input$f_mpio)
    
    df |>
      dplyr::group_by(anio, mes) |>
      dplyr::summarise(valor_mensual = sum(as.numeric(valor), na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(fecha = as.Date(sprintf("%d-%02d-15", anio, mes)))
  })
  
  anom_mm_candles_full <- reactive({
    m <- mensual_full_by_geo()
    if (!nrow(m)) return(NULL)
    
    yrs <- sort(unique(m$anio))
    if (length(yrs) < 3) return(NULL)
    base_years <- yrs[1:3]
    
    clim_base <- m |>
      dplyr::filter(anio %in% base_years) |>
      dplyr::group_by(mes) |>
      dplyr::summarise(mu = mean(valor_mensual, na.rm = TRUE), .groups="drop")
    
    s <- m |>
      dplyr::inner_join(clim_base, by = "mes") |>
      dplyr::arrange(fecha) |>
      dplyr::mutate(anom = valor_mensual - mu)
    
    k <- 5 # ventana por defecto para las velas de la MM de anomalía
    mm <- zoo::rollmean(s$anom, k = k, fill = NA, align = "right")
    
    n <- length(mm)
    if (n < k + 1) return(NULL)
    idx <- (k+0):n
    open  <- mm[pmax(idx-1, 1)]
    close <- mm[idx]
    highs <- lows <- rep(NA_real_, length(idx))
    for (i in seq_along(idx)) {
      win <- (idx[i]-k+1):idx[i]
      highs[i] <- max(mm[win], na.rm = TRUE)
      lows[i]  <- min(mm[win], na.rm = TRUE)
    }
    
    tibble::tibble(
      x = s$fecha[idx],
      O = open, C = close, H = highs, L = lows,
      base_info = paste(base_years, collapse = ", ")
    )
  })
  
  output$titulo_anomalia <- renderText({
    "Anomalía de precipitación (velas de MM) — Todo el periodo"
  })
  output$anom_resumen <- renderText({
    df <- anom_mm_candles_full(); if (is.null(df) || !nrow(df)) return("Sin datos suficientes")
    ult <- tail(df, 1)
    signo <- ifelse(ult$C - ult$O >= 0, "↑", "↓")
    paste0("Vela más reciente: ", signo, " cierre = ", scales::comma(round(ult$C,1)),
           " mm (O=", scales::comma(round(ult$O,1)), ", H=", scales::comma(round(ult$H,1)),
           ", L=", scales::comma(round(ult$L,1)), "). Base: 5 primeros años.")
  })
  output$anom_detalle <- renderText({
    "Base climatológica = promedio mensual de los primeros 5 años del subconjunto geográfico. Anomalía = observado − climatología mensual (base)."
  })
  
  output$anom_plot <- plotly::renderPlotly({
    df <- anom_mm_candles_full(); if (is.null(df) || !nrow(df)) return(plotly::plot_ly())
    plotly::plot_ly(
      type  = "candlestick",
      x     = df$x,
      open  = df$O,
      high  = df$H,
      low   = df$L,
      close = df$C,
      increasing = list(line = list(color = "#2ca02c")),
      decreasing = list(line = list(color = "#d62728"))
    ) |>
      plotly::layout(
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "MM de anomalía",
                     zeroline = TRUE, zerolinecolor = "#999",
                     separatethousands = TRUE, gridcolor = "#e6e6e6"),
        margin = list(l=60, r=30, t=20, b=50),
        showlegend = FALSE
      )
  })
  
  # ------------------ Descargas ------------------
  tabla_export <- reactive({
    datos_filtrados() |> dplyr::transmute(DEPARTAMENTO_D, MUNICIPIO_D, anio, mes, precipitacion_mm = valor)
  })
  output$dl_csv_expl <- downloadHandler(
    filename = function() {
      if (!is.null(input$freq) && input$freq == "Mensual")
        paste0("NOAA_precipitacion_", input$f_anio, "_", sprintf("%02d", as.integer(input$f_mes)), "_", Sys.Date(), ".csv")
      else
        paste0("NOAA_precipitacion_", safe_chr(input$f_anio), "_", Sys.Date(), ".csv")
    },
    content  = function(file) readr::write_csv(tabla_export(), file, na = "")
  )
  
  output$dl_png_series <- downloadHandler(
    filename = function() {
      if (!is.null(input$freq) && input$freq == "Mensual")
        paste0("NOAA_serie_", input$f_anio, "_", sprintf("%02d", as.integer(input$f_mes)), "_", Sys.Date(), ".png")
      else
        paste0("NOAA_serie_", Sys.Date(), ".png")
    },
    content  = function(file){
      df <- series_data()
      if (!nrow(df)) { file.create(file); return() }
      
      if (!is.null(input$freq) && input$freq == "Mensual") {
        g <- ggplot(df, aes(x=mes, y=valor_total)) +
          geom_line(linewidth=0.9, color=SERIES_CLR) +   # color aplicado
          geom_point(size=2.2, color=SERIES_CLR) +       # color aplicado
          scale_x_continuous(breaks=1:12, labels=month.abb) +
          labs(x="Mes", y="Precipitación (mm)", title=paste0("Evolución mensual (", input$f_anio, ")")) +
          theme_minimal(base_size=12) +
          theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank())
      } else {
        g <- ggplot(df, aes(x=anio, y=valor_total)) +
          geom_line(linewidth=0.9, color=SERIES_CLR) +   # color aplicado
          geom_point(size=2.2, color=SERIES_CLR) +       # color aplicado
          scale_x_continuous(breaks=unique(df$anio)) +
          labs(x="Año", y="Precipitación (mm)", title="Evolución anual de la precipitación (mm)") +
          theme_minimal(base_size=12) +
          theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank())
      }
      ggsave(filename=file, plot=g, device=ragg::agg_png, width=10, height=5, dpi=200, units="in")
    }
  )
  
  map_widget_simple <- reactive({
    if (nivel_mapa()=="depto"){
      mdat <- depto_sf |> dplyr::left_join(agg_depto(), by="DEPARTAMENTO_D") |> dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
      pal  <- palBin5(mdat$valor)
      leaflet::leaflet(mdat, options=leaflet::leafletOptions(zoomControl=FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(fillColor=~pal(valor), weight=0.5, color="#666", fillOpacity=0.9) |>
        leaflet::addControl(html = htmltools::HTML(
          sprintf("<div style='font-weight:600;font-size:14px;background:#fff;padding:6px 8px;border-radius:8px;border:1px solid #e6e6e6'>
                    Precipitación (mm) por departamento — %s%s
                  </div>",
                  safe_chr(input$f_anio),
                  if (!is.null(input$freq) && input$freq=='Mensual') paste0(" (", nombre_mes(input$f_mes), ")") else "")),
          position="topleft")
    } else {
      dep <- depto_sel()
      mdat <- mpios_sf |> dplyr::filter(DEPARTAMENTO_D==dep) |>
        dplyr::left_join(agg_mpio(), by=c("MUNICIPIO_D","DEPARTAMENTO_D")) |> dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
      pal  <- palBin5(mdat$valor)
      leaflet::leaflet(mdat, options=leaflet::leafletOptions(zoomControl=FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(fillColor=~pal(valor), weight=0.4, color="#666", fillOpacity=0.9) |>
        leaflet::addControl(html = htmltools::HTML(
          sprintf("<div style='font-weight:600;font-size:14px;background:#fff;padding:6px 8px;border-radius:8px;border:1px solid #e6e6e6'>
                    Precipitación (mm) por municipios — %s%s
                  </div>",
                  safe_chr(input$f_anio),
                  if (!is.null(input$freq) && input$freq=='Mensual') paste0(" (", nombre_mes(input$f_mes), ")") else "")),
          position="topleft")
    }
  })
  output$dl_png_mapa <- downloadHandler(
    filename = function() {
      suf <- if (!is.null(input$freq) && input$freq=="Mensual")
        paste0("_", input$f_anio, "_", sprintf("%02d", as.integer(input$f_mes))) else ""
      paste0("NOAA_mapa", suf, "_", Sys.Date(), ".png")
    },
    content  = function(file){
      widget <- map_widget_simple()
      tmp_html <- tempfile(fileext=".html")
      htmlwidgets::saveWidget(widget, tmp_html, selfcontained=TRUE)
      webshot2::webshot(tmp_html, file=file, vwidth=1200, vheight=800, zoom=2)
    }
  )
  
  output$dl_png_ranking <- downloadHandler(
    filename = function() {
      suf <- if (!is.null(input$freq) && input$freq=="Mensual")
        paste0("_", input$f_anio, "_", sprintf("%02d", as.integer(input$f_mes))) else
          paste0("_", safe_chr(input$f_anio))
      paste0("NOAA_ranking", suf, "_", Sys.Date(), ".png")
    },
    content  = function(file){
      plot_df <- ranking_data() |> dplyr::mutate(etiqueta = paste0(MUNICIPIO_D, " (", DEPARTAMENTO_D, ")"))
      if (!nrow(plot_df)) { file.create(file); return() }
      g <- ggplot(plot_df, aes(x = valor_total, y = reorder(etiqueta, -valor_total))) +
        geom_col(fill = RANKING_CLR) +                       # color aplicado
        geom_text(aes(label = scales::comma(round(valor_total, 0))), hjust=-0.1, size=3) +
        scale_x_continuous(labels=scales::comma, expand = expansion(mult = c(0, 0.10))) +
        labs(x="Precipitación (mm)", y=NULL) +
        theme_minimal(base_size=12) +
        theme(axis.text.y=element_text(size=9),
              plot.margin=margin(r=30),
              panel.grid.minor=element_blank(),
              panel.grid.major.x=element_line(color="#e6e6e6"))
      ggsave(filename=file, plot=g, device=ragg::agg_png, width=10, height=6, dpi=200, units="in")
    }
  )
  
  output$dl_png_anom <- downloadHandler(
    filename = function() paste0("NOAA_anomalia_velasMM_full_", Sys.Date(), ".png"),
    content  = function(file){
      df <- anom_mm_candles_full(); if (is.null(df) || !nrow(df)) { file.create(file); return() }
      p <- plotly::plot_ly(
        type  = "candlestick",
        x     = df$x, open = df$O, high = df$H, low = df$L, close = df$C,
        increasing = list(line = list(color = "#2ca02c")),
        decreasing = list(line = list(color = "#d62728"))
      ) |>
        plotly::layout(
          xaxis = list(title = "Fecha"),
          yaxis = list(title = "MM de anomalía (mm)",
                       zeroline = TRUE, zerolinecolor = "#999",
                       separatethousands = TRUE, gridcolor = "#e6e6e6"),
          margin = list(l=60, r=30, t=20, b=50),
          showlegend = FALSE
        )
      tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(p, tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, file = file, vwidth = 1100, vheight = 520, zoom = 2)
    }
  )
}

# Lanzar
shinyApp(ui = ui, server = server)
