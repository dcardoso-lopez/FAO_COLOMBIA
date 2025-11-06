# =========================================================
# Shiny App — HANSEN Deforestación (Solo Tab 1)
# Mapa (dpto/municipio) + Serie anual + Ranking Top-10 + Descargas
# Etiquetas limpias por hover + Ranking descendente (mayor arriba)
# =========================================================

# 1) Paquetes
pkgs <- c("dplyr","readr","stringi","sf","leaflet","plotly","bslib",
          "ggplot2","htmltools","webshot2","htmlwidgets","ragg","glue","scales")
suppressWarnings(invisible(sapply(pkgs, require, character.only = TRUE)))
options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

# 2) Rutas (ajústalas a tu equipo)
APP_DIR  <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/HANSEN_DEFORESTATION"
DATA_RDS <- file.path(APP_DIR, "data/141_HANSEN_DEFORESTATION.rds")
SHP_DIR  <- file.path(APP_DIR, "data/shp")

if (!file.exists(DATA_RDS)) stop("No encuentro la base RDS en: ", DATA_RDS)
shp_files <- list.files(SHP_DIR, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
if (length(shp_files) == 0) stop("No encuentro archivos .shp en: ", SHP_DIR)

# 3) Helpers
norm_txt   <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
pick_first <- function(nms, candidates){ cand <- candidates[candidates %in% nms]; if (!length(cand)) NA_character_ else cand[1] }
find_shp   <- function(files, key){ i <- grep(key, basename(files), ignore.case = TRUE); if (!length(i)) NA_character_ else files[i[1]] }

# Title Case (visual; no cambia los datos)
to_title <- function(x){
  stringi::stri_trans_totitle(
    stringi::stri_trans_tolower(as.character(x), locale = "es"),
    opts_brkiter = stringi::stri_opts_brkiter(type="word")
  )
}
safe_chr <- function(x) if (is.null(x)) "" else as.character(x)

# --- Helpers filtros (pares únicos) ---
distinct_pairs <- function(df, key_col, disp_col){
  # Fallbacks si no existen las columnas
  if (!key_col %in% names(df))  key_col  <- (intersect(c("DEPTO_KEY","MPIO_KEY"), names(df)))[1]
  if (!disp_col %in% names(df)) disp_col <- (intersect(c("DEPTO_DISP","MPIO_DISP"), names(df)))[1]
  if (is.na(key_col) || is.na(disp_col)) stop("No existen columnas clave para construir los filtros.")
  df |>
    dplyr::distinct(dplyr::across(all_of(c(key_col, disp_col)))) |>
    dplyr::filter(!is.na(.data[[key_col]]), nzchar(.data[[key_col]])) |>
    dplyr::arrange(.data[[disp_col]])
}
mk_tc_from_pairs <- function(keys, labels_disp){
  stopifnot(length(keys) == length(labels_disp))
  labs_tc <- to_title(labels_disp)
  out <- stats::setNames(as.character(keys), labs_tc)  # nombres = etiqueta; valor = key
  out <- out[order(names(out), na.last = TRUE)]
  c("Todos" = "Todos", out)
}

# 4) Detectar SHP
ruta_shp_mpios <- find_shp(shp_files, "MPIO|MUN")
ruta_shp_dptos <- find_shp(shp_files, "DPTO|DEP|DEPT")
if (is.na(ruta_shp_mpios) || is.na(ruta_shp_dptos)) stop("No pude detectar SHP de mpios/dptos en ", SHP_DIR)

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
stopifnot(!is.na(muni_name_col), !is.na(muni_dpto_code), !is.na(depto_name_col), !is.na(depto_code_col))

depto_key <- depto_sf_raw |>
  sf::st_drop_geometry() |>
  dplyr::transmute(dpto_code = .data[[depto_code_col]], DEPARTAMENTO_D = .data[[depto_name_col]])

mpios_sf <- mpios_sf_raw |>
  dplyr::mutate(MUNICIPIO_D = .data[[muni_name_col]], dpto_code = .data[[muni_dpto_code]]) |>
  dplyr::left_join(depto_key, by = "dpto_code")

mpios_sf <- sf::st_transform(mpios_sf, 4326) |> sf::st_make_valid() |> sf::st_zm(drop = TRUE, what = "ZM")
depto_sf <- sf::st_transform(depto_sf_raw, 4326) |> sf::st_make_valid() |> sf::st_zm(drop = TRUE, what = "ZM") |>
  dplyr::mutate(DEPARTAMENTO_D = .data[[depto_name_col]])

mpios_sf <- mpios_sf |> dplyr::mutate(MUNICIPIO_D = norm_txt(MUNICIPIO_D), DEPARTAMENTO_D = toupper(norm_txt(DEPARTAMENTO_D)))
depto_sf <- depto_sf |> dplyr::mutate(DEPARTAMENTO_D = toupper(norm_txt(DEPARTAMENTO_D)))

# 5) Base HANSEN
base_raw <- readRDS(DATA_RDS)
year_cands  <- c("year","anio","ano","ano_evento","ano_cosechado","ano_sembrado","lossyear")
valor_cands <- c("has")
depto_cands <- c("DEPARTAMENTO_D","depto","departamento","DEPARTAMENTO")
mpio_cands  <- c("MUNICIPIO_D","mpio","municipio","MUNICIPIO")

bn <- names(base_raw)
col_year <- pick_first(bn, year_cands); col_val <- pick_first(bn, valor_cands)
col_dep  <- pick_first(bn, depto_cands); col_mpio <- pick_first(bn, mpio_cands)
if (is.na(col_year) || is.na(col_val)) stop("No pude detectar columnas de año/valor en la base RDS.")

eva_df <- base_raw |>
  dplyr::mutate(
    anio           = as.integer(.data[[col_year]]),
    valor          = suppressWarnings(as.numeric(.data[[col_val]])),
    MUNICIPIO_D    = norm_txt(dplyr::coalesce(.data[[col_mpio]], "")),
    DEPARTAMENTO_D = toupper(norm_txt(dplyr::coalesce(.data[[col_dep]], "")))
  )

# >>>>>>>>>>>>>> CREAR KEYS/DISPLAYS AQUÍ (antes del UI) <<<<<<<<<<<<<<
if (!all(c("DEPTO_KEY","DEPTO_DISP") %in% names(eva_df))) {
  eva_df <- eva_df |>
    dplyr::mutate(DEPTO_KEY = DEPARTAMENTO_D,
                  DEPTO_DISP = DEPARTAMENTO_D)
}
if (!all(c("MPIO_KEY","MPIO_DISP") %in% names(eva_df))) {
  eva_df <- eva_df |>
    dplyr::mutate(MPIO_KEY = MUNICIPIO_D,
                  MPIO_DISP = MUNICIPIO_D)
}

# ================== Colores ==================
SERIE_COLOR <- "#6B4F2C"  # café
BAR_COLOR   <- "#6B4F2C"  # café

# 6) Paleta y bins
pal5_vec <- grDevices::colorRampPalette(
  c("#F6E8C3", "#EBD3A6", "#C9A56A", "#9A7547", "#6B4F2C")
)(5)
make_bins5 <- function(values){
  v <- as.numeric(values); v <- v[is.finite(v)]
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

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    primary = "#F57C00",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight")
  ),
  tags$head(tags$style(HTML("
    /* Forzar Inter / Inter Tight en toda la UI */
    html, body { 
      font-family: 'Inter', system-ui, -apple-system, Segoe UI, Roboto, 'Helvetica Neue', Arial, 'Noto Sans', 'Liberation Sans', sans-serif;
    }
    h1, h2, h3, h4, h5, h6, .card-title { font-family: 'Inter Tight', inherit; }
    .form-control, .selectize-input, .selectize-dropdown, .picker, .btn, .shiny-options-group, .filter-label {
      font-family: 'Inter', inherit !important;
    }
    .leaflet-container, .leaflet-tooltip, .leaflet-control { font-family: 'Inter', inherit !important; }
    .plotly .xtick text, .plotly .ytick text, .plotly .legend text, .plotly .gtitle { font-family: 'Inter', inherit !important; }

    h2#app-title { text-align:center; margin-top:10px; margin-bottom:10px; }
    .left-pane  { height: 640px; }
    .right-pane { height: 310px; margin-bottom: 20px; }

    .card { background:#fff; border:1px solid #F57C00; border-radius:12px; padding:12px; box-shadow:0 1px 6px rgba(0,0,0,.05); }
    .filter-label { font-weight:600; margin-bottom:4px; }
    .btn, .btn-default { font-size:12px; padding:6px 10px; border-radius:8px; border-color:#F57C00 !important; }
    .dl-under { margin-top:8px; text-align:right; }
    .dl-footer { margin-top:10px; text-align:right; }

    .form-control { border-color:#F57C00 !important; }
    .form-control:focus { border-color:#F57C00 !important; box-shadow:0 0 0 0.2rem rgba(245,124,0,0.25); }
    .selectize-input { border-color:#F57C00 !important; }
    .selectize-input.focus { border-color:#F57C00 !important; box-shadow:0 0 0 0.2rem rgba(245,124,0,0.25); }
    input[type='radio'], input[type='checkbox'] { accent-color:#F57C00; }

    .leaflet-tooltip.lbl-clean {
      background: rgba(255,255,255,.92);
      border: 1px solid #e6e6e6;
      border-radius: 6px;
      padding: 4px 6px;
      color: #222;
      font-weight: 600;
      box-shadow: 0 1px 4px rgba(0,0,0,.08);
    }
  "))),
  h2("Explorador territorial — Deforestación (Hansen)", id = "app-title"),
  
  # Filtros (Title Case + pares únicos KEY/LABEL)
  fluidRow(
    column(4, div(class="filter-label","¿Qué año miramos?"), uiOutput("anio_ui")),
    column(4, div(class="filter-label","¿Dónde? (Departamento)"),
           {
             dep_pairs <- distinct_pairs(eva_df, "DEPTO_KEY", "DEPTO_DISP")
             selectInput("f_depto", NULL,
                         choices = mk_tc_from_pairs(dep_pairs$DEPTO_KEY, dep_pairs$DEPTO_DISP),
                         selected = "Todos")
           }),
    column(4, div(class="filter-label","¿Algún municipio en particular?"),
           {
             mpio_pairs <- distinct_pairs(eva_df, "MPIO_KEY", "MPIO_DISP")
             selectInput("f_mpio", NULL,
                         choices = mk_tc_from_pairs(mpio_pairs$MPIO_KEY, mpio_pairs$MPIO_DISP),
                         selected = "Todos")
           })
  ),
  
  fluidRow(
    column(
      6,
      div(class="card left-pane",
          h5(textOutput("titulo_mapa")),
          div(style="display:flex; gap:10px; align-items:center; margin-bottom:8px;",
              actionButton("btn_volver", "◀ Volver a Departamentos", class="btn btn-light"),
              strong(textOutput("nivel_txt", inline = TRUE))
          ),
          leafletOutput("map_eva", height = 560),
          div(class="dl-under", downloadButton("dl_png_mapa","PNG — Mapa (simple)"))
      )
    ),
    column(
      6,
      div(class="card right-pane",
          h5(textOutput("titulo_serie")),
          plotlyOutput("plot_arriba", height = "240px"),
          div(class="dl-under", downloadButton("dl_png_series","PNG — Serie temporal"))
      ),
      div(class="card right-pane",
          h5(textOutput("titulo_ranking")),
          plotlyOutput("ranking_abajo", height = "300px"),
          div(class="dl-under", downloadButton("dl_png_ranking","PNG — Ranking Top-10"))
      )
    )
  ),
  fluidRow(
    column(12, div(class="dl-footer", downloadButton("dl_csv_expl","Descargar CSV (filtro actual)")))
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  # Estado de mapa
  nivel_mapa <- reactiveVal("depto")
  depto_sel  <- reactiveVal(NULL)
  output$nivel_txt <- renderText({ 
    if (nivel_mapa()=="depto") "Nivel: Departamentos" else paste0("Nivel: Municipios — ", to_title(depto_sel())) 
  })
  
  # Años
  output$anio_ui <- renderUI({
    yrs <- sort(unique(na.omit(as.integer(eva_df$anio))))
    selectInput("f_anio", NULL, choices = yrs, selected = max(yrs))
  })
  
  # Cascada dpto->mpio por KEY
  observeEvent(input$f_depto, ignoreInit = TRUE, {
    if (is.null(input$f_depto) || input$f_depto == "Todos"){
      munis <- distinct_pairs(eva_df, "MPIO_KEY", "MPIO_DISP")
    } else {
      munis <- eva_df |>
        dplyr::filter(DEPTO_KEY == input$f_depto) |>
        distinct_pairs("MPIO_KEY", "MPIO_DISP")
    }
    updateSelectInput(session, "f_mpio",
                      choices = mk_tc_from_pairs(munis$MPIO_KEY, munis$MPIO_DISP),
                      selected = "Todos")
  })
  
  # Datos filtrados (por KEY)
  datos_filtrados <- reactive({
    df <- eva_df
    if (!is.null(input$f_depto) && input$f_depto!="Todos") df <- df |> dplyr::filter(DEPTO_KEY == input$f_depto)
    if (!is.null(input$f_mpio)  && input$f_mpio !="Todos") df <- df |> dplyr::filter(MPIO_KEY  == input$f_mpio)
    if (!is.null(input$f_anio)) df <- df |> dplyr::filter(anio == input$f_anio)
    df |> dplyr::mutate(valor = suppressWarnings(as.numeric(valor)))
  })
  
  # Títulos storytelling
  output$titulo_mapa    <- renderText({ 
    paste0("¿Dónde está pegando más la deforestación ", if (!is.null(input$f_anio)) paste0("en ", input$f_anio) else "", "?")
  })
  output$titulo_serie   <- renderText({ "¿Cómo viene la deforestación año a año?" })
  output$titulo_ranking <- renderText({ 
    paste0("Los 10 municipios que más pierden bosque", if (!is.null(input$f_anio)) paste0(" en ", input$f_anio) else "")
  })
  
  # Agregaciones
  agg_depto <- reactive({
    datos_filtrados() |>
      dplyr::group_by(DEPARTAMENTO_D) |>
      dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
  })
  agg_mpio <- reactive({
    df <- datos_filtrados()
    if (!is.null(depto_sel())) df <- df |> dplyr::filter(DEPARTAMENTO_D == depto_sel())
    df |>
      dplyr::group_by(MUNICIPIO_D) |>
      dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
  })
  
  # Badge
  badge_filtros <- reactive({
    yr <- safe_chr(input$f_anio)
    htmltools::HTML(sprintf(
      '<div style="background:#fff;padding:6px 10px;border-radius:8px;
                   box-shadow:0 1px 6px rgba(0,0,0,.15);font-size:12px;line-height:1.3;">
         <b>Indicador:</b> Deforestación (Ha)<br>
         <b>Año:</b> %s
       </div>', yr))
  })
  
  # Label options (hover)
  hover_label_opts       <- leaflet::labelOptions(direction="auto", textsize="12px", sticky=TRUE, opacity=0.95, className="lbl-clean")
  hover_label_opts_small <- leaflet::labelOptions(direction="auto", textsize="11px", sticky=TRUE, opacity=0.95, className="lbl-clean")
  
  # Mapa inicial
  output$map_eva <- leaflet::renderLeaflet({
    mdat <- depto_sf |>
      dplyr::left_join(agg_depto(), by="DEPARTAMENTO_D") |> 
      dplyr::mutate(valor = ifelse(is.na(valor), 0, valor),
                    DEPTO_TC = to_title(DEPARTAMENTO_D))
    pal  <- palBin5(mdat$valor)
    leaflet::leaflet(mdat) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addPolygons(
        layerId = ~DEPARTAMENTO_D,
        fillColor = ~pal(valor),
        weight = 0.7, color = "#666", fillOpacity = 0.9,
        label = ~sprintf("%s — %s Ha", DEPTO_TC, scales::comma(round(valor,0))),
        labelOptions = hover_label_opts,
        highlightOptions = leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
      ) |>
      leaflet::addLegend(position="bottomright", pal=pal, values=~valor, title="Deforestación (Ha)",
                         labFormat = leaflet::labelFormat(big.mark=",")) |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  })
  observe({
    leaflet::leafletProxy("map_eva") |>
      leaflet::removeControl("badge_filtros") |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  })
  
  # Redibujos
  dibujar_deptos <- function(){
    mdat <- depto_sf |>
      dplyr::left_join(agg_depto(), by="DEPARTAMENTO_D") |> 
      dplyr::mutate(valor = ifelse(is.na(valor), 0, valor),
                    DEPTO_TC = to_title(DEPARTAMENTO_D))
    pal  <- palBin5(mdat$valor)
    leaflet::leafletProxy("map_eva", data=mdat) |>
      leaflet::clearPopups() |> leaflet::clearShapes() |> leaflet::clearMarkers() |> leaflet::clearControls() |>
      leaflet::addPolygons(layerId=~DEPARTAMENTO_D, fillColor=~pal(valor),
                           weight=0.7, color="#666", fillOpacity=0.9,
                           label=~sprintf("%s — %s Ha", DEPTO_TC, scales::comma(round(valor,0))),
                           labelOptions=hover_label_opts,
                           highlightOptions=leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)) |>
      leaflet::addLegend(position="bottomright", pal=pal, values=~valor, title="Deforestación (Ha)",
                         labFormat = leaflet::labelFormat(big.mark=",")) |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  }
  dibujar_mpios <- function(dep){
    mdat <- mpios_sf |> dplyr::filter(DEPARTAMENTO_D==dep) |>
      dplyr::left_join(agg_mpio(), by="MUNICIPIO_D") |>
      dplyr::mutate(valor = ifelse(is.na(valor), 0, valor),
                    MPIO_TC = to_title(MUNICIPIO_D),
                    DEPTO_TC = to_title(DEPARTAMENTO_D))
    pal  <- palBin5(mdat$valor)
    leaflet::leafletProxy("map_eva", data=mdat) |>
      leaflet::clearPopups() |> leaflet::clearShapes() |> leaflet::clearMarkers() |> leaflet::clearControls() |>
      leaflet::addPolygons(layerId=~MUNICIPIO_D, fillColor=~pal(valor),
                           weight=0.4, color="#666", fillOpacity=0.9,
                           label=~sprintf("%s (%s) — %s Ha", MPIO_TC, DEPTO_TC, scales::comma(round(valor,0))),
                           labelOptions=hover_label_opts_small,
                           highlightOptions=leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)) |>
      leaflet::addLegend(position="bottomright", pal=pal, values=mdat$valor, title=paste0("Deforestación (Ha) — ", to_title(dep)),
                         labFormat = leaflet::labelFormat(big.mark=",")) |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  }
  
  observeEvent(input$f_depto, {
    dep <- if (is.null(input$f_depto) || input$f_depto=="Todos") NULL else input$f_depto
    if (is.null(dep)) { nivel_mapa("depto"); depto_sel(NULL); dibujar_deptos() }
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
  
  # Serie temporal
  series_data <- reactive({
    base <- eva_df
    if (!is.null(input$f_depto) && input$f_depto!="Todos") base <- base |> dplyr::filter(DEPTO_KEY == input$f_depto)
    if (!is.null(input$f_mpio)  && input$f_mpio !="Todos") base <- base |> dplyr::filter(MPIO_KEY  == input$f_mpio)
    base |> dplyr::group_by(anio) |> dplyr::summarise(valor_total = sum(as.numeric(valor), na.rm = TRUE), .groups = "drop")
  })
  output$plot_arriba <- plotly::renderPlotly({
    df <- series_data()
    plotly::plot_ly(
      data=df, x=~anio, y=~valor_total,
      type="scatter", mode="lines+markers",
      line=list(width=2, color=SERIE_COLOR),
      marker=list(size=6, color=SERIE_COLOR),
      text=~scales::comma(round(valor_total,0)),
      textposition="top center",
      textfont=list(family="Inter", color=SERIE_COLOR),
      hovertemplate="<b>Año:</b> %{x}<br>Deforestación (Ha): %{y:,}<extra></extra>"
    ) |>
      plotly::layout(
        font = list(family = "Inter"),
        xaxis=list(title="Año", tickmode="linear", dtick=1),
        yaxis=list(title="Deforestación (Ha)", separatethousands=TRUE),
        hovermode="x unified", margin=list(l=60, r=20, t=40, b=50)
      )
  })
  
  # Ranking descendente
  ranking_data <- reactive({
    datos_filtrados() |>
      dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) |>
      dplyr::summarise(valor_total = sum(valor, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(valor_total)) |>
      dplyr::slice_head(n = 10) |>
      dplyr::mutate(MPIO_TC = to_title(MUNICIPIO_D), DEPTO_TC = to_title(DEPARTAMENTO_D))
  })
  output$ranking_abajo <- plotly::renderPlotly({
    plot_df <- ranking_data()
    if (!nrow(plot_df)) {
      return(plotly::plot_ly() |> plotly::layout(annotations = list(
        text="Sin datos para el ranking", x=0.5, y=0.5, showarrow=FALSE)))
    }
    plot_df <- plot_df |> dplyr::mutate(etiqueta = paste0(MPIO_TC, " (", DEPTO_TC, ")"))
    
    plotly::plot_ly(
      data = plot_df, x = ~valor_total, y = ~etiqueta,
      type = "bar", orientation = "h",
      marker = list(color = BAR_COLOR),
      text = ~scales::comma(round(valor_total, 0)),
      textposition = "outside",
      textfont = list(family = "Inter", color = BAR_COLOR),
      hovertemplate = "<b>Municipio:</b> %{customdata[0]}<br><b>Departamento:</b> %{customdata[1]}<br><b>Deforestación (Ha):</b> %{x:,}<extra></extra>",
      customdata = cbind(plot_df$MPIO_TC, plot_df$DEPTO_TC)
    ) |>
      plotly::layout(
        font = list(family = "Inter"),
        xaxis = list(title = "Deforestación (Ha)", separatethousands = TRUE, gridcolor = "#e6e6e6"),
        yaxis = list(title = "", categoryorder = "array", categoryarray = rev(plot_df$etiqueta)),
        margin = list(l = 160, r = 40, t = 20, b = 40)
      )
  })
  
  # Descargas
  tabla_export <- reactive({
    datos_filtrados() |> dplyr::transmute(DEPARTAMENTO_D, MUNICIPIO_D, anio, deforestacion_ha = valor)
  })
  output$dl_csv_expl <- downloadHandler(
    filename = function() paste0("HANSEN_deforestacion_", safe_chr(input$f_anio), "_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(tabla_export(), file, na = "")
  )
  output$dl_png_series <- downloadHandler(
    filename = function() paste0("HANSEN_serie_", Sys.Date(), ".png"),
    content  = function(file){
      df <- series_data()
      g <- ggplot(df, aes(x=anio, y=valor_total)) +
        geom_line(linewidth=0.9, color=SERIE_COLOR) +
        geom_point(size=2.2, color=SERIE_COLOR) +
        geom_text(aes(label=scales::comma(round(valor_total,0))), vjust=-0.6, size=3, color=SERIE_COLOR, family="Inter") +
        scale_x_continuous(breaks=unique(df$anio)) +
        labs(x="Año", y="Deforestación (Ha)", title="¿Cómo viene la deforestación año a año?") +
        theme_minimal(base_size=12) +
        theme(text = element_text(family="Inter"),
              panel.grid.minor=element_blank(), panel.grid.major.x=element_blank())
      ggsave(filename=file, plot=g, device=ragg::agg_png, width=10, height=5, dpi=200, units="in")
    }
  )
  map_widget_simple <- reactive({
    if (nivel_mapa()=="depto"){
      mdat <- depto_sf |> dplyr::left_join(agg_depto(), by="DEPARTAMENTO_D") |> 
        dplyr::mutate(valor = ifelse(is.na(valor), 0, valor),
                      DEPTO_TC = to_title(DEPARTAMENTO_D))
      pal  <- palBin5(mdat$valor)
      leaflet::leaflet(mdat, options=leaflet::leafletOptions(zoomControl=FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(fillColor=~pal(valor), weight=0.5, color="#666", fillOpacity=0.9,
                             label = ~sprintf("%s — %s Ha", DEPTO_TC, scales::comma(round(valor,0))),
                             labelOptions = hover_label_opts) |>
        leaflet::addControl(html = htmltools::HTML(
          sprintf("<div style='font-weight:600;font-size:14px;background:#fff;padding:6px 8px;border-radius:8px;border:1px solid #e6e6e6'>
                    ¿Dónde está pegando más la deforestación en %s?
                  </div>", safe_chr(input$f_anio))), position="topleft")
    } else {
      dep <- depto_sel()
      mdat <- mpios_sf |> dplyr::filter(DEPARTAMENTO_D==dep) |>
        dplyr::left_join(agg_mpio(), by="MUNICIPIO_D") |> 
        dplyr::mutate(valor = ifelse(is.na(valor), 0, valor),
                      MPIO_TC = to_title(MUNICIPIO_D))
      pal  <- palBin5(mdat$valor)
      leaflet::leaflet(mdat, options=leaflet::leafletOptions(zoomControl=FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(fillColor=~pal(valor), weight=0.4, color="#666", fillOpacity=0.9,
                             label=~sprintf("%s — %s Ha", MPIO_TC, scales::comma(round(valor,0))),
                             labelOptions = hover_label_opts_small) |>
        leaflet::addControl(html = htmltools::HTML(
          sprintf("<div style='font-weight:600;font-size:14px;background:#fff;padding:6px 8px;border-radius:8px;border:1px solid #e6e6e6'>
                    ¿Dónde se concentra en %s?
                  </div>", safe_chr(input$f_anio))), position="topleft")
    }
  })
  output$dl_png_mapa <- downloadHandler(
    filename = function() paste0("HANSEN_mapa_", Sys.Date(), ".png"),
    content  = function(file){
      widget <- map_widget_simple()
      tmp_html <- tempfile(fileext=".html")
      htmlwidgets::saveWidget(widget, tmp_html, selfcontained=TRUE)
      webshot2::webshot(tmp_html, file=file, vwidth=1200, vheight=800, zoom=2)
    }
  )
  output$dl_png_ranking <- downloadHandler(
    filename = function() paste0("HANSEN_ranking_", safe_chr(input$f_anio), "_", Sys.Date(), ".png"),
    content  = function(file){
      plot_df <- ranking_data() |> dplyr::mutate(etiqueta = paste0(MPIO_TC, " (", DEPTO_TC, ")"))
      g <- ggplot(plot_df, aes(x = valor_total, y = reorder(etiqueta, -valor_total))) +
        geom_col(fill = BAR_COLOR) +
        geom_text(aes(label = scales::comma(round(valor_total, 0))), hjust=-0.1, size=3, color = BAR_COLOR, family="Inter") +
        scale_x_continuous(labels=scales::comma, expand = expansion(mult = c(0, 0.10))) +
        labs(x="Deforestación (Ha)", y=NULL, title=paste0("Los 10 municipios que más pierden bosque",
                                                          if (!is.null(input$f_anio)) paste0(" en ", input$f_anio) else "")) +
        theme_minimal(base_size=12) +
        theme(text = element_text(family="Inter"),
              axis.text.y=element_text(size=9),
              plot.margin=margin(r=30),
              panel.grid.minor=element_blank(),
              panel.grid.major.x=element_line(color="#e6e6e6"))
      ggsave(filename=file, plot=g, device=ragg::agg_png, width=10, height=6, dpi=200, units="in")
    }
  )
}

# Lanzar App
shinyApp(ui = ui, server = server)


