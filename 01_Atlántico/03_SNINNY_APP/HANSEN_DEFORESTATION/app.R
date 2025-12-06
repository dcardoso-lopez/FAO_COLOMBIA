# =========================================================
# Shiny App — HANSEN Deforestación (Solo Tab 1)
# =========================================================

# 1) Paquetes
pkgs <- c(
  "shiny","bslib","dplyr","readr","stringi","sf","leaflet",
  "plotly","ggplot2","htmltools","webshot2","htmlwidgets",
  "ragg","glue","scales"
)
suppressWarnings(invisible(sapply(pkgs, require, character.only = TRUE)))
options(stringsAsFactors = FALSE, scipen = 999)
sf::sf_use_s2(FALSE)

# 2) Rutas (ajústalas a tu equipo)
APP_DIR  <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/HANSEN_DEFORESTATION"
DATA_RDS <- file.path(APP_DIR, "data/141_HANSEN_DEFORESTATION.rds")
SHP_DIR  <- file.path(APP_DIR, "data/shp")

if (!file.exists(DATA_RDS)) stop("No encuentro la base RDS en: ", DATA_RDS)
shp_files <- list.files(SHP_DIR, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
if (length(shp_files) == 0) stop("No encuentro archivos .shp en: ", SHP_DIR)

# 3) Helpers básicos
norm_txt <- function(x){
  x <- as.character(x)
  x <- trimws(x)
  x <- stringi::stri_trans_nfc(x)
  x
}

safe_chr <- function(x) if (is.null(x)) "" else as.character(x)

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
      if (i == 1 || !(base %in% stopw)) {
        toks_out[i] <- stringi::stri_trans_totitle(base, locale = "es")
      } else {
        toks_out[i] <- base
      }
    }
    paste(toks_out, collapse = " ")
  }, character(1))
}

# Helpers filtros (pares únicos)
distinct_pairs <- function(df, key_col, disp_col){
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

# Formato numérico es-CO
fmt_num <- function(x, accuracy = 1){
  scales::number(
    x,
    accuracy     = accuracy,
    big.mark     = ".",
    decimal.mark = ","
  )
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

legend_lab_es <- function(prefix = "", suffix = "", between = " – "){
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

# Paleta
SERIE_COLOR <- "#6B4F2C"
BAR_COLOR   <- "#6B4F2C"

pal4_vec <- grDevices::colorRampPalette(
  c("#F6E8C3", "#EBD3A6", "#C9A56A", "#9A7547", "#6B4F2C")
)(4)

make_bins4 <- function(values){
  v <- as.numeric(values); v <- v[is.finite(v)]
  if (!length(v)) return(seq(0,4))
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
  bins <- make_bins4(values)
  pal <- leaflet::colorBin(
    palette = pal4_vec,
    bins    = bins,
    domain  = values,
    na.color = "#f0f0f0",
    right    = FALSE
  )
  attr(pal, "bins") <- bins
  pal
}

# =========================================================
# 4) Shapefiles — usando columnas exactas:
#    - Dptos: DPTO_CCDGO, DPTO_CNMBR
#    - Mpios: DPTO_CCDGO, MPIO_CDPMP, MPIO_CNMBR
# =========================================================

ruta_shp_dptos <- shp_files[grep("DPTOS", basename(shp_files), ignore.case = TRUE)][1]
ruta_shp_mpios <- shp_files[grep("MPIOS", basename(shp_files), ignore.case = TRUE)][1]

if (is.na(ruta_shp_dptos) || is.na(ruta_shp_mpios)) {
  stop("No pude identificar shapefiles de departamentos/municipios (busqué 'DPTOS' y 'MPIOS').")
}

depto_sf_raw <- sf::st_read(ruta_shp_dptos, quiet = TRUE)
mpios_sf_raw <- sf::st_read(ruta_shp_mpios, quiet = TRUE)

# Departamentos shapefile
depto_sf <- depto_sf_raw |>
  dplyr::mutate(
    COD_DANE_DPTO_D = stringi::stri_pad_left(as.character(DPTO_CCDGO), 2, "0"),
    DEPARTAMENTO_D  = toupper(norm_txt(DPTO_CNMBR))
  ) |>
  sf::st_transform(4326) |>
  sf::st_make_valid() |>
  sf::st_zm(drop = TRUE, what = "ZM")

depto_key <- depto_sf |>
  sf::st_drop_geometry() |>
  dplyr::select(COD_DANE_DPTO_D, DEPARTAMENTO_D) |>
  dplyr::distinct()

# Municipios shapefile
mpios_sf <- mpios_sf_raw |>
  dplyr::mutate(
    COD_DANE_DPTO_D = stringi::stri_pad_left(as.character(DPTO_CCDGO), 2, "0"),
    COD_DANE_MPIO_D = stringi::stri_pad_left(as.character(MPIO_CDPMP), 5, "0"),
    MUNICIPIO_D     = norm_txt(MPIO_CNMBR)
  ) |>
  dplyr::left_join(depto_key, by = "COD_DANE_DPTO_D") |>
  sf::st_transform(4326) |>
  sf::st_make_valid() |>
  sf::st_zm(drop = TRUE, what = "ZM") |>
  dplyr::mutate(
    MUNICIPIO_D    = norm_txt(MUNICIPIO_D),
    DEPARTAMENTO_D = toupper(norm_txt(DEPARTAMENTO_D))
  )

# Claves de nombres desde shapefile (para asegurar nombres bonitos en filtros)
depto_key_shp <- depto_key |>
  dplyr::rename(DEPARTAMENTO_SH = DEPARTAMENTO_D)

mpio_key_shp <- mpios_sf |>
  sf::st_drop_geometry() |>
  dplyr::select(COD_DANE_MPIO_D, MUNICIPIO_SH = MUNICIPIO_D) |>
  dplyr::distinct()

# =========================================================
# 5) Base HANSEN (RDS)
#    Debe tener COD_DANE_DPTO_D, COD_DANE_MPIO_D, año, valor y nombres
# =========================================================
base_raw <- readRDS(DATA_RDS)
base_raw <- base_raw %>% dplyr::filter(DEPARTAMENTO_D=="ATLÁNTICO")

year_cands  <- c("year","anio","ano","ano_evento","ano_cosechado","ano_sembrado","lossyear")
valor_cands <- c("has")
depto_cands <- c("DEPARTAMENTO_D","depto","departamento","DEPARTAMENTO")
mpio_cands  <- c("MUNICIPIO_D","mpio","municipio","MUNICIPIO")

depto_code_cands_rds <- c("COD_DANE_DPTO_D","COD_DANE_DEPTO","COD_DEPTO","CODDPTO")
muni_code_cands_rds  <- c("COD_DANE_MPIO_D","COD_DANE_MUNIC_D","COD_DANE_MUNIC","COD_MPIO","CODMUN")

pick_first <- function(nms, candidates){
  cand <- candidates[candidates %in% nms]
  if (!length(cand)) NA_character_ else cand[1]
}

bn <- names(base_raw)
col_year     <- pick_first(bn, year_cands)
col_val      <- pick_first(bn, valor_cands)
col_dep_name <- pick_first(bn, depto_cands)
col_mpio_name<- pick_first(bn, mpio_cands)
col_dep_code <- pick_first(bn, depto_code_cands_rds)
col_mun_code <- pick_first(bn, muni_code_cands_rds)

if (is.na(col_year) || is.na(col_val))
  stop("No pude detectar columnas de año/valor en la base RDS.")
if (is.na(col_dep_code) || is.na(col_mun_code))
  stop("No pude detectar COD_DANE_DPTO_D / COD_DANE_MPIO_D en la base RDS.")

# Construimos eva_df y SOBREESCRIBIMOS nombres con los de shapefile
eva_df <- base_raw |>
  dplyr::mutate(
    anio  = as.integer(.data[[col_year]]),
    valor = suppressWarnings(as.numeric(.data[[col_val]])),
    COD_DANE_DPTO_D = stringi::stri_pad_left(
      as.character(.data[[col_dep_code]]), width = 2, pad = "0"
    ),
    COD_DANE_MPIO_D = stringi::stri_pad_left(
      as.character(.data[[col_mun_code]]), width = 5, pad = "0"
    ),
    # Nombres originales (por si acaso)
    MUNICIPIO_D_RAW    = norm_txt(dplyr::coalesce(.data[[col_mpio_name]], "")),
    DEPARTAMENTO_D_RAW = toupper(norm_txt(dplyr::coalesce(.data[[col_dep_name]], "")))
  ) |>
  # Traemos nombres del shapefile (asegura que el filtro NO muestre números)
  dplyr::left_join(depto_key_shp, by = "COD_DANE_DPTO_D") |>
  dplyr::left_join(mpio_key_shp,  by = "COD_DANE_MPIO_D") |>
  dplyr::mutate(
    DEPARTAMENTO_D = toupper(norm_txt(dplyr::coalesce(DEPARTAMENTO_SH, DEPARTAMENTO_D_RAW))),
    MUNICIPIO_D    = norm_txt(dplyr::coalesce(MUNICIPIO_SH, MUNICIPIO_D_RAW)),
    DEPTO_KEY      = COD_DANE_DPTO_D,
    DEPTO_DISP     = DEPARTAMENTO_D,
    MPIO_KEY       = COD_DANE_MPIO_D,
    MPIO_DISP      = MUNICIPIO_D
  )

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    primary = "#F57C00",
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius"  = "0.9rem",
    "font-size-base" = "0.95rem"
  ),
  tags$head(
    tags$style(HTML("
      :root{
        --accent-border:#F57C00;
        --gap:12px;
        --viz-row-top:360px;
        --viz-row-bottom:320px;
      }
      .wrap{
        max-width:1360px;
        margin:0 auto;
        padding:16px 20px 32px;
      }
      h2#app-title{
        font-weight:700;
        letter-spacing:.2px;
        margin-top:4px;
        margin-bottom:6px;
        text-align:left;
      }
      .data-note{
        font-size:13px;
        color:#6b7280;
        margin:0 0 16px;
      }
      .filters{
        background:#fff;
        border:1px solid var(--accent-border);
        border-radius:16px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
        padding:6px 12px 8px;
        margin-bottom:12px;
      }
      .filters-grid{
        display:grid;
        grid-template-columns:repeat(3,minmax(220px,1fr));
        gap:12px;
        align-items:stretch;
      }
      .filter{
        display:flex;
        flex-direction:column;
        justify-content:flex-start;
      }
      .filter-label,
      .filters-grid label,
      .filters-grid .control-label{
        font-family:'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-size:14px;
        font-weight:500;
        letter-spacing:.2px;
        color:#111827;
        margin-bottom:6px;
      }
      .filters-grid .shiny-input-container{
        margin:0 !important;
      }
      .filters-grid .selectize-input,
      .filters-grid .form-control,
      .filters-grid .form-select{
        height:60px !important;
        min-height:60px;
        padding-top:10px;
        padding-bottom:10px;
        border-radius:10px;
        border:1px solid var(--accent-border) !important;
      }
      .filters-grid .selectize-input:focus,
      .filters-grid .form-control:focus,
      .filters-grid .form-select:focus{
        border-color:var(--accent-border) !important;
        box-shadow:0 0 0 .2rem rgba(245,124,0,.25) !important;
      }
      .card{
        background:#fff;
        border:1px solid var(--accent-border);
        border-radius:16px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
        padding:12px;
        margin-bottom:12px;
      }
      .card-title{
        font-weight:700;
        font-size:16px;
        margin-bottom:8px;
        color:#111827;
      }
      .content-grid{
        display:grid;
        grid-template-columns:1.05fr 1fr;
        grid-template-rows:var(--viz-row-top) var(--viz-row-bottom);
        gap:var(--gap);
      }
      .viz-card{
        display:flex;
        flex-direction:column;
        height:100%;
        margin:0;
      }
      .viz-body{
        flex:1 1 auto;
        min-height:0;
      }
      .viz-map{
        grid-row:1 / span 2;
      }
      .viz-body .leaflet,
      .viz-body .plotly.html-widget{
        height:100% !important;
      }
      .leaflet-tooltip.lbl-clean{
        background: rgba(255,255,255,.92);
        border: 1px solid #e6e6e6;
        border-radius: 6px;
        padding: 4px 6px;
        color: #222;
        font-weight: 600;
        box-shadow: 0 1px 4px rgba(0,0,0,.08);
      }
      .leaflet-control, .leaflet-control .legend, .leaflet-control .info{
        border-radius:12px;
      }
      .leaflet-top .leaflet-control { margin-top: 6px; }
      .leaflet-left .leaflet-control { margin-left: 6px; }
      .btn, .btn-default {
        font-size:12px;
        padding:6px 10px;
        border-radius:8px;
        border-color:var(--accent-border) !important;
      }
      .form-control{
        border-color:var(--accent-border) !important;
      }
      .form-control:focus{
        border-color:var(--accent-border) !important;
        box-shadow:0 0 0 0.2rem rgba(245,124,0,0.25);
      }
      .selectize-input{
        border-color:var(--accent-border) !important;
      }
      .selectize-input.focus{
        border-color:var(--accent-border) !important;
        box-shadow:0 0 0 0.2rem rgba(245,124,0,0.25);
      }
      input[type='radio'],
      input[type='checkbox']{
        accent-color:#F57C00;
      }
      .dl-under{ margin-top:8px; text-align:right; }
      .dl-footer{ margin-top:10px; text-align:right; }
      .map-note{
        font-size:12px;
        color:#4b5563;
        margin-top:6px;
      }
    "))
  ),
  div(
    class = "wrap",
    h2("Explorador territorial — Deforestación (Hansen)", id = "app-title"),
    div(
      class = "data-note",
      HTML("Exploración de la deforestación anual (ha) a nivel departamental y municipal, con información Hansen.")
    ),
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
          div(class="filter-label","¿En qué departamento?"),
          {
            dep_pairs <- distinct_pairs(eva_df, "DEPTO_KEY", "DEPTO_DISP")
            # Default: Atlántico según nombre, pero seleccionamos el CÓDIGO
            idx_atl <- which(
              toupper(norm_txt(dep_pairs$DEPTO_DISP)) %in% c("ATLANTICO","ATLÁNTICO")
            )
            default_dep <- if (length(idx_atl)) dep_pairs$DEPTO_KEY[idx_atl[1]] else "Todos"
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
    div(
      class = "content-grid",
      div(
        class = "card viz-card viz-map",
        div(
          class="card-title d-flex align-items-center",
          span(textOutput("titulo_mapa"))
        ),
        div(
          style="display:flex; gap:10px; align-items:center; margin-bottom:8px;",
          actionButton("btn_volver", "◀ Volver a Departamentos", class="btn btn-light"),
          strong(textOutput("nivel_txt", inline = TRUE))
        ),
        div(class="viz-body", leafletOutput("map_eva", height = "100%")),
        div(
          class = "map-note",
          "Nota: los rangos de color del mapa se construyen con cuartiles del indicador (4 clases) según el filtro actual."
        ),
        div(class="dl-under", downloadButton("dl_png_mapa","PNG — Mapa (simple)"))
      ),
      div(
        class = "card viz-card",
        div(class="card-title", textOutput("titulo_serie")),
        div(class="viz-body", plotlyOutput("plot_arriba", height = "100%")),
        div(class="dl-under", downloadButton("dl_png_series","PNG — Serie temporal"))
      ),
      div(
        class = "card viz-card",
        div(class="card-title", textOutput("titulo_ranking")),
        div(class="viz-body", plotlyOutput("ranking_abajo", height = "100%")),
        div(class="dl-under", downloadButton("dl_png_ranking","PNG — Ranking Top-10"))
      )
    ),
    div(class="dl-footer", downloadButton("dl_csv_expl","Descargar CSV (filtro actual)"))
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  
  nivel_mapa <- reactiveVal("depto")
  depto_sel  <- reactiveVal(NULL)  # guarda COD_DANE_DPTO_D
  
  output$nivel_txt <- renderText({
    if (nivel_mapa() == "depto") {
      "Nivel: Departamentos"
    } else {
      dep_code <- depto_sel()
      if (is.null(dep_code) || !nzchar(dep_code)) return("Nivel: Municipios")
      nombre <- depto_key |>
        dplyr::filter(COD_DANE_DPTO_D == dep_code) |>
        dplyr::pull(DEPARTAMENTO_D)
      nombre <- if (length(nombre)) nombre[1] else dep_code
      paste0("Nivel: Municipios — ", title_case_es(nombre))
    }
  })
  
  output$anio_ui <- renderUI({
    yrs <- sort(unique(na.omit(as.integer(eva_df$anio))))
    selectInput("f_anio", NULL, choices = yrs, selected = max(yrs))
  })
  
  # Cascada dpto -> mpio (usando DEPTO_KEY/MPIO_KEY, que son códigos)
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
  
  # Datos filtrados (PARA SERIE, RANKING, CSV)
  datos_filtrados <- reactive({
    df <- eva_df
    if (!is.null(input$f_depto) && input$f_depto != "Todos")
      df <- df |> dplyr::filter(DEPTO_KEY == input$f_depto)
    if (!is.null(input$f_mpio) && input$f_mpio != "Todos")
      df <- df |> dplyr::filter(MPIO_KEY == input$f_mpio)
    if (!is.null(input$f_anio))
      df <- df |> dplyr::filter(anio == input$f_anio)
    df |> dplyr::mutate(valor = suppressWarnings(as.numeric(valor)))
  })
  
  output$titulo_mapa    <- renderText("¿Qué territorios presentan mayor cantidad de hectáreas deforestadas?")
  output$titulo_serie   <- renderText("¿Cómo viene la deforestación año a año?")
  output$titulo_ranking <- renderText("Top 10 de los territorios con mayor pérdida de cobertura boscosa")
  
  # Agregaciones usando COD_DANE_DPTO_D / COD_DANE_MPIO_D
  agg_depto <- reactive({
    datos_filtrados() |>
      dplyr::group_by(COD_DANE_DPTO_D) |>
      dplyr::summarise(
        valor = sum(valor, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  agg_mpio <- reactive({
    df <- datos_filtrados()
    if (!is.null(depto_sel()))
      df <- df |> dplyr::filter(COD_DANE_DPTO_D == depto_sel())
    df |>
      dplyr::group_by(
        COD_DANE_MPIO_D,
        COD_DANE_DPTO_D
      ) |>
      dplyr::summarise(
        valor = sum(valor, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  badge_filtros <- reactive({
    yr <- safe_chr(input$f_anio)
    htmltools::HTML(sprintf(
      '<div style="background:#fff;padding:6px 10px;border-radius:8px;
                   box-shadow:0 1px 6px rgba(0,0,0,.15);font-size:12px;line-height:1.3;">
         <b>Indicador:</b> Deforestación (ha)<br>
         <b>Año:</b> %s
       </div>', yr))
  })
  
  hover_label_opts       <- leaflet::labelOptions(
    direction="auto", textsize="12px", sticky=TRUE,
    opacity=0.95, className="lbl-clean"
  )
  hover_label_opts_small <- leaflet::labelOptions(
    direction="auto", textsize="11px", sticky=TRUE,
    opacity=0.95, className="lbl-clean"
  )
  
  # ---- Mapa inicial: si hay depto -> municipios; si no, departamentos ----
  output$map_eva <- leaflet::renderLeaflet({
    req(input$f_anio, input$f_depto)
    
    # SIN departamento filtrado -> vista departamental
    if (is.null(input$f_depto) || input$f_depto == "Todos") {
      nivel_mapa("depto")
      depto_sel(NULL)
      
      mdat <- depto_sf |>
        dplyr::left_join(
          agg_depto(),
          by = "COD_DANE_DPTO_D"
        ) |>
        dplyr::mutate(
          valor    = suppressWarnings(as.numeric(valor)),
          DEPTO_TC = title_case_es(DEPARTAMENTO_D)
        )
      pal  <- palBin4(mdat$valor)
      
      leaflet::leaflet(mdat) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          layerId   = ~COD_DANE_DPTO_D,
          fillColor = ~pal(valor),
          weight    = 0.7, color = "#666", fillOpacity = 0.9,
          label = ~sprintf(
            "%s — %s ha",
            DEPTO_TC,
            fmt_num(valor, accuracy = 0.1)
          ),
          labelOptions     = hover_label_opts,
          highlightOptions = leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
        ) |>
        leaflet::addLegend(
          position   = "bottomright",
          pal        = pal,
          values     = ~valor,
          title      = "Hectáreas",
          labFormat  = legend_lab_es()
        ) |>
        leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
      
      # CON departamento filtrado -> vista municipal de ese departamento
    } else {
      dep <- input$f_depto
      nivel_mapa("mpio")
      depto_sel(dep)
      
      # Agregación local por municipio en ese departamento y año
      df_agg <- eva_df |>
        dplyr::filter(
          COD_DANE_DPTO_D == dep,
          anio == input$f_anio
        ) |>
        dplyr::group_by(COD_DANE_MPIO_D, COD_DANE_DPTO_D) |>
        dplyr::summarise(
          valor = sum(as.numeric(valor), na.rm = TRUE),
          .groups = "drop"
        )
      
      mdat <- mpios_sf |>
        dplyr::filter(COD_DANE_DPTO_D == dep) |>
        dplyr::left_join(
          df_agg,
          by = c("COD_DANE_MPIO_D","COD_DANE_DPTO_D")
        ) |>
        dplyr::mutate(
          valor    = suppressWarnings(as.numeric(valor)),
          MPIO_TC  = title_case_es(MUNICIPIO_D),
          DEPTO_TC = title_case_es(DEPARTAMENTO_D)
        )
      
      pal  <- palBin4(mdat$valor)
      
      leaflet::leaflet(mdat) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          layerId   = ~COD_DANE_MPIO_D,
          fillColor = ~pal(valor),
          weight    = 0.4, color="#666", fillOpacity=0.9,
          label = ~sprintf(
            "%s (%s) — %s ha",
            MPIO_TC, DEPTO_TC,
            fmt_num(valor, accuracy = 0.1)
          ),
          labelOptions = hover_label_opts_small,
          highlightOptions = leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
        ) |>
        leaflet::addLegend(
          position   = "bottomright",
          pal        = pal,
          values     = mdat$valor,
          title      = "Hectáreas",
          labFormat  = legend_lab_es()
        ) |>
        leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
    }
  })
  
  observe({
    leaflet::leafletProxy("map_eva") |>
      leaflet::removeControl("badge_filtros") |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  })
  
  dibujar_deptos <- function(){
    mdat <- depto_sf |>
      dplyr::left_join(agg_depto(),
                       by = "COD_DANE_DPTO_D") |>
      dplyr::mutate(
        valor    = suppressWarnings(as.numeric(valor)),  # NA se queda NA
        DEPTO_TC = title_case_es(DEPARTAMENTO_D)
      )
    pal  <- palBin4(mdat$valor)
    
    leaflet::leafletProxy("map_eva", data = mdat) |>
      leaflet::clearPopups() |>
      leaflet::clearShapes() |>
      leaflet::clearMarkers() |>
      leaflet::clearControls() |>
      leaflet::addPolygons(
        layerId   = ~COD_DANE_DPTO_D,
        fillColor = ~pal(valor),
        weight    = 0.7, color="#666", fillOpacity=0.9,
        label = ~sprintf(
          "%s — %s ha",
          DEPTO_TC,
          fmt_num(valor, accuracy = 0.1)
        ),
        labelOptions = hover_label_opts,
        highlightOptions = leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
      ) |>
      leaflet::addLegend(
        position   = "bottomright",
        pal        = pal,
        values     = ~valor,
        title      = "Hectáreas",
        labFormat  = legend_lab_es()
      ) |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  }
  
  dibujar_mpios <- function(dep_code){
    mdat <- mpios_sf |>
      dplyr::filter(COD_DANE_DPTO_D == dep_code) |>
      dplyr::left_join(
        agg_mpio(),
        by = c("COD_DANE_MPIO_D","COD_DANE_DPTO_D")
      ) |>
      dplyr::mutate(
        valor    = suppressWarnings(as.numeric(valor)),
        MPIO_TC  = title_case_es(MUNICIPIO_D),
        DEPTO_TC = title_case_es(DEPARTAMENTO_D)
      )
    
    pal  <- palBin4(mdat$valor)
    
    leaflet::leafletProxy("map_eva", data = mdat) |>
      leaflet::clearPopups() |>
      leaflet::clearShapes() |>
      leaflet::clearMarkers() |>
      leaflet::clearControls() |>
      leaflet::addPolygons(
        layerId   = ~COD_DANE_MPIO_D,
        fillColor = ~pal(valor),
        weight    = 0.4, color="#666", fillOpacity=0.9,
        label = ~sprintf(
          "%s (%s) — %s ha",
          MPIO_TC, DEPTO_TC,
          fmt_num(valor, accuracy = 0.1)
        ),
        labelOptions = hover_label_opts_small,
        highlightOptions = leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
      ) |>
      leaflet::addLegend(
        position   = "bottomright",
        pal        = pal,
        values     = mdat$valor,
        title      = "Hectáreas",
        labFormat  = legend_lab_es()
      ) |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  }
  
  # Cambios de departamento → cambiar nivel y redibujar
  observeEvent(input$f_depto, {
    dep <- if (is.null(input$f_depto) || input$f_depto == "Todos") NULL else input$f_depto
    if (is.null(dep)) {
      nivel_mapa("depto"); depto_sel(NULL); dibujar_deptos()
    } else {
      nivel_mapa("mpio"); depto_sel(dep); dibujar_mpios(dep)
    }
  }, ignoreInit = TRUE)
  
  # Cambios de municipio → redibujar mapa de municipios (mostrando el resto en NA)
  observeEvent(input$f_mpio, {
    if (nivel_mapa() == "mpio" && !is.null(depto_sel())) {
      dibujar_mpios(depto_sel())
    }
  }, ignoreInit = TRUE)
  
  # Click en mapa de departamentos → bajar a municipios
  observeEvent(input$map_eva_shape_click, {
    click <- input$map_eva_shape_click
    if (is.null(click$id)) return()
    if (nivel_mapa() == "depto") {
      depto_sel(click$id)
      updateSelectInput(session, "f_depto", selected = click$id)
      nivel_mapa("mpio")
      dibujar_mpios(click$id)
    }
  })
  
  observeEvent(input$btn_volver, {
    updateSelectInput(session, "f_depto", selected="Todos")
    updateSelectInput(session, "f_mpio",  selected="Todos")
    nivel_mapa("depto"); depto_sel(NULL); dibujar_deptos()
  })
  
  # -------- Serie temporal --------
  series_data <- reactive({
    base <- eva_df
    if (!is.null(input$f_depto) && input$f_depto!="Todos")
      base <- base |> dplyr::filter(DEPTO_KEY == input$f_depto)
    if (!is.null(input$f_mpio) && input$f_mpio!="Todos")
      base <- base |> dplyr::filter(MPIO_KEY == input$f_mpio)
    base |>
      dplyr::group_by(anio) |>
      dplyr::summarise(
        valor_total = sum(as.numeric(valor), na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(anio)
  })
  
  output$plot_arriba <- plotly::renderPlotly({
    df <- series_data()
    if (!nrow(df)) return(plotly::plot_ly())
    
    max_val   <- max(df$valor_total, na.rm = TRUE)
    breaks_y  <- pretty(c(0, max_val), n = 5)
    breaks_y  <- breaks_y[breaks_y >= 0]
    tick_text <- format_short(breaks_y)
    
    plotly::plot_ly(
      data=df,
      x=~anio,
      y=~valor_total,
      type="scatter", mode="lines+markers",
      line=list(width=2, color=SERIE_COLOR),
      marker=list(size=6, color=SERIE_COLOR),
      text = ~fmt_num(valor_total, accuracy = 0.1),
      hovertemplate="<b>Año:</b> %{x}<br>Deforestación (ha): %{text}<extra></extra>"
    ) |>
      plotly::layout(
        font = list(family = "Inter"),
        xaxis=list(
          title="",
          tickmode="linear", dtick=1,
          showgrid = FALSE
        ),
        yaxis=list(
          title="Hectáreas",
          tickvals = breaks_y,
          ticktext = tick_text,
          showgrid = FALSE
        ),
        hovermode="x unified",
        margin=list(l=60, r=20, t=30, b=50),
        legend=list(orientation="h")
      )
  })
  
  # -------- Ranking --------
  ranking_data <- reactive({
    datos_filtrados() |>
      dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D) |>
      dplyr::summarise(valor_total = sum(valor, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(valor_total)) |>
      dplyr::slice_head(n = 10) |>
      dplyr::mutate(
        muni_tc  = title_case_es(MUNICIPIO_D),
        depto_tc = title_case_es(DEPARTAMENTO_D)
      )
  })
  
  output$ranking_abajo <- plotly::renderPlotly({
    plot_df <- ranking_data()
    if (!nrow(plot_df)) {
      return(
        plotly::plot_ly() |>
          plotly::layout(annotations = list(
            text="Sin datos para el ranking",
            x=0.5, y=0.5, showarrow=FALSE))
      )
    }
    
    max_val   <- max(plot_df$valor_total, na.rm = TRUE)
    breaks    <- pretty(c(0, max_val), n = 5)
    breaks    <- breaks[breaks >= 0]
    tick_text <- format_short(breaks)
    
    plotly::plot_ly(
      data  = plot_df,
      x     = ~valor_total,
      y     = ~muni_tc,
      type  = "bar",
      orientation = "h",
      marker = list(color = BAR_COLOR),
      text  = ~fmt_num(valor_total, accuracy = 0.1),
      textposition = "inside",
      insidetextanchor = "middle",
      insidetextfont = list(
        family = "Inter SemiBold, Inter, Arial, sans-serif",
        size   = 14,
        color  = "white"
      ),
      customdata = cbind(
        plot_df$muni_tc,
        plot_df$depto_tc,
        fmt_num(plot_df$valor_total, accuracy = 0.1)
      ),
      hovertemplate = paste0(
        "<b>Municipio:</b> %{customdata[0]}",
        "<br><b>Departamento:</b> %{customdata[1]}",
        "<br><b>Deforestación (ha):</b> %{customdata[2]}",
        "<extra></extra>"
      ),
      cliponaxis = FALSE
    ) |>
      plotly::layout(
        font = list(family = "Inter"),
        xaxis = list(
          title   = "Hectáreas",
          tickvals = breaks,
          ticktext = tick_text,
          showgrid = FALSE
        ),
        yaxis = list(
          title = "",
          categoryorder = "array",
          categoryarray = rev(plot_df$muni_tc),
          showgrid = FALSE
        ),
        margin = list(l = 160, r = 40, t = 20, b = 40)
      )
  })
  
  # -------- CSV --------
  tabla_export <- reactive({
    datos_filtrados() |>
      dplyr::transmute(
        COD_DANE_DPTO_D  = COD_DANE_DPTO_D,
        COD_DANE_MPIO_D  = COD_DANE_MPIO_D,
        DEPARTAMENTO = title_case_es(DEPARTAMENTO_D),
        MUNICIPIO    = title_case_es(MUNICIPIO_D),
        anio,
        deforestacion_ha = valor
      )
  })
  
  output$dl_csv_expl <- downloadHandler(
    filename = function() paste0("HANSEN_deforestacion_", safe_chr(input$f_anio), "_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(tabla_export(), file, na = "")
  )
  
  # -------- PNG Serie --------
  output$dl_png_series <- downloadHandler(
    filename = function() paste0("HANSEN_serie_", Sys.Date(), ".png"),
    content  = function(file){
      df <- series_data()
      if (!nrow(df)) { file.create(file); return() }
      max_val   <- max(df$valor_total, na.rm = TRUE)
      breaks_y  <- pretty(c(0, max_val), n = 5)
      breaks_y  <- breaks_y[breaks_y >= 0]
      
      g <- ggplot(df, aes(x=anio, y=valor_total)) +
        geom_line(linewidth=0.9, color=SERIE_COLOR) +
        geom_point(size=2.2, color=SERIE_COLOR) +
        scale_x_continuous(breaks=unique(df$anio)) +
        scale_y_continuous(
          labels = format_short,
          breaks = breaks_y
        ) +
        labs(x="Año", y="Deforestación (ha)",
             title="¿Cómo viene la deforestación año a año?") +
        theme_minimal(base_size=12) +
        theme(
          text = element_text(family="Inter"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()
        )
      ggsave(filename=file, plot=g, device=ragg::agg_png,
             width=10, height=5, dpi=200, units="in")
    }
  )
  
  # -------- Mapa simple para PNG --------
  map_widget_simple <- reactive({
    if (nivel_mapa()=="depto"){
      mdat <- depto_sf |>
        dplyr::left_join(agg_depto(),
                         by = "COD_DANE_DPTO_D") |>
        dplyr::mutate(
          valor    = suppressWarnings(as.numeric(valor)),
          DEPTO_TC = title_case_es(DEPARTAMENTO_D)
        )
      pal  <- palBin4(mdat$valor)
      leaflet::leaflet(mdat, options=leaflet::leafletOptions(zoomControl=FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          fillColor=~pal(valor), weight=0.5, color="#666", fillOpacity=0.9,
          label = ~sprintf(
            "%s — %s ha",
            DEPTO_TC,
            fmt_num(valor, accuracy = 0.1)
          ),
          labelOptions = hover_label_opts
        ) |>
        leaflet::addControl(html = htmltools::HTML(
          sprintf("<div style='font-weight:600;font-size:14px;background:#fff;
                   padding:6px 8px;border-radius:8px;border:1px solid #e6e6e6'>
                    ¿Dónde está pegando más la deforestación en %s?
                  </div>", safe_chr(input$f_anio))), position="topleft")
    } else {
      dep <- depto_sel()
      mdat <- mpios_sf |>
        dplyr::filter(COD_DANE_DPTO_D == dep) |>
        dplyr::left_join(
          agg_mpio(),
          by = c("COD_DANE_MPIO_D","COD_DANE_DPTO_D")
        ) |>
        dplyr::mutate(
          valor    = suppressWarnings(as.numeric(valor)),
          MPIO_TC  = title_case_es(MUNICIPIO_D)
        )
      pal  <- palBin4(mdat$valor)
      leaflet::leaflet(mdat, options=leaflet::leafletOptions(zoomControl=FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          fillColor=~pal(valor), weight=0.4, color="#666", fillOpacity=0.9,
          label=~sprintf(
            "%s — %s ha",
            MPIO_TC,
            fmt_num(valor, accuracy = 0.1)
          ),
          labelOptions=hover_label_opts_small
        ) |>
        leaflet::addControl(html = htmltools::HTML(
          sprintf("<div style='font-weight:600;font-size:14px;background:#fff;
                   padding:6px 8px;border-radius:8px;border:1px solid #e6e6e6'>
                    ¿Dónde se concentra en %s?
                  </div>", safe_chr(input$f_anio))), position="topleft")
    }
  })
  
  output$dl_png_mapa <- downloadHandler(
    filename = function() paste0("HANSEN_mapa_", Sys.Date(), ".png"),
    content  = function(file){
      widget   <- map_widget_simple()
      tmp_html <- tempfile(fileext=".html")
      htmlwidgets::saveWidget(widget, tmp_html, selfcontained=TRUE)
      webshot2::webshot(tmp_html, file=file, vwidth=1200, vheight=800, zoom=2)
    }
  )
  
  # -------- PNG Ranking --------
  output$dl_png_ranking <- downloadHandler(
    filename = function() paste0("HANSEN_ranking_", safe_chr(input$f_anio), "_", Sys.Date(), ".png"),
    content  = function(file){
      plot_df <- ranking_data()
      if (!nrow(plot_df)) { file.create(file); return() }
      plot_df <- plot_df |>
        dplyr::mutate(etiqueta = muni_tc)
      max_val <- max(plot_df$valor_total, na.rm = TRUE)
      breaks  <- pretty(c(0, max_val), n = 5)
      breaks  <- breaks[breaks >= 0]
      
      g <- ggplot(plot_df,
                  aes(x = valor_total,
                      y = reorder(etiqueta, -valor_total))) +
        geom_col(fill = BAR_COLOR) +
        geom_text(
          aes(x = valor_total / 2,
              label = fmt_num(valor_total, accuracy = 0.1)),
          color = "white", size = 3
        ) +
        scale_x_continuous(
          labels = format_short,
          breaks = breaks,
          expand = expansion(mult = c(0, 0.05))
        ) +
        labs(x="Deforestación (ha)", y=NULL,
             title=paste0("Los 10 municipios que más pierden bosque",
                          if (!is.null(input$f_anio)) paste0(" en ", input$f_anio) else "")) +
        theme_minimal(base_size=12) +
        theme(
          axis.text.y=element_text(size=9),
          plot.margin=margin(r=30),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank()
        )
      ggsave(filename=file, plot=g, device=ragg::agg_png,
             width=10, height=6, dpi=200, units="in")
    }
  )
}

shinyApp(ui = ui, server = server)

