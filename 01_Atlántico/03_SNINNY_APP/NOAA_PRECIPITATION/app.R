# =========================================================
# Shiny App — NOAA Precipitación (Anual / Mensual)
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
APP_ROOT      <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/FAO_COLOMBIA/01_Atlántico/03_SNINNY_APP/"
NOAA_DIR      <- file.path(APP_ROOT, "NOAA_PRECIPITATION")
NOAA_DATA_DIR <- file.path(NOAA_DIR, "data")
SHP_DIR       <- file.path(APP_ROOT, "NOAA_PRECIPITATION/data/shp")

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

title_case_es <- function(x){
  stopw <- c("de","del","la","las","los","y","e","o","u","en","a","al","por","para",
             "con","sin","sobre","entre","hasta","desde","contra","ante","tras",
             "que","el","su","un","una","unos","unas")
  vapply(x, function(s){
    if (is.null(s) || is.na(s) || !nzchar(s)) return(s)
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

safe_chr  <- function(x) if (is.null(x)) "" else as.character(x)
find_shp  <- function(files, key){
  i <- grep(key, basename(files), ignore.case = TRUE)
  if (!length(i)) NA_character_ else files[i[1]]
}
nombre_mes <- function(m) c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                            "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")[as.integer(m)]

pick_first <- function(nms, candidates){
  cand <- candidates[candidates %in% nms]
  if (!length(cand)) NA_character_ else cand[1]
}

# ---------- Códigos DANE ----------
std_dpto <- function(x){
  ifelse(is.na(x), NA_character_, sprintf("%02d", as.integer(x)))
}

std_mpio <- function(dpto, mpio){
  d_na <- is.na(dpto) | is.na(mpio)
  d    <- sprintf("%02d", as.integer(dpto))
  m_int <- suppressWarnings(as.integer(mpio))
  m3    <- sprintf("%03d", m_int %% 1000)
  out   <- paste0(d, m3)
  out[d_na] <- NA_character_
  out
}

# ---------- Formato es-CO ----------
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

# ---------- Paleta y cuartiles ----------
pal4_vec <- grDevices::colorRampPalette(
  c("#e5f5f9","#99d8c9","#66c2a4","#2ca25f","#006d2c")
)(4)

make_bins4 <- function(values){
  v <- suppressWarnings(as.numeric(values))
  v <- v[is.finite(v)]
  if (!length(v)) return(seq(0,4))
  qs <- quantile(v, probs = seq(0,1,length.out=5), na.rm=TRUE, type=7)
  qs <- sort(unique(as.numeric(qs)))
  if (length(qs) < 5){
    r <- range(v, na.rm=TRUE)
    if (r[1]==r[2]) r <- c(0, max(1, r[2]))
    qs <- pretty(r, n=4)
  }
  if (length(qs) < 5) qs <- seq(min(qs), max(qs), length.out=5)
  qs
}

build_bins_labels <- function(values){
  v <- suppressWarnings(as.numeric(values))
  v <- v[is.finite(v)]
  if (!length(v)) v <- c(0, 1)
  
  bins <- make_bins4(v)
  pal  <- leaflet::colorBin(
    palette  = pal4_vec,
    bins     = bins,
    domain   = v,
    na.color = "#f0f0f0",
    right    = FALSE
  )
  
  labs <- vapply(
    seq_len(length(bins) - 1),
    function(i){
      a  <- bins[i]
      b  <- bins[i + 1]
      sa <- fmt_num(a, accuracy = 1)
      sb <- fmt_num(b, accuracy = 1)
      if (i == 1) {
        sprintf("%s – %s", sa, sb)
      } else {
        sprintf("> %s – %s", sa, sb)
      }
    },
    character(1)
  )
  mids <- (bins[-length(bins)] + bins[-1]) / 2
  cols <- pal(mids)
  
  list(
    bins   = bins,
    pal    = pal,
    labels = labs,
    colors = cols
  )
}

SERIES_CLR  <- "#006d2c"
RANKING_CLR <- "#006d2c"

# ---------- Shapefiles ----------
shp_files <- list.files(SHP_DIR, pattern="\\.shp$", full.names=TRUE, recursive=TRUE)
if (!length(shp_files)) stop("No encuentro .shp en: ", SHP_DIR)
ruta_shp_mpios <- find_shp(shp_files, "MPIO|MUN")
ruta_shp_dptos <- find_shp(shp_files, "DPTO|DEP|DEPT")
if (is.na(ruta_shp_mpios) || is.na(ruta_shp_dptos))
  stop("No pude detectar SHP de mpios/dptos en ", SHP_DIR)

mpios_sf_raw <- sf::st_read(ruta_shp_mpios, quiet = TRUE)
depto_sf_raw <- sf::st_read(ruta_shp_dptos, quiet = TRUE)

muni_name_cands       <- c("MUNICIPIO_D","MPIO_CNMBR","NOMBRE_MPIO","NOMBRE_MUNICIP",
                           "NOMBRE","MUNICIPIO")
depto_name_cands      <- c("DEPARTAMENTO_D","DPTO_CNMBR","NOMBRE_DPT","NOMBRE_DEPTO",
                           "DEPARTAMEN","DEPARTAMENTO")
depto_code_cands      <- c("DPTO_CCDGO","COD_DPTO","DPTO","CODIGO_DPTO","COD_DEPART","DPTO_COD")
muni_depto_code_cands <- c("DPTO_CCDGO","COD_DPTO","DPTO","CODIGO_DPTO","DPTO_COD")
muni_code_cands       <- c("MPIO_CCDGO","COD_MPIO","MUNICIPIO_COD",
                           "COD_MUN","COD_MPIO","MPIO_COD")

mpn <- names(mpios_sf_raw); dpn <- names(depto_sf_raw)
muni_name_col  <- pick_first(mpn, muni_name_cands)
muni_dpto_code <- pick_first(mpn, muni_depto_code_cands)
muni_code_col  <- pick_first(mpn, muni_code_cands)
depto_name_col <- pick_first(dpn, depto_name_cands)
depto_code_col <- pick_first(dpn, depto_code_cands)
stopifnot(!is.na(muni_name_col), !is.na(muni_dpto_code),
          !is.na(muni_code_col),
          !is.na(depto_name_col), !is.na(depto_code_col))

depto_sf <- depto_sf_raw |>
  dplyr::mutate(
    dpto_code      = std_dpto(.data[[depto_code_col]]),
    DEPARTAMENTO_D = up_es(.data[[depto_name_col]])
  ) |>
  sf::st_transform(4326) |>
  sf::st_make_valid() |>
  sf::st_zm(drop = TRUE, what = "ZM")

depto_key <- depto_sf |>
  sf::st_drop_geometry() |>
  dplyr::select(dpto_code, DEPARTAMENTO_D) |>
  dplyr::distinct()

mpios_sf <- mpios_sf_raw |>
  dplyr::mutate(
    dpto_code_raw = .data[[muni_dpto_code]],
    mpio_code_raw = .data[[muni_code_col]],
    dpto_code     = std_dpto(dpto_code_raw),
    mpio_code     = std_mpio(dpto_code_raw, mpio_code_raw),
    MUNICIPIO_D   = up_es(.data[[muni_name_col]])
  ) |>
  dplyr::left_join(depto_key, by = "dpto_code") |>
  sf::st_transform(4326) |>
  sf::st_make_valid() |>
  sf::st_zm(drop = TRUE, what = "ZM")

# ---------- Base NOAA ----------
base_raw <- readRDS(DATA_RDS)
base_raw <- base_raw %>% dplyr::filter(DEPARTAMENTO_D=="ATLÁNTICO")

required_cols <- c("fecha_completa","ano","mes","DEPARTAMENTO_D",
                   "MUNICIPIO_D","precip_mm",
                   "COD_DANE_DPTO_D","COD_DANE_MUNIC_D")
if (!all(required_cols %in% names(base_raw))) {
  faltan <- setdiff(required_cols, names(base_raw))
  stop("Faltan columnas en el RDS NOAA: ", paste(faltan, collapse=", "))
}

eva_df <- tibble::tibble(
  anio           = as.integer(base_raw[["ano"]]),
  mes            = as.integer(base_raw[["mes"]]),
  valor          = suppressWarnings(as.numeric(base_raw[["precip_mm"]])),
  dpto_code      = std_dpto(base_raw[["COD_DANE_DPTO_D"]]),
  mpio_code      = std_mpio(base_raw[["COD_DANE_DPTO_D"]],
                            base_raw[["COD_DANE_MUNIC_D"]]),
  DEPARTAMENTO_D = up_es(base_raw[["DEPARTAMENTO_D"]]),
  MUNICIPIO_D    = up_es(base_raw[["MUNICIPIO_D"]])
) |>
  dplyr::filter(!is.na(anio), !is.na(mes)) |>
  dplyr::mutate(
    mes   = pmax(pmin(mes, 12L), 1L),
    valor = dplyr::if_else(is.finite(valor), valor, NA_real_),
    valor = dplyr::if_else(!is.na(valor) & valor < 0, 0, valor)
  )

stopifnot(nrow(eva_df) > 0)

DEPS_LOOKUP <- eva_df |>
  dplyr::distinct(dpto_code, DEPARTAMENTO_D) |>
  dplyr::arrange(dpto_code)

# ---> Código del departamento Atlántico para usarlo como seleccionado por defecto
ATLANTICO_CODE <- DEPS_LOOKUP$dpto_code[DEPS_LOOKUP$DEPARTAMENTO_D == "ATLÁNTICO"]
if (length(ATLANTICO_CODE) == 0 || is.na(ATLANTICO_CODE)) ATLANTICO_CODE <- "Todos"

code_to_depto_name <- function(code){
  if (is.null(code) || is.na(code) || code == "Todos") return(NA_character_)
  nm <- DEPS_LOOKUP$DEPARTAMENTO_D[match(code, DEPS_LOOKUP$dpto_code)]
  ifelse(is.na(nm), NA_character_, nm)
}

DEPS_CHOICES <- c(
  "Todos" = "Todos",
  setNames(
    DEPS_LOOKUP$dpto_code,
    title_case_es(DEPS_LOOKUP$DEPARTAMENTO_D)
  )
)

MPIOS_UP_ALL <- sort(unique(na.omit(eva_df$MUNICIPIO_D)))
MPIOS_CHOICES_ALL <- c("Todos" = "Todos",
                       setNames(MPIOS_UP_ALL, title_case_es(MPIOS_UP_ALL)))

# ========================= UI =========================
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    primary = "#006d2c",
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius"  = "0.9rem",
    "font-size-base" = "0.95rem"
  ),
  tags$head(
    tags$style(HTML("
      :root{
        --accent-border:#ffb366;
        --gap:12px;
        --viz-row-1:380px;
        --viz-row-2:350px;
        --anom-h:380px;
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
        grid-template-columns:repeat(5,minmax(180px,1fr));
        gap:12px;
        align-items:stretch;
      }
      .filter{
        display:flex;
        flex-direction:column;
        justify-content:flex-start;
      }
      .filter-label{
        font-family:'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-size:14px;
        font-weight:500;
        letter-spacing:.2px;
        color:#111827;
        margin-bottom:6px;
      }
      .filters-grid .shiny-input-container{
        margin:0 !important;
        width:100%;
      }
      .filters-grid .selectize-control{
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
        box-shadow:0 0 0 .2rem rgba(255,179,102,.25) !important;
      }
      .card{
        background:#fff;
        border:1px solid var(--accent-border);
        border-radius:16px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
        padding:12px;
        margin-bottom:12px;
      }
      .anom-note{
        font-size:12px;
        color:#4b5563;
        margin-top:6px;
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
        grid-template-rows:var(--viz-row-1) var(--viz-row-2) var(--anom-h);
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
      .viz-anom{
        grid-column:1 / span 2;
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
        box-shadow:0 0 0 0.2rem rgba(255,179,102,0.25);
      }
      .selectize-input{
        border-color:var(--accent-border) !important;
      }
      .selectize-input.focus{
        border-color:var(--accent-border) !important;
        box-shadow:0 0 0 0.2rem rgba(255,179,102,0.25);
      }
      input[type='radio'],
      input[type='checkbox']{
        accent-color:#ffb366;
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
    h2("Explorador territorial — Precipitación (NOAA)", id = "app-title"),
    div(
      class = "data-note",
      HTML("Exploración de la precipitación acumulada (mm³) a nivel departamental y municipal, con información NOAA.")
    ),
    
    # ----------------- FILTROS -----------------
    div(
      class = "filters",
      div(
        class = "filters-grid",
        div(
          class="filter",
          div(class="filter-label","¿Qué frecuencia?"),
          radioButtons(
            "freq", NULL,
            choices  = c("Anual","Mensual"),
            selected = "Anual",
            inline   = TRUE
          )
        ),
        div(
          class="filter",
          div(class="filter-label","¿Qué año analizamos?"),
          uiOutput("anio_ui")
        ),
        div(
          class="filter",
          div(class="filter-label","¿Qué mes analizamos?"),
          conditionalPanel(
            "input.freq == 'Mensual'",
            selectInput(
              "f_mes", NULL,
              choices = setNames(
                1:12,
                c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                  "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
              ),
              selected = 1
            )
          )
        ),
        div(
          class="filter",
          div(class="filter-label","¿En qué departamento?"),
          selectInput(
            "f_depto", NULL,
            choices  = DEPS_CHOICES,
            selected = ATLANTICO_CODE   # <-- ahora selecciona Atlántico por código
          )
        ),
        div(
          class="filter",
          div(class="filter-label","¿Algún municipio en particular?"),
          selectInput(
            "f_mpio", NULL,
            choices  = MPIOS_CHOICES_ALL,
            selected = "Todos"
          )
        )
      )
    ),
    
    # ----------------- CONTENIDO -----------------
    div(
      class = "content-grid",
      
      # Mapa
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
        div(
          class="viz-body",
          leafletOutput("map_eva", height = "100%")
        ),
        div(
          class = "map-note",
          "Nota: los rangos de color del mapa se construyen con cuartiles (4 clases) del indicador de precipitación según el subconjunto de datos filtrado."
        ),
        div(
          class="dl-under",
          downloadButton("dl_png_mapa","PNG — Mapa (simple)")
        )
      ),
      
      # Serie temporal
      div(
        class = "card viz-card",
        div(class="card-title", textOutput("titulo_serie")),
        div(class="viz-body",
            plotlyOutput("plot_arriba", height = "100%")),
        div(class="dl-under",
            downloadButton("dl_png_series","PNG — Serie temporal"))
      ),
      
      # Ranking
      div(
        class = "card viz-card",
        div(class="card-title", textOutput("titulo_ranking")),
        div(class="viz-body",
            plotlyOutput("ranking_abajo", height = "100%")),
        div(class="dl-under",
            downloadButton("dl_png_ranking","PNG — Ranking Top-10"))
      ),
      
      # Anomalía
      div(
        class = "card viz-card viz-anom",
        div(
          class="card-title d-flex align-items-center",
          span(textOutput("titulo_anomalia"))
        ),
        div(
          style="display:flex; gap:10px; align-items:baseline; justify-content:space-between; margin-bottom:6px;",
          div(
            htmltools::tags$div(
              id="anom_head",
              htmltools::tags$span(
                textOutput("anom_resumen"),
                style="font-weight:600; font-size:14px;"
              )
            )
          ),
          div(
            class="dl-under",
            downloadButton("dl_png_anom","PNG — Anomalía (velas mm³)")
          )
        ),
        div(
          class="viz-body",
          plotlyOutput("anom_plot", height = "100%")
        ),
        div(
          class = "anom-note",
          textOutput("anom_detalle")
        )
      )
    ),
    
    # Descarga CSV
    div(
      class="dl-footer",
      downloadButton("dl_csv_expl","Descargar CSV (filtro actual)")
    )
  )
)



# ======================= SERVER =======================

server <- function(input, output, session){
  
  # -------- Estado del mapa --------
  nivel_mapa <- reactiveVal("depto")  # "depto" o "mpio"
  depto_sel  <- reactiveVal(NULL)
  
  output$nivel_txt <- renderText({
    if (nivel_mapa() == "depto") {
      "Nivel: Departamentos"
    } else {
      if (is.null(depto_sel()) || depto_sel() == "Todos") {
        "Nivel: Municipios — Filtro depto: Todos"
      } else {
        nm <- code_to_depto_name(depto_sel())
        paste0("Nivel: Municipios — Filtro depto: ",
               ifelse(is.na(nm),"(cód. desconocido)", title_case_es(nm)))
      }
    }
  })
  
  # -------- UI año --------
  output$anio_ui <- renderUI({
    yrs <- sort(unique(na.omit(as.integer(eva_df$anio))))
    selectInput("f_anio", NULL, choices = yrs, selected = max(yrs))
  })
  
  observeEvent(input$f_anio, ignoreInit = TRUE, {
    req(input$freq == "Mensual")
    meses <- eva_df |>
      dplyr::filter(anio == input$f_anio) |>
      dplyr::distinct(mes) |>
      dplyr::arrange(mes) |>
      dplyr::pull(mes)
    if (length(meses)) {
      updateSelectInput(
        session, "f_mes",
        choices  = setNames(meses, nombre_mes(meses)),
        selected = min(meses, na.rm = TRUE)
      )
    }
  })
  
  # -------- Municipios por depto --------
  observeEvent(input$f_depto, ignoreInit = TRUE, {
    munis_up <- if (is.null(input$f_depto) || input$f_depto=="Todos")
      sort(unique(na.omit(eva_df$MUNICIPIO_D))) else
        sort(unique(na.omit(
          eva_df$MUNICIPIO_D[eva_df$dpto_code == input$f_depto]
        )))
    
    if (length(munis_up)) {
      choices_mpio <- c("Todos" = "Todos",
                        setNames(munis_up, title_case_es(munis_up)))
    } else {
      choices_mpio <- c("Todos" = "Todos")
    }
    updateSelectInput(session, "f_mpio",
                      choices = choices_mpio,
                      selected = "Todos")
  })
  
  # -------- Datos filtrados base --------
  datos_filtrados <- reactive({
    df <- eva_df
    if (!is.null(input$f_depto) && input$f_depto!="Todos")
      df <- df |> dplyr::filter(dpto_code == input$f_depto)
    if (!is.null(input$f_mpio)  && input$f_mpio !="Todos")
      df <- df |> dplyr::filter(MUNICIPIO_D    == input$f_mpio)
    if (!is.null(input$f_anio))
      df <- df |> dplyr::filter(anio == input$f_anio)
    if (!is.null(input$freq) && input$freq == "Mensual" &&
        !is.null(input$f_mes)) {
      df <- df |> dplyr::filter(mes == as.integer(input$f_mes))
    }
    df |> dplyr::mutate(valor = suppressWarnings(as.numeric(valor)))
  })
  
  # -------- Títulos --------
  output$titulo_mapa    <- renderText({"¿Qué departamentos tienen el mayor volumen de lluvias?"})
  output$titulo_serie   <- renderText({"¿Cómo ha evolucionado el volumen de lluvias en el tiempo?"})
  output$titulo_ranking <- renderText({"Top 10 municipios con mayor volumen de lluvias"})
  
  # -------- Badge filtros --------
  badge_filtros <- reactive({
    if (input$freq == "Mensual") {
      htmltools::HTML(sprintf(
        '<div style="background:#fff;padding:6px 10px;border-radius:8px;
                    box-shadow:0 1px 6px rgba(0,0,0,.15);
                    font-size:12px;line-height:1.3;">
           <b>Indicador:</b> Precipitación (mm³)<br>
           <b>Período:</b> %s %s
         </div>',
        nombre_mes(input$f_mes), input$f_anio))
    } else {
      htmltools::HTML(sprintf(
        '<div style="background:#fff;padding:6px 10px;border-radius:8px;
                    box-shadow:0 1px 6px rgba(0,0,0,.15);
                    font-size:12px;line-height:1.3;">
           <b>Indicador:</b> Precipitación (mm³)<br>
           <b>Año:</b> %s
         </div>',
        input$f_anio))
    }
  })
  
  # -------- Agregados mapa --------
  agg_depto <- reactive({
    datos_filtrados() |>
      dplyr::group_by(dpto_code) |>
      dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
  })
  
  agg_mpio <- reactive({
    df <- datos_filtrados()
    if (!is.null(depto_sel()))
      df <- df |> dplyr::filter(dpto_code == depto_sel())
    df |>
      dplyr::group_by(dpto_code, mpio_code) |>
      dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
  })
  
  hover_label_opts       <- leaflet::labelOptions(
    direction="auto", textsize="12px", sticky=TRUE,
    opacity=0.95, className="lbl-clean")
  hover_label_opts_small <- leaflet::labelOptions(
    direction="auto", textsize="11px", sticky=TRUE,
    opacity=0.95, className="lbl-clean")
  
  # -------- Función para dibujar deptos --------
  render_deptos <- function(){
    # Subconjunto según filtros actuales
    df_vals <- datos_filtrados() |> 
      dplyr::group_by(dpto_code) |> 
      dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
    
    mdat <- depto_sf |>
      dplyr::left_join(df_vals, by = "dpto_code") |>
      dplyr::mutate(valor = dplyr::coalesce(valor, 0))
    
    # Calcular bins y paleta solo con ese subconjunto
    bl  <- build_bins_labels(mdat$valor)
    pal <- bl$pal
    
    leaflet::leafletProxy("map_eva", data = mdat) |>
      leaflet::clearPopups() |>
      leaflet::clearShapes() |>
      leaflet::clearMarkers() |>
      leaflet::clearControls() |>
      leaflet::addPolygons(
        layerId = ~dpto_code, fillColor = ~pal(valor),
        weight = 0.7, color = "#666", fillOpacity = 0.9,
        label = ~sprintf("%s — %s",
                         title_case_es(DEPARTAMENTO_D),
                         fmt_num(valor, accuracy = 1)),
        labelOptions = hover_label_opts,
        highlightOptions = leaflet::highlightOptions(
          color = "black", weight = 2, bringToFront = TRUE)
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        colors   = bl$colors,
        labels   = bl$labels,
        opacity  = 0.9,
        title    = "mm³"
      ) |>
      leaflet::addControl(
        badge_filtros(), position = "topright", layerId = "badge_filtros"
      )
  }
  
  # -------- Función para dibujar mpios --------
  render_mpios <- function(dep_code){
    # Subconjunto exacto de ese departamento
    df_vals <- datos_filtrados() |>
      dplyr::filter(dpto_code == dep_code) |>
      dplyr::group_by(mpio_code) |>
      dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
    
    mdat <- mpios_sf |>
      dplyr::filter(dpto_code == dep_code) |>
      dplyr::left_join(df_vals, by = "mpio_code") |>
      dplyr::mutate(valor = dplyr::coalesce(valor, 0))
    
    bl  <- build_bins_labels(mdat$valor)
    pal <- bl$pal
    
    leaflet::leafletProxy("map_eva", data = mdat) |>
      leaflet::clearPopups() |>
      leaflet::clearShapes() |>
      leaflet::clearMarkers() |>
      leaflet::clearControls() |>
      leaflet::addPolygons(
        layerId = ~mpio_code, fillColor = ~pal(valor),
        weight = 0.4, color = "#666", fillOpacity = 0.9,
        label = ~sprintf("%s (%s) — %s",
                         title_case_es(MUNICIPIO_D),
                         title_case_es(DEPARTAMENTO_D),
                         fmt_num(valor, accuracy = 1)),
        labelOptions = hover_label_opts_small,
        highlightOptions = leaflet::highlightOptions(
          color = "black", weight = 2, bringToFront = TRUE)
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        colors   = bl$colors,
        labels   = bl$labels,
        opacity  = 0.9,
        title    = "mm³"
      ) |>
      leaflet::addControl(
        badge_filtros(), position = "topright", layerId = "badge_filtros"
      )
  }
  
  # -------- Mapa inicial --------
  output$map_eva <- leaflet::renderLeaflet({
    mdat <- depto_sf |>
      dplyr::left_join(agg_depto(), by="dpto_code") |>
      dplyr::mutate(valor = dplyr::coalesce(valor, 0))
    
    bl  <- build_bins_labels(mdat$valor)
    pal <- bl$pal
    
    leaflet::leaflet(mdat) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addPolygons(
        layerId = ~dpto_code,
        fillColor = ~pal(valor), weight = 0.7,
        color = "#666", fillOpacity = 0.9,
        label = ~sprintf("%s — %s",
                         title_case_es(DEPARTAMENTO_D),
                         fmt_num(valor, accuracy = 1)),
        labelOptions = hover_label_opts,
        highlightOptions = leaflet::highlightOptions(
          color="black", weight=2, bringToFront=TRUE)
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        colors   = bl$colors,
        labels   = bl$labels,
        opacity  = 0.9,
        title    = "mm³"
      ) |>
      leaflet::addControl(
        badge_filtros(), position="topright", layerId="badge_filtros"
      )
  })
  
  # -------- Badge cuando cambian filtros --------
  observe({
    leaflet::leafletProxy("map_eva") |>
      leaflet::removeControl("badge_filtros") |>
      leaflet::addControl(badge_filtros(), position="topright",
                          layerId="badge_filtros")
  })
  
  # -------- Cambios por selección de departamento --------
  observeEvent(input$f_depto, {
    dep <- input$f_depto
    
    # Caso 1: sin filtro de departamento → mapa nacional
    if (is.null(dep) || dep == "Todos") {
      nivel_mapa("depto")
      depto_sel(NULL)
      render_deptos()
      return()
    }
    
    # Caso 2: con filtro de departamento (aunque municipio = Todos)
    nivel_mapa("mpio")
    depto_sel(dep)
    render_mpios(dep)
  }, ignoreInit = FALSE)  # <-- se ejecuta también al inicio
  
  
  # Click sobre el mapa (depto → mpios)
  observeEvent(input$map_eva_shape_click, {
    click <- input$map_eva_shape_click
    if (is.null(click$id)) return()
    if (nivel_mapa()=="depto") {
      depto_sel(click$id); nivel_mapa("mpio"); render_mpios(click$id)
    }
  })
  
  # Botón volver a departamentos
  observeEvent(input$btn_volver, {
    updateSelectInput(session, "f_depto", selected="Todos")
    updateSelectInput(session, "f_mpio",  selected="Todos")
    nivel_mapa("depto"); depto_sel(NULL); render_deptos()
  })
  
  # Redibujar mapa cuando cambian otros filtros
  observeEvent(
    list(input$freq, input$f_anio, input$f_mes, input$f_mpio),
    {
      if (nivel_mapa() == "depto") {
        render_deptos()
      } else {
        dep <- depto_sel()
        if (!is.null(dep)) render_mpios(dep)
      }
    },
    ignoreInit = TRUE
  )
  
  # ======================================================
  # SERIE TEMPORAL
  # ======================================================
  series_data <- reactive({
    base <- eva_df
    if (!is.null(input$f_depto) && input$f_depto!="Todos")
      base <- base |> dplyr::filter(dpto_code == input$f_depto)
    if (!is.null(input$f_mpio)  && input$f_mpio !="Todos")
      base <- base |> dplyr::filter(MUNICIPIO_D    == input$f_mpio)
    
    if (!is.null(input$freq) && input$freq == "Mensual") {
      req(input$f_anio)
      base <- base |> dplyr::filter(anio == input$f_anio)
      base |>
        dplyr::group_by(mes) |>
        dplyr::summarise(valor_total = sum(as.numeric(valor), na.rm = TRUE),
                         .groups="drop") |>
        dplyr::arrange(mes)
    } else {
      base |>
        dplyr::group_by(anio) |>
        dplyr::summarise(valor_total = sum(as.numeric(valor), na.rm = TRUE),
                         .groups="drop") |>
        dplyr::arrange(anio)
    }
  })
  
  output$plot_arriba <- plotly::renderPlotly({
    df <- series_data()
    if (!nrow(df)) return(plotly::plot_ly())
    
    max_val   <- max(df$valor_total, na.rm = TRUE)
    breaks_y  <- pretty(c(0, max_val), n = 5)
    breaks_y  <- breaks_y[breaks_y >= 0]
    tick_text <- format_short(breaks_y)
    
    if (!is.null(input$freq) && input$freq == "Mensual") {
      plotly::plot_ly(
        df,
        x=~mes,
        y=~valor_total,
        type="scatter", mode="lines+markers",
        line=list(width=2, color=SERIES_CLR),
        marker=list(size=6, color=SERIES_CLR),
        name="Serie",
        text = ~fmt_num(valor_total, accuracy = 1),
        hovertemplate="<b>Mes:</b> %{x}<br>mm³ %{text}<extra></extra>"
      ) |>
        plotly::layout(
          xaxis=list(
            title="",
            tickmode="array",
            tickvals=1:12,
            ticktext=c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                       "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"),
            showgrid=FALSE
          ),
          yaxis=list(
            title="mm³",
            tickvals = breaks_y,
            ticktext = tick_text,
            showgrid = TRUE,
            gridcolor = "#e5e7eb",
            gridwidth = 1
          ),
          hovermode="x unified",
          margin=list(l=60,r=20,t=20,b=40),
          legend=list(orientation="h")
        )
    } else {
      plotly::plot_ly(
        df,
        x=~anio,
        y=~valor_total,
        type="scatter", mode="lines+markers",
        line=list(width=2, color=SERIES_CLR),
        marker=list(size=6, color=SERIES_CLR),
        name="Serie",
        text = ~fmt_num(valor_total, accuracy = 1),
        hovertemplate="<b>Año:</b> %{x}<br>mm³ %{text}<extra></extra>"
      ) |>
        plotly::layout(
          xaxis=list(
            title="",
            tickmode="linear", dtick=1,
            showgrid=FALSE
          ),
          yaxis=list(
            title="mm³",
            tickvals = breaks_y,
            ticktext = tick_text,
            showgrid = TRUE,
            gridcolor = "#e5e7eb",
            gridwidth = 1
          ),
          hovermode="x unified",
          margin=list(l=60,r=20,t=20,b=40),
          legend=list(orientation="h")
        )
    }
  })
  
  # ======================================================
  # RANKING
  # ======================================================
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
      return(
        plotly::plot_ly() |>
          plotly::layout(annotations = list(
            text="Sin datos para el ranking",
            x=0.5, y=0.5, showarrow=FALSE))
      )
    }
    plot_df <- plot_df |>
      dplyr::mutate(
        muni_tc  = title_case_es(MUNICIPIO_D),
        depto_tc = title_case_es(DEPARTAMENTO_D)
      )
    
    max_val   <- max(plot_df$valor_total, na.rm = TRUE)
    breaks    <- pretty(c(0, max_val), n = 5)
    breaks    <- breaks[breaks >= 0]
    tick_text <- format_short(breaks)
    
    plotly::plot_ly(
      data = plot_df,
      x = ~valor_total,
      y = ~muni_tc,
      type = "bar",
      orientation = "h",
      marker = list(color = RANKING_CLR),
      text = ~fmt_num(valor_total, accuracy = 1),
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
        "<br><b>mm³:</b> %{customdata[2]}",
        "<extra></extra>"
      ),
      cliponaxis = FALSE
    ) |>
      plotly::layout(
        xaxis = list(
          title = "mm³",
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
        margin = list(l = 140, r = 40, t = 10, b = 40)
      )
  })
  
  # ======================================================
  # ANOMALÍA (velas)
  # ======================================================
  mensual_full_by_geo <- reactive({
    df <- eva_df
    if (!is.null(input$f_depto) && input$f_depto!="Todos")
      df <- df |> dplyr::filter(dpto_code == input$f_depto)
    if (!is.null(input$f_mpio)  && input$f_mpio !="Todos")
      df <- df |> dplyr::filter(MUNICIPIO_D    == input$f_mpio)
    
    df |>
      dplyr::group_by(anio, mes) |>
      dplyr::summarise(
        valor_mensual = sum(as.numeric(valor), na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(fecha = as.Date(sprintf("%d-%02d-15", anio, mes)))
  })
  
  anom_mm_candles_full <- reactive({
    m <- mensual_full_by_geo()
    if (!nrow(m)) return(NULL)
    
    yrs <- sort(unique(m$anio))
    if (length(yrs) < 5) return(NULL)
    base_years <- yrs[1:5]
    
    clim_base <- m |>
      dplyr::filter(anio %in% base_years) |>
      dplyr::group_by(mes) |>
      dplyr::summarise(mu = mean(valor_mensual, na.rm = TRUE),
                       .groups="drop")
    
    s <- m |>
      dplyr::inner_join(clim_base, by = "mes") |>
      dplyr::arrange(fecha) |>
      dplyr::mutate(anom = valor_mensual - mu)
    
    k  <- 5
    mm <- zoo::rollmean(s$anom, k = k, fill = NA, align = "right")
    
    n <- length(mm)
    if (n < k + 1) return(NULL)
    idx   <- (k+0):n
    open  <- mm[pmax(idx-1, 1)]
    close <- mm[idx]
    highs <- lows <- rep(NA_real_, length(idx))
    for (i in seq_along(idx)) {
      win      <- (idx[i]-k+1):idx[i]
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
    "Anomalía de precipitación (velas de mm³)"
  })
  
  output$anom_resumen <- renderText({
    df <- anom_mm_candles_full()
    if (is.null(df) || !nrow(df)) return("Sin datos suficientes")
    ult   <- tail(df, 1)
    signo <- ifelse(ult$C - ult$O >= 0, "↑", "↓")
    paste0(
      "Vela más reciente: ", signo,
      " cierre = ", fmt_num(ult$C, accuracy = 0.1), " mm³",
      " (apertura = ", fmt_num(ult$O, accuracy = 0.1),
      ", máximo = ", fmt_num(ult$H, accuracy = 0.1),
      ", mínimo = ", fmt_num(ult$L, accuracy = 0.1), "). Base: 5 primeros años."
    )
  })
  
  output$anom_detalle <- renderText({
    "Base climatológica = promedio mensual de los primeros 5 años del subconjunto geográfico. Anomalía = observado − climatología mensual (base)."
  })
  
  output$anom_plot <- plotly::renderPlotly({
    df <- anom_mm_candles_full()
    if (is.null(df) || !nrow(df)) return(plotly::plot_ly())
    
    rng_y       <- range(c(df$O, df$H, df$L, df$C), na.rm = TRUE)
    breaks_y    <- pretty(rng_y, n = 6)
    breaks_y    <- breaks_y[is.finite(breaks_y)]
    tick_text_y <- format_short(breaks_y)
    
    idx_x       <- seq(1, nrow(df), by = 9)
    tickvals_x  <- df$x[idx_x]
    ticktext_x  <- paste0(
      nombre_mes(lubridate::month(df$x[idx_x])),"<br>",
      lubridate::year(df$x[idx_x])
    )
    
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
        xaxis = list(
          title    = "",
          showgrid = FALSE,
          tickvals = tickvals_x,
          ticktext = ticktext_x
        ),
        yaxis = list(
          title    = "mm³ de anomalía",
          tickvals = breaks_y,
          ticktext = tick_text_y,
          zeroline = TRUE, zerolinecolor = "#555555",
          showgrid = FALSE
        ),
        margin = list(l = 60, r = 30, t = 50, b = 60),
        showlegend = FALSE
      )
  })
  
  # ======================================================
  # DESCARGAS
  # ======================================================
  tabla_export <- reactive({
    datos_filtrados() |>
      dplyr::transmute(
        COD_DPTO      = dpto_code,
        COD_MPIO      = mpio_code,
        DEPARTAMENTO  = title_case_es(DEPARTAMENTO_D),
        MUNICIPIO     = title_case_es(MUNICIPIO_D),
        anio, mes,
        precipitacion_mm = valor
      )
  })
  
  output$dl_csv_expl <- downloadHandler(
    filename = function() {
      if (!is.null(input$freq) && input$freq == "Mensual")
        paste0("NOAA_precipitacion_", input$f_anio, "_",
               sprintf("%02d", as.integer(input$f_mes)), "_",
               Sys.Date(), ".csv")
      else
        paste0("NOAA_precipitacion_", safe_chr(input$f_anio),
               "_", Sys.Date(), ".csv")
    },
    content  = function(file) readr::write_csv(tabla_export(), file, na = "")
  )
  
  output$dl_png_series <- downloadHandler(
    filename = function() {
      if (!is.null(input$freq) && input$freq == "Mensual")
        paste0("NOAA_serie_", input$f_anio, "_",
               sprintf("%02d", as.integer(input$f_mes)), "_",
               Sys.Date(), ".png")
      else
        paste0("NOAA_serie_", Sys.Date(), ".png")
    },
    content  = function(file){
      df <- series_data()
      if (!nrow(df)) { file.create(file); return() }
      
      max_val   <- max(df$valor_total, na.rm = TRUE)
      breaks_y  <- pretty(c(0, max_val), n = 5)
      breaks_y  <- breaks_y[breaks_y >= 0]
      
      if (!is.null(input$freq) && input$freq == "Mensual") {
        g <- ggplot(df, aes(x=mes, y=valor_total)) +
          geom_line(linewidth=0.9, color=SERIES_CLR) +
          geom_point(size=2.2, color=SERIES_CLR) +
          scale_x_continuous(breaks=1:12, labels=month.abb) +
          scale_y_continuous(
            labels = format_short,
            breaks = breaks_y
          ) +
          labs(x="Mes", y="Precipitación (mm)",
               title=paste0("Evolución mensual (", input$f_anio, ")")) +
          theme_minimal(base_size=12) +
          theme(
            panel.grid.minor   = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#e5e7eb")
          )
      } else {
        g <- ggplot(df, aes(x=anio, y=valor_total)) +
          geom_line(linewidth=0.9, color=SERIES_CLR) +
          geom_point(size=2.2, color=SERIES_CLR) +
          scale_x_continuous(breaks=unique(df$anio)) +
          scale_y_continuous(
            labels = format_short,
            breaks = breaks_y
          ) +
          labs(x=NULL, y="Precipitación (mm)",
               title="Evolución anual de la precipitación (mm)") +
          theme_minimal(base_size=12) +
          theme(
            panel.grid.minor   = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#e5e7eb")
          )
      }
      ggsave(filename=file, plot=g, device=ragg::agg_png,
             width=10, height=5, dpi=200, units="in")
    }
  )
  
  map_widget_simple <- reactive({
    if (nivel_mapa()=="depto"){
      mdat <- depto_sf |>
        dplyr::left_join(agg_depto(), by="dpto_code") |>
        dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
      bl  <- build_bins_labels(mdat$valor)
      pal <- bl$pal
      leaflet::leaflet(mdat,
                       options=leaflet::leafletOptions(zoomControl=FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          fillColor=~pal(valor), weight=0.5,
          color="#666", fillOpacity=0.9
        ) |>
        leaflet::addControl(
          html = htmltools::HTML(
            sprintf("<div style='font-weight:600;font-size:14px;
                     background:#fff;padding:6px 8px;
                     border-radius:8px;border:1px solid #e6e6e6'>
                      Precipitación (mm³) por departamento — %s%s
                     </div>",
                    safe_chr(input$f_anio),
                    if (!is.null(input$freq) && input$freq=='Mensual')
                      paste0(" (", nombre_mes(input$f_mes), ")")
                    else "")),
          position="topleft")
    } else {
      dep    <- depto_sel()
      dep_nm <- code_to_depto_name(dep)
      mdat <- mpios_sf |>
        dplyr::filter(dpto_code==dep) |>
        dplyr::left_join(
          agg_mpio(),
          by=c("dpto_code","mpio_code")
        ) |>
        dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
      bl  <- build_bins_labels(mdat$valor)
      pal <- bl$pal
      leaflet::leaflet(mdat,
                       options=leaflet::leafletOptions(zoomControl=FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          fillColor=~pal(valor), weight=0.4,
          color="#666", fillOpacity=0.9
        ) |>
        leaflet::addControl(
          html = htmltools::HTML(
            sprintf("<div style='font-weight:600;font-size:14px;
                     background:#fff;padding:6px 8px;
                     border-radius:8px;border:1px solid #e6e6e6'>
                      Precipitación (mm³) por municipios — %s%s
                     </div>",
                    ifelse(is.na(dep_nm),"Depto", title_case_es(dep_nm)),
                    if (!is.null(input$freq) && input$freq=='Mensual')
                      paste0(" (", nombre_mes(input$f_mes), " ", safe_chr(input$f_anio), ")")
                    else paste0(" (", safe_chr(input$f_anio), ")"))),
          position="topleft")
    }
  })
  
  output$dl_png_mapa <- downloadHandler(
    filename = function() {
      suf <- if (!is.null(input$freq) && input$freq=="Mensual")
        paste0("_", input$f_anio, "_",
               sprintf("%02d", as.integer(input$f_mes)))
      else ""
      paste0("NOAA_mapa", suf, "_", Sys.Date(), ".png")
    },
    content  = function(file){
      widget   <- map_widget_simple()
      tmp_html <- tempfile(fileext=".html")
      htmlwidgets::saveWidget(widget, tmp_html, selfcontained=TRUE)
      webshot2::webshot(tmp_html, file=file,
                        vwidth=1200, vheight=800, zoom=2)
    }
  )
  
  output$dl_png_ranking <- downloadHandler(
    filename = function() {
      suf <- if (!is.null(input$freq) && input$freq=="Mensual")
        paste0("_", input$f_anio, "_",
               sprintf("%02d", as.integer(input$f_mes)))
      else
        paste0("_", safe_chr(input$f_anio))
      paste0("NOAA_ranking", suf, "_", Sys.Date(), ".png")
    },
    content  = function(file){
      plot_df <- ranking_data() |>
        dplyr::mutate(etiqueta = title_case_es(MUNICIPIO_D))
      if (!nrow(plot_df)) { file.create(file); return() }
      max_val <- max(plot_df$valor_total, na.rm = TRUE)
      breaks  <- pretty(c(0, max_val), n = 5)
      breaks  <- breaks[breaks >= 0]
      g <- ggplot(plot_df,
                  aes(x = valor_total,
                      y = reorder(etiqueta, -valor_total))) +
        geom_col(fill = RANKING_CLR) +
        geom_text(
          aes(x = valor_total/2,
              label = fmt_num(valor_total, accuracy = 0.1)),
          color = "white",
          size  = 3
        ) +
        scale_x_continuous(
          labels = format_short,
          breaks = breaks,
          expand = expansion(mult = c(0, 0.05))
        ) +
        labs(x="Precipitación (mm)", y=NULL) +
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
  
  output$dl_png_anom <- downloadHandler(
    filename = function()
      paste0("NOAA_anomalia_velasMM_full_", Sys.Date(), ".png"),
    content  = function(file){
      df <- anom_mm_candles_full()
      if (is.null(df) || !nrow(df)) { file.create(file); return() }
      p <- plotly::plot_ly(
        type  = "candlestick",
        x     = df$x, open = df$O, high = df$H, low = df$L, close = df$C,
        increasing = list(line = list(color = "#2ca02c")),
        decreasing = list(line = list(color = "#d62728"))
      ) |>
        plotly::layout(
          xaxis = list(title = "Fecha", showgrid=FALSE),
          yaxis = list(
            title = "MM de anomalía (mm)",
            zeroline = TRUE, zerolinecolor = "#999",
            showgrid=FALSE
          ),
          margin = list(l=60, r=30, t=20, b=50),
          showlegend = FALSE
        )
      tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(p, tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, file = file,
                        vwidth = 1100, vheight = 520, zoom = 2)
    }
  )
}


shinyApp(ui = ui, server = server)
