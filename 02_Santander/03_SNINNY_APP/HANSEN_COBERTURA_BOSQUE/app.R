# =========================================================
# Shiny App — COBERTURA NETA DE BOSQUE (Hansen) — SANTANDER
# (MODIFICADO)
# - Filtra la base a SANTANDER (robusto con toupper/norm_txt)
# - Serie temporal: eje X cada 2 años y etiquetas horizontales
# - Ranking (plotly): etiquetas/texto concuerdan con las barras
# =========================================================

# 1) Paquetes
pkgs <- c(
  "shiny","bslib","dplyr","readr","stringi","sf","leaflet",
  "plotly","ggplot2","htmltools","webshot2","htmlwidgets",
  "ragg","glue","scales","tidyr"
)
suppressWarnings(invisible(sapply(pkgs, require, character.only = TRUE)))
options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

# 2) Rutas (usando el directorio actual del script)
if (interactive()) {
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    APP_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
  } else {
    APP_DIR <- getwd()
  }
} else {
  APP_DIR <- getwd()
}
if (is.null(APP_DIR) || APP_DIR == "") APP_DIR <- getwd()

DATA_RDS <- file.path(APP_DIR, "data/141_HANSEN_COBERTURA_NETA_TOTAL.rds")
SHP_DIR  <- file.path(APP_DIR, "data/shp")

if (!file.exists(DATA_RDS)) stop("No encuentro la base RDS en: ", DATA_RDS)
shp_files <- list.files(SHP_DIR, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
if (length(shp_files) == 0) stop("No encuentro archivos .shp en: ", SHP_DIR)

# 3) Helpers
norm_txt   <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
pick_first <- function(nms, candidates){
  cand <- candidates[candidates %in% nms]
  if (!length(cand)) NA_character_ else cand[1]
}
find_shp   <- function(files, key){
  i <- grep(key, basename(files), ignore.case = TRUE)
  if (!length(i)) NA_character_ else files[i[1]]
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

format_num_es <- function(x, digits = 1){
  scales::number(x, accuracy = 10^-digits, big.mark=".", decimal.mark=",")
}
format_int_es <- function(x) format_num_es(x, digits = 0)
format_pct_es <- function(x, digits = 1) paste0(format_num_es(x, digits = digits), "%")

format_short <- function(x){
  ifelse(
    is.na(x), NA_character_,
    ifelse(
      abs(x) >= 1e6, paste0(format_num_es(x / 1e6, digits = 1), "M"),
      ifelse(abs(x) >= 1e3, paste0(format_num_es(x / 1e3, digits = 1), "K"),
             format_int_es(x))
    )
  )
}

distinct_pairs <- function(df, key_col, disp_col){
  stopifnot(key_col %in% names(df), disp_col %in% names(df))
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

labelFormat_es <- function(suffix = "", digits = 1,
                           big.mark=".", decimal.mark=",",
                           between=" – ", first_no_symbol=TRUE) {
  force(suffix); force(digits); force(big.mark); force(decimal.mark)
  force(between); force(first_no_symbol)
  function(type = "numeric", cuts, p = NULL) {
    if (type %in% c("numeric","bin")) {
      n <- length(cuts)
      if (n <= 1) return(character(0))
      left  <- cuts[-n]
      right <- cuts[-1]
      fmt <- function(x) format(round(x, digits), big.mark=big.mark, decimal.mark=decimal.mark)
      left_chr  <- fmt(left)
      right_chr <- fmt(right)
      labs <- character(n - 1)
      for (i in seq_len(n - 1)) {
        if (i == 1 && first_no_symbol) {
          labs[i] <- paste0(left_chr[i], between, right_chr[i], suffix)
        } else if (i == n - 1) {
          labs[i] <- paste0("> ", left_chr[i], suffix)
        } else {
          labs[i] <- paste0("> ", left_chr[i], between, right_chr[i], suffix)
        }
      }
      return(labs)
    }
    vals_chr <- format(round(cuts, digits), big.mark=big.mark, decimal.mark=decimal.mark)
    paste0(vals_chr, suffix)
  }
}

# ================== Colores ==================
SERIE_COLOR <- "#2E7D32"
BAR_COLOR   <- "#2E7D32"

pal4_vec <- grDevices::colorRampPalette(
  c("#E8F5E9", "#81C784", "#43A047", "#1B5E20")
)(4)

make_bins4 <- function(values, clamp01 = FALSE){
  v <- as.numeric(values); v <- v[is.finite(v)]
  if (clamp01) v <- pmax(pmin(v, 100), 0)
  if (!length(v)) return(seq(0, 4))
  qs <- quantile(v, probs=c(0,0.25,0.5,0.75,1), na.rm=TRUE, type=7)
  qs <- sort(unique(as.numeric(qs)))
  if (length(qs) < 5){
    r <- range(v, na.rm=TRUE)
    if (r[1] == r[2]) r <- c(0, max(1, r[2]))
    qs <- pretty(r, n=4)
  }
  if (length(qs) < 5) qs <- seq(min(qs), max(qs), length.out=5)
  qs
}
palBin4 <- function(values, clamp01 = FALSE){
  bins <- make_bins4(values, clamp01 = clamp01)
  pal  <- leaflet::colorBin(
    palette=pal4_vec, bins=bins, domain=values,
    na.color="#f0f0f0", right=FALSE
  )
  attr(pal, "bins") <- bins
  pal
}

# =========================================================
# 4) Shapefiles
# =========================================================
ruta_shp_mpios <- find_shp(shp_files, "MPIO|MUN")
ruta_shp_dptos <- find_shp(shp_files, "DPTO|DEP|DEPT")
if (is.na(ruta_shp_mpios) || is.na(ruta_shp_dptos))
  stop("No pude detectar SHP de mpios/dptos en ", SHP_DIR)

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
stopifnot(!is.na(muni_name_col), !is.na(muni_dpto_code),
          !is.na(depto_name_col), !is.na(depto_code_col))

depto_key <- depto_sf_raw |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    dpto_code        = .data[[depto_code_col]],
    DEPARTAMENTO_RAW = as.character(.data[[depto_name_col]]),
    DEPARTAMENTO_D   = toupper(norm_txt(.data[[depto_name_col]]))
  )

mpios_sf <- mpios_sf_raw |>
  dplyr::mutate(
    MUNICIPIO_RAW = as.character(.data[[muni_name_col]]),
    MUNICIPIO_D   = norm_txt(.data[[muni_name_col]]),
    dpto_code     = .data[[muni_dpto_code]]
  ) |>
  dplyr::left_join(depto_key, by = "dpto_code") |>
  sf::st_transform(4326) |>
  sf::st_make_valid() |>
  sf::st_zm(drop = TRUE, what = "ZM") |>
  dplyr::mutate(
    MPIO_TC  = title_case_es(MUNICIPIO_RAW),
    DEPTO_TC = title_case_es(DEPARTAMENTO_RAW)
  )

depto_sf <- depto_sf_raw |>
  sf::st_transform(4326) |>
  sf::st_make_valid() |>
  sf::st_zm(drop = TRUE, what = "ZM") |>
  dplyr::mutate(
    DEPARTAMENTO_RAW = as.character(.data[[depto_name_col]]),
    DEPARTAMENTO_D   = toupper(norm_txt(DEPARTAMENTO_RAW)),
    DEPTO_TC         = title_case_es(DEPARTAMENTO_RAW)
  )

# =========================================================
# 5) Base COBERTURA NETA — SANTANDER
# =========================================================
base_raw <- readRDS(DATA_RDS)

# >>> CAMBIO CLAVE: filtrar a SANTANDER (robusto)
base_raw <- base_raw |>
  dplyr::filter(toupper(norm_txt(DEPARTAMENTO_D)) == "SANTANDER")

eva_df <- base_raw |>
  dplyr::mutate(
    anio             = as.integer(.data[["ano"]]),
    cobertura_ha     = suppressWarnings(as.numeric(.data[["cobertura_neta_ha"]])),
    cobertura_pct    = suppressWarnings(as.numeric(.data[["cobertura_neta_pct"]])),
    base_ha_2000     = suppressWarnings(as.numeric(.data[["base_ha_2000"]])),
    MUNICIPIO_RAW    = as.character(.data[["MUNICIPIO_D"]]),
    DEPARTAMENTO_RAW = as.character(.data[["DEPARTAMENTO_D"]]),
    MUNICIPIO_D      = norm_txt(MUNICIPIO_RAW),
    DEPARTAMENTO_D   = toupper(norm_txt(DEPARTAMENTO_RAW))
  ) |>
  dplyr::mutate(
    DEPTO_KEY  = DEPARTAMENTO_D,
    MPIO_KEY   = MUNICIPIO_D,
    DEPTO_DISP = DEPARTAMENTO_RAW,
    MPIO_DISP  = MUNICIPIO_RAW,
    DEPTO_TC   = title_case_es(DEPARTAMENTO_RAW),
    MPIO_TC    = title_case_es(MUNICIPIO_RAW)
  )

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    primary = "#2E7D32",
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius"  = "0.9rem",
    "font-size-base" = "0.95rem"
  ),
  tags$head(
    tags$style(HTML("
      :root{
        --accent-border:#2E7D32;
        --gap:12px;
        --viz-row-top:410px;
        --viz-row-bottom:410px;
      }
      .wrap{ max-width:1360px; margin:0 auto; padding:16px 20px 32px; }
      h2#app-title{ font-weight:700; letter-spacing:.2px; margin-top:4px; margin-bottom:6px; text-align:left; }
      .data-note{ font-size:13px; color:#6b7280; margin:0 0 16px; }
      .filters{
        background:#fff; border:1px solid var(--accent-border); border-radius:16px;
        box-shadow:0 2px 10px rgba(0,0,0,.05); padding:6px 12px 8px; margin-bottom:12px;
      }
      .filters-grid{
        display:grid; grid-template-columns:repeat(4,minmax(200px,1fr));
        gap:12px; align-items:stretch;
      }
      .filter{ display:flex; flex-direction:column; justify-content:flex-start; }
      .filter-label{ font-family:'Inter', system-ui; font-size:14px; font-weight:500; letter-spacing:.2px; color:#111827; margin-bottom:6px; }
      .filters-grid .shiny-input-container{ margin:0 !important; }
      .filters-grid .selectize-input,
      .filters-grid .form-control,
      .filters-grid .form-select{
        height:60px !important; min-height:60px; padding-top:10px; padding-bottom:10px;
        border-radius:10px; border:1px solid var(--accent-border) !important;
      }
      .filters-grid .selectize-input:focus,
      .filters-grid .form-control:focus,
      .filters-grid .form-select:focus{
        border-color:var(--accent-border) !important;
        box-shadow:0 0 0 .2rem rgba(46,125,50,.25) !important;
      }
      .card{
        background:#fff; border:1px solid var(--accent-border); border-radius:16px;
        box-shadow:0 2px 10px rgba(0,0,0,.05); padding:12px; margin-bottom:12px;
      }
      .card-title{ font-weight:700; font-size:16px; margin-bottom:8px; color:#111827; }
      .content-grid{
        display:grid; grid-template-columns:1.05fr 1fr;
        grid-template-rows:var(--viz-row-top) var(--viz-row-bottom);
        gap:var(--gap);
      }
      .viz-card{ display:flex; flex-direction:column; height:100%; margin:0; }
      .viz-body{ flex:1 1 auto; min-height:0; }
      .viz-map{ grid-row:1 / span 2; }
      .viz-body .leaflet, .viz-body .plotly.html-widget{ height:100% !important; }
      .leaflet-tooltip.lbl-clean{
        background: rgba(255,255,255,.92); border: 1px solid #e6e6e6; border-radius: 6px;
        padding: 4px 6px; color: #222; font-weight: 600; box-shadow: 0 1px 4px rgba(0,0,0,.08);
      }
      .leaflet-control, .leaflet-control .legend, .leaflet-control .info{ border-radius:12px; }
      .leaflet-top .leaflet-control { margin-top: 6px; }
      .leaflet-left .leaflet-control { margin-left: 6px; }
      .btn, .btn-default{
        font-size:12px; padding:6px 10px; border-radius:8px;
        border-color:var(--accent-border) !important;
      }
      .dl-under{ margin-top:8px; text-align:right; }
      .dl-footer{ margin-top:10px; text-align:right; }
      .map-note{ margin-top:6px; font-size:12px; color:#4b5563; }
    "))
  ),
  div(
    class = "wrap",
    h2("", id = "app-title"),
    div(class = "data-note", HTML("")),
    div(
      class = "filters",
      div(
        class = "filters-grid",
        div(class="filter", div(class="filter-label","¿Qué año analizamos?"), uiOutput("anio_ui")),
        div(
          class="filter",
          div(class="filter-label","¿Qué indicador quieres ver?"),
          selectInput("f_metric", NULL,
                      choices = c("Cobertura neta (Ha)"="ha", "Cobertura neta (%)"="pct"),
                      selected = "pct")
        ),
        div(
          class="filter",
          div(class="filter-label","¿En que departamento?"),
          {
            dep_pairs <- distinct_pairs(eva_df, "DEPTO_KEY", "DEPTO_DISP")
            # Como la base ya está filtrada a Santander, esto quedará en Santander
            idx_santander <- which(toupper(norm_txt(dep_pairs$DEPTO_DISP)) == "SANTANDER")
            default_dep <- if (length(idx_santander)) dep_pairs$DEPTO_KEY[idx_santander[1]] else "Todos"
            selectInput("f_depto", NULL,
                        choices  = mk_tc_from_pairs(dep_pairs$DEPTO_KEY, dep_pairs$DEPTO_DISP),
                        selected = default_dep)
          }
        ),
        div(
          class="filter",
          div(class="filter-label","¿Algún municipio en particular?"),
          {
            mpio_pairs <- distinct_pairs(eva_df, "MPIO_KEY", "MPIO_DISP")
            selectInput("f_mpio", NULL,
                        choices  = mk_tc_from_pairs(mpio_pairs$MPIO_KEY, mpio_pairs$MPIO_DISP),
                        selected = "Todos")
          }
        )
      )
    ),
    div(
      class = "content-grid",
      div(
        class = "card viz-card viz-map",
        div(class="card-title d-flex align-items-center", span(textOutput("titulo_mapa"))),
        div(
          style="display:flex; gap:10px; align-items:center; margin-bottom:8px;",
          actionButton("btn_volver", "◀ Volver al panorama nacional", class="btn btn-light"),
          strong(textOutput("nivel_txt", inline = TRUE))
        ),
        div(class="viz-body", leafletOutput("map_eva", height = "100%")),
        div(class = "map-note",
            HTML("Nota: Los colores del mapa se segmentan automáticamente en cuartiles (4 grupos de igual tamaño) del indicador seleccionado.")),
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
  depto_sel  <- reactiveVal(NULL)
  
  output$nivel_txt <- renderText({
    if (nivel_mapa() == "depto") "Nivel: Departamentos"
    else paste0("Nivel: Municipios — ", title_case_es(depto_sel()))
  })
  
  output$anio_ui <- renderUI({
    yrs <- sort(unique(na.omit(as.integer(eva_df$anio))))
    selectInput("f_anio", NULL, choices = yrs, selected = max(yrs))
  })
  
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
  
  datos_filtrados <- reactive({
    req(input$f_anio)
    df <- eva_df |> dplyr::filter(anio == input$f_anio)
    if (!is.null(input$f_depto) && input$f_depto!="Todos")
      df <- df |> dplyr::filter(DEPTO_KEY == input$f_depto)
    if (!is.null(input$f_mpio)  && input$f_mpio !="Todos")
      df <- df |> dplyr::filter(MPIO_KEY  == input$f_mpio)
    df
  })
  
  output$titulo_mapa <- renderText({
    if (input$f_metric == "ha")
      "¿En cuáles territorios está la mayor cantidad de hectáreas en cobertura neta boscosa?"
    else
      "¿En cuáles territorios está el mayor porcentaje de cobertura neta boscosa?"
  })
  output$titulo_serie <- renderText({
    if (input$f_metric == "ha")
      "¿Cómo ha evolucionado la cantidad de cobertura de bosque en el tiempo?"
    else
      "¿Cómo ha evolucionado el porcentaje de cobertura de bosque en el tiempo?"
  })
  output$titulo_ranking <- renderText({
    if (input$f_metric == "ha")
      "Top 10 municipios con mayor cantidad de hectáreas en cobertura neta de bosque"
    else
      "Top 10 municipios con mayor porcentaje de cobertura neta de bosque"
  })
  
  agg_depto <- reactive({
    df <- datos_filtrados()
    if (input$f_metric == "ha") {
      df |>
        dplyr::group_by(DEPARTAMENTO_D) |>
        dplyr::summarise(valor = sum(cobertura_ha, na.rm = TRUE), .groups="drop")
    } else {
      df |>
        dplyr::group_by(DEPARTAMENTO_D) |>
        dplyr::summarise(
          base_sum = sum(base_ha_2000, na.rm=TRUE),
          cob_sum  = sum(cobertura_ha,  na.rm=TRUE),
          valor    = dplyr::if_else(base_sum > 0, 100*cob_sum/base_sum, NA_real_),
          .groups="drop"
        ) |>
        dplyr::select(DEPARTAMENTO_D, valor)
    }
  })
  
  agg_mpio <- reactive({
    req(input$f_anio)
    df <- eva_df |> dplyr::filter(anio == input$f_anio)
    if (!is.null(depto_sel())) df <- df |> dplyr::filter(DEPARTAMENTO_D == depto_sel())
    
    if (input$f_metric == "ha") {
      df |>
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D, MUNICIPIO_RAW, DEPARTAMENTO_RAW) |>
        dplyr::summarise(valor = sum(cobertura_ha, na.rm=TRUE), .groups="drop")
    } else {
      df |>
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D, MUNICIPIO_RAW, DEPARTAMENTO_RAW) |>
        dplyr::summarise(
          base_sum = sum(base_ha_2000, na.rm=TRUE),
          cob_sum  = sum(cobertura_ha,  na.rm=TRUE),
          valor    = dplyr::if_else(base_sum > 0, 100*cob_sum/base_sum, NA_real_),
          .groups="drop"
        )
    }
  })
  
  badge_filtros <- reactive({
    yr  <- safe_chr(input$f_anio)
    met <- if (input$f_metric=="ha") "Cobertura neta (Ha)" else "Cobertura neta (%)"
    htmltools::HTML(sprintf(
      '<div style="background:#fff;padding:6px 10px;border-radius:8px;
                   box-shadow:0 1px 6px rgba(0,0,0,.15);font-size:12px;line-height:1.3;">
         <b>Indicador:</b> %s<br>
         <b>Año:</b> %s
       </div>', met, yr))
  })
  
  hover_label_opts       <- leaflet::labelOptions(direction="auto", textsize="12px", sticky=TRUE, opacity=0.95, className="lbl-clean")
  hover_label_opts_small <- leaflet::labelOptions(direction="auto", textsize="11px", sticky=TRUE, opacity=0.95, className="lbl-clean")
  
  output$map_eva <- leaflet::renderLeaflet({
    req(input$f_depto, input$f_metric, input$f_anio)
    
    if (is.null(input$f_depto) || input$f_depto == "Todos") {
      nivel_mapa("depto"); depto_sel(NULL)
      
      mdat <- depto_sf |>
        dplyr::left_join(agg_depto(), by="DEPARTAMENTO_D") |>
        dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
      
      pal <- palBin4(mdat$valor, clamp01 = (input$f_metric=="pct"))
      ttl <- if (input$f_metric=="ha") "Hectáreas" else "Porcentaje"
      
      leaflet::leaflet(mdat) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          layerId=~DEPARTAMENTO_D, fillColor=~pal(valor),
          weight=0.7, color="#666", fillOpacity=0.9,
          label=~sprintf(
            "%s — %s",
            DEPTO_TC,
            if (input$f_metric=="ha") paste0(format_short(valor)," Ha") else format_pct_es(valor, digits=2)
          ),
          labelOptions=hover_label_opts,
          highlightOptions=leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
        ) |>
        leaflet::addLegend(
          position="bottomright", pal=pal, values=~valor, title=ttl,
          labFormat = if (input$f_metric=="ha") labelFormat_es(digits=0, first_no_symbol=TRUE)
          else labelFormat_es(suffix="%", digits=1, first_no_symbol=TRUE)
        ) |>
        leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
      
    } else {
      nivel_mapa("mpio"); depto_sel(input$f_depto)
      
      mdat <- mpios_sf |>
        dplyr::filter(DEPARTAMENTO_D == depto_sel()) |>
        dplyr::left_join(agg_mpio(), by=c("MUNICIPIO_D","DEPARTAMENTO_D"))
      
      pal <- palBin4(mdat$valor, clamp01 = (input$f_metric=="pct"))
      ttl <- if (input$f_metric=="ha") "Hectáreas" else "Porcentaje"
      
      leaflet::leaflet(mdat) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          layerId=~MUNICIPIO_D, fillColor=~pal(valor),
          weight=0.4, color="#666", fillOpacity=0.9,
          label=~sprintf(
            "%s (%s) — %s",
            MPIO_TC, DEPTO_TC,
            if (input$f_metric=="ha") paste0(format_short(valor)," Ha") else format_pct_es(valor, digits=2)
          ),
          labelOptions=hover_label_opts_small,
          highlightOptions=leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
        ) |>
        leaflet::addLegend(
          position="bottomright", pal=pal, values=~valor, title=ttl,
          labFormat = if (input$f_metric=="ha") labelFormat_es(digits=0, first_no_symbol=TRUE)
          else labelFormat_es(suffix="%", digits=1, first_no_symbol=TRUE)
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
      dplyr::left_join(agg_depto(), by="DEPARTAMENTO_D") |>
      dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
    pal <- palBin4(mdat$valor, clamp01 = (input$f_metric=="pct"))
    ttl <- if (input$f_metric=="ha") "Área de bosque que queda (Ha)" else "Porcentaje de bosque que queda (%)"
    
    leaflet::leafletProxy("map_eva", data=mdat) |>
      leaflet::clearPopups() |> leaflet::clearShapes() |> leaflet::clearMarkers() |> leaflet::clearControls() |>
      leaflet::addPolygons(
        layerId=~DEPARTAMENTO_D, fillColor=~pal(valor),
        weight=0.7, color="#666", fillOpacity=0.9,
        label=~sprintf(
          "%s — %s",
          DEPTO_TC,
          if (input$f_metric=="ha") paste0(format_short(valor)," Ha") else format_pct_es(valor, digits=2)
        ),
        labelOptions=hover_label_opts,
        highlightOptions=leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
      ) |>
      leaflet::addLegend(
        position="bottomright", pal=pal, values=~valor, title=ttl,
        labFormat = if (input$f_metric=="ha") labelFormat_es(digits=0, first_no_symbol=TRUE)
        else labelFormat_es(suffix="%", digits=1, first_no_symbol=TRUE)
      ) |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  }
  
  dibujar_mpios <- function(dep){
    mdat <- mpios_sf |>
      dplyr::filter(DEPARTAMENTO_D == dep) |>
      dplyr::left_join(agg_mpio(), by=c("MUNICIPIO_D","DEPARTAMENTO_D"))
    pal <- palBin4(mdat$valor, clamp01 = (input$f_metric=="pct"))
    
    if (!is.null(input$f_mpio) && input$f_mpio != "Todos") {
      sel_key <- input$f_mpio
      mdat <- mdat |> dplyr::mutate(valor_plot = dplyr::if_else(MUNICIPIO_D == sel_key, valor, NA_real_))
    } else {
      mdat <- mdat |> dplyr::mutate(valor_plot = valor)
    }
    
    ttl <- if (input$f_metric=="ha") "Hectáreas" else "Porcentaje"
    
    leaflet::leafletProxy("map_eva", data=mdat) |>
      leaflet::clearPopups() |> leaflet::clearShapes() |> leaflet::clearMarkers() |> leaflet::clearControls() |>
      leaflet::addPolygons(
        layerId=~MUNICIPIO_D, fillColor=~pal(valor_plot),
        weight=0.4, color="#666", fillOpacity=0.9,
        label=~sprintf(
          "%s (%s) — %s",
          MPIO_TC, DEPTO_TC,
          ifelse(
            is.na(valor_plot), "Sin información",
            if (input$f_metric=="ha") paste0(format_short(valor_plot)," Ha") else format_pct_es(valor_plot, digits=2)
          )
        ),
        labelOptions=hover_label_opts_small,
        highlightOptions=leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)
      ) |>
      leaflet::addLegend(
        position="bottomright", pal=pal, values=mdat$valor, title=ttl,
        labFormat = if (input$f_metric=="ha") labelFormat_es(digits=0, first_no_symbol=TRUE)
        else labelFormat_es(suffix="%", digits=1, first_no_symbol=TRUE)
      ) |>
      leaflet::addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  }
  
  observeEvent(input$f_depto, {
    dep <- input$f_depto
    if (is.null(dep) || dep=="Todos") {
      nivel_mapa("depto"); depto_sel(NULL); dibujar_deptos()
    } else {
      nivel_mapa("mpio"); depto_sel(dep); dibujar_mpios(dep)
    }
  })
  
  observeEvent(input$f_mpio, {
    if (nivel_mapa() == "mpio" && !is.null(depto_sel())) dibujar_mpios(depto_sel())
  }, ignoreInit = TRUE)
  
  observeEvent(input$map_eva_shape_click, {
    click <- input$map_eva_shape_click
    if (is.null(click$id)) return()
    if (nivel_mapa()=="depto") {
      depto_sel(click$id); nivel_mapa("mpio"); dibujar_mpios(click$id)
    }
  })
  
  observeEvent(input$btn_volver, {
    updateSelectInput(session, "f_depto", selected="Todos")
    updateSelectInput(session, "f_mpio",  selected="Todos")
    nivel_mapa("depto"); depto_sel(NULL); dibujar_deptos()
  })
  
  # ========= Serie temporal =========
  series_data <- reactive({
    base <- eva_df
    if (!is.null(input$f_depto) && input$f_depto!="Todos") base <- base |> dplyr::filter(DEPTO_KEY == input$f_depto)
    if (!is.null(input$f_mpio)  && input$f_mpio !="Todos") base <- base |> dplyr::filter(MPIO_KEY  == input$f_mpio)
    
    if (input$f_metric == "ha") {
      base |>
        dplyr::group_by(anio) |>
        dplyr::summarise(valor_total = sum(cobertura_ha, na.rm=TRUE), .groups="drop") |>
        dplyr::arrange(anio)
    } else {
      base |>
        dplyr::group_by(anio) |>
        dplyr::summarise(
          base_sum    = sum(base_ha_2000, na.rm=TRUE),
          cob_sum     = sum(cobertura_ha,  na.rm=TRUE),
          valor_total = dplyr::if_else(base_sum > 0, 100*cob_sum/base_sum, NA_real_),
          .groups="drop"
        ) |>
        dplyr::select(anio, valor_total) |>
        dplyr::arrange(anio)
    }
  })
  
  output$plot_arriba <- plotly::renderPlotly({
    df <- series_data()
    if (!nrow(df)) return(plotly::plot_ly())
    
    if (input$f_metric=="ha") {
      ylab        <- "Hectáreas"
      y_breaks    <- pretty(df$valor_total, n=5); y_breaks <- y_breaks[y_breaks >= 0]
      y_ticktext  <- format_short(y_breaks)
      hover_tmpl  <- "<b>Año:</b> %{x}<br>Hectáreas %{customdata}<extra></extra>"
      custom_vals <- format_short(df$valor_total)
    } else {
      ylab        <- "Porcentaje"
      custom_vals <- format_pct_es(df$valor_total, digits=2)
      min_val <- max(0, floor(min(df$valor_total, na.rm=TRUE) / 5) * 5)
      max_val <- min(100, ceiling(max(df$valor_total, na.rm=TRUE) / 5) * 5)
      if (!is.finite(min_val) || !is.finite(max_val) || min_val >= max_val) { min_val <- 0; max_val <- 100 }
      y_breaks   <- seq(min_val, max_val, by=5)
      y_ticktext <- format_pct_es(y_breaks, digits=0)
      hover_tmpl <- "<b>Año:</b> %{x}<br>Porcentaje %{customdata}<extra></extra>"
    }
    
    x_min <- suppressWarnings(min(df$anio, na.rm=TRUE))
    if (!is.finite(x_min)) x_min <- 0
    
    plotly::plot_ly(
      data=df, x=~anio, y=~valor_total,
      type="scatter", mode="lines+markers",
      line=list(width=2, color=SERIE_COLOR),
      marker=list(size=6, color=SERIE_COLOR),
      customdata = custom_vals,
      hovertemplate=hover_tmpl
    ) |>
      plotly::layout(
        font = list(family="Inter"),
        xaxis=list(title="", tickmode="linear", tick0=x_min, dtick=2, tickangle=0, showgrid=FALSE),
        yaxis=list(title=ylab, tickvals=y_breaks, ticktext=y_ticktext, showgrid=FALSE),
        hovermode="x unified",
        margin=list(l=60, r=20, t=30, b=50),
        legend=list(orientation="h")
      )
  })
  
  # ========= Ranking Top-10 =========
  ranking_data <- reactive({
    df <- datos_filtrados()
    if (input$f_metric=="ha") {
      df |>
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D, MUNICIPIO_RAW, DEPARTAMENTO_RAW) |>
        dplyr::summarise(valor_total = sum(cobertura_ha, na.rm=TRUE), .groups="drop") |>
        dplyr::arrange(dplyr::desc(valor_total)) |>
        dplyr::slice_head(n=10) |>
        dplyr::mutate(muni_tc=title_case_es(MUNICIPIO_RAW), depto_tc=title_case_es(DEPARTAMENTO_RAW))
    } else {
      df |>
        dplyr::group_by(MUNICIPIO_D, DEPARTAMENTO_D, MUNICIPIO_RAW, DEPARTAMENTO_RAW) |>
        dplyr::summarise(
          base_sum    = sum(base_ha_2000, na.rm=TRUE),
          cob_sum     = sum(cobertura_ha,  na.rm=TRUE),
          valor_total = dplyr::if_else(base_sum > 0, 100*cob_sum/base_sum, NA_real_),
          .groups="drop"
        ) |>
        dplyr::arrange(dplyr::desc(valor_total)) |>
        dplyr::slice_head(n=10) |>
        dplyr::mutate(muni_tc=title_case_es(MUNICIPIO_RAW), depto_tc=title_case_es(DEPARTAMENTO_RAW))
    }
  })
  
  output$ranking_abajo <- plotly::renderPlotly({
    plot_df <- ranking_data()
    if (!nrow(plot_df)) {
      return(plotly::plot_ly() |>
               plotly::layout(annotations=list(text="Sin datos para el ranking", x=0.5, y=0.5, showarrow=FALSE)))
    }
    
    plot_df <- plot_df |>
      dplyr::mutate(muni_label = muni_tc) |>
      dplyr::arrange(valor_total)
    
    if (input$f_metric == "ha") {
      val_fmt <- format_short(plot_df$valor_total)
      max_val <- max(plot_df$valor_total, na.rm=TRUE)
      breaks  <- pretty(c(0, max_val), n=5); breaks <- breaks[breaks >= 0]
      xaxis_opts <- list(title="Hectáreas", tickvals=breaks, ticktext=format_short(breaks), showgrid=FALSE)
      hover_tpl <- "<b>Municipio:</b> %{customdata[0]}<br><b>Departamento:</b> %{customdata[1]}<br><b>Cobertura neta (Ha):</b> %{customdata[2]}<extra></extra>"
    } else {
      val_fmt <- format_pct_es(plot_df$valor_total, digits=1)
      breaks <- seq(0, 100, 20)
      xaxis_opts <- list(title="Porcentaje", tickvals=breaks, ticktext=format_pct_es(breaks, digits=0), showgrid=FALSE, range=c(0,100))
      hover_tpl <- "<b>Municipio:</b> %{customdata[0]}<br><b>Departamento:</b> %{customdata[1]}<br><b>Cobertura neta (%):</b> %{customdata[2]}<extra></extra>"
    }
    
    plotly::plot_ly(
      data=plot_df,
      x=~valor_total, y=~muni_label,
      type="bar", orientation="h",
      marker=list(color=BAR_COLOR),
      text=val_fmt, textposition="inside",
      insidetextanchor="middle",
      insidetextfont=list(family="Inter SemiBold, Inter, Arial, sans-serif", size=12, color="white"),
      hovertemplate=hover_tpl,
      customdata=cbind(plot_df$muni_tc, plot_df$depto_tc, val_fmt),
      cliponaxis=FALSE
    ) |>
      plotly::layout(
        font=list(family="Inter"),
        xaxis=xaxis_opts,
        yaxis=list(title="", categoryorder="array", categoryarray=rev(plot_df$muni_label), showgrid=FALSE),
        margin=list(l=120, r=40, t=20, b=40)
      )
  })
  
  # ========= Descarga CSV =========
  tabla_export <- reactive({
    df <- datos_filtrados()
    if (input$f_metric=="ha") {
      df |> dplyr::transmute(
        DEPARTAMENTO = title_case_es(DEPARTAMENTO_RAW),
        MUNICIPIO    = title_case_es(MUNICIPIO_RAW),
        anio,
        cobertura_neta_ha = cobertura_ha,
        base_ha_2000
      )
    } else {
      df |> dplyr::transmute(
        DEPARTAMENTO = title_case_es(DEPARTAMENTO_RAW),
        MUNICIPIO    = title_case_es(MUNICIPIO_RAW),
        anio,
        cobertura_neta_pct = cobertura_pct,
        cobertura_neta_ha  = cobertura_ha,
        base_ha_2000
      )
    }
  })
  
  output$dl_csv_expl <- downloadHandler(
    filename = function() paste0("HANSEN_cobertura_neta_", safe_chr(input$f_metric), "_", safe_chr(input$f_anio), "_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(tabla_export(), file, na = "")
  )
  
  # -------- Descarga PNG Serie --------
  output$dl_png_series <- downloadHandler(
    filename = function() paste0("HANSEN_cobertura_serie_", safe_chr(input$f_metric), "_", Sys.Date(), ".png"),
    content  = function(file){
      df <- series_data()
      if (!nrow(df)) { file.create(file); return() }
      
      ylab <- if (input$f_metric=="ha") "Cobertura neta (Ha)" else "Cobertura neta (%)"
      x_breaks <- seq(min(df$anio, na.rm=TRUE), max(df$anio, na.rm=TRUE), by=2)
      
      g <- ggplot(df, aes(x=anio, y=valor_total)) +
        geom_line(linewidth=0.9, color=SERIE_COLOR) +
        geom_point(size=2.2, color=SERIE_COLOR) +
        scale_x_continuous(breaks=x_breaks) +
        scale_y_continuous(
          labels = if (input$f_metric=="ha") format_short
          else scales::label_number(big.mark=".", decimal.mark=",")
        ) +
        labs(x="Año", y=ylab, title=paste0("Evolución anual de ", ylab)) +
        theme_minimal(base_size=12) +
        theme(
          text = element_text(family="Inter"),
          axis.text.x = element_text(angle=0, hjust=0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()
        )
      
      ggsave(filename=file, plot=g, device=ragg::agg_png, width=10, height=5, dpi=200, units="in")
    }
  )
  
  # -------- Descarga PNG Mapa --------
  map_widget_simple <- reactive({
    if (nivel_mapa()=="depto"){
      mdat <- depto_sf |>
        dplyr::left_join(agg_depto(), by="DEPARTAMENTO_D") |>
        dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
      pal <- palBin4(mdat$valor, clamp01 = (input$f_metric=="pct"))
      ttl <- if (input$f_metric=="ha") "Área de bosque que queda (Ha) por departamento"
      else "Porcentaje de bosque que queda (%) por departamento"
      leaflet::leaflet(mdat, options=leaflet::leafletOptions(zoomControl=FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(fillColor=~pal(valor), weight=0.5, color="#666", fillOpacity=0.9) |>
        leaflet::addControl(html=htmltools::HTML(sprintf(
          "<div style='font-weight:600;font-size:14px;background:#fff;padding:6px 8px;border-radius:8px;border:1px solid #e6e6e6'>%s — %s</div>",
          ttl, safe_chr(input$f_anio)
        )), position="topleft")
    } else {
      dep <- depto_sel()
      mdat <- mpios_sf |>
        dplyr::filter(DEPARTAMENTO_D==dep) |>
        dplyr::left_join(agg_mpio(), by=c("MUNICIPIO_D","DEPARTAMENTO_D"))
      pal <- palBin4(mdat$valor, clamp01 = (input$f_metric=="pct"))
      if (!is.null(input$f_mpio) && input$f_mpio != "Todos") {
        sel_key <- input$f_mpio
        mdat <- mdat |> dplyr::mutate(valor_plot = dplyr::if_else(MUNICIPIO_D == sel_key, valor, NA_real_))
      } else {
        mdat <- mdat |> dplyr::mutate(valor_plot = valor)
      }
      ttl <- if (input$f_metric=="ha") "Área de bosque que queda (Ha) por municipios"
      else "Porcentaje de bosque que queda (%) por municipios"
      leaflet::leaflet(mdat, options=leaflet::leafletOptions(zoomControl=FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(fillColor=~pal(valor_plot), weight=0.4, color="#666", fillOpacity=0.9) |>
        leaflet::addControl(html=htmltools::HTML(sprintf(
          "<div style='font-weight:600;font-size:14px;background:#fff;padding:6px 8px;border-radius:8px;border:1px solid #e6e6e6'>%s — %s</div>",
          ttl, safe_chr(input$f_anio)
        )), position="topleft")
    }
  })
  
  output$dl_png_mapa <- downloadHandler(
    filename = function() paste0("HANSEN_cobertura_mapa_", safe_chr(input$f_metric), "_", Sys.Date(), ".png"),
    content  = function(file){
      widget   <- map_widget_simple()
      tmp_html <- tempfile(fileext=".html")
      htmlwidgets::saveWidget(widget, tmp_html, selfcontained=TRUE)
      webshot2::webshot(tmp_html, file=file, vwidth=1200, vheight=800, zoom=2)
    }
  )
  
  # -------- Descarga PNG Ranking --------
  output$dl_png_ranking <- downloadHandler(
    filename = function() paste0("HANSEN_cobertura_ranking_", safe_chr(input$f_metric), "_", safe_chr(input$f_anio), "_", Sys.Date(), ".png"),
    content  = function(file){
      plot_df <- ranking_data()
      if (!nrow(plot_df)) { file.create(file); return() }
      
      plot_df <- plot_df |>
        dplyr::mutate(muni_label = muni_tc) |>
        dplyr::arrange(dplyr::desc(valor_total))
      
      if (input$f_metric == "ha") {
        max_val <- max(plot_df$valor_total, na.rm=TRUE)
        breaks  <- pretty(c(0, max_val), n=5); breaks <- breaks[breaks >= 0]
        g <- ggplot(plot_df, aes(x=valor_total, y=reorder(muni_label, valor_total))) +
          geom_col(fill=BAR_COLOR) +
          geom_text(aes(x=valor_total/2, label=format_short(valor_total)), color="white", size=3) +
          scale_x_continuous(labels=format_short, breaks=breaks, expand=expansion(mult=c(0,0.05))) +
          labs(x="Cobertura neta (Ha)", y=NULL, title=paste0("Top-10 municipios por área de bosque (Ha) — ", safe_chr(input$f_anio)))
      } else {
        breaks <- seq(0,100,20)
        g <- ggplot(plot_df, aes(x=valor_total, y=reorder(muni_label, valor_total))) +
          geom_col(fill=BAR_COLOR) +
          geom_text(aes(x=valor_total/2, label=format_pct_es(valor_total, digits=1)), color="white", size=3) +
          scale_x_continuous(labels=function(x) format_pct_es(x, digits=0), breaks=breaks, limits=c(0,100),
                             expand=expansion(mult=c(0,0.05))) +
          labs(x="Porcentaje", y=NULL, title=paste0("Top-10 municipios por porcentaje de bosque (%) — ", safe_chr(input$f_anio)))
      }
      
      g <- g + theme_minimal(base_size=12) +
        theme(text=element_text(family="Inter"),
              axis.text.y=element_text(size=9),
              plot.margin=margin(r=30),
              panel.grid.minor=element_blank(),
              panel.grid.major=element_blank())
      
      ggsave(filename=file, plot=g, device=ragg::agg_png, width=10, height=6, dpi=200, units="in")
    }
  )
}

shinyApp(ui = ui, server = server)
