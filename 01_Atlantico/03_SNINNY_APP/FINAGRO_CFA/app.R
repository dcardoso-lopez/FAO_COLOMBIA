# =========================================================
# app_finagro_moderno.R — Tendencias + Indicadores
# MODIFICADA CON LÓGICA DE BOTONES TIPO ICA
# - Botones PNG por visual
# - Botón CSV
# - Botón PDF robusto con Rmarkdown
# - PNGs fijos en ./Descargas
# - PDF solo se permite cuando ya se hayan visitado las 2 pestañas
# - El informe procesa ambas pestañas
# =========================================================

suppressWarnings({
  library(shiny); library(dplyr); library(plotly)
  library(scales); library(ggplot2); library(networkD3)
  library(sf); library(leaflet); library(bslib); library(stringr)
  library(htmlwidgets); library(webshot2); library(rmarkdown)
  library(readr); library(htmltools)
})

options(stringsAsFactors = FALSE, scipen = 999)
options(shiny.maxRequestSize = 100*1024^2)
sf::sf_use_s2(FALSE)

# =========================================================
# HELPERS GENERALES
# =========================================================
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

get_app_root <- function(){
  normalizePath(shiny::getShinyOption("appDir") %||% getwd(), winslash = "/", mustWork = FALSE)
}

safe_first <- function(x, default = "?"){
  x <- x[!is.na(x)]
  if (length(x) == 0) default else x[1]
}

# ---------- Helper: Title Case en español ----------
title_case_es <- function(x){
  stopwords <- c("de","del","la","las","el","los","y","e","o","u",
                 "con","en","por","para","a","al")
  vapply(x, function(z){
    if (is.na(z) || !nzchar(z)) return(z)
    z_low  <- tolower(z)
    parts  <- unlist(strsplit(z_low, "\\s+"))
    parts  <- parts[parts != ""]
    if (!length(parts)) return(z)
    parts2 <- sapply(seq_along(parts), function(i){
      w <- parts[i]
      if (i == 1 || !(w %in% stopwords)) {
        paste0(toupper(substr(w, 1, 1)), substring(w, 2))
      } else w
    })
    paste(parts2, collapse = " ")
  }, FUN.VALUE = character(1L))
}

# ✅ ORDEN FIJO ESLABÓN
ESLABON_LEVELS <- c(
  "Producción",
  "Transformación",
  "Comercialización",
  "Servicios de Apoyo"
)

normalize_eslabon <- function(x){
  trimws(title_case_es(as.character(x)))
}

# ---------- Helpers numéricos ----------
fmt_int   <- function(x) number(x, big.mark = ".", decimal.mark = ",", accuracy = 1)
fmt_cop   <- function(x) paste0("$", number(x, big.mark = ".", decimal.mark = ",", accuracy = 1))
fmt_mmilM <- function(x) paste0("$", number(x/1e9, big.mark = ".", decimal.mark = ",", accuracy = 0.1), " Mil M")
fmt_milM  <- function(x) paste0(number(x/1e9, big.mark = ".", decimal.mark = ",", accuracy = 0.1), " Mil M")
mes_labels <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

# ---------- Paleta por CUARTILES (robusta) ----------
pal_quartiles_safe <- function(palette, x){
  vals <- x[is.finite(x)]
  if (!length(vals) || all(vals <= 0, na.rm = TRUE)) {
    return(leaflet::colorBin(palette, domain = c(0, 1), bins = 4, na.color = "#f0f0f0"))
  }
  if (length(unique(vals)) == 1) {
    v   <- unique(vals)
    eps <- ifelse(v == 0, 1, abs(v) * 1e-9)
    dom <- c(v - eps, v + eps)
    return(leaflet::colorBin(palette, domain = dom, bins = 4, na.color = "#f0f0f0"))
  }
  qs <- stats::quantile(vals, probs = seq(0, 1, length.out = 5), na.rm = TRUE)
  qs <- unique(qs)
  if (length(qs) < 2) {
    rng <- range(vals, na.rm = TRUE)
    if (rng[1] == rng[2]) {
      v   <- rng[1]
      eps <- ifelse(v == 0, 1, abs(v) * 1e-9)
      dom <- c(v - eps, v + eps)
      return(leaflet::colorBin(palette, domain = dom, bins = 4, na.color = "#f0f0f0"))
    } else {
      qs <- seq(rng[1], rng[2], length.out = 5)
    }
  }
  leaflet::colorBin(palette, domain = x, bins = qs, na.color = "#f0f0f0")
}

COL_BORDE_POLY <- "#2E7D32"

pal_story <- c(
  "#8d6e63",
  "#a17236",
  "#c49000",
  "#e0a93a",
  "#f2c86b",
  "#f9e4b7"
)

# =========================================================
# EXPORTACIÓN TIPO ICA
# =========================================================
app_root   <- get_app_root()
EXPORT_DIR <- file.path(app_root, "Descargas")
dir.create(EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)

ruta_rmd <- file.path(app_root, "Informe_descargable.Rmd")

PNG_VWIDTH   <- 3000
PNG_VHEIGHT  <- 2300
PNG_DELAY_CO <- 1.4
PNG_DELAY_MUN <- 2.6

# nombres fijos para el Rmd
IMG_T1_MAP   <- file.path(EXPORT_DIR, "finagro_tab1_mapa.png")
IMG_T1_SER   <- file.path(EXPORT_DIR, "finagro_tab1_serie.png")
IMG_T1_TOP   <- file.path(EXPORT_DIR, "finagro_tab1_top10.png")
IMG_T2_SER   <- file.path(EXPORT_DIR, "finagro_tab2_serie.png")
IMG_T2_TOP   <- file.path(EXPORT_DIR, "finagro_tab2_toplineas.png")
IMG_T2_SAN   <- file.path(EXPORT_DIR, "finagro_tab2_sankey.png")

save_widget_png <- function(widget, out_png, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay = PNG_DELAY_CO){
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  
  tmp_dir  <- tempfile("wshot_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  lib_dir  <- file.path(tmp_dir, "lib")
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
  tmp_html <- file.path(tmp_dir, "widget.html")
  
  htmlwidgets::saveWidget(widget, file = tmp_html, selfcontained = FALSE, libdir = lib_dir)
  
  webshot2::webshot(
    url     = tmp_html,
    file    = out_png,
    vwidth  = vwidth,
    vheight = vheight,
    delay   = delay
  )
  
  file.exists(out_png) && is.finite(file.info(out_png)$size) && file.info(out_png)$size > 0
}

save_widget_png_retry <- function(widget, out_png, vwidth, vheight, delay_base){
  delays <- c(delay_base, delay_base + 2.0, delay_base + 4.0)
  for (d in delays){
    ok <- tryCatch(
      save_widget_png(widget, out_png, vwidth = vwidth, vheight = vheight, delay = d),
      error = function(e) FALSE
    )
    if (isTRUE(ok)) return(TRUE)
  }
  FALSE
}

# fallback para sankey en png
save_sankey_png_fallback <- function(out_png, width = 1800, height = 1000){
  png(filename = out_png, width = width, height = height, res = 150, bg = "white")
  par(mar = c(0,0,0,0))
  plot.new()
  text(0.5, 0.65, "Mapa de asignación: líneas de crédito \u2192 eslabones productivos", cex = 1.2, font = 2)
  text(0.5, 0.45, "La visualización Sankey es interactiva en la app.", cex = 1.0)
  text(0.5, 0.35, "Para el informe PDF se incluye este marcador de posición", cex = 1.0)
  text(0.5, 0.25, "cuando no es posible rasterizar directamente el htmlwidget.", cex = 1.0)
  dev.off()
  file.exists(out_png)
}

# =========================================================
# RUTAS / DATOS
# =========================================================
data_dir <- "data"

finagro_fast       <- readRDS(file.path(data_dir, "081_FINAGRO_CFA.rds"))
finagro_depto_map  <- readRDS(file.path(data_dir, "map_finagro_depto.rds"))
finagro_mpio_map   <- readRDS(file.path(data_dir, "map_finagro_mpio.rds"))
mpios_sf           <- readRDS(file.path(data_dir, "mpios_sf_simpl.rds"))
dptos_sf           <- readRDS(file.path(data_dir, "dptos_sf_simpl.rds"))

# =========================================================
# NORMALIZACIÓN FINAGRO
# =========================================================
finagro_fast <- finagro_fast %>%
  mutate(
    COD_DPTO2 = stringr::str_pad(as.character(COD_DANE_DPTO_D), 2, pad = "0"),
    COD_MUN5  = stringr::str_pad(as.character(COD_DANE_MUNIC_D), 5, pad = "0"),
    NOM_DPTO  = as.character(DEPARTAMENTO_D),
    NOM_MPIO  = as.character(MUNICIPIO_D),
    SEXO2     = as.character(SEXO),
    ESLABON_TC = normalize_eslabon(ESLABON_CADENA)
  )

# opcional: dejar solo Atlántico
finagro_fast <- finagro_fast %>% dplyr::filter(NOM_DPTO == "ATLÁNTICO")

# ---------- IPP y deflactación ----------
ipp_tbl <- data.frame(
  ano = c(2010:2025),
  IPP = c(
    0.8851, 0.9633, 0.9625, 0.9502, 0.9778, 1.0158, 1.0707, 1.0801,
    1.1356, 1.1848, 1.1758, 1.3756, 1.7822, 1.7923, 1.7989, 1.8585
  )
)

finagro_fast <- finagro_fast %>%
  left_join(ipp_tbl, by = "ano") %>%
  mutate(
    IPP = ifelse(is.na(IPP), 1, IPP),
    VALOR_CREDITO_REAL = VALOR_CREDITO / IPP
  )

# ---------- Choices ----------
depto_vec     <- sort(unique(finagro_fast$NOM_DPTO))
depto_choices <- c("Todos" = "Todos", stats::setNames(depto_vec, title_case_es(depto_vec)))

mpio_vec         <- sort(unique(finagro_fast$NOM_MPIO))
mpio_choices_all <- c("Todos" = "Todos", stats::setNames(mpio_vec, title_case_es(mpio_vec)))

DEFAULT_DEPTO <- if ("ATLÁNTICO" %in% depto_vec) "ATLÁNTICO" else if (length(depto_vec)) depto_vec[1] else "Todos"

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  theme = bs_theme(
    version = 5, primary = "#2563eb",
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius"  = "0.9rem",
    "font-size-base" = "0.97rem"
  ),
  tags$head(
    tags$style(HTML(sprintf("
    .wrap{ max-width:1360px; margin:0 auto; padding:16px 20px 32px; }
    .data-note{ font-size:13px; color:#6b7280; margin:0 0 16px; }
    .filters{
      background:#fff; border:1px solid %s; border-radius:16px;
      padding:14px 16px; margin-bottom:16px; box-shadow:0 4px 14px rgba(0,0,0,.06);
    }
    .filters-grid{ display:grid; grid-template-columns:repeat(3,minmax(220px,1fr)); gap:12px; }
    .filter-label{ font-size:14px; font-weight:500; color:#000; margin-bottom:6px; }
    .selectize-input, .selectize-dropdown, .form-control{ font-size:14px; font-weight:500; color:#000; }
    .selectize-input, .form-control{
      min-height:42px; border-radius:10px; border:1px solid %s;
      box-shadow:0 0 0 1px rgba(255,179,102,.12);
    }
    .selectize-input.focus, .selectize-input.input-active, .form-control:focus{
      border-color:%s; box-shadow:0 0 0 2px rgba(255,179,102,.35); outline:none;
    }
    .card{
      background:#fff; border:1px solid %s; border-radius:16px; padding:14px;
      box-shadow:0 2px 10px rgba(0,0,0,.05); margin-bottom:12px;
    }
    .card-title{ font-weight:700; font-size:16px; margin-bottom:8px; color:#111827; }
    .grid-4{ display:grid; grid-template-columns:repeat(4,1fr); gap:12px; }
    .metric-value{ font-size:28px; font-weight:800; color:#111827; margin:2px 0 0; }
    .metric-sub{ font-size:12px; color:#6b7280; margin-top:2px; }

    .btn-unified{
      background:#ffffff !important;
      border:1px solid %s !important;
      color:#374151 !important;
      font-weight:700 !important;
      border-radius:12px !important;
      padding:6px 10px !important;
      font-size:12px !important;
    }
    .footer-actions{
      margin-top: 10px;
      display:flex;
      justify-content:flex-end;
      gap: 8px;
      padding: 6px 6px 0;
      flex-wrap: wrap;
    }
    .pdf-note{
      margin-top: 8px;
      font-size: 12px;
      color:#4b5563;
      text-align:right;
    }
    ", COL_BORDE_POLY, COL_BORDE_POLY, COL_BORDE_POLY, COL_BORDE_POLY, COL_BORDE_POLY)))
  ),
  div(
    class = "wrap",
    div(class = "data-note", ""),
    tabsetPanel(
      id   = "tabs_finagro",
      type = "tabs",
      
      # ============ TAB 1 ============
      tabPanel(
        "Dinámica histórica del crédito agropecuario", br(),
        div(
          class = "filters",
          div(
            class = "filters-grid",
            div(class="filter",
                div(class="filter-label","¿Qué año analizamos?"),
                selectInput("ano_t1", NULL,
                            choices  = c("Todos", sort(unique(finagro_fast$ano))),
                            selected = "Todos")
            ),
            div(class="filter",
                div(class="filter-label","¿En que departamento?"),
                selectInput("depto_t1", NULL,
                            choices  = depto_choices,
                            selected = DEFAULT_DEPTO)
            ),
            div(class="filter",
                div(class="filter-label","¿Algún municipio en particular?"),
                selectizeInput("mpio_t1", NULL,
                               choices  = c("Todos"="Todos"),
                               selected = "Todos",
                               options  = list(placeholder="Escribe para buscar…"))
            ),
            div(class="filter",
                div(class="filter-label","¿Eslabón de la cadena?"),
                selectInput("eslabon_t1", NULL,
                            choices  = c("Todos", ESLABON_LEVELS),
                            selected = "Todos")
            ),
            div(class="filter",
                div(class="filter-label","¿Tipo de productor?"),
                selectInput("productor_t1", NULL,
                            choices  = c("Todos", sort(unique(finagro_fast$TIPO_PRODUCTOR))),
                            selected = "Todos")
            ),
            div(class="filter",
                div(class="filter-label","¿Qué indicador quieres ver?"),
                selectInput("var_m_t1", NULL,
                            choices  = c("Monto total real del crédito"="monto",
                                         "Número de créditos"="creditos"),
                            selected = "monto")
            )
          )
        ),
        
        div(
          class = "grid-4",
          div(class="card",
              uiOutput("kpi_hist_monto_title"),
              uiOutput("hist_monto_txt"),
              div(class="metric-sub","")
          ),
          div(class="card",
              uiOutput("kpi_hist_creditos_title"),
              uiOutput("hist_creditos_txt"),
              div(class="metric-sub"," ")
          ),
          div(class="card",
              uiOutput("kpi_hist_prom_title"),
              uiOutput("hist_prom_txt"),
              div(class="metric-sub","Monto / Nº créditos")
          ),
          div(class="card",
              uiOutput("kpi_hist_mujeres_title"),
              uiOutput("hist_mujeres_txt"),
              div(class="metric-sub","% sobre total de créditos")
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            div(class="card",
                div(class="card-title d-flex justify-content-between align-items-center",
                    span(uiOutput("titulo_mapa_t1")),
                    downloadButton("dl_png_t1_mapa","Descargar PNG", class="btn-unified")
                ),
                leafletOutput("mapa_m_t1", height = 758)
            )
          ),
          column(
            width = 6,
            div(class="card",
                div(class="card-title d-flex justify-content-between align-items-center",
                    span(uiOutput("titulo_serie_t1")),
                    downloadButton("dl_png_t1_serie","Descargar PNG", class="btn-unified")
                ),
                plotlyOutput("hist_monto_total", height = 330)
            ),
            div(class="card",
                div(class="card-title d-flex justify-content-between align-items-center",
                    span(uiOutput("titulo_top10_t1")),
                    downloadButton("dl_png_t1_top","Descargar PNG", class="btn-unified")
                ),
                plotlyOutput("top5_m_t1", height = 330)
            )
          )
        )
      ),
      
      # ============ TAB 2 ============
      tabPanel(
        "Indicadores anuales para gestionar el portafolio de crédito", br(),
        div(
          class = "filters",
          div(
            class = "filters-grid",
            div(class="filter",
                div(class="filter-label","¿Qué año analizamos?"),
                selectInput("ano", NULL,
                            choices  = sort(unique(finagro_fast$ano)),
                            selected = min(finagro_fast$ano, na.rm = TRUE))
            ),
            div(class="filter",
                div(class="filter-label","¿En que departamento?"),
                selectInput("depto_t2", NULL,
                            choices  = depto_choices,
                            selected = DEFAULT_DEPTO)
            ),
            div(class="filter",
                div(class="filter-label","¿Algún municipio en particular?"),
                selectizeInput("mpio_t2", NULL,
                               choices  = c("Todos"="Todos"),
                               selected = "Todos",
                               options  = list(placeholder="Escribe para buscar…"))
            ),
            div(class="filter",
                div(class="filter-label","¿Eslabón de la cadena?"),
                selectInput("eslabon_t2", NULL,
                            choices  = c("Todos", ESLABON_LEVELS),
                            selected = "Todos")
            ),
            div(class="filter",
                div(class="filter-label","¿Tipo de productor?"),
                selectInput("productor", NULL,
                            choices  = c("Todos", sort(unique(finagro_fast$TIPO_PRODUCTOR))),
                            selected = "Todos")
            ),
            div(class="filter",
                div(class="filter-label","¿Persona natural (Hombre/Mujer) o jurídico?"),
                selectInput("sexo", NULL,
                            choices  = c("Todos","Hombre","Mujer","Jurídico"),
                            selected = "Todos")
            )
          )
        ),
        
        fluidRow(
          column(6,
                 div(class="card",
                     div(class="card-title d-flex justify-content-between align-items-center",
                         span("Evolución mensual de créditos y monto real desembolsado"),
                         downloadButton("dl_png_t2_serie","Descargar PNG", class="btn-unified")
                     ),
                     plotlyOutput("serie_tiempo", height = 390)
                 )
          ),
          column(6,
                 div(class="card",
                     div(class="card-title d-flex justify-content-between align-items-center",
                         span("Concentración de las principales líneas de crédito por eslabón (reales)"),
                         downloadButton("dl_png_t2_top","Descargar PNG", class="btn-unified")
                     ),
                     plotlyOutput("top_lineas_eslabon", height = 390)
                 )
          )
        ),
        fluidRow(
          column(12,
                 div(class="card",
                     div(class="card-title d-flex justify-content-between align-items-center",
                         span("Mapa de asignación: líneas de crédito → eslabones productivos (montos reales)"),
                         downloadButton("dl_png_t2_sankey","Descargar PNG", class="btn-unified")
                     ),
                     sankeyNetworkOutput("sankey", height = "370px")
                 )
          )
        )
      )
    ),
    
    div(class = "pdf-note", textOutput("estado_pdf_txt")),
    
    div(
      class = "footer-actions",
      downloadButton("dl_csv_expl","Descargar CSV", class="btn-unified"),
      downloadButton("dl_reporte_pdf","Descargar informe (PDF)", class="btn-unified")
    )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  
  # colores eslabón
  PAL_ESLAB <- c(
    "Producción"         = "#8d6e63",
    "Transformación"     = "#a17236",
    "Comercialización"   = "#c49000",
    "Servicios de Apoyo" = "#cbd5e1"
  )
  
  # =========================================================
  # CONTROL DE VISITA A LAS DOS PESTAÑAS
  # =========================================================
  tabs_seen <- reactiveVal(character(0))
  
  observeEvent(input$tabs_finagro, {
    req(input$tabs_finagro)
    actuales <- tabs_seen()
    if (!(input$tabs_finagro %in% actuales)) {
      tabs_seen(unique(c(actuales, input$tabs_finagro)))
    }
  }, ignoreInit = FALSE)
  
  ya_vio_tab1 <- reactive({
    "Dinámica histórica del crédito agropecuario" %in% tabs_seen()
  })
  ya_vio_tab2 <- reactive({
    "Indicadores anuales para gestionar el portafolio de crédito" %in% tabs_seen()
  })
  ya_vio_ambas_tabs <- reactive({
    ya_vio_tab1() && ya_vio_tab2()
  })
  
  output$estado_pdf_txt <- renderText({
    if (ya_vio_ambas_tabs()) {
      "Ya puedes descargar el informe: se han visualizado las dos pestañas."
    } else if (ya_vio_tab1() && !ya_vio_tab2()) {
      "Para descargar el informe primero debes visualizar la segunda pestaña."
    } else if (!ya_vio_tab1() && ya_vio_tab2()) {
      "Para descargar el informe primero debes visualizar la primera pestaña."
    } else {
      "Para descargar el informe debes visualizar las dos pestañas."
    }
  })
  
  # =========================================================
  # MUNICIPIOS DEPENDEN DE DEPTO
  # =========================================================
  observe({
    updateSelectizeInput(session, "mpio_t1", choices = mpio_choices_all, selected = "Todos", server = TRUE)
    updateSelectizeInput(session, "mpio_t2", choices = mpio_choices_all, selected = "Todos", server = TRUE)
  })
  
  observeEvent(input$depto_t1, {
    if (is.null(input$depto_t1) || input$depto_t1 == "Todos") {
      updateSelectizeInput(session, "mpio_t1",
                           choices  = mpio_choices_all,
                           selected = "Todos",
                           server   = TRUE)
    } else {
      mpios_dep <- finagro_fast %>%
        filter(NOM_DPTO == input$depto_t1) %>%
        pull(NOM_MPIO) %>% unique() %>% sort()
      mpio_choices_dep <- c("Todos"="Todos", stats::setNames(mpios_dep, title_case_es(mpios_dep)))
      updateSelectizeInput(session, "mpio_t1",
                           choices  = mpio_choices_dep,
                           selected = "Todos",
                           server   = TRUE)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$depto_t2, {
    if (is.null(input$depto_t2) || input$depto_t2 == "Todos") {
      updateSelectizeInput(session, "mpio_t2",
                           choices  = mpio_choices_all,
                           selected = "Todos",
                           server   = TRUE)
    } else {
      mpios_dep <- finagro_fast %>%
        filter(NOM_DPTO == input$depto_t2) %>%
        pull(NOM_MPIO) %>% unique() %>% sort()
      mpio_choices_dep <- c("Todos"="Todos", stats::setNames(mpios_dep, title_case_es(mpios_dep)))
      updateSelectizeInput(session, "mpio_t2",
                           choices  = mpio_choices_dep,
                           selected = "Todos",
                           server   = TRUE)
    }
  }, ignoreInit = TRUE)
  
  # =========================================================
  # TAB 1 — BASES
  # =========================================================
  base_t1 <- reactive({
    df <- finagro_fast
    if (!is.null(input$ano_t1) && input$ano_t1 != "Todos") df <- df %>% filter(ano == as.numeric(input$ano_t1))
    if (!is.null(input$depto_t1) && input$depto_t1 != "Todos") df <- df %>% filter(NOM_DPTO == input$depto_t1)
    if (!is.null(input$mpio_t1) && input$mpio_t1 != "Todos")   df <- df %>% filter(NOM_MPIO == input$mpio_t1)
    if (!is.null(input$eslabon_t1) && input$eslabon_t1 != "Todos") df <- df %>% filter(ESLABON_TC == input$eslabon_t1)
    if (!is.null(input$productor_t1) && input$productor_t1 != "Todos") df <- df %>% filter(TIPO_PRODUCTOR == input$productor_t1)
    df
  })
  
  base_serie_t1 <- reactive({
    df <- finagro_fast
    if (!is.null(input$depto_t1) && input$depto_t1 != "Todos") df <- df %>% filter(NOM_DPTO == input$depto_t1)
    if (!is.null(input$mpio_t1) && input$mpio_t1 != "Todos")   df <- df %>% filter(NOM_MPIO == input$mpio_t1)
    if (!is.null(input$eslabon_t1) && input$eslabon_t1 != "Todos") df <- df %>% filter(ESLABON_TC == input$eslabon_t1)
    if (!is.null(input$productor_t1) && input$productor_t1 != "Todos") df <- df %>% filter(TIPO_PRODUCTOR == input$productor_t1)
    df
  })
  
  base_mapa_t1 <- reactive({
    df <- finagro_fast
    if (!is.null(input$ano_t1) && input$ano_t1 != "Todos") df <- df %>% filter(ano == as.numeric(input$ano_t1))
    if (!is.null(input$depto_t1) && input$depto_t1 != "Todos") df <- df %>% filter(NOM_DPTO == input$depto_t1)
    if (!is.null(input$mpio_t1) && input$mpio_t1 != "Todos")   df <- df %>% filter(NOM_MPIO == input$mpio_t1)
    if (!is.null(input$eslabon_t1) && input$eslabon_t1 != "Todos") df <- df %>% filter(ESLABON_TC == input$eslabon_t1)
    if (!is.null(input$productor_t1) && input$productor_t1 != "Todos") df <- df %>% filter(TIPO_PRODUCTOR == input$productor_t1)
    df
  })
  
  contexto_kpi_t1 <- reactive({
    if (is.null(input$ano_t1) || input$ano_t1 == "Todos") {
      anomin <- min(finagro_fast$ano, na.rm = TRUE)
      anomax <- max(finagro_fast$ano, na.rm = TRUE)
      if (anomin == anomax) paste("en", anomin) else sprintf("entre %d y %d", anomin, anomax)
    } else paste("en", input$ano_t1)
  })
  
  output$kpi_hist_monto_title <- renderUI(tags$div(class="card-title", paste("Monto real total de los créditos", contexto_kpi_t1())))
  output$kpi_hist_creditos_title <- renderUI(tags$div(class="card-title", paste("Número de créditos otorgados", contexto_kpi_t1())))
  output$kpi_hist_prom_title <- renderUI(tags$div(class="card-title", paste("Monto promedio real por crédito", contexto_kpi_t1())))
  output$kpi_hist_mujeres_title <- renderUI(tags$div(class="card-title", paste("Participación de las mujeres en el crédito", contexto_kpi_t1())))
  
  output$hist_monto_txt <- renderUI(tags$div(class="metric-value", fmt_mmilM(sum(base_t1()$VALOR_CREDITO_REAL, na.rm = TRUE))))
  output$hist_creditos_txt <- renderUI(tags$div(class="metric-value", fmt_int(sum(base_t1()$NUMERO_CREDITO, na.rm = TRUE))))
  output$hist_prom_txt <- renderUI({
    df <- base_t1()
    n  <- sum(df$NUMERO_CREDITO, na.rm = TRUE)
    m  <- sum(df$VALOR_CREDITO_REAL, na.rm = TRUE)
    tags$div(class="metric-value", fmt_cop(if (n > 0) m/n else 0))
  })
  output$hist_mujeres_txt <- renderUI({
    df  <- base_t1()
    tot <- sum(df$NUMERO_CREDITO, na.rm = TRUE)
    muj <- sum(df$NUMERO_CREDITO[df$SEXO2 == "Mujer"], na.rm = TRUE)
    pct <- if (tot > 0) 100 * muj/tot else 0
    tags$div(class="metric-value", paste0(number(pct, accuracy = 0.1, decimal.mark=","), "%"))
  })
  
  col_monto_total <- pal_story[1]
  
  titulo_visuales_t1 <- reactive({
    ind <- if (!is.null(input$var_m_t1) && input$var_m_t1 %in% c("monto","creditos")) input$var_m_t1 else "monto"
    if (ind == "monto") {
      list(
        mapa  = "¿En que territorios es mayor el monto de créditos agropecuarios?",
        serie = "¿Cómo ha evolucionado temporalmente el monto de créditos agropecuarios aprobados?",
        top10 = "Top 10 territorios por monto total de créditos aprobados"
      )
    } else {
      list(
        mapa  = "¿En que territorios es mayor la cantidad de créditos agropecuarios?",
        serie = "¿Cómo ha evolucionado temporalmente la cantidad de créditos agropecuarios aprobados?",
        top10 = "Top 10 territorios por cantidad de créditos aprobados"
      )
    }
  })
  
  output$titulo_mapa_t1  <- renderUI(HTML(sprintf("<strong>%s</strong>", titulo_visuales_t1()$mapa)))
  output$titulo_serie_t1 <- renderUI(HTML(sprintf("<strong>%s</strong>", titulo_visuales_t1()$serie)))
  output$titulo_top10_t1 <- renderUI(HTML(sprintf("<strong>%s</strong>", titulo_visuales_t1()$top10)))
  
  build_hist_monto_total <- reactive({
    df <- base_serie_t1() %>%
      group_by(ano) %>%
      summarise(
        monto    = sum(VALOR_CREDITO_REAL, na.rm = TRUE) / 1e9,
        creditos = sum(NUMERO_CREDITO,     na.rm = TRUE),
        .groups  = "drop"
      ) %>% arrange(ano)
    
    if (nrow(df) == 0) return(NULL)
    
    ind <- if (!is.null(input$var_m_t1) && input$var_m_t1 %in% c("monto","creditos")) input$var_m_t1 else "monto"
    
    if (ind == "monto") {
      y_col      <- df$monto
      y_title    <- "Miles de millones (reales)"
      name_tr    <- "Monto total real"
      hover_tmpl <- "<b>Año %{x}</b><br>Monto real: %{y:.2f} Mil M<extra></extra>"
    } else {
      y_col      <- df$creditos
      y_title    <- "Número de créditos"
      name_tr    <- "Número de créditos"
      hover_tmpl <- "<b>Año %{x}</b><br>Créditos: %{y:,}<extra></extra>"
    }
    
    plot_ly(
      x = df$ano, y = y_col,
      type = "scatter", mode = "lines+markers",
      name = name_tr,
      line   = list(color = col_monto_total),
      marker = list(color = col_monto_total),
      hovertemplate = hover_tmpl
    ) %>% layout(
      xaxis  = list(title="", showgrid=FALSE),
      yaxis  = list(title=y_title, showgrid=TRUE),
      legend = list(orientation="h"),
      paper_bgcolor = "#ffffff",
      plot_bgcolor  = "#ffffff"
    )
  })
  output$hist_monto_total <- renderPlotly({ build_hist_monto_total() })
  
  # ---------- MAPA (Tab 1) ----------
  output$mapa_m_t1 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -74.3, lat = 4.6, zoom = 5)
  })
  
  observeEvent(
    list(input$tabs_finagro, input$ano_t1, input$depto_t1,
         input$mpio_t1, input$productor_t1, input$eslabon_t1, input$var_m_t1),
    {
      req(input$tabs_finagro == "Dinámica histórica del crédito agropecuario")
      
      df_raw <- base_mapa_t1()
      if (nrow(df_raw) == 0) {
        leafletProxy("mapa_m_t1") %>% clearShapes() %>% clearControls()
        return()
      }
      
      if (is.null(input$depto_t1) || input$depto_t1 == "Todos") {
        
        df_ag <- df_raw %>%
          group_by(COD_DPTO2) %>%
          summarise(
            monto    = sum(VALOR_CREDITO_REAL, na.rm = TRUE),
            creditos = sum(NUMERO_CREDITO,     na.rm = TRUE),
            .groups  = "drop"
          )
        
        shp <- dptos_sf %>%
          left_join(df_ag, by = "COD_DPTO2") %>%
          sf::st_as_sf()
        
        tipo_mapa <- "depto"
        
      } else {
        
        cod_dep <- dptos_sf$COD_DPTO2[dptos_sf$NOM_DPTO == input$depto_t1][1]
        
        df_ag <- df_raw %>%
          filter(COD_DPTO2 == cod_dep) %>%
          group_by(COD_DPTO2, COD_MUN5) %>%
          summarise(
            monto    = sum(VALOR_CREDITO_REAL, na.rm = TRUE),
            creditos = sum(NUMERO_CREDITO,     na.rm = TRUE),
            .groups  = "drop"
          )
        
        shp <- mpios_sf %>%
          filter(COD_DPTO2 == cod_dep) %>%
          left_join(df_ag, by = c("COD_DPTO2","COD_MUN5")) %>%
          mutate(
            NOM_MPIO_TC = title_case_es(NOM_MPIO),
            monto    = ifelse(is.na(monto),    0, monto),
            creditos = ifelse(is.na(creditos), 0, creditos)
          ) %>%
          sf::st_as_sf()
        
        tipo_mapa <- "mpio"
      }
      
      req(nrow(shp) > 0, !all(sf::st_is_empty(shp)))
      
      ind <- if (!is.null(input$var_m_t1) && input$var_m_t1 %in% c("monto","creditos")) input$var_m_t1 else "monto"
      shp$valor <- if (ind == "monto") shp$monto else shp$creditos
      
      pal_map <- pal_story[2:6]
      pal     <- pal_quartiles_safe(rev(pal_map), shp$valor)
      bb      <- sf::st_bbox(shp)
      
      if (ind == "monto") {
        legend_title <- "Monto total del crédito (real)<br>(Miles de millones)"
        legend_lab   <- labelFormat(transform=function(x) x/1e9, big.mark=".", digits=1, suffix=" Mil M")
      } else {
        legend_title <- "Número de créditos"
        legend_lab   <- labelFormat(big.mark=".", digits=0)
      }
      
      proxy <- leafletProxy("mapa_m_t1") %>% clearShapes() %>% clearControls()
      
      if (tipo_mapa == "depto") {
        proxy <- proxy %>%
          addPolygons(
            data        = shp,
            layerId     = ~COD_DPTO2,
            fillColor   = ~pal(valor),
            color       = COL_BORDE_POLY,
            weight      = 0.8,
            opacity     = 1,
            fillOpacity = 0.8,
            smoothFactor = 0.5,
            label = ~paste0(
              title_case_es(NOM_DPTO),
              "<br>Monto total real: ", fmt_milM(monto),
              "<br>Nº créditos: ", fmt_int(creditos)
            ),
            labelOptions = labelOptions(direction = "auto")
          )
      } else {
        proxy <- proxy %>%
          addPolygons(
            data        = shp,
            layerId     = ~COD_MUN5,
            fillColor   = ~pal(valor),
            color       = COL_BORDE_POLY,
            weight      = 0.6,
            opacity     = 1,
            fillOpacity = 0.7,
            smoothFactor = 0.5,
            label = ~paste0(
              NOM_MPIO_TC,
              "<br>Monto total real: ", fmt_milM(monto),
              "<br>Nº créditos: ", fmt_int(creditos)
            ),
            labelOptions = labelOptions(direction = "auto")
          )
      }
      
      proxy %>%
        addLegend("bottomright", pal=pal, values=shp$valor, title=legend_title, labFormat=legend_lab) %>%
        fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    },
    ignoreInit = FALSE
  )
  
  build_map_t1_export <- reactive({
    df_raw <- base_mapa_t1()
    req(nrow(df_raw) > 0)
    
    if (is.null(input$depto_t1) || input$depto_t1 == "Todos") {
      
      df_ag <- df_raw %>%
        group_by(COD_DPTO2) %>%
        summarise(
          monto    = sum(VALOR_CREDITO_REAL, na.rm = TRUE),
          creditos = sum(NUMERO_CREDITO,     na.rm = TRUE),
          .groups  = "drop"
        )
      
      shp <- dptos_sf %>%
        left_join(df_ag, by = "COD_DPTO2") %>%
        sf::st_as_sf()
      
      tipo_mapa <- "depto"
      
    } else {
      
      cod_dep <- dptos_sf$COD_DPTO2[dptos_sf$NOM_DPTO == input$depto_t1][1]
      
      df_ag <- df_raw %>%
        filter(COD_DPTO2 == cod_dep) %>%
        group_by(COD_DPTO2, COD_MUN5) %>%
        summarise(
          monto    = sum(VALOR_CREDITO_REAL, na.rm = TRUE),
          creditos = sum(NUMERO_CREDITO,     na.rm = TRUE),
          .groups  = "drop"
        )
      
      shp <- mpios_sf %>%
        filter(COD_DPTO2 == cod_dep) %>%
        left_join(df_ag, by = c("COD_DPTO2","COD_MUN5")) %>%
        mutate(
          NOM_MPIO_TC = title_case_es(NOM_MPIO),
          monto    = ifelse(is.na(monto),    0, monto),
          creditos = ifelse(is.na(creditos), 0, creditos)
        ) %>%
        sf::st_as_sf()
      
      tipo_mapa <- "mpio"
    }
    
    ind <- if (!is.null(input$var_m_t1) && input$var_m_t1 %in% c("monto","creditos")) input$var_m_t1 else "monto"
    shp$valor <- if (ind == "monto") shp$monto else shp$creditos
    
    pal_map <- pal_story[2:6]
    pal     <- pal_quartiles_safe(rev(pal_map), shp$valor)
    
    bb <- sf::st_bbox(shp)
    lng <- mean(c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"])))
    lat <- mean(c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"])))
    zoom <- if (tipo_mapa == "depto") 7 else 9
    
    if (ind == "monto") {
      legend_title <- "Monto total del crédito (real)"
    } else {
      legend_title <- "Número de créditos"
    }
    
    m <- leaflet(options = leafletOptions(zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = lng, lat = lat, zoom = zoom)
    
    if (tipo_mapa == "depto") {
      m <- m %>%
        addPolygons(
          data = shp,
          fillColor = ~pal(valor),
          color = COL_BORDE_POLY,
          weight = 0.8,
          fillOpacity = 0.8
        )
    } else {
      m <- m %>%
        addPolygons(
          data = shp,
          fillColor = ~pal(valor),
          color = COL_BORDE_POLY,
          weight = 0.6,
          fillOpacity = 0.7
        )
    }
    
    m %>% addLegend("bottomright", pal = pal, values = shp$valor, title = legend_title)
  })
  
  # --- Top 10 (Tab 1) ---
  build_top5_t1 <- reactive({
    df_raw <- base_mapa_t1()
    if (nrow(df_raw) == 0) return(NULL)
    
    ind <- if (!is.null(input$var_m_t1) && input$var_m_t1 %in% c("monto","creditos")) input$var_m_t1 else "monto"
    
    if (is.null(input$depto_t1) || input$depto_t1 == "Todos") {
      df <- df_raw %>%
        group_by(COD_DPTO2) %>%
        summarise(
          monto    = sum(VALOR_CREDITO_REAL, na.rm = TRUE),
          creditos = sum(NUMERO_CREDITO,     na.rm = TRUE),
          .groups  = "drop"
        ) %>%
        left_join(dptos_sf %>% sf::st_drop_geometry() %>% select(COD_DPTO2, NOM_DPTO),
                  by = "COD_DPTO2") %>%
        mutate(nombre = title_case_es(NOM_DPTO))
    } else {
      cod_dep <- dptos_sf$COD_DPTO2[dptos_sf$NOM_DPTO == input$depto_t1][1]
      df <- df_raw %>%
        filter(COD_DPTO2 == cod_dep) %>%
        group_by(COD_DPTO2, COD_MUN5) %>%
        summarise(
          monto    = sum(VALOR_CREDITO_REAL, na.rm = TRUE),
          creditos = sum(NUMERO_CREDITO,     na.rm = TRUE),
          .groups  = "drop"
        ) %>%
        left_join(mpios_sf %>% sf::st_drop_geometry() %>% select(COD_DPTO2, COD_MUN5, NOM_MPIO),
                  by = c("COD_DPTO2","COD_MUN5")) %>%
        mutate(nombre = title_case_es(NOM_MPIO))
    }
    
    if (nrow(df) == 0) return(NULL)
    
    if (ind == "monto") {
      df      <- df %>% arrange(desc(monto)) %>% slice_head(n = 10)
      y_vals  <- df$monto / 1e9
      labels  <- number(y_vals, big.mark=".", decimal.mark=",", accuracy=0.1)
      y_title <- "Monto total (miles de millones)"
    } else {
      df      <- df %>% arrange(desc(creditos)) %>% slice_head(n = 10)
      y_vals  <- df$creditos
      labels  <- number(y_vals, big.mark=".", decimal.mark=",", accuracy=1)
      y_title <- "Número de créditos"
    }
    
    df$y_val     <- y_vals
    df$label_val <- labels
    
    g <- ggplot(df, aes(x = reorder(nombre, y_val), y = y_val)) +
      geom_col(fill = "#8d6e63") +
      geom_text(aes(y = y_val/2, label = label_val), color = "white", size = 3.7) +
      coord_flip() +
      labs(title = "", x = NULL, y = y_title) +
      scale_y_continuous(labels = label_number(big.mark=".", decimal.mark=","), expand = expansion(mult = c(0,0.1))) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title         = element_text(face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background  = element_rect(fill="white", color=NA),
        panel.background = element_rect(fill="white", color=NA)
      )
    
    ggplotly(g) %>% layout(paper_bgcolor="#ffffff", plot_bgcolor="#ffffff")
  })
  output$top5_m_t1 <- renderPlotly({ build_top5_t1() })
  
  # =========================================================
  # TAB 2 — INDICADORES
  # =========================================================
  base_filtrada <- reactive({
    df <- finagro_fast
    if (!is.null(input$ano)) df <- df %>% filter(ano == as.numeric(input$ano))
    if (!is.null(input$depto_t2) && input$depto_t2 != "Todos") df <- df %>% filter(NOM_DPTO == input$depto_t2)
    if (!is.null(input$mpio_t2)  && input$mpio_t2  != "Todos") df <- df %>% filter(NOM_MPIO == input$mpio_t2)
    if (!is.null(input$eslabon_t2) && input$eslabon_t2 != "Todos") df <- df %>% filter(ESLABON_TC == input$eslabon_t2)
    if (!is.null(input$productor) && input$productor != "Todos") df <- df %>% filter(TIPO_PRODUCTOR == input$productor)
    if (!is.null(input$sexo) && input$sexo != "Todos") df <- df %>% filter(SEXO2 == input$sexo)
    df
  })
  
  build_serie_tiempo <- reactive({
    df <- base_filtrada() %>%
      group_by(mes) %>%
      summarise(
        creditos = sum(NUMERO_CREDITO,      na.rm = TRUE),
        monto    = sum(VALOR_CREDITO_REAL,  na.rm = TRUE) / 1e9,
        .groups  = "drop"
      ) %>%
      mutate(mes_lbl = factor(mes, levels = 1:12, labels = mes_labels))
    
    col_creditos <- "#8d6e63"
    col_monto    <- "#e6b65c"
    meses_tick   <- mes_labels[seq(1, 12, by = 2)]
    
    plot_ly(df, x = ~mes_lbl) %>%
      add_trace(
        y      = ~creditos, name = "Créditos", yaxis = "y1",
        type   = "scatter", mode  = "lines+markers",
        line   = list(width = 2, color = col_creditos),
        marker = list(size  = 6, color = col_creditos),
        hovertemplate = "<b>%{x}</b><br>Créditos: %{y:,}<extra></extra>"
      ) %>%
      add_trace(
        y      = ~monto, name = "Miles de Millones (reales)", yaxis = "y2",
        type   = "scatter", mode  = "lines+markers",
        line   = list(width = 2, dash = "dot", color = col_monto),
        marker = list(size  = 6, color = col_monto),
        hovertemplate = "<b>%{x}</b><br>Monto real: %{y:.1f} Mil M<extra></extra>"
      ) %>%
      layout(
        xaxis = list(
          title         = "",
          type          = "category",
          categoryorder = "array",
          categoryarray = mes_labels,
          tickmode      = "array",
          tickvals      = meses_tick,
          ticktext      = meses_tick,
          tickangle     = 0,
          tickfont      = list(size = 11),
          showgrid      = FALSE
        ),
        yaxis = list(title = "Número de créditos", showgrid = TRUE, zeroline = FALSE),
        yaxis2 = list(title = "Miles de millones (reales)", overlaying = "y", side = "right",
                      showgrid = FALSE, zeroline = FALSE),
        legend = list(orientation = "h", x = 0, y = 1.12),
        margin = list(l = 70, r = 70, b = 60, t = 30),
        paper_bgcolor="#ffffff",
        plot_bgcolor ="#ffffff"
      )
  })
  output$serie_tiempo <- renderPlotly({ build_serie_tiempo() })
  
  build_top_lineas_eslabon <- reactive({
    df <- base_filtrada() %>%
      group_by(LINEA_CREDITO, ESLABON_TC) %>%
      summarise(monto = sum(VALOR_CREDITO_REAL, na.rm = TRUE), .groups = "drop")
    
    top_lines <- df %>%
      group_by(LINEA_CREDITO) %>%
      summarise(monto_total = sum(monto, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(monto_total)) %>%
      slice_head(n = 5) %>%
      pull(LINEA_CREDITO)
    
    df_top <- df %>%
      filter(LINEA_CREDITO %in% top_lines) %>%
      mutate(
        LINEA_TC   = title_case_es(LINEA_CREDITO),
        ESLABON_F  = factor(ESLABON_TC, levels = ESLABON_LEVELS),
        monto_m    = monto / 1e9,
        label_m    = scales::number(monto_m, big.mark=".", decimal.mark=",", accuracy=0.1)
      )
    
    if (nrow(df_top) == 0) return(NULL)
    
    linea_order <- df_top %>%
      group_by(LINEA_TC) %>%
      summarise(monto_total = sum(monto_m, na.rm = TRUE), .groups = "drop") %>%
      arrange(monto_total) %>%
      pull(LINEA_TC)
    
    df_top <- df_top %>% mutate(LINEA_TC = factor(LINEA_TC, levels = linea_order))
    
    plot_ly(
      df_top,
      x      = ~monto_m,
      y      = ~LINEA_TC,
      color  = ~ESLABON_F,
      colors = PAL_ESLAB,
      type   = "bar",
      orientation      = "h",
      text             = ~label_m,
      textposition     = "inside",
      texttemplate     = "%{text}",
      insidetextanchor = "middle",
      hovertemplate    = "<b>%{y}</b><br>Eslabón: %{color}<br>Monto real: %{x:.2f} Mil M<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis   = list(title = "Miles de Millones (reales)", showgrid = FALSE),
        yaxis   = list(title = "", showgrid = FALSE),
        legend  = list(orientation = "h", y = -0.2),
        margin  = list(l = 70, r = 30, b = 40, t = 10),
        paper_bgcolor="#ffffff",
        plot_bgcolor ="#ffffff"
      )
  })
  output$top_lineas_eslabon <- renderPlotly({ build_top_lineas_eslabon() })
  
  build_sankey <- reactive({
    df_links <- base_filtrada() %>%
      group_by(LINEA_CREDITO, ESLABON_TC) %>%
      summarise(value = sum(VALOR_CREDITO_REAL, na.rm = TRUE) / 1e9, .groups = "drop") %>%
      mutate(
        LINEA_TC   = title_case_es(LINEA_CREDITO),
        ESLABON_F  = factor(ESLABON_TC, levels = ESLABON_LEVELS)
      )
    
    if (nrow(df_links) == 0) return(NULL)
    
    nodos <- data.frame(
      name = c(unique(df_links$LINEA_TC), ESLABON_LEVELS),
      stringsAsFactors = FALSE
    )
    nodos$group <- ifelse(nodos$name %in% ESLABON_LEVELS, nodos$name, "Linea")
    
    df_links$source <- match(df_links$LINEA_TC, nodos$name) - 1
    df_links$target <- match(as.character(df_links$ESLABON_F), nodos$name) - 1
    df_links$group  <- as.character(df_links$ESLABON_F)
    
    domain_vec <- c("Linea", ESLABON_LEVELS)
    range_vec  <- c("#e5e7eb", PAL_ESLAB[ESLABON_LEVELS])
    
    colourScale <- paste0(
      'd3.scaleOrdinal().domain(["',
      paste(domain_vec, collapse = '","'),
      '"]).range(["',
      paste(range_vec, collapse = '","'),
      '"])'
    )
    
    sankeyNetwork(
      Links       = df_links,
      Nodes       = nodos,
      Source      = "source",
      Target      = "target",
      Value       = "value",
      NodeID      = "name",
      NodeGroup   = "group",
      LinkGroup   = "group",
      units       = "Miles de Millones (reales)",
      fontSize    = 10,
      nodeWidth   = 30,
      colourScale = colourScale
    )
  })
  output$sankey <- renderSankeyNetwork({ build_sankey() })
  
  # =========================================================
  # TABLA EXPORTABLE CSV
  # =========================================================
  tabla_export <- reactive({
    bind_rows(
      base_t1() %>%
        mutate(pestana = "Tab 1"),
      base_filtrada() %>%
        mutate(pestana = "Tab 2")
    ) %>%
      transmute(
        pestana,
        ano,
        mes,
        departamento = NOM_DPTO,
        municipio = NOM_MPIO,
        eslabon = ESLABON_TC,
        tipo_productor = TIPO_PRODUCTOR,
        sexo = SEXO2,
        linea_credito = LINEA_CREDITO,
        numero_credito = NUMERO_CREDITO,
        valor_credito = VALOR_CREDITO,
        valor_credito_real = VALOR_CREDITO_REAL
      )
  })
  
  # =========================================================
  # DESCARGAS PNG
  # =========================================================
  output$dl_png_t1_mapa <- downloadHandler(
    filename = function(){
      paste0("finagro_tab1_mapa_", Sys.Date(), ".png")
    },
    content = function(file){
      dly <- if (!is.null(input$depto_t1) && input$depto_t1 != "Todos") PNG_DELAY_MUN else PNG_DELAY_CO
      ok <- save_widget_png_retry(build_map_t1_export(), file, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay_base = dly)
      if (!ok) stop("No se pudo generar el PNG del mapa de la pestaña 1.")
    }
  )
  
  output$dl_png_t1_serie <- downloadHandler(
    filename = function(){
      paste0("finagro_tab1_serie_", Sys.Date(), ".png")
    },
    content = function(file){
      ok <- save_widget_png_retry(build_hist_monto_total(), file, vwidth = 1800, vheight = 900, delay_base = 0.9)
      if (!ok) stop("No se pudo generar el PNG de la serie de la pestaña 1.")
    }
  )
  
  output$dl_png_t1_top <- downloadHandler(
    filename = function(){
      paste0("finagro_tab1_top10_", Sys.Date(), ".png")
    },
    content = function(file){
      ok <- save_widget_png_retry(build_top5_t1(), file, vwidth = 1800, vheight = 1000, delay_base = 0.9)
      if (!ok) stop("No se pudo generar el PNG del top 10 de la pestaña 1.")
    }
  )
  
  output$dl_png_t2_serie <- downloadHandler(
    filename = function(){
      paste0("finagro_tab2_serie_", Sys.Date(), ".png")
    },
    content = function(file){
      ok <- save_widget_png_retry(build_serie_tiempo(), file, vwidth = 1800, vheight = 900, delay_base = 0.9)
      if (!ok) stop("No se pudo generar el PNG de la serie de la pestaña 2.")
    }
  )
  
  output$dl_png_t2_top <- downloadHandler(
    filename = function(){
      paste0("finagro_tab2_top_lineas_", Sys.Date(), ".png")
    },
    content = function(file){
      ok <- save_widget_png_retry(build_top_lineas_eslabon(), file, vwidth = 1800, vheight = 1000, delay_base = 0.9)
      if (!ok) stop("No se pudo generar el PNG del top de líneas de la pestaña 2.")
    }
  )
  
  output$dl_png_t2_sankey <- downloadHandler(
    filename = function(){
      paste0("finagro_tab2_sankey_", Sys.Date(), ".png")
    },
    content = function(file){
      ok <- tryCatch(
        save_widget_png_retry(build_sankey(), file, vwidth = 1800, vheight = 1000, delay_base = 1.2),
        error = function(e) FALSE
      )
      if (!isTRUE(ok)) {
        ok2 <- save_sankey_png_fallback(file, width = 1800, height = 1000)
        if (!ok2) stop("No se pudo generar el PNG del sankey de la pestaña 2.")
      }
    }
  )
  
  # =========================================================
  # CSV
  # =========================================================
  output$dl_csv_expl <- downloadHandler(
    filename = function(){
      paste0("FINAGRO_base_filtrada_", Sys.Date(), ".csv")
    },
    content = function(file){
      readr::write_csv(tabla_export(), file, na = "")
    }
  )
  
  # =========================================================
  # PDF ROBUSTO TIPO ICA
  # =========================================================
  output$dl_reporte_pdf <- downloadHandler(
    filename = function(){
      paste0("Informe_descargable_FINAGRO_", Sys.Date(), ".pdf")
    },
    content = function(file){
      
      if (!ya_vio_ambas_tabs()) {
        stop("Para descargar el informe debes haber visualizado las dos pestañas de la app.")
      }
      
      if (!file.exists(ruta_rmd)) {
        stop("No encuentro Informe_descargable.Rmd en la raíz del proyecto.")
      }
      
      # snapshot filtros tab 1
      ano_t1_now       <- input$ano_t1 %||% "Todos"
      depto_t1_now     <- input$depto_t1 %||% "Todos"
      mpio_t1_now      <- input$mpio_t1 %||% "Todos"
      eslabon_t1_now   <- input$eslabon_t1 %||% "Todos"
      productor_t1_now <- input$productor_t1 %||% "Todos"
      var_t1_now       <- input$var_m_t1 %||% "monto"
      
      # snapshot filtros tab 2
      ano_t2_now       <- input$ano %||% NA
      depto_t2_now     <- input$depto_t2 %||% "Todos"
      mpio_t2_now      <- input$mpio_t2 %||% "Todos"
      eslabon_t2_now   <- input$eslabon_t2 %||% "Todos"
      productor_t2_now <- input$productor %||% "Todos"
      sexo_t2_now      <- input$sexo %||% "Todos"
      
      # 1) generar PNGs fijos en ./Descargas
      dly_map <- if (!is.null(depto_t1_now) && depto_t1_now != "Todos") PNG_DELAY_MUN else PNG_DELAY_CO
      
      ok_t1_map <- save_widget_png_retry(build_map_t1_export(), IMG_T1_MAP, vwidth = PNG_VWIDTH, vheight = PNG_VHEIGHT, delay_base = dly_map)
      ok_t1_ser <- save_widget_png_retry(build_hist_monto_total(), IMG_T1_SER, vwidth = 1800, vheight = 900, delay_base = 0.9)
      ok_t1_top <- save_widget_png_retry(build_top5_t1(),         IMG_T1_TOP, vwidth = 1800, vheight = 1000, delay_base = 0.9)
      
      ok_t2_ser <- save_widget_png_retry(build_serie_tiempo(),       IMG_T2_SER, vwidth = 1800, vheight = 900,  delay_base = 0.9)
      ok_t2_top <- save_widget_png_retry(build_top_lineas_eslabon(), IMG_T2_TOP, vwidth = 1800, vheight = 1000, delay_base = 0.9)
      
      ok_t2_san <- tryCatch(
        save_widget_png_retry(build_sankey(), IMG_T2_SAN, vwidth = 1800, vheight = 1000, delay_base = 1.2),
        error = function(e) FALSE
      )
      if (!isTRUE(ok_t2_san)) {
        ok_t2_san <- save_sankey_png_fallback(IMG_T2_SAN, width = 1800, height = 1000)
      }
      
      if (!ok_t1_map) stop("No se pudo generar Descargas/finagro_tab1_mapa.png para el informe.")
      if (!ok_t1_ser) stop("No se pudo generar Descargas/finagro_tab1_serie.png para el informe.")
      if (!ok_t1_top) stop("No se pudo generar Descargas/finagro_tab1_top10.png para el informe.")
      if (!ok_t2_ser) stop("No se pudo generar Descargas/finagro_tab2_serie.png para el informe.")
      if (!ok_t2_top) stop("No se pudo generar Descargas/finagro_tab2_toplineas.png para el informe.")
      if (!ok_t2_san) stop("No se pudo generar Descargas/finagro_tab2_sankey.png para el informe.")
      
      # 2) filtros del informe
      filtros_tbl <- data.frame(
        Parametro = c(
          "Pestaña 1 - Año",
          "Pestaña 1 - Departamento",
          "Pestaña 1 - Municipio",
          "Pestaña 1 - Eslabón",
          "Pestaña 1 - Tipo de productor",
          "Pestaña 1 - Indicador",
          "Pestaña 2 - Año",
          "Pestaña 2 - Departamento",
          "Pestaña 2 - Municipio",
          "Pestaña 2 - Eslabón",
          "Pestaña 2 - Tipo de productor",
          "Pestaña 2 - Sexo",
          "Pestañas visualizadas"
        ),
        Valor = c(
          as.character(ano_t1_now),
          as.character(depto_t1_now),
          as.character(mpio_t1_now),
          as.character(eslabon_t1_now),
          as.character(productor_t1_now),
          as.character(var_t1_now),
          as.character(ano_t2_now),
          as.character(depto_t2_now),
          as.character(mpio_t2_now),
          as.character(eslabon_t2_now),
          as.character(productor_t2_now),
          as.character(sexo_t2_now),
          "Sí: pestaña 1 y pestaña 2"
        ),
        stringsAsFactors = FALSE
      )
      
      # 3) logo
      logo_src <- file.path(app_root, "www", "LOGO_PLATEA.png")
      if (!file.exists(logo_src)) {
        logo_src2 <- file.path(app_root, "WWW", "LOGO_PLATEA.png")
        logo_src  <- if (file.exists(logo_src2)) logo_src2 else NA_character_
      }
      logo_dst <- file.path(EXPORT_DIR, "LOGO_PLATEA.png")
      if (!is.na(logo_src) && file.exists(logo_src)) file.copy(logo_src, logo_dst, overwrite = TRUE)
      logo_tex <- gsub("\\\\", "/", normalizePath(logo_dst, winslash = "/", mustWork = FALSE))
      
      # 4) placeholder logo en Rmd si existe
      td <- tempfile("rmd_finagro_")
      dir.create(td, recursive = TRUE, showWarnings = FALSE)
      
      rmd_to_render <- ruta_rmd
      rmd_lines <- readLines(ruta_rmd, warn = FALSE, encoding = "UTF-8")
      if (any(grepl("__LOGO_PLATEA_PATH__", rmd_lines, fixed = TRUE))) {
        rmd_tmp <- file.path(td, "Informe_descargable_FINAGRO_render.Rmd")
        rmd_lines <- gsub("__LOGO_PLATEA_PATH__", logo_tex, rmd_lines, fixed = TRUE)
        writeLines(rmd_lines, rmd_tmp, useBytes = TRUE)
        rmd_to_render <- rmd_tmp
      }
      
      # 5) render directo al archivo de Shiny
      rmarkdown::render(
        input         = rmd_to_render,
        output_format = "pdf_document",
        output_file   = basename(file),
        output_dir    = dirname(file),
        quiet         = TRUE,
        params        = list(
          app_root        = app_root,
          export_dir      = "Descargas",
          filtros         = filtros_tbl,
          
          img_tab1_mapa   = basename(IMG_T1_MAP),
          img_tab1_serie  = basename(IMG_T1_SER),
          img_tab1_top10  = basename(IMG_T1_TOP),
          
          img_tab2_serie  = basename(IMG_T2_SER),
          img_tab2_lineas = basename(IMG_T2_TOP),
          img_tab2_sankey = basename(IMG_T2_SAN),
          
          csv_filtrado    = NULL
        ),
        knit_root_dir = app_root,
        envir         = new.env(parent = globalenv())
      )
    },
    contentType = "application/pdf"
  )
}

# =========================================================
# RUN
# =========================================================
shinyApp(ui, server)
