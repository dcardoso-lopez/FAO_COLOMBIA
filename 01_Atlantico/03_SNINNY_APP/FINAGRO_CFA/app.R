# =========================================================
# app_finagro_moderno.R — Tendencias + Indicadores (FIX + Orden eslabón)
# - Ajustado a tu estructura real: COD_DANE_DPTO_D / COD_DANE_MUNIC_D
# - Crea llaves estándar: COD_DPTO2, COD_MUN5, NOM_DPTO, NOM_MPIO, SEXO2
# - Tab 2 usa SUS inputs (ano, depto_t2, mpio_t2, eslabon_t2, productor, sexo)
# - Municipios con selectize server-side (mejor performance)
# - FIX MAPA: cuando es municipal, SOLO dibuja municipios del departamento seleccionado
# - NUEVO: Orden fijo de eslabón: Producción, Transformación, Comercialización, Servicios de Apoyo
# =========================================================

suppressWarnings({
  library(shiny); library(dplyr); library(plotly)
  library(scales); library(ggplot2); library(networkD3)
  library(sf); library(leaflet); library(bslib); library(stringr)
})

options(stringsAsFactors = FALSE, scipen = 999)
options(shiny.maxRequestSize = 100*1024^2)
sf::sf_use_s2(FALSE)

# ---------- Rutas ----------
data_dir <- "data"

# ---------- Lectura ----------
finagro_fast       <- readRDS(file.path(data_dir, "081_FINAGRO_CFA.rds"))
finagro_depto_map  <- readRDS(file.path(data_dir, "map_finagro_depto.rds"))
finagro_mpio_map   <- readRDS(file.path(data_dir, "map_finagro_mpio.rds"))
mpios_sf           <- readRDS(file.path(data_dir, "mpios_sf_simpl.rds"))
dptos_sf           <- readRDS(file.path(data_dir, "dptos_sf_simpl.rds"))

# =========================================================
# HELPERS
# =========================================================

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

# ✅ ORDEN FIJO ESLABÓN (lo que pediste)
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
# NORMALIZACIÓN FINAGRO (según TU estructura real)
# =========================================================
finagro_fast <- finagro_fast %>%
  mutate(
    COD_DPTO2 = stringr::str_pad(as.character(COD_DANE_DPTO_D), 2, pad = "0"),
    COD_MUN5  = stringr::str_pad(as.character(COD_DANE_MUNIC_D), 5, pad = "0"),
    NOM_DPTO  = as.character(DEPARTAMENTO_D),
    NOM_MPIO  = as.character(MUNICIPIO_D),
    SEXO2     = as.character(SEXO),
    # eslabón normalizado para filtros/plots
    ESLABON_TC = normalize_eslabon(ESLABON_CADENA)
  )

# (Opcional) dejar solo Atlántico
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

# ---------- Choices (limitadas a lo que existe) ----------
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
    ", COL_BORDE_POLY, COL_BORDE_POLY, COL_BORDE_POLY, COL_BORDE_POLY)))
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
                # ✅ orden fijo
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
                uiOutput("titulo_mapa_t1"),
                leafletOutput("mapa_m_t1", height = 758)
            )
          ),
          column(
            width = 6,
            div(class="card",
                uiOutput("titulo_serie_t1"),
                plotlyOutput("hist_monto_total", height = 330)
            ),
            div(class="card",
                uiOutput("titulo_top10_t1"),
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
                # ✅ orden fijo
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
                     div(class="card-title","Evolución mensual de créditos y monto real desembolsado"),
                     plotlyOutput("serie_tiempo", height = 390)
                 )
          ),
          column(6,
                 div(class="card",
                     div(class="card-title","Concentración de las principales líneas de crédito por eslabón (reales)"),
                     plotlyOutput("top_lineas_eslabon", height = 390)
                 )
          )
        ),
        fluidRow(
          column(12,
                 div(class="card",
                     div(class="card-title","Mapa de asignación: líneas de crédito → eslabones productivos (montos reales)"),
                     sankeyNetworkOutput("sankey", height = "370px")
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
  
  # colores eslabón (orden fijo)
  PAL_ESLAB <- c(
    "Producción"         = "#8d6e63",
    "Transformación"     = "#a17236",
    "Comercialización"   = "#c49000",
    "Servicios de Apoyo" = "#cbd5e1"
  )
  
  # =========================================================
  # MUNICIPIOS DEPENDEN DE DEPTO (server-side selectize)
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
    # ✅ filtro por eslabón usando normalizado
    if (!is.null(input$eslabon_t1) && input$eslabon_t1 != "Todos") df <- df %>% filter(ESLABON_TC == input$eslabon_t1)
    if (!is.null(input$productor_t1) && input$productor_t1 != "Todos") df <- df %>% filter(TIPO_PRODUCTOR == input$productor_t1)
    df
  })
  
  base_serie_t1 <- reactive({
    df <- finagro_fast
    if (!is.null(input$depto_t1) && input$depto_t1 != "Todos") df <- df %>% filter(NOM_DPTO == input$depto_t1)
    if (!is.null(input$mpio_t1) && input$mpio_t1 != "Todos")   df <- df %>% filter(NOM_MPIO == input$mpio_t1)
    # ✅
    if (!is.null(input$eslabon_t1) && input$eslabon_t1 != "Todos") df <- df %>% filter(ESLABON_TC == input$eslabon_t1)
    if (!is.null(input$productor_t1) && input$productor_t1 != "Todos") df <- df %>% filter(TIPO_PRODUCTOR == input$productor_t1)
    df
  })
  
  base_mapa_t1 <- reactive({
    df <- finagro_fast
    if (!is.null(input$ano_t1) && input$ano_t1 != "Todos") df <- df %>% filter(ano == as.numeric(input$ano_t1))
    if (!is.null(input$depto_t1) && input$depto_t1 != "Todos") df <- df %>% filter(NOM_DPTO == input$depto_t1)
    if (!is.null(input$mpio_t1) && input$mpio_t1 != "Todos")   df <- df %>% filter(NOM_MPIO == input$mpio_t1)
    # ✅
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
  
  output$titulo_mapa_t1  <- renderUI(tags$div(class="card-title", titulo_visuales_t1()$mapa))
  output$titulo_serie_t1 <- renderUI(tags$div(class="card-title", titulo_visuales_t1()$serie))
  output$titulo_top10_t1 <- renderUI(tags$div(class="card-title", titulo_visuales_t1()$top10))
  
  output$hist_monto_total <- renderPlotly({
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
      legend = list(orientation="h")
    )
  })
  
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
        
        # ✅ FIX: solo municipios del dpto
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
  
  # --- Top 10 (Tab 1) ---
  output$top5_m_t1 <- renderPlotly({
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
        panel.grid.minor.x = element_blank()
      )
    
    ggplotly(g)
  })
  
  # =========================================================
  # TAB 2 — INDICADORES
  # =========================================================
  base_filtrada <- reactive({
    df <- finagro_fast
    if (!is.null(input$ano)) df <- df %>% filter(ano == as.numeric(input$ano))
    if (!is.null(input$depto_t2) && input$depto_t2 != "Todos") df <- df %>% filter(NOM_DPTO == input$depto_t2)
    if (!is.null(input$mpio_t2)  && input$mpio_t2  != "Todos") df <- df %>% filter(NOM_MPIO == input$mpio_t2)
    # ✅ filtro por eslabón usando normalizado
    if (!is.null(input$eslabon_t2) && input$eslabon_t2 != "Todos") df <- df %>% filter(ESLABON_TC == input$eslabon_t2)
    if (!is.null(input$productor) && input$productor != "Todos") df <- df %>% filter(TIPO_PRODUCTOR == input$productor)
    if (!is.null(input$sexo) && input$sexo != "Todos") df <- df %>% filter(SEXO2 == input$sexo)
    df
  })
  
  output$serie_tiempo <- renderPlotly({
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
        margin = list(l = 70, r = 70, b = 60, t = 30)
      )
  })
  
  output$top_lineas_eslabon <- renderPlotly({
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
        margin  = list(l = 70, r = 30, b = 40, t = 10)
      )
  })
  
  output$sankey <- renderSankeyNetwork({
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
}

# =========================================================
# RUN
# =========================================================
shinyApp(ui, server)

