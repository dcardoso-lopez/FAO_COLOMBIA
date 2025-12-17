# =========================================================
# app_sipsa_abast_serie_simple.R
# SIPSA Abastecimiento (RDS)
# - SOLO filtros: año, mes, COD_DANE_MUNIC_O, COD_DANE_MUNIC_D
# - ÚNICO visual: serie temporal de CantKg_total
# =========================================================

suppressWarnings({
  library(shiny); library(dplyr); library(tidyr); library(stringi)
  library(ggplot2); library(plotly); library(scales); library(bslib)
  library(htmltools); library(ragg); library(lubridate)
})

options(stringsAsFactors = FALSE, scipen = 999)

# =========================================================
# CONFIG
# =========================================================
DEFAULT_RDS <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/041_DANE_SIPSA-Abast"

# =========================================================
# HELPERS
# =========================================================

fmt_comma <- function(x) scales::comma(x, big.mark = ".", decimal.mark = ",")

title_case_es <- function(x){
  stopw <- c("de","del","la","las","los","y","e","o","u","en","a","al","por","para",
             "con","sin","sobre","entre","hasta","desde","contra","ante","tras",
             "que","el","su","un","una","unos","unas")
  vapply(x, function(s){
    if (is.na(s) || !nzchar(s)) return(s)
    s <- tolower(trimws(as.character(s)))
    toks <- strsplit(s, "\\s+", perl = TRUE)[[1]]
    out  <- character(length(toks))
    for (i in seq_along(toks)){
      base <- toks[i]
      if (i == 1 || !(base %in% stopw)) out[i] <- stringi::stri_trans_totitle(base, locale = "es")
      else out[i] <- base
    }
    paste(out, collapse = " ")
  }, character(1))
}

make_choices_simple <- function(vec, include_all = TRUE){
  v <- sort(unique(vec))
  v <- v[!is.na(v)]
  if (include_all) c("Todos" = "Todos", setNames(v, v)) else setNames(v, v)
}

# Códigos municipales: fuerza a string (y si es numérico, pad a 5)
fmt_code5 <- function(x){
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) x <- as.integer(x)
  x <- as.character(x)
  x <- ifelse(is.na(x), NA_character_, x)
  x <- stringi::stri_trim_both(x)
  suppressWarnings({
    x_num <- as.integer(x)
    x <- ifelse(!is.na(x_num) & nchar(x) < 5, sprintf("%05d", x_num), x)
  })
  x
}

resolve_rds_path <- function(p){
  p <- trimws(p)
  if (!nzchar(p)) stop("Ruta vacía.")
  
  if (file.exists(p) && !dir.exists(p) && grepl("\\.rds$", tolower(p))) return(p)
  
  if (file.exists(p) && !dir.exists(p) && !grepl("\\.rds$", tolower(p))){
    p2 <- paste0(p, ".rds")
    if (file.exists(p2)) return(p2)
  }
  
  if (dir.exists(p)){
    cand <- list.files(p, pattern = "\\.rds$", full.names = TRUE)
    if (length(cand) == 0) stop("No encontré archivos .rds dentro de: ", p)
    pref <- cand[grepl("041_DANE_SIPSA\\-Abast", basename(cand), ignore.case = TRUE)]
    if (length(pref) > 0) return(pref[1])
    return(cand[1])
  }
  
  p2 <- paste0(p, ".rds")
  if (file.exists(p2)) return(p2)
  
  stop("No existe la ruta o no pude resolver el .rds: ", p)
}

# Serie temporal: suma CantKg_total por (ano, mes) después de filtros
build_serie <- function(df, f_ano, f_mes, f_o, f_d){
  need <- c("ano","mes","COD_DANE_MUNIC_O","COD_DANE_MUNIC_D","CantKg_total")
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) stop("Faltan columnas: ", paste(miss, collapse = ", "))
  
  df <- df %>%
    mutate(
      ano = as.integer(ano),
      mes = as.integer(mes),
      COD_DANE_MUNIC_O = fmt_code5(COD_DANE_MUNIC_O),
      COD_DANE_MUNIC_D = fmt_code5(COD_DANE_MUNIC_D),
      CantKg_total = suppressWarnings(as.numeric(CantKg_total))
    )
  
  if (!is.null(f_ano) && f_ano != "Todos") df <- df %>% filter(ano == as.integer(f_ano))
  if (!is.null(f_mes) && f_mes != "Todos") df <- df %>% filter(mes == as.integer(f_mes))
  if (!is.null(f_o)   && f_o   != "Todos") df <- df %>% filter(COD_DANE_MUNIC_O == f_o)
  if (!is.null(f_d)   && f_d   != "Todos") df <- df %>% filter(COD_DANE_MUNIC_D == f_d)
  
  if (nrow(df) == 0){
    return(data.frame(fecha = as.Date(character()), cant = numeric()))
  }
  
  out <- df %>%
    group_by(ano, mes) %>%
    summarise(cant = sum(CantKg_total, na.rm = TRUE), .groups = "drop") %>%
    mutate(fecha = as.Date(sprintf("%04d-%02d-01", ano, mes))) %>%
    arrange(fecha) %>%
    select(fecha, cant)
  
  out
}

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  theme = bs_theme(
    version      = 5,
    primary      = "#2563eb",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter Tight"),
    "border-radius"  = "0.9rem",
    "font-size-base" = "0.95rem"
  ),
  
  tags$head(
    tags$style(HTML("
      :root{ --bdr:#f57c00; --txt-main:#111827; }

      .wrap{ max-width:1360px; margin:0 auto; padding:16px 20px 24px; }
      h3{ font-weight:700; letter-spacing:.2px; margin-bottom:8px; }
      .data-note{ font-size:13px; color:#6b7280; margin:0 0 16px; }

      .filters, .card{
        background:#fff;
        border:2px solid var(--bdr) !important;
        border-radius:16px;
        padding:14px 16px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
      }
      .filters{ margin-bottom:16px; }
      .section-row{ margin-bottom:16px; }

      .filters-grid{
        display:grid;
        grid-template-columns:repeat(4,minmax(180px,1fr));
        gap:12px;
      }
      .filter-label{
        font-size:14px; font-weight:500; letter-spacing:.3px;
        color:var(--txt-main); margin-bottom:6px;
      }

      .card-title{ font-weight:700; font-size:16px; margin-bottom:8px; color:#111827; }

      .card-plot{
        min-height: 430px;
        display:flex;
        flex-direction:column;
      }
      .card-plot .html-widget, .card-plot .plotly{ flex:1 1 auto; }

      .dl-under{ margin-top:8px; text-align:right; }
      .dl-under .btn{
        border:2px solid var(--bdr) !important;
        color:var(--txt-main) !important;
        background:#ffffff !important;
        border-radius:999px;
        padding:4px 10px;
        font-size:0.80rem;
        font-weight:500;
        box-shadow:none !important;
      }
      .dl-under .btn:hover{ background:#fff7ec !important; color:#111827 !important; }

      .filters .selectize-control.single .selectize-input{
        border-radius:999px !important;
        border:2px solid var(--bdr) !important;
        box-shadow:none !important;
        background-color:#ffffff !important;
        padding-top:6px; padding-bottom:6px;
        font-size:0.95rem;
      }
      .filters .selectize-control.single .selectize-input.focus,
      .filters .selectize-control.single .selectize-input.input-active{
        border-radius:999px !important;
        border:2px solid var(--bdr) !important;
        box-shadow:0 0 0 2px rgba(245,124,0,.35) !important;
        background-color:#ffffff !important;
      }

      @media (max-width: 992px){
        .filters-grid{ grid-template-columns:repeat(2,minmax(0,1fr)); }
      }
    "))
  ),
  
  div(
    class="wrap",
    h3("SIPSA Abastecimiento — Serie temporal (CantKg_total)"),
    div(class="data-note","Solo filtros: año, mes, COD_DANE_MUNIC_O y COD_DANE_MUNIC_D. El gráfico muestra CantKg_total agregado por mes."),
    
    div(
      class="filters",
      div(
        class="filters-grid",
        div(
          class="filter",
          div(class="filter-label","Ruta RDS (archivo o carpeta)"),
          textInput("rds_path", NULL, value = DEFAULT_RDS)
        ),
        div(
          class="filter",
          div(class="filter-label","Año"),
          selectInput("f_ano", NULL, choices = c("Todos"), selected = "Todos")
        ),
        div(
          class="filter",
          div(class="filter-label","Mes"),
          selectInput("f_mes", NULL, choices = c("Todos"), selected = "Todos")
        ),
        div(
          class="filter",
          div(class="filter-label","COD_DANE_MUNIC_O"),
          selectInput("f_o", NULL, choices = c("Todos"), selected = "Todos")
        )
      ),
      div(style="height:10px;"),
      div(
        class="filters-grid",
        div(
          class="filter",
          div(class="filter-label","COD_DANE_MUNIC_D"),
          selectInput("f_d", NULL, choices = c("Todos"), selected = "Todos")
        ),
        div(
          class="filter",
          div(class="filter-label","Actualizar"),
          actionButton("btn_refresh", "Actualizar serie", class = "btn btn-primary", style="width:100%;")
        ),
        div(class="filter", div(class="filter-label",""), div()),
        div(class="filter", div(class="filter-label",""), div())
      )
    ),
    
    div(
      class="section-row",
      div(
        class="card card-plot",
        div(class="card-title","Serie mensual de CantKg_total"),
        plotlyOutput("plot_series", height = 380),
        div(class="dl-under", downloadButton("dl_png_series","PNG — Serie CantKg_total"))
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  
  data_raw <- reactiveVal(NULL)
  
  # Cargar RDS y poblar choices
  observeEvent(input$rds_path, {
    p <- tryCatch(resolve_rds_path(input$rds_path), error = function(e) NULL)
    if (is.null(p)) return()
    
    df <- tryCatch(readRDS(p), error = function(e) NULL)
    if (is.null(df) || !is.data.frame(df)) return()
    
    # Normaliza tipos mínimos para choices
    if (!all(c("ano","mes","COD_DANE_MUNIC_O","COD_DANE_MUNIC_D") %in% names(df))) return()
    
    df <- df %>%
      mutate(
        ano = as.integer(ano),
        mes = as.integer(mes),
        COD_DANE_MUNIC_O = fmt_code5(COD_DANE_MUNIC_O),
        COD_DANE_MUNIC_D = fmt_code5(COD_DANE_MUNIC_D)
      )
    
    data_raw(df)
    
    updateSelectInput(session, "f_ano", choices = c("Todos"="Todos", sort(unique(df$ano))), selected = "Todos")
    updateSelectInput(session, "f_mes", choices = c("Todos"="Todos", sort(unique(df$mes))), selected = "Todos")
    updateSelectInput(session, "f_o",   choices = make_choices_simple(df$COD_DANE_MUNIC_O, include_all = TRUE), selected = "Todos")
    updateSelectInput(session, "f_d",   choices = make_choices_simple(df$COD_DANE_MUNIC_D, include_all = TRUE), selected = "Todos")
  }, ignoreInit = FALSE)
  
  serie_data <- eventReactive(input$btn_refresh, {
    df <- data_raw()
    req(is.data.frame(df))
    
    build_serie(
      df  = df,
      f_ano = input$f_ano,
      f_mes = input$f_mes,
      f_o   = input$f_o,
      f_d   = input$f_d
    )
  }, ignoreInit = FALSE)
  
  output$plot_series <- renderPlotly({
    S <- serie_data()
    if (is.null(S) || nrow(S) == 0) return(NULL)
    
    plot_ly(
      data = S,
      x    = ~fecha,
      y    = ~cant,
      type = "scatter",
      mode = "lines+markers",
      hovertemplate = "<b>%{x|%Y-%m}</b><br>Cantidad (kg): %{y:,}<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = "", showgrid = FALSE),
        yaxis = list(title = "CantKg_total (kg)", showgrid = TRUE,
                     gridcolor = "rgba(229,231,235,1)", gridwidth = 1),
        margin = list(l = 80, r = 20, t = 10, b = 45),
        hovermode = "x unified"
      )
  })
  
  output$dl_png_series <- downloadHandler(
    filename = function(){ paste0("SIPSA_serie_CantKg_total_", Sys.Date(), ".png") },
    content = function(file){
      S <- serie_data()
      if (is.null(S) || nrow(S) == 0) { file.create(file); return() }
      
      g <- ggplot(S, aes(x = fecha, y = cant)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        labs(x = NULL, y = "CantKg_total (kg)") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.major.y = element_line(color = "#e5e7eb"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank()
        )
      
      ggsave(file, g, device = ragg::agg_png, width = 9, height = 5, dpi = 200, units = "in")
    }
  )
}

shinyApp(ui, server)


