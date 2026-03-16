# =========================================================
# app_sisben_dashboard_v2.R — DNP_SISBEN (CORREGIDO FINAL)
# + Botón CSV tipo EVA
# + Botón PDF descargable al navegador
# + Generación de PNG previos para Informe_descargable.Rmd
# + Render en carpeta temporal limpia
# + Copia del Rmd + PNG al mismo render_dir
# =========================================================

suppressWarnings({
  library(shiny); library(dplyr); library(ggplot2); library(plotly)
  library(scales); library(bslib); library(htmltools); library(stringi)
  library(tidyr); library(ragg)
  library(rmarkdown); library(knitr); library(bsicons)
})

options(stringsAsFactors = FALSE, scipen = 999)

# =========================================================
# Helpers globales
# =========================================================
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

fmt_comma <- function(x) scales::comma(x, big.mark = ".", decimal.mark = ",")
pc1       <- function(x) paste0(format(round(x, 1), nsmall = 1, decimal.mark = ","), "%")

sanitize_filename <- function(x){
  x <- as.character(x)
  x <- gsub("[/\\\\:*?\"<>|]", "_", x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("__+", "_", x)
  trimws(x)
}

plot_vacio_gg <- function(txt = "Sin datos para la selección actual") {
  ggplot() +
    annotate("text", x = 1, y = 1, label = txt, size = 5) +
    xlim(0, 2) + ylim(0, 2) +
    theme_void()
}

get_app_root <- function(){
  normalizePath(shiny::getShinyOption("appDir") %||% getwd(), winslash = "/", mustWork = FALSE)
}

save_gg_png <- function(plot_obj, out_png, width = 1800, height = 1100, res = 150){
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  ragg::agg_png(out_png, width = width, height = height, res = res)
  print(plot_obj)
  grDevices::dev.off()
  file.exists(out_png) && is.finite(file.info(out_png)$size) && file.info(out_png)$size > 0
}

# ---------- Title Case ES ----------
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

make_tc_choices <- function(vec, include_all = FALSE){
  v    <- sort(unique(vec))
  labs <- title_case_es(v)
  ch   <- stats::setNames(v, labs)
  if (include_all) c("Todos" = "Todos", ch) else ch
}

fmt_short_hog <- function(x){
  ifelse(
    is.na(x), NA_character_,
    ifelse(
      abs(x) >= 1e6,
      paste0(format(round(x/1e6, 1), nsmall = 1, decimal.mark = ","), "M"),
      ifelse(
        abs(x) >= 1e3,
        paste0(format(round(x/1e3, 1), nsmall = 1, decimal.mark = ","), "K"),
        fmt_comma(x)
      )
    )
  )
}

# =========================================================
# Rutas app / exportación
# =========================================================
app_root      <- get_app_root()
EXPORT_DIR    <- file.path(app_root, "Descargas")
dir.create(EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)

ruta_rmd_root <- file.path(app_root, "Informe_descargable.Rmd")
ruta_rmd_data <- file.path(app_root, "data", "Informe_descargable.Rmd")
ruta_rmd      <- if (file.exists(ruta_rmd_root)) ruta_rmd_root else ruta_rmd_data

IMG_GRP   <- file.path(EXPORT_DIR, "sisben_grupos.png")
IMG_PRIV  <- file.path(EXPORT_DIR, "sisben_privaciones.png")
IMG_POB   <- file.path(EXPORT_DIR, "sisben_pobreza_ab.png")

# =========================================================
# Datos
# =========================================================
data_dir    <- "data"
sisben_path <- file.path(data_dir, "031_DNP_SISBEN.rds")
sisben      <- readRDS(sisben_path)

sisben <- sisben %>% dplyr::filter(DEPARTAMENTO_D == "ATLÁNTICO")

stopifnot("Nw_hogares" %in% names(sisben))

# ---------- Etiquetas para i1–i15 ----------
i_labels <- c(
  i1  = "Bajo logro educativo",
  i2  = "Analfabetismo",
  i3  = "Inasistencia escolar",
  i4  = "Rezago escolar",
  i5  = "Barreras a cuidado primera infancia",
  i6  = "Trabajo infantil",
  i7  = "Desempleo de larga duración",
  i8  = "Trabajo informal",
  i9  = "Sin aseguramiento en salud",
  i10 = "Barreras acceso a salud",
  i11 = "Sin fuente de agua mejorada",
  i12 = "Eliminación inadecuada de excretas",
  i13 = "Pisos inadecuados",
  i14 = "Paredes exteriores inadecuadas",
  i15 = "Hacinamiento crítico"
)
priv_cols <- intersect(names(sisben), paste0("i", 1:15))

GRP_COLS <- c(A = "#8e44ad", B = "#009edb", C = "#007a3d", D = "#f57c00")

github_url <- "https://github.com/tu_usuario/tu_repo"

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
      :root{
        --bdr:#f57c00;
        --txt-main:#111827;
      }

      .wrap{
        max-width:1360px;
        margin:0 auto;
        padding:16px 20px 24px;
      }
      h3{
        font-weight:700;
        letter-spacing:.2px;
        margin-bottom:8px;
      }
      .data-note{
        font-size:13px;
        color:#6b7280;
        margin:0 0 16px;
      }

      .filters,
      .card{
        background:#fff;
        border:2px solid var(--bdr) !important;
        border-radius:16px;
        padding:14px 16px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
      }

      .filters{ margin-bottom:16px; }
      .section-row{ margin-bottom:16px; }
      .card{ margin-bottom:0; }

      .card-plot{
        min-height: 430px;
        display:flex;
        flex-direction:column;
      }
      .card-plot .html-widget,
      .card-plot .plotly{
        flex:1 1 auto;
      }

      .filters-grid{
        display:grid;
        grid-template-columns:repeat(4,minmax(180px,1fr));
        gap:12px;
      }

      .filter-label{
        font-size:14px;
        font-weight:500;
        letter-spacing:.3px;
        color:var(--txt-main);
        margin-bottom:6px;
      }

      .card-title{
        font-weight:700;
        font-size:16px;
        margin-bottom:8px;
        color:#111827;
      }
      .kpi{
        font-weight:800;
        font-size:28px;
        color:#111827;
      }
      .kpi-sub{
        font-size:12px;
        color:#6b7280;
        margin-top:-4px;
      }

      .kpi-row{
        display:flex;
        gap:24px;
      }
      .kpi-row > .col-kpi{
        flex:1 1 0;
      }
      .kpi-row .card{
        height:100%;
      }

      .card-kpi{
        display:flex;
        flex-direction:column;
        justify-content:space-between;
        min-height:120px;
      }

      .card-row{
        display:flex;
        gap:24px;
      }
      .card-row > .col-card{
        flex:1 1 0;
      }
      .card-row > .col-card-full{
        flex:1 1 0;
      }

      .dl-under{
        margin-top:8px;
        text-align:right;
      }
      .dl-footer{
        margin-top:14px;
        text-align:right;
      }

      .btn, .btn-default {
        font-size:12px;
        padding:6px 10px;
        border-radius:999px;
      }
      .btn + .btn { margin-left:6px; }

      .dl-under .btn,
      .dl-footer .btn{
        border:2px solid var(--bdr) !important;
        color:var(--txt-main) !important;
        background:#ffffff !important;
        box-shadow:none !important;
      }
      .dl-under .btn:hover,
      .dl-footer .btn:hover{
        background:#fff7ec !important;
        color:#111827 !important;
      }

      .filters .selectize-control.single .selectize-input{
        border-radius:999px !important;
        border:2px solid var(--bdr) !important;
        box-shadow:none !important;
        background-color:#ffffff !important;
        padding-top:6px;
        padding-bottom:6px;
        font-size:0.95rem;
      }

      .filters .selectize-control.single .selectize-input.focus,
      .filters .selectize-control.single .selectize-input.input-active{
        border-radius:999px !important;
        border:2px solid var(--bdr) !important;
        box-shadow:0 0 0 2px rgba(245,124,0,.35) !important;
        background-color:#ffffff !important;
      }

      .filters .selectize-control.single .selectize-input.dropdown-active{
        border-bottom-left-radius:0 !important;
        border-bottom-right-radius:0 !important;
      }

      .filters .selectize-dropdown .option{
        font-size:0.95rem;
      }

      @media (max-width: 992px){
        .filters-grid{
          grid-template-columns:repeat(2,minmax(0,1fr));
        }
        .kpi-row,
        .card-row{
          flex-direction:column;
        }
      }
    "))
  ),
  
  div(
    class = "wrap",
    h3(""),
    div(class="data-note",""),
    
    div(
      class="filters",
      div(
        class="filters-grid",
        div(
          class="filter",
          div(class="filter-label","¿Qué año analizamos?"),
          selectInput(
            "f_ano", NULL,
            choices  = sort(unique(sisben$ano)),
            selected = max(sisben$ano)
          )
        ),
        div(
          class="filter",
          div(class="filter-label","¿En qué departamento?"),
          selectInput(
            "f_dep", NULL,
            choices  = make_tc_choices(sisben$DEPARTAMENTO_D, include_all = TRUE),
            selected = "ATLÁNTICO"
          )
        ),
        div(
          class="filter",
          div(class="filter-label","¿Algún municipio en particular?"),
          selectInput(
            "f_mun", NULL,
            choices  = make_tc_choices(sisben$MUNICIPIO_D, include_all = TRUE),
            selected = "Todos"
          )
        ),
        div(
          class="filter",
          div(class="filter-label","¿Quieres ver algún grupo de Sisbén IV?"),
          selectInput(
            "f_grupo", NULL,
            choices  = c("Todos", sort(unique(sisben$grupo))),
            selected = "Todos"
          )
        )
      )
    ),
    
    div(
      class = "section-row",
      div(
        class="kpi-row",
        div(
          class="col-kpi",
          div(
            class="card card-kpi",
            div(class="card-title","Hogares totales"),
            div(class="kpi", textOutput("kpi_hog")),
            div(class="kpi-sub","")
          )
        ),
        div(
          class="col-kpi",
          div(
            class="card card-kpi",
            div(class="card-title","% hogares en condición de pobreza"),
            div(class="kpi", textOutput("kpi_ab")),
            div(class="kpi-sub","Hogares en A o B sobre total")
          )
        ),
        div(
          class="col-kpi",
          div(
            class="card card-kpi",
            div(class="card-title","% hogares con más de una privación del IPM"),
            div(class="kpi", textOutput("kpi_anypriv")),
            div(class="kpi-sub","")
          )
        ),
        div(
          class="col-kpi",
          div(
            class="card card-kpi",
            div(class="card-title","Cantidad promedio de privaciones por hogar"),
            div(class="kpi", textOutput("kpi_prompriv")),
            div(class="kpi-sub","")
          )
        )
      )
    ),
    
    div(
      class = "section-row",
      div(
        class = "card-row",
        div(
          class = "col-card",
          div(
            class = "card card-plot",
            uiOutput("ttl_grupos"),
            plotlyOutput("plot_grupos", height = 360),
            div(
              class = "dl-under",
              downloadButton("dl_png_grupos","PNG — Distribución grupos")
            )
          )
        ),
        div(
          class = "col-card",
          div(
            class = "card card-plot",
            div(class="card-title","¿Qué privaciones del IPM son más frecuentes en el territorio?"),
            plotlyOutput("plot_priv_top", height = 360),
            div(
              class = "dl-under",
              downloadButton("dl_png_priv","PNG — Top-10 privaciones")
            )
          )
        )
      )
    ),
    
    div(
      class = "section-row",
      div(
        class = "card-row",
        div(
          class = "col-card-full",
          div(
            class="card card-plot",
            div(class="card-title","¿Cómo ha evolucionado el porcentaje de hogares en condición de pobreza entre 2021 y 2024?"),
            plotlyOutput("plot_pobreza_hist", height = 360),
            div(
              class="dl-under",
              downloadButton("dl_png_pobreza","PNG — Evolución % A+B")
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        div(
          class = "dl-footer",
          downloadButton("dl_csv_sisben", label = "Descargar CSV"),
          downloadButton("dl_reporte_pdf", label = "Descargar informe (PDF)"),
          tags$a(
            href   = github_url,
            target = "_blank",
            class  = "btn btn-dark",
            style  = "color:white;",
            list(bsicons::bs_icon("github"), " GitHub")
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
  
  observeEvent(input$f_dep, {
    if (is.null(input$f_dep) || input$f_dep == "Todos"){
      ch <- make_tc_choices(sisben$MUNICIPIO_D, include_all = TRUE)
      updateSelectInput(session, "f_mun", choices = ch, selected = "Todos")
    } else {
      ch_raw <- sisben %>%
        filter(DEPARTAMENTO_D == input$f_dep) %>%
        distinct(MUNICIPIO_D) %>%
        arrange(MUNICIPIO_D) %>%
        pull(MUNICIPIO_D)
      ch <- make_tc_choices(ch_raw, include_all = TRUE)
      updateSelectInput(session, "f_mun", choices = ch, selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  base_filtrada <- reactive({
    df <- sisben %>% filter(ano == input$f_ano)
    if (input$f_dep   != "Todos") df <- df %>% filter(DEPARTAMENTO_D == input$f_dep)
    if (input$f_mun   != "Todos") df <- df %>% filter(MUNICIPIO_D == input$f_mun)
    if (input$f_grupo != "Todos") df <- df %>% filter(grupo == input$f_grupo)
    df
  }) |> bindCache(input$f_ano, input$f_dep, input$f_mun, input$f_grupo)
  
  output$ttl_grupos <- renderUI({
    HTML('<div class="card-title">¿Cómo se distribuye la población según el grupo de Sisbén IV?</div>')
  })
  
  grupos_data <- reactive({
    base_filtrada() %>%
      mutate(grupo = factor(grupo, levels = c("A","B","C","D"))) %>%
      group_by(grupo) %>%
      summarise(hogares = sum(Nw_hogares, na.rm = TRUE), .groups = "drop") %>%
      tidyr::complete(
        grupo = factor(c("A","B","C","D"), levels = c("A","B","C","D")),
        fill = list(hogares = 0)
      )
  })
  
  priv_top_data <- reactive({
    df <- base_filtrada()
    if (length(priv_cols) == 0 || nrow(df) == 0) return(NULL)
    w      <- df$Nw_hogares
    m_bin  <- as.matrix(df[, priv_cols, drop = FALSE] > 0)
    tot_w  <- sum(w, na.rm = TRUE)
    if (is.na(tot_w) || tot_w == 0) return(NULL)
    prev_vec <- colSums(m_bin * w, na.rm = TRUE) / tot_w
    data.frame(var = names(prev_vec), prev = as.numeric(prev_vec)) %>%
      mutate(label = ifelse(var %in% names(i_labels), i_labels[var], var)) %>%
      arrange(desc(prev)) %>%
      slice_head(n = 10)
  })
  
  pobreza_hist_data <- reactive({
    df <- sisben
    if (input$f_dep != "Todos") df <- df %>% filter(DEPARTAMENTO_D == input$f_dep)
    if (input$f_mun != "Todos") df <- df %>% filter(MUNICIPIO_D == input$f_mun)
    
    df %>%
      group_by(ano) %>%
      summarise(
        total_h = sum(Nw_hogares, na.rm = TRUE),
        ab_h    = sum(Nw_hogares[grupo %in% c("A","B")], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(pct_ab = if_else(total_h > 0, 100 * ab_h / total_h, NA_real_)) %>%
      arrange(ano)
  })
  
  tabla_exportable <- reactive({
    df <- base_filtrada()
    if (nrow(df) == 0) return(data.frame())
    
    df %>%
      mutate(
        Departamento = title_case_es(DEPARTAMENTO_D),
        Municipio    = title_case_es(MUNICIPIO_D)
      ) %>%
      select(
        ano, Departamento, Municipio, grupo, Nw_hogares,
        any_of(priv_cols)
      ) %>%
      rename(
        Año = ano,
        `Grupo Sisbén` = grupo,
        Hogares = Nw_hogares
      )
  })
  
  # ---------- KPIs ----------
  output$kpi_hog <- renderText({
    fmt_comma(sum(base_filtrada()$Nw_hogares, na.rm = TRUE))
  })
  
  output$kpi_ab <- renderText({
    df  <- base_filtrada()
    tot <- sum(df$Nw_hogares, na.rm = TRUE)
    ab  <- sum(df$Nw_hogares[df$grupo %in% c("A","B")], na.rm = TRUE)
    pc1(if (tot > 0) 100 * ab / tot else 0)
  })
  
  output$kpi_anypriv <- renderText({
    df <- base_filtrada()
    if (length(priv_cols) == 0 || nrow(df) == 0) return("0%")
    w     <- df$Nw_hogares
    any_h <- sum(w * as.integer(rowSums(df[, priv_cols] > 0, na.rm = TRUE) > 0), na.rm = TRUE)
    tot_w <- sum(w, na.rm = TRUE)
    pc1(if (tot_w > 0) 100 * any_h / tot_w else 0)
  })
  
  output$kpi_prompriv <- renderText({
    df <- base_filtrada()
    if (length(priv_cols) == 0 || nrow(df) == 0) return("0")
    w     <- df$Nw_hogares
    denom <- sum(w, na.rm = TRUE)
    if (is.na(denom) || denom <= 0) return("0")
    npriv <- rowSums(df[, priv_cols] > 0, na.rm = TRUE)
    prom  <- sum(npriv * w, na.rm = TRUE) / denom
    format(round(prom, 1), decimal.mark = ",")
  })
  
  # ---------- Gráficos plotly ----------
  output$plot_grupos <- renderPlotly({
    df <- grupos_data()
    if (is.null(df)) return(NULL)
    
    p <- ggplot(
      df,
      aes(x = grupo, y = hogares, fill = grupo,
          text = paste0("Grupo: ", grupo, "<br>Hogares: ", fmt_comma(hogares)))
    ) +
      geom_col(width = 0.75) +
      geom_text(
        aes(
          label = fmt_short_hog(hogares),
          y     = hogares / 2
        ),
        color = "white",
        size = 3.3,
        fontface = "bold"
      ) +
      scale_fill_manual(values = GRP_COLS, breaks = c("A","B","C","D")) +
      scale_y_continuous(labels = fmt_short_hog) +
      labs(x = NULL, y = "Hogares", fill = "Grupo") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_line(color = "#e5e7eb"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_priv_top <- renderPlotly({
    prev <- priv_top_data()
    if (is.null(prev)) return(NULL)
    
    p <- ggplot(
      prev,
      aes(x = prev, y = reorder(label, prev),
          text = paste0(
            label, "<br>Porcentaje de hogares: ",
            scales::percent(prev, accuracy = 0.1, decimal.mark = ",")
          ))
    ) +
      geom_col(fill = "#9d4b01") +
      geom_text(
        aes(
          label = scales::percent(prev, accuracy = 0.1, decimal.mark = ","),
          x     = prev / 2
        ),
        color = "white",
        size  = 3.1,
        fontface = "bold"
      ) +
      scale_x_continuous(
        labels = scales::percent_format(accuracy = 1, decimal.mark = ",")
      ) +
      labs(x = "Porcentaje de hogares", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.x = element_line(color = "#e5e7eb"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank()
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_pobreza_hist <- renderPlotly({
    serie <- pobreza_hist_data() %>%
      dplyr::filter(!is.na(ano), !is.na(pct_ab))
    
    req(nrow(serie) > 0)
    
    plot_ly(
      data = serie,
      x    = ~ano,
      y    = ~pct_ab,
      type = "scatter",
      mode = "lines+markers",
      line   = list(width = 2, color = "#9d4b01"),
      marker = list(size = 7,  color = "#9d4b01"),
      hovertemplate = "<b>Año %{x}</b><br>% A+B: %{y:.1f}%<extra></extra>"
    ) |>
      layout(
        xaxis = list(
          title    = "",
          tickmode = "linear",
          dtick    = 1,
          showgrid = FALSE
        ),
        yaxis = list(
          title      = "% hogares",
          ticksuffix = "%",
          rangemode  = "tozero",
          showgrid   = TRUE,
          gridcolor  = "rgba(229,231,235,1)",
          gridwidth  = 1
        ),
        margin    = list(l = 60, r = 20, t = 30, b = 40),
        hovermode = "x unified"
      )
  })
  
  # ---------- Versiones ggplot para exportar PNG/PDF ----------
  g_grupos_gg <- reactive({
    df <- grupos_data()
    if (is.null(df) || nrow(df) == 0) return(plot_vacio_gg())
    
    ggplot(df, aes(x = grupo, y = hogares, fill = grupo)) +
      geom_col(width = 0.75) +
      geom_text(
        aes(
          label = fmt_short_hog(hogares),
          y     = hogares / 2
        ),
        color = "white",
        size  = 3.3,
        fontface = "bold"
      ) +
      scale_fill_manual(values = GRP_COLS, breaks = c("A","B","C","D")) +
      scale_y_continuous(labels = fmt_short_hog) +
      labs(x = NULL, y = "Hogares", fill = "Grupo", title = "Distribución por grupo de Sisbén IV") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_line(color = "#e5e7eb"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank()
      )
  })
  
  g_priv_gg <- reactive({
    prev <- priv_top_data()
    if (is.null(prev) || nrow(prev) == 0) return(plot_vacio_gg())
    
    ggplot(prev, aes(x = prev, y = reorder(label, prev))) +
      geom_col(fill = "#8e44ad") +
      geom_text(
        aes(
          label = scales::percent(prev, accuracy = 0.1, decimal.mark = ","),
          x     = prev / 2
        ),
        color = "white",
        size  = 3.1,
        fontface = "bold"
      ) +
      scale_x_continuous(
        labels = scales::percent_format(accuracy = 1, decimal.mark = ",")
      ) +
      labs(x = "Prevalencia (hogares)", y = NULL, title = "Top-10 privaciones del IPM") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.x = element_line(color = "#e5e7eb"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank()
      )
  })
  
  g_pobreza_gg <- reactive({
    serie <- pobreza_hist_data() %>%
      dplyr::filter(!is.na(ano), !is.na(pct_ab))
    
    if (nrow(serie) == 0) return(plot_vacio_gg())
    
    ggplot(serie, aes(x = ano, y = pct_ab)) +
      geom_line(color = "#8e44ad", linewidth = 1) +
      geom_point(color = "#8e44ad", size = 2.5) +
      labs(
        x = "Año",
        y = "% hogares en pobreza (A+B)",
        title = "Evolución del % de hogares en pobreza"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_line(color = "#e5e7eb"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank()
      )
  })
  
  # ---------- Descargas PNG ----------
  output$dl_png_grupos <- downloadHandler(
    filename = function(){
      paste0("SISBEN_grupos_", input$f_ano, "_", Sys.Date(), ".png")
    },
    content = function(file){
      ok <- save_gg_png(g_grupos_gg(), file, width = 1600, height = 1000, res = 150)
      if (!isTRUE(ok)) stop("No se pudo exportar el gráfico de grupos.")
    }
  )
  
  output$dl_png_priv <- downloadHandler(
    filename = function(){
      paste0("SISBEN_top_privaciones_", input$f_ano, "_", Sys.Date(), ".png")
    },
    content = function(file){
      ok <- save_gg_png(g_priv_gg(), file, width = 1600, height = 1000, res = 150)
      if (!isTRUE(ok)) stop("No se pudo exportar el gráfico de privaciones.")
    }
  )
  
  output$dl_png_pobreza <- downloadHandler(
    filename = function(){
      paste0("SISBEN_pobreza_AB_", input$f_ano, "_", Sys.Date(), ".png")
    },
    content = function(file){
      ok <- save_gg_png(g_pobreza_gg(), file, width = 1600, height = 900, res = 150)
      if (!isTRUE(ok)) stop("No se pudo exportar el gráfico de pobreza.")
    }
  )
  
  # ---------- CSV ----------
  output$dl_csv_sisben <- downloadHandler(
    filename = function() {
      paste0("sisben_tabla_detalle_", Sys.Date(), ".csv")
    },
    content = function(file) {
      utils::write.csv(tabla_exportable(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # ---------- Filtros para informe ----------
  filtros_informe <- reactive({
    data.frame(
      Parametro = c("Año", "Departamento", "Municipio", "Grupo"),
      Valor = c(
        input$f_ano %||% "",
        ifelse(is.null(input$f_dep), "", ifelse(input$f_dep == "Todos", "Todos", title_case_es(input$f_dep))),
        ifelse(is.null(input$f_mun), "", ifelse(input$f_mun == "Todos", "Todos", title_case_es(input$f_mun))),
        input$f_grupo %||% ""
      ),
      stringsAsFactors = FALSE
    )
  })
  
  # ======================================================
  # PDF: render en carpeta temporal y entrega al navegador
  # ======================================================
  render_informe_pdf <- function(file) {
    
    log_file <- file.path(tempdir(), "debug_pdf_sisben.txt")
    
    write_log <- function(...) {
      cat(..., "\n", file = log_file, append = TRUE)
    }
    
    write_log("====================================")
    write_log("Inicio render PDF:", as.character(Sys.time()))
    write_log("ruta_rmd original:", ruta_rmd)
    write_log("archivo destino downloadHandler:", file)
    
    if (!file.exists(ruta_rmd)) {
      stop("No encuentro Informe_descargable.Rmd en la raíz del proyecto ni en data/.")
    }
    
    # 1) Generar PNG previos en Descargas
    ok1 <- save_gg_png(g_grupos_gg(),  IMG_GRP,  width = 1800, height = 1000, res = 150)
    ok2 <- save_gg_png(g_priv_gg(),    IMG_PRIV, width = 1800, height = 1100, res = 150)
    ok3 <- save_gg_png(g_pobreza_gg(), IMG_POB,  width = 1800, height = 1000, res = 150)
    
    if (!isTRUE(ok1)) stop("No se pudo generar sisben_grupos.png")
    if (!isTRUE(ok2)) stop("No se pudo generar sisben_privaciones.png")
    if (!isTRUE(ok3)) stop("No se pudo generar sisben_pobreza_ab.png")
    
    # 2) Carpeta temporal limpia
    render_dir <- file.path(
      tempdir(),
      paste0("render_sisben_", Sys.getpid(), "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    )
    dir.create(render_dir, recursive = TRUE, showWarnings = FALSE)
    
    temp_rmd <- file.path(render_dir, "Informe_descargable.Rmd")
    out_pdf  <- file.path(render_dir, "Informe_SISBEN_final.pdf")
    
    # 3) Copiar Rmd a temporal
    ok_rmd <- file.copy(ruta_rmd, temp_rmd, overwrite = TRUE)
    if (!isTRUE(ok_rmd) || !file.exists(temp_rmd)) {
      stop("No se pudo copiar el archivo Rmd a la carpeta temporal de render.")
    }
    
    # 4) Copiar imágenes al mismo directorio temporal
    img_grp_local  <- file.path(render_dir, basename(IMG_GRP))
    img_priv_local <- file.path(render_dir, basename(IMG_PRIV))
    img_pob_local  <- file.path(render_dir, basename(IMG_POB))
    
    ok_img1 <- file.copy(IMG_GRP,  img_grp_local,  overwrite = TRUE)
    ok_img2 <- file.copy(IMG_PRIV, img_priv_local, overwrite = TRUE)
    ok_img3 <- file.copy(IMG_POB,  img_pob_local,  overwrite = TRUE)
    
    if (!isTRUE(ok_img1) || !file.exists(img_grp_local)) {
      stop("No se pudo copiar sisben_grupos.png al directorio temporal.")
    }
    if (!isTRUE(ok_img2) || !file.exists(img_priv_local)) {
      stop("No se pudo copiar sisben_privaciones.png al directorio temporal.")
    }
    if (!isTRUE(ok_img3) || !file.exists(img_pob_local)) {
      stop("No se pudo copiar sisben_pobreza_ab.png al directorio temporal.")
    }
    
    filtros_tbl <- filtros_informe()
    
    write_log("render_dir:", render_dir)
    write_log("temp_rmd:", temp_rmd)
    write_log("out_pdf:", out_pdf)
    write_log("img_grp_local:", img_grp_local)
    write_log("img_priv_local:", img_priv_local)
    write_log("img_pob_local:", img_pob_local)
    
    # 5) Render del Rmd dentro del render_dir
    res <- tryCatch({
      rmarkdown::render(
        input             = temp_rmd,
        output_format     = "pdf_document",
        output_file       = basename(out_pdf),
        output_dir        = dirname(out_pdf),
        intermediates_dir = render_dir,
        knit_root_dir     = render_dir,
        clean             = TRUE,
        envir             = new.env(parent = globalenv()),
        quiet             = TRUE,
        params = list(
          app_root    = ".",
          export_dir  = ".",
          filtros     = filtros_tbl,
          img_grupos  = basename(img_grp_local),
          img_priv    = basename(img_priv_local),
          img_pobreza = basename(img_pob_local),
          ano         = input$f_ano,
          dep         = input$f_dep,
          mun         = input$f_mun,
          grupo       = input$f_grupo
        )
      )
    }, error = function(e) {
      write_log("ERROR EN RENDER:", conditionMessage(e))
      stop(e)
    })
    
    write_log("render devolvió:", res)
    
    pdf_final <- if (file.exists(res)) res else out_pdf
    
    if (!file.exists(pdf_final)) {
      stop("El PDF no se generó correctamente.")
    }
    
    pdf_info <- file.info(pdf_final)
    if (is.na(pdf_info$size) || pdf_info$size <= 0) {
      stop("El PDF generado está vacío.")
    }
    
    # 6) Entregar al navegador
    ok_copy <- file.copy(pdf_final, file, overwrite = TRUE)
    if (!isTRUE(ok_copy) || !file.exists(file)) {
      stop("No se pudo transferir el PDF al archivo de descarga del navegador.")
    }
    
    file_info <- file.info(file)
    if (is.na(file_info$size) || file_info$size <= 0) {
      stop("El archivo descargable quedó vacío.")
    }
    
    write_log("PDF entregado al navegador:", file)
    write_log("FIN render PDF:", as.character(Sys.time()))
  }
  
  # ---------- Botón PDF ----------
  output$dl_reporte_pdf <- downloadHandler(
    filename = function() {
      ano_tag <- input$f_ano %||% format(Sys.Date(), "%Y")
      dep_tag <- if (is.null(input$f_dep) || input$f_dep == "Todos") {
        "Todos"
      } else {
        sanitize_filename(title_case_es(input$f_dep))
      }
      mun_tag <- if (is.null(input$f_mun) || input$f_mun == "Todos") {
        "Todos"
      } else {
        sanitize_filename(title_case_es(input$f_mun))
      }
      grp_tag <- input$f_grupo %||% "Todos"
      
      paste0(
        "Informe_SISBEN_",
        dep_tag, "_", mun_tag, "_", grp_tag, "_",
        ano_tag, "_", Sys.Date(), ".pdf"
      )
    },
    content = function(file) {
      tryCatch(
        render_informe_pdf(file),
        error = function(e) {
          showNotification(
            paste("Error al generar PDF:", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
          stop(e)
        }
      )
    },
    contentType = "application/pdf"
  )
}

# =========================================================
# RUN
# =========================================================
shinyApp(ui, server)