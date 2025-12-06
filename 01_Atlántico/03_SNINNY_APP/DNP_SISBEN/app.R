# =========================================================
# app_sisben_dashboard_v2.R — Sisbén (Hogares con Nw_hogares e i's 0 o Nw)
# =========================================================
suppressWarnings({
  library(shiny); library(dplyr); library(ggplot2); library(plotly)
  library(scales); library(bslib); library(htmltools); library(stringi)
  library(tidyr);  library(ragg)
})

options(stringsAsFactors = FALSE, scipen = 999)

# ---------- Ruta y carga ----------
data_dir    <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/DNP_SISBEN/data"
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

# Paleta fija por grupo A–D
GRP_COLS <- c(A = "#8e44ad", B = "#009edb", C = "#007a3d", D = "#f57c00")

fmt_comma <- function(x) scales::comma(x, big.mark = ".", decimal.mark = ",")
pc1       <- function(x) paste0(format(round(x, 1), nsmall = 1, decimal.mark = ","), "%")

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

      /* Tarjetas, filtros y valueboxes con borde naranja */
      .filters,
      .card{
        background:#fff;
        border:2px solid var(--bdr) !important;
        border-radius:16px;
        padding:14px 16px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
      }
      
            .filter-label{
        font-family: 'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-size:14px;
        font-weight:500;        /* medium */
        letter-spacing:.4px;
        color:#000000;          /* negro puro */
        margin-bottom:6px;
      }


      /* Margen inferior UNIFORME entre bloques */
      .filters{
        margin-bottom:16px;
      }
      .section-row{
        margin-bottom:16px;
      }
      .card{
        margin-bottom:0;
      }

      /* Tarjetas de gráficos: misma altura visual */
      .card-plot{
        min-height: 430px;
        display:flex;
        flex-direction:column;
      }
      .card-plot .html-widget,
      .card-plot .plotly{
        flex:1 1 auto;
      }

      /* Grid filtros */
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

      /* KPIs en una sola fila y misma altura (más espacio entre cajas) */
      .kpi-row{
        display:flex;
        gap:24px;  /* antes 12px */
      }
      .kpi-row > .col-kpi{
        flex:1 1 0;
      }
      .kpi-row .card{
        height:100%;
      }

      /* Contenedor específico para KPI: misma altura y espaciado interno */
      .card-kpi{
        display:flex;
        flex-direction:column;
        justify-content:space-between;
        min-height:120px;
      }

      /* Filas de tarjetas (gráficas) alineadas con KPIs (mismo gap) */
      .card-row{
        display:flex;
        gap:24px;  /* antes 12px */
      }
      .card-row > .col-card{
        flex:1 1 0;
      }
      .card-row > .col-card-full{
        flex:1 1 0;
      }

      /* Botones descarga PNG */
      .dl-under{
        margin-top:8px;
        text-align:right;
      }
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
      .dl-under .btn:hover{
        background:#fff7ec !important;
        color:#111827 !important;
      }

      /* Filtros selectize: borde NARANJA + fuente más grande */
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
    h3("Sisbén — Explorador de privaciones (Hogares)"),
    div(class="data-note","Indicadores a nivel de hogar ponderados por Nw_hogares."),
    
    # ---------- Filtros ----------
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
    
    # ---------- KPIs en una fila ----------
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
    
    # ---------- Visuales (mismo ancho que KPIs) ----------
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
    )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  
  # --- Dependencia municipio–departamento ---
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
  
  # --- Base filtrada ---
  base_filtrada <- reactive({
    df <- sisben %>% filter(ano == input$f_ano)
    if (input$f_dep  != "Todos") df <- df %>% filter(DEPARTAMENTO_D == input$f_dep)
    if (input$f_mun  != "Todos") df <- df %>% filter(MUNICIPIO_D   == input$f_mun)
    if (input$f_grupo!= "Todos") df <- df %>% filter(grupo == input$f_grupo)
    df
  }) |> bindCache(input$f_ano, input$f_dep, input$f_mun, input$f_grupo)
  
  ambito_sel <- reactive({
    dep <- input$f_dep; mun <- input$f_mun; ano <- input$f_ano
    ambito_txt <- if (is.null(dep) || dep == "Todos") {
      "Colombia"
    } else if (!is.null(mun) && mun != "Todos") {
      paste0(title_case_es(mun), ", ", title_case_es(dep))
    } else {
      title_case_es(dep)
    }
    list(ambito = ambito_txt, ano = ano)
  })
  
  output$ttl_grupos <- renderUI({
    info <- ambito_sel()
    HTML(sprintf(
      '<div class="card-title">¿Cómo se distribuye la población según el grupo de Sisbén IV? <span style="color:#6b7280;font-weight:600;"></span></div>',
      htmlEscape(info$ambito),
      htmlEscape(info$ano)
    ))
  })
  
  grupos_data <- reactive({
    base_filtrada() %>%
      mutate(grupo = factor(grupo, levels = c("A","B","C","D"))) %>%
      group_by(grupo) %>%
      summarise(hogares = sum(Nw_hogares, na.rm = TRUE), .groups = "drop") %>%
      tidyr::complete(
        grupo = factor(c("A","B","C","D"), levels = c("A","B","C","D")),
        fill   = list(hogares = 0)
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
    if (input$f_dep  != "Todos") df <- df %>% filter(DEPARTAMENTO_D == input$f_dep)
    if (input$f_mun  != "Todos") df <- df %>% filter(MUNICIPIO_D   == input$f_mun)
    df %>%
      group_by(ano) %>%
      summarise(
        total_h = sum(Nw_hogares, na.rm = TRUE),
        ab_h    = sum(Nw_hogares[grupo %in% c("A","B")], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(pct_ab = if_else(total_h > 0, 100 * ab_h / total_h, 0)) %>%
      arrange(ano)
  })
  
  # ================== KPIs ==================
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
    w       <- df$Nw_hogares
    any_h   <- sum(w * as.integer(rowSums(df[, priv_cols] > 0, na.rm = TRUE) > 0), na.rm = TRUE)
    tot_w   <- sum(w, na.rm = TRUE)
    pc1(if (tot_w > 0) 100 * any_h / tot_w else 0)
  })
  output$kpi_prompriv <- renderText({
    df <- base_filtrada()
    if (length(priv_cols) == 0 || nrow(df) == 0) return("0")
    w     <- df$Nw_hogares
    npriv <- rowSums(df[, priv_cols] > 0, na.rm = TRUE)
    prom  <- sum(npriv * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
    format(round(prom, 1), decimal.mark = ",")
  })
  
  # ================== Gráficos ==================
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
        size  = 3.3,
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
        legend.title = element_text(size = 11),
        legend.text  = element_text(size = 10),
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
            scales::percent(prev, accuracy = 0.1, decimal.mark=",")
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
        labels = scales::percent_format(accuracy = 1, decimal.mark=",")
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
    serie <- pobreza_hist_data()
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
          title   = "",
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
  
  # ================== Descargas PNG ==================
  output$dl_png_grupos <- downloadHandler(
    filename = function(){
      paste0("SISBEN_grupos_", input$f_ano, "_", Sys.Date(), ".png")
    },
    content = function(file){
      df <- grupos_data()
      if (is.null(df)) { file.create(file); return() }
      g <- ggplot(df, aes(x = grupo, y = hogares, fill = grupo)) +
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
        labs(x = NULL, y = "Hogares", fill = "Grupo") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.major.y = element_line(color = "#e5e7eb"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          legend.title = element_text(size = 11),
          legend.text  = element_text(size = 10)
        )
      ggsave(file, g, device = ragg::agg_png,
             width = 8, height = 5, dpi = 200, units = "in")
    }
  )
  
  output$dl_png_priv <- downloadHandler(
    filename = function(){
      paste0("SISBEN_top_privaciones_", input$f_ano, "_", Sys.Date(), ".png")
    },
    content = function(file){
      prev <- priv_top_data()
      if (is.null(prev)) { file.create(file); return() }
      g <- ggplot(prev, aes(x = prev, y = reorder(label, prev))) +
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
          labels = scales::percent_format(accuracy = 1, decimal.mark=",")
        ) +
        labs(x = "Prevalencia (hogares)", y = NULL) +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.major.x = element_line(color = "#e5e7eb"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank()
        )
      ggsave(file, g, device = ragg::agg_png,
             width = 8, height = 5, dpi = 200, units = "in")
    }
  )
  
  output$dl_png_pobreza <- downloadHandler(
    filename = function(){
      paste0("SISBEN_pobreza_AB_", input$f_ano, "_", Sys.Date(), ".png")
    },
    content = function(file){
      serie <- pobreza_hist_data()
      if (nrow(serie) == 0) { file.create(file); return() }
      g <- ggplot(serie, aes(x = ano, y = pct_ab)) +
        geom_line(color = "#8e44ad", linewidth = 1) +
        geom_point(color = "#8e44ad", size = 2.5) +
        labs(x = "Año", y = "% hogares en pobreza (A+B)") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.major.y = element_line(color = "#e5e7eb"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank()
        )
      ggsave(file, g, device = ragg::agg_png,
             width = 8, height = 4.5, dpi = 200, units = "in")
    }
  )
}

# =========================================================
# RUN
# =========================================================
shinyApp(ui, server)

