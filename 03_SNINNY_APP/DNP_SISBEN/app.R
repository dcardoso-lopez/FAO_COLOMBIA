# =========================================================
# app_sisben_dashboard_v2.R — Sisbén (Hogares con Nw_hogares e i's 0 o Nw)
# Sin filtro de TRIMESTRE + título reactivo en gráfico de grupos
# =========================================================
suppressWarnings({
  library(shiny); library(dplyr); library(ggplot2); library(plotly)
  library(scales); library(bslib); library(htmltools)
})

options(stringsAsFactors = FALSE, scipen = 999)

# ---------- Ruta y carga ----------
data_dir    <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/DNP_SISBEN/data"
sisben_path <- file.path(data_dir, "031_DNP_SISBEN.rds")
sisben      <- readRDS(sisben_path)

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

# Paleta fija por grupo A–D (categorías)
GRP_COLS <- c(A = "#8e44ad", B = "#009edb", C = "#007a3d", D = "#f57c00")

fmt_comma <- function(x) scales::comma(x, big.mark = ".", decimal.mark = ",")
pc1 <- function(x) paste0(format(round(x, 1), nsmall = 1, decimal.mark = ","), "%")

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  theme = bs_theme(
    version = 5, primary = "#2563eb",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter Tight"),
    "border-radius" = "0.9rem", "font-size-base" = "0.95rem"
  ),
  tags$head(tags$style(HTML("
    :root{
    --bdr:#d9aee9;
  }
  .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
  h3{font-weight:700;letter-spacing:.2px;margin-bottom:8px}
  .data-note{font-size:13px;color:#6b7280;margin:0 0 16px}

  /* Tarjetas, filtros y valueboxes con borde lila */
  .filters,
  .card{
    background:#fff;
    border:2px solid var(--bdr) !important;
    border-radius:16px;
    padding:14px 16px;
    box-shadow:0 2px 10px rgba(0,0,0,.05);
    margin-bottom:12px;
  }

  /* Grid filtros */
  .filters-grid{display:grid;grid-template-columns:repeat(4,minmax(180px,1fr));gap:12px}
  .filter-label{font-size:12px;font-weight:600;letter-spacing:.4px;text-transform:uppercase;color:#6b7280;margin-bottom:6px}
  .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}
  .kpi{font-weight:800;font-size:28px;color:#111827}
  .kpi-sub{font-size:12px;color:#6b7280;margin-top:-4px}

  /* Controles con borde lila (selectize, select, inputs) */
  .selectize-control.single .selectize-input,
  .selectize-dropdown,
  .form-select,
  .form-control{
    border:2px solid var(--bdr) !important;
    border-radius:12px !important;
    box-shadow:none !important;
  }
  /* Hover/focus suaves al mismo tono */
  .selectize-control.single .selectize-input.dropdown-active{
    border-color:var(--bdr) !important;
  }
  .form-select:focus, .form-control:focus{
    border-color:var(--bdr) !important;
    box-shadow:0 0 0 .2rem rgba(217,174,233,.25) !important;
  }
  "))),
  
  div(class="wrap",
      h3("Sisbén — Explorador de privaciones (Hogares)"),
      div(class="data-note","Indicadores a nivel de hogar ponderados por Nw_hogares."),
      
      # ---------- Filtros (sin trimestre) ----------
      div(class="filters",
          div(class="filters-grid",
              div(class="filter", div(class="filter-label","Seleccione Año:"),
                  selectInput("f_ano", NULL, choices = sort(unique(sisben$ano)), selected = max(sisben$ano))),
              div(class="filter", div(class="filter-label","Seleccione Departamento:"),
                  selectizeInput("f_dep", NULL, choices = NULL, options = list(placeholder="Todos"))),
              div(class="filter", div(class="filter-label","Seleccione Municipio:"),
                  selectizeInput("f_mun", NULL, choices = NULL, options = list(placeholder="Todos"))),
              div(class="filter", div(class="filter-label","Seleccione Grupo Sisbén"),
                  selectInput("f_grupo", NULL, choices = c("Todos", sort(unique(sisben$grupo))), selected = "Todos"))
          )
      ),
      
      # ---------- KPIs ----------
      fluidRow(
        column(3, div(class="card",
                      div(class="card-title","Hogares totales (ponderados)"),
                      div(class="kpi", textOutput("kpi_hog")),
                      div(class="kpi-sub","")
        )),
        column(3, div(class="card",
                      div(class="card-title","% hogares en pobreza (A+B)"),
                      div(class="kpi", textOutput("kpi_ab")),
                      div(class="kpi-sub","Hogares en A o B sobre total")
        )),
        column(3, div(class="card",
                      div(class="card-title","% hogares con ≥1 privación"),
                      div(class="kpi", textOutput("kpi_anypriv")),
                      div(class="kpi-sub","Alguna i1–i15 > 0")
        )),
        column(3, div(class="card",
                      div(class="card-title","Promedio de privaciones por hogar"),
                      div(class="kpi", textOutput("kpi_prompriv")),
                      div(class="kpi-sub","(suma de flags i) / hogares")
        ))
      ),
      
      # ---------- Visuales ----------
      fluidRow(
        column(6, div(class="card",
                      # Título reactivo (UI)
                      uiOutput("ttl_grupos"),
                      plotlyOutput("plot_grupos", height = 360)
        )),
        column(6, div(class="card",
                      div(class="card-title","¿Qué privaciones del IPM (indice de pobreza multidimensional) son más frecuentes en la población?"),
                      plotlyOutput("plot_priv_top", height = 360)
        ))
      ),
      
      # ---------- Evolución histórica pobreza (A+B) ----------
      fluidRow(
        column(12, div(class="card",
                       div(class="card-title","¿Cómo ha evolucionado el porcentaje de población clasificada en los grupos A  y B entre 2021 y 2024?"),
                       plotlyOutput("plot_pobreza_hist", height = 360)
        ))
      )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  # --- Cargar choices de dpto/mun server-side ---
  updateSelectizeInput(session, "f_dep",
                       choices = c("Todos", sort(unique(sisben$DEPARTAMENTO_D))), selected = "Todos", server = TRUE
  )
  updateSelectizeInput(session, "f_mun",
                       choices = c("Todos", sort(unique(sisben$MUNICIPIO_D))),   selected = "Todos", server = TRUE
  )
  
  # --- Filtro dependiente de municipios por departamento ---
  observeEvent(input$f_dep, {
    if (is.null(input$f_dep) || input$f_dep == "Todos"){
      updateSelectizeInput(session, "f_mun",
                           choices = c("Todos", sort(unique(sisben$MUNICIPIO_D))),
                           selected = "Todos", server = TRUE
      )
    } else {
      ch <- sisben %>%
        dplyr::filter(DEPARTAMENTO_D == input$f_dep) %>%
        dplyr::distinct(MUNICIPIO_D) %>%
        dplyr::arrange(MUNICIPIO_D) %>% dplyr::pull(MUNICIPIO_D)
      updateSelectizeInput(session, "f_mun",
                           choices = c("Todos", ch),
                           selected = "Todos", server = TRUE
      )
    }
  }, ignoreInit = TRUE)
  
  # --- Base filtrada (sin trimestre) ---
  base_filtrada <- reactive({
    df <- sisben %>% dplyr::filter(ano == input$f_ano)
    if (input$f_dep  != "Todos") df <- df %>% dplyr::filter(DEPARTAMENTO_D == input$f_dep)
    if (input$f_mun  != "Todos") df <- df %>% dplyr::filter(MUNICIPIO_D   == input$f_mun)
    if (input$f_grupo!= "Todos") df <- df %>% dplyr::filter(grupo == input$f_grupo)
    df
  }) |> bindCache(input$f_ano, input$f_dep, input$f_mun, input$f_grupo)
  
  # ---- Helper de ámbito para el título ----
  ambito_sel <- reactive({
    dep <- input$f_dep; mun <- input$f_mun; ano <- input$f_ano
    ambito_txt <- if (is.null(dep) || dep == "Todos") {
      "Colombia"
    } else if (!is.null(mun) && mun != "Todos") {
      paste0(mun, ", ", dep)
    } else {
      dep
    }
    list(ambito = ambito_txt, ano = ano)
  })
  
  # ---- Título dinámico del card de grupos ----
  output$ttl_grupos <- renderUI({
    info <- ambito_sel()
    HTML(sprintf(
      '<div class="card-title">¿Cómo se distribuye la población de %s según la clasificación del Sisbén? <span style="color:#6b7280;font-weight:600;">(Año %s)</span></div>',
      htmlEscape(info$ambito),
      htmlEscape(info$ano)
    ))
  })
  
  # --- KPIs ---
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
    w <- df$Nw_hogares
    any_h <- sum( w * as.integer( rowSums(df[, priv_cols] > 0, na.rm = TRUE) > 0 ), na.rm = TRUE )
    tot_w <- sum(w, na.rm = TRUE)
    pc1(if (tot_w > 0) 100 * any_h / tot_w else 0)
  })
  
  output$kpi_prompriv <- renderText({
    df <- base_filtrada()
    if (length(priv_cols) == 0 || nrow(df) == 0) return("0")
    w <- df$Nw_hogares
    npriv_por_hogar <- rowSums(df[, priv_cols] > 0, na.rm = TRUE)
    prom <- sum(npriv_por_hogar * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
    format(round(prom, 2), decimal.mark = ",")
  })
  
  # --- Distribución por grupos (colores fijos y reactividad plena) ---
  output$plot_grupos <- renderPlotly({
    df <- base_filtrada() %>%
      dplyr::mutate(grupo = factor(grupo, levels = c("A","B","C","D"))) %>%
      dplyr::group_by(grupo) %>%
      dplyr::summarise(hogares = sum(Nw_hogares, na.rm = TRUE), .groups = "drop")
    
    # Mantener niveles aun si algún grupo no aparece bajo los filtros
    df <- tidyr::complete(df, grupo = factor(c("A","B","C","D"), levels = c("A","B","C","D")),
                          fill = list(hogares = 0))
    
    p <- ggplot(df, aes(x = grupo, y = hogares, fill = grupo,
                        text = paste0("Grupo: ", grupo, "<br>Hogares: ", fmt_comma(hogares)))) +
      geom_col(width = 0.75) +
      scale_fill_manual(values = GRP_COLS, breaks = c("A","B","C","D")) +
      scale_y_continuous(labels = fmt_comma) +
      labs(x = NULL, y = "Hogares (ponderados)", fill = "Grupo") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # --- Top-10 privaciones (prevalencia en hogares) ---
  output$plot_priv_top <- renderPlotly({
    df <- base_filtrada()
    if (length(priv_cols) == 0 || nrow(df) == 0) return(NULL)
    
    w <- df$Nw_hogares
    m_bin <- as.matrix(df[, priv_cols, drop = FALSE] > 0)
    tot_w <- sum(w, na.rm = TRUE); if (is.na(tot_w) || tot_w == 0) return(NULL)
    
    prev_vec <- colSums(m_bin * w, na.rm = TRUE) / tot_w
    prev <- data.frame(var = names(prev_vec), prev = as.numeric(prev_vec)) %>%
      dplyr::mutate(label = ifelse(var %in% names(i_labels), i_labels[var], var)) %>%
      dplyr::arrange(desc(prev)) %>% dplyr::slice_head(n = 10)
    
    p <- ggplot(prev, aes(x = prev, y = reorder(label, prev),
                          text = paste0(label, "<br>Prevalencia (hogares): ",
                                        scales::percent(prev, accuracy = 0.1, decimal.mark=",")))) +
      geom_col(fill = "#8e44ad") +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1, decimal.mark=",")) +
      labs(x = "Prevalencia ponderada (hogares)", y = NULL) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # --- Evolución histórica pobreza (A+B) — LÍNEA unida ---
  output$plot_pobreza_hist <- renderPlotly({
    df <- sisben
    if (input$f_dep  != "Todos") df <- df %>% dplyr::filter(DEPARTAMENTO_D == input$f_dep)
    if (input$f_mun  != "Todos") df <- df %>% dplyr::filter(MUNICIPIO_D   == input$f_mun)
    
    serie <- df %>%
      dplyr::group_by(ano) %>%
      dplyr::summarise(
        total_h = sum(Nw_hogares, na.rm = TRUE),
        ab_h    = sum(Nw_hogares[grupo %in% c("A","B")], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(pct_ab = dplyr::if_else(total_h > 0, 100 * ab_h / total_h, 0)) %>%
      dplyr::arrange(ano)
    
    req(nrow(serie) > 0)
    
    plotly::plot_ly(
      data = serie,
      x = ~as.numeric(ano), y = ~pct_ab,
      type = "scatter", mode = "lines+markers",
      line   = list(width = 2, color = "#8e44ad"),
      marker = list(size = 7,  color = "#8e44ad"),
      connectgaps = TRUE,
      hovertemplate = "<b>Año %{x:.0f}</b><br>% A+B: %{y:.1f}%<extra></extra>"
    ) |>
      plotly::layout(
        xaxis = list(title = "Año", tick0 = min(serie$ano), dtick = 1),
        yaxis = list(title = "% hogares en pobreza (A+B)",
                     tickformat = ".0f",
                     ticksuffix = "%",
                     rangemode = "tozero"),
        margin = list(l = 60, r = 20, t = 30, b = 40),
        hovermode = "x unified"
      )
  })
  
  
}

# =========================================================
# RUN
# =========================================================
shinyApp(ui, server)


