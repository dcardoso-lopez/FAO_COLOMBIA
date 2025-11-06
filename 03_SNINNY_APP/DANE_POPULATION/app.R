# =========================================================
# app_poblacion_dashboard_v2.R — Proyecciones DANE (pirámide decenal)
# =========================================================
suppressWarnings({
  library(shiny); library(dplyr); library(tidyr); library(ggplot2); library(plotly)
  library(scales); library(bslib); library(stringi)
})

options(stringsAsFactors = FALSE, scipen = 999)

# ---------- Ruta y carga ----------
data_dir <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/DANE_POPULATION/data"
pob_path <- file.path(data_dir, "051_DANE_Proyecciones_P.rds")
stopifnot(file.exists(pob_path))
pob <- readRDS(pob_path)

# ---------- Normalización mínima ----------
if (!"DEPARTAMENTO_D" %in% names(pob) && "DEPARTAMENTO" %in% names(pob)) pob$DEPARTAMENTO_D <- pob$DEPARTAMENTO
if (!"MUNICIPIO_D"   %in% names(pob) && "MUNICIPIO"   %in% names(pob)) pob$MUNICIPIO_D   <- pob$MUNICIPIO

suppressWarnings({
  if (!is.numeric(pob$ano))       pob$ano       <- as.integer(pob$ano)
  if (!is.numeric(pob$edad))      pob$edad      <- as.integer(pob$edad)
  if (!is.numeric(pob$poblacion)) pob$poblacion <- as.numeric(pob$poblacion)
})

# ---------- Helpers ----------
fmt_comma <- function(x) comma(x, big.mark = ".", decimal.mark = ",")
make_dec <- function(edad){
  e <- pmin(pmax(edad, 0), 80)
  brks <- seq(0, 80, 10)
  labs <- c(paste0(seq(0,70,10), "-", seq(9,79,10)), "80+")
  factor(cut(e, breaks = c(brks, Inf), right = FALSE, labels = labs), levels = labs)
}

# Gráfico vacío robusto (evita validate/need)
empty_plot <- function(txt = "Sin datos para los filtros actuales."){
  plotly::plotly_empty(type = "scatter", mode = "markers") %>%
    plotly::layout(
      annotations = list(
        x = 0.5, y = 0.5, text = as.character(txt),
        showarrow = FALSE, xref = "paper", yref = "paper",
        font = list(size = 14)
      ),
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      margin = list(l = 10, r = 10, b = 10, t = 10)
    )
}

# Opciones filtros
anos   <- sort(unique(na.omit(pob$ano)))
deps   <- sort(unique(na.omit(pob$DEPARTAMENTO_D)))
munis  <- sort(unique(na.omit(pob$MUNICIPIO_D)))
clase  <- c("Todas", sort(unique(na.omit(pob$clase))))
sexos  <- c("Ambos", sort(unique(na.omit(pob$sexo))))

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  theme = bs_theme(
    version = 5, primary = "#2563eb",
    base_font = font_google("Inter"), heading_font = font_google("Inter Tight"),
    "border-radius" = "0.9rem", "font-size-base" = "0.95rem"
  ),
  tags$head(tags$style(HTML("
    .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
    h3{font-weight:700;letter-spacing:.2px;margin-bottom:8px}
    .data-note{font-size:13px;color:#6b7280;margin:0 0 16px}

    /* Contenedores (filtros y cards) con borde #ffb366 */
    .filters{
      background:#fff; border:1.5px solid #ffb366; border-radius:16px;
      padding:14px 16px; margin-bottom:16px; box-shadow:0 4px 14px rgba(0,0,0,.06)
    }
    .card{
      background:#fff; border:1.5px solid #ffb366; border-radius:16px; padding:12px;
      box-shadow:0 2px 10px rgba(0,0,0,.05); margin-bottom:12px
    }

    .filters-grid{display:grid;grid-template-columns:repeat(5,minmax(160px,1fr));gap:12px}
    .filter-label{font-size:12px;font-weight:600;letter-spacing:.4px;
                  text-transform:uppercase;color:#6b7280;margin-bottom:6px}
    .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}
    .kpi{font-weight:800;font-size:28px;color:#111827}
    .kpi-sub{font-size:12px;color:#6b7280;margin-top:-4px}

    /* Inputs con borde #ffb366 */
    .form-control{ border:1.5px solid #ffb366 !important; border-radius:10px; }
    .form-control:focus{
      border-color:#ffb366 !important;
      box-shadow:0 0 0 .2rem rgba(255,179,102,.35);
    }
    .selectize-input{ border:1.5px solid #ffb366 !important; border-radius:10px; }
    .selectize-input.focus{
      border-color:#ffb366 !important;
      box-shadow:0 0 0 .2rem rgba(255,179,102,.35);
    }
    .selectize-dropdown{ border:1.5px solid #ffb366 !important; }

    /* Espacio extra debajo de la primera fila de visuales */
    .visuals-row{ margin-bottom: 24px; }
  "))),
  
  div(class="wrap",
      h3("Proyecciones de Población — DANE"),
      div(class="data-note","Filtros y visualizaciones de población nacional, departamental y municipal."),
      
      # ---------- Filtros ----------
      div(class="filters",
          div(class="filters-grid",
              div(class="filter", div(class="filter-label","Seleccione Año:"),
                  selectInput("f_ano", NULL, choices = anos, selected = max(anos, na.rm = TRUE))),
              div(class="filter", div(class="filter-label","Seleccione Departamento:"),
                  selectInput("f_dep", NULL, choices = c("Todos", deps), selected = "Todos")),
              div(class="filter", div(class="filter-label","Seleccione Municipio:"),
                  selectizeInput("f_mun", NULL, choices = "Todos", selected = "Todos",
                                 options = list(placeholder = "Seleccione municipio..."))),
              div(class="filter", div(class="filter-label","Seleccione Clase:"),
                  selectInput("f_clase", NULL, choices = clase, selected = "Todas")),
              div(class="filter", div(class="filter-label","Seleccione Sexo:"),
                  selectInput("f_sexo", NULL, choices = sexos, selected = "Ambos"))
          )
      ),
      
      # ---------- KPIs ----------
      fluidRow(
        column(3, div(class="card",
                      div(class="card-title","Población total"),
                      div(class="kpi", textOutput("kpi_pob")),
                      div(class="kpi-sub","Personas según filtros")
        )),
        column(3, div(class="card",
                      div(class="card-title","% Jóvenes (0–14)"),
                      div(class="kpi", textOutput("kpi_jovenes")),
                      div(class="kpi-sub","Participación sobre total")
        )),
        column(3, div(class="card",
                      div(class="card-title","% Adultos mayores (65+)"),
                      div(class="kpi", textOutput("kpi_mayores")),
                      div(class="kpi-sub","Participación sobre total")
        )),
        column(3, div(class="card",
                      div(class="card-title","Razón de dependencia"),
                      div(class="kpi", textOutput("kpi_dep")),
                      div(class="kpi-sub","(0–14 y 65+) / 15–64")
        ))
      ),
      
      # ---------- Visuales (fila 1) ----------
      fluidRow(class = "visuals-row",
               column(6, div(class="card",
                             div(class="card-title","¿Cómo ha cambiado el número de habitantes a lo largo del tiempo?"),
                             plotlyOutput("plot_hist", height = 420)
               )),
               column(6, div(class="card",
                             div(class="card-title","¿Cómo se distribuye la población de un territorio por grupos de edad y sexo?"),
                             plotlyOutput("plot_piramide", height = 420)
               ))
      ),
      
      # ---------- Visual independiente del año: CAGR 30 años ----------
      fluidRow(
        column(12, div(class="card",
                       div(class="card-title","¿Cómo ha variado la población, en promedio anual, durante los últimos 30 años?"),
                       plotlyOutput("plot_cagr30", height = 320)
        ))
      )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  # --- Municipios dependientes del departamento ---
  observeEvent(input$f_dep, {
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      ch <- c("Todos", sort(unique(na.omit(pob$MUNICIPIO_D))))
    } else {
      ch <- pob %>% filter(DEPARTAMENTO_D == input$f_dep) %>%
        distinct(MUNICIPIO_D) %>% arrange(MUNICIPIO_D) %>% pull(MUNICIPIO_D)
      ch <- c("Todos", ch)
    }
    updateSelectizeInput(session, "f_mun", choices = ch, selected = "Todos", server = TRUE)
  }, ignoreInit = TRUE)
  
  # --- Base filtrada ---
  base_filtrada <- reactive({
    df <- pob
    if (!is.null(input$f_dep)    && input$f_dep    != "Todos") df <- df %>% filter(DEPARTAMENTO_D == input$f_dep)
    if (!is.null(input$f_mun)    && input$f_mun    != "Todos") df <- df %>% filter(MUNICIPIO_D   == input$f_mun)
    if (!is.null(input$f_clase)  && input$f_clase  != "Todas") df <- df %>% filter(clase         == input$f_clase)
    if (!is.null(input$f_sexo)   && input$f_sexo   != "Ambos") df <- df %>% filter(sexo          == input$f_sexo)
    df
  })
  
  # Asegurar año válido dentro del subset (si cambian filtros)
  observe({
    df <- base_filtrada()
    yrs <- sort(unique(na.omit(df$ano)))
    if (length(yrs)) {
      sel <- if (!is.null(input$f_ano) && input$f_ano %in% yrs) input$f_ano else max(yrs)
      updateSelectInput(session, "f_ano", choices = yrs, selected = sel)
    }
  })
  
  # ================= KPIs =================
  output$kpi_pob <- renderText({
    df <- base_filtrada() %>% filter(ano == input$f_ano)
    fmt_comma(sum(df$poblacion, na.rm = TRUE))
  })
  output$kpi_jovenes <- renderText({
    df <- base_filtrada() %>% filter(ano == input$f_ano)
    tot <- sum(df$poblacion, na.rm = TRUE)
    j <- sum(df$poblacion[df$edad <= 14], na.rm = TRUE)
    paste0(round(if (tot > 0) 100 * j / tot else 0, 1), "%")
  })
  output$kpi_mayores <- renderText({
    df <- base_filtrada() %>% filter(ano == input$f_ano)
    tot <- sum(df$poblacion, na.rm = TRUE)
    m <- sum(df$poblacion[df$edad >= 65], na.rm = TRUE)
    paste0(round(if (tot > 0) 100 * m / tot else 0, 1), "%")
  })
  output$kpi_dep <- renderText({
    df <- base_filtrada() %>% filter(ano == input$f_ano)
    j <- sum(df$poblacion[df$edad <= 14], na.rm = TRUE)
    m <- sum(df$poblacion[df$edad >= 65], na.rm = TRUE)
    act <- sum(df$poblacion[df$edad >= 15 & df$edad <= 64], na.rm = TRUE)
    round(if (act > 0) (j + m) / act else 0, 2)
  })
  
  # ================= Gráficas =================
  output$plot_hist <- renderPlotly({
    df <- base_filtrada() %>%
      group_by(ano) %>%
      summarise(poblacion = sum(poblacion, na.rm = TRUE), .groups = "drop") %>%
      arrange(ano)
    
    if (nrow(df) == 0) return(empty_plot("Sin datos para la evolución histórica con los filtros actuales."))
    
    p <- ggplot(df, aes(x = ano, y = poblacion,
                        text = paste0("Año: ", ano, "<br>Población: ", fmt_comma(poblacion)))) +
      geom_line(color = "#0a83ff", linewidth = 1.2) +
      geom_point(color = "#0a83ff", size = 2) +
      scale_y_continuous(labels = fmt_comma) +
      labs(x = NULL, y = "Población total") +
      theme_minimal()
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l=60, r=20, t=30, b=40))
  })
  
  # Pirámide decenal (Hombres = #0a83ff, Mujeres = #f57c00)
  output$plot_piramide <- renderPlotly({
    df <- base_filtrada() %>% 
      filter(ano == input$f_ano) %>%
      mutate(
        grupo_edad = make_dec(edad),
        sexo_lc = tolower(trimws(as.character(sexo))),
        sexo_lc = dplyr::case_when(
          sexo_lc %in% c("hombre","hombres","m","masculino","masculinos") ~ "hombres",
          sexo_lc %in% c("mujer","mujeres","f","femenino","femeninos")     ~ "mujeres",
          TRUE ~ sexo_lc
        )
      ) %>%
      group_by(sexo_lc, grupo_edad) %>%
      summarise(pob = sum(poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(pob_plot = ifelse(sexo_lc == "hombres", -pob, pob))
    
    if (nrow(df) == 0) return(empty_plot("Sin datos para la pirámide con los filtros actuales."))
    
    p <- ggplot(df, aes(x = grupo_edad, y = pob_plot, fill = sexo_lc,
                        text = paste0("Grupo: ", grupo_edad,
                                      "<br>Sexo: ", tools::toTitleCase(sexo_lc),
                                      "<br>Población: ", fmt_comma(pob)))) +
      geom_col(width = 0.9) +
      coord_flip() +
      scale_y_continuous(labels = function(x) fmt_comma(abs(x))) +
      scale_fill_manual(values = c("hombres"="#0a83ff","mujeres"="#f57c00"),
                        breaks = c("hombres","mujeres"),
                        labels = c("Hombres","Mujeres"),
                        name   = "Sexo") +
      labs(x = "Grupo de edad (intervalos de 10 años)", y = "Población") +
      theme_minimal()
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l=60, r=20, t=30, b=40))
  })
  
  # Serie independiente del año: CAGR 30 años
  output$plot_cagr30 <- renderPlotly({
    df_tot <- base_filtrada() %>%
      group_by(ano) %>%
      summarise(poblacion = sum(poblacion, na.rm = TRUE), .groups = "drop") %>%
      arrange(ano)
    
    if (nrow(df_tot) == 0) return(empty_plot("Sin datos suficientes para estimar la serie de crecimiento."))
    
    df_join <- df_tot %>%
      left_join(df_tot %>% mutate(ano = ano + 30),
                by = "ano", suffix = c("_t0", "_t")) %>%
      transmute(ano,
                cagr = ifelse(!is.na(poblacion_t) & !is.na(poblacion_t0) & poblacion_t0 > 0,
                              (poblacion_t / poblacion_t0)^(1/30) - 1, NA_real_)) %>%
      filter(!is.na(cagr)) %>%
      arrange(ano)
    
    if (nrow(df_join) == 0) return(empty_plot("No hay pares t y t-30 disponibles con los filtros actuales."))
    
    p <- ggplot(df_join, aes(x = ano, y = cagr, group = 1,
                             text = paste0("Año t: ", ano,
                                           "<br>Crec. promedio anual (30 años): ",
                                           scales::percent(cagr, accuracy = 0.01,
                                                           big.mark = ".", decimal.mark = ",")))) +
      geom_line(color = "#0a83ff", linewidth = 1.2) +
      geom_point(color = "#0a83ff", size = 2) +
      scale_x_continuous(breaks = unique(df_join$ano)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.01,
                                                         big.mark = ".", decimal.mark = ",")) +
      labs(x = NULL, y = "Crecimiento promedio anual (30 años)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l=60, r=20, t=20, b=40),
             xaxis = list(tickmode = "linear", dtick = 1))
  })
}

# =========================================================
# RUN
# =========================================================
shinyApp(ui, server)

