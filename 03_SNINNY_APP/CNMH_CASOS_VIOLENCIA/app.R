# =========================================================
# app_cnmh_dashboard.R — Histórico + Territorial (estilo moderno)
# =========================================================
suppressWarnings({
  library(shiny); library(dplyr); library(ggplot2); library(plotly)
  library(lubridate); library(scales); library(sf); library(leaflet); library(bslib)
  library(glue); library(stringr)
})

options(stringsAsFactors = FALSE, scipen = 999)

# ---- Arreglo definitivo de validaciones (evita choque con plotly::validate) ----
validate <- shiny::validate
need     <- shiny::need

# ---------- Rutas y carga ----------
data_dir  <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/CNMH_CASOS_VIOLENCIA/data"
cnmh_path <- file.path(data_dir, "061_Centro Nacional de Memoria Histórica_Casos_Violencia.rds")
ruta_shp_dptos <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")
ruta_shp_mpios <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")

cnmh <- readRDS(cnmh_path) %>% filter(!is.na(fecha_completa))
min_f <- min(cnmh$fecha_completa, na.rm = TRUE)
max_f <- max(cnmh$fecha_completa, na.rm = TRUE)

# Choices globales para filtros del Tab 1
deps_all  <- sort(unique(na.omit(cnmh$DEPARTAMENTO_D)))
munis_all <- sort(unique(na.omit(cnmh$MUNICIPIO_D)))

# Shapes
dptos_sf <- st_read(ruta_shp_dptos, quiet = TRUE) %>%
  mutate(COD_DPTO2 = sprintf("%02d", as.integer(DPTO_CCDGO))) %>%
  st_transform(4326) %>%
  dplyr::select(DPTO_CCDGO, DPTO_CNMBR, COD_DPTO2, geometry)

mpios_sf <- st_read(ruta_shp_mpios, quiet = TRUE) %>%
  mutate(COD_MUN5 = sprintf("%05d", as.integer(MPIO_CDPMP)),
         COD_DPTO2 = substr(COD_MUN5, 1, 2)) %>%
  st_transform(4326) %>%
  dplyr::select(MPIO_CDPMP, MPIO_CNMBR, COD_MUN5, COD_DPTO2, geometry)

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
    :root{ --brand-border:#d9aee9; } /* color de bordes */

    .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
    h3{font-weight:700;letter-spacing:.2px;margin-bottom:8px}
    .data-note{font-size:13px;color:#6b7280;margin:0 0 16px}

    .filters{
      background:#fff;border:1px solid var(--brand-border);border-radius:16px;
      padding:14px 16px;margin-bottom:16px;box-shadow:0 4px 14px rgba(0,0,0,.06)
    }
    .filters-grid{display:grid;grid-template-columns:repeat(4,minmax(180px,1fr));gap:12px}
    .filter-label{font-size:12px;font-weight:600;letter-spacing:.4px;
                  text-transform:uppercase;color:#6b7280;margin-bottom:6px}

    .card{
      background:#fff;border:1px solid var(--brand-border);border-radius:16px;padding:12px;
      box-shadow:0 2px 10px rgba(0,0,0,.05);margin-bottom:12px
    }
    .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}

    /* Bordes en objetos visuales */
    .card .leaflet-container, .card .html-widget{
      border:1px solid var(--brand-border);border-radius:12px;
    }
    .leaflet-control.legend, .leaflet-control .legend, .info.legend{
      border:1px solid var(--brand-border) !important;border-radius:10px !important;
    }

    /* Inputs para coherencia visual */
    .form-select, .form-control, .btn-outline-primary{ border-color: var(--brand-border); }
    .btn-outline-primary{ color:#2563eb;background:#fff; }
    .btn-outline-primary:hover{ background:#f9f8ff;border-color:var(--brand-border); }
  "))),
  
  div(class="wrap",
      h3("CNMH — Explorador de Casos de Violencia"),
      div(class="data-note","Aplicación interactiva para explorar los casos registrados en el Centro Nacional de Memoria Histórica."),
      
      tabsetPanel(
        id="tabs_cnmh", type="tabs",
        
        # ---------- TAB 1: HISTÓRICO ----------
        tabPanel("Histórico de casos", br(),
                 div(class="filters",
                     div(class="filters-grid",
                         div(class="filter", div(class="filter-label","Agrupar por"),
                             radioButtons("freq", NULL,
                                          choices=c("Día","Mes","Trimestre","Año"),
                                          selected="Mes", inline=TRUE)),
                         div(class="filter", div(class="filter-label","Rango de fechas"),
                             dateRangeInput("rango", NULL,
                                            start=min_f, end=max_f, min=min_f, max=max_f, separator=" a ")),
                         div(class="filter", div(class="filter-label","Categoría"),
                             selectInput("categoria", NULL,
                                         choices=c("Todas", sort(unique(na.omit(cnmh$categoria)))),
                                         selected="Todas")),
                         div(class="filter", div(class="filter-label","Departamento"),
                             selectInput("dep_hist", NULL,
                                         choices=c("Todos", deps_all), selected="Todos")),
                         div(class="filter", div(class="filter-label","Municipio"),
                             selectizeInput("mun_hist", NULL,
                                            choices=c("Todos", munis_all), selected="Todos",
                                            options=list(placeholder="Seleccione municipio...")))
                     )
                 ),
                 div(class="card",
                     uiOutput("title_ts_card"),
                     plotlyOutput("ts_hist", height=450)
                 )
        ),
        
        # ---------- TAB 2: TERRITORIO ----------
        tabPanel("Casos por territorio", br(),
                 div(class="filters",
                     div(class="filters-grid",
                         div(class="filter", div(class="filter-label","Año"),
                             selectInput("ano", NULL, choices=sort(unique(cnmh$ano)), selected=max(cnmh$ano))),
                         div(class="filter", div(class="filter-label","Categoría"),
                             selectInput("cat", NULL, choices=c("Todas", sort(unique(cnmh$categoria))), selected="Todas")),
                         div(class="filter", div(class="filter-label","Acción"),
                             tagList(actionButton("volver","← Volver a Departamentos", class="btn btn-outline-primary btn-sm")))
                     )
                 ),
                 fluidRow(
                   column(3, div(class="card", div(class="card-title","Casos totales"), textOutput("vb_total"))),
                   column(3, div(class="card", div(class="card-title","% Mujeres"), textOutput("vb_mujeres"))),
                   column(3, div(class="card", div(class="card-title","% Civiles"), textOutput("vb_civiles"))),
                   column(3, div(class="card", div(class="card-title","Entidad más afectada"), textOutput("vb_topent")))
                 ),
                 fluidRow(
                   column(6,
                          div(class="card",
                              uiOutput("title_mapa_card"),
                              leafletOutput("mapa", height=600)
                          )
                   ),
                   column(6,
                          div(class="card",
                              uiOutput("title_top10_card"),
                              plotlyOutput("top10", height=300)
                          ),
                          div(class="card",
                              div(class="card-title","¿Cómo se distribuyen los casos de violencia según la calidad y el sexo de la víctima?"),
                              plotlyOutput("comp_victimas", height=280)
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
  # --- Dep -> Mun dinámico (Tab 1) ---
  observeEvent(input$dep_hist, {
    if (is.null(input$dep_hist) || input$dep_hist == "Todos") {
      ch <- c("Todos", munis_all)
    } else {
      ch <- cnmh %>% filter(DEPARTAMENTO_D == input$dep_hist) %>%
        distinct(MUNICIPIO_D) %>% arrange(MUNICIPIO_D) %>% pull(MUNICIPIO_D)
      ch <- c("Todos", ch)
    }
    updateSelectizeInput(session, "mun_hist", choices = ch, selected = "Todos", server = TRUE)
  }, ignoreInit = TRUE)
  
  # ================= HISTÓRICO =================
  agg_ts <- reactive({
    req(input$rango, input$freq)
    df <- cnmh %>% filter(fecha_completa >= input$rango[1], fecha_completa <= input$rango[2])
    if (!is.null(input$categoria) && input$categoria != "Todas") df <- df %>% filter(categoria == input$categoria)
    if (!is.null(input$dep_hist)   && input$dep_hist   != "Todos") df <- df %>% filter(DEPARTAMENTO_D == input$dep_hist)
    if (!is.null(input$mun_hist)   && input$mun_hist   != "Todos") df <- df %>% filter(MUNICIPIO_D   == input$mun_hist)
    unidad <- switch(input$freq, "Día"="day", "Mes"="month", "Trimestre"="quarter", "Año"="year")
    df %>% mutate(fecha_g = floor_date(fecha_completa, unit = unidad)) %>% count(fecha_g, name = "casos") %>% arrange(fecha_g)
  })
  
  # ---- Título storytelling reactivo (Tab 1) ----
  titulo_ts <- reactive({
    loc <- if (!is.null(input$mun_hist) && input$mun_hist != "Todos" &&
               !is.null(input$dep_hist) && input$dep_hist != "Todos") {
      paste0("en ", input$mun_hist, ", ", input$dep_hist)
    } else if (!is.null(input$dep_hist) && input$dep_hist != "Todos") {
      paste0("en ", input$dep_hist)
    } else { "en Colombia" }
    cat_txt   <- if (!is.null(input$categoria) && input$categoria != "Todas") paste0(" para ", tolower(input$categoria)) else ""
    rango_txt <- if (!is.null(input$rango) && length(input$rango) == 2)
      paste0(" entre ", format(input$rango[1], "%Y-%m-%d"), " y ", format(input$rango[2], "%Y-%m-%d")) else ""
    freq_txt  <- if (!is.null(input$freq)) paste0(" (agrupado por ", tolower(input$freq), ")") else ""
    paste0("¿Cómo han cambiado los casos de violencia", cat_txt, " ", loc, rango_txt, "?", freq_txt)
  })
  output$title_ts_card <- renderUI({ div(class = "card-title", titulo_ts()) })
  
  output$ts_hist <- renderPlotly({
    dd <- agg_ts(); validate(need(nrow(dd) > 0, "Sin datos en el rango seleccionado."))
    plot_ly(dd, x=~fecha_g, y=~casos, type="scatter", mode="lines",
            line=list(width=2, color="#8e44ad")) %>%
      layout(hovermode="x unified",
             xaxis=list(rangeselector=list(buttons=list(
               list(count=1, step="year", stepmode="backward", label="1A"),
               list(count=5, step="year", stepmode="backward", label="5A"),
               list(count=10,step="year", stepmode="backward", label="10A"),
               list(step="all", label="Todo"))),
               rangeslider=list(visible=TRUE)),
             yaxis=list(title="Casos", rangemode="tozero"),
             margin=list(l=40, r=20, b=40, t=20))
  })
  
  # ================= TERRITORIO =================
  vista <- reactiveVal("depto")
  depto_sel <- reactiveVal(NULL)
  
  base_filtrada <- reactive({
    req(input$ano)
    df <- cnmh %>% filter(ano == input$ano)
    if (input$cat != "Todas") df <- df %>% filter(categoria == input$cat)
    if (vista() == "mpio" && !is.null(depto_sel())) df <- df %>% filter(COD_DANE_DPTO_D == depto_sel())
    df
  })
  
  agg_data <- reactive({
    df <- base_filtrada()
    if (vista() == "depto"){
      df %>% group_by(COD_DANE_DPTO_D, DEPARTAMENTO_D) %>%
        summarise(casos = n(), .groups = "drop") %>%
        mutate(COD_DPTO2 = sprintf("%02d", as.integer(COD_DANE_DPTO_D)))
    } else {
      df %>% group_by(COD_DANE_MUNIC_D, MUNICIPIO_D) %>%
        summarise(casos = n(), .groups = "drop") %>%
        mutate(COD_MUN5 = sprintf("%05d", as.integer(COD_DANE_MUNIC_D)))
    }
  })
  
  # ---- Título del mapa (reactivo por vista y hecho cometido) ----
  titulo_mapa <- reactive({
    hecho <- if (!is.null(input$cat) && input$cat != "Todas") paste0(" ", tolower(input$cat)) else ""
    if (vista() == "depto") {
      paste0("¿Cuáles han sido los municipios más golpeados por la violencia", hecho, "?")
    } else {
      if (is.null(depto_sel())) return("Selecciona un departamento para ver sus municipios")
      dep_name <- dptos_sf$DPTO_CNMBR[match(depto_sel(), dptos_sf$COD_DPTO2)]
      dep_name <- ifelse(is.na(dep_name) | dep_name == "", "el departamento seleccionado", dep_name)
      paste0("¿Cómo se distribuyen los casos de violencia", hecho, " en los municipios de ", dep_name, "?")
    }
  })
  output$title_mapa_card <- renderUI({ div(class = "card-title", titulo_mapa()) })
  
  output$vb_total   <- renderText({ comma(nrow(base_filtrada())) })
  output$vb_mujeres <- renderText({
    pct <- mean(base_filtrada()$sexo == "MUJER", na.rm = TRUE) * 100
    if (is.nan(pct)) pct <- 0; paste0(round(pct, 1), "%")
  })
  output$vb_civiles <- renderText({
    pct <- mean(base_filtrada()$calidad_de_la_victima_o_la_baja == "CIVIL", na.rm = TRUE) * 100
    if (is.nan(pct)) pct <- 0; paste0(round(pct, 1), "%")
  })
  output$vb_topent <- renderText({
    df <- agg_data(); if (nrow(df) == 0) return("Sin datos")
    nm <- if (vista() == "depto") "DEPARTAMENTO_D" else "MUNICIPIO_D"
    m  <- max(df$casos, na.rm = TRUE)
    top_df <- df %>% dplyr::filter(casos == m)
    top_names <- top_df[[nm]] %>% unique() %>% sort()
    if (length(top_names) == 1) paste0(top_names, ": ", scales::comma(m))
    else paste0(paste(top_names, collapse = " / "), ": ", scales::comma(m))
  })
  
  # Mapa
  output$mapa <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lng = -74, lat = 4.5, zoom = 5)
  })
  
  observe({
    df_join <- agg_data()
    shp <- if (vista() == "depto") {
      dptos_sf %>% left_join(df_join, by = "COD_DPTO2")
    } else {
      validate(need(!is.null(depto_sel()), "Haz clic en un departamento para ver sus municipios."))
      mpios_sf %>% filter(COD_DPTO2 == depto_sel()) %>% left_join(df_join, by = "COD_MUN5")
    }
    validate(need(nrow(shp) > 0, "Sin datos para el mapa."))
    
    # Paleta personalizada (invertida: más alto = más oscuro)
    pal_vec <- rev(c("#602070", "#8e44ad", "#b46cd2", "#d9aee9", "#f4e6f9"))
    pal <- colorBin(palette = pal_vec, domain = shp$casos, bins = length(pal_vec), na.color = "#f0f0f0")
    
    leafletProxy("mapa", data = shp) %>%
      clearShapes() %>% clearControls() %>%
      addPolygons(
        layerId = if (vista() == "depto") ~COD_DPTO2 else ~COD_MUN5,
        fillColor = ~pal(casos), color = "#3b2a4a", weight = 0.6, fillOpacity = 0.85,
        label = ~paste0(if (vista() == "depto") DPTO_CNMBR else MPIO_CNMBR,
                        "<br>Casos: ", ifelse(is.na(casos), "0", comma(casos))),
        highlightOptions = highlightOptions(weight = 1.2, color = "#2b193d", bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~casos, title = "Número de casos")
  })
  
  observeEvent(input$mapa_shape_click, {
    if (vista() == "depto") {
      click <- input$mapa_shape_click
      if (!is.null(click$id)) { vista("mpio"); depto_sel(click$id) }
    }
  })
  observeEvent(input$volver, { vista("depto"); depto_sel(NULL) })
  
  # ---- Título Top-10 (reactivo por filtros y vista) ----
  titulo_top10 <- reactive({
    hecho <- if (!is.null(input$cat) && input$cat != "Todas") paste0(" de ", tolower(input$cat)) else ""
    anio  <- if (!is.null(input$ano)) paste0(" en ", input$ano) else ""
    if (vista() == "depto") {
      paste0("¿Cuáles son los 10 departamentos que registran el mayor número de víctimas de la violencia",
             hecho, anio, "?")
    } else {
      dep_name <- dptos_sf$DPTO_CNMBR[match(depto_sel(), dptos_sf$COD_DPTO2)]
      dep_name <- ifelse(is.na(dep_name) | dep_name == "", "el departamento seleccionado", dep_name)
      paste0("¿Cuáles son los 10 municipios de ", dep_name,
             " con más víctimas de la violencia", hecho, anio, "?")
    }
  })
  output$title_top10_card <- renderUI({ div(class = "card-title", titulo_top10()) })
  
  # Gráfico Top-10 (barras #8e44ad)
  output$top10 <- renderPlotly({
    df <- agg_data() %>% arrange(desc(casos)) %>% slice_head(n = 10)
    validate(need(nrow(df) > 0, "Sin datos para el Top-10."))
    df <- df %>% mutate(nombre = if (vista() == "depto") DEPARTAMENTO_D else MUNICIPIO_D)
    p <- ggplot(df, aes(x = reorder(nombre, casos), y = casos,
                        text = paste0(nombre, "<br>Casos: ", comma(casos)))) +
      geom_col(fill = "#8e44ad") + coord_flip() + labs(x = NULL, y = "Casos") + theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  # ================= Composición de víctimas (colores por combinación) =================
  output$comp_victimas <- renderPlotly({
    validate(need(nrow(base_filtrada()) > 0, "Sin datos para la composición de víctimas."))
    df_counts <- base_filtrada() %>%
      mutate(
        sexo    = ifelse(is.na(sexo) | sexo == "", "SIN INFORMACIÓN", toupper(sexo)),
        calidad = ifelse(is.na(calidad_de_la_victima_o_la_baja) | calidad_de_la_victima_o_la_baja == "",
                         "SIN INFORMACIÓN", toupper(calidad_de_la_victima_o_la_baja)),
        grupo = dplyr::case_when(
          calidad == "CIVIL"       & sexo == "MUJER"  ~ "Civil - Mujer",
          calidad == "CIVIL"       & sexo == "HOMBRE" ~ "Civil - Hombre",
          calidad == "COMBATIENTE" & sexo == "MUJER"  ~ "Combatiente - Mujer",
          calidad == "COMBATIENTE" & sexo == "HOMBRE" ~ "Combatiente - Hombre",
          TRUE ~ "Otros"
        )
      ) %>% count(calidad, grupo, name = "casos")
    tot <- df_counts %>% group_by(calidad) %>% summarise(total = sum(casos), .groups = "drop") %>% arrange(desc(total))
    df <- df_counts %>% left_join(tot, by = "calidad") %>%
      mutate(
        pct   = ifelse(total > 0, casos/total, 0),
        grupo = factor(grupo, levels = c("Civil - Mujer","Civil - Hombre","Combatiente - Mujer","Combatiente - Hombre","Otros")),
        calidad = factor(calidad, levels = tot$calidad),
        calidad_lab = stringr::str_wrap(as.character(calidad), width = 14)
      )
    pal <- c("Civil - Mujer"="#8e44ad", "Civil - Hombre"="#009edb",
             "Combatiente - Mujer"="#f57c00", "Combatiente - Hombre"="#007a3d",
             "Otros"="#9ca3af")
    p <- ggplot(df, aes(x = calidad_lab, y = pct, fill = grupo,
                        text = glue("<b>Calidad:</b> {calidad}<br>",
                                    "<b>Combinación:</b> {grupo}<br>",
                                    "<b>Casos:</b> {scales::comma(casos)}<br>",
                                    "<b>Participación:</b> {scales::percent(pct, accuracy = 0.1)}"))) +
      geom_col(position = "fill", width = 0.8) +
      scale_fill_manual(values = pal, name = "Calidad – Sexo") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = "Calidad", y = "Composición (%)") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(), legend.position = "right",
            axis.title.x = element_text(margin = margin(t = 6)),
            axis.title.y = element_text(margin = margin(r = 6)))
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l = 40, r = 10, t = 10, b = 40),
                                             legend = list(orientation = "v"))
  })
}

# =========================================================
# RUN
# =========================================================
shinyApp(ui, server)

