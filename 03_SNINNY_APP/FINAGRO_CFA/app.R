# =========================================================
# app_finagro_moderno.R — Tendencias + Indicadores + Mapas (optimizada)
# - Usa .rds precomputados (agregados + shapes simplificados)
# - Mapa se dibuja solo cuando abres la pestaña "Mapas"
# - Serie y Top-5 con Plotly nativo
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
data_dir <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/FINAGRO_CFA/data"

# ---------- Lectura de precomputados (rápido) ----------
finagro_fast       <- readRDS(file.path(data_dir, "081_FINAGRO_CFA_fast.rds"))
finagro_depto_map  <- readRDS(file.path(data_dir, "map_finagro_depto.rds"))
finagro_mpio_map   <- readRDS(file.path(data_dir, "map_finagro_mpio.rds"))
mpios_sf           <- readRDS(file.path(data_dir, "mpios_sf_simpl.rds"))
dptos_sf           <- readRDS(file.path(data_dir, "dptos_sf_simpl.rds"))

# ---------- Helpers ----------
fmt_int   <- function(x) number(x, big.mark = ".", decimal.mark = ",", accuracy = 1)
fmt_cop   <- function(x) paste0("$", number(x, big.mark = ".", decimal.mark = ",", accuracy = 1))
fmt_mmilM <- function(x) paste0("$", number(x/1e9, big.mark = ".", decimal.mark = ",", accuracy = 0.1), " Mil M")
fmt_milM  <- function(x) paste0(number(x/1e9, big.mark = ".", decimal.mark = ",", accuracy = 0.1), " Mil M")
mes_labels <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

pal_bin_safe <- function(palette, x, bins = 5){
  dom <- x[is.finite(x)]; if (!length(dom)) dom <- c(0,1)
  if (length(unique(dom))==1) dom <- unique(dom)+c(-1e-9,1e-9)
  leaflet::colorBin(palette, domain = dom, bins = bins, na.color = "#f0f0f0")
}

# Paleta solicitada (para 1er tab)
pal_story <- c("#c49000", "#fbc02d", "#ffd54f", "#ffe082", "#fff9db")

# ---------- Agregados para Mapas (join con shapes) ----------
mapa_depto <- dptos_sf %>%
  left_join(finagro_depto_map, by = "COD_DPTO2") %>%
  sf::st_as_sf()

mapa_mpio  <- mpios_sf %>%
  left_join(finagro_mpio_map,  by = c("COD_MUN5","COD_DPTO2")) %>%
  sf::st_as_sf()

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
  tags$head(tags$style(HTML("
    .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
    h3{font-weight:700;letter-spacing:.2px;margin-bottom:8px}
    .data-note{font-size:13px;color:#6b7280;margin:0 0 16px}
    .filters{background:#fff;border:1px solid #eaecef;border-radius:16px;
             padding:14px 16px;margin-bottom:16px;box-shadow:0 4px 14px rgba(0,0,0,.06)}
    .filters-grid{display:grid;grid-template-columns:repeat(3,minmax(220px,1fr));gap:12px}
    .filters-grid.maps{grid-template-columns:repeat(1,minmax(220px,1fr))}
    .filter-label{font-size:12px;font-weight:600;letter-spacing:.4px;text-transform:uppercase;color:#6b7280;margin-bottom:6px}
    .selectize-input,.form-control{min-height:42px;border-radius:10px}
    .selectize-input{padding:10px 12px}
    .card{background:#fff;border:1px solid #eaecef;border-radius:16px;padding:14px;box-shadow:0 2px 10px rgba(0,0,0,.05);margin-bottom:12px}
    .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}
    .grid-4{display:grid;grid-template-columns:repeat(4,1fr);gap:12px}
    .metric-value{font-size:28px;font-weight:800;color:#111827;margin:2px 0 0}
    .metric-sub{font-size:12px;color:#6b7280;margin-top:2px}
  "))),
  div(class="wrap",
      h3("FINAGRO"),
      div(class="data-note","Tendencias, indicadores y mapas — versión con estilo moderno."),
      
      tabsetPanel(
        id = "tabs_finagro", type = "tabs",
        
        tabPanel(
          "Tendencias históricas", br(),
          # ----------- FILTROS (agregados: municipio y eslabón) -----------
          div(class="filters",
              div(class="filters-grid",
                  div(class="filter",
                      div(class="filter-label","Departamento"),
                      selectInput("depto_t1", NULL,
                                  choices = c("Todos", sort(unique(dptos_sf$NOM_DPTO))),
                                  selected = "Todos")),
                  div(class="filter",
                      div(class="filter-label","Municipio"),
                      selectInput("mpio_t1", NULL,
                                  choices = c("Todos", sort(unique(mpios_sf$NOM_MPIO))),
                                  selected = "Todos")),
                  div(class="filter",
                      div(class="filter-label","Eslabón de la cadena"),
                      selectInput("eslabon_t1", NULL,
                                  choices = c("Todos", sort(unique(na.omit(finagro_fast$ESLABON_CADENA)))),
                                  selected = "Todos"))
              )
          ),
          div(class="grid-4",
              div(class="card", div(class="card-title","¿Cuánto financiamiento hemos movilizado en el tiempo?"),
                  uiOutput("hist_monto_txt"), div(class="metric-sub","Suma de VALOR_CREDITO")),
              div(class="card", div(class="card-title","¿Cuántos créditos se han otorgado históricamente?"),
                  uiOutput("hist_creditos_txt"), div(class="metric-sub","Suma de NUMERO_CREDITO")),
              div(class="card", div(class="card-title","¿Cuál ha sido el monto típico por crédito?"),
                  uiOutput("hist_prom_txt"), div(class="metric-sub","Monto / Nº créditos")),
              div(class="card", div(class="card-title","¿Qué tan presentes están las mujeres en el crédito?"),
                  uiOutput("hist_mujeres_txt"), div(class="metric-sub","% sobre total de créditos"))
          ),
          fluidRow(
            column(6, div(class="card", div(class="card-title","¿Cómo evoluciona el monto total año a año?"),
                          plotlyOutput("hist_monto_total", height = 330))),
            column(6, div(class="card", div(class="card-title","¿Quiénes lideran por tipo de productor a lo largo del tiempo?"),
                          plotlyOutput("hist_tipo_productor", height = 330)))
          ),
          fluidRow(
            column(6, div(class="card", div(class="card-title","¿Cómo cambia el financiamiento por sexo / tipo de persona?"),
                          plotlyOutput("hist_sexo", height = 330))),
            column(6, div(class="card", div(class="card-title","¿Cómo varía el monto promedio por crédito?"),
                          plotlyOutput("hist_promedio", height = 330)))
          )
        ),
        
        tabPanel(
          "Indicadores", br(),
          div(class="filters",
              div(class="filters-grid",
                  div(class="filter",
                      div(class="filter-label","Año"),
                      selectInput("ano", NULL, choices = sort(unique(finagro_fast$ano)),
                                  selected = max(finagro_fast$ano))),
                  div(class="filter",
                      div(class="filter-label","Tipo de productor"),
                      selectInput("productor", NULL,
                                  choices = c("Todos", sort(unique(finagro_fast$TIPO_PRODUCTOR))),
                                  selected = "Todos")),
                  div(class="filter",
                      div(class="filter-label","Sexo / Tipo de persona"),
                      selectInput("sexo", NULL,
                                  choices = c("Todos", "Hombre", "Mujer", "Jurídico"),
                                  selected = "Todos")),
                  div(class="filter",
                      div(class="filter-label","Departamento"),
                      selectInput("depto_t2", NULL,
                                  choices = c("Todos", sort(unique(dptos_sf$NOM_DPTO))),
                                  selected = "Todos"))
              )
          ),
          div(class="grid-4",
              div(class="card", div(class="card-title","Total créditos"),
                  uiOutput("ind_total_creditos_txt")),
              div(class="card", div(class="card-title","Monto total"),
                  uiOutput("ind_total_monto_txt")),
              div(class="card", div(class="card-title","Monto promedio por crédito"),
                  uiOutput("ind_monto_prom_txt")),
              div(class="card", div(class="card-title","Créditos a mujeres"),
                  uiOutput("ind_pct_mujeres_txt"))
          ),
          fluidRow(
            column(6, div(class="card", div(class="card-title","Evolución mensual"),
                          plotlyOutput("serie_tiempo", height = 330))),
            column(6, div(class="card", div(class="card-title","Top 5 líneas de crédito (Monto)"),
                          plotlyOutput("top_lineas", height = 330)))
          ),
          fluidRow(
            column(6, div(class="card", div(class="card-title","Créditos por eslabón de cadena"),
                          plotlyOutput("eslabon_cadena", height = 330))),
            column(6, div(class="card", div(class="card-title","Flujo Línea → Eslabón"),
                          sankeyNetworkOutput("sankey", height = "330px")))
          )
        ),
        
        tabPanel(
          "Mapas", br(),
          div(class="filters",
              div(class="filters-grid maps",
                  div(class="filter",
                      div(class="filter-label","Selecciona un departamento"),
                      selectInput("depto_sel_m", NULL,
                                  choices = c("Todos", sort(unique(mapa_depto$NOM_DPTO))),
                                  selected = "Todos"))
              )
          ),
          fluidRow(
            column(7, div(class="card", div(class="card-title","Mapa interactivo"),
                          leafletOutput("mapa_m", height = 600))),
            column(5, div(class="card", div(class="card-title","Top 5"),
                          plotOutput("top5_m", height = 600)))
          )
        )
      )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  
  # ---- Dependencia MUNICIPIO según DEPARTAMENTO (1er tab) ----
  observeEvent(input$depto_t1, {
    if (is.null(input$depto_t1) || input$depto_t1 == "Todos") {
      updateSelectInput(session, "mpio_t1",
                        choices = c("Todos", sort(unique(mpios_sf$NOM_MPIO))),
                        selected = "Todos")
    } else {
      cod <- dptos_sf$COD_DPTO2[dptos_sf$NOM_DPTO == input$depto_t1][1]
      mpios <- mpios_sf %>% filter(COD_DPTO2 == cod) %>% pull(NOM_MPIO) %>% unique() %>% sort()
      updateSelectInput(session, "mpio_t1",
                        choices = c("Todos", mpios),
                        selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  # ---------- Base para Tendencias (filtra por depto, mpio, eslabón) ----------
  base_t1 <- reactive({
    df <- finagro_fast %>%
      left_join(dptos_sf %>% sf::st_drop_geometry() %>% select(COD_DPTO2, NOM_DPTO), by = "COD_DPTO2") %>%
      left_join(mpios_sf %>% sf::st_drop_geometry() %>% select(COD_MUN5, NOM_MPIO), by = "COD_MUN5")
    
    # Departamento
    if (!is.null(input$depto_t1) && input$depto_t1 != "Todos") {
      df <- df %>% filter(NOM_DPTO == input$depto_t1)
    }
    # Municipio (ya condicionado por el depto si corresponde)
    if (!is.null(input$mpio_t1) && input$mpio_t1 != "Todos") {
      df <- df %>% filter(NOM_MPIO == input$mpio_t1)
    }
    # Eslabón
    if (!is.null(input$eslabon_t1) && input$eslabon_t1 != "Todos") {
      df <- df %>% filter(ESLABON_CADENA == input$eslabon_t1)
    }
    df
  })
  
  # Métricas (histórico)
  output$hist_monto_txt <- renderUI({
    tags$div(class="metric-value", fmt_mmilM(sum(base_t1()$VALOR_CREDITO, na.rm=TRUE)))
  })
  output$hist_creditos_txt <- renderUI({
    tags$div(class="metric-value", fmt_int(sum(base_t1()$NUMERO_CREDITO, na.rm=TRUE)))
  })
  output$hist_prom_txt <- renderUI({
    df <- base_t1(); n <- sum(df$NUMERO_CREDITO, na.rm=TRUE); m <- sum(df$VALOR_CREDITO, na.rm=TRUE)
    tags$div(class="metric-value", fmt_cop(if(n>0) m/n else 0))
  })
  output$hist_mujeres_txt <- renderUI({
    df <- base_t1(); tot <- sum(df$NUMERO_CREDITO, na.rm=TRUE); muj <- sum(df$NUMERO_CREDITO[df$SEXO2=="Mujer"], na.rm=TRUE)
    pct <- if (tot>0) 100*muj/tot else 0
    tags$div(class="metric-value", paste0(number(pct, accuracy=0.1, decimal.mark=","), "%"))
  })
  
  # ---------- Colores para el 1er tab ----------
  col_monto_total <- pal_story[1]
  col_promedio    <- pal_story[2]
  col_sets        <- pal_story
  
  # Gráfico: Monto total por año (línea + marcadores) — color fijo #c49000
  output$hist_monto_total <- renderPlotly({
    df <- base_t1() %>% group_by(ano) %>% summarise(monto=sum(VALOR_CREDITO,na.rm=TRUE)/1e9, .groups="drop")
    plot_ly(df, x=~ano, y=~monto, type="scatter", mode="lines+markers",
            name="Monto total", line=list(color=col_monto_total), marker=list(color=col_monto_total)) %>%
      layout(xaxis=list(title="Año"),
             yaxis=list(title="Miles de Millones"),
             legend=list(orientation="h"))
  })
  
  # Gráfico: Monto por tipo de productor — colores de pal_story asignados a categorías
  output$hist_tipo_productor <- renderPlotly({
    df <- base_t1() %>% group_by(ano, TIPO_PRODUCTOR) %>%
      summarise(monto=sum(VALOR_CREDITO,na.rm=TRUE)/1e9, .groups="drop")
    tipos <- sort(unique(df$TIPO_PRODUCTOR))
    cols  <- setNames(rep(col_sets, length.out=length(tipos)), tipos)
    
    plt <- plot_ly()
    for (tp in tipos){
      dfi <- df %>% filter(TIPO_PRODUCTOR==tp)
      plt <- plt %>% add_trace(data=dfi, x=~ano, y=~monto, type="scatter", mode="lines",
                               name=tp, line=list(color=cols[tp]),
                               hovertemplate="<b>%{fullData.name}</b><br>Año %{x}<br>Monto %{y:.2f} Mil M<extra></extra>")
    }
    plt %>% layout(xaxis=list(title="Año"), yaxis=list(title="Miles de Millones"),
                   legend=list(orientation="h"))
  })
  
  # Gráfico: Monto por sexo / persona — colores de pal_story
  output$hist_sexo <- renderPlotly({
    df <- base_t1() %>% group_by(ano, SEXO2) %>%
      summarise(monto=sum(VALOR_CREDITO,na.rm=TRUE)/1e9, .groups="drop")
    sexos <- sort(unique(df$SEXO2))
    cols  <- setNames(rep(col_sets, length.out=length(sexos)), sexos)
    
    plt <- plot_ly()
    for (sx in sexos){
      dfi <- df %>% filter(SEXO2==sx)
      plt <- plt %>% add_trace(data=dfi, x=~ano, y=~monto, type="scatter", mode="lines",
                               name=sx, line=list(color=cols[sx]),
                               hovertemplate="<b>%{fullData.name}</b><br>Año %{x}<br>Monto %{y:.2f} Mil M<extra></extra>")
    }
    plt %>% layout(xaxis=list(title="Año"), yaxis=list(title="Miles de Millones"),
                   legend=list(orientation="h"))
  })
  
  # Gráfico: Monto promedio por crédito — color #fbc02d
  output$hist_promedio <- renderPlotly({
    df <- base_t1() %>%
      group_by(ano) %>%
      summarise(m=sum(VALOR_CREDITO,na.rm=TRUE), n=sum(NUMERO_CREDITO,na.rm=TRUE), .groups="drop") %>%
      mutate(prom = ifelse(n>0, m/n, 0)/1e9)
    plot_ly(df, x=~ano, y=~prom, type="scatter", mode="lines+markers",
            name="Promedio", line=list(color=col_promedio), marker=list(color=col_promedio)) %>%
      layout(xaxis=list(title="Año"), yaxis=list(title="Promedio (Miles de Millones)"),
             legend=list(orientation="h"))
  })
  
  # ---------- INDICADORES (sin cambios) ----------
  base_filtrada <- reactive({
    df <- finagro_fast %>% filter(ano == input$ano)
    if (!is.null(input$productor) && input$productor!="Todos") df <- df %>% filter(TIPO_PRODUCTOR==input$productor)
    if (!is.null(input$sexo)      && input$sexo!="Todos")      df <- df %>% filter(SEXO2==input$sexo)
    if (!is.null(input$depto_t2)  && input$depto_t2!="Todos")  df <- df %>%
        filter(COD_DPTO2 == (dptos_sf$COD_DPTO2[dptos_sf$NOM_DPTO==input$depto_t2][1]))
    df
  })
  
  output$ind_total_creditos_txt <- renderUI({
    tags$div(class="metric-value", fmt_int(sum(base_filtrada()$NUMERO_CREDITO, na.rm=TRUE)))
  })
  output$ind_total_monto_txt <- renderUI({
    tags$div(class="metric-value", fmt_mmilM(sum(base_filtrada()$VALOR_CREDITO, na.rm=TRUE)))
  })
  output$ind_monto_prom_txt <- renderUI({
    n <- sum(base_filtrada()$NUMERO_CREDITO, na.rm=TRUE); m <- sum(base_filtrada()$VALOR_CREDITO, na.rm=TRUE)
    tags$div(class="metric-value", fmt_cop(if(n>0) m/n else 0))
  })
  output$ind_pct_mujeres_txt <- renderUI({
    df <- base_filtrada(); tot <- sum(df$NUMERO_CREDITO, na.rm=TRUE); muj <- sum(df$NUMERO_CREDITO[df$SEXO2=="Mujer"], na.rm=TRUE)
    tags$div(class="metric-value", paste0(number(if (tot>0) 100*muj/tot else 0, accuracy=0.1, decimal.mark=","), "%"))
  })
  
  # Serie mensual (Indicadores)
  output$serie_tiempo <- renderPlotly({
    df <- base_filtrada() %>%
      group_by(mes) %>%
      summarise(creditos=sum(NUMERO_CREDITO,na.rm=TRUE),
                monto=sum(VALOR_CREDITO,na.rm=TRUE)/1e9, .groups="drop") %>%
      mutate(mes_lbl=factor(mes,levels=1:12,labels=mes_labels))
    plot_ly(df, x=~mes_lbl) %>%
      add_lines(y=~creditos, name="Créditos", yaxis="y1") %>%
      add_lines(y=~monto,    name="Miles de Millones", yaxis="y2", line=list(dash="dot")) %>%
      layout(yaxis=list(title="Número de créditos"),
             yaxis2=list(title="Miles de Millones", overlaying="y", side="right"),
             legend=list(orientation="h"))
  })
  
  # Top 5 líneas (Indicadores)
  output$top_lineas <- renderPlotly({
    df <- base_filtrada() %>%
      group_by(LINEA_CREDITO) %>%
      summarise(monto=sum(VALOR_CREDITO,na.rm=TRUE), .groups="drop") %>%
      arrange(desc(monto)) %>% slice_head(n=5) %>%
      mutate(monto_m = monto/1e9, linea = reorder(LINEA_CREDITO, monto_m))
    plot_ly(df, x=~monto_m, y=~linea, type="bar", orientation="h",
            hovertemplate = "<b>%{y}</b><br>Monto: %{x:.2f} Mil M<extra></extra>") %>%
      layout(xaxis=list(title="Miles de Millones"), yaxis=list(title=NULL))
  })
  
  output$eslabon_cadena <- renderPlotly({
    df <- base_filtrada() %>%
      group_by(ESLABON_CADENA) %>%
      summarise(creditos=sum(NUMERO_CREDITO,na.rm=TRUE), .groups="drop") %>%
      mutate(eslabon = reorder(ESLABON_CADENA, creditos))
    plot_ly(df, x=~creditos, y=~eslabon, type="bar", orientation="h",
            hovertemplate="<b>%{y}</b><br>Créditos: %{x:.0f}<extra></extra>")
  })
  
  output$sankey <- renderSankeyNetwork({
    df_links <- base_filtrada() %>%
      group_by(LINEA_CREDITO,ESLABON_CADENA) %>%
      summarise(value=sum(VALOR_CREDITO,na.rm=TRUE)/1e9, .groups="drop")
    nodos <- data.frame(name=c(unique(df_links$LINEA_CREDITO), unique(df_links$ESLABON_CADENA)))
    df_links$source <- match(df_links$LINEA_CREDITO, nodos$name)-1
    df_links$target <- match(df_links$ESLABON_CADENA, nodos$name)-1
    sankeyNetwork(Links=df_links, Nodes=nodos, Source="source", Target="target", Value="value",
                  NodeID="name", units="Miles de Millones", fontSize=10, nodeWidth=30)
  })
  
  # ---------- MAPAS (sin cambios) ----------
  output$mapa_m <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lng=-74.3, lat=4.6, zoom=5)
  })
  
  mapa_sel <- reactive({
    if (is.null(input$depto_sel_m) || input$depto_sel_m=="Todos") {
      list(tipo="depto", shp=mapa_depto)
    } else {
      cod <- mapa_depto$COD_DPTO2[mapa_depto$NOM_DPTO==input$depto_sel_m][1]
      shp <- mapa_mpio %>% dplyr::filter(COD_DPTO2==cod)
      list(tipo="mpio", shp=shp)
    }
  })
  
  observeEvent(list(input$tabs_finagro, input$depto_sel_m), {
    req(input$tabs_finagro == "Mapas")
    sel <- mapa_sel(); shp <- sel$shp
    req(inherits(shp, "sf"), nrow(shp) > 0, !all(sf::st_is_empty(shp)))
    
    if (sel$tipo=="depto") {
      pal <- pal_bin_safe("YlGnBu", shp$monto, bins=5)
      bb  <- sf::st_bbox(shp)
      leafletProxy("mapa_m") %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(
          data=shp, layerId=~COD_DPTO2,
          fillColor=~pal(monto), color="#444", weight=0.6, opacity=1, fillOpacity=0.8,
          smoothFactor = 0.5,
          label=~paste0(NOM_DPTO,"<br>Prom. monto: ",fmt_milM(monto),
                        "<br>Prom. créditos: ",fmt_int(creditos)),
          labelOptions=labelOptions(direction="auto")
        ) %>%
        addLegend("bottomright", pal=pal, values=shp$monto,
                  title="Promedio monto crédito<br>(Miles de millones)") %>%
        fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    } else {
      pal <- pal_bin_safe("YlOrRd", shp$monto, bins=5)
      bb  <- sf::st_bbox(shp)
      leafletProxy("mapa_m") %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(
          data=shp, layerId=~COD_MUN5,
          fillColor=~pal(monto), color="#666", weight=0.4, opacity=1, fillOpacity=0.7,
          smoothFactor = 0.5,
          label=~paste0(NOM_MPIO,"<br>Prom. monto: ",fmt_milM(monto),
                        "<br>Prom. créditos: ",fmt_int(creditos)),
          labelOptions=labelOptions(direction="auto")
        ) %>%
        addLegend("bottomright", pal=pal, values=shp$monto,
                  title="Promedio monto crédito<br>(Miles de millones)") %>%
        fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$mapa_m_shape_click, {
    click <- input$mapa_m_shape_click
    if (!is.null(click$id) && nchar(click$id)==2){
      nom <- mapa_depto$NOM_DPTO[mapa_depto$COD_DPTO2==click$id][1]
      if (!is.na(nom)) updateSelectInput(session, "depto_sel_m", selected=nom)
    }
  })
  
  output$top5_m <- renderPlot({
    sel <- mapa_sel(); shp <- sel$shp
    if (nrow(shp)==0) return(NULL)
    if (sel$tipo=="depto") {
      df <- shp %>% sf::st_drop_geometry() %>% transmute(nombre=NOM_DPTO, valor=monto)
      titulo <- "Top 5 departamentos — Prom. monto"
    } else {
      df <- shp %>% sf::st_drop_geometry() %>% transmute(nombre=NOM_MPIO, valor=monto)
      titulo <- paste0("Top 5 municipios — ", input$depto_sel_m)
    }
    df <- df %>% filter(is.finite(valor)) %>% arrange(desc(valor)) %>% slice_head(n=5)
    ggplot(df, aes(x=reorder(nombre, valor), y=valor/1e9)) +
      geom_col(fill="#3b82f6") +
      geom_text(aes(label=number(valor/1e9, big.mark=".", decimal.mark=",", accuracy=0.1)),
                hjust=-0.1, size=4) +
      coord_flip() +
      labs(title=titulo, x=NULL, y="Monto (miles de millones)") +
      scale_y_continuous(labels=label_number(big.mark=".", decimal.mark=","), expand=expansion(mult=c(0,0.1))) +
      theme_minimal(base_size=12) + theme(plot.title=element_text(face="bold"))
  })
}

# =========================================================
# RUN
# =========================================================
shinyApp(ui, server)


