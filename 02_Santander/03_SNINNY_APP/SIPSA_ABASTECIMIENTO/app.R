# =========================================================
# SIPSA ABASTECIMIENTO — Treemap Jerárquico (Grupo -> Alimento)
# - Caja 1 (mapa) en blanco, Caja 2 (serie) en blanco
# - Caja 3: UN solo treemap con GRUPOS conteniendo ALIMENTOS
# - Carga relativa: ./data/041_DANE_SIPSA-Abast.rds
# - ROBUSTO a nombres de columnas
# =========================================================

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(dplyr); library(stringr); library(janitor); library(scales)
  library(readr)
  library(highcharter)
})

options(stringsAsFactors = FALSE, scipen = 999)

validate <- shiny::validate
need     <- shiny::need
`%||%`   <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# =========================================================
# 0) Rutas robustas (runApp() y source())  ->  TODO desde ./data
# =========================================================
app_root <- tryCatch({
  of <- sys.frame(1)$ofile
  if (!is.null(of)) dirname(normalizePath(of, winslash = "/", mustWork = TRUE))
  else normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}, error = function(e){
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
})

data_dir <- file.path(app_root, "data")
rds_path <- file.path(data_dir, "041_DANE_SIPSA-Abast.rds")
stopifnot(file.exists(rds_path))

# =========================================================
# 1) Helpers
# =========================================================
title_case_es <- function(x){
  x <- str_trim(as.character(x))
  x <- str_to_lower(x)
  x <- str_replace_all(x, "\\s+", " ")
  small_words <- c("de","del","la","las","los","y","e","o","u","a","en","el","al","da","do","das","dos")
  vapply(x, function(s){
    if (is.na(s) || s == "") return(NA_character_)
    w <- strsplit(s, "\\s+")[[1]]
    w <- vapply(seq_along(w), function(i){
      if (i > 1 && w[i] %in% small_words) w[i] else str_to_title(w[i], locale = "es")
    }, character(1))
    paste(w, collapse = " ")
  }, character(1))
}

parse_num_co <- function(x){
  readr::parse_number(
    as.character(x),
    locale = readr::locale(grouping_mark = ".", decimal_mark = ",")
  )
}

pick_first <- function(nms, cands){
  hit <- cands[cands %in% nms]
  if (!length(hit)) NA_character_ else hit[1]
}

req_col <- function(nms, cands, label){
  hit <- pick_first(nms, cands)
  if (is.na(hit)) {
    stop(paste0(
      "No encuentro columna para: ", label, "\n",
      "Busqué: ", paste(cands, collapse = ", "), "\n",
      "Columnas disponibles: ", paste(nms, collapse = ", ")
    ))
  }
  hit
}

fmt_num <- function(x, accuracy = 1){
  scales::number(x, accuracy = accuracy, big.mark = ".", decimal.mark = ",")
}

# =========================================================
# 2) Cargar RDS y mapear columnas reales
# =========================================================
sipsa_raw <- readRDS(rds_path)
sipsa <- janitor::clean_names(sipsa_raw)
nms  <- names(sipsa)

ycol <- req_col(nms, c("ano","anio","year"), "AÑO (ano/anio/year)")
mcol <- req_col(nms, c("mes","month"), "MES (mes/month)")
gcol <- req_col(nms, c("grupo","grupo_alimento","grupo_alimentos","grupo_de_alimento"), "GRUPO")
pcol <- req_col(nms, c("alimento","producto","item","articulo","artículo"), "ALIMENTO/PRODUCTO")
qcol <- req_col(nms, c("cantkg_total","cant_kg_total","cantidad_kg","cantidadkg","cantkg","kg_total","cant_total_kg"), "CANTIDAD KG")

dep_o_col <- pick_first(nms, c("departamento_o","depto_o","departamento_origen","depto_origen"))
mun_o_col <- pick_first(nms, c("municipio_o","mpio_o","municipio_origen","mpio_origen"))

# =========================================================
# 3) Base estándar
# =========================================================
base_sipsa <- sipsa %>%
  transmute(
    anio = suppressWarnings(as.integer(.data[[ycol]])),
    mes  = suppressWarnings(as.integer(.data[[mcol]])),
    departamento_o = if (!is.na(dep_o_col)) title_case_es(.data[[dep_o_col]]) else "Todos",
    municipio_o    = if (!is.na(mun_o_col)) title_case_es(.data[[mun_o_col]]) else "Todos",
    grupo          = title_case_es(.data[[gcol]]),
    alimento       = title_case_es(.data[[pcol]]),
    kg             = parse_num_co(.data[[qcol]])
  ) %>%
  filter(is.finite(kg), kg > 0)

# =========================================================
# 4) UI
# =========================================================
ui <- fluidPage(
  theme = bslib::bs_theme(
    version      = 5,
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight")
  ),
  tags$head(
    tags$style(HTML("
      body{ background:#ffffff; }
      :root{ --brand-border:#a1d99b; }

      .wrap{ max-width:1360px; margin:0 auto; padding:16px 20px 32px; }
      h2#app-title{ text-align:center; margin-top:10px; margin-bottom:10px; font-weight:800; letter-spacing:.3px; }

      .card{
        background:#ffffff;
        border:1px solid var(--brand-border) !important;
        border-radius:16px;
        padding:12px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
        margin-bottom:12px;
      }

      .card-title{ font-weight:700; font-size:16px; margin-bottom:8px; color:#111827; }
      .filter-label{ font-weight:500; font-size:14px; margin-bottom:4px; color:#000000; }

      .filters-grid{
        display:grid;
        grid-template-columns: 1fr 1fr 1fr 1.3fr;
        column-gap:16px; row-gap:10px; align-items:end;
      }
      @media (max-width: 992px){ .filters-grid{ grid-template-columns: 1fr 1fr; } }
      @media (max-width: 576px){ .filters-grid{ grid-template-columns: 1fr; } }

      .form-select, .bootstrap-select > .dropdown-toggle, .selectize-input{
        border:1px solid var(--brand-border) !important;
        border-radius:10px !important;
        box-shadow:none !important;
        font-size:14px; font-weight:500; color:#000000;
        background-color:#ffffff !important;
      }

      .viz-grid{ display:grid; grid-template-columns: 1.05fr 1fr; gap:12px; align-items:stretch; }
      .viz-right{ display:grid; grid-template-rows: 1fr 1fr; gap:12px; }
      .card-mapa, .card-viz{ display:flex; flex-direction:column; }
      .blank-box{ flex:1; min-height:350px; border-radius:12px; background:#ffffff; }
      .card-viz .html-widget{ flex:1; min-height:350px; border-radius:12px; }

      @media (max-width: 768px){
        .viz-grid{ grid-template-columns: 1fr; }
        .viz-right{ grid-template-rows: auto auto; }
      }
    "))
  ),
  
  div(
    class = "wrap",
    h2("SIPSA Abastecimiento — Treemap Grupo ⟶ Alimento", id = "app-title"),
    
    # ======== Filtros ========
    div(
      class = "card",
      div(
        class = "filters-grid",
        div(class="filter-block", div(class="filter-label","¿Qué año?"), uiOutput("anio_ui")),
        div(class="filter-block", div(class="filter-label","¿Qué mes?"), uiOutput("mes_ui")),
        div(class="filter-block", div(class="filter-label","Nivel de origen"),
            selectInput("nivel_origen", NULL, choices = c("Departamento","Municipio"), selected = "Departamento")
        ),
        div(class="filter-block", div(class="filter-label","¿Sale de (origen)?"), uiOutput("origen_ui"))
      )
    ),
    
    # ======== Visualizaciones ========
    div(
      class = "viz-grid",
      # Caja 1: MAPA (BLANCO)
      div(class="card card-mapa", div(class="card-title",""), div(class="blank-box")),
      # Caja 2: SERIE (BLANCO) + Caja 3: TREEMAP JERÁRQUICO (ACTIVO)
      div(
        class="viz-right",
        div(class="card card-viz", div(class="card-title",""), div(class="blank-box")),
        div(
          class="card card-viz",
          div(class="card-title", strong("Treemap jerárquico: Grupo contiene Alimentos")),
          highchartOutput("treemap_nested", height = "100%")
        )
      )
    )
  )
)

# =========================================================
# 5) SERVER
# =========================================================
server <- function(input, output, session){
  
  years  <- sort(unique(base_sipsa$anio[is.finite(base_sipsa$anio)]))
  months <- sort(unique(base_sipsa$mes[is.finite(base_sipsa$mes)]))
  
  output$anio_ui <- renderUI({
    selectInput("anio", NULL,
                choices  = c("Todos" = "Todos", years),
                selected = if (length(years)) max(years) else "Todos"
    )
  })
  
  output$mes_ui <- renderUI({
    selectInput("mes", NULL,
                choices  = c("Todos" = "Todos", months),
                selected = "Todos"
    )
  })
  
  output$origen_ui <- renderUI({
    opts <- if (is.null(input$nivel_origen) || input$nivel_origen == "Departamento") {
      sort(unique(na.omit(base_sipsa$departamento_o)))
    } else {
      sort(unique(na.omit(base_sipsa$municipio_o)))
    }
    
    pickerInput(
      "origen", NULL,
      choices  = c("Todos", opts),
      selected = "Todos",
      options  = list(`live-search` = TRUE, size = 7)
    )
  })
  
  datos_filtrados <- reactive({
    df <- base_sipsa
    if (!is.null(input$anio) && input$anio != "Todos") df <- df %>% filter(anio == as.integer(input$anio))
    if (!is.null(input$mes)  && input$mes  != "Todos") df <- df %>% filter(mes  == as.integer(input$mes))
    
    if (is.null(input$nivel_origen) || input$nivel_origen == "Departamento") {
      df <- df %>% mutate(origen_lbl = departamento_o)
    } else {
      df <- df %>% mutate(origen_lbl = municipio_o)
    }
    
    if (!is.null(input$origen) && input$origen != "Todos") df <- df %>% filter(origen_lbl == input$origen)
    
    validate(need(nrow(df) > 0, "Sin datos para los filtros actuales"))
    df
  })
  
  output$treemap_nested <- renderHighchart({
    df <- datos_filtrados()
    
    # Totales por (grupo, alimento)
    det <- df %>%
      group_by(grupo, alimento) %>%
      summarise(kg = sum(kg, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(kg), kg > 0)
    
    validate(need(nrow(det) > 0, "Sin datos para treemap"))
    
    # Totales por grupo
    gtot <- det %>%
      group_by(grupo) %>%
      summarise(kg_grupo = sum(kg, na.rm = TRUE), .groups = "drop")
    
    total_all <- sum(gtot$kg_grupo, na.rm = TRUE)
    
    # ids seguros (sin espacios raros)
    gtot <- gtot %>%
      mutate(
        gid = paste0("G_", str_replace_all(str_to_lower(grupo), "[^a-z0-9]+", "_")),
        pct_grupo = ifelse(total_all > 0, kg_grupo / total_all, NA_real_),
        tooltip_text = paste0(
          "Grupo: ", grupo,
          "<br>Kg: ", formatC(kg_grupo, format = "f", digits = 0, big.mark = "."),
          "<br>Participación total: ", round(pct_grupo * 100, 1), "%"
        )
      )
    
    det2 <- det %>%
      left_join(gtot %>% select(grupo, gid, kg_grupo), by = "grupo") %>%
      mutate(
        fid = paste0("F_", str_replace_all(str_to_lower(paste0(grupo,"_",alimento)), "[^a-z0-9]+", "_")),
        pct_in_group = ifelse(kg_grupo > 0, kg / kg_grupo, NA_real_),
        tooltip_text = paste0(
          "Grupo: ", grupo,
          "<br>Alimento: ", alimento,
          "<br>Kg: ", formatC(kg, format = "f", digits = 0, big.mark = "."),
          "<br>% dentro del grupo: ", round(pct_in_group * 100, 1), "%"
        )
      )
    
    # Data para Highcharts treemap jerárquico (padres + hijos)
    parents <- gtot %>%
      transmute(
        id = gid,
        parent = NA_character_,
        name = grupo,
        value = as.numeric(kg_grupo),
        colorValue = as.numeric(pct_grupo * 100),
        tooltip_text = tooltip_text
      )
    
    children <- det2 %>%
      transmute(
        id = fid,
        parent = gid,
        name = alimento,
        value = as.numeric(kg),
        colorValue = as.numeric(pct_in_group * 100),
        tooltip_text = tooltip_text
      )
    
    tm <- bind_rows(parents, children)
    
    low_color  <- "#2E7730"
    high_color <- "#007CC3"
    
    hchart(
      tm, "treemap",
      hcaes(id = id, parent = parent, name = name, value = value, color = colorValue)
    ) %>%
      hc_title(text = "") %>%
      hc_colorAxis(minColor = low_color, maxColor = high_color) %>%
      hc_tooltip(pointFormat = "{point.tooltip_text}") %>%
      hc_plotOptions(
        treemap = list(
          layoutAlgorithm = "squarified",
          allowTraversingTree = FALSE,   # <- se mantiene “contenido”, sin drill a pantalla completa
          levelIsConstant = FALSE,
          dataLabels = list(enabled = TRUE)
        ),
        series = list(
          animation = FALSE
        )
      ) %>%
      hc_levels(list(
        list(
          level = 1,
          dataLabels = list(enabled = TRUE, style = list(fontWeight = "700")),
          borderWidth = 3,
          borderColor = "#ffffff"
        ),
        list(
          level = 2,
          dataLabels = list(enabled = TRUE, style = list(fontWeight = "500")),
          borderWidth = 1,
          borderColor = "#ffffff"
        )
      )) %>%
      hc_caption(text = "Color: % (Nivel 1 = participación total; Nivel 2 = % dentro del grupo)", align = "center",
                 verticalAlign = "bottom", y = -10)
  })
}

shinyApp(ui, server)


