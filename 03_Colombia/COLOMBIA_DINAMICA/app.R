# app.R — COLOMBIA_DINAMICA (SIPSA PRECIOS) — TAB 2 comparativa 2 mapas
# =========================================================
# TAB 1: Serie anual nacional + tarjetas
# TAB 2: Comparativos departamentales (2 mapas) — MISMO INDICADOR
# - Lock de año (año fijo para ambos mapas)
# - Indicador global (Precio / YoY) para ambos mapas
# - Filtros por mapa: Grupo, Alimento, Central mayorista, Año (si no está lock)
# =========================================================

suppressWarnings({
  library(shiny); library(bslib)
  library(dplyr); library(stringi); library(htmltools)
  library(plotly); library(scales)
  library(sf); library(leaflet)
})

options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---------------- Rutas ----------------
data_dir <- "data"

# =========================================================
# Alturas
# =========================================================
H_PLOT <- 650
H_MAP  <- 600

# =========================================================
# Colores UI
# =========================================================
COL_UI   <- "#ffe082"   # <- borde global
COL_PLOT <- "#008f38"

PLOTLY_HOVERLABEL <- list(
  bgcolor     = "#ffffff",
  bordercolor = "rgba(17,24,39,0.25)",
  font        = list(color = "#111827", size = 12)
)

# =========================================================
# Helpers
# =========================================================
parse_num_co <- function(x){
  if (is.null(x)) return(NA_real_)
  if (is.numeric(x)) return(as.numeric(x))
  x <- as.character(x)
  x <- stringi::stri_replace_all_fixed(x, "\u00A0", " ")
  x <- stringi::stri_trim_both(x)
  x <- gsub("\\.", "", x)
  x <- gsub(",", ".", x, fixed = TRUE)
  x <- gsub("[^0-9\\-\\.]", "", x)
  suppressWarnings(as.numeric(x))
}

safe_chr <- function(x) as.character(x)

norm_dep2 <- function(x){
  x <- gsub("\\D","",as.character(x))
  x[nchar(x)==0] <- NA
  stringi::stri_pad_left(x, 2, "0")
}

pick_col_simple <- function(nms, cands){
  for (pat in cands) {
    idx <- which(tolower(nms) == tolower(pat) | grepl(pat, nms, ignore.case = TRUE))[1]
    if (length(idx) && !is.na(idx)) return(nms[idx])
  }
  NA_character_
}

pick_year_col <- function(nms){
  pick_col_simple(nms, c("ano","año","anio","year","anio_ref","año_ref"))
}

pick_dep_col <- function(nms){
  cands <- c(
    "dpto_ccdgo","cod_dpto","cod_depto","dpto_cod","cod_dep","codigo_dep",
    "COD_DANE_DPTO_D","COD_DANE_DPTO","cod_dane_dpto_d","cod_dane_dpto","cod_dane_dep",
    "DPTO_CCDGO","DPTO_COD"
  )
  pick_col_simple(nms, cands)
}

pick_dep_name_col <- function(nms){
  cands <- c(
    "dpto_cnmb","nom_dpto","nombre_dpto","nombre_depto",
    "DEPARTAMENTO_D","departamento_d","DEPARTAMENTO","departamento"
  )
  pick_col_simple(nms, cands)
}

fmt_num_es <- function(x, digits = 0){
  scales::number(x, accuracy = 10^-digits, big.mark = ".", decimal.mark = ",")
}
fmt_price <- function(x, digits = 0){
  ifelse(is.na(x), "NA", paste0("$", fmt_num_es(x, digits)))
}
fmt_pct <- function(x, digits = 1){
  ifelse(is.na(x), "NA", paste0(fmt_num_es(x, digits), "%"))
}
fmt_short <- function(x, digits = 1){
  vapply(x, function(v){
    if (is.na(v) || !is.finite(v)) return(NA_character_)
    av <- abs(v)
    if (av >= 1e6) paste0(fmt_num_es(v/1e6, digits), "M")
    else if (av >= 1e3) paste0(fmt_num_es(v/1e3, digits), "K")
    else fmt_num_es(v, digits)
  }, FUN.VALUE = character(1))
}

safe_growth <- function(curr, prev){
  if (is.na(curr) || is.na(prev) || !is.finite(curr) || !is.finite(prev) || prev == 0) return(NA_real_)
  (curr - prev) / prev
}

year_ticks_2 <- function(years){
  yrs <- sort(unique(as.integer(years[is.finite(years)])))
  if (!length(yrs)) return(NULL)
  y_min <- min(yrs); y_max <- max(yrs)
  tick0 <- floor(y_min/2)*2
  ticks <- seq(tick0, y_max, by = 2)
  if (tail(ticks, 1) != y_max) ticks <- c(ticks, y_max)
  ticks
}

empty_ts_plot <- function(msg = "Sin datos para los filtros actuales."){
  plotly::plotly_empty(type = "scatter", mode = "markers") %>%
    plotly::layout(
      annotations = list(
        x = 0.5, y = 0.5, text = as.character(msg),
        showarrow = FALSE, xref = "paper", yref = "paper",
        font = list(size = 14),
        bgcolor = "#ffffff",
        bordercolor = "rgba(17,24,39,0.20)",
        borderwidth = 1
      ),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      margin = list(l = 10, r = 10, b = 10, t = 10),
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

force_plot_color <- function(p, col = COL_PLOT){
  if (is.null(p) || is.null(p$x) || is.null(p$x$data)) return(p)
  for (i in seq_along(p$x$data)) {
    tr <- p$x$data[[i]]
    if (!is.null(tr$type) && tr$type == "scatter") {
      if (is.null(tr$line)) tr$line <- list()
      tr$line$color <- col
      if (is.null(tr$marker)) tr$marker <- list()
      tr$marker$color <- col
    }
    p$x$data[[i]] <- tr
  }
  p
}

lbl <- function(x) div(class = "filter-label", x)

# =========================================================
# CARGA BASE SIPSA PRECIOS
# =========================================================
precios_raw <- tryCatch({
  p <- file.path(data_dir, "042_DANE_SIPSA-Precios_nacional.rds")
  if (!file.exists(p)) stop("No existe: ./data/042_DANE_SIPSA-Precios_nacional.rds")
  df <- readRDS(p)
  if (is.null(df) || !nrow(df)) stop("Base vacía.")
  df
}, error = function(e){
  message("Carga SIPSA precios error: ", e$message)
  NULL
})

precios_df <- NULL
choices_grupo  <- c("Sin datos"="__NONE__")
choices_fuente <- c("Sin datos"="__NONE__")
choices_year_all <- integer(0)

if (!is.null(precios_raw)) {
  nms <- names(precios_raw)
  
  col_grupo  <- pick_col_simple(nms, c("Grupo","grupo","GRUPO"))
  col_alim   <- pick_col_simple(nms, c("Alimento","alimento","ALIMENTO","Producto","producto"))
  col_prec   <- pick_col_simple(nms, c("PrecioKg","precioKg","PRECIOKG","precio_kg","precio","Precio"))
  col_ano    <- pick_year_col(nms)
  col_dep    <- pick_dep_col(nms)
  col_depnm  <- pick_dep_name_col(nms)
  col_fuente <- pick_col_simple(nms, c("Fuente","fuente","FUENTE","Central","central","Central_Mayorista","central_mayorista"))
  
  if (any(is.na(c(col_grupo, col_alim, col_prec, col_ano, col_dep, col_fuente)))) {
    stop("Faltan columnas clave (Grupo/Alimento/Fuente/PrecioKg/Año/COD_DPTO).")
  }
  
  precios_df <- precios_raw %>%
    transmute(
      Grupo        = stringi::stri_trim_both(safe_chr(.data[[col_grupo]])),
      Alimento     = stringi::stri_trim_both(safe_chr(.data[[col_alim]])),
      Fuente       = stringi::stri_trim_both(safe_chr(.data[[col_fuente]])),
      PrecioKg     = parse_num_co(.data[[col_prec]]),
      ano          = as.integer(parse_num_co(.data[[col_ano]])),
      COD_DPTO2    = norm_dep2(.data[[col_dep]]),
      DEPARTAMENTO = {
        if (!is.na(col_depnm) && col_depnm %in% nms) {
          x <- safe_chr(.data[[col_depnm]])
          x <- stringi::stri_trans_general(x, "Latin-ASCII")
          x <- toupper(stringi::stri_trim_both(x))
          x
        } else {
          NA_character_
        }
      }
    ) %>%
    filter(
      !is.na(ano),
      is.finite(PrecioKg), PrecioKg > 0,
      !is.na(COD_DPTO2), nzchar(COD_DPTO2),
      !is.na(Grupo), Grupo != "",
      !is.na(Alimento), Alimento != "",
      !is.na(Fuente), Fuente != ""
    )
  
  if (nrow(precios_df)) {
    gvals <- sort(unique(precios_df$Grupo))
    fvals <- sort(unique(precios_df$Fuente))
    choices_grupo  <- stats::setNames(gvals, gvals)
    choices_fuente <- c("Todas las centrales mayoristas"="__ALL__", stats::setNames(fvals, fvals))
    choices_year_all <- sort(unique(precios_df$ano))
  }
}

# =========================================================
# SHAPE departamental (para mapas)
# =========================================================
ruta_shp_dep <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")
shp_dep <- tryCatch({
  if (!file.exists(ruta_shp_dep)) return(NULL)
  x <- sf::st_read(ruta_shp_dep, quiet = TRUE)
  dep_code_col_shp <- pick_dep_col(names(x))
  if (is.na(dep_code_col_shp)) return(NULL)
  
  # simplifica en CRS proyectado (evita warning lon/lat)
  x_3116 <- sf::st_transform(x, 3116)
  x_3116 <- sf::st_simplify(x_3116, dTolerance = 1500, preserveTopology = TRUE)
  x <- sf::st_transform(x_3116, 4326)
  
  x$cod_dep_join <- norm_dep2(x[[dep_code_col_shp]])
  x
}, error = function(e) NULL)

dep_name_col_shp <- if (!is.null(shp_dep)) pick_dep_name_col(names(shp_dep)) else NA_character_

# =========================================================
# Indicadores
# =========================================================
indicador_choices <- c(
  "Precio promedio (actual) — COP/kg" = "precio",
  "Tasa de crecimiento interanual (YoY) — %" = "yoy"
)

# =========================================================
# UI helpers (tarjetas)
# =========================================================
card_box <- function(title, body){
  div(class = "kpi-card",
      div(class = "kpi-title", title),
      div(class = "kpi-body", body))
}
kv_table <- function(rows){
  div(class = "kpi-table",
      lapply(rows, function(r){
        div(class = "kpi-row",
            div(class = "kpi-k", r$k),
            div(class = "kpi-v", r$v))
      }))
}
na_txt <- function() span("NA", style = "color:#6b7280;font-weight:600;")

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    primary = COL_UI,
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight")
  ),
  tags$head(
    tags$style(HTML(paste0("
      :root{ --ecv-bdr:", COL_UI, "; }
      *{ box-sizing:border-box; }
      body{ background:#fff; font-family:'Inter', system-ui, -apple-system, 'Segoe UI', sans-serif; }
      .wrap{ max-width:1280px; margin:0 auto; padding:24px 24px 40px; }

      /* (CAMBIO) todo con borde amarillo */
      .outer-card{
        background:#fff; border:1px solid var(--ecv-bdr); border-radius:18px;
        padding:18px 18px 20px; box-shadow:0 2px 10px rgba(0,0,0,.05);
      }

      /* (CAMBIO) antes verde: ahora borde amarillo */
      .filters-box{
        background:#fff; border:1px solid var(--ecv-bdr);
        border-radius:14px; padding:14px 16px; margin-bottom:12px;
        box-shadow:0 2px 6px rgba(0,0,0,.03);
      }

      .filter-label{ font-size:13px; font-weight:600; color:#111827; margin-bottom:4px; }

      .filters-box .shiny-input-container{ margin-bottom:0 !important; width:100% !important; }
      .filters-box .form-select{
        border:2px solid var(--ecv-bdr) !important; border-radius:10px !important;
        background:#fff !important; height:46px; min-height:46px; color:#111827;
      }

      .filters-stack{ display:flex; flex-direction:column; gap:10px; }

      .ts-grid{ display:grid; grid-template-columns: 2.15fr 1fr; gap:16px; align-items:stretch; }
      .ts-left{ display:flex; flex-direction:column; gap:10px; }
      .ts-right{ display:flex; flex-direction:column; gap:10px; }

      .kpi-card{ background:#fff; border:1px solid var(--ecv-bdr); border-radius:16px; padding:12px 14px; box-shadow:0 2px 6px rgba(0,0,0,.03); }
      .kpi-title{ font-weight:700; font-size:12px; letter-spacing:.06em; text-transform:uppercase; color:#6b7280; margin-bottom:8px; }
      .kpi-body{ font-size:13px; color:#111827; line-height:1.38; }
      .kpi-table{ display:flex; flex-direction:column; gap:6px; }

      /* (CAMBIO) antes dashed verde: ahora dashed amarillo */
      .kpi-row{ display:flex; justify-content:space-between; align-items:baseline; border-top:1px dashed var(--ecv-bdr); padding-top:6px; }
      .kpi-row:first-child{ border-top:none; padding-top:0; }

      .kpi-k{ color:#374151; font-size:12px; }
      .kpi-v{ color:#111827; font-size:12px; font-weight:700; }

      .nav-tabs .nav-link{ font-weight:700; }
      .nav-tabs .nav-link.active{ font-weight:800; }

      /* TAB 2 comparativa */
      .filters-grid-2{ display:grid; grid-template-columns:1fr 1fr; gap:10px; align-items:end; }
      .grid-2maps{ display:grid; grid-template-columns:1fr 1fr; gap:20px; align-items:stretch; }
      .map-panel{
        display:flex; flex-direction:column; gap:10px;
        background:#fff; border:1px solid var(--ecv-bdr);
        border-radius:16px; padding:16px 16px 18px; box-shadow:0 2px 6px rgba(0,0,0,.03);
      }
      .card-title-small{ font-weight:700; font-size:12px; letter-spacing:.06em; text-transform:uppercase; color:#6b7280; margin-bottom:4px; }

      /* (CAMBIO) borde del contenedor leaflet */
      .leaflet-container{
        border:1px solid var(--ecv-bdr) !important;
        border-radius:14px !important;
      }

      /* (CAMBIO) controles/leyendas leaflet con borde amarillo */
      .leaflet-control, .leaflet-bar, .leaflet-control-layers{
        border:1px solid var(--ecv-bdr) !important;
        box-shadow:0 2px 6px rgba(0,0,0,.08) !important;
        border-radius:10px !important;
      }
      .leaflet-bar a{
        border-bottom:1px solid var(--ecv-bdr) !important;
      }
      .leaflet-bar a:last-child{
        border-bottom:none !important;
      }

      @media (max-width:980px){
        .ts-grid{ grid-template-columns:1fr; }
        .grid-2maps{ grid-template-columns:1fr; }
        .filters-grid-2{ grid-template-columns:1fr; }
      }
    ")))
  ),
  div(
    class = "wrap",
    div(
      class = "outer-card",
      tabsetPanel(
        id = "tabs",
        
        # =========================================================
        # TAB 1 — SERIE
        # =========================================================
        tabPanel(
          title = "Serie nacional",
          
          div(
            class = "filters-box",
            div(lbl("")),
            div(
              class = "filters-stack",
              div(lbl("¿Qué indicador de dinámica quieres analizar?"),
                  selectInput("s_indicador", label = NULL, choices = indicador_choices, selected = "precio")),
              div(lbl("Grupo"),
                  selectInput("s_grupo", label = NULL, choices = choices_grupo,
                              selected = if (length(choices_grupo)) names(choices_grupo)[1] else "__NONE__")),
              div(lbl("Alimento"),
                  selectInput("s_alimento", label = NULL, choices = c("Cargando..."="__WAIT__"), selected = "__WAIT__")),
              div(lbl("Central mayorista"),
                  selectInput("s_fuente", label = NULL, choices = choices_fuente, selected = "__ALL__"))
            )
          ),
          
          div(
            class = "ts-grid",
            div(class = "ts-left",
                plotlyOutput("ts_plot", height = paste0(H_PLOT, "px"))
            ),
            div(class = "ts-right",
                uiOutput("summary_cards")
            )
          )
        ),
        
        # =========================================================
        # TAB 2 — COMPARATIVOS DEPARTAMENTALES (2 mapas)
        # =========================================================
        tabPanel(
          title = "Mapa departamental",
          
          # Bloque superior (lock año + indicador global)
          div(
            class = "filters-box",
            div(
              class = "filters-grid-2",
              div(
                lbl("¿Usar un año fijo para ambos mapas?"),
                checkboxInput("lock_years", label = NULL, value = FALSE)
              ),
              div(
                lbl("Año fijo (cuando está activado)"),
                uiOutput("fixed_year_ui")
              )
            ),
            div(style="margin-top:10px;",
                lbl("Indicador (aplica a ambos mapas)"),
                selectInput("map_indicador", label = NULL, choices = indicador_choices, selected = "precio")
            )
          ),
          
          # 2 mapas
          div(
            class = "grid-2maps",
            
            # MAPA 1
            div(
              class = "map-panel",
              div(class = "card-title-small", "INDICADOR 1"),
              div(
                class = "filters-box",
                div(class = "filters-stack",
                    div(lbl("Grupo"),
                        selectInput("m1_grupo", label = NULL, choices = choices_grupo,
                                    selected = if (length(choices_grupo)) names(choices_grupo)[1] else "__NONE__")),
                    div(lbl("Alimento"),
                        selectInput("m1_alimento", label = NULL, choices = c("Cargando..."="__WAIT__"), selected = "__WAIT__")),
                    div(lbl("Central mayorista"),
                        selectInput("m1_fuente", label = NULL, choices = choices_fuente, selected = "__ALL__")),
                    div(lbl("¿Qué año analizamos?"),
                        uiOutput("m1_year_ui"))
                )
              ),
              leafletOutput("map_1", height = paste0(H_MAP, "px"))
            ),
            
            # MAPA 2
            div(
              class = "map-panel",
              div(class = "card-title-small", "INDICADOR 2"),
              div(
                class = "filters-box",
                div(class = "filters-stack",
                    div(lbl("Grupo"),
                        selectInput("m2_grupo", label = NULL, choices = choices_grupo,
                                    selected = if (length(choices_grupo)) names(choices_grupo)[1] else "__NONE__")),
                    div(lbl("Alimento"),
                        selectInput("m2_alimento", label = NULL, choices = c("Cargando..."="__WAIT__"), selected = "__WAIT__")),
                    div(lbl("Central mayorista"),
                        selectInput("m2_fuente", label = NULL, choices = choices_fuente, selected = "__ALL__")),
                    div(lbl("¿Qué año analizamos?"),
                        uiOutput("m2_year_ui"))
                )
              ),
              leafletOutput("map_2", height = paste0(H_MAP, "px"))
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
  
  # ---------------------------
  # Inicializa alimentos TAB 1
  # ---------------------------
  alimentos_por <- function(g, f){
    if (is.null(precios_df) || !nrow(precios_df)) return(character(0))
    df <- precios_df %>% filter(Grupo == g)
    if (!is.null(f) && f != "__ALL__") df <- df %>% filter(Fuente == f)
    df %>% distinct(Alimento) %>% arrange(Alimento) %>% pull(Alimento)
  }
  
  observeEvent(list(input$s_grupo, input$s_fuente), {
    if (is.null(precios_df) || !nrow(precios_df)) return()
    g <- input$s_grupo %||% names(choices_grupo)[1]
    f <- input$s_fuente %||% "__ALL__"
    a_vals <- alimentos_por(g, f)
    if (!length(a_vals)) a_vals <- sort(unique(precios_df$Alimento))
    sel <- input$s_alimento
    if (is.null(sel) || !(sel %in% a_vals)) sel <- a_vals[1] %||% "__NONE__"
    updateSelectInput(session, "s_alimento", choices = stats::setNames(a_vals, a_vals), selected = sel)
  }, ignoreInit = TRUE)
  
  observeEvent(TRUE, {
    if (is.null(precios_df) || !nrow(precios_df)) return()
    g <- names(choices_grupo)[1] %||% "__NONE__"
    f <- "__ALL__"
    a_vals <- alimentos_por(g, f)
    if (!length(a_vals)) a_vals <- sort(unique(precios_df$Alimento))
    updateSelectInput(session, "s_alimento", choices = stats::setNames(a_vals, a_vals), selected = a_vals[1] %||% "__NONE__")
  }, once = TRUE)
  
  # ---------------------------
  # DF filtrado TAB 1 (Serie)
  # ---------------------------
  df_filt_s <- reactive({
    if (is.null(precios_df) || !nrow(precios_df)) return(NULL)
    g <- input$s_grupo %||% ""
    a <- input$s_alimento %||% ""
    f <- input$s_fuente %||% "__ALL__"
    if (!nzchar(g) || !nzchar(a) || g == "__NONE__" || a %in% c("__WAIT__","__NONE__")) return(NULL)
    df <- precios_df %>% filter(Grupo == g, Alimento == a)
    if (!is.null(f) && f != "__ALL__") df <- df %>% filter(Fuente == f)
    if (!nrow(df)) return(NULL)
    df
  })
  
  ts_nacional <- reactive({
    df <- df_filt_s()
    if (is.null(df) || !nrow(df)) return(NULL)
    df %>%
      group_by(ano) %>%
      summarise(
        precio_prom = mean(PrecioKg, na.rm = TRUE),
        n_obs       = dplyr::n(),
        .groups = "drop"
      ) %>%
      arrange(ano) %>%
      mutate(yoy_pct = 100 * (precio_prom / dplyr::lag(precio_prom) - 1))
  })
  
  output$ts_plot <- renderPlotly({
    ts <- ts_nacional()
    if (is.null(ts) || !nrow(ts)) return(empty_ts_plot("Sin datos para los filtros seleccionados."))
    
    indic <- input$s_indicador %||% "precio"
    y_col <- if (indic == "yoy") "yoy_pct" else "precio_prom"
    y_lab <- if (indic == "yoy") "Tasa de crecimiento interanual (%)" else "COP por kg (promedio nacional)"
    
    ticks_x <- year_ticks_2(ts$ano)
    
    customdata <- if (indic == "yoy") {
      paste0("YoY: ", fmt_pct(ts$yoy_pct, 1), " | Precio: ", fmt_price(ts$precio_prom, 0), " /kg | n=", fmt_num_es(ts$n_obs, 0))
    } else {
      paste0("Precio: ", fmt_price(ts$precio_prom, 0), " /kg | n=", fmt_num_es(ts$n_obs, 0))
    }
    
    hovertemplate <- if (indic == "yoy") {
      "<b>Año:</b> %{x}<br><b>YoY:</b> %{y:.1f}%<br>%{customdata}<extra></extra>"
    } else {
      "<b>Año:</b> %{x}<br><b>Precio promedio:</b> %{customdata}<extra></extra>"
    }
    
    p <- plotly::plot_ly(ts, x = ~ano) %>%
      plotly::add_trace(
        y = ts[[y_col]],
        type = "scatter", mode = "lines+markers",
        line = list(width = 2),
        marker = list(size = 6),
        customdata = customdata,
        hovertemplate = hovertemplate
      ) %>%
      plotly::layout(
        xaxis = list(title = "", tickmode = "array", tickvals = ticks_x, ticktext = ticks_x, showgrid = FALSE, automargin = TRUE),
        yaxis = list(title = y_lab, showgrid = FALSE, automargin = TRUE, rangemode = "tozero"),
        margin = list(l = 70, r = 40, t = 10, b = 60),
        hovermode = "x unified",
        hoverlabel = PLOTLY_HOVERLABEL,
        plot_bgcolor  = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
    
    force_plot_color(p, COL_PLOT)
  })
  
  current_summary <- reactive({
    g <- input$s_grupo %||% ""
    a <- input$s_alimento %||% ""
    f <- input$s_fuente %||% "__ALL__"
    indic <- input$s_indicador %||% "precio"
    ts <- ts_nacional()
    
    f_txt <- if (is.null(f) || f == "__ALL__") "todas las centrales" else f
    
    out <- list(
      story = "Sin datos para construir la lectura analítica con los filtros actuales.",
      last_rows = list(list(k="Año", v=na_txt()), list(k="Indicador", v=na_txt()), list(k="Observaciones (n)", v=na_txt())),
      growth_rows = list(list(k="Año anterior", v=na_txt()), list(k="Referencia", v=na_txt()))
    )
    if (is.null(ts) || !nrow(ts)) return(out)
    
    last_year <- max(ts$ano, na.rm = TRUE)
    last <- ts %>% filter(ano == last_year) %>% slice(1)
    prev <- ts %>% filter(ano == (last_year - 1)) %>% slice(1)
    gr_price <- safe_growth(last$precio_prom, if (nrow(prev)) prev$precio_prom else NA_real_)
    
    if (indic == "precio") {
      out$story <- sprintf(
        "Para %s — %s (%s), en %d el precio promedio nacional fue %s por kg (n=%s). La variación interanual del precio es %s.",
        g, a, f_txt, last_year,
        fmt_price(last$precio_prom, 0),
        fmt_num_es(last$n_obs, 0),
        fmt_pct(100*gr_price, 1)
      )
      out$last_rows <- list(
        list(k="Año", v=as.character(last_year)),
        list(k="Precio promedio (COP/kg)", v=fmt_price(last$precio_prom, 0)),
        list(k="Observaciones (n)", v=fmt_num_es(last$n_obs, 0))
      )
      out$growth_rows <- list(
        list(k="Año anterior", v=if (nrow(prev)) as.character(last_year - 1) else "NA"),
        list(k="Crecimiento precio (YoY)", v=fmt_pct(100*gr_price, 1))
      )
      return(out)
    }
    
    out$story <- sprintf(
      "Para %s — %s (%s), en %d la tasa de crecimiento interanual del precio fue %s (precio promedio: %s por kg; n=%s).",
      g, a, f_txt, last_year,
      fmt_pct(last$yoy_pct, 1),
      fmt_price(last$precio_prom, 0),
      fmt_num_es(last$n_obs, 0)
    )
    out$last_rows <- list(
      list(k="Año", v=as.character(last_year)),
      list(k="Crecimiento interanual (YoY)", v=fmt_pct(last$yoy_pct, 1)),
      list(k="Precio promedio (COP/kg)", v=fmt_price(last$precio_prom, 0))
    )
    out$growth_rows <- list(
      list(k="Año anterior", v=if (nrow(prev)) as.character(last_year - 1) else "NA"),
      list(k="Δ YoY (p.p.)", v=ifelse(is.na(last$yoy_pct) || is.na(prev$yoy_pct), "NA", paste0(fmt_num_es(last$yoy_pct - prev$yoy_pct, 1), " p.p.")))
    )
    out
  })
  
  output$summary_cards <- renderUI({
    s <- current_summary()
    tagList(
      card_box("Lectura analítica", div(s$story)),
      card_box("Último año (niveles)", kv_table(s$last_rows)),
      card_box("Comparativo (último año vs. anterior)", kv_table(s$growth_rows))
    )
  })
  
  # =========================================================
  # TAB 2 — COMPARATIVOS (2 MAPAS)
  # =========================================================
  
  years_for_indic <- reactive({
    yrs <- choices_year_all
    indic <- input$map_indicador %||% "precio"
    if (indic == "yoy" && length(yrs) >= 2) {
      yrs <- yrs[yrs > min(yrs, na.rm = TRUE)]
    }
    yrs
  })
  
  output$fixed_year_ui <- renderUI({
    lock <- input$lock_years %||% FALSE
    yrs <- years_for_indic()
    if (!isTRUE(lock)) return(tags$span("Active el año fijo para usar este filtro.", style="font-size:12px;color:#9ca3af;"))
    if (!length(yrs)) return(tags$span("Sin años disponibles.", style="font-size:12px;color:#9ca3af;"))
    selectInput("fixed_year", label = NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  output$m1_year_ui <- renderUI({
    lock <- input$lock_years %||% FALSE
    yrs <- years_for_indic()
    if (isTRUE(lock)) return(tags$span("Año controlado por el filtro global.", style="font-size:12px;color:#6b7280;"))
    if (!length(yrs)) return(tags$span("Sin años disponibles.", style="font-size:12px;color:#9ca3af;"))
    selectInput("m1_year", label = NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  output$m2_year_ui <- renderUI({
    lock <- input$lock_years %||% FALSE
    yrs <- years_for_indic()
    if (isTRUE(lock)) return(tags$span("Año controlado por el filtro global.", style="font-size:12px;color:#6b7280;"))
    if (!length(yrs)) return(tags$span("Sin años disponibles.", style="font-size:12px;color:#9ca3af;"))
    selectInput("m2_year", label = NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  update_foods_map <- function(prefix){
    g <- input[[paste0(prefix, "_grupo")]] %||% names(choices_grupo)[1]
    f <- input[[paste0(prefix, "_fuente")]] %||% "__ALL__"
    a_vals <- alimentos_por(g, f)
    if (!length(a_vals) && !is.null(precios_df)) a_vals <- sort(unique(precios_df$Alimento))
    sel <- input[[paste0(prefix, "_alimento")]]
    if (is.null(sel) || !(sel %in% a_vals)) sel <- a_vals[1] %||% "__NONE__"
    updateSelectInput(session, paste0(prefix, "_alimento"), choices = stats::setNames(a_vals, a_vals), selected = sel)
  }
  
  observeEvent(list(input$m1_grupo, input$m1_fuente), {
    if (is.null(precios_df) || !nrow(precios_df)) return()
    update_foods_map("m1")
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$m2_grupo, input$m2_fuente), {
    if (is.null(precios_df) || !nrow(precios_df)) return()
    update_foods_map("m2")
  }, ignoreInit = TRUE)
  
  observeEvent(TRUE, {
    if (is.null(precios_df) || !nrow(precios_df)) return()
    update_foods_map("m1")
    update_foods_map("m2")
  }, once = TRUE)
  
  build_map_data <- function(grupo, alimento, fuente, year_sel, indicador){
    if (is.null(shp_dep) || is.null(precios_df) || !nrow(precios_df)) return(NULL)
    if (is.null(year_sel) || is.na(year_sel)) return(NULL)
    if (is.null(grupo) || is.null(alimento) || grupo == "__NONE__" || alimento %in% c("__WAIT__","__NONE__")) return(NULL)
    
    yr <- as.integer(year_sel)
    
    df <- precios_df %>% filter(Grupo == grupo, Alimento == alimento)
    if (!is.null(fuente) && fuente != "__ALL__") df <- df %>% filter(Fuente == fuente)
    if (!nrow(df)) return(NULL)
    
    dep_year <- df %>%
      group_by(ano, cod_dep_join = COD_DPTO2) %>%
      summarise(precio_prom = mean(PrecioKg, na.rm = TRUE), n_obs = dplyr::n(), .groups = "drop")
    
    if (indicador == "yoy") {
      dep_now  <- dep_year %>% filter(ano == yr) %>% select(cod_dep_join, precio_now = precio_prom, n_obs)
      dep_prev <- dep_year %>% filter(ano == (yr - 1)) %>% select(cod_dep_join, precio_prev = precio_prom)
      dd <- dep_now %>% left_join(dep_prev, by = "cod_dep_join") %>% mutate(valor = 100 * (precio_now / precio_prev - 1))
    } else {
      dd <- dep_year %>% filter(ano == yr) %>% mutate(valor = precio_prom)
    }
    
    if (!nrow(dd)) return(NULL)
    left_join(shp_dep, dd, by = "cod_dep_join")
  }
  
  output$map_1 <- leaflet::renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(lng = -74, lat = 4.5, zoom = 5)
  })
  output$map_2 <- leaflet::renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(lng = -74, lat = 4.5, zoom = 5)
  })
  
  make_bin_labels <- function(bins, indic){
    bins <- sort(unique(as.numeric(bins)))
    if (length(bins) < 2) return(character(0))
    vapply(seq_len(length(bins) - 1), function(i){
      a <- bins[i]; b <- bins[i+1]
      if (indic == "yoy") {
        if (i == 1) paste0(fmt_num_es(a, 1), "%–", fmt_num_es(b, 1), "%")
        else        paste0(">", fmt_num_es(a, 1), "%–", fmt_num_es(b, 1), "%")
      } else {
        if (i == 1) paste0(fmt_short(a, 1), "–", fmt_short(b, 1))
        else        paste0(">", fmt_short(a, 1), "–", fmt_short(b, 1))
      }
    }, FUN.VALUE = character(1))
  }
  
  update_map <- function(map_id, md, indic, palette_name){
    if (is.null(md) || !"valor" %in% names(md)) {
      leafletProxy(map_id) %>% clearShapes() %>% clearControls()
      return(invisible(NULL))
    }
    
    vals <- md$valor
    vals <- vals[is.finite(vals)]
    if (!length(vals)) {
      leafletProxy(map_id) %>% clearShapes() %>% clearControls()
      return(invisible(NULL))
    }
    
    qs <- stats::quantile(vals, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
    bins <- sort(unique(as.numeric(qs)))
    if (length(bins) < 3) bins <- sort(unique(pretty(range(vals, na.rm = TRUE), n = 6)))
    
    pal <- leaflet::colorBin(palette = palette_name, domain = vals, bins = bins, na.color = "#f5f5f5")
    
    dep_name <- if (!is.na(dep_name_col_shp) && dep_name_col_shp %in% names(md)) as.character(md[[dep_name_col_shp]]) else as.character(md$cod_dep_join)
    
    labels <- if (indic == "yoy") {
      sprintf("<strong>%s</strong><br/>YoY: %s<br/>n=%s",
              dep_name, fmt_pct(md$valor, 1), fmt_num_es(md$n_obs, 0)) %>% lapply(HTML)
    } else {
      sprintf("<strong>%s</strong><br/>Precio: %s /kg<br/>n=%s",
              dep_name, fmt_price(md$valor, 0), fmt_num_es(md$n_obs, 0)) %>% lapply(HTML)
    }
    
    legend_labels <- make_bin_labels(bins, indic)
    mids <- (bins[-1] + bins[-length(bins)]) / 2
    legend_cols <- pal(mids)
    
    legend_title <- if (indic == "yoy") "Tasa de crecimiento interanual (%)" else "Precio promedio (COP/kg)"
    
    leafletProxy(map_id, data = md) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor   = ~pal(valor),
        weight      = 1,
        opacity     = 1,
        # (CAMBIO) borde del polígono amarillo
        color       = COL_UI,
        fillOpacity = 0.85,
        # (CAMBIO) borde de highlight amarillo
        highlightOptions = highlightOptions(weight = 2, color = COL_UI, fillOpacity = 0.92, bringToFront = TRUE),
        label = labels
      ) %>%
      addLegend(colors = legend_cols, labels = legend_labels, opacity = 0.8, title = legend_title, position = "bottomright") %>%
      setView(lng = -74, lat = 4.5, zoom = 5)
  }
  
  map1_data <- reactive({
    indic <- input$map_indicador %||% "precio"
    lock  <- input$lock_years %||% FALSE
    yr <- if (isTRUE(lock)) input$fixed_year else input$m1_year
    
    build_map_data(
      grupo     = input$m1_grupo,
      alimento  = input$m1_alimento,
      fuente    = input$m1_fuente,
      year_sel  = yr,
      indicador = indic
    )
  })
  
  map2_data <- reactive({
    indic <- input$map_indicador %||% "precio"
    lock  <- input$lock_years %||% FALSE
    yr <- if (isTRUE(lock)) input$fixed_year else input$m2_year
    
    build_map_data(
      grupo     = input$m2_grupo,
      alimento  = input$m2_alimento,
      fuente    = input$m2_fuente,
      year_sel  = yr,
      indicador = indic
    )
  })
  
  observe({
    indic <- input$map_indicador %||% "precio"
    update_map("map_1", map1_data(), indic, palette_name = "Greens")
  })
  
  observe({
    indic <- input$map_indicador %||% "precio"
    update_map("map_2", map2_data(), indic, palette_name = "Blues")
  })
}

shinyApp(ui, server)
