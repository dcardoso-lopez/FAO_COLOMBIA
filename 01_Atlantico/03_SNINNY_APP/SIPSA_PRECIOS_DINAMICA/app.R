# app.R
# =========================================================
# SIPSA_PRECIOS_DINAMICA — 3 pestañas (Storytelling)
# (FOCO = ATLÁNTICO)
# =========================================================

DPTO_FOCO_NOMBRE    <- "Atlántico"
DPTO_FOCO_COD       <- "08"
CAPITAL_FOCO_NOMBRE <- "Barranquilla"
MUNI_FOCO_COD       <- "08001"  # Barranquilla

APP_TITLE <- paste0("")

# ------------------------------
# Paquetes (NO instalar aquí)
# ------------------------------
pkgs <- c(
  "shiny","bslib",
  "dplyr","stringr","janitor","scales",
  "readr","stringi","htmltools",
  "lubridate","plotly","ggplot2"
)

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    "Faltan paquetes requeridos (NO los instalo automáticamente):\n- ",
    paste(missing, collapse = "\n- "),
    "\n\nInstálalos manualmente y vuelve a ejecutar."
  )
}

suppressWarnings({
  library(shiny); library(bslib)
  library(dplyr); library(stringr); library(janitor); library(scales)
  library(readr); library(stringi); library(htmltools)
  library(lubridate); library(plotly); library(ggplot2)
})

options(stringsAsFactors = FALSE, scipen = 999)

# ✅ Blindaje contra funciones enmascaradas
select    <- dplyr::select
mutate    <- dplyr::mutate
filter    <- dplyr::filter
summarise <- dplyr::summarise
arrange   <- dplyr::arrange
left_join <- dplyr::left_join

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
validate <- shiny::validate
need     <- shiny::need

# =========================================================
# Rutas robustas (runApp / source) + DATA DIR RELATIVO
# =========================================================
app_root <- tryCatch({
  of <- sys.frame(1)$ofile
  if (!is.null(of)) dirname(normalizePath(of, winslash = "/", mustWork = TRUE))
  else normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}, error = function(e){
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
})

data_dir <- file.path(app_root, "data")

# =========================================================
# Helpers
# =========================================================
title_case_es <- function(x){
  x <- stringr::str_trim(as.character(x))
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  small_words <- c("de","del","la","las","los","y","e","o","u","a","en","el","al","da","do","das","dos")
  vapply(x, function(s){
    if (is.na(s) || s == "") return(NA_character_)
    w <- strsplit(s, "\\s+")[[1]]
    w <- vapply(seq_along(w), function(i){
      if (i > 1 && w[i] %in% small_words) w[i] else stringr::str_to_title(w[i], locale = "es")
    }, character(1))
    paste(w, collapse = " ")
  }, character(1))
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

pad_dpto <- function(x){
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\D", "")
  x <- ifelse(nchar(x) == 0, NA_character_, x)
  stringr::str_pad(x, width = 2, side = "left", pad = "0")
}

pad_munic <- function(x){
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\D", "")
  x <- ifelse(nchar(x) == 0, NA_character_, x)
  stringr::str_pad(x, width = 5, side = "left", pad = "0")
}

fmt_price_co <- function(x, digits = 0){
  scales::number(x, big.mark=".", decimal.mark=",", accuracy = 10^(-digits))
}
fmt_money_co <- function(x, digits = 0){
  ifelse(is.finite(x), paste0("$", fmt_price_co(x, digits)), "NA")
}
fmt_pct_co <- function(x, digits = 1){
  ifelse(is.finite(x),
         paste0(scales::number(x, big.mark=".", decimal.mark=",", accuracy = 10^(-digits)), "%"),
         "NA")
}

is_all <- function(x){
  is.null(x) || length(x) == 0 || identical(x, "Todos")
}
filter_single <- function(df, col, sel){
  if (is_all(sel)) return(df)
  df %>% dplyr::filter(.data[[col]] == sel)
}

# =========================================================
# Finder genérico de RDS (LECTURA RELATIVA)
# =========================================================
rds_candidates <- function(stem){
  # stem: nombre SIN extensión
  c(
    file.path(data_dir, paste0(stem, ".rds")),
    file.path(data_dir, paste0(stem, ".RDS")),
    file.path(app_root, paste0(stem, ".rds")),
    file.path(app_root, paste0(stem, ".RDS")),
    file.path("data", paste0(stem, ".rds")),
    file.path("data", paste0(stem, ".RDS")),
    paste0(stem, ".rds"),
    paste0(stem, ".RDS")
  )
}

find_prices_rds <- function(paths){
  for (p in paths) {
    if (!is.na(p) && file.exists(p) && !dir.exists(p) && grepl("\\.rds$", tolower(p))) {
      return(normalizePath(p, winslash = "/", mustWork = TRUE))
    }
  }
  for (p in paths) {
    if (!is.na(p) && dir.exists(p)) {
      rds <- list.files(p, pattern="\\.rds$", full.names=TRUE, ignore.case=TRUE)
      if (length(rds)) {
        pick <- rds[grepl("042.*precios", basename(rds), ignore.case=TRUE)][1]
        if (is.na(pick)) pick <- rds[1]
        return(normalizePath(pick, winslash = "/", mustWork = TRUE))
      }
    }
  }
  NA_character_
}

# =========================================================
# 1) Cargar RDS TAB 1 (base original)
# =========================================================
rds_path_1 <- find_prices_rds(rds_candidates("042_DANE_SIPSA-Precios"))
if (is.na(rds_path_1)) {
  stop(
    "No encontré el archivo de precios TAB 1 (.rds).\n\n",
    "Solución rápida:\n",
    "1) Copia el .rds a: ", data_dir, "\n",
    "2) y nómbralo: 042_DANE_SIPSA-Precios.rds"
  )
}

raw1 <- readRDS(rds_path_1)
df1  <- janitor::clean_names(raw1)
nms1 <- names(df1)

ycol1   <- req_col(nms1, c("ano","anio","year"), "AÑO")
mcol1   <- req_col(nms1, c("mes","month"), "MES")
gcol1   <- req_col(nms1, c("grupo","grupo_alimento","grupo_alimentos"), "GRUPO")
acol1   <- req_col(nms1, c("alimento","producto","articulo"), "ALIMENTO")
pcol1   <- req_col(nms1, c("preciokg","precio_kg","precio","precio_promedio","valor"), "PRECIO (Kg)")
dpto1_c <- pick_first(nms1, c("cod_dane_dpto_d","dane_cod_dpto_d","cod_dpto_d","cod_depto_d","cod_dane_dpto"))

prices <- df1 %>%
  dplyr::transmute(
    anio       = suppressWarnings(as.integer(.data[[ycol1]])),
    mes        = suppressWarnings(as.integer(.data[[mcol1]])),
    grupo      = title_case_es(.data[[gcol1]]),
    alimento   = title_case_es(.data[[acol1]]),
    cod_dpto_d = if (!is.na(dpto1_c)) pad_dpto(.data[[dpto1_c]]) else NA_character_,
    precio_kg  = suppressWarnings(as.numeric(.data[[pcol1]]))
  ) %>%
  dplyr::filter(
    is.finite(anio), anio >= 2018,
    is.finite(mes), mes >= 1, mes <= 12,
    is.finite(precio_kg), precio_kg > 0
  ) %>%
  dplyr::mutate(fecha = as.Date(sprintf("%04d-%02d-01", anio, mes)))

# Filtrar Atlántico si hay cod depto destino (sin impedir NA)
if (!all(is.na(prices$cod_dpto_d))) {
  prices <- prices %>% dplyr::filter(is.na(cod_dpto_d) | cod_dpto_d == DPTO_FOCO_COD)
}

# =========================================================
# 2) Cargar RDS TAB 2 (base indice2 con más dptos)
# =========================================================
rds_path_2 <- find_prices_rds(rds_candidates("042_DANE_SIPSA-Precios_indice2"))
if (is.na(rds_path_2)) {
  stop(
    "No encontré el archivo de precios TAB 2 (.rds).\n\n",
    "Solución rápida:\n",
    "1) Copia el .rds a: ", data_dir, "\n",
    "2) y nómbralo: 042_DANE_SIPSA-Precios_indice2.rds"
  )
}

raw2 <- readRDS(rds_path_2)
df2  <- janitor::clean_names(raw2)
nms2 <- names(df2)

ycol2   <- req_col(nms2, c("ano","anio","year"), "AÑO")
gcol2   <- req_col(nms2, c("grupo","grupo_alimento","grupo_alimentos"), "GRUPO")
acol2   <- req_col(nms2, c("alimento","producto","articulo"), "ALIMENTO")
pcol2   <- req_col(nms2, c("preciokg","precio_kg","precio","precio_promedio","valor"), "PRECIO (Kg)")
dpto2_c <- pick_first(nms2, c("cod_dane_dpto_d","dane_cod_dpto_d","cod_dpto_d","cod_depto_d","cod_dane_dpto"))
dpto2_n <- pick_first(nms2, c("departamento_d","depto_d","departamento","depto"))

prices2 <- df2 %>%
  dplyr::transmute(
    anio        = suppressWarnings(as.integer(.data[[ycol2]])),
    grupo       = title_case_es(.data[[gcol2]]),
    alimento    = title_case_es(.data[[acol2]]),
    cod_dpto_d  = if (!is.na(dpto2_c)) pad_dpto(.data[[dpto2_c]]) else NA_character_,
    departamento_d = if (!is.na(dpto2_n)) title_case_es(.data[[dpto2_n]]) else NA_character_,
    precio_kg   = suppressWarnings(as.numeric(.data[[pcol2]]))
  ) %>%
  dplyr::filter(
    is.finite(anio), anio >= 2018,
    is.finite(precio_kg), precio_kg > 0
  ) %>%
  dplyr::mutate(
    departamento_d = dplyr::if_else(
      is.na(departamento_d) | departamento_d == "",
      dplyr::if_else(!is.na(cod_dpto_d), paste0("Dpto ", cod_dpto_d), NA_character_),
      departamento_d
    )
  ) %>%
  dplyr::filter(!is.na(departamento_d), departamento_d != "")

# =========================================================
# 3) Cargar RDS TAB 3 (base indice3 a nivel CIUDADES)
# =========================================================
rds_path_3 <- find_prices_rds(rds_candidates("042_DANE_SIPSA-Precios_indice3"))
if (is.na(rds_path_3)) {
  stop(
    "No encontré el archivo de precios TAB 3 (.rds).\n\n",
    "Solución rápida:\n",
    "1) Copia el .rds a: ", data_dir, "\n",
    "2) y nómbralo: 042_DANE_SIPSA-Precios_indice3.rds"
  )
}

raw3 <- readRDS(rds_path_3)
df3  <- janitor::clean_names(raw3)
nms3 <- names(df3)

ycol3   <- req_col(nms3, c("ano","anio","year"), "AÑO")
gcol3   <- req_col(nms3, c("grupo","grupo_alimento","grupo_alimentos"), "GRUPO")
acol3   <- req_col(nms3, c("alimento","producto","articulo"), "ALIMENTO")
pcol3   <- req_col(nms3, c("preciokg","precio_kg","precio","precio_promedio","valor"), "PRECIO (Kg)")

mun3_c  <- pick_first(nms3, c(
  "cod_dane_munic","cod_dane_municipio","cod_dane_mpio","cod_mpio","cod_munic",
  "cod_dane_munic_d","cod_dane_municipio_d","dane_cod_munic","dane_cod_mpio"
))
mun3_n  <- pick_first(nms3, c(
  "municipio","mpio","ciudad","cabecera","municipio_d","mpio_d","ciudad_d"
))

dpto3_c <- pick_first(nms3, c("cod_dane_dpto","cod_dane_dpto_d","dane_cod_dpto","cod_dpto","cod_depto"))
dpto3_n <- pick_first(nms3, c("departamento","departamento_d","depto","depto_d"))

plaza3  <- pick_first(nms3, c("plaza","plaza_mercado","plazamercado","central","central_mayorista","nombre_plaza","mercado","fuente","origen"))
tipo3   <- pick_first(nms3, c("tipo_plaza","tipo_mercado","categoria_plaza","tipo","tipo_central"))

prices3 <- df3 %>%
  dplyr::transmute(
    anio        = suppressWarnings(as.integer(.data[[ycol3]])),
    grupo       = title_case_es(.data[[gcol3]]),
    alimento    = title_case_es(.data[[acol3]]),
    cod_munic   = if (!is.na(mun3_c)) pad_munic(.data[[mun3_c]]) else NA_character_,
    municipio   = if (!is.na(mun3_n)) title_case_es(.data[[mun3_n]]) else NA_character_,
    cod_dpto    = if (!is.na(dpto3_c)) pad_dpto(.data[[dpto3_c]]) else NA_character_,
    departamento = if (!is.na(dpto3_n)) title_case_es(.data[[dpto3_n]]) else NA_character_,
    plaza       = if (!is.na(plaza3)) title_case_es(.data[[plaza3]]) else NA_character_,
    tipo_plaza  = if (!is.na(tipo3))  title_case_es(.data[[tipo3]]) else NA_character_,
    precio_kg   = suppressWarnings(as.numeric(.data[[pcol3]]))
  ) %>%
  dplyr::filter(
    is.finite(anio), anio >= 2018,
    is.finite(precio_kg), precio_kg > 0
  ) %>%
  dplyr::mutate(
    municipio = dplyr::if_else(
      is.na(municipio) | municipio == "",
      dplyr::if_else(!is.na(cod_munic), paste0("Mpio ", cod_munic), NA_character_),
      municipio
    ),
    departamento = dplyr::if_else(
      is.na(departamento) | departamento == "",
      dplyr::if_else(!is.na(cod_dpto), paste0("Dpto ", cod_dpto), NA_character_),
      departamento
    )
  ) %>%
  dplyr::filter(!is.na(municipio), municipio != "")

prices3 <- prices3 %>%
  dplyr::mutate(
    city_label = dplyr::if_else(
      !is.na(departamento) & departamento != "",
      paste0(municipio, " (", departamento, ")"),
      municipio
    )
  )

# =========================================================
# UI helpers (SIN plaza/fuente)
# =========================================================
filters_box_prices <- function(tag){
  id_anio  <- paste0("anio_ui_", tag)
  id_grupo <- paste0("grupo_ui_", tag)
  id_alim  <- paste0("alim_ui_", tag)
  
  div(
    class="filters",
    div(
      class="filters-grid-3",
      div(class="filter", div(class="filter-label","¿Qué año analizamos?"), uiOutput(id_anio)),
      div(class="filter", div(class="filter-label","¿Qué grupo alimenticio?"), uiOutput(id_grupo)),
      div(class="filter", div(class="filter-label","¿Qué alimento?"), uiOutput(id_alim))
    )
  )
}

filters_box_comp <- function(tag){
  id_anio  <- paste0("anio_ui_", tag)
  id_grupo <- paste0("grupo_ui_", tag)
  id_alim  <- paste0("alim_ui_", tag)
  
  div(
    class="filters",
    div(
      class="filters-grid-3",
      div(class="filter", div(class="filter-label","¿Qué año analizamos?"), uiOutput(id_anio)),
      div(class="filter", div(class="filter-label","¿Qué grupo alimenticio?"), uiOutput(id_grupo)),
      div(class="filter", div(class="filter-label","¿Qué alimento?"), uiOutput(id_alim))
    )
  )
}

col_palette <- c(
  "#007CC3", "#456ABB","#1A4922", "#2E7730", "#0D8D38", "#85A728", "#AEBF22", "#F2E203",
  "#F1B709", "#F39F06", "#BE7E11", "#08384D", "#094B5C", "#00596C", "#006A75", "#007A71",
  "#00909C", "#0088BB", "#007CC3", "#456ABB"
)

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  theme = bslib::bs_theme(
    version      = 5,
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius"  = "0.9rem",
    "font-size-base" = "0.95rem"
  ),
  tags$head(
    tags$style(HTML("
      :root{ --accent-border:#a1d99b; --plot-big-h: 600px; --plot-small-h: 260px; }
      body{ background:#ffffff; }
      .wrap{ max-width:1360px; margin:0 auto; padding:16px 20px 32px; }
      h2#app-title{ text-align:center; margin-top:10px; margin-bottom:10px; font-weight:800; letter-spacing:.3px; }

      .tabs-box{
        background:#fff; border:1px solid var(--accent-border) !important;
        border-radius:16px; padding:12px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
      }

      .filters{
        background:#fff; border:1px solid var(--accent-border); border-radius:16px;
        padding:14px 16px; margin-bottom:12px; box-shadow:0 4px 14px rgba(0,0,0,.06);
        width:100%; overflow: visible; position: relative; z-index: 20;
      }

      .filters-grid-3{
        width:100%;
        display:grid;
        grid-template-columns: repeat(3, minmax(240px, 1fr));
        column-gap:16px; row-gap:10px; align-items:end;
      }
      @media (max-width: 1100px){ .filters-grid-3{ grid-template-columns: repeat(2, minmax(240px, 1fr)); } }
      @media (max-width: 650px){ .filters-grid-3{ grid-template-columns: 1fr; } }

      .filter-label{
        font-weight:800; font-size:13px; margin-bottom:6px; color:#111827;
        white-space: normal; line-height: 1.15; min-height: 28px;
      }

      .form-select, .selectize-input{
        border:1px solid var(--accent-border) !important;
        border-radius:10px !important;
        box-shadow:none !important;
        font-size:14px; font-weight:600; color:#000;
        background-color:#fff !important;
        min-height:42px;
        width:100% !important;
      }
      .selectize-dropdown{ z-index: 99999 !important; }

      .card{
        background:#fff; border:1px solid var(--accent-border) !important;
        border-radius:16px; padding:12px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
      }
      .card-title{ font-weight:900; font-size:16px; margin-bottom:8px; color:#111827; }

      #ts_price{ height: var(--plot-big-h) !important; }
      #ts_mom{ height: var(--plot-small-h) !important; }
      #ts_yoy{ height: var(--plot-small-h) !important; }
      #comp_plot{ height: 520px !important; }
      #comp_plot3{ height: 520px !important; }

      .tbl-wrap{ margin-top: 10px; overflow-x:auto; }
      table.simple{ width:100%; border-collapse: collapse; font-size: 13px; }
      table.simple th, table.simple td{
        border-bottom:1px solid #e5e7eb;
        padding:8px 10px;
        text-align:left;
        white-space: nowrap;
      }
      table.simple th{ font-weight:900; color:#111827; }
    "))
  ),
  
  div(
    class="wrap",
    h2(APP_TITLE, id="app-title"),
    
    div(
      class="tabs-box",
      tabsetPanel(
        type = "tabs",
        
        # =========================================================
        # TAB 1 — 6 + 6
        # =========================================================
        tabPanel(
          paste0("¿Cómo están cambiando los precios?"),
          filters_box_prices("t1"),
          
          fluidRow(
            column(
              width = 6,
              div(
                class="card",
                div(class="card-title", strong("Promedio mensual de precios por producto en cada uno de los centros de acopio")),
                plotlyOutput("ts_price")
              )
            ),
            column(
              width = 6,
              div(
                style="display:flex;flex-direction:column;gap:12px;",
                div(
                  class="card",
                  div(class="card-title", strong("Cambio porcentual mensual en el precio de cada producto agrícola en los centros de acopio")),
                  plotlyOutput("ts_mom")
                ),
                div(
                  class="card",
                  div(class="card-title", strong("Cambio porcentual anual en el precio de cada producto agrícola en los centros de acopio")),
                  plotlyOutput("ts_yoy")
                )
              )
            )
          )
        ),
        
        # =========================================================
        # TAB 2 — Diferencias ENTRE DEPARTAMENTOS (SIN MES)
        # =========================================================
        tabPanel(
          paste0("¿En qué departamentos los precios son más altos o más bajos?"),
          filters_box_comp("t2"),
          
          div(
            class="card",
            div(class="card-title", strong("Diferenciales de precios por departamentos vs el departamento priorizado")),
            plotlyOutput("comp_plot"),
            div(class="tbl-wrap", uiOutput("comp_table"))
          )
        ),
        
        # =========================================================
        # TAB 3 — Diferencias ENTRE CIUDADES (referencia: capital, centrales mayoristas)
        # =========================================================
        tabPanel(
          paste0("¿En qué ciudades los precios son más altos o más bajos?"),
          filters_box_comp("t3"),
          
          div(
            class="card",
            div(class="card-title", strong("Diferenciales de precios por ciudad vs central mayorista principal")),
            plotlyOutput("comp_plot3"),
            div(class="tbl-wrap", uiOutput("comp_table3"))
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
  
  # ======================
  # TAB 1 — filtros
  # ======================
  years1 <- sort(unique(prices$anio[is.finite(prices$anio)]), decreasing = TRUE)
  
  output$anio_ui_t1 <- renderUI({
    selectInput("anio_t1", NULL, choices = c("Todos"="Todos", years1), selected = "Todos")
  })
  
  base_all_t1 <- reactive({
    df <- prices
    if (!is_all(input$anio_t1)) df <- df %>% dplyr::filter(anio == as.integer(input$anio_t1))
    df
  })
  
  output$grupo_ui_t1 <- renderUI({
    grupos <- sort(unique(na.omit(base_all_t1()$grupo)))
    selectInput("grupo_t1", NULL, choices = c("Todos"="Todos", grupos), selected = "Todos")
  })
  
  output$alim_ui_t1 <- renderUI({
    df <- base_all_t1()
    if (!is_all(input$grupo_t1)) df <- df %>% dplyr::filter(grupo == input$grupo_t1)
    alims <- sort(unique(na.omit(df$alimento)))
    selectInput("alim_t1", NULL, choices = c("Todos"="Todos", alims), selected = "Todos")
  })
  
  datos_filtrados_t1 <- reactive({
    df <- base_all_t1()
    df <- filter_single(df, "grupo", input$grupo_t1)
    df <- filter_single(df, "alimento", input$alim_t1)
    validate(need(nrow(df) > 0, "Sin datos con los filtros seleccionados."))
    df
  })
  
  serie_indicadores <- reactive({
    df <- datos_filtrados_t1() %>%
      dplyr::group_by(fecha) %>%
      dplyr::summarise(precio = mean(precio_kg, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(fecha)
    
    validate(need(nrow(df) > 0, "Sin serie temporal con los filtros seleccionados."))
    
    seq_m <- seq(from = min(df$fecha), to = max(df$fecha), by = "month")
    base <- data.frame(fecha = seq_m)
    
    base %>%
      dplyr::left_join(df, by = "fecha") %>%
      dplyr::arrange(fecha) %>%
      dplyr::mutate(
        mom = ifelse(is.finite(precio) & is.finite(dplyr::lag(precio, 1)) & dplyr::lag(precio, 1) > 0,
                     (precio / dplyr::lag(precio, 1) - 1) * 100, NA_real_),
        yoy = ifelse(is.finite(precio) & is.finite(dplyr::lag(precio, 12)) & dplyr::lag(precio, 12) > 0,
                     (precio / dplyr::lag(precio, 12) - 1) * 100, NA_real_),
        precio_lbl = ifelse(is.finite(precio), fmt_money_co(precio, 0), "NA"),
        mom_lbl    = ifelse(is.finite(mom),    fmt_pct_co(mom, 1),   "NA"),
        yoy_lbl    = ifelse(is.finite(yoy),    fmt_pct_co(yoy, 1),   "NA")
      )
  })
  
  labels_contexto_t1 <- reactive({
    lab_grupo <- if (is_all(input$grupo_t1)) "Todos los grupos" else input$grupo_t1
    lab_alim  <- if (is_all(input$alim_t1))  "Todos los alimentos" else input$alim_t1
    paste0(lab_grupo, " · ", lab_alim)
  })
  
  last_vals_t1 <- reactive({
    ts <- serie_indicadores()
    last_price <- tail(ts$precio[is.finite(ts$precio)], 1)
    last_mom   <- tail(ts$mom[is.finite(ts$mom)], 1)
    last_yoy   <- tail(ts$yoy[is.finite(ts$yoy)], 1)
    list(
      price = ifelse(length(last_price)==0, NA_real_, last_price),
      mom   = ifelse(length(last_mom)==0, NA_real_, last_mom),
      yoy   = ifelse(length(last_yoy)==0, NA_real_, last_yoy)
    )
  })
  
  output$ts_price <- renderPlotly({
    ts <- serie_indicadores()
    ctx <- labels_contexto_t1()
    lv <- last_vals_t1()
    
    ttl <- paste0(
      "",
      "<br><span style='font-size:12px; font-weight:600; color:#374151;'>",
      ctx,
      ifelse(is.finite(lv$price), paste0(" · Último: ", fmt_money_co(lv$price, 0)), ""),
      "</span>"
    )
    
    plotly::plot_ly(
      data = ts,
      x = ~fecha, y = ~precio,
      text = ~precio_lbl,
      type = "scatter", mode = "lines+markers",
      hovertemplate = paste(
        "<b>%{x|%Y-%m}</b><br>",
        "Precio: %{text}",
        "<extra></extra>"
      )
    ) %>%
      plotly::layout(
        title = list(text = ttl, x = 0, xanchor = "left"),
        xaxis = list(title = ""),
        yaxis = list(title = "Precio ($/Kg)"),
        margin = list(l=55, r=25, t=70, b=50)
      )
  })
  
  output$ts_mom <- renderPlotly({
    ts <- serie_indicadores()
    ctx <- labels_contexto_t1()
    lv <- last_vals_t1()
    
    ttl <- paste0(
      "",
      "<br><span style='font-size:12px; font-weight:600; color:#374151;'>",
      ctx,
      ifelse(is.finite(lv$mom), paste0(" · Último: ", fmt_pct_co(lv$mom, 1)), ""),
      "</span>"
    )
    
    plotly::plot_ly(
      data = ts,
      x = ~fecha, y = ~mom,
      text = ~mom_lbl,
      type = "scatter", mode = "lines+markers",
      hovertemplate = paste(
        "<b>%{x|%Y-%m}</b><br>",
        "MoM: %{text}",
        "<extra></extra>"
      )
    ) %>%
      plotly::layout(
        title = list(text = ttl, x = 0, xanchor = "left"),
        xaxis = list(title = ""),
        yaxis = list(title = "%"),
        margin = list(l=55, r=25, t=70, b=40)
      )
  })
  
  output$ts_yoy <- renderPlotly({
    ts <- serie_indicadores()
    ctx <- labels_contexto_t1()
    lv <- last_vals_t1()
    
    ttl <- paste0(
      "",
      "<br><span style='font-size:12px; font-weight:600; color:#374151;'>",
      ctx,
      ifelse(is.finite(lv$yoy), paste0(" · Último: ", fmt_pct_co(lv$yoy, 1)), ""),
      "</span>"
    )
    
    plotly::plot_ly(
      data = ts,
      x = ~fecha, y = ~yoy,
      text = ~yoy_lbl,
      type = "scatter", mode = "lines+markers",
      hovertemplate = paste(
        "<b>%{x|%Y-%m}</b><br>",
        "YoY: %{text}",
        "<extra></extra>"
      )
    ) %>%
      plotly::layout(
        title = list(text = ttl, x = 0, xanchor = "left"),
        xaxis = list(title = ""),
        yaxis = list(title = "%"),
        margin = list(l=55, r=25, t=70, b=40)
      )
  })
  
  # ======================
  # TAB 2 — filtros (SIN mes)
  # ======================
  years2 <- sort(unique(prices2$anio[is.finite(prices2$anio)]), decreasing = TRUE)
  
  output$anio_ui_t2 <- renderUI({
    selectInput("anio_t2", NULL, choices = c("Todos"="Todos", years2), selected = "Todos")
  })
  
  base_all_t2 <- reactive({
    df <- prices2
    if (!is_all(input$anio_t2)) df <- df %>% dplyr::filter(anio == as.integer(input$anio_t2))
    df
  })
  
  output$grupo_ui_t2 <- renderUI({
    grupos <- sort(unique(na.omit(base_all_t2()$grupo)))
    selectInput("grupo_t2", NULL, choices = c("Todos"="Todos", grupos), selected = "Todos")
  })
  
  output$alim_ui_t2 <- renderUI({
    df <- base_all_t2()
    if (!is_all(input$grupo_t2)) df <- df %>% dplyr::filter(grupo == input$grupo_t2)
    alims <- sort(unique(na.omit(df$alimento)))
    selectInput("alim_t2", NULL, choices = c("Todos"="Todos", alims), selected = "Todos")
  })
  
  datos_filtrados_t2 <- reactive({
    df <- base_all_t2()
    df <- filter_single(df, "grupo", input$grupo_t2)
    df <- filter_single(df, "alimento", input$alim_t2)
    validate(need(nrow(df) > 0, "Sin datos con los filtros seleccionados."))
    df
  })
  
  # comp = (promedio anual dpto) - (promedio anual dpto foco)
  comp_data <- reactive({
    df <- datos_filtrados_t2()
    
    m_dpto <- df %>%
      dplyr::group_by(cod_dpto_d, departamento_d, anio) %>%
      dplyr::summarise(precio_a = mean(precio_kg, na.rm = TRUE), .groups = "drop")
    
    validate(need(nrow(m_dpto) > 0, "No se pudo construir el promedio anual por departamento."))
    
    by_dpto <- m_dpto %>%
      dplyr::group_by(cod_dpto_d, departamento_d) %>%
      dplyr::summarise(
        mean_price = mean(precio_a, na.rm = TRUE),
        dev = sd(precio_a, na.rm = TRUE),
        n_anios = sum(is.finite(precio_a)),
        .groups = "drop"
      )
    
    foco <- by_dpto %>% dplyr::filter(cod_dpto_d == DPTO_FOCO_COD)
    validate(need(nrow(foco) > 0, paste0("No encuentro a ", DPTO_FOCO_NOMBRE, " (", DPTO_FOCO_COD, ") para comparar.")))
    
    foco_mean <- foco$mean_price[1]
    
    by_dpto %>%
      dplyr::mutate(
        comp = mean_price - foco_mean,
        dev_plot = ifelse(is.finite(dev), dev, 0),
        tooltip_text = paste0(
          "Departamento: ", departamento_d,
          "<br>Diferencia vs ", DPTO_FOCO_NOMBRE, ": ", fmt_money_co(comp, 0),
          "<br>Promedio dpto: ", fmt_money_co(mean_price, 0),
          "<br>Desv. est. (anual): ", ifelse(is.finite(dev), fmt_price_co(dev, 0), "NA"),
          "<br>Años usados: ", n_anios
        )
      )
  })
  
  labels_contexto_t2 <- reactive({
    lab_anio  <- if (is_all(input$anio_t2)) "Todos los años" else paste0("Año ", input$anio_t2)
    lab_grupo <- if (is_all(input$grupo_t2)) "Todos los grupos" else input$grupo_t2
    lab_alim  <- if (is_all(input$alim_t2))  "Todos los alimentos" else input$alim_t2
    paste0(lab_anio, " · ", lab_grupo, " · ", lab_alim)
  })
  
  output$comp_plot <- renderPlotly({
    df <- comp_data()
    ctx <- labels_contexto_t2()
    
    dptos <- df$departamento_d
    cols <- rep(col_palette, length.out = length(dptos))
    names(cols) <- dptos
    
    ttl <- paste0(
      "Diferencia de precios por departamento vs ", DPTO_FOCO_NOMBRE,
      "<br><span style='font-size:12px; font-weight:600; color:#374151;'>",
      ctx,
      "</span>"
    )
    
    g <- ggplot(df, aes(x = comp, y = 1, color = departamento_d)) +
      geom_point(aes(size = dev_plot, text = tooltip_text), alpha = 0.85) +
      geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.5, alpha = 0.25) +
      theme_bw() +
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      labs(x = "", y = "") +
      scale_size(range = c(5, 15)) +
      scale_color_manual(values = cols)
    
    p <- plotly::ggplotly(g, tooltip = "text")
    
    for (i in seq_len(nrow(df))) {
      yshift_value <- ifelse(i %% 2 == 0, 80, -80)
      p <- p %>% plotly::add_annotations(
        x = df$comp[i], y = 1,
        text = df$departamento_d[i],
        showarrow = FALSE,
        yshift = yshift_value,
        textangle = -90,
        font = list(size = 10, family = "Inter", color = "#6b7280")
      )
    }
    
    p %>% plotly::layout(
      title = list(text = ttl, x = 0, xanchor = "left"),
      xaxis = list(title = "", zeroline = FALSE),
      yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
      margin = list(l=45, r=25, t=70, b=40)
    )
  })
  
  output$comp_table <- renderUI({
    df <- comp_data() %>%
      dplyr::arrange(comp) %>%
      dplyr::mutate(
        diferencia = fmt_money_co(comp, 0),
        promedio_dpto = fmt_money_co(mean_price, 0),
        dev_anual = ifelse(is.finite(dev), fmt_money_co(dev, 0), "NA")
      ) %>%
      dplyr::select(
        departamento = departamento_d,
        diferencia,
        promedio_dpto,
        desviacion_anual = dev_anual,
        anios = n_anios
      )
    
    tags$table(
      class = "simple",
      tags$thead(
        tags$tr(
          tags$th("Departamento"),
          tags$th(paste0("Diferencia vs ", DPTO_FOCO_NOMBRE, " (de - a +)")),
          tags$th("Promedio dpto"),
          tags$th("Desv. est. (anual)"),
          tags$th("Años")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i){
          tags$tr(
            tags$td(df$departamento[i]),
            tags$td(df$diferencia[i]),
            tags$td(df$promedio_dpto[i]),
            tags$td(df$desviacion_anual[i]),
            tags$td(df$anios[i])
          )
        })
      )
    )
  })
  
  # ======================
  # TAB 3 — filtros (SIN mes)
  # ======================
  years3 <- sort(unique(prices3$anio[is.finite(prices3$anio)]), decreasing = TRUE)
  
  output$anio_ui_t3 <- renderUI({
    selectInput("anio_t3", NULL, choices = c("Todos"="Todos", years3), selected = "Todos")
  })
  
  base_all_t3 <- reactive({
    df <- prices3
    if (!is_all(input$anio_t3)) df <- df %>% dplyr::filter(anio == as.integer(input$anio_t3))
    df
  })
  
  output$grupo_ui_t3 <- renderUI({
    grupos <- sort(unique(na.omit(base_all_t3()$grupo)))
    selectInput("grupo_t3", NULL, choices = c("Todos"="Todos", grupos), selected = "Todos")
  })
  
  output$alim_ui_t3 <- renderUI({
    df <- base_all_t3()
    if (!is_all(input$grupo_t3)) df <- df %>% dplyr::filter(grupo == input$grupo_t3)
    alims <- sort(unique(na.omit(df$alimento)))
    selectInput("alim_t3", NULL, choices = c("Todos"="Todos", alims), selected = "Todos")
  })
  
  datos_filtrados_t3 <- reactive({
    df <- base_all_t3()
    df <- filter_single(df, "grupo", input$grupo_t3)
    df <- filter_single(df, "alimento", input$alim_t3)
    validate(need(nrow(df) > 0, "Sin datos con los filtros seleccionados."))
    df
  })
  
  # comp = (promedio anual ciudad) - (promedio anual referencia: capital, centrales mayoristas)
  comp_data3 <- reactive({
    df <- datos_filtrados_t3()
    
    m_city <- df %>%
      dplyr::group_by(cod_munic, municipio, cod_dpto, departamento, city_label, anio) %>%
      dplyr::summarise(precio_a = mean(precio_kg, na.rm = TRUE), .groups = "drop")
    
    validate(need(nrow(m_city) > 0, "No se pudo construir el promedio anual por ciudad."))
    
    by_city <- m_city %>%
      dplyr::group_by(cod_munic, municipio, cod_dpto, departamento, city_label) %>%
      dplyr::summarise(
        mean_price = mean(precio_a, na.rm = TRUE),
        dev = sd(precio_a, na.rm = TRUE),
        n_anios = sum(is.finite(precio_a)),
        .groups = "drop"
      )
    
    # Referencia: Barranquilla SOLO centrales mayoristas
    bga <- df %>%
      dplyr::filter(
        (!is.na(cod_munic) & cod_munic == MUNI_FOCO_COD) |
          (!is.na(municipio) & municipio == CAPITAL_FOCO_NOMBRE)
      )
    
    validate(need(nrow(bga) > 0, paste0("No encuentro registros para ", CAPITAL_FOCO_NOMBRE, " para construir la referencia.")))
    
    bga_ref <- bga
    if (!all(is.na(bga$tipo_plaza))) {
      bga_ref <- bga_ref %>% dplyr::filter(stringr::str_detect(stringr::str_to_lower(tipo_plaza), "central|mayorist|abastos"))
    } else if (!all(is.na(bga$plaza))) {
      bga_ref <- bga_ref %>% dplyr::filter(stringr::str_detect(stringr::str_to_lower(plaza), "central|mayorist|abastos|centroabastos"))
    }
    
    validate(need(nrow(bga_ref) > 0, paste0(
      "Encontré ", CAPITAL_FOCO_NOMBRE, ", pero no pude aislar 'centrales mayoristas' (revisa columnas tipo_plaza/plaza). ",
      "Ajusta el patrón si el nombre viene distinto."
    )))
    
    bga_ref_a <- bga_ref %>%
      dplyr::group_by(anio) %>%
      dplyr::summarise(precio_a = mean(precio_kg, na.rm = TRUE), .groups = "drop")
    
    foco_mean <- mean(bga_ref_a$precio_a, na.rm = TRUE)
    validate(need(is.finite(foco_mean), paste0("No se pudo calcular el promedio de referencia (", CAPITAL_FOCO_NOMBRE, " centrales mayoristas).")))
    
    by_city %>%
      dplyr::mutate(
        comp = mean_price - foco_mean,
        dev_plot = ifelse(is.finite(dev), dev, 0),
        tooltip_text = paste0(
          "Ciudad: ", city_label,
          "<br>Diferencia vs ", CAPITAL_FOCO_NOMBRE, " (centrales): ", fmt_money_co(comp, 0),
          "<br>Promedio ciudad: ", fmt_money_co(mean_price, 0),
          "<br>Desv. est. (anual): ", ifelse(is.finite(dev), fmt_price_co(dev, 0), "NA"),
          "<br>Años usados: ", n_anios
        )
      )
  })
  
  labels_contexto_t3 <- reactive({
    lab_anio  <- if (is_all(input$anio_t3)) "Todos los años" else paste0("Año ", input$anio_t3)
    lab_grupo <- if (is_all(input$grupo_t3)) "Todos los grupos" else input$grupo_t3
    lab_alim  <- if (is_all(input$alim_t3))  "Todos los alimentos" else input$alim_t3
    paste0(lab_anio, " · ", lab_grupo, " · ", lab_alim)
  })
  
  output$comp_plot3 <- renderPlotly({
    df <- comp_data3()
    ctx <- labels_contexto_t3()
    
    labs <- df$city_label
    cols <- rep(col_palette, length.out = length(labs))
    names(cols) <- labs
    
    ttl <- paste0(
      "Diferencia de precios por ciudad vs ", CAPITAL_FOCO_NOMBRE, " (centrales mayoristas)",
      "<br><span style='font-size:12px; font-weight:600; color:#374151;'>",
      ctx,
      "</span>"
    )
    
    g <- ggplot(df, aes(x = comp, y = 1, color = city_label)) +
      geom_point(aes(size = dev_plot, text = tooltip_text), alpha = 0.85) +
      geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.5, alpha = 0.25) +
      theme_bw() +
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      labs(x = "", y = "") +
      scale_size(range = c(5, 15)) +
      scale_color_manual(values = cols)
    
    p <- plotly::ggplotly(g, tooltip = "text")
    
    df_lab <- df %>%
      dplyr::mutate(abs_comp = abs(comp)) %>%
      dplyr::arrange(dplyr::desc(abs_comp)) %>%
      dplyr::slice_head(n = min(60, nrow(df))) %>%
      dplyr::arrange(comp)
    
    for (i in seq_len(nrow(df_lab))) {
      yshift_value <- ifelse(i %% 2 == 0, 80, -80)
      p <- p %>% plotly::add_annotations(
        x = df_lab$comp[i], y = 1,
        text = df_lab$city_label[i],
        showarrow = FALSE,
        yshift = yshift_value,
        textangle = -90,
        font = list(size = 10, family = "Inter", color = "#6b7280")
      )
    }
    
    p %>% plotly::layout(
      title = list(text = ttl, x = 0, xanchor = "left"),
      xaxis = list(title = "", zeroline = FALSE),
      yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
      margin = list(l=45, r=25, t=70, b=40)
    )
  })
  
  output$comp_table3 <- renderUI({
    df <- comp_data3() %>%
      dplyr::arrange(comp) %>%
      dplyr::mutate(
        diferencia = fmt_money_co(comp, 0),
        promedio_ciudad = fmt_money_co(mean_price, 0),
        dev_anual = ifelse(is.finite(dev), fmt_money_co(dev, 0), "NA")
      ) %>%
      dplyr::select(
        ciudad = city_label,
        diferencia,
        promedio_ciudad,
        desviacion_anual = dev_anual,
        anios = n_anios
      )
    
    tags$table(
      class = "simple",
      tags$thead(
        tags$tr(
          tags$th("Ciudad"),
          tags$th(paste0("Diferencia vs ", CAPITAL_FOCO_NOMBRE, " (centrales) (de - a +)")),
          tags$th("Promedio ciudad"),
          tags$th("Desv. est. (anual)"),
          tags$th("Años")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i){
          tags$tr(
            tags$td(df$ciudad[i]),
            tags$td(df$diferencia[i]),
            tags$td(df$promedio_ciudad[i]),
            tags$td(df$desviacion_anual[i]),
            tags$td(df$anios[i])
          )
        })
      )
    )
  })
}

shinyApp(ui, server)
