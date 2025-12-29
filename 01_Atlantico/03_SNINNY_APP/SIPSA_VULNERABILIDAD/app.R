# app.R
# =========================================================
# SIPSA_ABASTECIMIENTO_HHI — 3 pestañas (Storytelling)
# (FOCO = ATLÁNTICO)
# =========================================================

DPTO_FOCO_NOMBRE <- "Atlántico"
DPTO_FOCO_COD    <- "08"
APP_TITLE <- paste0("SIPSA — Concentración (HHI) hacia ", DPTO_FOCO_NOMBRE)

# ------------------------------
# Paquetes (NO instalar aquí)
# ------------------------------
pkgs <- c(
  "shiny","bslib",
  "dplyr","stringr","janitor","scales",
  "readr","stringi","htmltools",
  "lubridate",
  "plotly","ggplot2",
  "DT"
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
  library(lubridate)
  library(plotly); library(ggplot2)
  library(DT)
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

fmt_num_co <- function(x, digits = 0){
  scales::number(x, big.mark=".", decimal.mark=",", accuracy = 10^(-digits))
}
fmt_pct_co <- function(x, digits = 1){
  ifelse(is.finite(x),
         paste0(scales::number(x, big.mark=".", decimal.mark=",", accuracy = 10^(-digits)), "%"),
         "NA")
}

is_all <- function(x){
  is.null(x) || length(x) == 0 || identical(x, "Todos")
}

kg_to_ton <- function(x) x/1000

# =========================================================
# DT (DataTable) — idioma + opciones ✅ FIX REAL (language dentro de options)
# =========================================================
dt_lang_es <- list(
  processing = "Procesando...",
  search = "Buscar:",
  lengthMenu = "Mostrar _MENU_",
  info = "Mostrando _START_ a _END_ de _TOTAL_",
  infoEmpty = "Mostrando 0 a 0 de 0",
  infoFiltered = "(filtrado de _MAX_ en total)",
  loadingRecords = "Cargando...",
  zeroRecords = "No se encontraron resultados",
  emptyTable = "Sin datos",
  paginate = list(first="Primero", previous="Anterior", `next`="Siguiente", last="Último")
)

dt_opts <- function(pageLength = 10){
  list(
    pageLength = pageLength,
    lengthMenu = c(5,10,15,25,50,100),
    scrollX = TRUE,
    autoWidth = TRUE,
    stateSave = TRUE,
    deferRender = TRUE
  )
}
dt_opts_lang <- function(pageLength = 10){
  opts <- dt_opts(pageLength)
  opts$language <- dt_lang_es
  opts
}

# =========================================================
# Finder genérico de RDS (LECTURA RELATIVA)
# =========================================================
rds_candidates <- function(stem){
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

find_rds <- function(paths){
  for (p in paths) {
    if (!is.na(p) && file.exists(p) && !dir.exists(p) && grepl("\\.rds$", tolower(p))) {
      return(normalizePath(p, winslash = "/", mustWork = TRUE))
    }
  }
  NA_character_
}

# =========================================================
# 1) TAB 1 — Cargar RDS 041_DANE_SIPSA-Abast.rds
# =========================================================
rds_path <- find_rds(rds_candidates("041_DANE_SIPSA-Abast"))
if (is.na(rds_path)) {
  stop(
    "No encontré el archivo ./data/041_DANE_SIPSA-Abast.rds\n\n",
    "Solución rápida:\n",
    "1) Copia el .rds a: ", data_dir, "\n",
    "2) y nómbralo exactamente: 041_DANE_SIPSA-Abast.rds"
  )
}

raw <- readRDS(rds_path)
df  <- janitor::clean_names(raw)
nms <- names(df)

ycol  <- req_col(nms, c("ano","anio","year"), "AÑO")
gcol  <- req_col(nms, c("grupo","grupo_alimento","grupo_alimentos"), "GRUPO")
acol  <- req_col(nms, c("alimento","producto","articulo"), "ALIMENTO")
qcol  <- req_col(nms, c("cantkg_total","cant_kg_total","cantidad_kg","cantidad","kg","cantkg","cantkg_total_kg"), "CANTIDAD (Kg)")

dpto_d_c <- pick_first(nms, c("cod_dane_dpto_d","dane_cod_dpto_d","cod_dpto_d","cod_depto_d","cod_dane_depto_d"))

abast <- df %>%
  dplyr::transmute(
    anio = suppressWarnings(as.integer(.data[[ycol]])),
    grupo    = title_case_es(.data[[gcol]]),
    alimento = title_case_es(.data[[acol]]),
    cant_kg  = suppressWarnings(as.numeric(.data[[qcol]])),
    cod_dpto_d = if (!is.na(dpto_d_c)) pad_dpto(.data[[dpto_d_c]]) else NA_character_
  ) %>%
  dplyr::filter(
    is.finite(anio), anio >= 2018,
    !is.na(grupo), grupo != "",
    !is.na(alimento), alimento != "",
    is.finite(cant_kg), cant_kg > 0
  )

# Filtrar destino Atlántico si existe cod destino
if (!all(is.na(abast$cod_dpto_d))) {
  abast <- abast %>% dplyr::filter(cod_dpto_d == DPTO_FOCO_COD)
}

# =========================================================
# 2) TAB 2 — Cargar RDS 041_DANE_SIPSA-Abast_2.rds
# =========================================================
rds_path2 <- find_rds(rds_candidates("041_DANE_SIPSA-Abast_2"))
abast2 <- NULL

if (!is.na(rds_path2)) {
  raw2 <- readRDS(rds_path2)
  df2  <- janitor::clean_names(raw2)
  nms2 <- names(df2)
  
  y2 <- req_col(nms2, c("ano","anio","year"), "AÑO (base 2)")
  q2 <- req_col(nms2, c("cantkg_total","cant_kg_total","cantidad_kg","cantidad","kg","cantkg","cantkg_total_kg"), "CANTIDAD (Kg) (base 2)")
  
  dpto_d2_c <- pick_first(nms2, c("cod_dane_dpto_d","dane_cod_dpto_d","cod_dpto_d","cod_depto_d","cod_dane_depto_d"))
  dpto_o2_c <- pick_first(nms2, c("cod_dane_dpto_o","dane_cod_dpto_o","cod_dpto_o","cod_depto_o","cod_dane_depto_o"))
  dpto_d2_n <- pick_first(nms2, c("departamento_d","depto_d","departamento_destino","depto_destino"))
  dpto_o2_n <- pick_first(nms2, c("departamento_o","depto_o","departamento_origen","depto_origen"))
  
  g2 <- pick_first(nms2, c("grupo","grupo_alimento","grupo_alimentos"))
  a2 <- pick_first(nms2, c("alimento","producto","articulo"))
  
  abast2 <- df2 %>%
    dplyr::transmute(
      anio    = suppressWarnings(as.integer(.data[[y2]])),
      cant_kg = suppressWarnings(as.numeric(.data[[q2]])),
      cod_dpto_d = if (!is.na(dpto_d2_c)) pad_dpto(.data[[dpto_d2_c]]) else NA_character_,
      cod_dpto_o = if (!is.na(dpto_o2_c)) pad_dpto(.data[[dpto_o2_c]]) else NA_character_,
      dpto_d = if (!is.na(dpto_d2_n)) title_case_es(.data[[dpto_d2_n]]) else NA_character_,
      dpto_o = if (!is.na(dpto_o2_n)) title_case_es(.data[[dpto_o2_n]]) else NA_character_,
      grupo    = if (!is.na(g2)) title_case_es(.data[[g2]]) else NA_character_,
      alimento = if (!is.na(a2)) title_case_es(.data[[a2]]) else NA_character_
    ) %>%
    dplyr::filter(is.finite(anio), anio >= 2018, is.finite(cant_kg), cant_kg > 0)
  
  if (!all(is.na(abast2$dpto_o))) abast2 <- abast2 %>% dplyr::filter(!is.na(dpto_o), dpto_o != "")
}

# =========================================================
# 3) TAB 3 — Cargar RDS 041_DANE_SIPSA-Abast_3.rds
# =========================================================
rds_path3 <- find_rds(rds_candidates("041_DANE_SIPSA-Abast_3"))
abast3 <- NULL

if (!is.na(rds_path3)) {
  raw3 <- readRDS(rds_path3)
  df3  <- janitor::clean_names(raw3)
  nms3 <- names(df3)
  
  y3 <- req_col(nms3, c("ano","anio","year"), "AÑO (base 3)")
  q3 <- req_col(nms3, c("cantkg_total","cant_kg_total","cantidad_kg","cantidad","kg","cantkg","cantkg_total_kg"), "CANTIDAD (Kg) (base 3)")
  
  dpto_d3_c <- pick_first(nms3, c("cod_dane_dpto_d","dane_cod_dpto_d","cod_dpto_d","cod_depto_d","cod_dane_depto_d"))
  dpto_o3_c <- pick_first(nms3, c("cod_dane_dpto_o","dane_cod_dpto_o","cod_dpto_o","cod_depto_o","cod_dane_depto_o"))
  dpto_d3_n <- pick_first(nms3, c("departamento_d","depto_d","departamento_destino","depto_destino"))
  dpto_o3_n <- pick_first(nms3, c("departamento_o","depto_o","departamento_origen","depto_origen"))
  
  g3 <- pick_first(nms3, c("grupo","grupo_alimento","grupo_alimentos"))
  a3 <- pick_first(nms3, c("alimento","producto","articulo"))
  
  abast3 <- df3 %>%
    dplyr::transmute(
      anio    = suppressWarnings(as.integer(.data[[y3]])),
      cant_kg = suppressWarnings(as.numeric(.data[[q3]])),
      cod_dpto_d = if (!is.na(dpto_d3_c)) pad_dpto(.data[[dpto_d3_c]]) else NA_character_,
      cod_dpto_o = if (!is.na(dpto_o3_c)) pad_dpto(.data[[dpto_o3_c]]) else NA_character_,
      dpto_d = if (!is.na(dpto_d3_n)) title_case_es(.data[[dpto_d3_n]]) else NA_character_,
      dpto_o = if (!is.na(dpto_o3_n)) title_case_es(.data[[dpto_o3_n]]) else NA_character_,
      grupo    = if (!is.na(g3)) title_case_es(.data[[g3]]) else NA_character_,
      alimento = if (!is.na(a3)) title_case_es(.data[[a3]]) else NA_character_
    ) %>%
    dplyr::filter(is.finite(anio), anio >= 2018, is.finite(cant_kg), cant_kg > 0)
  
  if (!all(is.na(abast3$dpto_o))) abast3 <- abast3 %>% dplyr::filter(!is.na(dpto_o), dpto_o != "")
  if (!all(is.na(abast3$dpto_d))) abast3 <- abast3 %>% dplyr::filter(!is.na(dpto_d), dpto_d != "")
}

# =========================================================
# ✅ Colores fijos por grupo (TAB 1)
# =========================================================
col_palette <- c(
  "#007CC3", "#456ABB","#1A4922", "#2E7730", "#0D8D38", "#85A728", "#AEBF22", "#F2E203",
  "#F1B709", "#F39F06", "#BE7E11", "#08384D", "#094B5C", "#00596C", "#006A75", "#007A71",
  "#00909C", "#0088BB", "#007CC3", "#456ABB"
)

group_levels_all <- sort(unique(na.omit(abast$grupo)))
group_colors_map <- setNames(rep(col_palette, length.out = length(group_levels_all)), group_levels_all)

# =========================================================
# UI helpers
# =========================================================
filters_box_t1 <- function(){
  div(
    class="filters",
    div(
      class="filters-grid-3",
      div(class="filter", div(class="filter-label","¿Qué año analizamos?"), uiOutput("anio_ui_t1")),
      div(class="filter", div(class="filter-label","¿Quieres enfocar un grupo?"), uiOutput("grupo_ui_t1")),
      div(class="filter", div(class="filter-label",""), tags$div(style="height:42px;"))
    )
  )
}

filters_box_t2 <- function(){
  div(
    class="filters",
    div(
      class="filters-grid-3",
      div(class="filter", div(class="filter-label","¿Qué año analizamos?"), uiOutput("anio_ui_t2")),
      div(class="filter", div(class="filter-label","¿Qué grupo alimenticio?"), uiOutput("grupo_ui_t2")),
      div(class="filter", div(class="filter-label","¿Qué alimento?"), uiOutput("alim_ui_t2"))
    )
  )
}

filters_box_blank <- function(tag){
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
      :root{ --accent-border:#ffe082; }
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

      /* TAB grids */
      .blocks-grid{
        display:grid;
        grid-template-columns: repeat(2, minmax(420px, 1fr));
        gap:12px;
        grid-auto-rows: minmax(380px, auto);
      }
      @media (max-width: 980px){
        .blocks-grid{ grid-template-columns: 1fr; grid-auto-rows: auto; }
        .span-2rows{ grid-row: auto; }
      }
      .span-2rows{ grid-row: span 2; }

      #blk_plot_1, #blk_plot_3 { height: 380px !important; }
      #blk_plot_2 { height: 772px !important; }

      .blocks-grid-2{
        display:grid;
        grid-template-columns: repeat(2, minmax(420px, 1fr));
        gap:12px;
        grid-auto-rows: minmax(420px, auto);
      }
      @media (max-width: 980px){ .blocks-grid-2{ grid-template-columns: 1fr; } }

      #t2_plot_a, #t2_plot_b, #t3_plot_a, #t3_plot_b, #t3_plot_c, #t3_plot_d2 { height: 420px !important; }

      .dataTables_wrapper{ width:100% !important; }

      details.summary-box{
        margin-top:12px;
        border:1px dashed var(--accent-border);
        border-radius:12px;
        padding:10px 12px;
      }
      details.summary-box > summary{
        cursor:pointer;
        font-weight:800;
        color:#111827;
      }
    "))
  ),
  
  div(
    class="wrap",
    h2(APP_TITLE, id="app-title"),
    
    div(
      class="tabs-box",
      tabsetPanel(
        type = "tabs",
        
        # TAB 1
        tabPanel(
          "¿Qué tan concentrada está la canasta por grupo? (HHI)",
          filters_box_t1(),
          
          div(
            class="blocks-grid",
            div(class="card",
                div(class="card-title", strong("Bloque 1 — HHI por grupo (más concentrado → menos)")),
                plotlyOutput("blk_plot_1")
            ),
            div(class="card span-2rows",
                div(class="card-title", strong("Bloque 2 — Top 15 alimentos (TON) según grupo enfocado")),
                plotlyOutput("blk_plot_2")
            ),
            div(class="card",
                div(class="card-title", strong("Bloque 3 — Serie temporal del HHI por grupo")),
                plotlyOutput("blk_plot_3")
            )
          ),
          
          div(class="card", style="margin-top:12px;",
              div(class="card-title", strong("Tabla de respaldo — ranking de concentración por grupo")),
              DTOutput("hhi_group_table")
          )
        ),
        
        # TAB 2
        tabPanel(
          "¿Qué tan concentrados están los abastecedores de Atlántico? (HHI por origen)",
          filters_box_t2(),
          
          div(
            class="blocks-grid-2",
            div(class="card",
                div(class="card-title", strong("Bloque A — Departamentos abastecedores a Atlántico (Top 15)")),
                plotlyOutput("t2_plot_a")
            ),
            div(class="card",
                div(class="card-title", strong("Bloque B — Evolución del HHI (origen → Atlántico)")),
                plotlyOutput("t2_plot_b")
            )
          ),
          
          div(class="card", style="margin-top:12px;",
              div(class="card-title", strong("Tabla de respaldo — participación por departamento de origen")),
              DTOutput("t2_table")
          )
        ),
        
        # TAB 3
        tabPanel(
          "¿Qué tan diversificados están los destinos desde Atlántico? (HHI por destino)",
          filters_box_blank("t3"),
          
          div(
            class="blocks-grid-2",
            div(class="card",
                div(class="card-title", strong("Bloque C — Top 15 alimentos (HHI) + destino principal")),
                plotlyOutput("t3_plot_a")
            ),
            div(class="card",
                div(class="card-title", strong("Bloque D — Evolución del HHI de destinos (Atlántico → otros)")),
                plotlyOutput("t3_plot_b")
            )
          ),
          
          # ✅ AQUÍ: lo que era “tabla” ahora se muestra como BLOQUES GRÁFICOS
          div(
            class="blocks-grid-2", style="margin-top:12px;",
            div(class="card",
                div(class="card-title", strong("Bloque E — Destino principal más frecuente (conteo de alimentos)")),
                plotlyOutput("t3_plot_c")
            ),
            div(class="card",
                div(class="card-title", strong("Bloque F — Relación: HHI vs Toneladas (alimentos)")),
                plotlyOutput("t3_plot_d2")
            )
          ),
          
          # ✅ tabla solo como detalle plegable
          tags$details(
            class = "summary-box",
            tags$summary("Ver tabla detallada (HHI por alimento y destino principal)"),
            div(class="card", style="margin-top:10px;",
                DTOutput("t3_table")
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
  
  # ✅ FIX: outputOptions() solo después de que Shiny registre outputs
  safe_outputOptions <- function(name, ...) {
    tryCatch(shiny::outputOptions(output, name, ...), error = function(e) NULL)
  }
  session$onFlushed(function() {
    safe_outputOptions("hhi_group_table", suspendWhenHidden = FALSE)
    safe_outputOptions("t2_table",        suspendWhenHidden = FALSE)
    safe_outputOptions("t3_table",        suspendWhenHidden = FALSE)
  }, once = TRUE)
  
  # =========================================================
  # TAB 1 — filtros
  # =========================================================
  years1 <- sort(unique(abast$anio[is.finite(abast$anio)]), decreasing = TRUE)
  
  output$anio_ui_t1 <- renderUI({
    selectInput("anio_t1", NULL, choices = c("Todos"="Todos", years1), selected = "Todos")
  })
  
  base_all_t1 <- reactive({
    df <- abast
    if (!is_all(input$anio_t1)) df <- df %>% dplyr::filter(anio == as.integer(input$anio_t1))
    validate(need(nrow(df) > 0, "Sin datos con los filtros seleccionados."))
    df
  })
  
  output$grupo_ui_t1 <- renderUI({
    grupos <- sort(unique(na.omit(base_all_t1()$grupo)))
    selectInput("grupo_t1", NULL, choices = c("Todos"="Todos", grupos), selected = "Todos")
  })
  
  ga_t1 <- reactive({
    df <- base_all_t1()
    
    ga <- df %>%
      dplyr::group_by(grupo, alimento) %>%
      dplyr::summarise(kg = sum(cant_kg, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(is.finite(kg), kg > 0)
    
    validate(need(nrow(ga) > 0, "No se pudo construir Grupo → Alimento."))
    
    gtot <- ga %>%
      dplyr::group_by(grupo) %>%
      dplyr::summarise(
        total_kg = sum(kg),
        n_alimentos = dplyr::n_distinct(alimento),
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(total_kg), total_kg > 0)
    
    ga %>%
      dplyr::left_join(gtot, by="grupo") %>%
      dplyr::mutate(share = kg / total_kg) %>%
      dplyr::filter(is.finite(share), share > 0)
  })
  
  hhi_by_group <- reactive({
    ga <- ga_t1()
    
    top_food <- ga %>%
      dplyr::arrange(grupo, dplyr::desc(share)) %>%
      dplyr::group_by(grupo) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(grupo, top_alimento = alimento, top_share = share, top_share_pct = share*100)
    
    out <- ga %>%
      dplyr::group_by(grupo) %>%
      dplyr::summarise(
        total_kg = dplyr::first(total_kg),
        n_alimentos = dplyr::first(n_alimentos),
        hhi01 = sum(share^2, na.rm = TRUE),
        hhi10000 = sum((share*100)^2, na.rm = TRUE),
        nequiv = ifelse(is.finite(hhi01) && hhi01 > 0, 1/hhi01, NA_real_),
        .groups = "drop"
      ) %>%
      dplyr::left_join(top_food, by="grupo") %>%
      dplyr::mutate(
        total_ton = kg_to_ton(total_kg),
        total_ton_lbl = fmt_num_co(total_ton, 1),
        hhi01_lbl = ifelse(is.finite(hhi01), fmt_num_co(hhi01, 3), "NA"),
        hhi10000_lbl = ifelse(is.finite(hhi10000), fmt_num_co(hhi10000, 0), "NA"),
        nequiv_lbl = ifelse(is.finite(nequiv), fmt_num_co(nequiv, 1), "NA"),
        top_share_lbl = fmt_pct_co(top_share_pct, 1),
        tooltip_hhi = paste0(
          "<b>", grupo, "</b>",
          "<br>IHH (0–1): ", hhi01_lbl,
          "<br>IHH (0–10.000): ", hhi10000_lbl,
          "<br># alimentos: ", n_alimentos,
          "<br>N efectivo: ", nequiv_lbl,
          "<br>Top alimento: ", top_alimento, " (", top_share_lbl, ")",
          "<br>Total (Ton): ", total_ton_lbl,
          "<extra></extra>"
        )
      )
    
    validate(need(nrow(out) >= 1, "No hay grupos suficientes para HHI."))
    out
  })
  
  hhi_by_group_year <- reactive({
    df <- base_all_t1()
    
    ga_y <- df %>%
      dplyr::group_by(anio, grupo, alimento) %>%
      dplyr::summarise(kg = sum(cant_kg, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(is.finite(kg), kg > 0)
    
    validate(need(nrow(ga_y) > 0, "No se pudo construir serie anual Grupo → Alimento."))
    
    gtot_y <- ga_y %>%
      dplyr::group_by(anio, grupo) %>%
      dplyr::summarise(
        total_kg = sum(kg),
        n_alimentos = dplyr::n_distinct(alimento),
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(total_kg), total_kg > 0)
    
    hhi_y <- ga_y %>%
      dplyr::left_join(gtot_y, by=c("anio","grupo")) %>%
      dplyr::mutate(share = kg / total_kg) %>%
      dplyr::group_by(anio, grupo) %>%
      dplyr::summarise(
        hhi01 = sum(share^2, na.rm = TRUE),
        hhi01_lbl = ifelse(is.finite(hhi01), fmt_num_co(hhi01, 3), "NA"),
        total_kg = dplyr::first(total_kg),
        n_alimentos = dplyr::first(n_alimentos),
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(hhi01)) %>%
      dplyr::mutate(
        total_ton = kg_to_ton(total_kg),
        tooltip = paste0(
          "<b>", grupo, "</b>",
          "<br>Año: ", anio,
          "<br>IHH (0–1): ", hhi01_lbl,
          "<br># alimentos: ", n_alimentos,
          "<br>Total (Ton): ", fmt_num_co(total_ton, 1),
          "<extra></extra>"
        )
      )
    
    if (!is_all(input$grupo_t1)) hhi_y <- hhi_y %>% dplyr::filter(grupo == input$grupo_t1)
    
    validate(need(nrow(hhi_y) > 0, "No hay datos para la serie temporal con esos filtros."))
    hhi_y %>% dplyr::mutate(grupo = factor(grupo, levels = group_levels_all))
  })
  
  output$blk_plot_1 <- renderPlotly({
    df <- hhi_by_group() %>%
      dplyr::filter(is.finite(hhi01)) %>%
      dplyr::arrange(dplyr::desc(hhi01)) %>%
      dplyr::mutate(grupo_ord = factor(grupo, levels = rev(grupo)))
    
    validate(need(nrow(df) > 0, "Sin datos para graficar HHI por grupo."))
    
    plotly::plot_ly(
      data = df,
      x = ~hhi01,
      y = ~grupo_ord,
      type = "bar",
      orientation = "h",
      text = ~hhi01_lbl,
      textposition = "auto",
      hovertext = ~tooltip_hhi,
      hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = "IHH (0–1)", rangemode = "tozero"),
        yaxis = list(title = ""),
        margin = list(l=150, r=20, t=10, b=50)
      )
  })
  
  output$blk_plot_2 <- renderPlotly({
    df <- base_all_t1()
    if (!is_all(input$grupo_t1)) df <- df %>% dplyr::filter(grupo == input$grupo_t1)
    
    top15 <- df %>%
      dplyr::group_by(alimento) %>%
      dplyr::summarise(kg = sum(cant_kg, na.rm = TRUE), .groups="drop") %>%
      dplyr::filter(is.finite(kg), kg > 0) %>%
      dplyr::mutate(ton = kg_to_ton(kg)) %>%
      dplyr::arrange(dplyr::desc(ton)) %>%
      dplyr::slice_head(n = 15) %>%
      dplyr::mutate(
        alimento_ord = factor(alimento, levels = rev(alimento)),
        ton_lbl = fmt_num_co(ton, 1),
        tooltip = paste0(
          "<b>", alimento, "</b>",
          if (!is_all(input$grupo_t1)) paste0("<br>Grupo: ", input$grupo_t1) else "<br>Grupo: (todos)",
          "<br>Total: ", ton_lbl, " Ton",
          "<extra></extra>"
        )
      )
    
    validate(need(nrow(top15) > 0, "Sin datos para construir el Top 15."))
    
    plotly::plot_ly(
      data = top15,
      x = ~ton,
      y = ~alimento_ord,
      type = "bar",
      orientation = "h",
      text = ~ton_lbl,
      textposition = "auto",
      hovertext = ~tooltip,
      hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Toneladas", rangemode = "tozero"),
        yaxis = list(title = ""),
        margin = list(l=190, r=20, t=10, b=55)
      )
  })
  
  output$blk_plot_3 <- renderPlotly({
    ts <- hhi_by_group_year() %>%
      dplyr::mutate(grupo = factor(as.character(grupo), levels = group_levels_all))
    
    plotly::plot_ly(
      data = ts,
      x = ~anio,
      y = ~hhi01,
      color = ~grupo,
      colors = unname(group_colors_map),
      type = "scatter",
      mode = "lines+markers",
      hovertext = ~tooltip,
      hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Año", tickmode = "linear", dtick = 1),
        yaxis = list(title = "IHH (0–1)", rangemode = "tozero"),
        legend = list(orientation = "h", y = -0.25),
        margin = list(l=60, r=20, t=10, b=80)
      )
  })
  
  output$hhi_group_table <- DT::renderDT({
    df <- hhi_by_group() %>%
      dplyr::arrange(dplyr::desc(hhi01)) %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::transmute(
        `#` = rank,
        Grupo = grupo,
        `IHH (0–1)` = hhi01_lbl,
        `IHH (0–10.000)` = hhi10000_lbl,
        `# alimentos` = n_alimentos,
        `N efectivo` = nequiv_lbl,
        `Top alimento` = top_alimento,
        `Top (%)` = top_share_lbl,
        `Total (Ton)` = total_ton_lbl
      )
    
    DT::datatable(
      df, rownames = FALSE, escape = TRUE,
      options = dt_opts_lang(pageLength = 10),
      class = "stripe hover order-column compact"
    )
  }, server = FALSE)
  
  # =========================================================
  # TAB 2 — filtros (base 2)
  # =========================================================
  output$anio_ui_t2 <- renderUI({
    validate(need(!is.null(abast2), "No se encontró ./data/041_DANE_SIPSA-Abast_2.rds"))
    years2 <- sort(unique(abast2$anio[is.finite(abast2$anio)]), decreasing = TRUE)
    selectInput("anio_t2", NULL, choices = c("Todos"="Todos", years2), selected = "Todos")
  })
  
  output$grupo_ui_t2 <- renderUI({
    validate(need(!is.null(abast2), "No se encontró ./data/041_DANE_SIPSA-Abast_2.rds"))
    if (all(is.na(abast2$grupo))) return(selectInput("grupo_t2", NULL, choices=c("Todos"="Todos"), selected="Todos"))
    df <- abast2
    if (!is_all(input$anio_t2)) df <- df %>% dplyr::filter(anio == as.integer(input$anio_t2))
    grupos <- sort(unique(na.omit(df$grupo)))
    selectInput("grupo_t2", NULL, choices = c("Todos"="Todos", grupos), selected = "Todos")
  })
  
  output$alim_ui_t2 <- renderUI({
    validate(need(!is.null(abast2), "No se encontró ./data/041_DANE_SIPSA-Abast_2.rds"))
    if (all(is.na(abast2$alimento))) return(selectInput("alim_t2", NULL, choices=c("Todos"="Todos"), selected="Todos"))
    df <- abast2
    if (!is_all(input$anio_t2)) df <- df %>% dplyr::filter(anio == as.integer(input$anio_t2))
    if (!is_all(input$grupo_t2) && !all(is.na(df$grupo))) df <- df %>% dplyr::filter(grupo == input$grupo_t2)
    alims <- sort(unique(na.omit(df$alimento)))
    selectInput("alim_t2", NULL, choices = c("Todos"="Todos", alims), selected = "Todos")
  })
  
  base_t2 <- reactive({
    validate(need(!is.null(abast2), "No se cargó 041_DANE_SIPSA-Abast_2.rds"))
    df <- abast2
    
    if (!is_all(input$anio_t2))  df <- df %>% dplyr::filter(anio == as.integer(input$anio_t2))
    if (!is_all(input$grupo_t2) && !all(is.na(df$grupo)))    df <- df %>% dplyr::filter(grupo == input$grupo_t2)
    if (!is_all(input$alim_t2)  && !all(is.na(df$alimento))) df <- df %>% dplyr::filter(alimento == input$alim_t2)
    
    has_cod_dest <- !all(is.na(df$cod_dpto_d))
    has_nom_dest <- !all(is.na(df$dpto_d))
    
    if (has_cod_dest) df <- df %>% dplyr::filter(cod_dpto_d == DPTO_FOCO_COD)
    else if (has_nom_dest) df <- df %>% dplyr::filter(dpto_d == DPTO_FOCO_NOMBRE)
    
    if (!all(is.na(df$dpto_o))) df <- df %>% dplyr::filter(!is.na(dpto_o), dpto_o != "")
    
    validate(need(nrow(df) > 0, "Sin datos hacia Atlántico con los filtros seleccionados."))
    df
  })
  
  orig_shares_t2 <- reactive({
    df <- base_t2()
    if (!all(is.na(df$dpto_o))) df <- df %>% dplyr::mutate(origen = dpto_o)
    else df <- df %>% dplyr::mutate(origen = cod_dpto_o)
    
    agg <- df %>%
      dplyr::group_by(origen) %>%
      dplyr::summarise(kg = sum(cant_kg, na.rm = TRUE), .groups="drop") %>%
      dplyr::filter(is.finite(kg), kg > 0) %>%
      dplyr::mutate(ton = kg_to_ton(kg))
    
    tot <- sum(agg$ton, na.rm = TRUE)
    validate(need(is.finite(tot) && tot > 0, "No hay total positivo (Ton) hacia Atlántico."))
    
    out <- agg %>%
      dplyr::mutate(
        share = ton / tot,
        share_pct = share * 100,
        ton_lbl = fmt_num_co(ton, 1),
        share_lbl = fmt_pct_co(share_pct, 1)
      ) %>%
      dplyr::arrange(dplyr::desc(ton))
    
    hhi01 <- sum(out$share^2, na.rm = TRUE)
    attr(out, "hhi01_lbl") <- ifelse(is.finite(hhi01), fmt_num_co(hhi01, 3), "NA")
    out
  })
  
  hhi_year_t2 <- reactive({
    df <- base_t2()
    if (!all(is.na(df$dpto_o))) df <- df %>% dplyr::mutate(origen = dpto_o)
    else df <- df %>% dplyr::mutate(origen = cod_dpto_o)
    
    agg_y <- df %>%
      dplyr::group_by(anio, origen) %>%
      dplyr::summarise(kg = sum(cant_kg, na.rm = TRUE), .groups="drop") %>%
      dplyr::filter(is.finite(kg), kg > 0) %>%
      dplyr::mutate(ton = kg_to_ton(kg))
    
    tot_y <- agg_y %>%
      dplyr::group_by(anio) %>%
      dplyr::summarise(total_ton = sum(ton), .groups="drop") %>%
      dplyr::filter(is.finite(total_ton), total_ton > 0)
    
    hhi_y <- agg_y %>%
      dplyr::left_join(tot_y, by="anio") %>%
      dplyr::mutate(share = ton / total_ton) %>%
      dplyr::group_by(anio) %>%
      dplyr::summarise(
        hhi01 = sum(share^2, na.rm = TRUE),
        total_ton = dplyr::first(total_ton),
        .groups="drop"
      ) %>%
      dplyr::filter(is.finite(hhi01)) %>%
      dplyr::mutate(
        hhi01_lbl = fmt_num_co(hhi01, 3),
        total_lbl = fmt_num_co(total_ton, 1),
        tooltip = paste0(
          "<b>Atlántico</b>",
          "<br>Año: ", anio,
          "<br>IHH (0–1): ", hhi01_lbl,
          "<br>Total: ", total_lbl, " Ton",
          "<extra></extra>"
        )
      )
    
    validate(need(nrow(hhi_y) > 0, "No hay serie temporal HHI con esos filtros."))
    hhi_y
  })
  
  output$t2_plot_a <- renderPlotly({
    df <- orig_shares_t2()
    hhi_lbl <- attr(df, "hhi01_lbl")
    
    top <- df %>%
      dplyr::slice_head(n = 15) %>%
      dplyr::mutate(
        origen_ord = factor(origen, levels = rev(origen)),
        txt = paste0(ton_lbl, " Ton (", share_lbl, ")"),
        tooltip = paste0(
          "<b>", origen, "</b>",
          "<br>Ton: ", ton_lbl,
          "<br>Participación: ", share_lbl,
          "<extra></extra>"
        )
      )
    
    plotly::plot_ly(
      data = top,
      x = ~ton,
      y = ~origen_ord,
      type = "bar",
      orientation = "h",
      text = ~txt,
      textposition = "auto",
      hovertext = ~tooltip,
      hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Toneladas hacia Atlántico", rangemode = "tozero"),
        yaxis = list(title = ""),
        annotations = list(
          list(
            x = 0, y = 1.10, xref = "paper", yref = "paper",
            text = paste0("<b>HHI (0–1):</b> ", hhi_lbl),
            showarrow = FALSE, align = "left"
          )
        ),
        margin = list(l=210, r=20, t=50, b=55)
      )
  })
  
  output$t2_plot_b <- renderPlotly({
    ts <- hhi_year_t2()
    
    plotly::plot_ly(
      data = ts,
      x = ~anio,
      y = ~hhi01,
      type = "scatter",
      mode = "lines+markers",
      hovertext = ~tooltip,
      hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Año", tickmode = "linear", dtick = 1),
        yaxis = list(title = "HHI (0–1)", rangemode = "tozero"),
        margin = list(l=60, r=20, t=10, b=60)
      )
  })
  
  output$t2_table <- DT::renderDT({
    df <- orig_shares_t2() %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::transmute(
        `#` = rank,
        `Departamento origen` = origen,
        `Ton hacia Atlántico` = ton_lbl,
        `Participación` = share_lbl
      )
    
    DT::datatable(
      df, rownames = FALSE, escape = TRUE,
      options = dt_opts_lang(pageLength = 10),
      class = "stripe hover order-column compact"
    )
  }, server = FALSE)
  
  # =========================================================
  # TAB 3 — filtros
  # =========================================================
  output$anio_ui_t3 <- renderUI({
    validate(need(!is.null(abast3), "No se encontró ./data/041_DANE_SIPSA-Abast_3.rds"))
    years3 <- sort(unique(abast3$anio[is.finite(abast3$anio)]), decreasing = TRUE)
    selectInput("anio_t3", NULL, choices = c("Todos"="Todos", years3), selected = "Todos")
  })
  
  output$grupo_ui_t3 <- renderUI({
    validate(need(!is.null(abast3), "No se encontró ./data/041_DANE_SIPSA-Abast_3.rds"))
    df <- abast3
    
    has_cod_org <- !all(is.na(df$cod_dpto_o))
    has_nom_org <- !all(is.na(df$dpto_o))
    if (has_cod_org) df <- df %>% dplyr::filter(cod_dpto_o == DPTO_FOCO_COD)
    else if (has_nom_org) df <- df %>% dplyr::filter(dpto_o == DPTO_FOCO_NOMBRE)
    
    if (!is_all(input$anio_t3)) df <- df %>% dplyr::filter(anio == as.integer(input$anio_t3))
    
    if (all(is.na(df$grupo))) return(selectInput("grupo_t3", NULL, choices=c("Todos"="Todos"), selected="Todos"))
    grupos <- sort(unique(na.omit(df$grupo)))
    selectInput("grupo_t3", NULL, choices = c("Todos"="Todos", grupos), selected = "Todos")
  })
  
  output$alim_ui_t3 <- renderUI({
    validate(need(!is.null(abast3), "No se encontró ./data/041_DANE_SIPSA-Abast_3.rds"))
    df <- abast3
    
    has_cod_org <- !all(is.na(df$cod_dpto_o))
    has_nom_org <- !all(is.na(df$dpto_o))
    if (has_cod_org) df <- df %>% dplyr::filter(cod_dpto_o == DPTO_FOCO_COD)
    else if (has_nom_org) df <- df %>% dplyr::filter(dpto_o == DPTO_FOCO_NOMBRE)
    
    if (!is_all(input$anio_t3)) df <- df %>% dplyr::filter(anio == as.integer(input$anio_t3))
    if (!is_all(input$grupo_t3) && !all(is.na(df$grupo))) df <- df %>% dplyr::filter(grupo == input$grupo_t3)
    
    if (all(is.na(df$alimento))) return(selectInput("alim_t3", NULL, choices=c("Todos"="Todos"), selected="Todos"))
    alims <- sort(unique(na.omit(df$alimento)))
    selectInput("alim_t3", NULL, choices = c("Todos"="Todos", alims), selected = "Todos")
  })
  
  base_t3 <- reactive({
    validate(need(!is.null(abast3), "No se cargó 041_DANE_SIPSA-Abast_3.rds"))
    df <- abast3
    
    if (!is_all(input$anio_t3))  df <- df %>% dplyr::filter(anio == as.integer(input$anio_t3))
    if (!is_all(input$grupo_t3) && !all(is.na(df$grupo)))    df <- df %>% dplyr::filter(grupo == input$grupo_t3)
    if (!is_all(input$alim_t3)  && !all(is.na(df$alimento))) df <- df %>% dplyr::filter(alimento == input$alim_t3)
    
    # origen Atlántico
    has_cod_org <- !all(is.na(df$cod_dpto_o))
    has_nom_org <- !all(is.na(df$dpto_o))
    if (has_cod_org) df <- df %>% dplyr::filter(cod_dpto_o == DPTO_FOCO_COD)
    else if (has_nom_org) df <- df %>% dplyr::filter(dpto_o == DPTO_FOCO_NOMBRE)
    
    # destino != Atlántico
    has_cod_dest <- !all(is.na(df$cod_dpto_d))
    has_nom_dest <- !all(is.na(df$dpto_d))
    if (has_cod_dest) df <- df %>% dplyr::filter(!is.na(cod_dpto_d), cod_dpto_d != "", cod_dpto_d != DPTO_FOCO_COD)
    else if (has_nom_dest) df <- df %>% dplyr::filter(!is.na(dpto_d), dpto_d != "", dpto_d != DPTO_FOCO_NOMBRE)
    
    validate(need(nrow(df) > 0, "Sin datos (origen Atlántico → otros destinos) con los filtros seleccionados."))
    df
  })
  
  hhi_food_t3 <- reactive({
    df <- base_t3()
    
    if (!all(is.na(df$dpto_d))) df <- df %>% dplyr::mutate(destino = dpto_d)
    else df <- df %>% dplyr::mutate(destino = cod_dpto_d)
    
    agg <- df %>%
      dplyr::group_by(grupo, alimento, destino) %>%
      dplyr::summarise(kg = sum(cant_kg, na.rm = TRUE), .groups="drop") %>%
      dplyr::filter(is.finite(kg), kg > 0) %>%
      dplyr::mutate(ton = kg_to_ton(kg))
    
    tot_i <- agg %>%
      dplyr::group_by(grupo, alimento) %>%
      dplyr::summarise(total_ton = sum(ton), .groups="drop") %>%
      dplyr::filter(is.finite(total_ton), total_ton > 0)
    
    shares <- agg %>%
      dplyr::left_join(tot_i, by=c("grupo","alimento")) %>%
      dplyr::mutate(p = ton / total_ton) %>%
      dplyr::filter(is.finite(p), p > 0)
    
    top_dest <- shares %>%
      dplyr::group_by(grupo, alimento) %>%
      dplyr::slice_max(order_by = p, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(grupo, alimento, destino_top = destino, top_p = p, top_p_pct = p*100)
    
    hhi <- shares %>%
      dplyr::group_by(grupo, alimento) %>%
      dplyr::summarise(
        hhi01 = sum(p^2, na.rm = TRUE),
        total_ton = dplyr::first(total_ton),
        ndest = dplyr::n_distinct(destino),
        .groups="drop"
      ) %>%
      dplyr::filter(is.finite(hhi01)) %>%
      dplyr::left_join(top_dest, by=c("grupo","alimento")) %>%
      dplyr::mutate(
        hhi01_lbl = fmt_num_co(hhi01, 3),
        total_lbl = fmt_num_co(total_ton, 1),
        nequiv = ifelse(hhi01 > 0, 1/hhi01, NA_real_),
        nequiv_lbl = ifelse(is.finite(nequiv), fmt_num_co(nequiv, 1), "NA"),
        destino_top = as.character(destino_top),
        top_share_lbl = fmt_pct_co(top_p_pct, 1),
        tooltip = paste0(
          "<b>", alimento, "</b>",
          "<br>Grupo: ", grupo,
          "<br>Destino principal: <b>", destino_top, "</b>",
          "<br>HHI destinos (0–1): ", hhi01_lbl,
          "<br>N destinos: ", ndest,
          "<br>N efectivo: ", nequiv_lbl,
          "<br>Total: ", total_lbl, " Ton",
          "<extra></extra>"
        )
      ) %>%
      dplyr::arrange(dplyr::desc(hhi01), dplyr::desc(total_ton))
    
    validate(need(nrow(hhi) > 0, "No se pudo calcular HHI por alimento (TAB 3)."))
    hhi
  })
  
  hhi_year_t3 <- reactive({
    df0 <- base_t3()
    
    if (!all(is.na(df0$dpto_d))) df0 <- df0 %>% dplyr::mutate(destino = dpto_d)
    else df0 <- df0 %>% dplyr::mutate(destino = cod_dpto_d)
    
    agg <- df0 %>%
      dplyr::group_by(anio, destino) %>%
      dplyr::summarise(kg = sum(cant_kg, na.rm = TRUE), .groups="drop") %>%
      dplyr::filter(is.finite(kg), kg > 0) %>%
      dplyr::mutate(ton = kg_to_ton(kg))
    
    tot <- agg %>%
      dplyr::group_by(anio) %>%
      dplyr::summarise(total_ton = sum(ton), .groups="drop") %>%
      dplyr::filter(is.finite(total_ton), total_ton > 0)
    
    hhi <- agg %>%
      dplyr::left_join(tot, by="anio") %>%
      dplyr::mutate(p = ton / total_ton) %>%
      dplyr::group_by(anio) %>%
      dplyr::summarise(hhi01 = sum(p^2, na.rm = TRUE), total_ton = dplyr::first(total_ton), .groups="drop") %>%
      dplyr::filter(is.finite(hhi01)) %>%
      dplyr::mutate(
        hhi01_lbl = fmt_num_co(hhi01, 3),
        total_lbl = fmt_num_co(total_ton, 1),
        tooltip = paste0(
          "<b>Atlántico → destinos</b>",
          if (!is_all(input$grupo_t3)) paste0("<br>Grupo: ", input$grupo_t3) else "<br>Grupo: (todos)",
          if (!is_all(input$alim_t3))  paste0("<br>Alimento: ", input$alim_t3) else "",
          "<br>Año: ", anio,
          "<br>HHI destinos (0–1): ", hhi01_lbl,
          "<br>Total: ", total_lbl, " Ton",
          "<extra></extra>"
        )
      )
    
    validate(need(nrow(hhi) > 0, "No hay serie temporal HHI (TAB 3) con esos filtros."))
    hhi
  })
  
  output$t3_plot_a <- renderPlotly({
    df <- hhi_food_t3()
    top <- df %>% dplyr::slice_head(n = 15) %>% dplyr::mutate(alimento_ord = factor(alimento, levels = rev(alimento)))
    
    plotly::plot_ly(
      data = top,
      x = ~hhi01,
      y = ~alimento_ord,
      type = "bar",
      orientation = "h",
      text = ~hhi01_lbl,
      textposition = "auto",
      hovertext = ~tooltip,
      hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = "HHI destinos (0–1)", rangemode = "tozero"),
        yaxis = list(title = ""),
        margin = list(l=210, r=20, t=10, b=55)
      )
  })
  
  output$t3_plot_b <- renderPlotly({
    ts <- hhi_year_t3()
    plotly::plot_ly(
      data = ts,
      x = ~anio, y = ~hhi01,
      type = "scatter", mode = "lines+markers",
      hovertext = ~tooltip, hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Año", tickmode = "linear", dtick = 1),
        yaxis = list(title = "HHI destinos (0–1)", rangemode = "tozero"),
        margin = list(l=60, r=20, t=10, b=60)
      )
  })
  
  # ✅ BLOQUE E (resumen gráfico de la tabla)
  output$t3_plot_c <- renderPlotly({
    df <- hhi_food_t3()
    
    top_dest <- df %>%
      dplyr::group_by(destino_top) %>%
      dplyr::summarise(
        n_alimentos = dplyr::n(),
        total_ton = sum(as.numeric(gsub("\\.","", gsub(",",".", gsub("[^0-9,\\.]", "", total_lbl)))), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(n_alimentos)) %>%
      dplyr::slice_head(n = 12) %>%
      dplyr::mutate(
        destino_ord = factor(destino_top, levels = rev(destino_top)),
        tooltip = paste0(
          "<b>", destino_top, "</b>",
          "<br># alimentos donde es principal: ", n_alimentos,
          "<extra></extra>"
        )
      )
    
    validate(need(nrow(top_dest) > 0, "Sin datos para Bloque E."))
    
    plotly::plot_ly(
      data = top_dest,
      x = ~n_alimentos,
      y = ~destino_ord,
      type = "bar",
      orientation = "h",
      text = ~n_alimentos,
      textposition = "auto",
      hovertext = ~tooltip,
      hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = "# alimentos", rangemode = "tozero"),
        yaxis = list(title = ""),
        margin = list(l=210, r=20, t=10, b=55)
      )
  })
  
  # ✅ BLOQUE F (resumen gráfico de la tabla)
  output$t3_plot_d2 <- renderPlotly({
    df <- hhi_food_t3() %>%
      dplyr::mutate(
        total_ton_num = suppressWarnings(as.numeric(
          gsub("\\.","", gsub(",",".", gsub("[^0-9,\\.]", "", total_lbl)))
        )),
        total_ton_lbl2 = total_lbl
      ) %>%
      dplyr::filter(is.finite(total_ton_num))
    
    validate(need(nrow(df) > 0, "Sin datos para Bloque F."))
    
    plotly::plot_ly(
      data = df,
      x = ~total_ton_num,
      y = ~hhi01,
      type = "scatter",
      mode = "markers",
      hovertext = ~paste0(
        "<b>", alimento, "</b>",
        "<br>Destino principal: ", destino_top,
        "<br>HHI: ", hhi01_lbl,
        "<br>Total (Ton): ", total_ton_lbl2,
        "<extra></extra>"
      ),
      hovertemplate = "%{hovertext}"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Total (Ton)", rangemode = "tozero"),
        yaxis = list(title = "HHI (0–1)", rangemode = "tozero"),
        margin = list(l=60, r=20, t=10, b=60)
      )
  })
  
  output$t3_table <- DT::renderDT({
    df <- hhi_food_t3() %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::transmute(
        `#` = rank,
        Grupo = grupo,
        Alimento = alimento,
        `Destino principal` = destino_top,
        `HHI destinos (0–1)` = hhi01_lbl,
        `N destinos` = ndest,
        `N efectivo` = nequiv_lbl,
        `Total (Ton)` = total_lbl
      )
    
    DT::datatable(
      df, rownames = FALSE, escape = TRUE,
      options = dt_opts_lang(pageLength = 10),
      class = "stripe hover order-column compact"
    )
  }, server = FALSE)
}

shinyApp(ui, server)




