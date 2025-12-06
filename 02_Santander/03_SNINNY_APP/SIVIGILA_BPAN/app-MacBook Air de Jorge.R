# app_bpan.R
# =========================================================
# BPAN — Dashboard (app exclusiva)
# - Tab 1: Exploración (mapa, top-10, balance neto con tooltip)
# - Tab 2: Análisis poblacional (4 cuadrantes):
#     Q1 Mapa incidencia, Q2 Top-10 incidencia DESTINO,
#     Q3 Dispersión Casos vs Población, Q4 Top-10 incidencia ORIGEN
# - Fix: usar leaflet::addLegend para evitar conflicto con xts
# =========================================================

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(leaflet); library(sf); library(dplyr); library(tidyr)
  library(scales); library(htmltools); library(DT); library(plotly)
})

options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)
options(shiny.maxRequestSize = 100*1024^2)

validate <- shiny::validate
need     <- shiny::need

# ---------- Rutas ----------
local_data_dir <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/SIVIGILA_BPAN/data"
app_root     <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
rel_data_dir <- file.path(app_root, "data")
data_dir <- if (dir.exists(rel_data_dir)) rel_data_dir else local_data_dir

bpan_path      <- file.path(data_dir, "021_INS_SIVIGILA-BPAN.rds")
ruta_pob       <- file.path(data_dir, "051_DANE_Proyecciones_P_total.rds")
ruta_shp_mpios <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")
ruta_shp_dptos <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")

must_exist <- c(bpan_path, ruta_pob, ruta_shp_mpios, ruta_shp_dptos)

miss <- must_exist[!file.exists(must_exist)]
if (length(miss)) stop("Faltan archivos. data_dir usado: ", data_dir, "\n", paste("-", miss, collapse = "\n"))
check_shp_parts <- function(shp){ base <- sub("\\.shp$", "", shp); req <- paste0(base, c(".shp",".dbf",".shx",".prj")); req[!file.exists(req)] }
miss_shp <- c(check_shp_parts(ruta_shp_mpios), check_shp_parts(ruta_shp_dptos))
if (length(miss_shp)) stop("Faltan componentes de shapefile:\n", paste("-", miss_shp, collapse = "\n"))

# ---------- 1) Leer BPAN (casos = filas) ----------
bpan_raw <- readRDS(bpan_path) %>% dplyr::filter(!is.na(MUNICIPIO_D))

b_month_col       <- if ("mes" %in% names(bpan_raw)) "mes" else if ("MES" %in% names(bpan_raw)) "MES" else stop("BPAN: no 'mes'/'MES'")
b_year_col       <- if ("ano" %in% names(bpan_raw)) "ano" else if ("ANO" %in% names(bpan_raw)) "ANO" else stop("BPAN: no 'ano'/'ANO'")
b_mun_code_col   <- if ("COD_DANE_MUNIC_D" %in% names(bpan_raw)) "COD_DANE_MUNIC_D" else stop("BPAN: no 'COD_DANE_MUNIC_D'")
b_dep_name_col   <- if ("DEPARTAMENTO_D" %in% names(bpan_raw)) "DEPARTAMENTO_D" else if ("DEPARTMENTO_D" %in% names(bpan_raw)) "DEPARTMENTO_D" else "DEPARTAMENTO_D"
b_mun_name_col   <- if ("MUNICIPIO_D" %in% names(bpan_raw)) "MUNICIPIO_D" else "MUNICIPIO_D"
b_dep_origen_col <- if ("DEPARTAMENTO_O" %in% names(bpan_raw)) "DEPARTAMENTO_O" else NA_character_
b_mun_origen_col <- if ("MUNICIPIO_O"   %in% names(bpan_raw)) "MUNICIPIO_O"   else NA_character_

bpan <- bpan_raw %>%
  dplyr::transmute(
    mes       = suppressWarnings(as.integer(.data[[b_month_col]])),
    ano       = suppressWarnings(as.integer(.data[[b_year_col]])),
    COD_MUN5  = sprintf("%05d", suppressWarnings(as.integer(.data[[b_mun_code_col]]))),
    COD_DPTO2 = substr(COD_MUN5, 1, 2),
    DEP_D     = trimws(as.character(.data[[b_dep_name_col]])),
    MUN_D     = trimws(as.character(.data[[b_mun_name_col]])),
    DEP_O     = if (!is.na(b_dep_origen_col)) trimws(as.character(.data[[b_dep_origen_col]])) else NA_character_,
    MUN_O     = if (!is.na(b_mun_origen_col)) trimws(as.character(.data[[b_mun_origen_col]])) else NA_character_,
    edad      = suppressWarnings(as.numeric(edad)),
    PAC_HOS   = suppressWarnings(as.integer(pac_hos))   # 👈 NUEVO
  ) %>%
  dplyr::filter(!is.na(ano), !is.na(COD_MUN5), !is.na(COD_DPTO2))

# Años/filas con ORIGEN válido (para Tab 1)
bpan_valid <- bpan %>% dplyr::filter(!is.na(DEP_O), DEP_O != "", !is.na(MUN_O), MUN_O != "")

# ---------- 1b) Población ----------
pob_raw <- readRDS(ruta_pob)
pob_year_col <- dplyr::case_when("ano"%in%names(pob_raw)~"ano","ANO"%in%names(pob_raw)~"ANO","year"%in%names(pob_raw)~"year",TRUE~NA_character_)
stopifnot(!is.na(pob_year_col))
pob_mun_code_col <- dplyr::case_when("COD_MUN5"%in%names(pob_raw)~"COD_MUN5","COD_DANE_MUNIC_D"%in%names(pob_raw)~"COD_DANE_MUNIC_D","COD_MPIO"%in%names(pob_raw)~"COD_MPIO","MPIO_CDPMP"%in%names(pob_raw)~"MPIO_CDPMP",TRUE~NA_character_)
stopifnot(!is.na(pob_mun_code_col))
pob_val_col <- dplyr::case_when("P_TOTAL"%in%names(pob_raw)~"P_TOTAL","Poblacion"%in%names(pob_raw)~"Poblacion","POBLACION"%in%names(pob_raw)~"POBLACION","total"%in%names(pob_raw)~"total","poblacion"%in%names(pob_raw)~"poblacion",TRUE~NA_character_)
stopifnot(!is.na(pob_val_col))

pob_norm <- pob_raw %>%
  dplyr::transmute(
    ano       = suppressWarnings(as.integer(.data[[pob_year_col]])),
    COD_MUN5  = sprintf("%05d", suppressWarnings(as.integer(.data[[pob_mun_code_col]]))),
    COD_DPTO2 = substr(COD_MUN5, 1, 2),
    POB       = suppressWarnings(as.numeric(.data[[pob_val_col]]))
  ) %>% dplyr::filter(!is.na(ano), !is.na(COD_MUN5), !is.na(COD_DPTO2), is.finite(POB))

pob_mpio  <- pob_norm %>% dplyr::group_by(ano, COD_MUN5, COD_DPTO2) %>% dplyr::summarise(POB = sum(POB, na.rm = TRUE), .groups = "drop")
pob_depto <- pob_norm %>% dplyr::group_by(ano, COD_DPTO2) %>% dplyr::summarise(POB = sum(POB, na.rm = TRUE), .groups = "drop")

# ---------- 2) Shapes ----------
mpios_raw <- sf::st_read(ruta_shp_mpios, quiet = TRUE)
dptos_raw <- sf::st_read(ruta_shp_dptos, quiet = TRUE)

mpios_sf <- mpios_raw %>%
  dplyr::mutate(
    COD_MUN5 = if ("MPIO_CDPMP" %in% names(.)) sprintf("%05d", as.integer(MPIO_CDPMP))
    else if ("COD_MPIO" %in% names(.)) sprintf("%05d", as.integer(COD_MPIO))
    else stop("Shp municipios: falta MPIO_CDPMP/COD_MPIO"),
    COD_DPTO2   = substr(COD_MUN5, 1, 2),
    MUNICIPIO_N = if ("MPIO_CNMBR" %in% names(.)) as.character(MPIO_CNMBR)
    else if ("NOMBRE_MPIO" %in% names(.)) as.character(NOMBRE_MPIO)
    else "MUNICIPIO"
  ) %>% sf::st_transform(4326) %>% sf::st_make_valid()

dptos_sf <- dptos_raw %>%
  dplyr::mutate(
    COD_DPTO2 = if ("DPTO_CCDGO" %in% names(.)) sprintf("%02d", as.integer(DPTO_CCDGO))
    else if ("COD_DEPTO" %in% names(.)) sprintf("%02d", as.integer(COD_DEPTO))
    else stop("Shp deptos: falta DPTO_CCDGO/COD_DEPTO"),
    DEPARTAMENTO_N = if ("DEPARTAMENTO_D" %in% names(.)) as.character(DEPARTAMENTO_D)
    else if ("DPTO_CNMBR" %in% names(.)) as.character(DPTO_CNMBR)
    else if ("NOMBRE_DEPTO" %in% names(.)) as.character(NOMBRE_DEPTO)
    else COD_DPTO2
  ) %>% sf::st_transform(4326) %>% sf::st_make_valid()

# ---------- Lookups ----------
dpt_lookup_bpan <- bpan %>% dplyr::select(COD_DPTO2, DEP_D) %>%
  dplyr::mutate(COD_DPTO2=sprintf("%02d",as.integer(COD_DPTO2)), DEP_D=trimws(DEP_D)) %>%
  dplyr::distinct() %>% dplyr::arrange(DEP_D)

mun_lookup_bpan <- bpan %>% dplyr::select(COD_DPTO2, COD_MUN5, MUN_D) %>%
  dplyr::mutate(COD_DPTO2=sprintf("%02d",as.integer(COD_DPTO2)), COD_MUN5=sprintf("%05d",as.integer(COD_MUN5)), MUN_D=trimws(MUN_D)) %>%
  dplyr::distinct()

# mapa nombres→códigos para ORIGEN depto
dpt_name_code <- dptos_sf %>% sf::st_drop_geometry() %>%
  dplyr::transmute(DEP_O = trimws(DEPARTAMENTO_N), COD_DPTO2 = sprintf("%02d", as.integer(COD_DPTO2)))

# ---------- Helper paleta ----------
make_pal <- function(values, palette) {
  vals <- values[is.finite(values)]
  if (!length(vals)) vals <- 0
  qs <- stats::quantile(vals[vals > 0], probs = seq(0, 1, length.out = 6), na.rm = TRUE)
  qs <- unique(as.numeric(qs))
  if (length(qs) < 3) qs <- pretty(vals, n = 5)
  bins <- sort(unique(c(min(vals, na.rm = TRUE), qs, max(vals, na.rm = TRUE))))
  leaflet::colorBin(palette, domain = vals, bins = bins, na.color = "#f0f0f0")
}

# ---------- 3) UI ----------
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    primary = "#2563eb",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius" = "0.9rem",
    "font-size-base" = "0.98rem"
  ),
  tags$head(
    tags$style(HTML("
      .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
      h3{font-weight:700;letter-spacing:.2px;margin-bottom:8px}
      .data-note{font-size:13px;color:#6b7280;margin:0 0 16px}
      .filters{background:#fff;border:1px solid #eaecef;border-radius:16px;padding:14px 16px;margin-bottom:16px;box-shadow:0 4px 14px rgba(0,0,0,.06)}
      .filters-grid{display:grid;grid-template-columns:repeat(6,minmax(180px,1fr));gap:12px}
      .filter-label{font-size:12px;font-weight:600;letter-spacing:.4px;text-transform:uppercase;color:#6b7280;margin-bottom:6px}
      .selectize-input,.form-control{min-height:42px;border-radius:10px}
      .selectize-input{padding:10px 12px}
      .card{background:#fff;border:1px solid #eaecef;border-radius:16px;padding:12px;box-shadow:0 2px 10px rgba(0,0,0,.05);margin-bottom:12px}
      .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}
      .info-badge{display:inline-flex;justify-content:center;align-items:center;width:18px;height:18px;margin-left:6px;border-radius:50%;font-size:12px;line-height:1;font-weight:700;background:#e5e7eb;color:#111827;cursor:help;border:1px solid #d1d5db;}
      .info-badge:hover{ background:#dbe1e6 }
    ")),
    tags$script(HTML("
      function initBsTooltips(){
        var list = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'));
        list.map(function(el){ try { return new bootstrap.Tooltip(el, {container: 'body'}); } catch(e){} });
      }
      document.addEventListener('DOMContentLoaded', initBsTooltips);
      document.addEventListener('shiny:value', initBsTooltips);
      document.addEventListener('shown.bs.tab', initBsTooltips);
    "))
  ),
  div(class="wrap",
      h3("SIVIGILA — BPAN"),
      div(class="data-note", HTML("Nota: Las cifras de 2023 son <b>resultados preliminares</b>.")),
      tabsetPanel(
        id = "tabs_bpan",
        type = "tabs",
        
        # ---------------- TAB 1: Exploración ----------------
        tabPanel(
          title = "Exploración BPAN",
          br(),
          div(class="filters",
              div(class="filters-grid",
                  div(class="filter", div(class="filter-label","Seleccione Año:"), uiOutput("anio_bpan_ui")),
                  div(class="filter", div(class="filter-label","Sel. Departamento (Ocurrencia):"),
                      selectInput("f_depto_bpan", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Sel. Municipio (Ocurrencia):"),
                      selectInput("f_mpio_bpan", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Sel. Departamento (Notificación): "),
                      selectInput("f_depto_dest", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Municipio (Notificación)"),
                      selectInput("f_mpio_dest", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Acción"),
                      actionLink("btn_reset_bpan","← Limpiar filtros"))
              )
          ),
          fluidRow(
            column(
              width = 6,
              div(class="card",
                  # ====== Título dinámico del mapa ======
                  div(class="card-title", textOutput("map_bpan_title")),
                  leafletOutput("map_bpan", height = 700)
              )
            ),
            column(
              width = 6,
              div(class="card",
                  # ====== NUEVO TÍTULO del Top-10 ======
                  div(class="card-title","¿Cuáles son los 10 municipios con el mayor número de casos reportados de bajo peso al nacer?"),
                  plotlyOutput("bar_bpan", height = 310)
              ),
              div(
                class="card",
                div(
                  class="card-title",
                  HTML(
                    'Balance de atención (neto origen↔destino)
                     <span class="info-badge"
                           data-bs-toggle="tooltip"
                           data-bs-placement="left"
                           title="¿Qué muestra este gráfico?
Verde: tu territorio ENVÍA más casos a ese destino (sale más de lo que entra).
Rojo: tu territorio RECIBE más casos desde ese destino (entra más de lo que sale).
Gris: ida y vuelta parecidos.
Úsalo para ver si tu territorio depende de otros para atender los casos o si actúa como centro receptor.">
                     i</span>'
                  )
                ),
                plotlyOutput("balance_bpan", height = 310)
              )
            )
          )
        ),
        
        # ---------------- TAB 2: Análisis poblacional (4 cuadrantes) ----------------
        tabPanel(
          title = "Análisis poblacional",
          br(),
          div(class="filters",
              div(class="filters-grid",
                  div(class="filter", div(class="filter-label","Año"), uiOutput("anio_b2_ui")),
                  div(class="filter", div(class="filter-label","Departamento (para ver municipios)"),
                      selectInput("f_depto_b2", NULL, choices = c("Todos", dpt_lookup_bpan$DEP_D), selected = "Todos")),
                  div(class="filter", div(class="filter-label","Municipio"),
                      selectInput("f_mpio_b2", NULL, choices = "Todos", selected = "Todos"))
              )
          ),
          fluidRow(
            # Q1
            column(6,
                   div(class="card",
                       div(class="card-title","Mapa de incidencia (x100.000 hab.)"),
                       leafletOutput("map_bpan_incid", height = 380)
                   ),
                   # Q3
                   div(class="card",
                       div(class="card-title","Casos BPAN por grupos de edad materna"),
                       plotlyOutput("edad_materna_barras", height = 320)
                   )
            ),
            # Q2 & Q4 (dos barras)
            column(6,
                   # ---- Top-20 incidencia (DESTINO) ----
                   div(
                     class = "card",
                     div(class = "card-title", "Top-20 incidencia por departamento (DESTINO)"),
                     plotlyOutput("top_incid_dest_top20", height = 750)
                   )
            )
          )
        ),
        tabPanel(
          title = "Análisis de fallecimientos por Bajo Peso al Nacer (BPAN)",
          br(),
          div(class="filters",
              div(class="filters-grid",
                  div(class="filter", div(class="filter-label","Años"),
                      uiOutput("anio_b3_ui")),
                  div(class="filter", div(class="filter-label","Departamento (ORIGEN)"),
                      uiOutput("dep_o_b3_ui")),
                  div(class="filter", div(class="filter-label","Municipio (ORIGEN)"),
                      uiOutput("mun_o_b3_ui")),
                  div(class="filter", div(class="filter-label","Departamento (DESTINO)"),
                      uiOutput("dep_d_b3_ui")),
                  div(class="filter", div(class="filter-label","Municipio (DESTINO)"),
                      uiOutput("mun_d_b3_ui"))
              )
          ),
          
          fluidRow(
            # Columna izquierda: Q1 + Q2 (apilados)
            column(
              6,
              div(class="card",
                  div(class="card-title", HTML('Serie anual de casos de Supervivencia y Fallecimientos')),
                  plotlyOutput("serie_anual_tab3", height = 360)
              ),
              div(class="card",
                  div(class="card-title",
                      HTML('Fallecimientos de la edad 
                           <span class="info-badge" data-bs-toggle="tooltip"
                                 title="Solo se grafican los registros con edad válida.">i</span>')),
                  plotlyOutput("hist_edad_tab3", height = 320)
              )
            ),
            
            # Columna derecha: Q3 alto (mapa de supervivencia)
            column(
              6,
              div(class="card",
                  div(class="card-title",
                      HTML('Tasa de supervivencia (vivos/(vivos+muertes))
               <span class="info-badge" data-bs-toggle="tooltip"
                     title="Porcentaje de casos con resultado Vivo entre (Vivos+Muertes). Se calcula sobre los registros con PAC_HOS válido (1 o 2).">i</span>')),
                  leafletOutput("map_superv_b3", height = 750)   # 👈 NUEVO
              )
            )
            
          )
        )
        
      )
  )
)

# ---------- 4) SERVER ----------
server <- function(input, output, session){
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  
  # ================= TAB 1 — Exploración =================
  output$anio_bpan_ui <- renderUI({
    yrs <- sort(unique(bpan_valid$ano))
    selectInput("f_anio_bpan", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  bpan_base_no_dest <- reactive({
    req(input$f_anio_bpan)
    df <- bpan_valid %>% dplyr::filter(ano == input$f_anio_bpan)
    if (!is.null(input$f_depto_bpan) && input$f_depto_bpan != "Todos") df <- df %>% dplyr::filter(DEP_O == input$f_depto_bpan)
    if (!is.null(input$f_mpio_bpan)  && input$f_mpio_bpan  != "Todos") df <- df %>% dplyr::filter(MUN_O == input$f_mpio_bpan)
    df
  })
  bpan_base <- reactive({
    df <- bpan_base_no_dest()
    if (!is.null(input$f_depto_dest) && input$f_depto_dest != "Todos") df <- df %>% dplyr::filter(DEP_D == input$f_depto_dest)
    if (!is.null(input$f_mpio_dest)  && input$f_mpio_dest  != "Todos") df <- df %>% dplyr::filter(MUN_D == input$f_mpio_dest)
    df
  })
  
  # Combos
  observeEvent(input$f_anio_bpan, {
    df_year <- bpan_valid %>% dplyr::filter(ano == input$f_anio_bpan)
    deps_o  <- df_year %>% dplyr::distinct(DEP_O) %>% dplyr::arrange(DEP_O) %>% dplyr::pull(DEP_O)
    sel_dep <- if (!is.null(input$f_depto_bpan) && input$f_depto_bpan %in% deps_o) input$f_depto_bpan else "Todos"
    updateSelectInput(session, "f_depto_bpan", choices = c("Todos", deps_o), selected = sel_dep)
    updateSelectInput(session, "f_mpio_bpan",  choices = "Todos", selected = "Todos")
  }, ignoreInit = FALSE)
  
  observeEvent(input$f_depto_bpan, {
    df <- bpan_valid %>% dplyr::filter(ano == input$f_anio_bpan)
    if (!is.null(input$f_depto_bpan) && input$f_depto_bpan != "Todos") {
      mpios_o <- df %>% dplyr::filter(DEP_O == input$f_depto_bpan) %>% dplyr::distinct(MUN_O) %>% dplyr::arrange(MUN_O) %>% dplyr::pull(MUN_O)
      updateSelectInput(session, "f_mpio_bpan", choices = c("Todos", mpios_o), selected = "Todos")
    } else {
      updateSelectInput(session, "f_mpio_bpan", choices = "Todos", selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$f_anio_bpan, input$f_depto_bpan, input$f_mpio_bpan), {
    df <- bpan_base_no_dest()
    deps_d <- df %>% dplyr::distinct(DEP_D) %>% dplyr::arrange(DEP_D) %>% dplyr::pull(DEP_D)
    sel_dep_d <- if (!is.null(input$f_depto_dest) && input$f_depto_dest %in% deps_d) input$f_depto_dest else "Todos"
    updateSelectInput(session, "f_depto_dest", choices = c("Todos", deps_d), selected = sel_dep_d)
    updateSelectInput(session, "f_mpio_dest",  choices = "Todos", selected = "Todos")
  }, ignoreInit = FALSE)
  
  observeEvent(input$f_depto_dest, {
    df <- bpan_base_no_dest()
    if (!is.null(input$f_depto_dest) && input$f_depto_dest != "Todos") {
      mpios_d <- df %>% dplyr::filter(DEP_D == input$f_depto_dest) %>% dplyr::distinct(MUN_D) %>% dplyr::arrange(MUN_D) %>% dplyr::pull(MUN_D)
      updateSelectInput(session, "f_mpio_dest", choices = c("Todos", mpios_d), selected = "Todos")
    } else {
      updateSelectInput(session, "f_mpio_dest", choices = "Todos", selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  map_nivel <- reactive({ if (!is.null(input$f_depto_dest) && input$f_depto_dest != "Todos") "mpios" else "deptos" })
  sel_cod_dep <- reactive({
    if (map_nivel() == "mpios") {
      cod <- dptos_sf %>% dplyr::filter(DEPARTAMENTO_N == input$f_depto_dest) %>% dplyr::pull(COD_DPTO2) %>% .[1]
      if (is.na(cod) || !nzchar(cod)) cod <- bpan_base() %>% dplyr::distinct(COD_DPTO2) %>% dplyr::pull(COD_DPTO2) %>% .[1]
      cod
    } else NA_character_
  })
  
  # ====== Título dinámico del mapa de casos ======
  output$map_bpan_title <- renderText({
    if (map_nivel() == "deptos") {
      "¿Dónde se concentran los casos de bajo peso al nacer a nivel departamental?"
    } else {
      dep_sel <- if (!is.null(input$f_depto_dest) && input$f_depto_dest != "Todos") input$f_depto_dest else "Departamento seleccionado"
      paste0("¿Dónde se concentran los casos de bajo peso al nacer a nivel municipal (", dep_sel, ")?")
    }
  })
  
  bpan_agg_depto <- reactive({
    bpan_base() %>% dplyr::group_by(COD_DPTO2, DEP_D) %>% dplyr::summarise(valor=dplyr::n(), .groups="drop")
  })
  bpan_agg_mpio  <- reactive({
    bpan_base() %>% dplyr::group_by(COD_DPTO2, COD_MUN5, MUN_D) %>% dplyr::summarise(valor=dplyr::n(), .groups="drop")
  })
  
  # Mapa (casos)
  output$map_bpan <- renderLeaflet({
    leaflet::leaflet() %>% leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% leaflet::setView(lng=-74.3, lat=4.6, zoom=5)
  })
  observe({
    titulo <- if (map_nivel()=="deptos") "Casos por departamento" else "Casos por municipio"
    fmt_val <- function(x) scales::comma(x)
    pal_cols <- c("#fff9db", "#ffe082", "#ffd54f", "#fbc02d", "#c49000")
    
    if (map_nivel()=="deptos") {
      shp <- dptos_sf %>% dplyr::left_join(bpan_agg_depto(), by="COD_DPTO2") %>%
        dplyr::mutate(valor=tidyr::replace_na(valor,0), etq=paste0("<b>", DEPARTAMENTO_N, "</b><br>", titulo, ": ", fmt_val(valor)))
      pal <- make_pal(shp$valor, pal_cols)
      leaflet::leafletProxy("map_bpan", data=shp) %>% leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId=~COD_DPTO2, fillColor=~pal(valor), color="#666", weight=0.7, fillOpacity=0.9,
                             label=~lapply(etq, htmltools::HTML),
                             highlightOptions=leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        leaflet::addLegend(position="bottomright", pal=pal, values=~valor, title=titulo)
    } else {
      sel_cod <- sel_cod_dep(); req(!is.na(sel_cod), nzchar(sel_cod))
      shp <- mpios_sf %>% dplyr::filter(COD_DPTO2==sel_cod) %>% dplyr::left_join(bpan_agg_mpio(), by=c("COD_DPTO2","COD_MUN5")) %>%
        dplyr::mutate(valor=tidyr::replace_na(valor,0), etq=paste0("<b>", MUNICIPIO_N, "</b><br>", titulo, ": ", fmt_val(valor)))
      pal <- make_pal(shp$valor, pal_cols); bb  <- sf::st_bbox(shp)
      leaflet::leafletProxy("map_bpan", data=shp) %>% leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId=~COD_MUN5, fillColor=~pal(valor), color="#666", weight=0.4, fillOpacity=0.9,
                             label=~lapply(etq, htmltools::HTML),
                             highlightOptions=leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        leaflet::addLegend(position="bottomright", pal=pal, values=~valor, title=titulo) %>%
        leaflet::fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  
  observeEvent(input$map_bpan_shape_click, {
    click <- input$map_bpan_shape_click; req(click$id)
    if (map_nivel()=="deptos") {
      cod <- sprintf("%02d", as.integer(click$id))
      nom_shape <- dptos_sf$DEPARTAMENTO_N[match(cod, dptos_sf$COD_DPTO2)]
      deps_year <- bpan_base_no_dest() %>% dplyr::distinct(DEP_D) %>% dplyr::pull(DEP_D)
      if (!is.na(nom_shape) && nzchar(nom_shape) && nom_shape %in% deps_year) updateSelectInput(session, "f_depto_dest", selected = nom_shape)
    } else {
      codm <- sprintf("%05d", as.integer(click$id))
      nom_mpio <- mpios_sf$MUNICIPIO_N[match(codm, mpios_sf$COD_MUN5)]
      if (!is.null(input$f_depto_dest) && input$f_depto_dest != "Todos") {
        mpios_year_dep <- bpan_base_no_dest() %>% dplyr::filter(DEP_D == input$f_depto_dest) %>% dplyr::distinct(MUN_D) %>% dplyr::pull(MUN_D)
        if (!is.na(nom_mpio) && nzchar(nom_mpio) && nom_mpio %in% mpios_year_dep) updateSelectInput(session, "f_mpio_dest", selected = nom_mpio)
      }
    }
  }, ignoreInit = TRUE)
  
  # ====== Top-10 MUNICIPIOS (DESTINO), respetando filtros ======
  top10_df <- reactive({
    df <- bpan_base() %>%
      dplyr::group_by(COD_MUN5, MUN_D) %>%
      dplyr::summarise(valor = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(nombre = dplyr::if_else(!is.na(MUN_D) & nzchar(MUN_D), MUN_D, COD_MUN5)) %>%
      dplyr::arrange(dplyr::desc(valor)) %>%
      dplyr::slice_head(n = 10)
    df
  })
  
  output$bar_bpan <- renderPlotly({
    df <- top10_df()
    validate(need(nrow(df) > 0, "No hay datos para el Top-10 con los filtros actuales."))
    
    df2 <- df %>% dplyr::arrange(valor)
    plot_ly(
      data = df2,
      x = ~valor,
      y = ~factor(nombre, levels = df2$nombre),
      type = "bar",
      orientation = "h",
      marker = list(color = "#fbc02d"),   # ====== COLOR DE BARRAS
      hovertemplate = "%{y}<br>Casos: %{x:,}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Casos", showgrid = FALSE, zeroline = FALSE),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 10, r = 10, t = 10, b = 10),
        showlegend = FALSE
      )
  })
  
  # Balance neto
  balance_df <- reactive({
    req(input$f_anio_bpan)
    origen_dep <- input$f_depto_bpan %||% "Todos"
    origen_mun <- input$f_mpio_bpan  %||% "Todos"
    validate(need(origen_dep != "Todos","Selecciona un Departamento (ORIGEN) para calcular el balance."))
    df_year <- bpan %>% dplyr::filter(ano == input$f_anio_bpan)
    
    if (map_nivel() == "deptos" || origen_mun == "Todos") {
      AtoB <- df_year %>% dplyr::filter(DEP_O == origen_dep) %>% dplyr::group_by(DEP_D) %>% dplyr::summarise(AtoB = dplyr::n(), .groups = "drop") %>% dplyr::rename(nombre = DEP_D)
      BtoA <- df_year %>% dplyr::filter(DEP_D == origen_dep) %>% dplyr::group_by(DEP_O) %>% dplyr::summarise(BtoA = dplyr::n(), .groups = "drop") %>% dplyr::rename(nombre = DEP_O)
      dplyr::full_join(AtoB, BtoA, by = "nombre") %>% dplyr::mutate(AtoB = tidyr::replace_na(AtoB, 0L), BtoA = tidyr::replace_na(BtoA, 0L), neto = AtoB - BtoA) %>%
        dplyr::arrange(dplyr::desc(abs(neto)))
    } else {
      validate(need(!is.null(input$f_depto_dest) && input$f_depto_dest != "Todos","Selecciona un Departamento (DESTINO) para ver el balance municipal."))
      dest_dep <- input$f_depto_dest
      AtoB <- df_year %>% dplyr::filter(DEP_O == origen_dep, DEP_D == dest_dep) %>% { if (origen_mun != "Todos") dplyr::filter(., MUN_O == origen_mun) else . } %>%
        dplyr::group_by(MUN_D) %>% dplyr::summarise(AtoB = dplyr::n(), .groups = "drop") %>% dplyr::rename(nombre = MUN_D)
      BtoA <- df_year %>% dplyr::filter(DEP_O == dest_dep, DEP_D == origen_dep) %>% { if (origen_mun != "Todos") dplyr::filter(., MUN_D == origen_mun) else . } %>%
        dplyr::group_by(MUN_O) %>% dplyr::summarise(BtoA = dplyr::n(), .groups = "drop") %>% dplyr::rename(nombre = MUN_O)
      dplyr::full_join(AtoB, BtoA, by = "nombre") %>% dplyr::mutate(AtoB = tidyr::replace_na(AtoB, 0L), BtoA = tidyr::replace_na(BtoA, 0L), neto = AtoB - BtoA) %>%
        dplyr::arrange(dplyr::desc(abs(neto)))
    }
  })
  output$balance_bpan <- renderPlotly({
    df <- balance_df()
    validate(need(nrow(df) > 0, "No hay datos para el balance con los filtros actuales."))
    df <- df %>% dplyr::mutate(color = dplyr::case_when(neto > 0 ~ "#22c55e", neto < 0 ~ "#ef4444", TRUE ~ "#9ca3af"),
                               AtoB = tidyr::replace_na(AtoB, 0L), BtoA = tidyr::replace_na(BtoA, 0L))
    df2 <- df %>% dplyr::arrange(neto)
    cd <- as.matrix(df2[, c("AtoB", "BtoA")])
    plot_ly(data = df2, x = ~neto, y = ~factor(nombre, levels = df2$nombre),
            type = "bar", orientation = "h", marker = list(color = df2$color),
            customdata = cd,
            hovertemplate = "%{y}<br><b>Balance neto:</b> %{x:,}<br>Sale (A→B): %{customdata[0]:,}<br>Entra (B→A): %{customdata[1]:,}<extra></extra>") %>%
      layout(xaxis = list(title = "Balance neto (A→B − B→A)", zeroline = TRUE, zerolinewidth = 1),
             yaxis = list(title = "", automargin = TRUE),
             margin = list(l = 10, r = 10, t = 10, b = 10),
             showlegend = FALSE)
  })
  
  # ================= TAB 2 — Análisis poblacional =================
  output$anio_b2_ui <- renderUI({
    yrs <- sort(unique(bpan$ano))
    selectInput("f_anio_b2", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  observeEvent(input$f_depto_b2, {
    if (is.null(input$f_depto_b2) || input$f_depto_b2 == "Todos") {
      updateSelectInput(session, "f_mpio_b2", choices = "Todos", selected = "Todos")
    } else {
      sel_dep <- dpt_lookup_bpan$COD_DPTO2[dpt_lookup_bpan$DEP_D == input$f_depto_b2][1]
      mm <- mun_lookup_bpan %>% dplyr::filter(COD_DPTO2 == sel_dep) %>% dplyr::arrange(MUN_D)
      updateSelectInput(session, "f_mpio_b2", choices = c("Todos", mm$MUN_D), selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  nivel_b2 <- reactive({ if (is.null(input$f_depto_b2) || input$f_depto_b2 == "Todos") "deptos" else "mpios" })
  base_b2  <- reactive({
    req(input$f_anio_b2)
    df <- bpan %>% dplyr::filter(ano == input$f_anio_b2)
    if (!is.null(input$f_depto_b2) && input$f_depto_b2 != "Todos") df <- df %>% dplyr::filter(DEP_D == input$f_depto_b2)
    if (!is.null(input$f_mpio_b2)  && input$f_mpio_b2  != "Todos") df <- df %>% dplyr::filter(MUN_D == input$f_mpio_b2)
    df
  })
  
  # Incidencia por DESTINO
  agg_incid_depto <- reactive({
    df <- base_b2()
    casos <- df %>% dplyr::group_by(COD_DPTO2) %>% dplyr::summarise(casos=dplyr::n(), .groups="drop")
    pop   <- pob_depto %>% dplyr::filter(ano==input$f_anio_b2)
    dplyr::left_join(casos, pop, by="COD_DPTO2") %>% dplyr::mutate(incid=ifelse(POB>0,(casos/POB)*1e5,NA_real_))
  })
  agg_incid_mpio <- reactive({
    df <- base_b2()
    casos <- df %>% dplyr::group_by(COD_DPTO2, COD_MUN5, MUN_D) %>% dplyr::summarise(casos=dplyr::n(), .groups="drop")
    pop   <- pob_mpio %>% dplyr::filter(ano==input$f_anio_b2) %>% dplyr::select(COD_MUN5, POB)
    dplyr::left_join(casos, pop, by="COD_MUN5") %>% dplyr::mutate(incid=ifelse(POB>0,(casos/POB)*1e5,NA_real_))
  })
  
  # Incidencia por ORIGEN (departamentos)
  agg_incid_origen_depto <- reactive({
    req(input$f_anio_b2)
    casos <- bpan %>% dplyr::filter(ano==input$f_anio_b2, !is.na(DEP_O), DEP_O!="") %>%
      dplyr::mutate(DEP_O=trimws(DEP_O)) %>%
      dplyr::group_by(DEP_O) %>% dplyr::summarise(casos=n(), .groups="drop")
    casos %>% dplyr::left_join(dpt_name_code, by="DEP_O") %>%
      dplyr::left_join(pob_depto %>% dplyr::filter(ano==input$f_anio_b2), by="COD_DPTO2") %>%
      dplyr::filter(!is.na(POB), POB>0) %>%
      dplyr::mutate(incid=(casos/POB)*1e5, nombre=DEP_O) %>%
      dplyr::select(nombre, incid, casos, POB) %>%
      dplyr::arrange(dplyr::desc(incid))
  })
  
  # Q1: Mapa incidencia
  output$map_bpan_incid <- renderLeaflet({
    leaflet::leaflet() %>% leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% leaflet::setView(lng=-74.3, lat=4.6, zoom=5)
  })
  observe({
    titulo <- "Incidencia (x100.000 hab.)"
    fmt <- function(x) number(x, big.mark=",", accuracy=0.1)
    if (nivel_b2()=="deptos") {
      dd <- agg_incid_depto()
      shp <- dptos_sf %>% dplyr::left_join(dd, by="COD_DPTO2") %>% dplyr::left_join(dpt_lookup_bpan, by="COD_DPTO2") %>%
        dplyr::mutate(incid=coalesce(incid,0), etq=paste0("<b>", DEP_D, "</b><br>", titulo, ": ", fmt(incid)))
      pal <- make_pal(shp$incid, "PuBuGn")
      leaflet::leafletProxy("map_bpan_incid", data=shp) %>% leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId=~COD_DPTO2, fillColor=~pal(incid), color="#666", weight=0.7, fillOpacity=0.9,
                             label=~lapply(etq, htmltools::HTML),
                             highlightOptions=leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        leaflet::addLegend("bottomright", pal=pal, values=~incid, title=titulo)
    } else {
      sel_dep <- dpt_lookup_bpan %>% dplyr::filter(DEP_D==input$f_depto_b2) %>% dplyr::pull(COD_DPTO2) %>% .[1]
      req(!is.na(sel_dep), nzchar(sel_dep))
      dd <- agg_incid_mpio()
      shp <- mpios_sf %>% dplyr::filter(COD_DPTO2==sel_dep) %>% dplyr::left_join(dd %>% dplyr::select(COD_MUN5, MUN_D, incid), by="COD_MUN5") %>%
        dplyr::mutate(MUN_D=ifelse(is.na(MUN_D), MUNICIPIO_N, MUN_D), incid=coalesce(incid,0),
                      etq=paste0("<b>", MUN_D, "</b><br>", titulo, ": ", fmt(incid)))
      pal <- make_pal(shp$incid, "PuRd"); bb <- sf::st_bbox(shp)
      leaflet::leafletProxy("map_bpan_incid", data=shp) %>% leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId=~COD_MUN5, fillColor=~pal(incid), color="#666", weight=0.4, fillOpacity=0.9,
                             label=~lapply(etq, htmltools::HTML),
                             highlightOptions=leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        leaflet::addLegend("bottomright", pal=pal, values=~incid, title=titulo) %>%
        leaflet::fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  observeEvent(input$map_bpan_incid_shape_click, {
    if (nivel_b2()=="deptos") {
      click <- input$map_bpan_incid_shape_click; req(click$id)
      cod <- sprintf("%02d", as.integer(click$id))
      nom <- dpt_lookup_bpan$DEP_D[dpt_lookup_bpan$COD_DPTO2==cod][1]
      if (!is.na(nom) && nzchar(nom)) updateSelectInput(session, "f_depto_b2", selected=nom)
    }
  }, ignoreInit = TRUE)
  
  output$top_incid_dest_top20 <- renderPlotly({
    df <- base_b2() %>%
      group_by(COD_DPTO2, DEP_D) %>%
      summarise(casos = n(), .groups = "drop") %>%
      left_join(pob_depto %>% filter(ano == input$f_anio_b2), by = "COD_DPTO2") %>%
      mutate(
        incid  = ifelse(POB > 0, (casos / POB) * 1e5, NA_real_),
        nombre = ifelse(!is.na(DEP_D) & nzchar(DEP_D), DEP_D, COD_DPTO2)
      ) %>%
      filter(is.finite(incid)) %>%
      arrange(desc(incid)) %>%
      slice_head(n = 20)
    
    validate(need(nrow(df) > 0, "Sin datos para los filtros seleccionados."))
    
    df2 <- df %>% arrange(incid)
    cd  <- cbind(df2$casos, df2$POB)
    
    plot_ly(
      data = df2, x = ~incid, y = ~factor(nombre, levels = df2$nombre),
      type = "bar", orientation = "h", customdata = cd,
      hovertemplate = "%{y}<br>Incidencia: %{x:.2f}<br>Casos: %{customdata[0]:,}<br>Población: %{customdata[1]:,}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Incidencia por 100.000 hab."),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 10, r = 10, b = 10, t = 10),
        showlegend = FALSE
      )
  })
  
  output$edad_materna_barras <- renderPlotly({
    df <- base_b2() %>%
      filter(!is.na(edad)) %>%
      mutate(
        edad = suppressWarnings(as.numeric(edad)),
        grupo_edad = case_when(
          edad < 10              ~ "<10",
          edad >=10 & edad <=14  ~ "10–14",
          edad >=15 & edad <=19  ~ "15–19",
          edad >=20 & edad <=24  ~ "20–24",
          edad >=25 & edad <=29  ~ "25–29",
          edad >=30 & edad <=34  ~ "30–34",
          edad >=35 & edad <=39  ~ "35–39",
          edad >=40 & edad <=44  ~ "40–44",
          edad >=45 & edad <=49  ~ "45–49",
          edad >=50              ~ "50+",
          TRUE                   ~ "Sin dato"
        )
      ) %>%
      group_by(grupo_edad) %>%
      summarise(Casos = n(), .groups = "drop")
    
    validate(need(nrow(df) > 0, "No hay datos para los filtros seleccionados."))
    
    orden_niveles <- intersect(
      c("<10","10–14","15–19","20–24","25–29",
        "30–34","35–39","40–44","45–49","50+","Sin dato"),
      unique(df$grupo_edad)
    )
    
    df <- df %>% mutate(grupo_edad = factor(grupo_edad, levels = orden_niveles))
    
    plot_ly(
      data = df, x = ~Casos, y = ~grupo_edad,
      type = "bar", orientation = "h",
      hovertemplate = "Grupo de edad: %{y}<br>Casos: %{x}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Número de casos"),
        yaxis = list(title = "Edad de la madre (años)"),
        margin = list(l = 80, r = 10, b = 40, t = 10),
        showlegend = FALSE
      )
  })
  
  # ================= TAB 3 — Perfil y calidad (3 cuadrantes) =================
  # --- Filtro de Año ---
  output$anio_b3_ui <- renderUI({
    yrs <- sort(unique(bpan$ano))
    selectInput(
      "f_anio_b3", "",
      choices  = yrs,
      selected = max(yrs, na.rm = TRUE)
    )
  })
  
  # --- Filtros dinámicos ORIGEN ---
  output$dep_o_b3_ui <- renderUI({
    deps <- sort(unique(na.omit(bpan$DEP_O)))
    selectInput("f_dep_o_b3", "",
                choices = c("Todos", deps), selected = "Todos")
  })
  
  output$mun_o_b3_ui <- renderUI({
    req(input$f_dep_o_b3)
    if (input$f_dep_o_b3 == "Todos") {
      selectInput("f_mun_o_b3", "",
                  choices = "Todos", selected = "Todos")
    } else {
      mm <- bpan %>%
        dplyr::filter(DEP_O == input$f_dep_o_b3) %>%
        dplyr::distinct(MUN_O) %>% dplyr::arrange(MUN_O)
      selectInput("f_mun_o_b3", "",
                  choices = c("Todos", mm$MUN_O), selected = "Todos")
    }
  })
  
  # --- Filtros dinámicos DESTINO ---
  output$dep_d_b3_ui <- renderUI({
    deps <- sort(unique(na.omit(bpan$DEP_D)))
    selectInput("f_dep_d_b3", "",
                choices = c("Todos", deps), selected = "Todos")
  })
  
  output$mun_d_b3_ui <- renderUI({
    req(input$f_dep_d_b3)
    if (input$f_dep_d_b3 == "Todos") {
      selectInput("f_mun_d_b3", "",
                  choices = "Todos", selected = "Todos")
    } else {
      mm <- bpan %>%
        dplyr::filter(DEP_D == input$f_dep_d_b3) %>%
        dplyr::distinct(MUN_D) %>% dplyr::arrange(MUN_D)
      selectInput("f_mun_d_b3", "Municipio (DESTINO)",
                  choices = c("Todos", mm$MUN_D), selected = "Todos")
    }
  })
  
  # --- Base filtrada ---
  base_b3 <- reactive({
    req(input$f_anio_b3)
    df <- bpan %>% dplyr::filter(ano == input$f_anio_b3)
    
    if (!is.null(input$f_dep_o_b3) && input$f_dep_o_b3 != "Todos")
      df <- df %>% dplyr::filter(DEP_O == input$f_dep_o_b3)
    if (!is.null(input$f_mun_o_b3) && input$f_mun_o_b3 != "Todos")
      df <- df %>% dplyr::filter(MUN_O == input$f_mun_o_b3)
    if (!is.null(input$f_dep_d_b3) && input$f_dep_d_b3 != "Todos")
      df <- df %>% dplyr::filter(DEP_D == input$f_dep_d_b3)
    if (!is.null(input$f_mun_d_b3) && input$f_mun_d_b3 != "Todos")
      df <- df %>% dplyr::filter(MUN_D == input$f_mun_d_b3)
    
    df
  })
  
  # --- Q1: Serie temporal Vivos vs Muertes (ejes dobles) ---
  output$serie_anual_tab3 <- renderPlotly({
    df0 <- base_b3() %>%
      dplyr::filter(!is.na(PAC_HOS), PAC_HOS %in% c(1L, 2L))
    
    validate(need(nrow(df0) > 0, "Sin datos para los filtros seleccionados."))
    
    mes_col <- if ("mes" %in% names(df0)) "mes" else if ("MES" %in% names(df0)) "MES" else NA_character_
    if (!is.na(mes_col)) {
      df0 <- df0 %>% dplyr::mutate(
        MES = suppressWarnings(as.integer(.data[[mes_col]])),
        MES = dplyr::if_else(is.finite(MES) & MES >= 1 & MES <= 12, MES, 1L)
      )
    } else {
      df0 <- df0 %>% dplyr::mutate(MES = 1L)
    }
    
    dfm <- df0 %>%
      dplyr::mutate(
        ANO = suppressWarnings(as.integer(ano)),
        FECHA = as.Date(sprintf("%04d-%02d-01", ANO, MES))
      ) %>%
      dplyr::filter(!is.na(FECHA)) %>%
      dplyr::group_by(FECHA, PAC_HOS) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = PAC_HOS, values_from = n,
                         values_fill = 0, names_prefix = "cat_") %>%
      dplyr::transmute(FECHA, vivos = cat_1, muertes = cat_2) %>%
      dplyr::arrange(FECHA)
    
    plot_ly(dfm, x = ~FECHA) %>%
      add_lines(y = ~vivos, name = "Vivos", yaxis = "y",
                hovertemplate = "Fecha: %{x|%Y-%m}<br>Vivos: %{y:,}<extra></extra>") %>%
      add_lines(y = ~muertes, name = "Muertes", yaxis = "y2",
                hovertemplate = "Fecha: %{x|%Y-%m}<br>Muertes: %{y:,}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Año–Mes", tickformat = "%Y-%m"),
        yaxis = list(title = "Vivos"),
        yaxis2 = list(title = "Muertes", overlaying = "y", side = "right"),
        legend = list(orientation = "h", x = 0, y = 1.1),
        margin = list(l = 50, r = 50, b = 40, t = 10)
      )
  })
  
  # --- Q2: Barras por grupos de edad materna (SOLO FALLECIDOS) ---
  output$hist_edad_tab3 <- renderPlotly({
    df <- base_b3() %>%
      dplyr::filter(!is.na(edad), !is.na(PAC_HOS), PAC_HOS == 2L) %>%   # solo muertes
      dplyr::mutate(
        edad = suppressWarnings(as.numeric(edad)),
        grupo_edad = dplyr::case_when(
          edad < 10              ~ "<10",
          edad >=10 & edad <=14  ~ "10–14",
          edad >=15 & edad <=19  ~ "15–19",
          edad >=20 & edad <=24  ~ "20–24",
          edad >=25 & edad <=29  ~ "25–29",
          edad >=30 & edad <=34  ~ "30–34",
          edad >=35 & edad <=39  ~ "35–39",
          edad >=40 & edad <=44  ~ "40–44",
          edad >=45 & edad <=49  ~ "45–49",
          edad >=50              ~ "50+",
          TRUE                   ~ "Sin dato"
        )
      ) %>%
      dplyr::group_by(grupo_edad) %>%
      dplyr::summarise(Fallecimientos = dplyr::n(), .groups = "drop")
    
    validate(need(nrow(df) > 0, "No hay fallecidos con edad válida para los filtros seleccionados."))
    
    # Ordenar grupos de edad
    orden_niveles <- c("<10","10–14","15–19","20–24","25–29",
                       "30–34","35–39","40–44","45–49","50+","Sin dato")
    df <- df %>% dplyr::mutate(grupo_edad = factor(grupo_edad, levels = orden_niveles))
    
    plot_ly(
      data = df, x = ~Fallecimientos, y = ~grupo_edad,
      type = "bar", orientation = "h",
      hovertemplate = "Grupo de edad: %{y}<br>Fallecimientos: %{x}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Número de fallecimientos"),
        yaxis = list(title = "Edad de la madre (años)"),
        margin = list(l = 80, r = 10, b = 40, t = 10),
        showlegend = FALSE
      )
  })
  
  # --- Q3: Mapa Tasa de Supervivencia ---
  nivel_b3_map <- reactive({
    if (!is.null(input$f_dep_d_b3) && input$f_dep_d_b3 != "Todos") "mpios" else "deptos"
  })
  
  sel_cod_dep_b3 <- reactive({
    if (nivel_b3_map() == "mpios") {
      cod <- dptos_sf %>% dplyr::filter(DEPARTAMENTO_N == input$f_dep_d_b3) %>%
        dplyr::pull(COD_DPTO2) %>% .[1]
      if (is.na(cod) || !nzchar(cod)) {
        cod <- base_b3() %>% dplyr::distinct(COD_DPTO2) %>%
          dplyr::pull(COD_DPTO2) %>% .[1]
      }
      cod
    } else NA_character_
  })
  
  agg_superv_depto_b3 <- reactive({
    base_b3() %>%
      dplyr::filter(!is.na(PAC_HOS), PAC_HOS %in% c(1L,2L)) %>%
      dplyr::group_by(COD_DPTO2, DEP_D) %>%
      dplyr::summarise(vivos = sum(PAC_HOS == 1L),
                       muertes = sum(PAC_HOS == 2L),
                       n = vivos + muertes, .groups = "drop") %>%
      dplyr::mutate(tasa = dplyr::if_else(n > 0, vivos/n, NA_real_))
  })
  
  agg_superv_mpio_b3 <- reactive({
    base_b3() %>%
      dplyr::filter(!is.na(PAC_HOS), PAC_HOS %in% c(1L,2L)) %>%
      dplyr::group_by(COD_DPTO2, COD_MUN5, MUN_D) %>%
      dplyr::summarise(vivos = sum(PAC_HOS == 1L),
                       muertes = sum(PAC_HOS == 2L),
                       n = vivos + muertes, .groups = "drop") %>%
      dplyr::mutate(tasa = dplyr::if_else(n > 0, vivos/n, NA_real_))
  })
  
  output$map_superv_b3 <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::setView(lng = -74.3, lat = 4.6, zoom = 5)
  })
  
  observe({
    titulo <- "Tasa de supervivencia (Vivos / (Vivos+Muertes))"
    fmt_p  <- function(x) ifelse(is.na(x), "s/d", scales::percent(x, accuracy = 0.1))
    fmt_n  <- function(x) scales::comma(x)
    
    if (nivel_b3_map() == "deptos") {
      dd  <- agg_superv_depto_b3()
      lkp <- dpt_lookup_bpan %>% dplyr::rename(DEP_D_LKP = DEP_D)
      
      shp <- dptos_sf %>%
        dplyr::left_join(dd,  by = "COD_DPTO2") %>%
        dplyr::left_join(lkp, by = "COD_DPTO2") %>%
        dplyr::mutate(
          nombre  = dplyr::coalesce(DEP_D, DEP_D_LKP, DEPARTAMENTO_N),
          vivos   = tidyr::replace_na(vivos, 0L),
          muertes = tidyr::replace_na(muertes, 0L),
          n       = tidyr::replace_na(n, 0L),
          etq     = paste0("<b>", nombre, "</b><br>",
                           titulo, ": ", fmt_p(tasa), "<br>",
                           "Vivos: ", fmt_n(vivos), "<br>",
                           "Muertes: ", fmt_n(muertes))
        )
      bins <- c(0, 0.5, 0.7, 0.85, 0.95, 1.0)
      pal  <- leaflet::colorBin("YlGn", domain = shp$tasa, bins = bins, na.color = "#f0f0f0")
      
      leaflet::leafletProxy("map_superv_b3", data = shp) %>%
        leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId = ~COD_DPTO2, fillColor = ~pal(tasa),
                             color = "#666", weight = 0.7, fillOpacity = 0.9,
                             label = ~lapply(etq, htmltools::HTML),
                             highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)) %>%
        leaflet::addLegend("bottomright", pal = pal, values = ~tasa, title = titulo)
      
    } else {
      sel_cod <- sel_cod_dep_b3(); req(!is.na(sel_cod), nzchar(sel_cod))
      dd <- agg_superv_mpio_b3()
      shp <- mpios_sf %>%
        dplyr::filter(COD_DPTO2 == sel_cod) %>%
        dplyr::left_join(dd %>% dplyr::select(COD_MUN5, MUN_D, vivos, muertes, n, tasa),
                         by = "COD_MUN5") %>%
        dplyr::mutate(
          MUN_D = dplyr::coalesce(MUN_D, MUNICIPIO_N),
          vivos = tidyr::replace_na(vivos, 0L),
          muertes = tidyr::replace_na(muertes, 0L),
          n = tidyr::replace_na(n, 0L),
          etq = paste0("<b>", MUN_D, "</b><br>",
                       titulo, ": ", fmt_p(tasa), "<br>",
                       "Vivos: ", fmt_n(vivos), "<br>",
                       "Muertes: ", fmt_n(muertes))
        )
      bins <- c(0, 0.5, 0.7, 0.85, 0.95, 1.0)
      pal  <- leaflet::colorBin("YlGn", domain = shp$tasa, bins = bins, na.color = "#f0f0f0")
      bb   <- sf::st_bbox(shp)
      
      leaflet::leafletProxy("map_superv_b3", data = shp) %>%
        leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId = ~COD_MUN5, fillColor = ~pal(tasa),
                             color = "#666", weight = 0.4, fillOpacity = 0.9,
                             label = ~lapply(etq, htmltools::HTML),
                             highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)) %>%
        leaflet::addLegend("bottomright", pal = pal, values = ~tasa, title = titulo) %>%
        leaflet::fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  
  # --- Click en depto -> zoom municipios ---
  observeEvent(input$map_superv_b3_shape_click, {
    if (nivel_b3_map() == "deptos") {
      click <- input$map_superv_b3_shape_click; req(click$id)
      cod <- sprintf("%02d", as.integer(click$id))
      nom <- dptos_sf$DEPARTAMENTO_N[match(cod, dptos_sf$COD_DPTO2)]
      deps_disp <- c("Todos", sort(unique(na.omit(bpan$DEP_D))))
      if (!is.na(nom) && nzchar(nom) && (nom %in% deps_disp)) {
        updateSelectInput(session, "f_dep_d_b3", selected = nom)
      }
    }
  }, ignoreInit = TRUE)
  
}

shinyApp(ui, server)

