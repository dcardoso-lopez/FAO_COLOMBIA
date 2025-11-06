# app_nda.R
# =========================================================
# NDA — Dashboard (app exclusiva)
# - Tab 1: Exploración (mapa, top-10, destinos por depto, top municipios destino)
# - Tab 2: Análisis poblacional (incidencia, edades, sexo)
# - Tab 3: Defunciones y Supervivencia
# =========================================================

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(leaflet); library(sf); library(dplyr); library(tidyr)
  library(scales); library(htmltools); library(DT); library(plotly)
  library(stringi)
})

options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)
options(shiny.maxRequestSize = 100*1024^2)

# ---------- Colores globales ----------
MAP_COLORS <- c("#ffe0cc", "#fa8916", "#fa8916", "#e6550d", "#9c4a00")

BAR_COLOR  <- "#f57c00"
BORDER_UI  <- "#ffb366"
SEX_COLORS <- c("Hombres"="#f57c00", "Mujeres"="#0a83ff", "Sin dato"="#cbd5e1")

# ---------- Utils ----------
norm_txt <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
NUP      <- function(x) toupper(norm_txt(x))

# ---------- Rutas ----------
local_data_dir <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/NDA_APP/data"
app_root     <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
rel_data_dir <- file.path(app_root, "data")
data_dir <- if (dir.exists(rel_data_dir)) rel_data_dir else local_data_dir

nda_path       <- file.path(data_dir, "023_INS_SIVIGILA-NDA.rds")
ruta_pob       <- file.path(data_dir, "051_DANE_Proyecciones_P_total.rds")
ruta_shp_mpios <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")
ruta_shp_dptos <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")

must_exist <- c(nda_path, ruta_pob, ruta_shp_mpios, ruta_shp_dptos)
miss <- must_exist[!file.exists(must_exist)]
if (length(miss)) stop("Faltan archivos. data_dir usado: ", data_dir, "\n", paste("-", miss, collapse = "\n"))
check_shp_parts <- function(shp){ base <- sub("\\.shp$", "", shp); req <- paste0(base, c(".shp",".dbf",".shx",".prj")); req[!file.exists(req)] }
miss_shp <- c(check_shp_parts(ruta_shp_mpios), check_shp_parts(ruta_shp_dptos))
if (length(miss_shp)) stop("Faltan componentes de shapefile:\n", paste("-", miss_shp, collapse = "\n"))

# ---------- 1) Leer NDA (casos = filas) ----------
nda_raw <- readRDS(nda_path) %>% dplyr::filter(!is.na(MUNICIPIO_D))

get_col <- function(df, opts, stop_msg){
  nm <- opts[opts %in% names(df)][1]
  if (is.na(nm) || !nzchar(nm)) stop(stop_msg) else nm
}

n_month_col     <- get_col(nda_raw, c("mes","MES"), "NDA: no 'mes'/'MES'")
n_year_col      <- get_col(nda_raw, c("ano","ANO"), "NDA: no 'ano'/'ANO'")
n_mun_code_col  <- get_col(nda_raw, c("COD_DANE_MUNIC_D","COD_MUN5","COD_MPIO","MPIO_CDPMP"), "NDA: no código municipal destino")
n_dep_name_col  <- get_col(nda_raw, c("DEPARTAMENTO_D","DEPARTMENTO_D"), "NDA: no 'DEPARTAMENTO_D'")
n_mun_name_col  <- get_col(nda_raw, c("MUNICIPIO_D","NOMBRE_MPIO"), "NDA: no 'MUNICIPIO_D'")
n_dep_origen_col<- get_col(nda_raw, c("DEPARTAMENTO_O"), "NDA: no 'DEPARTAMENTO_O'")
n_mun_origen_col<- get_col(nda_raw, c("MUNICIPIO_O"), "NDA: no 'MUNICIPIO_O'")

# >>> Normalización de sexo a Hombres/Mujeres <<<
normalize_sex <- function(x){
  x <- trimws(toupper(as.character(x)))
  dplyr::case_when(
    x %in% c("M","MASCULINO","HOMBRE","HOMBRES","1") ~ "Hombres",
    x %in% c("F","FEMENINO","MUJER","MUJERES","2")   ~ "Mujeres",
    TRUE                                            ~ NA_character_
  )
}

nda <- nda_raw %>%
  dplyr::transmute(
    mes       = suppressWarnings(as.integer(.data[[n_month_col]])),
    ano       = suppressWarnings(as.integer(.data[[n_year_col]])),
    COD_MUN5  = sprintf("%05d", suppressWarnings(as.integer(.data[[n_mun_code_col]]))),
    COD_DPTO2 = substr(COD_MUN5, 1, 2),
    DEP_D     = trimws(as.character(.data[[n_dep_name_col]])),
    MUN_D     = trimws(as.character(.data[[n_mun_name_col]])),
    DEP_O     = trimws(as.character(.data[[n_dep_origen_col]])),
    MUN_O     = trimws(as.character(.data[[n_mun_origen_col]])),
    edad_ni   = suppressWarnings(as.numeric(edad)),
    sexo_ni   = normalize_sex(sexo),
    confirmados = suppressWarnings(as.integer(confirmados)),
    PAC_HOS     = suppressWarnings(as.integer(pac_hos)),
    tip_cas     = trimws(as.character(tip_cas))
  ) %>%
  dplyr::filter(!is.na(ano), !is.na(COD_MUN5), !is.na(COD_DPTO2))

# Solo filas con ORIGEN válido (para Tab 1)
nda_valid <- nda %>% dplyr::filter(!is.na(DEP_O), DEP_O != "", !is.na(MUN_O), MUN_O != "")

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
dpt_lookup_nda <- nda %>% dplyr::select(COD_DPTO2, DEP_D) %>%
  dplyr::mutate(COD_DPTO2=sprintf("%02d",as.integer(COD_DPTO2)), DEP_D=trimws(DEP_D)) %>%
  dplyr::distinct() %>% dplyr::arrange(DEP_D)

mun_lookup_nda <- nda %>% dplyr::select(COD_DPTO2, COD_MUN5, MUN_D) %>%
  dplyr::mutate(COD_DPTO2=sprintf("%02d",as.integer(COD_DPTO2)), COD_MUN5=sprintf("%05d",as.integer(COD_MUN5)), MUN_D=trimws(MUN_D)) %>%
  dplyr::distinct()

# ---------- Helper paleta ----------
make_pal <- function(values, palette = MAP_COLORS) {
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
  # ===== CSS: bordes y controles en #ffb366 =====
  tags$head(
    tags$style(HTML(sprintf("
      :root{ --border-col:%s; }
      .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
      h3{font-weight:700;letter-spacing:.2px;margin-bottom:8px}
      .data-note{font-size:13px;color:#6b7280;margin:0 0 16px}
      .filters{
        background:#fff;border:1.5px solid var(--border-col);
        border-radius:16px;padding:14px 16px;margin-bottom:16px;
        box-shadow:0 2px 10px rgba(0,0,0,.04)
      }
      .filters-grid{display:grid;grid-template-columns:repeat(6,minmax(180px,1fr));gap:12px}
      .filter-label{font-size:12px;font-weight:600;letter-spacing:.4px;text-transform:uppercase;color:#6b7280;margin-bottom:6px}
      .selectize-input,.form-control{
        min-height:42px;border-radius:10px;border:1.5px solid var(--border-col);
        box-shadow:none !important;
      }
      .selectize-input:focus,.form-control:focus{
        border-color:var(--border-col)!important; outline:0 !important;
        box-shadow:0 0 0 .15rem rgba(255,179,102,.35)!important;
      }
      .card{
        background:#fff;border:1.5px solid var(--border-col);
        border-radius:16px;padding:12px;box-shadow:0 2px 10px rgba(0,0,0,.05);margin-bottom:12px
      }
      .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}
      .info-badge{display:inline-flex;justify-content:center;align-items:center;width:18px;height:18px;margin-left:6px;border-radius:50%%;font-size:12px;line-height:1;font-weight:700;background:#e5e7eb;color:#111827;cursor:help;border:1px solid #d1d5db;}
      .nav-tabs .nav-link.active{border-color:var(--border-col) var(--border-col) #fff !important}
      .nav-tabs{border-bottom:1.5px solid var(--border-col)}
    ", BORDER_UI))),
    # Tooltips bootstrap
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
      h3("SIVIGILA — NDA (Niños con Desnutrición Aguda)"),
      div(class="data-note", HTML("Nota: Las cifras recientes pueden ser <b>preliminares</b>.")),
      tabsetPanel(
        id = "tabs_nda",
        type = "tabs",
        
        # ---------------- TAB 1: Exploración ----------------
        tabPanel(
          title = "Exploración NDA",
          br(),
          div(class="filters",
              div(class="filters-grid",
                  div(class="filter", div(class="filter-label","Año"), uiOutput("anio_nda_ui")),
                  div(class="filter", div(class="filter-label","Departamento (ORIGEN)"),
                      selectInput("f_depto_nda", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Municipio (ORIGEN)"),
                      selectInput("f_mpio_nda", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Departamento (DESTINO)"),
                      selectInput("f_depto_dest", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Municipio (DESTINO)"),
                      selectInput("f_mpio_dest", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Sexo del niño/a"),
                      selectInput("f_sexo_tab1", NULL, choices = c("Todos","Hombres","Mujeres"), selected = "Todos")),
                  div(class="filter", div(class="filter-label","Acción"),
                      actionLink("btn_reset_nda","← Limpiar filtros"))
              )
          ),
          fluidRow(
            column(
              width = 6,
              div(class="card",
                  div(class="card-title", textOutput("ttl_map_tab1")),
                  leafletOutput("map_nda", height = 700)
              )
            ),
            column(
              width = 6,
              div(class="card",
                  div(class="card-title", textOutput("ttl_top10_tab1")),
                  plotlyOutput("bar_nda", height = 310)
              ),
              div(
                class = "card",
                div(
                  class = "card-title",
                  textOutput("ttl_top_mpios_dest")
                ),
                plotlyOutput("destinos_mpio_top", height = 310)
              )
            )
          )
        ),
        
        # ---------------- TAB 2: Análisis poblacional ----------------
        tabPanel(
          title = "Análisis poblacional",
          br(),
          div(class="filters",
              div(class="filters-grid",
                  div(class="filter", div(class="filter-label","Año"), uiOutput("anio_b2_ui")),
                  div(class="filter", div(class="filter-label","Departamento (para ver municipios)"),
                      selectInput("f_depto_b2", NULL, choices = c("Todos"), selected = "Todos")),
                  div(class="filter", div(class="filter-label","Municipio"),
                      selectInput("f_mpio_b2", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Sexo del niño/a"),
                      selectInput("f_sexo_tab2", NULL, choices = c("Todos","Hombres","Mujeres"), selected = "Todos"))
              )
          ),
          fluidRow(
            column(6,
                   div(class="card",
                       div(class="card-title", textOutput("ttl_map_incid_b2")),
                       leafletOutput("map_nda_incid", height = 360)
                   ),
                   div(class="card",
                       div(class="card-title", textOutput("ttl_edad_b2")),
                       plotlyOutput("edad_nna_barras", height = 300)
                   ),
                   div(class="card",
                       div(class="card-title", textOutput("ttl_sexo_b2")),
                       plotlyOutput("sexo_nna_barras", height = 260)
                   )
            ),
            column(6,
                   div(
                     class = "card",
                     div(class = "card-title", textOutput("ttl_top_incid_b2")),
                     plotlyOutput("top_incid_dest_top20", height = 920)
                   )
            )
          )
        ),
        
        # ---------------- TAB 3: Defunciones y Supervivencia ----------------
        tabPanel(
          title = "Defunciones y Supervivencia",
          br(),
          div(class="filters",
              div(class="filters-grid",
                  div(class="filter", div(class="filter-label","Año"),                 uiOutput("anio_b3_ui")),
                  div(class="filter", div(class="filter-label","Depto (ORIGEN)"),      uiOutput("dep_o_b3_ui")),
                  div(class="filter", div(class="filter-label","Municipio (ORIGEN)"),  uiOutput("mun_o_b3_ui")),
                  div(class="filter", div(class="filter-label","Depto (DESTINO)"),     uiOutput("dep_d_b3_ui")),
                  div(class="filter", div(class="filter-label","Municipio (DESTINO)"), uiOutput("mun_d_b3_ui")),
                  div(class="filter", div(class="filter-label","Sexo del niño/a"),
                      uiOutput("sexo_b3_ui"))
              )
          ),
          fluidRow(
            column(
              width = 6,
              div(class="card",
                  div(class="card-title", textOutput("ttl_serie_b3")),
                  plotlyOutput("serie_anual_tab3", height = 320)
              ),
              div(class="card",
                  div(class="card-title", textOutput("ttl_hist_b3")),
                  plotlyOutput("hist_edad_tab3", height = 320)
              )
            ),
            column(
              width = 6,
              div(class="card",
                  div(
                    class="card-title",
                    textOutput("ttl_map_superv_b3")
                  ),
                  leafletOutput("map_superv_b3", height = 700)
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
  
  # placeholder para gráficos vacíos
  empty_plot <- function(txt = "Sin datos para los filtros seleccionados.") {
    plotly::plotly_empty(type = "scatter", mode = "markers") %>%
      plotly::layout(
        annotations = list(x = 0.5, y = 0.5, text = as.character(txt),
                           showarrow = FALSE, xref = "paper", yref = "paper",
                           font = list(size = 14)),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        margin = list(l = 10, r = 10, b = 10, t = 10)
      )
  }
  
  # ---------- Storytelling helpers ----------
  scope_origen <- reactive({
    if (!is.null(input$f_mpio_nda) && input$f_mpio_nda != "Todos") paste(input$f_mpio_nda, ",", input$f_depto_nda)
    else if (!is.null(input$f_depto_nda) && input$f_depto_nda != "Todos") paste("de", input$f_depto_nda)
    else "en Colombia"
  })
  scope_dest <- reactive({
    if (!is.null(input$f_mpio_dest) && input$f_mpio_dest != "Todos") paste(input$f_mpio_dest, ",", input$f_depto_dest)
    else if (!is.null(input$f_depto_dest) && input$f_depto_dest != "Todos") input$f_depto_dest
    else "Colombia"
  })
  scope_b2 <- reactive({
    if (!is.null(input$f_mpio_b2) && input$f_mpio_b2 != "Todos") paste(input$f_mpio_b2, ",", input$f_depto_b2)
    else if (!is.null(input$f_depto_b2) && input$f_depto_b2 != "Todos") input$f_depto_b2
    else "Colombia"
  })
  scope_b3 <- reactive({
    d <- if (!is.null(input$f_dep_d_b3) && input$f_dep_d_b3 != "Todos") input$f_dep_d_b3 else "Colombia"
    o <- if (!is.null(input$f_dep_o_b3) && input$f_dep_o_b3 != "Todos") paste0(" (origen: ", input$f_dep_o_b3, ")") else ""
    paste0(d, o)
  })
  
  # ================= TAB 1 — Exploración =================
  output$anio_nda_ui <- renderUI({
    yrs <- sort(unique(nda_valid$ano))
    selectInput("f_anio_nda", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  observeEvent(input$btn_reset_nda, {
    yrs <- sort(unique(nda_valid$ano))
    updateSelectInput(session, "f_anio_nda",  selected = max(yrs, na.rm = TRUE))
    updateSelectInput(session, "f_depto_nda", choices = "Todos", selected = "Todos")
    updateSelectInput(session, "f_mpio_nda",  choices = "Todos", selected = "Todos")
    updateSelectInput(session, "f_depto_dest", choices = "Todos", selected = "Todos")
    updateSelectInput(session, "f_mpio_dest",  choices = "Todos", selected = "Todos")
    updateSelectInput(session, "f_sexo_tab1",  selected = "Todos")
  })
  
  nda_base_no_dest <- reactive({
    req(input$f_anio_nda)
    df <- nda_valid %>% dplyr::filter(ano == input$f_anio_nda)
    if (!is.null(input$f_depto_nda) && input$f_depto_nda != "Todos") df <- df %>% dplyr::filter(DEP_O == input$f_depto_nda)
    if (!is.null(input$f_mpio_nda)  && input$f_mpio_nda  != "Todos") df <- df %>% dplyr::filter(MUN_O == input$f_mpio_nda)
    if (!is.null(input$f_sexo_tab1) && input$f_sexo_tab1 != "Todos") df <- df %>% dplyr::filter(sexo_ni == input$f_sexo_tab1)
    df
  })
  nda_base <- reactive({
    df <- nda_base_no_dest()
    if (!is.null(input$f_depto_dest) && input$f_depto_dest != "Todos") df <- df %>% dplyr::filter(DEP_D == input$f_depto_dest)
    if (!is.null(input$f_mpio_dest)  && input$f_mpio_dest  != "Todos") df <- df %>% dplyr::filter(MUN_D == input$f_mpio_dest)
    df
  })
  
  # Combos dinámicos
  observeEvent(input$f_anio_nda, {
    df_year <- nda_valid %>% dplyr::filter(ano == input$f_anio_nda)
    deps_o  <- df_year %>% dplyr::distinct(DEP_O) %>% dplyr::arrange(DEP_O) %>% dplyr::pull(DEP_O)
    sel_dep <- if (!is.null(input$f_depto_nda) && input$f_depto_nda %in% deps_o) input$f_depto_nda else "Todos"
    updateSelectInput(session, "f_depto_nda", choices = c("Todos", deps_o), selected = sel_dep)
    updateSelectInput(session, "f_mpio_nda",  choices = "Todos", selected = "Todos")
  }, ignoreInit = FALSE)
  
  observeEvent(input$f_depto_nda, {
    df <- nda_valid %>% dplyr::filter(ano == input$f_anio_nda)
    if (!is.null(input$f_depto_nda) && input$f_depto_nda != "Todos") {
      mpios_o <- df %>% dplyr::filter(DEP_O == input$f_depto_nda) %>% dplyr::distinct(MUN_O) %>% dplyr::arrange(MUN_O) %>% dplyr::pull(MUN_O)
      updateSelectInput(session, "f_mpio_nda", choices = c("Todos", mpios_o), selected = "Todos")
    } else {
      updateSelectInput(session, "f_mpio_nda", choices = "Todos", selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$f_anio_nda, input$f_depto_nda, input$f_mpio_nda, input$f_sexo_tab1), {
    df <- nda_base_no_dest()
    deps_d <- df %>% dplyr::distinct(DEP_D) %>% dplyr::arrange(DEP_D) %>% dplyr::pull(DEP_D)
    sel_dep_d <- if (!is.null(input$f_depto_dest) && input$f_depto_dest %in% deps_d) input$f_depto_dest else "Todos"
    updateSelectInput(session, "f_depto_dest", choices = c("Todos", deps_d), selected = sel_dep_d)
    updateSelectInput(session, "f_mpio_dest",  choices = "Todos", selected = "Todos")
  }, ignoreInit = FALSE)
  
  observeEvent(input$f_depto_dest, {
    df <- nda_base_no_dest()
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
      if (is.na(cod) || !nzchar(cod)) cod <- nda_base() %>% dplyr::distinct(COD_DPTO2) %>% dplyr::pull(COD_DPTO2) %>% .[1]
      cod
    } else NA_character_
  })
  
  nda_agg_depto <- reactive({
    nda_base() %>% dplyr::group_by(COD_DPTO2, DEP_D) %>% dplyr::summarise(valor=dplyr::n(), .groups="drop")
  })
  nda_agg_mpio  <- reactive({
    nda_base() %>% dplyr::group_by(COD_DPTO2, COD_MUN5, MUN_D) %>% dplyr::summarise(valor=dplyr::n(), .groups="drop")
  })
  
  # ----- Títulos storytelling TAB 1
  output$ttl_map_tab1 <- renderText({
    nv <- if (map_nivel()=="deptos") "departamental" else "municipal"
    paste0("¿Dónde está afectando más la desnutrición infantil a nivel ", nv,
           " (DESTINO) ", "en ", input$f_anio_nda, "?")
  })
  output$ttl_top10_tab1 <- renderText({
    paste0("¿Cuáles son los 10 ", if (map_nivel()=="deptos") "departamentos" else "municipios",
           " con más casos de NDA (DESTINO) ", scope_origen(), " — ", input$f_anio_nda, "?")
  })
  output$ttl_top_mpios_dest <- renderText({
    paste0("Principales municipios DESTINO desde ", scope_origen(),
           " (excluye el mismo departamento) — ", input$f_anio_nda)
  })
  
  # Mapa (casos)
  output$map_nda <- renderLeaflet({
    leaflet::leaflet() %>% leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% leaflet::setView(lng=-74.3, lat=4.6, zoom=5)
  })
  observe({
    titulo <- "Casos (filas)"; fmt_val <- function(x) scales::comma(x)
    if (map_nivel()=="deptos") {
      shp <- dptos_sf %>% dplyr::left_join(nda_agg_depto(), by="COD_DPTO2") %>%
        dplyr::mutate(valor=tidyr::replace_na(valor,0), etq=paste0("<b>", DEPARTAMENTO_N, "</b><br>", titulo, ": ", fmt_val(valor)))
      pal <- make_pal(shp$valor, MAP_COLORS)
      leaflet::leafletProxy("map_nda", data=shp) %>% leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId=~COD_DPTO2, fillColor=~pal(valor), color=BORDER_UI, weight=0.7, fillOpacity=0.9,
                             label=~lapply(etq, htmltools::HTML),
                             highlightOptions=leaflet::highlightOptions(color=BORDER_UI, weight=2, bringToFront=TRUE)) %>%
        leaflet::addLegend(position="bottomright", pal=pal, values=~valor, title=titulo)
    } else {
      sel_cod <- sel_cod_dep(); req(!is.na(sel_cod), nzchar(sel_cod))
      shp <- mpios_sf %>% dplyr::filter(COD_DPTO2==sel_cod) %>% dplyr::left_join(nda_agg_mpio(), by=c("COD_DPTO2","COD_MUN5")) %>%
        dplyr::mutate(valor=tidyr::replace_na(valor,0), etq=paste0("<b>", MUNICIPIO_N, "</b><br>", titulo, ": ", fmt_val(valor)))
      pal <- make_pal(shp$valor, MAP_COLORS); bb  <- sf::st_bbox(shp)
      leaflet::leafletProxy("map_nda", data=shp) %>% leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId=~COD_MUN5, fillColor=~pal(valor), color=BORDER_UI, weight=0.4, fillOpacity=0.9,
                             label=~lapply(etq, htmltools::HTML),
                             highlightOptions=leaflet::highlightOptions(color=BORDER_UI, weight=2, bringToFront=TRUE)) %>%
        leaflet::addLegend(position="bottomright", pal=pal, values=~valor, title=titulo) %>%
        leaflet::fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  
  observeEvent(input$map_nda_shape_click, {
    click <- input$map_nda_shape_click; req(click$id)
    if (map_nivel()=="deptos") {
      cod <- sprintf("%02d", as.integer(click$id))
      nom_shape <- dptos_sf$DEPARTAMENTO_N[match(cod, dptos_sf$COD_DPTO2)]
      deps_year <- nda_base_no_dest() %>% dplyr::distinct(DEP_D) %>% dplyr::pull(DEP_D)
      if (!is.na(nom_shape) && nzchar(nom_shape) && nom_shape %in% deps_year) updateSelectInput(session, "f_depto_dest", selected = nom_shape)
    } else {
      codm <- sprintf("%05d", as.integer(click$id))
      nom_mpio <- mpios_sf$MUNICIPIO_N[match(codm, mpios_sf$COD_MUN5)]
      if (!is.null(input$f_depto_dest) && input$f_depto_dest != "Todos") {
        mpios_year_dep <- nda_base_no_dest() %>% dplyr::filter(DEP_D == input$f_depto_dest) %>% dplyr::distinct(MUN_D) %>% dplyr::pull(MUN_D)
        if (!is.na(nom_mpio) && nzchar(nom_mpio) && nom_mpio %in% mpios_year_dep) updateSelectInput(session, "f_mpio_dest", selected = nom_mpio)
      }
    }
  }, ignoreInit = TRUE)
  
  # Top-10 casos (según nivel)
  top10_df <- reactive({
    if (map_nivel() == "deptos") {
      nda_agg_depto() %>% dplyr::mutate(nombre = ifelse(!is.na(DEP_D) & nzchar(DEP_D), DEP_D, COD_DPTO2)) %>% dplyr::select(nombre, valor) %>%
        dplyr::arrange(dplyr::desc(valor)) %>% dplyr::slice_head(n = 10)
    } else {
      nda_agg_mpio() %>% dplyr::filter(COD_DPTO2 == sel_cod_dep()) %>% dplyr::mutate(nombre = ifelse(!is.na(MUN_D) & nzchar(MUN_D), MUN_D, COD_MUN5)) %>%
        dplyr::select(nombre, valor) %>% dplyr::arrange(dplyr::desc(valor)) %>% dplyr::slice_head(n = 10)
    }
  })
  output$bar_nda <- renderPlotly({
    df <- top10_df()
    if (is.null(df) || nrow(df) == 0) return(empty_plot("No hay datos para el Top-10 con los filtros actuales."))
    plot_ly(data = df %>% dplyr::arrange(valor),
            x = ~valor, y = ~factor(nombre, levels = df$nombre[order(df$valor)]),
            type = "bar", orientation = "h",
            marker = list(color = BAR_COLOR),
            hovertemplate = "%{y}<br>Casos: %{x:,}<extra></extra>") %>%
      layout(xaxis = list(title = "Casos", showgrid = FALSE, zeroline = FALSE),
             yaxis = list(title = "", automargin = TRUE),
             margin = list(l = 10, r = 10, t = 10, b = 10))
  })
  
  # ======= Destinos desde el ORIGEN (por DEPARTAMENTO) =======
  destinos_origen_df <- reactive({
    req(input$f_anio_nda)
    origen_dep <- input$f_depto_nda %||% "Todos"
    origen_mun <- input$f_mpio_nda  %||% "Todos"
    validate(need(origen_dep != "Todos",
                  "Selecciona un Departamento (ORIGEN) para ver los destinos."))
    
    df <- nda %>% dplyr::filter(ano == input$f_anio_nda)
    if (!is.null(input$f_sexo_tab1) && input$f_sexo_tab1 != "Todos")
      df <- df %>% dplyr::filter(sexo_ni == input$f_sexo_tab1)
    
    df <- df %>% dplyr::filter(NUP(DEP_O) == NUP(origen_dep))
    if (!is.null(origen_mun) && origen_mun != "Todos")
      df <- df %>% dplyr::filter(NUP(MUN_O) == NUP(origen_mun))
    
    out <- df %>%
      dplyr::count(DEP_D, name = "casos") %>%
      dplyr::mutate(destino = ifelse(!is.na(DEP_D) & nzchar(DEP_D), DEP_D, "(s/d)")) %>%
      dplyr::select(destino, casos) %>%
      dplyr::arrange(dplyr::desc(casos)) %>%
      dplyr::filter(NUP(destino) != NUP(origen_dep))
    out
  })
  
  output$destinos_desde_origen <- renderPlotly({
    df <- destinos_origen_df()
    if (is.null(df) || nrow(df) == 0) {
      return(empty_plot("No hay destinos con casos para el ORIGEN seleccionado."))
    }
    plot_ly(
      data = df %>% dplyr::mutate(destino=factor(destino,levels=rev(destino))),
      x = ~casos, y = ~destino, type = "bar", orientation = "h",
      marker = list(color = BAR_COLOR),
      hovertemplate = "%{y}<br>Casos (ORIGEN seleccionado → %{y}): %{x:,}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Casos (filas)"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 10, r = 10, t = 10, b = 10),
        showlegend = FALSE
      )
  })
  
  # ======= Top municipios DESTINO (excluye el mismo depto que ORIGEN) =======
  destinos_mpio_df <- reactive({
    req(input$f_anio_nda)
    origen_dep <- input$f_depto_nda %||% "Todos"
    origen_mun <- input$f_mpio_nda  %||% "Todos"
    validate(need(origen_dep != "Todos",
                  "Selecciona un Departamento (ORIGEN) para ver los municipios destino."))
    
    df <- nda %>% dplyr::filter(ano == input$f_anio_nda)
    if (!is.null(input$f_sexo_tab1) && input$f_sexo_tab1 != "Todos")
      df <- df %>% dplyr::filter(sexo_ni == input$f_sexo_tab1)
    
    df <- df %>%
      dplyr::filter(NUP(DEP_O) == NUP(origen_dep))
    if (!is.null(origen_mun) && origen_mun != "Todos")
      df <- df %>% dplyr::filter(NUP(MUN_O) == NUP(origen_mun))
    
    df <- df %>% dplyr::filter(NUP(DEP_D) != NUP(origen_dep))
    
    if (!is.null(input$f_depto_dest) && input$f_depto_dest != "Todos")
      df <- df %>% dplyr::filter(NUP(DEP_D) == NUP(input$f_depto_dest))
    
    out <- df %>%
      dplyr::filter(!is.na(MUN_D), nzchar(MUN_D)) %>%
      dplyr::mutate(destino_lbl = paste0(MUN_D, " (", DEP_D, ")")) %>%
      dplyr::count(destino_lbl, name = "casos") %>%
      dplyr::arrange(dplyr::desc(casos))
    out
  })
  
  output$destinos_mpio_top <- renderPlotly({
    df <- try(destinos_mpio_df(), silent = TRUE)
    if (inherits(df, "try-error") || is.null(df) || nrow(df) == 0)
      return(empty_plot("No hay municipios destino (excluyendo el mismo depto del ORIGEN)."))
    
    df2 <- df %>%
      dplyr::slice_head(n = 15) %>%
      dplyr::arrange(casos) %>%
      dplyr::mutate(destino_lbl = factor(destino_lbl, levels = destino_lbl))
    
    plot_ly(
      data = df2,
      x = ~casos, y = ~destino_lbl, type = "bar", orientation = "h",
      marker = list(color = BAR_COLOR),
      hovertemplate = "%{y}<br>Casos (ORIGEN → %{y}): %{x:,}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Casos"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 10, r = 10, t = 10, b = 10),
        showlegend = FALSE
      )
  })
  
  # ================= TAB 2 — Análisis poblacional =================
  output$anio_b2_ui <- renderUI({
    yrs <- sort(unique(nda$ano))
    selectInput("f_anio_b2", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  observeEvent(input$f_anio_b2, {
    deps <- nda %>% dplyr::filter(ano == input$f_anio_b2) %>% dplyr::distinct(DEP_D) %>% dplyr::arrange(DEP_D) %>% dplyr::pull(DEP_D)
    updateSelectInput(session, "f_depto_b2", choices = c("Todos", deps), selected = "Todos")
    updateSelectInput(session, "f_mpio_b2",  choices = "Todos", selected = "Todos")
  }, ignoreInit = FALSE)
  
  observeEvent(input$f_depto_b2, {
    if (is.null(input$f_depto_b2) || input$f_depto_b2 == "Todos") {
      updateSelectInput(session, "f_mpio_b2", choices = "Todos", selected = "Todos")
    } else {
      sel_dep <- dpt_lookup_nda$COD_DPTO2[dpt_lookup_nda$DEP_D == input$f_depto_b2][1]
      mm <- mun_lookup_nda %>% dplyr::filter(COD_DPTO2 == sel_dep) %>% dplyr::arrange(MUN_D)
      updateSelectInput(session, "f_mpio_b2", choices = c("Todos", mm$MUN_D), selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  nivel_b2 <- reactive({ if (is.null(input$f_depto_b2) || input$f_depto_b2 == "Todos") "deptos" else "mpios" })
  base_b2  <- reactive({
    req(input$f_anio_b2)
    df <- nda %>% dplyr::filter(ano == input$f_anio_b2)
    if (!is.null(input$f_depto_b2) && input$f_depto_b2 != "Todos") df <- df %>% dplyr::filter(DEP_D == input$f_depto_b2)
    if (!is.null(input$f_mpio_b2)  && input$f_mpio_b2  != "Todos") df <- df %>% dplyr::filter(MUN_D == input$f_mpio_b2)
    if (!is.null(input$f_sexo_tab2) && input$f_sexo_tab2 != "Todos") df <- df %>% dplyr::filter(sexo_ni == input$f_sexo_tab2)
    df
  })
  
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
  
  # ---- Títulos storytelling TAB 2
  output$ttl_map_incid_b2 <- renderText({
    amb <- if (nivel_b2()=="deptos") "departamentos de Colombia" else paste0("municipios de ", input$f_depto_b2)
    paste0("¿Dónde es mayor la incidencia (x100k) de NDA en los ", amb, " durante ", input$f_anio_b2, "?")
  })
  output$ttl_edad_b2 <- renderText({
    paste0("¿Qué edades concentran más casos en ", scope_b2(), " — ", input$f_anio_b2, "?")
  })
  output$ttl_sexo_b2 <- renderText({
    paste0("¿Cómo se distribuyen los casos entre Hombres y Mujeres en ", scope_b2(), " — ", input$f_anio_b2, "?")
  })
  output$ttl_top_incid_b2 <- renderText({
    paste0("¿Qué departamentos (DESTINO) concentran la mayor incidencia — ", input$f_anio_b2, "?")
  })
  
  output$map_nda_incid <- renderLeaflet({
    leaflet::leaflet() %>% leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% leaflet::setView(lng=-74.3, lat=4.6, zoom=5)
  })
  observe({
    titulo <- "Incidencia (x100.000 hab.)"
    fmt <- function(x) number(x, big.mark=",", accuracy=0.1)
    if (nivel_b2()=="deptos") {
      dd <- agg_incid_depto()
      shp <- dptos_sf %>% dplyr::left_join(dd, by="COD_DPTO2") %>% dplyr::left_join(dpt_lookup_nda, by="COD_DPTO2") %>%
        dplyr::mutate(incid=coalesce(incid,0), etq=paste0("<b>", DEP_D, "</b><br>", titulo, ": ", fmt(incid)))
      pal <- make_pal(shp$incid, MAP_COLORS)
      leaflet::leafletProxy("map_nda_incid", data=shp) %>% leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId=~COD_DPTO2, fillColor=~pal(incid), color=BORDER_UI, weight=0.7, fillOpacity=0.9,
                             label=~lapply(etq, htmltools::HTML),
                             highlightOptions=leaflet::highlightOptions(color=BORDER_UI, weight=2, bringToFront=TRUE)) %>%
        leaflet::addLegend("bottomright", pal=pal, values=~incid, title=titulo)
    } else {
      sel_dep <- dpt_lookup_nda %>% dplyr::filter(DEP_D==input$f_depto_b2) %>% dplyr::pull(COD_DPTO2) %>% .[1]
      req(!is.na(sel_dep), nzchar(sel_dep))
      dd <- agg_incid_mpio()
      shp <- mpios_sf %>% dplyr::filter(COD_DPTO2==sel_dep) %>% dplyr::left_join(dd %>% dplyr::select(COD_MUN5, MUN_D, incid), by="COD_MUN5") %>%
        dplyr::mutate(MUN_D=ifelse(is.na(MUN_D), MUNICIPIO_N, MUN_D), incid=coalesce(incid,0),
                      etq=paste0("<b>", MUN_D, "</b><br>", titulo, ": ", fmt(incid)))
      pal <- make_pal(shp$incid, MAP_COLORS); bb <- sf::st_bbox(shp)
      leaflet::leafletProxy("map_nda_incid", data=shp) %>% leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId=~COD_MUN5, fillColor=~pal(incid), color=BORDER_UI, weight=0.4, fillOpacity=0.9,
                             label=~lapply(etq, htmltools::HTML),
                             highlightOptions=leaflet::highlightOptions(color=BORDER_UI, weight=2, bringToFront=TRUE)) %>%
        leaflet::addLegend("bottomright", pal=pal, values=~incid, title=titulo) %>%
        leaflet::fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  observeEvent(input$map_nda_incid_shape_click, {
    if (nivel_b2()=="deptos") {
      click <- input$map_nda_incid_shape_click; req(click$id)
      cod <- sprintf("%02d", as.integer(click$id))
      nom <- dpt_lookup_nda$DEP_D[dpt_lookup_nda$COD_DPTO2==cod][1]
      if (!is.na(nom) && nzchar(nom)) updateSelectInput(session, "f_depto_b2", selected=nom)
    }
  }, ignoreInit = TRUE)
  
  output$top_incid_dest_top20 <- renderPlotly({
    df <- base_b2() %>%
      dplyr::group_by(COD_DPTO2, DEP_D) %>%
      dplyr::summarise(casos = dplyr::n(), .groups = "drop") %>%
      dplyr::left_join(pob_depto %>% dplyr::filter(ano == input$f_anio_b2), by = "COD_DPTO2") %>%
      dplyr::mutate(
        incid  = ifelse(POB > 0, (casos / POB) * 1e5, NA_real_),
        nombre = ifelse(!is.na(DEP_D) & nzchar(DEP_D), DEP_D, COD_DPTO2)
      ) %>%
      dplyr::filter(is.finite(incid)) %>%
      dplyr::arrange(dplyr::desc(incid)) %>%
      dplyr::slice_head(n = 33)
    
    if (is.null(df) || nrow(df) == 0) return(empty_plot())
    
    df2 <- df %>% dplyr::arrange(incid)
    cd  <- cbind(df2$casos, df2$POB)
    
    plot_ly(
      data = df2, x = ~incid, y = ~factor(nombre, levels = df2$nombre),
      type = "bar", orientation = "h", customdata = cd,
      marker = list(color = BAR_COLOR),
      hovertemplate = "%{y}<br>Incidencia: %{x:.2f}<br>Casos: %{customdata[0]:,}<br>Población: %{customdata[1]:,}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Incidencia por 100.000 hab."),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 10, r = 10, b = 10, t = 10),
        showlegend = FALSE
      )
  })
  
  # ---- BARRAS: grupos de edad (Tab 2) ----
  output$edad_nna_barras <- renderPlotly({
    df <- base_b2() %>%
      dplyr::filter(!is.na(edad_ni)) %>%
      dplyr::mutate(
        edad_ni = suppressWarnings(as.numeric(edad_ni)),
        grupo_edad = dplyr::case_when(
          edad_ni < 1                 ~ "<1 año",
          edad_ni >=1 & edad_ni < 2   ~ "1–1.9",
          edad_ni >=2 & edad_ni < 3   ~ "2–2.9",
          edad_ni >=3 & edad_ni < 5   ~ "3–4.9",
          edad_ni >=5 & edad_ni < 10  ~ "5–9.9",
          edad_ni >=10 & edad_ni < 15 ~ "10–14.9",
          edad_ni >=15                ~ "15+",
          TRUE                        ~ "Sin dato"
        )
      ) %>%
      dplyr::group_by(grupo_edad) %>%
      dplyr::summarise(Casos = dplyr::n(), .groups = "drop")
    
    if (nrow(df) == 0) return(empty_plot("No hay datos de edad para los filtros seleccionados."))
    
    orden <- c("<1 año","1–1.9","2–2.9","3–4.9","5–9.9","10–14.9","15+","Sin dato")
    df <- df %>% dplyr::mutate(grupo_edad = factor(grupo_edad, levels = orden))
    
    plot_ly(df, x = ~Casos, y = ~grupo_edad, type = "bar", orientation = "h",
            marker = list(color = BAR_COLOR),
            hovertemplate = "Edad: %{y}<br>Casos: %{x:,}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Casos"),
        yaxis = list(title = "Edad del niño/a"),
        margin = list(l = 90, r = 10, b = 40, t = 10),
        showlegend = FALSE
      )
  })
  
  # ---- BARRAS: distribución por sexo (Tab 2) ----
  output$sexo_nna_barras <- renderPlotly({
    df <- base_b2() %>%
      dplyr::mutate(sexo_cat = dplyr::case_when(
        sexo_ni %in% c("Hombres","Mujeres") ~ sexo_ni,
        TRUE ~ "Sin dato"
      )) %>%
      dplyr::group_by(sexo_cat) %>%
      dplyr::summarise(Casos = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(match(sexo_cat, c("Hombres","Mujeres","Sin dato")))
    
    if (nrow(df) == 0) return(empty_plot("No hay datos de sexo para los filtros seleccionados."))
    
    cols <- SEX_COLORS[df$sexo_cat]; cols[is.na(cols)] <- "#cbd5e1"
    plot_ly(df, x = ~Casos, y = ~sexo_cat, type = "bar", orientation = "h",
            marker = list(color = cols),
            hovertemplate = "%{y}<br>Casos: %{x:,}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Casos"),
        yaxis = list(title = "Sexo"),
        margin = list(l = 90, r = 10, b = 20, t = 10),
        showlegend = FALSE
      )
  })
  
  # ================= TAB 3 — Defunciones y Supervivencia =================
  output$anio_b3_ui <- renderUI({
    yrs <- sort(unique(nda$ano))
    selectInput("f_anio_b3", "", choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  output$dep_o_b3_ui <- renderUI({
    deps <- sort(unique(na.omit(nda$DEP_O)))
    selectInput("f_dep_o_b3", "", choices = c("Todos", deps), selected = "Todos")
  })
  output$mun_o_b3_ui <- renderUI({
    req(input$f_dep_o_b3)
    if (input$f_dep_o_b3 == "Todos") {
      selectInput("f_mun_o_b3", "", choices = "Todos", selected = "Todos")
    } else {
      mm <- nda %>% dplyr::filter(DEP_O == input$f_dep_o_b3) %>% dplyr::distinct(MUN_O) %>% dplyr::arrange(MUN_O)
      selectInput("f_mun_o_b3", "", choices = c("Todos", mm$MUN_O), selected = "Todos")
    }
  })
  output$dep_d_b3_ui <- renderUI({
    deps <- sort(unique(na.omit(nda$DEP_D)))
    selectInput("f_dep_d_b3", "", choices = c("Todos", deps), selected = "Todos")
  })
  output$mun_d_b3_ui <- renderUI({
    req(input$f_dep_d_b3)
    if (input$f_dep_d_b3 == "Todos") {
      selectInput("f_mun_d_b3", "", choices = "Todos", selected = "Todos")
    } else {
      mm <- nda %>% dplyr::filter(DEP_D == input$f_dep_d_b3) %>% dplyr::distinct(MUN_D) %>% dplyr::arrange(MUN_D)
      selectInput("f_mun_d_b3", "", choices = c("Todos", mm$MUN_D), selected = "Todos")
    }
  })
  output$sexo_b3_ui <- renderUI({
    selectInput("f_sexo_b3", "", choices = c("Todos","Hombres","Mujeres"), selected = "Todos")
  })
  
  base_b3 <- reactive({
    req(input$f_anio_b3)
    df <- nda %>% dplyr::filter(ano == input$f_anio_b3)
    if (!is.null(input$f_dep_o_b3) && input$f_dep_o_b3 != "Todos") df <- df %>% dplyr::filter(DEP_O == input$f_dep_o_b3)
    if (!is.null(input$f_mun_o_b3) && input$f_mun_o_b3 != "Todos") df <- df %>% dplyr::filter(MUN_O == input$f_mun_o_b3)
    if (!is.null(input$f_dep_d_b3) && input$f_dep_d_b3 != "Todos") df <- df %>% dplyr::filter(DEP_D == input$f_dep_d_b3)
    if (!is.null(input$f_mun_d_b3) && input$f_mun_d_b3 != "Todos") df <- df %>% dplyr::filter(MUN_D == input$f_mun_d_b3)
    if (!is.null(input$f_sexo_b3)  && input$f_sexo_b3  != "Todos")  df <- df %>% dplyr::filter(sexo_ni == input$f_sexo_b3)
    df
  })
  
  # ---- Títulos storytelling TAB 3
  output$ttl_serie_b3 <- renderText({
    paste0("¿Cómo ha evolucionado mensualmente la supervivencia y las defunciones en ", scope_b3(), " — ", input$f_anio_b3, "?")
  })
  output$ttl_hist_b3 <- renderText({
    paste0("¿En qué edades se concentran las defunciones de NDA — ", scope_b3(), " — ", input$f_anio_b3, "?")
  })
  output$ttl_map_superv_b3 <- renderText({
    paste0("¿Dónde es más alta la tasa de supervivencia (Vivos/(Vivos+Muertes)) en ", scope_b3(), " — ", input$f_anio_b3, "?")
  })
  
  output$serie_anual_tab3 <- renderPlotly({
    df0 <- base_b3() %>% dplyr::filter(!is.na(PAC_HOS), PAC_HOS %in% c(1L,2L))
    if (is.null(df0) || nrow(df0) == 0) return(empty_plot("Sin datos para los filtros seleccionados."))
    
    mes_col <- if ("mes" %in% names(df0)) "mes" else if ("MES" %in% names(df0)) "MES" else NA_character_
    if (!is.na(mes_col)) {
      df0 <- df0 %>% dplyr::mutate(
        MES = suppressWarnings(as.integer(.data[[mes_col]])),
        MES = dplyr::if_else(is.finite(MES) & MES >= 1 & MES <= 12, MES, 1L)
      )
    } else df0 <- df0 %>% dplyr::mutate(MES = 1L)
    
    dfm <- df0 %>%
      dplyr::mutate(ANO = suppressWarnings(as.integer(ano)),
                    FECHA = as.Date(sprintf("%04d-%02d-01", ANO, MES))) %>%
      dplyr::filter(!is.na(FECHA)) %>%
      dplyr::group_by(FECHA, PAC_HOS) %>% dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = PAC_HOS, values_from = n, values_fill = 0, names_prefix = "cat_") %>%
      dplyr::transmute(FECHA, vivos = cat_1, muertes = cat_2) %>%
      dplyr::arrange(FECHA)
    
    plot_ly(dfm, x = ~FECHA) %>%
      add_lines(y = ~vivos, name = "Vivos", yaxis = "y",
                line = list(color = "#2e7d32"),
                hovertemplate = "Fecha: %{x|%Y-%m}<br>Vivos: %{y:,}<extra></extra>") %>%
      add_lines(y = ~muertes, name = "Muertes", yaxis = "y2",
                line = list(color = "#c62828"),
                hovertemplate = "Fecha: %{x|%Y-%m}<br>Muertes: %{y:,}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Año–Mes", tickformat = "%Y-%m"),
        yaxis = list(title = "Vivos"),
        yaxis2 = list(title = "Muertes", overlaying = "y", side = "right"),
        legend = list(orientation = "h", x = 0, y = 1.1),
        margin = list(l = 50, r = 50, b = 40, t = 10)
      )
  })
  
  output$hist_edad_tab3 <- renderPlotly({
    df <- base_b3() %>%
      dplyr::filter(!is.na(edad_ni), !is.na(PAC_HOS), PAC_HOS == 2L) %>%
      dplyr::mutate(
        edad_ni = suppressWarnings(as.numeric(edad_ni)),
        grupo_edad = dplyr::case_when(
          edad_ni < 1                 ~ "<1 año",
          edad_ni >=1 & edad_ni < 2   ~ "1–1.9",
          edad_ni >=2 & edad_ni < 3   ~ "2–2.9",
          edad_ni >=3 & edad_ni < 5   ~ "3–4.9",
          edad_ni >=5 & edad_ni < 10  ~ "5–9.9",
          edad_ni >=10 & edad_ni < 15 ~ "10–14.9",
          edad_ni >=15                ~ "15+",
          TRUE                        ~ "Sin dato"
        )
      ) %>% dplyr::group_by(grupo_edad) %>% dplyr::summarise(Fallecimientos = dplyr::n(), .groups = "drop")
    
    if (is.null(df) || nrow(df) == 0) return(empty_plot("No hay fallecidos con edad válida para los filtros seleccionados."))
    
    orden <- c("<1 año","1–1.9","2–2.9","3–4.9","5–9.9","10–14.9","15+","Sin dato")
    df <- df %>% dplyr::mutate(grupo_edad = factor(grupo_edad, levels = orden))
    
    plot_ly(
      data = df, x = ~Fallecimientos, y = ~grupo_edad,
      type = "bar", orientation = "h",
      marker = list(color = BAR_COLOR),
      hovertemplate = "Edad: %{y}<br>Fallecimientos: %{x}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Número de fallecimientos"),
        yaxis = list(title = "Edad del niño/a"),
        margin = list(l = 90, r = 10, b = 40, t = 10),
        showlegend = FALSE
      )
  })
  
  # Mapa de Supervivencia
  nivel_b3_map <- reactive({ if (!is.null(input$f_dep_d_b3) && input$f_dep_d_b3 != "Todos") "mpios" else "deptos" })
  sel_cod_dep_b3 <- reactive({
    if (nivel_b3_map() == "mpios") {
      cod <- dptos_sf %>% dplyr::filter(DEPARTAMENTO_N == input$f_dep_d_b3) %>% dplyr::pull(COD_DPTO2) %>% .[1]
      if (is.na(cod) || !nzchar(cod)) cod <- base_b3() %>% dplyr::distinct(COD_DPTO2) %>% dplyr::pull(COD_DPTO2) %>% .[1]
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
    leaflet::leaflet() %>% leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% leaflet::setView(lng = -74.3, lat = 4.6, zoom = 5)
  })
  observe({
    titulo <- "Tasa de supervivencia (Vivos / (Vivos+Muertes))"
    fmt_p  <- function(x) ifelse(is.na(x), "s/d", scales::percent(x, accuracy = 0.1))
    fmt_n  <- function(x) scales::comma(x)
    if (nivel_b3_map() == "deptos") {
      dd  <- agg_superv_depto_b3()
      lkp <- dpt_lookup_nda %>% dplyr::rename(DEP_D_LKP = DEP_D)
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
      pal  <- leaflet::colorBin(MAP_COLORS, domain = shp$tasa, bins = bins, na.color = "#f0f0f0")
      leaflet::leafletProxy("map_superv_b3", data = shp) %>%
        leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId = ~COD_DPTO2, fillColor = ~pal(tasa),
                             color = BORDER_UI, weight = 0.7, fillOpacity = 0.9,
                             label = ~lapply(etq, htmltools::HTML),
                             highlightOptions = leaflet::highlightOptions(color = BORDER_UI, weight = 2, bringToFront = TRUE)) %>%
        leaflet::addLegend("bottomright", pal = pal, values = ~tasa, title = titulo)
    } else {
      sel_cod <- sel_cod_dep_b3(); req(!is.na(sel_cod), nzchar(sel_cod))
      dd <- agg_superv_mpio_b3()
      shp <- mpios_sf %>%
        dplyr::filter(COD_DPTO2 == sel_cod) %>%
        dplyr::left_join(dd %>% dplyr::select(COD_MUN5, MUN_D, vivos, muertes, n, tasa), by = "COD_MUN5") %>%
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
      pal  <- leaflet::colorBin(MAP_COLORS, domain = shp$tasa, bins = bins, na.color = "#f0f0f0")
      bb   <- sf::st_bbox(shp)
      leaflet::leafletProxy("map_superv_b3", data = shp) %>%
        leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId = ~COD_MUN5, fillColor = ~pal(tasa),
                             color = BORDER_UI, weight = 0.4, fillOpacity = 0.9,
                             label = ~lapply(etq, htmltools::HTML),
                             highlightOptions = leaflet::highlightOptions(color = BORDER_UI, weight = 2, bringToFront = TRUE)) %>%
        leaflet::addLegend("bottomright", pal = pal, values = ~tasa, title = titulo) %>%
        leaflet::fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  observeEvent(input$map_superv_b3_shape_click, {
    if (nivel_b3_map() == "deptos") {
      click <- input$map_superv_b3_shape_click; req(click$id)
      cod <- sprintf("%02d", as.integer(click$id))
      nom <- dptos_sf$DEPARTAMENTO_N[match(cod, dptos_sf$COD_DPTO2)]
      deps_disp <- c("Todos", sort(unique(na.omit(nda$DEP_D))))
      if (!is.na(nom) && nzchar(nom) && (nom %in% deps_disp)) {
        updateSelectInput(session, "f_dep_d_b3", selected = nom)
      }
    }
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
