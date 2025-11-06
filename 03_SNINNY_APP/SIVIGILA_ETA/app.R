# app_eta.R
# =========================================================
# ETA — Dashboard (app exclusiva)
# - Tab 1 (Exploración ETA): filtros propios (e1)
# - Tab 2 (Análisis ETA): filtros propios (e2)
# =========================================================

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(leaflet); library(sf); library(dplyr); library(tidyr)
  library(scales); library(htmltools); library(DT); library(plotly)
})

options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)
options(shiny.maxRequestSize = 100*1024^2)

# ---- Alias seguros (evita choque con plotly::validate) ----
validate <- shiny::validate
need     <- shiny::need

# ---------- Rutas ----------
local_data_dir <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/SIVIGILA_ETA/data"
app_root     <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
rel_data_dir <- file.path(app_root, "data")
data_dir <- if (dir.exists(rel_data_dir)) rel_data_dir else local_data_dir

eta_path       <- file.path(data_dir, "022_INS_SIVIGILA-ETA.rds")
ruta_pob       <- file.path(data_dir, "051_DANE_Proyecciones_P_total.rds")
ruta_shp_mpios <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")
ruta_shp_dptos <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")

must_exist <- c(eta_path, ruta_pob, ruta_shp_mpios, ruta_shp_dptos)
miss <- must_exist[!file.exists(must_exist)]
if (length(miss)) stop("Faltan archivos. data_dir usado: ", data_dir, "\n", paste("-", miss, collapse = "\n"))
check_shp_parts <- function(shp){ base <- sub("\\.shp$", "", shp); req <- paste0(base, c(".shp",".dbf",".shx",".prj")); req[!file.exists(req)] }
miss_shp <- c(check_shp_parts(ruta_shp_mpios), check_shp_parts(ruta_shp_dptos))
if (length(miss_shp)) stop("Faltan componentes de shapefile:\n", paste("-", miss_shp, collapse = "\n"))

# ---------- 1) Leer ETA y normalizar ----------
eta_raw <- readRDS(eta_path) %>% dplyr::filter(!is.na(MUNICIPIO_D))
year_col     <- if ("ano" %in% names(eta_raw)) "ano" else if ("ANO" %in% names(eta_raw)) "ANO" else stop("No encuentro 'ano'/'ANO'")
mun_code_col <- if ("COD_DANE_MUNIC_D" %in% names(eta_raw)) "COD_DANE_MUNIC_D" else stop("No encuentro 'COD_DANE_MUNIC_D'")
dep_name_col <- if ("DEPARTAMENTO_D" %in% names(eta_raw)) "DEPARTAMENTO_D" else if ("DEPARTMENTO_D" %in% names(eta_raw)) "DEPARTMENTO_D" else "DEPARTAMENTO_D"
mun_name_col <- if ("MUNICIPIO_D" %in% names(eta_raw)) "MUNICIPIO_D" else "MUNICIPIO_D"

tenf_col_candidates <- c("TOTAL_ENF","total_enf","TOTALENF","Total_enf","total_enfermos","TOTAL_ENFERMOS")
tenf_col <- {x<-tenf_col_candidates[tenf_col_candidates %in% names(eta_raw)]; if(length(x)) x[1] else NA_character_}

texp_col_candidates <- c("TOTAL_EXP","total_exp","TOTALEXP","Total_exp","total_expuestos","TOTAL_EXPUES")
texp_col <- {x<-texp_col_candidates[texp_col_candidates %in% names(eta_raw)]; if(length(x)) x[1] else NA_character_}

origen_cols <- intersect(c("agua","alimentos","pers_pers","cont_ambie","otro","desconocid"), names(eta_raw))
hom_col <- if ("total_hom" %in% names(eta_raw)) "total_hom" else NA_character_
muj_col <- if ("total_muj" %in% names(eta_raw)) "total_muj" else NA_character_

eta <- eta_raw %>%
  dplyr::transmute(
    ano       = suppressWarnings(as.integer(.data[[year_col]])),
    COD_MUN5  = sprintf("%05d", suppressWarnings(as.integer(.data[[mun_code_col]]))),
    COD_DPTO2 = substr(COD_MUN5, 1, 2),
    DEP_N     = trimws(as.character(.data[[dep_name_col]])),
    MUN_N     = trimws(as.character(.data[[mun_name_col]])),
    TOTAL_ENF = if (!is.na(tenf_col)) suppressWarnings(as.numeric(.data[[tenf_col]])) else NA_real_,
    TOTAL_EXP = if (!is.na(texp_col)) suppressWarnings(as.numeric(.data[[texp_col]])) else NA_real_,
    TOTAL_HOM = if (!is.na(hom_col))  suppressWarnings(as.numeric(.data[[hom_col]])) else NA_real_,
    TOTAL_MUJ = if (!is.na(muj_col))  suppressWarnings(as.numeric(.data[[muj_col]])) else NA_real_
  ) %>%
  dplyr::mutate(TOTAL_NR = pmax(coalesce(TOTAL_ENF, 0) - (coalesce(TOTAL_HOM, 0) + coalesce(TOTAL_MUJ, 0)), 0)) %>%
  dplyr::filter(!is.na(ano), !is.na(COD_MUN5), !is.na(COD_DPTO2))

if (length(origen_cols) > 0) {
  eta <- dplyr::bind_cols(eta, eta_raw %>% dplyr::select(dplyr::all_of(origen_cols))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(origen_cols), ~ ifelse(as.integer(.x) == 1, 1L, 0L)))
}

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

# ---------- Lookups (ETA) ----------
dpt_lookup_eta <- eta %>% dplyr::select(COD_DPTO2, DEP_N) %>%
  dplyr::mutate(COD_DPTO2=sprintf("%02d",as.integer(COD_DPTO2)), DEP_N=trimws(DEP_N)) %>%
  dplyr::distinct() %>% dplyr::arrange(DEP_N)

mun_lookup_eta <- eta %>% dplyr::select(COD_DPTO2, COD_MUN5, MUN_N) %>%
  dplyr::mutate(COD_DPTO2=sprintf("%02d",as.integer(COD_DPTO2)), COD_MUN5=sprintf("%05d",as.integer(COD_MUN5)), MUN_N=trimws(MUN_N)) %>%
  dplyr::distinct()

# ---------- Paletas/colores ----------
MAP_COLORS <- c("#f4e6f9","#d9aee9","#b46cd2","#8e44ad","#602070")  # choropleths

BAR_COLOR  <- "#8e44ad"                                            # barras
GEN_PIE    <- c("#8e44ad", "#71BB52")                              # morado + opuesto

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
  # ===== CSS: bordes en #d9aee9 para filtros, cards e inputs =====
  tags$head(tags$style(HTML("
    :root{ --border-col:#d9aee9; --accent:#8e44ad; }
    .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
    h3{font-weight:700;letter-spacing:.2px;margin-bottom:16px}
    .filters{
      background:#fff;border:1.5px solid var(--border-col);
      border-radius:16px;padding:14px 16px;margin-bottom:16px;
      box-shadow:0 2px 10px rgba(0,0,0,.04);
    }
    .filters-grid{display:grid;grid-template-columns:repeat(4,minmax(220px,1fr));gap:12px}
    .filter-label{font-size:12px;font-weight:600;letter-spacing:.4px;text-transform:uppercase;color:#6b7280;margin-bottom:6px}
    .selectize-input,.form-control{
      min-height:42px;border-radius:10px;border:1.5px solid var(--border-col);
      box-shadow:none !important;
    }
    .selectize-input:focus,.form-control:focus{
      border-color:var(--border-col) !important; outline:0 !important;
      box-shadow:0 0 0 .15rem rgba(217,174,233,.35) !important;
    }
    .card{
      background:#fff;border:1.5px solid var(--border-col);
      border-radius:16px;padding:12px;box-shadow:0 2px 10px rgba(0,0,0,.05);margin-bottom:12px
    }
    .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}
    /* Bordes suaves también en pestañas */
    .nav-tabs .nav-link.active{border-color:var(--border-col) var(--border-col) #fff !important}
    .nav-tabs{border-bottom:1.5px solid var(--border-col)}
  "))),
  div(class="wrap",
      h3("SIVIGILA — ETA"),
      tabsetPanel(
        id = "tabs_eta",
        type = "tabs",
        # ---------------- TAB 1: Exploración ETA ----------------
        tabPanel(
          title = "Exploración ETA",
          br(),
          div(class="filters",
              div(class="filters-grid",
                  div(class="filter", div(class="filter-label","Año"), uiOutput("anio_e1_ui")),
                  div(class="filter", div(class="filter-label","Departamento"),
                      selectInput("f_depto_e1", NULL, choices = c("Todos", dpt_lookup_eta$DEP_N), selected = "Todos")),
                  div(class="filter", div(class="filter-label","Municipio"),
                      selectInput("f_mpio_e1", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Indicador"),
                      selectInput("f_indic_e1", NULL,
                                  choices = c("Casos (filas)" = "casos",
                                              "Total de enfermos (suma)" = "total_enf",
                                              "Incidencia (x100k)" = "incid"),
                                  selected = "casos"))
              )
          ),
          fluidRow(
            column(6,
                   div(class="card",
                       div(class="card-title",
                           textOutput("ttl_mapa_e1"),
                           span(style="float:right;", actionLink("btn_reset_e1","← Volver a departamentos"))
                       ),
                       leafletOutput("map_eta_e1", height = 640)
                   )
            ),
            column(6,
                   fluidRow(
                     column(12, div(class="card",
                                    div(class="card-title", textOutput("ttl_origen_e1")),
                                    div(style="margin-top:-6px;margin-bottom:6px;color:#6b7280;font-size:12px;",
                                        textOutput("ttl_origen_tot_e1")),
                                    plotlyOutput("plot_origen_e1", height = 260)
                     ))
                   ),
                   fluidRow(
                     column(12, div(class="card",
                                    div(class="card-title", textOutput("ttl_top_e1")),
                                    plotlyOutput("top_mpios_e1", height = 300)
                     ))
                   )
            )
          )
        ),
        # ---------------- TAB 2: Análisis ETA ----------------
        tabPanel(
          title = "Análisis ETA",
          br(),
          div(class="filters",
              div(class="filters-grid",
                  div(class="filter", div(class="filter-label","Año"), uiOutput("anio_e2_ui")),
                  div(class="filter", div(class="filter-label","Departamento"),
                      selectInput("f_depto_e2", NULL, choices = c("Todos", dpt_lookup_eta$DEP_N), selected = "Todos")),
                  div(class="filter", div(class="filter-label","Municipio"),
                      selectInput("f_mpio_e2", NULL, choices = "Todos", selected = "Todos"))
              )
          ),
          fluidRow(
            column(6,
                   div(class="card",
                       div(class="card-title", textOutput("ttl_genero_e2")),
                       plotlyOutput("plot_genero_e2", height = 320)
                   ),
                   div(class="card",
                       div(class="card-title", textOutput("ttl_exp_enf_map_e2")),
                       leafletOutput("map_exp_enf_e2", height = 400)
                   )
            ),
            column(6,
                   div(class="card",
                       div(class="card-title", textOutput("ttl_incid_bar_e2")),
                       plotlyOutput("incid_bar_e2", height = 800)
                   )
            )
          )
        )
      )
  )
)

# ---------- 4) SERVER ----------
server <- function(input, output, session){
  
  # Helpers para storytelling --------------------------------------------------
  scope_txt <- reactive({
    if (is.null(input$f_depto_e1) || input$f_depto_e1 == "Todos") "Colombia" else {
      if (!is.null(input$f_mpio_e1) && input$f_mpio_e1 != "Todos") paste(input$f_mpio_e1, ",", input$f_depto_e1) else input$f_depto_e1
    }
  })
  scope_txt_e2 <- reactive({
    if (is.null(input$f_depto_e2) || input$f_depto_e2 == "Todos") "Colombia" else {
      if (!is.null(input$f_mpio_e2) && input$f_mpio_e2 != "Todos") paste(input$f_mpio_e2, ",", input$f_depto_e2) else input$f_depto_e2
    }
  })
  indic_lbl_e1 <- reactive({ if (input$f_indic_e1=="casos") "Casos (filas)" else if (input$f_indic_e1=="total_enf") "Total de enfermos" else "Incidencia (x100k)" })
  
  # ================= ETA — Exploración (e1) =================
  output$anio_e1_ui <- renderUI({
    yrs <- sort(unique(eta$ano))
    selectInput("f_anio_e1", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  observeEvent(input$f_depto_e1, {
    if (is.null(input$f_depto_e1) || input$f_depto_e1 == "Todos") {
      updateSelectInput(session, "f_mpio_e1", choices = "Todos", selected = "Todos")
    } else {
      sel_dep <- dpt_lookup_eta$COD_DPTO2[dpt_lookup_eta$DEP_N == input$f_depto_e1][1]
      mm <- mun_lookup_eta %>% dplyr::filter(COD_DPTO2 == sel_dep) %>% dplyr::arrange(MUN_N)
      updateSelectInput(session, "f_mpio_e1", choices = c("Todos", mm$MUN_N), selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  nivel_e1 <- reactive({ if (is.null(input$f_depto_e1) || input$f_depto_e1 == "Todos") "deptos" else "mpios" })
  base_e1 <- reactive({
    req(input$f_anio_e1)
    df <- eta %>% dplyr::filter(ano == input$f_anio_e1)
    if (!is.null(input$f_depto_e1) && input$f_depto_e1 != "Todos") df <- df %>% dplyr::filter(DEP_N == input$f_depto_e1)
    if (!is.null(input$f_mpio_e1) && input$f_mpio_e1 != "Todos") df <- df %>% dplyr::filter(MUN_N == input$f_mpio_e1)
    df
  })
  agg_depto_e1 <- reactive({
    df <- base_e1()
    if (input$f_indic_e1 == "casos") df %>% dplyr::group_by(COD_DPTO2) %>% dplyr::summarise(valor=dplyr::n(), .groups="drop")
    else if (input$f_indic_e1 == "total_enf") df %>% dplyr::group_by(COD_DPTO2) %>% dplyr::summarise(valor=sum(TOTAL_ENF,na.rm=TRUE), .groups="drop")
    else {
      sum_enf <- df %>% dplyr::group_by(COD_DPTO2) %>% dplyr::summarise(total_enf=sum(TOTAL_ENF,na.rm=TRUE), .groups="drop")
      sum_enf %>% dplyr::left_join(pob_depto %>% dplyr::filter(ano==input$f_anio_e1), by="COD_DPTO2") %>%
        dplyr::mutate(valor=ifelse(POB>0,(total_enf/POB)*1e5,NA_real_)) %>% dplyr::select(COD_DPTO2, valor)
    }
  })
  agg_mpio_e1 <- reactive({
    df <- base_e1()
    if (input$f_indic_e1 == "casos") df %>% dplyr::group_by(COD_DPTO2, COD_MUN5, MUN_N) %>% dplyr::summarise(valor=dplyr::n(), .groups="drop")
    else if (input$f_indic_e1 == "total_enf") df %>% dplyr::group_by(COD_DPTO2, COD_MUN5, MUN_N) %>% dplyr::summarise(valor=sum(TOTAL_ENF,na.rm=TRUE), .groups="drop")
    else {
      sum_enf <- df %>% dplyr::group_by(COD_DPTO2, COD_MUN5, MUN_N) %>% dplyr::summarise(total_enf=sum(TOTAL_ENF,na.rm=TRUE), .groups="drop")
      sum_enf %>% dplyr::left_join(pob_mpio %>% dplyr::filter(ano==input$f_anio_e1) %>% dplyr::select(COD_MUN5,POB), by="COD_MUN5") %>%
        dplyr::mutate(valor=ifelse(POB>0,(total_enf/POB)*1e5,NA_real_)) %>% dplyr::select(COD_DPTO2, COD_MUN5, MUN_N, valor)
    }
  })
  
  # ---- Títulos storytelling (TAB 1) ----
  output$ttl_mapa_e1 <- renderText({
    amb <- scope_txt()
    ind <- tolower(indic_lbl_e1())
    paste0("¿Dónde se concentran los ", ind, " en ", amb, " en ", input$f_anio_e1, "?")
  })
  output$ttl_origen_e1 <- renderText({
    paste0("¿Cuál fue el principal origen reportado en ", scope_txt(), " durante ", input$f_anio_e1, "?")
  })
  output$ttl_top_e1 <- renderText({
    paste0("¿Qué municipios lideran el ", tolower(indic_lbl_e1()), " en ", scope_txt(), " — ", input$f_anio_e1, "?")
  })
  
  output$ttl_origen_tot_e1 <- renderText({
    if (!length(origen_cols)) return("Sin datos de origen")
    d <- base_e1() %>%
      dplyr::select(dplyr::all_of(origen_cols), TOTAL_ENF) %>%
      tidyr::pivot_longer(cols=dplyr::all_of(origen_cols), names_to="Origen", values_to="flag") %>%
      dplyr::filter(flag==1) %>%
      dplyr::group_by(Origen) %>% dplyr::summarise(Casos=dplyr::n(), Enfermos=sum(TOTAL_ENF,na.rm=TRUE), .groups="drop")
    paste0("Casos totales: ", scales::comma(sum(d$Casos,na.rm=TRUE)),
           "  |  Total de enfermos: ", scales::comma(sum(d$Enfermos,na.rm=TRUE)))
  })
  
  output$plot_origen_e1 <- renderPlotly({
    if (!length(origen_cols)) return(NULL)
    d <- base_e1() %>%
      dplyr::select(dplyr::all_of(origen_cols), TOTAL_ENF) %>%
      tidyr::pivot_longer(cols=dplyr::all_of(origen_cols), names_to="Origen", values_to="flag") %>%
      dplyr::filter(flag==1) %>%
      dplyr::group_by(Origen) %>% dplyr::summarise(Casos=dplyr::n(), Enfermos=sum(TOTAL_ENF,na.rm=TRUE), .groups="drop") %>%
      dplyr::mutate(Origen=dplyr::recode(Origen,"agua"="Agua","alimentos"="Alimentos","pers_pers"="Persona a persona","cont_ambie"="Contaminación ambiental","otro"="Otro","desconocid"="Desconocido",.default=Origen))
    plot_ly(d, x=~Casos, y=~reorder(Origen, Casos), type="bar", orientation="h",
            marker=list(color=BAR_COLOR),
            customdata=~Enfermos,
            hovertemplate="%{y}<br>Casos: %{x:,}<br>Enfermos: %{customdata:,}<extra></extra>") %>%
      layout(xaxis=list(title="Número de casos"), yaxis=list(title=""), margin=list(l=10,r=10,b=40,t=10))
  })
  
  # --- Mapa TAB 1
  output$map_eta_e1 <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::setView(lng=-74.3, lat=4.6, zoom=5)
  })
  observe({
    titulo <- indic_lbl_e1()
    fmt_val <- function(x) if (input$f_indic_e1=="incid") scales::number(x, big.mark=",", accuracy=0.1) else scales::comma(x)
    if (nivel_e1()=="deptos") {
      shp <- dptos_sf %>%
        dplyr::left_join(agg_depto_e1(), by="COD_DPTO2") %>%
        dplyr::left_join(dpt_lookup_eta, by="COD_DPTO2") %>%
        dplyr::mutate(valor=tidyr::replace_na(valor,0),
                      etq=paste0("<b>", DEP_N, "</b><br>", titulo, ": ", fmt_val(valor)))
      pal <- make_pal(shp$valor, MAP_COLORS)
      leaflet::leafletProxy("map_eta_e1", data=shp) %>%
        leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId=~COD_DPTO2, fillColor=~pal(valor), color="#d9aee9", weight=0.7, fillOpacity=0.9,
                             label=~lapply(etq, htmltools::HTML),
                             highlightOptions=leaflet::highlightOptions(color="#d9aee9", weight=2, bringToFront=TRUE)) %>%
        leaflet::addLegend("bottomright", pal=pal, values=~valor, title=titulo)
    } else {
      sel_dep <- dpt_lookup_eta %>% dplyr::filter(DEP_N==input$f_depto_e1) %>% dplyr::pull(COD_DPTO2) %>% .[1]
      req(!is.na(sel_dep), nzchar(sel_dep))
      shp <- mpios_sf %>%
        dplyr::filter(COD_DPTO2==sel_dep) %>%
        dplyr::left_join(agg_mpio_e1() %>% dplyr::select(COD_MUN5, valor), by="COD_MUN5") %>%
        dplyr::left_join(mun_lookup_eta %>% dplyr::select(COD_MUN5, MUN_N), by="COD_MUN5") %>%
        dplyr::mutate(valor=tidyr::replace_na(valor,0), MUN_N=ifelse(is.na(MUN_N), "SIN DATO", MUN_N),
                      etq=paste0("<b>", MUN_N, "</b><br>", titulo, ": ", fmt_val(valor)))
      pal <- make_pal(shp$valor, MAP_COLORS)
      bb  <- sf::st_bbox(shp)
      leaflet::leafletProxy("map_eta_e1", data=shp) %>%
        leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId=~COD_MUN5, fillColor=~pal(valor), color="#d9aee9", weight=0.4, fillOpacity=0.9,
                             label=~lapply(etq, htmltools::HTML),
                             highlightOptions=leaflet::highlightOptions(color="#d9aee9", weight=2, bringToFront=TRUE)) %>%
        leaflet::addLegend("bottomright", pal=pal, values=~valor, title=titulo) %>%
        leaflet::fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  observeEvent(input$map_eta_e1_shape_click, {
    click <- input$map_eta_e1_shape_click; req(click$id)
    if (nivel_e1()=="deptos") {
      cod <- sprintf("%02d", as.integer(click$id))
      nom <- dpt_lookup_eta$DEP_N[dpt_lookup_eta$COD_DPTO2==cod][1]
      if (!is.na(nom) && nzchar(nom)) updateSelectInput(session, "f_depto_e1", selected=nom)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$btn_reset_e1, {
    updateSelectInput(session, "f_depto_e1", selected="Todos")
    updateSelectInput(session, "f_mpio_e1",  selected="Todos")
  })
  
  output$top_mpios_e1 <- renderPlotly({
    titulo <- indic_lbl_e1()
    df <- agg_mpio_e1() %>% dplyr::arrange(dplyr::desc(valor)) %>% dplyr::slice(1:12)
    plot_ly(df, x=~valor, y=~reorder(MUN_N, valor), type="bar", orientation="h",
            marker=list(color=BAR_COLOR),
            hovertemplate=paste0("%{y}<br>", titulo, ": %{x:,}<extra></extra>")) %>%
      layout(xaxis=list(title=titulo), yaxis=list(title=""), margin=list(l=10,r=10,b=40,t=10))
  })
  
  # ================= ETA — Análisis (e2) =================
  output$anio_e2_ui <- renderUI({
    yrs <- sort(unique(eta$ano))
    selectInput("f_anio_e2", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  observeEvent(input$f_depto_e2, {
    if (is.null(input$f_depto_e2) || input$f_depto_e2 == "Todos") {
      updateSelectInput(session, "f_mpio_e2", choices = "Todos", selected = "Todos")
    } else {
      sel_dep <- dpt_lookup_eta$COD_DPTO2[dpt_lookup_eta$DEP_N == input$f_depto_e2][1]
      mm <- mun_lookup_eta %>% dplyr::filter(COD_DPTO2 == sel_dep) %>% dplyr::arrange(MUN_N)
      updateSelectInput(session, "f_mpio_e2", choices = c("Todos", mm$MUN_N), selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  base_e2 <- reactive({
    req(input$f_anio_e2)
    df <- eta %>% dplyr::filter(ano == input$f_anio_e2)
    if (!is.null(input$f_depto_e2) && input$f_depto_e2 != "Todos") df <- df %>% dplyr::filter(DEP_N == input$f_depto_e2)
    if (!is.null(input$f_mpio_e2) && input$f_mpio_e2 != "Todos") df <- df %>% dplyr::filter(MUN_N == input$f_mpio_e2)
    df
  })
  
  # ---- Títulos storytelling (TAB 2) ----
  output$ttl_incid_bar_e2 <- renderText({
    amb <- if (is.null(input$f_depto_e2) || input$f_depto_e2 == "Todos") "Colombia (departamentos)" else paste0(input$f_depto_e2, " (municipios)")
    paste0("¿Dónde es mayor la incidencia (x100k) en ", amb, " durante ", input$f_anio_e2, "?")
  })
  output$ttl_genero_e2 <- renderText({
    paste0("¿Cómo se distribuyen los enfermos por género en ", scope_txt_e2(), " en ", input$f_anio_e2, "?")
  })
  output$ttl_exp_enf_map_e2 <- renderText({
    amb <- if (is.null(input$f_depto_e2) || input$f_depto_e2 == "Todos") "departamentos de Colombia" else paste0("municipios de ", input$f_depto_e2)
    paste0("¿Qué tan alta es la relación Enfermos/Expuestos en los ", amb, " — ", input$f_anio_e2, "?")
  })
  
  output$incid_bar_e2 <- renderPlotly({
    req(input$f_anio_e2)
    if (is.null(input$f_depto_e2) || input$f_depto_e2 == "Todos") {
      df_eta <- eta %>% dplyr::filter(ano == input$f_anio_e2)
      num <- df_eta %>% dplyr::group_by(COD_DPTO2) %>% dplyr::summarise(enfermos=sum(TOTAL_ENF,na.rm=TRUE), .groups="drop")
      den <- pob_depto %>% dplyr::filter(ano == input$f_anio_e2)
      d <- num %>% dplyr::left_join(den, by="COD_DPTO2") %>% dplyr::left_join(dpt_lookup_eta, by="COD_DPTO2") %>%
        dplyr::mutate(incidencia=ifelse(POB>0,(enfermos/POB)*1e5,NA_real_)) %>% dplyr::arrange(dplyr::desc(incidencia))
      plot_ly(d, x=~incidencia, y=~reorder(DEP_N, incidencia), type="bar", orientation="h",
              marker=list(color=BAR_COLOR),
              customdata=~data.frame(Enfermos=enfermos, Poblacion=POB),
              hovertemplate="%{y}<br>Incidencia: %{x:.1f}<br>Enfermos: %{customdata.Enfermos:,}<br>Población: %{customdata.Poblacion:,}<extra></extra>") %>%
        layout(xaxis=list(title="Incidencia por 100.000 hab.", tickformat=".1f"), yaxis=list(title=""), margin=list(l=10,r=10,b=40,t=10))
    } else {
      df_eta <- eta %>% dplyr::filter(ano==input$f_anio_e2, DEP_N==input$f_depto_e2)
      num <- df_eta %>% dplyr::group_by(COD_MUN5, MUN_N) %>% dplyr::summarise(enfermos=sum(TOTAL_ENF,na.rm=TRUE), .groups="drop")
      den <- pob_mpio %>% dplyr::filter(ano==input$f_anio_e2) %>% dplyr::select(COD_MUN5, POB)
      d <- num %>% dplyr::left_join(den, by="COD_MUN5") %>%
        dplyr::mutate(incidencia=ifelse(POB>0,(enfermos/POB)*1e5,NA_real_)) %>% dplyr::arrange(dplyr::desc(incidencia))
      plot_ly(d, x=~incidencia, y=~reorder(MUN_N, incidencia), type="bar", orientation="h",
              marker=list(color=BAR_COLOR),
              customdata=~data.frame(Enfermos=enfermos, Poblacion=POB),
              hovertemplate="%{y}<br>Incidencia: %{x:.1f}<br>Enfermos: %{customdata.Enfermos:,}<br>Población: %{customdata.Poblacion:,}<extra></extra>") %>%
        layout(xaxis=list(title="Incidencia por 100.000 hab.", tickformat=".1f"), yaxis=list(title=""), margin=list(l=10,r=10,b=40,t=10))
    }
  })
  
  output$plot_genero_e2 <- renderPlotly({
    d <- base_e2()
    tot_hom <- sum(coalesce(d$TOTAL_HOM, 0)); tot_muj <- sum(coalesce(d$TOTAL_MUJ, 0))
    vis_vals <- c(max(tot_hom, 1e-6), max(tot_muj, 1e-6))
    plot_ly(labels=c("Hombres","Mujeres"), values=vis_vals, type="pie",
            marker=list(colors=GEN_PIE),
            textinfo="label+percent", insidetextorientation="radial",
            hovertext=paste0(c("Hombres","Mujeres"), ": ", scales::comma(c(tot_hom, tot_muj))), hoverinfo="text") %>%
      layout(showlegend=TRUE, margin=list(l=10,r=10,b=40,t=40))
  })
  
  # ----- Mapa Enfermos/Expuestos (e2) -----
  agg_exp_enf_depto_e2 <- reactive({
    req(input$f_anio_e2)
    eta %>% dplyr::filter(ano==input$f_anio_e2) %>% dplyr::group_by(COD_DPTO2) %>%
      dplyr::summarise(enf=sum(coalesce(TOTAL_ENF,0),na.rm=TRUE), exp=sum(coalesce(TOTAL_EXP,0),na.rm=TRUE), .groups="drop") %>%
      dplyr::mutate(ratio=dplyr::if_else(exp>0, (enf/exp)*100, NA_real_))
  })
  agg_exp_enf_mpio_e2 <- reactive({
    req(input$f_anio_e2)
    req(!is.null(input$f_depto_e2), input$f_depto_e2 != "Todos")
    eta %>% dplyr::filter(ano==input$f_anio_e2, DEP_N==input$f_depto_e2) %>% dplyr::group_by(COD_MUN5, MUN_N) %>%
      dplyr::summarise(enf=sum(coalesce(TOTAL_ENF,0),na.rm=TRUE), exp=sum(coalesce(TOTAL_EXP,0),na.rm=TRUE), .groups="drop") %>%
      dplyr::mutate(ratio=dplyr::if_else(exp>0, (enf/exp)*100, NA_real_))
  })
  quintile_bins <- function(x){
    v <- x[is.finite(x)]; if (!length(v)) return(c(0,1,2,3,4,5))
    qs <- as.numeric(quantile(v, probs=seq(0,1,length.out=6), na.rm=TRUE, type=7))
    qs <- unique(qs); if (length(qs)<3) qs <- pretty(v, n=5); sort(unique(qs))
  }
  output$map_exp_enf_e2 <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::setView(lng=-74.3, lat=4.6, zoom=5)
  })
  observe({
    fmt_num <- function(x) scales::comma(x)
    if (is.null(input$f_depto_e2) || input$f_depto_e2=="Todos") {
      dd <- agg_exp_enf_depto_e2()
      bins <- quintile_bins(dd$ratio)
      pal  <- leaflet::colorBin(MAP_COLORS, domain=dd$ratio, bins=bins, na.color="#f0f0f0")
      shp <- dptos_sf %>% dplyr::left_join(dd, by="COD_DPTO2") %>% dplyr::left_join(dpt_lookup_eta, by="COD_DPTO2") %>%
        dplyr::mutate(etq=paste0("<b>", DEP_N, "</b><br>Enf/Exp: ", ifelse(is.na(ratio),"s/d",sprintf("%.1f%%",ratio)),
                                 "<br>Enfermos: ", fmt_num(enf), " | Expuestos: ", fmt_num(exp)))
      leaflet::leafletProxy("map_exp_enf_e2", data=shp) %>%
        leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId=~COD_DPTO2, fillColor=~pal(ratio), color="#d9aee9", weight=0.7, fillOpacity=0.9,
                             label=~lapply(etq, htmltools::HTML),
                             highlightOptions=leaflet::highlightOptions(color="#d9aee9", weight=2, bringToFront=TRUE)) %>%
        leaflet::addLegend("bottomright", pal=pal, values=dd$ratio,
                           title="Enfermos / Expuestos (%)",
                           labFormat=leaflet::labelFormat(suffix="%"))
    } else {
      sel_dep <- dpt_lookup_eta %>% dplyr::filter(DEP_N==input$f_depto_e2) %>% dplyr::pull(COD_DPTO2) %>% .[1]
      req(!is.na(sel_dep), nzchar(sel_dep))
      dd <- agg_exp_enf_mpio_e2()
      bins <- quintile_bins(dd$ratio)
      pal  <- leaflet::colorBin(MAP_COLORS, domain=dd$ratio, bins=bins, na.color="#f0f0f0")
      shp <- mpios_sf %>% dplyr::filter(COD_DPTO2==sel_dep) %>%
        dplyr::left_join(dd %>% dplyr::select(COD_MUN5, MUN_N, enf, exp, ratio), by="COD_MUN5") %>%
        dplyr::mutate(MUN_N=ifelse(is.na(MUN_N), MUNICIPIO_N, MUN_N),
                      etq=paste0("<b>", MUN_N, "</b><br>Enf/Exp: ", ifelse(is.na(ratio),"s/d",sprintf("%.1f%%",ratio)),
                                 "<br>Enfermos: ", fmt_num(coalesce(enf,0)), " | Expuestos: ", fmt_num(coalesce(exp,0))))
      bb <- sf::st_bbox(shp)
      leaflet::leafletProxy("map_exp_enf_e2", data=shp) %>%
        leaflet::clearShapes() %>% leaflet::clearControls() %>%
        leaflet::addPolygons(layerId=~COD_MUN5, fillColor=~pal(ratio), color="#d9aee9", weight=0.4, fillOpacity=0.9,
                             label=~lapply(etq, htmltools::HTML),
                             highlightOptions=leaflet::highlightOptions(color="#d9aee9", weight=2, bringToFront=TRUE)) %>%
        leaflet::addLegend("bottomright", pal=pal, values=dd$ratio,
                           title="Enfermos / Expuestos (%)",
                           labFormat=leaflet::labelFormat(suffix="%")) %>%
        leaflet::fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
}

shinyApp(ui, server)

