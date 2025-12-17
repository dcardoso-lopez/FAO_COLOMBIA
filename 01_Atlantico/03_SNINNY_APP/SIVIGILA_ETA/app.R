# app_eta.R
# =========================================================
# ETA — Dashboard (app exclusiva)
# Vista única: Exploración ETA
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

# ---- Alias seguros (evita choque con plotly::validate) ----
validate <- shiny::validate
need     <- shiny::need

# ---------- Rutas ----------
local_data_dir <- "data"
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
check_shp_parts <- function(shp){
  base <- sub("\\.shp$", "", shp)
  req  <- paste0(base, c(".shp",".dbf",".shx",".prj"))
  req[!file.exists(req)]
}
miss_shp <- c(check_shp_parts(ruta_shp_mpios), check_shp_parts(ruta_shp_dptos))
if (length(miss_shp)) stop("Faltan componentes de shapefile:\n", paste("-", miss_shp, collapse = "\n"))

# ---------- Helper: Title Case español ----------
title_case_es <- function(x){
  palabras_minas <- c("de","del","la","las","los","y","o","u","en","el","por","para","a","e")
  vapply(x, function(txt){
    if (is.na(txt)) return(NA_character_)
    txt <- stringi::stri_trim_both(txt)
    if (txt == "") return("")
    txt <- stringi::stri_trans_tolower(txt)
    palabras <- unlist(strsplit(txt, "\\s+"))
    palabras <- palabras[palabras != ""]
    if (!length(palabras)) return("")
    palabras <- mapply(function(w, i){
      if (i > 1 && w %in% palabras_minas) {
        w
      } else {
        first <- stringi::stri_sub(w, 1, 1)
        rest  <- if (stringi::stri_length(w) > 1) stringi::stri_sub(w, 2) else ""
        paste0(stringi::stri_trans_toupper(first), rest)
      }
    }, palabras, seq_along(palabras), USE.NAMES = FALSE)
    paste(palabras, collapse = " ")
  }, FUN.VALUE = character(1))
}

# ---------- 1) Leer ETA y normalizar ----------
eta_raw <- readRDS(eta_path) %>% dplyr::filter(!is.na(MUNICIPIO_D))
year_col     <- if ("ano" %in% names(eta_raw)) "ano" else if ("ANO" %in% names(eta_raw)) "ANO" else stop("No encuentro 'ano'/'ANO'")
mun_code_col <- if ("COD_DANE_MUNIC_D" %in% names(eta_raw)) "COD_DANE_MUNIC_D" else stop("No encuentro 'COD_DANE_MUNIC_D'")
dep_name_col <- if ("DEPARTAMENTO_D" %in% names(eta_raw)) "DEPARTAMENTO_D" else if ("DEPARTMENTO_D" %in% names(eta_raw)) "DEPARTMENTO_D" else "DEPARTAMENTO_D"
mun_name_col <- if ("MUNICIPIO_D" %in% names(eta_raw)) "MUNICIPIO_D" else "MUNICIPIO_D"

tenf_col_candidates <- c("TOTAL_ENF","total_enf","TOTALENF","Total_enf","total_enfermos","TOTAL_ENFERMOS")
tenf_col <- { x <- tenf_col_candidates[tenf_col_candidates %in% names(eta_raw)]; if(length(x)) x[1] else NA_character_ }

texp_col_candidates <- c("TOTAL_EXP","total_exp","TOTALEXP","Total_exp","total_expuestos","TOTAL_EXPUES")
texp_col <- { x <- texp_col_candidates[texp_col_candidates %in% names(eta_raw)]; if(length(x)) x[1] else NA_character_ }

origen_cols <- intersect(c("agua","alimentos","pers_pers","cont_ambie","otro","desconocid"), names(eta_raw))
hom_col <- if ("total_hom" %in% names(eta_raw)) "total_hom" else NA_character_
muj_col <- if ("total_muj" %in% names(eta_raw)) "total_muj" else NA_character_

eta <- eta_raw %>%
  dplyr::transmute(
    ano       = suppressWarnings(as.integer(.data[[year_col]])),
    COD_MUN5  = sprintf("%05d", suppressWarnings(as.integer(.data[[mun_code_col]]))),
    COD_DPTO2 = substr(COD_MUN5, 1, 2),
    DEP_N     = title_case_es(trimws(as.character(.data[[dep_name_col]]))),
    MUN_N     = title_case_es(trimws(as.character(.data[[mun_name_col]]))),
    TOTAL_ENF = if (!is.na(tenf_col)) suppressWarnings(as.numeric(.data[[tenf_col]])) else NA_real_,
    TOTAL_EXP = if (!is.na(texp_col)) suppressWarnings(as.numeric(.data[[texp_col]])) else NA_real_,
    TOTAL_HOM = if (!is.na(hom_col))  suppressWarnings(as.numeric(.data[[hom_col]])) else NA_real_,
    TOTAL_MUJ = if (!is.na(muj_col))  suppressWarnings(as.numeric(.data[[muj_col]])) else NA_real_
  ) %>%
  dplyr::mutate(
    TOTAL_NR = pmax(coalesce(TOTAL_ENF, 0) - (coalesce(TOTAL_HOM, 0) + coalesce(TOTAL_MUJ, 0)), 0)
  ) %>%
  dplyr::filter(!is.na(ano), !is.na(COD_MUN5), !is.na(COD_DPTO2))

if (length(origen_cols) > 0) {
  eta <- dplyr::bind_cols(eta, eta_raw %>% dplyr::select(dplyr::all_of(origen_cols))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(origen_cols), ~ ifelse(as.integer(.x) == 1, 1L, 0L)))
}

# ---------- 1b) Población ----------
pob_raw <- readRDS(ruta_pob)
pob_year_col <- dplyr::case_when(
  "ano"  %in% names(pob_raw) ~ "ano",
  "ANO"  %in% names(pob_raw) ~ "ANO",
  "year" %in% names(pob_raw) ~ "year",
  TRUE ~ NA_character_
)
stopifnot(!is.na(pob_year_col))

pob_mun_code_col <- dplyr::case_when(
  "COD_MUN5"          %in% names(pob_raw) ~ "COD_MUN5",
  "COD_DANE_MUNIC_D"  %in% names(pob_raw) ~ "COD_DANE_MUNIC_D",
  "COD_MPIO"          %in% names(pob_raw) ~ "COD_MPIO",
  "MPIO_CDPMP"        %in% names(pob_raw) ~ "MPIO_CDPMP",
  TRUE ~ NA_character_
)
stopifnot(!is.na(pob_mun_code_col))

pob_val_col <- dplyr::case_when(
  "P_TOTAL"   %in% names(pob_raw) ~ "P_TOTAL",
  "Poblacion" %in% names(pob_raw) ~ "Poblacion",
  "POBLACION" %in% names(pob_raw) ~ "POBLACION",
  "total"     %in% names(pob_raw) ~ "total",
  "poblacion" %in% names(pob_raw) ~ "poblacion",
  TRUE ~ NA_character_
)
stopifnot(!is.na(pob_val_col))

pob_norm <- pob_raw %>%
  dplyr::transmute(
    ano       = suppressWarnings(as.integer(.data[[pob_year_col]])),
    COD_MUN5  = sprintf("%05d", suppressWarnings(as.integer(.data[[pob_mun_code_col]]))),
    COD_DPTO2 = substr(COD_MUN5, 1, 2),
    POB       = suppressWarnings(as.numeric(.data[[pob_val_col]]))
  ) %>%
  dplyr::filter(!is.na(ano), !is.na(COD_MUN5), !is.na(COD_DPTO2), is.finite(POB))

pob_mpio  <- pob_norm %>%
  dplyr::group_by(ano, COD_MUN5, COD_DPTO2) %>%
  dplyr::summarise(POB = sum(POB, na.rm = TRUE), .groups = "drop")

pob_depto <- pob_norm %>%
  dplyr::group_by(ano, COD_DPTO2) %>%
  dplyr::summarise(POB = sum(POB, na.rm = TRUE), .groups = "drop")

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
  ) %>%
  dplyr::mutate(MUNICIPIO_N = title_case_es(MUNICIPIO_N)) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid()

dptos_sf <- dptos_raw %>%
  dplyr::mutate(
    COD_DPTO2 = if ("DPTO_CCDGO" %in% names(.)) sprintf("%02d", as.integer(DPTO_CCDGO))
    else if ("COD_DEPTO" %in% names(.)) sprintf("%02d", as.integer(COD_DEPTO))
    else stop("Shp deptos: falta DPTO_CCDGO/COD_DEPTO"),
    DEPARTAMENTO_N = if ("DEPARTAMENTO_D" %in% names(.)) as.character(DEPARTAMENTO_D)
    else if ("DPTO_CNMBR" %in% names(.)) as.character(DPTO_CNMBR)
    else if ("NOMBRE_DEPTO" %in% names(.)) as.character(NOMBRE_DEPTO)
    else COD_DPTO2
  ) %>%
  dplyr::mutate(DEPARTAMENTO_N = title_case_es(DEPARTAMENTO_N)) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid()

# ---------- Lookups (ETA) ----------
dpt_lookup_eta <- eta %>%
  dplyr::select(COD_DPTO2, DEP_N) %>%
  dplyr::mutate(
    COD_DPTO2 = sprintf("%02d", as.integer(COD_DPTO2)),
    DEP_N     = title_case_es(trimws(DEP_N))
  ) %>%
  dplyr::distinct() %>%
  dplyr::arrange(DEP_N)

mun_lookup_eta <- eta %>%
  dplyr::select(COD_DPTO2, COD_MUN5, MUN_N) %>%
  dplyr::mutate(
    COD_DPTO2  = sprintf("%02d", as.integer(COD_DPTO2)),
    COD_MUN5   = sprintf("%05d", as.integer(COD_MUN5)),
    MUN_N      = trimws(as.character(MUN_N)),
    MUN_N_TC   = title_case_es(MUN_N)
  ) %>%
  dplyr::distinct()

# === Choices en Title Case para filtros (valores = códigos) ===
dept_choices <- c(
  "Todos",
  stats::setNames(dpt_lookup_eta$COD_DPTO2, dpt_lookup_eta$DEP_N)
)

# *** Código por defecto para Santander ***
SANTANDER_CODE <- {
  idx <- which(dpt_lookup_eta$DEP_N == "Atlántico")
  if (length(idx) > 0) {
    dpt_lookup_eta$COD_DPTO2[idx[1]]
  } else if (nrow(dpt_lookup_eta) > 0) {
    dpt_lookup_eta$COD_DPTO2[1]
  } else {
    "Todos"
  }
}

# ---------- Paletas/colores ----------
MAP_COLORS <- c("#f4e6f9","#d9aee9","#b46cd2","#8e44ad","#602070")  # choropleths
BAR_COLOR  <- "#8e44ad"        # barras

# ---------- Helpers de cuartiles ----------
compute_breaks_quartiles <- function(values){
  vals <- suppressWarnings(as.numeric(values))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(c(0, 1))
  
  # Si todo es cero, no hay cuartiles útiles
  if (all(vals == 0, na.rm = TRUE)) {
    return(c(0, 1))
  }
  
  # Cuartiles solo sobre valores positivos
  pos <- vals[vals > 0]
  if (!length(pos)) return(c(0, 1))
  
  qs <- stats::quantile(pos, probs = c(0.25, 0.50, 0.75, 1), na.rm = TRUE)
  brks <- c(0, as.numeric(qs))  # 0, Q25, Q50, Q75, Q100
  brks
}

format_interval_label <- function(a, b, as_percent = FALSE, is_first = TRUE){
  if (as_percent) {
    fa <- scales::number(a, accuracy = 0.1, decimal.mark = ",")
    fb <- scales::number(b, accuracy = 0.1, decimal.mark = ",")
    fa <- paste0(fa, "%")
    fb <- paste0(fb, "%")
  } else {
    fa <- scales::number(a, accuracy = 1, big.mark = ".", decimal.mark = ",")
    fb <- scales::number(b, accuracy = 1, big.mark = ".", decimal.mark = ",")
  }
  if (is_first) {
    sprintf("%s – %s", fa, fb)
  } else {
    sprintf(">%s – %s", fa, fb)
  }
}

build_interval_labels <- function(breaks, as_percent = FALSE){
  if (length(breaks) < 2) return(character(0))
  vapply(
    seq_len(length(breaks) - 1),
    function(i){
      format_interval_label(
        a          = breaks[i],
        b          = breaks[i + 1],
        as_percent = as_percent,
        is_first   = (i == 1)
      )
    },
    character(1)
  )
}

# ---------- 3) UI — Vista única ----------
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    primary = "#2563eb",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius" = "0.9rem",
    "font-size-base" = "0.98rem"
  ),
  tags$head(tags$style(HTML("
    :root{ --border-col:#f57c00; --accent:#8e44ad; }
    .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
    h3{font-weight:700;letter-spacing:.2px;margin-bottom:16px}
    .filters{
      background:#fff;border:1.5px solid var(--border-col);
      border-radius:16px;padding:14px 16px;margin-bottom:16px;
      box-shadow:0 2px 10px rgba(0,0,0,.04);
    }
    .filters-grid{display:grid;grid-template-columns:repeat(4,minmax(220px,1fr));gap:12px}
    .filter-label{
      font-size:14px;
      font-weight:500;
      letter-spacing:.2px;
      color:#111827;
      margin-bottom:6px;
      font-family:'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
    }
    .selectize-input,.form-control{
      min-height:42px;border-radius:10px;border:1.5px solid var(--border-col);
      box-shadow:none !important;
    }
    .selectize-input:focus,.form-control:focus{
      border-color:var(--border-col) !important; outline:0 !important;
      box-shadow:0 0 0 .15rem rgba(245,124,0,.35) !important;
    }
    .card{
      background:#fff;border:1.5px solid var(--border-col);
      border-radius:16px;padding:12px;box-shadow:0 2px 10px rgba(0,0,0,.05);margin-bottom:12px
    }
    .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827;display:flex;align-items:center;gap:4px;}
    .map-note{
      margin-top:6px;
      font-size:12px;
      color:#6b7280;
    }
  "))),
  div(class="wrap",
      h3(""),
      br(),
      # ---- Filtros ----
      div(class="filters",
          div(class="filters-grid",
              div(class="filter",
                  div(class="filter-label","¿Qué año analizamos?"),
                  uiOutput("anio_e1_ui")
              ),
              div(class="filter",
                  div(class="filter-label","¿En qué departamento?"),
                  selectInput(
                    "f_depto_e1", NULL,
                    choices  = dept_choices,
                    selected = SANTANDER_CODE   # <-- Santander por defecto
                  )
              ),
              div(class="filter",
                  div(class="filter-label","¿Algún municipio en particular?"),
                  selectInput(
                    "f_mpio_e1", NULL,
                    choices  = "Todos",
                    selected = "Todos"
                  )
              ),
              div(class="filter",
                  div(class="filter-label","Variable a considerar"),
                  selectInput(
                    "f_indic_e1", NULL,
                    choices = c(
                      "Total de enfermos"   = "total_enf",
                      "Incidencia (x100k)"  = "incid"
                    ),
                    selected = "total_enf"
                  )
              )
          )
      ),
      # ---- Contenido principal ----
      fluidRow(
        column(6,
               div(class="card",
                   div(class="card-title",
                       span(textOutput("ttl_mapa_e1")),
                       span(style="margin-left:auto;",
                            actionLink("btn_reset_e1","← Volver a Santander")
                       )
                   ),
                   leafletOutput("map_eta_e1", height = 660),
                   div(class = "map-note",
                       textOutput("nota_mapa_e1")
                   )
               )
        ),
        column(6,
               fluidRow(
                 column(12,
                        div(class="card",
                            div(class="card-title", textOutput("ttl_origen_e1")),
                            plotlyOutput("plot_origen_e1", height = 315)
                        )
                 )
               ),
               fluidRow(
                 column(12,
                        div(class="card",
                            div(class="card-title", textOutput("ttl_top_e1")),
                            plotlyOutput("top_mpios_e1", height = 318)
                        )
                 )
               )
        )
      )
  )
)

# ---------- 4) SERVER ----------
server <- function(input, output, session){
  
  # Helpers storytelling --------------------------------------------------
  scope_txt <- reactive({
    if (is.null(input$f_depto_e1) || input$f_depto_e1 == "Todos") {
      "Colombia"
    } else {
      dep_nom <- dpt_lookup_eta$DEP_N[dpt_lookup_eta$COD_DPTO2 == input$f_depto_e1][1]
      if (!is.null(input$f_mpio_e1) && input$f_mpio_e1 != "Todos") {
        mun_nom <- mun_lookup_eta$MUN_N_TC[mun_lookup_eta$COD_MUN5 == input$f_mpio_e1][1]
        paste(mun_nom, ",", dep_nom)
      } else {
        dep_nom
      }
    }
  })
  
  indic_lbl_e1 <- reactive({
    if (input$f_indic_e1=="total_enf") "Enfermos"
    else "Incidencia (x100k)"
  })
  
  # ================= ETA — Exploración (e1) =================
  output$anio_e1_ui <- renderUI({
    yrs <- sort(unique(eta$ano))
    selectInput("f_anio_e1", NULL, choices = yrs,
                selected = max(yrs, na.rm = TRUE))
  })
  
  # Actualizar municipios cuando cambia el departamento
  observeEvent(input$f_depto_e1, {
    if (is.null(input$f_depto_e1) || input$f_depto_e1 == "Todos") {
      updateSelectInput(session, "f_mpio_e1",
                        choices = "Todos", selected = "Todos")
    } else {
      sel_dep <- input$f_depto_e1   # código DANE del depto
      mm <- mun_lookup_eta %>%
        dplyr::filter(COD_DPTO2 == sel_dep) %>%
        dplyr::arrange(MUN_N_TC)
      updateSelectInput(
        session, "f_mpio_e1",
        choices  = c("Todos", stats::setNames(mm$COD_MUN5, mm$MUN_N_TC)),
        selected = "Todos"
      )
    }
  }, ignoreInit = TRUE)
  
  # Nivel de visualización
  nivel_e1 <- reactive({
    if (is.null(input$f_depto_e1) || input$f_depto_e1 == "Todos") "deptos" else "mpios"
  })
  
  # Base filtrada
  base_e1 <- reactive({
    req(input$f_anio_e1)
    df <- eta %>% dplyr::filter(ano == input$f_anio_e1)
    
    # filtrar por departamento (código)
    if (!is.null(input$f_depto_e1) && input$f_depto_e1 != "Todos") {
      df <- df %>% dplyr::filter(COD_DPTO2 == input$f_depto_e1)
    }
    
    # filtrar por municipio (código)
    if (!is.null(input$f_mpio_e1) && input$f_mpio_e1 != "Todos") {
      df <- df %>% dplyr::filter(COD_MUN5 == input$f_mpio_e1)
    }
    df
  })
  
  agg_depto_e1 <- reactive({
    df <- base_e1()
    if (input$f_indic_e1 == "total_enf") {
      df %>%
        dplyr::group_by(COD_DPTO2) %>%
        dplyr::summarise(valor = sum(TOTAL_ENF, na.rm=TRUE), .groups="drop")
    } else {
      sum_enf <- df %>%
        dplyr::group_by(COD_DPTO2) %>%
        dplyr::summarise(total_enf = sum(TOTAL_ENF, na.rm=TRUE), .groups="drop")
      sum_enf %>%
        dplyr::left_join(pob_depto %>% dplyr::filter(ano == input$f_anio_e1),
                         by="COD_DPTO2") %>%
        dplyr::mutate(
          valor = ifelse(POB > 0, (total_enf/POB)*1e5, NA_real_)
        ) %>%
        dplyr::select(COD_DPTO2, valor)
    }
  })
  
  agg_mpio_e1 <- reactive({
    df <- base_e1()
    if (input$f_indic_e1 == "total_enf") {
      df %>%
        dplyr::group_by(COD_DPTO2, COD_MUN5, MUN_N) %>%
        dplyr::summarise(valor = sum(TOTAL_ENF, na.rm=TRUE), .groups="drop")
    } else {
      sum_enf <- df %>%
        dplyr::group_by(COD_DPTO2, COD_MUN5, MUN_N) %>%
        dplyr::summarise(total_enf = sum(TOTAL_ENF, na.rm=TRUE), .groups="drop")
      sum_enf %>%
        dplyr::left_join(
          pob_mpio %>%
            dplyr::filter(ano == input$f_anio_e1) %>%
            dplyr::select(COD_MUN5, POB),
          by="COD_MUN5"
        ) %>%
        dplyr::mutate(
          valor = ifelse(POB > 0, (total_enf/POB)*1e5, NA_real_)
        ) %>%
        dplyr::select(COD_DPTO2, COD_MUN5, MUN_N, valor)
    }
  })
  
  # ---- Títulos storytelling ----
  output$ttl_mapa_e1 <- renderText({
    amb <- scope_txt()
    ind <- tolower(indic_lbl_e1())
    paste0("¿En qué territorios de ", amb, " está la mayor cantidad de ", ind, "?")
  })
  
  output$ttl_origen_e1 <- renderText({
    paste0("¿Cuál fue la principal fuente de origen de Enfermedades Transmitidas por Alimentos (ETA)?")
  })
  
  output$ttl_top_e1 <- renderText({
    paste0("¿Qué municipios tienen mayor cantidad de ",
           tolower(indic_lbl_e1()), "?")
  })
  
  output$ttl_origen_tot_e1 <- renderText({
    if (!length(origen_cols)) return("Sin datos de origen")
    d <- base_e1() %>%
      dplyr::select(dplyr::all_of(origen_cols), TOTAL_ENF) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(origen_cols),
        names_to = "Origen", values_to = "flag"
      ) %>%
      dplyr::filter(flag == 1) %>%
      dplyr::group_by(Origen) %>%
      dplyr::summarise(
        Casos    = dplyr::n(),
        Enfermos = sum(TOTAL_ENF, na.rm=TRUE),
        .groups  = "drop"
      )
    paste0(
      "Casos totales: ",
      scales::number(sum(d$Casos, na.rm=TRUE), big.mark=".", decimal.mark=",", accuracy = 1),
      "  |  Total de enfermos: ",
      scales::number(sum(d$Enfermos, na.rm=TRUE), big.mark=".", decimal.mark=",", accuracy = 1)
    )
  })
  
  # ---- Gráfico origen (ENFERMOS como métrica principal) ----
  output$plot_origen_e1 <- renderPlotly({
    if (!length(origen_cols)) return(NULL)
    
    d <- base_e1() %>%
      dplyr::select(dplyr::all_of(origen_cols), TOTAL_ENF) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(origen_cols),
        names_to = "Origen", values_to = "flag"
      ) %>%
      dplyr::filter(flag == 1) %>%
      dplyr::group_by(Origen) %>%
      dplyr::summarise(
        Casos    = dplyr::n(),
        Enfermos = sum(TOTAL_ENF, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(
        Origen = dplyr::recode(
          Origen,
          "agua"       = "Agua",
          "alimentos"  = "Alimentos",
          "pers_pers"  = "Persona a persona",
          "cont_ambie" = "Contaminación ambiental",
          "otro"       = "Otro",
          "desconocid" = "Desconocido",
          .default     = Origen
        ),
        Casos_fmt    = scales::number(Casos,    big.mark=".", decimal.mark=",", accuracy = 1),
        Enfermos_fmt = scales::number(Enfermos, big.mark=".", decimal.mark=",", accuracy = 1),
        hover_lab    = paste0(
          Origen,
          "<br>Enfermos: ", Enfermos_fmt,
          "<br>Casos: ", Casos_fmt
        )
      )
    
    axis_title <- "Número de enfermos"
    
    plot_ly(
      d, x = ~Enfermos, y = ~reorder(Origen, Enfermos),
      type      = "bar", orientation = "h",
      marker    = list(color = BAR_COLOR),
      text      = ~Enfermos_fmt,
      textposition     = "inside",
      insidetextanchor = "middle",
      textfont         = list(color = "white"),
      hovertext        = ~hover_lab,
      hoverinfo        = "text"
    ) %>%
      layout(
        xaxis  = list(title = axis_title),
        yaxis  = list(title = ""),
        margin = list(l=10,r=40,b=40,t=10)
      )
  })
  
  # --- Mapa ---
  output$map_eta_e1 <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::setView(lng=-74.3, lat=4.6, zoom=5)
  })
  
  output$nota_mapa_e1 <- renderText({
    df_vals <- if (nivel_e1() == "deptos") agg_depto_e1() else agg_mpio_e1()
    if (is.null(df_vals) || nrow(df_vals) == 0) {
      "Nota: Sin información suficiente para segmentar el indicador en cuartiles."
    } else {
      "Nota: Los colores del mapa representan cuartiles del indicador seleccionado para esta vista (rangos redondeados)."
    }
  })
  
  observe({
    titulo <- indic_lbl_e1()
    fmt_val <- function(x){
      if (input$f_indic_e1 == "incid") {
        scales::number(x, big.mark=".", decimal.mark=",", accuracy=0.1)
      } else {
        scales::number(x, big.mark=".", decimal.mark=",", accuracy=1)
      }
    }
    
    if (nivel_e1() == "deptos") {
      shp <- dptos_sf %>%
        dplyr::left_join(agg_depto_e1(), by="COD_DPTO2") %>%
        dplyr::left_join(dpt_lookup_eta, by="COD_DPTO2") %>%
        dplyr::mutate(
          valor = tidyr::replace_na(valor, 0),
          etq   = paste0("<b>", DEP_N, "</b><br>",
                         titulo, ": ", fmt_val(valor))
        )
      vals <- shp$valor
      brks <- compute_breaks_quartiles(vals)
      pal  <- leaflet::colorBin(MAP_COLORS, domain = vals, bins = brks, na.color = "#f0f0f0")
      labels_legend <- build_interval_labels(brks, as_percent = FALSE)
      mids          <- (brks[-length(brks)] + brks[-1]) / 2
      cols_legend   <- pal(mids)
      
      leaflet::leafletProxy("map_eta_e1", data=shp) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addPolygons(
          layerId  = ~COD_DPTO2,
          fillColor= ~pal(valor),
          color    = "#f57c00", weight = 0.7, fillOpacity = 0.9,
          label    = ~lapply(etq, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(
            color="#f57c00", weight=2, bringToFront=TRUE
          )
        ) %>%
        leaflet::addLegend(
          "bottomright",
          colors = cols_legend,
          labels = labels_legend,
          opacity = 0.9,
          title   = titulo
        )
    } else {
      sel_dep <- input$f_depto_e1
      req(!is.na(sel_dep), nzchar(sel_dep))
      
      shp <- mpios_sf %>%
        dplyr::filter(COD_DPTO2 == sel_dep) %>%
        dplyr::left_join(
          agg_mpio_e1() %>% dplyr::select(COD_MUN5, valor),
          by="COD_MUN5"
        ) %>%
        dplyr::left_join(
          mun_lookup_eta %>% dplyr::select(COD_MUN5, MUN_N),
          by="COD_MUN5"
        ) %>%
        dplyr::mutate(
          valor = tidyr::replace_na(valor, 0),
          MUN_N = ifelse(is.na(MUN_N), "Sin dato", MUN_N),
          etq   = paste0("<b>", MUN_N, "</b><br>",
                         titulo, ": ", fmt_val(valor))
        )
      
      vals <- shp$valor
      brks <- compute_breaks_quartiles(vals)
      pal  <- leaflet::colorBin(MAP_COLORS, domain = vals, bins = brks, na.color = "#f0f0f0")
      labels_legend <- build_interval_labels(brks, as_percent = FALSE)
      mids          <- (brks[-length(brks)] + brks[-1]) / 2
      cols_legend   <- pal(mids)
      bb  <- sf::st_bbox(shp)
      
      leaflet::leafletProxy("map_eta_e1", data=shp) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addPolygons(
          layerId  = ~COD_MUN5,
          fillColor= ~pal(valor),
          color    = "#f57c00", weight = 0.4, fillOpacity = 0.9,
          label    = ~lapply(etq, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(
            color="#f57c00", weight=2, bringToFront=TRUE
          )
        ) %>%
        leaflet::addLegend(
          "bottomright",
          colors = cols_legend,
          labels = labels_legend,
          opacity = 0.9,
          title   = titulo
        ) %>%
        leaflet::fitBounds(
          bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]
        )
    }
  })
  
  # Click en mapa: seleccionar departamento/municipio
  observeEvent(input$map_eta_e1_shape_click, {
    click <- input$map_eta_e1_shape_click
    req(click$id)
    if (nivel_e1() == "deptos") {
      cod <- sprintf("%02d", as.integer(click$id))
      updateSelectInput(session, "f_depto_e1", selected = cod)
      updateSelectInput(session, "f_mpio_e1",  selected = "Todos")
    } else {
      cod_mun <- sprintf("%05d", as.integer(click$id))
      updateSelectInput(session, "f_mpio_e1", selected = cod_mun)
    }
  }, ignoreInit = TRUE)
  
  # Reset → volver a Santander
  observeEvent(input$btn_reset_e1, {
    updateSelectInput(session, "f_depto_e1", selected = SANTANDER_CODE)
    updateSelectInput(session, "f_mpio_e1",  selected = "Todos")
  })
  
  # ---- Top municipios ----
  output$top_mpios_e1 <- renderPlotly({
    titulo <- indic_lbl_e1()
    acc    <- if (input$f_indic_e1 == "incid") 0.1 else 1
    
    df <- agg_mpio_e1() %>%
      dplyr::arrange(dplyr::desc(valor)) %>%
      dplyr::slice(1:12) %>%
      dplyr::mutate(
        valor_fmt = scales::number(valor, big.mark=".", decimal.mark=",", accuracy = acc),
        hover_lab = paste0(MUN_N, "<br>", titulo, ": ", valor_fmt)
      )
    
    axis_title <- if (input$f_indic_e1 == "incid") {
      "Incidencia (casos por 100.000 habitantes)"
    } else {
      "Número de enfermos"
    }
    
    plot_ly(
      df, x = ~valor, y = ~reorder(MUN_N, valor),
      type      = "bar", orientation = "h",
      marker    = list(color = BAR_COLOR),
      text      = ~valor_fmt,
      textposition     = "inside",
      insidetextanchor = "middle",
      textfont         = list(color = "white"),
      hovertext        = ~hover_lab,
      hoverinfo        = "text"
    ) %>%
      layout(
        xaxis  = list(title = axis_title),
        yaxis  = list(title = ""),
        margin = list(l=10,r=40,b=40,t=10)
      )
  })
}

shinyApp(ui, server)
