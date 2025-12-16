# app_nda.R
# =========================================================
# NDA — Dashboard (app exclusiva)
# - Tab 1: Exploración (mapa, top-10, serie, sexo)  → por ocurrencia
# - Tab 2: Resultados clínicos y supervivencia      → por ocurrencia
# =========================================================

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(leaflet); library(sf); library(dplyr); library(tidyr)
  library(scales); library(htmltools); library(DT); library(plotly)
  library(stringi)
})

options(stringsAsFactors = FALSE, OutDec = ",")
sf::sf_use_s2(FALSE)
options(shiny.maxRequestSize = 100*1024^2)

# ---------- Colores globales ----------
MAP_COLORS <- c("#ffe0cc", "#fa8916", "#fa8916", "#e6550d", "#9c4a00")

BAR_COLOR  <- "#f57c00"
BORDER_UI  <- "#ffb366"
SEX_COLORS <- c("Hombres"="#f57c00", "Mujeres"="#0a83ff", "Sin dato"="#cbd5e1")

GRID_COLOR <- "rgba(148,163,184,0.35)"  # gris suave para líneas horizontales

# ---------- Utils ----------
norm_txt <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
NUP      <- function(x) toupper(norm_txt(x))

title_case_es <- function(x){
  x <- trimws(as.character(x))
  is_na <- is.na(x) | x == ""
  if (all(is_na)) return(x)
  
  small_words <- c(
    "de","del","y","e","o","u",
    "la","las","el","los",
    "en","a","por","para","con"
  )
  
  x_ok <- x[!is_na]
  x_ok <- stringi::stri_trans_tolower(x_ok, locale = "es")
  x_ok <- stringi::stri_trans_totitle(x_ok, locale = "es")
  
  x_ok <- vapply(x_ok, function(s){
    parts <- unlist(strsplit(s, " ", fixed = TRUE))
    if (length(parts) == 0) return(s)
    for (i in seq_along(parts)) {
      w_low <- tolower(parts[i])
      if (i != 1 && w_low %in% small_words) {
        parts[i] <- w_low
      }
    }
    paste(parts, collapse = " ")
  }, FUN.VALUE = character(1))
  
  x[!is_na] <- x_ok
  x
}

# ---------- Rutas ----------
local_data_dir <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/FAO_COLOMBIA/01_Atlantico/03_SNINNY_APP/NDA_APP/data"
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

check_shp_parts <- function(shp){
  base <- sub("\\.shp$", "", shp)
  req  <- paste0(base, c(".shp",".dbf",".shx",".prj"))
  req[!file.exists(req)]
}
miss_shp <- c(check_shp_parts(ruta_shp_mpios), check_shp_parts(ruta_shp_dptos))
if (length(miss_shp)) stop("Faltan componentes de shapefile:\n", paste("-", miss_shp, collapse = "\n"))

# ---------- 1) Leer NDA (casos = filas) SOLO ORIGEN ----------
nda_raw <- readRDS(nda_path)
nda_raw <- nda_raw %>% dplyr::filter(DEPARTAMENTO_O=="ATLÁNTICO")

get_col <- function(df, opts, stop_msg){
  nm <- opts[opts %in% names(df)][1]
  if (is.na(nm) || !nzchar(nm)) stop(stop_msg) else nm
}

n_month_col    <- get_col(nda_raw, c("mes","MES"), "NDA: no 'mes'/'MES'")
n_year_col     <- get_col(nda_raw, c("ano","ANO"), "NDA: no 'ano'/'ANO'")

# Código municipal: usar ORIGEN (COD_DANE_MUNIC_O) o alternativas
n_mun_code_col <- get_col(
  nda_raw,
  c("COD_DANE_MUNIC_O", "COD_MUN5", "COD_MPIO", "MPIO_CDPMP"),
  "NDA: no código municipal de ocurrencia (origen)"
)

# DEPARTAMENTO_O / DEPARTMENTO_O (origen)
n_dep_origen_col <- get_col(
  nda_raw,
  c("DEPARTAMENTO_O", "DEPARTMENTO_O"),
  "NDA: no 'DEPARTAMENTO_O/DEPARTMENTO_O' (origen)"
)

# MUNICIPIO_O (origen)
n_mun_origen_col <- get_col(
  nda_raw,
  c("MUNICIPIO_O"),
  "NDA: no 'MUNICIPIO_O' (origen)"
)

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
    COD_MUN5  = sprintf("%05d", suppressWarnings(as.integer(.data[[n_mun_code_col]]))), # código DANE (origen)
    COD_DPTO2 = substr(COD_MUN5, 1, 2),
    DEP_O     = title_case_es(.data[[n_dep_origen_col]]), # departamento de ocurrencia (ORIGEN)
    MUN_O     = title_case_es(.data[[n_mun_origen_col]]), # municipio de ocurrencia (ORIGEN)
    edad_ni   = suppressWarnings(as.numeric(edad)),
    sexo_ni   = normalize_sex(sexo),
    confirmados = suppressWarnings(as.integer(confirmados)),
    PAC_HOS     = suppressWarnings(as.integer(pac_hos)),
    tip_cas     = trimws(as.character(tip_cas))
  ) %>%
  dplyr::filter(
    !is.na(ano),
    !is.na(COD_MUN5),
    !is.na(COD_DPTO2),
    !is.na(DEP_O), DEP_O != "",
    !is.na(MUN_O), MUN_O != ""
  )

# Solo filas válidas por ORIGEN
nda_valid <- nda

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
  "COD_MUN5"         %in% names(pob_raw) ~ "COD_MUN5",
  "COD_DANE_MUNIC_D" %in% names(pob_raw) ~ "COD_DANE_MUNIC_D",
  "COD_MPIO"         %in% names(pob_raw) ~ "COD_MPIO",
  "MPIO_CDPMP"       %in% names(pob_raw) ~ "MPIO_CDPMP",
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
  dplyr::mutate(
    MUNICIPIO_N = title_case_es(MUNICIPIO_N)
  ) %>%
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
  dplyr::mutate(
    DEPARTAMENTO_N = title_case_es(DEPARTAMENTO_N)
  ) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid()

# ---------- Helper paleta cuartiles (global, no se usa en server) ----------
make_pal_quartiles <- function(values, palette = MAP_COLORS) {
  vals <- values[is.finite(values)]
  if (!length(vals)) vals <- 0
  vals_pos <- vals[vals > 0]
  
  if (!length(vals_pos)) {
    minv <- min(vals, na.rm = TRUE)
    maxv <- max(vals, na.rm = TRUE)
    bins <- c(minv, maxv)
  } else {
    qs   <- stats::quantile(vals_pos, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    minv <- min(vals, na.rm = TRUE)
    maxv <- max(vals, na.rm = TRUE)
    bins <- unique(c(minv, as.numeric(qs), maxv))
    bins <- sort(bins)
  }
  
  leaflet::colorBin(palette, domain = vals, bins = bins, na.color = "#f0f0f0")
}

# ---------------- UI ----------------
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    primary = "#2563eb",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius" = "0.9rem",
    "font-size-base" = "0.98rem"
  ),
  
  # ===== CSS + JS =====
  tags$head(
    tags$style(HTML(sprintf("
      :root{ --border-col:%s; }
      .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
      h3{font-weight:700;letter-spacing:.2px;margin-bottom:8px}
      .data-note{font-size:13px;color:#6b7280;margin:8px 0 0}
      
      .filters{
        background:#fff;border:1.5px solid var(--border-col);
        border-radius:16px;padding:14px 16px;margin-bottom:16px;
        box-shadow:0 2px 10px rgba(0,0,0,.04)
      }
      .filters-grid{
        display:grid;
        grid-template-columns: repeat(auto-fit, minmax(180px,1fr));
        gap:12px;
      }
      .filter{
        display:flex;
        flex-direction:column;
      }
      .filter-label{
        font-family: 'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-size:14px;
        font-weight:500;
        color:#000000;
        letter-spacing:.2px;
        margin-bottom:6px;
        min-height:32px;
        display:flex;
        align-items:flex-end;
      }

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
        border-radius:16px;padding:12px;
        box-shadow:0 2px 10px rgba(0,0,0,.05);
        margin-bottom:12px
      }
      .card-title{
        font-weight:700;
        font-size:16px;
        margin-bottom:8px;
        color:#111827
      }
      .nav-tabs .nav-link.active{
        border-color:var(--border-col) var(--border-col) #fff !important
      }
      .nav-tabs{
        border-bottom:1.5px solid var(--border-col)
      }
    ", BORDER_UI))),
    
    tags$script(HTML("
      function initBsTooltips(){
        var list = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'));
        list.map(function(el){ 
          try { 
            return new bootstrap.Tooltip(el, {container: 'body'}); 
          } catch(e){} 
        });
      }
      document.addEventListener('DOMContentLoaded', initBsTooltips);
      document.addEventListener('shiny:value', initBsTooltips);
      document.addEventListener('shown.bs.tab', initBsTooltips);
    "))
  ),
  
  div(
    class = "wrap",
    h3("SIVIGILA — NDA (Niños con Desnutrición Aguda)"),
    div(
      class = "data-note",
      HTML("Nota: Las cifras recientes pueden ser <b>preliminares</b>.")
    ),
    
    tabsetPanel(
      id   = "tabs_nda",
      type = "tabs",
      
      # ================= TAB 1 =================
      tabPanel(
        title = "Distribución territorial de Niños con Desnutrición Aguda (NDA)",
        br(),
        
        # ---- filtros ----
        div(
          class = "filters",
          div(
            class = "filters-grid",
            
            div(
              class = "filter",
              div(class = "filter-label", "¿Qué año analizamos?"),
              uiOutput("anio_nda_ui")
            ),
            div(
              class = "filter",
              div(class = "filter-label", "¿En qué departamento?"),
              # <<< Atlántico por defecto en la UI
              selectInput("f_depto_nda", NULL,
                          choices  = c("Todos","Santander"),
                          selected = "Santander")
            ),
            div(
              class = "filter",
              div(class = "filter-label", "¿Algún municipio en particular?"),
              selectInput("f_mpio_nda", NULL, choices = "Todos", selected = "Todos")
            ),
            div(
              class = "filter",
              div(class = "filter-label", "¿Hombres o Mujeres?"),
              selectInput(
                "f_sexo_tab1", NULL,
                choices  = c("Todos","Hombres","Mujeres"),
                selected = "Todos"
              )
            ),
            div(
              class = "filter",
              div(class = "filter-label", "Variable a considerar"),
              selectInput(
                inputId  = "f_metrica_tab1",
                label    = NULL,
                choices  = c(
                  "Total de casos"             = "casos",
                  "Incidencia (x100.000 hab.)" = "incidencia"
                ),
                selected = "casos"
              )
            ),
            div(
              class = "filter",
              div(class = "filter-label", "Acción"),
              actionLink("btn_reset_nda","← Limpiar filtros")
            )
          )
        ),
        
        # ---- contenido ----
        fluidRow(
          column(
            width = 6,
            div(
              class = "card",
              div(
                class = "card-title",
                textOutput("ttl_map_tab1")
              ),
              leafletOutput("map_nda", height = 810),
              div(
                class = "data-note",
                HTML("Nota: Los colores del mapa corresponden a los <b>cuartiles</b> de la distribución del indicador seleccionado (según los datos visibles), usando el lugar de ocurrencia.")
              )
            )
          ),
          column(
            width = 6,
            div(
              class = "card",
              div(class = "card-title", textOutput("ttl_top10_tab1")),
              plotlyOutput("bar_nda", height = 260)
            ),
            div(
              class = "card",
              div(class = "card-title", textOutput("ttl_sexo_tab1")),
              plotlyOutput("sexo_nna_barras", height = 240)
            ),
            div(
              class = "card",
              div(class = "card-title", textOutput("ttl_top_mpios_dest")),
              plotlyOutput("destinos_mpio_top", height = 240)
            )
          )
        )
      ),
      
      # ================= TAB 2 — Resultados clínicos y supervivencia (por ocurrencia) =================
      tabPanel(
        title = "Evolución clínica y supervivencia de los casos",
        br(),
        
        div(
          class = "filters",
          div(
            class = "filters-grid",
            div(
              class = "filter",
              div(class = "filter-label", "¿Qué año analizamos?"),
              uiOutput("anio_b3_ui")
            ),
            div(
              class = "filter",
              div(class = "filter-label", "¿En qué departamento?"),
              uiOutput("dep_o_b3_ui")
            ),
            div(
              class = "filter",
              div(class = "filter-label", "¿Algún municipio en particular?"),
              uiOutput("mun_o_b3_ui")
            ),
            div(
              class = "filter",
              div(class = "filter-label", "¿Hombres o Mujeres?"),
              uiOutput("sexo_b3_ui")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            div(
              class = "card",
              div(class = "card-title", textOutput("ttl_serie_b3")),
              plotlyOutput("serie_anual_tab3", height = 285)
            ),
            div(
              class = "card",
              div(class = "card-title", textOutput("ttl_hist_b3")),
              plotlyOutput("hist_edad_tab3", height = 380)
            )
          ),
          column(
            width = 6,
            div(
              class = "card",
              div(
                class = "card-title",
                textOutput("ttl_map_superv_b3")
              ),
              leafletOutput("map_superv_b3", height = 710),
              div(
                class = "data-note",
                HTML("Nota: Los colores del mapa corresponden a los <b>cuartiles</b> de la distribución de la tasa de supervivencia (Vivos/(Vivos+Muertes)) según lugar de ocurrencia.")
              )
            )
          )
        )
      )
    )
  )
)


# ---------------- SERVER ----------------
server <- function(input, output, session){
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  
  # ---------- Helpers ----------
  empty_plot <- function(txt = "Sin datos para los filtros seleccionados.") {
    plotly::plotly_empty(type = "scatter", mode = "markers") %>%
      plotly::layout(
        annotations = list(
          x = 0.5, y = 0.5, text = as.character(txt),
          showarrow = FALSE, xref = "paper", yref = "paper",
          font = list(size = 14)
        ),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        margin = list(l = 10, r = 10, b = 10, t = 10)
      )
  }
  
  # Cuartiles robustos, cubriendo todo el rango (para Tab 1)
  make_pal_quartiles <- function(values, palette = MAP_COLORS) {
    vals <- values[is.finite(values)]
    if (!length(vals)) vals <- 0
    vals_pos <- vals[vals > 0]
    
    if (!length(vals_pos)) {
      minv <- min(vals, na.rm = TRUE)
      maxv <- max(vals, na.rm = TRUE)
      bins <- c(minv, maxv)
    } else {
      qs   <- stats::quantile(vals_pos, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      minv <- min(vals, na.rm = TRUE)
      maxv <- max(vals, na.rm = TRUE)
      bins <- unique(c(minv, as.numeric(qs), maxv))
      bins <- sort(bins)
    }
    
    leaflet::colorBin(palette, domain = vals, bins = bins, na.color = "#f0f0f0")
  }
  
  # Formateador de etiquetas: primer intervalo sin ">", los demás con "> ..."
  label_bins_gt <- function(cuts,
                            digits = 1,
                            big.mark = ".",
                            decimal.mark = ",",
                            suffix = "",
                            multiply = 1) {
    cuts <- cuts * multiply
    n <- length(cuts)
    if (n < 2) return(character(0))
    
    fmt <- function(x) scales::number(
      x,
      accuracy = if (digits > 0) 10^-digits else 1,
      big.mark = big.mark,
      decimal.mark = decimal.mark
    )
    
    labs <- character(n - 1)
    for (i in seq_len(n - 1)) {
      from <- cuts[i]
      to   <- cuts[i + 1]
      if (i == 1) {
        labs[i] <- paste0(fmt(from), " - ", fmt(to), suffix)
      } else if (i == n - 1) {
        labs[i] <- paste0("> ", fmt(from), suffix)
      } else {
        labs[i] <- paste0("> ", fmt(from), " - ", fmt(to), suffix)
      }
    }
    labs
  }
  
  metrica_tab1 <- reactive({
    if (is.null(input$f_metrica_tab1)) "casos" else input$f_metrica_tab1
  })
  
  # ================= TAB 1 — Exploración (ocurrencia) =================
  output$anio_nda_ui <- renderUI({
    yrs <- sort(unique(nda_valid$ano))
    selectInput("f_anio_nda", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  observeEvent(input$btn_reset_nda, {
    yrs <- sort(unique(nda_valid$ano))
    
    # <<< deps_all y Atlántico como valor por defecto al hacer RESET
    deps_all <- nda_valid %>%
      dplyr::distinct(DEP_O) %>%
      dplyr::filter(!is.na(DEP_O), DEP_O != "") %>%
      dplyr::arrange(DEP_O) %>%
      dplyr::pull(DEP_O)
    default_dep <- if ("Atlántico" %in% deps_all) "Atlántico" else (deps_all[1] %||% "Todos")
    
    updateSelectInput(session, "f_anio_nda",  selected = max(yrs, na.rm = TRUE))
    updateSelectInput(session, "f_depto_nda",
                      choices  = c("Todos", deps_all),
                      selected = default_dep)
    updateSelectInput(session, "f_mpio_nda",  choices = "Todos", selected = "Todos")
    updateSelectInput(session, "f_sexo_tab1",  selected = "Todos")
    updateSelectInput(session, "f_metrica_tab1", selected = "casos")
  })
  
  # Base por ocurrencia (Tab 1) — SIEMPRE filtrada por año
  nda_base_tab1 <- reactive({
    req(input$f_anio_nda)
    df <- nda_valid %>% dplyr::filter(ano == input$f_anio_nda)
    if (!is.null(input$f_depto_nda) && input$f_depto_nda != "Todos")
      df <- df %>% dplyr::filter(DEP_O == input$f_depto_nda)
    if (!is.null(input$f_mpio_nda)  && input$f_mpio_nda  != "Todos")
      df <- df %>% dplyr::filter(MUN_O == input$f_mpio_nda)
    if (!is.null(input$f_sexo_tab1) && input$f_sexo_tab1 != "Todos")
      df <- df %>% dplyr::filter(sexo_ni == input$f_sexo_tab1)
    df
  })
  
  # Combos dinámicos de ocurrencia, sujetos al año
  observeEvent(input$f_anio_nda, {
    df_year <- nda_valid %>% dplyr::filter(ano == input$f_anio_nda)
    deps_o  <- df_year %>%
      dplyr::distinct(DEP_O) %>%
      dplyr::filter(!is.na(DEP_O), DEP_O != "") %>%
      dplyr::arrange(DEP_O) %>%
      dplyr::pull(DEP_O)
    
    # <<< Atlántico como valor por defecto si está disponible
    default_dep <- if ("Atlántico" %in% deps_o) "Atlántico" else (deps_o[1] %||% "Todos")
    sel_dep <- if (!is.null(input$f_depto_nda) && input$f_depto_nda %in% deps_o) {
      input$f_depto_nda
    } else {
      default_dep
    }
    
    updateSelectInput(session, "f_depto_nda", choices = c("Todos", deps_o), selected = sel_dep)
    updateSelectInput(session, "f_mpio_nda",  choices = "Todos", selected = "Todos")
  }, ignoreInit = FALSE)
  
  observeEvent(input$f_depto_nda, {
    req(input$f_anio_nda)
    df <- nda_valid %>% dplyr::filter(ano == input$f_anio_nda)
    if (!is.null(input$f_depto_nda) && input$f_depto_nda != "Todos") {
      mpios_o <- df %>% dplyr::filter(DEP_O == input$f_depto_nda) %>%
        dplyr::distinct(MUN_O) %>%
        dplyr::filter(!is.na(MUN_O), MUN_O != "") %>%
        dplyr::arrange(MUN_O) %>%
        dplyr::pull(MUN_O)
      updateSelectInput(session, "f_mpio_nda", choices = c("Todos", mpios_o), selected = "Todos")
    } else {
      updateSelectInput(session, "f_mpio_nda", choices = "Todos", selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  # Nivel del mapa según ocurrencia
  map_nivel <- reactive({
    if (!is.null(input$f_depto_nda) && input$f_depto_nda != "Todos") "mpios" else "deptos"
  })
  
  # Código DANE del departamento "de ocurrencia"
  sel_cod_dep <- reactive({
    if (map_nivel() == "mpios") {
      req(input$f_anio_nda, input$f_depto_nda)
      cod <- nda_valid %>%
        dplyr::filter(ano == input$f_anio_nda, DEP_O == input$f_depto_nda) %>%
        dplyr::distinct(COD_DPTO2) %>%
        dplyr::pull(COD_DPTO2) %>%
        .[1]
      if (is.na(cod)) return(NA_character_)
      sprintf("%02d", as.integer(cod))
    } else {
      NA_character_
    }
  })
  
  # Agregados por ocurrencia (DEPARTAMENTOS)
  nda_agg_depto_tab1 <- reactive({
    req(input$f_anio_nda)
    df <- nda_base_tab1()
    
    casos <- df %>%
      dplyr::group_by(COD_DPTO2, DEP_O) %>%
      dplyr::summarise(casos = dplyr::n(), .groups = "drop")
    
    pop <- pob_depto %>% dplyr::filter(ano == input$f_anio_nda)
    
    casos %>%
      dplyr::left_join(pop, by = "COD_DPTO2") %>%
      dplyr::mutate(
        incidencia = dplyr::if_else(POB > 0, (casos / POB) * 1e5, NA_real_)
      )
  })
  
  # Agregados por ocurrencia (MUNICIPIOS)
  nda_agg_mpio_tab1 <- reactive({
    req(input$f_anio_nda)
    df <- nda_base_tab1()
    
    casos <- df %>%
      dplyr::group_by(COD_DPTO2, COD_MUN5, MUN_O) %>%
      dplyr::summarise(casos = dplyr::n(), .groups = "drop")
    
    pop <- pob_mpio %>% dplyr::filter(ano == input$f_anio_nda)
    
    casos %>%
      dplyr::left_join(pop, by = c("COD_DPTO2", "COD_MUN5")) %>%
      dplyr::mutate(
        incidencia = dplyr::if_else(POB > 0, (casos / POB) * 1e5, NA_real_)
      )
  })
  
  # ----- Títulos storytelling TAB 1 -----
  output$ttl_map_tab1 <- renderText({
    met <- if (metrica_tab1() == "casos") 
      "casos de niños con desnutrición aguda (NDA)" 
    else 
      "la incidencia (x100.000 hab.) de NDA"
    paste0(
      "¿En qué territorios es mayor la cantidad de ",
      met, "?"
    )
  })
  output$ttl_top10_tab1 <- renderText({
    met <- if (metrica_tab1() == "casos") "más casos de NDA" else "mayor incidencia de NDA"
    paste0("Top 10 de territorios con ", met)
  })
  output$ttl_top_mpios_dest <- renderText({
    "¿Cómo ha evolucionado en el tiempo el número de casos de NDA?"
  })
  output$ttl_sexo_tab1 <- renderText({
    "¿Cómo se distribuyen los casos de NDA entre Hombres y Mujeres?"
  })
  
  # ---------- Mapa TAB 1 (SOLO OCURRENCIA) ----------
  output$map_nda <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::setView(lng = -74.3, lat = 4.6, zoom = 5)
  })
  
  observe({
    req(input$f_anio_nda)
    metric <- metrica_tab1()
    is_casos <- identical(metric, "casos")
    
    titulo <- if (is_casos) "Casos" else "Incidencia (x100.000 hab.)"
    fmt_val <- if (is_casos) {
      function(x) scales::comma(x, big.mark = ".", decimal.mark = ",")
    } else {
      function(x) scales::number(x, big.mark = ".", decimal.mark = ",", accuracy = 0.1)
    }
    digitos <- if (is_casos) 0 else 1
    
    if (map_nivel() == "deptos") {
      dd <- nda_agg_depto_tab1()
      shp <- dptos_sf %>%
        dplyr::left_join(dd, by = "COD_DPTO2") %>%
        dplyr::mutate(
          valor = if (is_casos) casos else incidencia,
          valor = tidyr::replace_na(valor, 0),
          nombre = dplyr::coalesce(DEP_O, DEPARTAMENTO_N, COD_DPTO2),
          etq   = paste0(
            "<b>", nombre, "</b><br>",
            titulo, ": ", fmt_val(valor)
          )
        )
      
      pal <- make_pal_quartiles(shp$valor, MAP_COLORS)
      leaflet::leafletProxy("map_nda", data = shp) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addPolygons(
          layerId = ~COD_DPTO2,
          fillColor = ~pal(valor),
          color = BORDER_UI, weight = 0.7, fillOpacity = 0.9,
          label = ~lapply(etq, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(
            color = BORDER_UI, weight = 2, bringToFront = TRUE
          )
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = pal,
          values = ~valor,
          title = titulo,
          labFormat = function(type, cuts) {
            label_bins_gt(cuts, digits = digitos, big.mark = ".", decimal.mark = ",")
          }
        )
    } else {
      sel_cod <- sel_cod_dep()
      req(!is.na(sel_cod), nzchar(sel_cod))
      dd <- nda_agg_mpio_tab1()
      shp <- mpios_sf %>%
        dplyr::filter(COD_DPTO2 == sel_cod) %>%
        dplyr::left_join(dd, by = c("COD_DPTO2", "COD_MUN5")) %>%
        dplyr::mutate(
          valor = if (is_casos) casos else incidencia,
          valor = tidyr::replace_na(valor, 0),
          nombre = dplyr::coalesce(MUN_O, MUNICIPIO_N, COD_MUN5),
          etq   = paste0(
            "<b>", nombre, "</b><br>",
            titulo, ": ", fmt_val(valor)
          )
        )
      
      pal <- make_pal_quartiles(shp$valor, MAP_COLORS)
      bb  <- sf::st_bbox(shp)
      leaflet::leafletProxy("map_nda", data = shp) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addPolygons(
          layerId = ~COD_MUN5,
          fillColor = ~pal(valor),
          color = BORDER_UI, weight = 0.4, fillOpacity = 0.9,
          label = ~lapply(etq, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(
            color = BORDER_UI, weight = 2, bringToFront = TRUE
          )
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = pal,
          values = ~valor,
          title = titulo,
          labFormat = function(type, cuts) {
            label_bins_gt(cuts, digits = digitos, big.mark = ".", decimal.mark = ",")
          }
        ) %>%
        leaflet::fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  
  # Click en mapa → actualiza filtros de ocurrencia
  observeEvent(input$map_nda_shape_click, {
    click <- input$map_nda_shape_click
    req(click$id, input$f_anio_nda)
    if (map_nivel() == "deptos") {
      cod <- sprintf("%02d", as.integer(click$id))
      nom <- nda_valid %>%
        dplyr::filter(ano == input$f_anio_nda, COD_DPTO2 == cod) %>%
        dplyr::distinct(DEP_O) %>%
        dplyr::pull(DEP_O) %>%
        .[1]
      if (!is.na(nom) && nzchar(nom))
        updateSelectInput(session, "f_depto_nda", selected = nom)
    } else {
      codm <- sprintf("%05d", as.integer(click$id))
      nomm <- nda_valid %>%
        dplyr::filter(ano == input$f_anio_nda, COD_MUN5 == codm) %>%
        dplyr::distinct(MUN_O) %>%
        dplyr::pull(MUN_O) %>%
        .[1]
      if (!is.na(nomm) && nzchar(nomm))
        updateSelectInput(session, "f_mpio_nda", selected = nomm)
    }
  }, ignoreInit = TRUE)
  
  # ---------- Top-10 (según métrica, por ocurrencia) ----------
  top10_df <- reactive({
    metric <- metrica_tab1()
    is_casos <- identical(metric, "casos")
    
    if (map_nivel() == "deptos") {
      dd <- nda_agg_depto_tab1() %>%
        dplyr::mutate(
          nombre = ifelse(!is.na(DEP_O) & nzchar(DEP_O), DEP_O, COD_DPTO2)
        )
    } else {
      dd <- nda_agg_mpio_tab1() %>%
        dplyr::mutate(
          nombre = ifelse(!is.na(MUN_O) & nzchar(MUN_O), MUN_O, COD_MUN5)
        )
    }
    
    if (is_casos) {
      df <- dd %>%
        dplyr::filter(is.finite(casos)) %>%
        dplyr::transmute(nombre, valor = casos)
    } else {
      df <- dd %>%
        dplyr::filter(is.finite(incidencia)) %>%
        dplyr::transmute(nombre, valor = incidencia)
    }
    
    df %>%
      dplyr::arrange(dplyr::desc(valor)) %>%
      dplyr::slice_head(n = 10)
  })
  
  output$bar_nda <- renderPlotly({
    df <- top10_df()
    if (is.null(df) || nrow(df) == 0)
      return(empty_plot("No hay datos para el Top-10 con los filtros actuales."))
    
    metric <- metrica_tab1()
    is_casos <- identical(metric, "casos")
    
    lab_fun <- if (is_casos) {
      function(x) format(x, big.mark = ".", decimal.mark = ",")
    } else {
      function(x) scales::number(x, big.mark = ".", decimal.mark = ",", accuracy = 0.1)
    }
    x_title <- if (is_casos) "Casos" else "Incidencia (x100.000 hab.)"
    
    df2 <- df %>%
      dplyr::arrange(valor) %>%
      dplyr::mutate(
        nombre = factor(nombre, levels = nombre),
        label  = lab_fun(valor),
        hover  = paste0(nombre, "<br>", x_title, ": ", label)
      )
    
    plot_ly(
      data = df2,
      x = ~valor,
      y = ~nombre,
      type = "bar",
      orientation = "h",
      marker = list(color = BAR_COLOR),
      text = ~label,
      textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(color = "white", size = 11),
      customdata = ~hover,
      hovertemplate = "%{customdata}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = x_title, showgrid = FALSE),
        yaxis = list(
          title = "",
          automargin = TRUE,
          showgrid = TRUE,
          gridcolor = GRID_COLOR,
          gridwidth = 0.5
        ),
        margin = list(l = 10, r = 10, t = 10, b = 10),
        showlegend = FALSE
      )
  })
  
  # ======= Serie temporal de casos NDA (por ocurrencia, todos los años) =======
  serie_origen_df <- reactive({
    origen_dep <- input$f_depto_nda %||% "Todos"
    origen_mun <- input$f_mpio_nda  %||% "Todos"
    
    df <- nda_valid
    if (!is.null(input$f_sexo_tab1) && input$f_sexo_tab1 != "Todos") {
      df <- df %>% dplyr::filter(sexo_ni == input$f_sexo_tab1)
    }
    if (!is.null(origen_dep) && origen_dep != "Todos") {
      df <- df %>% dplyr::filter(DEP_O == origen_dep)
    }
    if (!is.null(origen_mun) && origen_mun != "Todos") {
      df <- df %>% dplyr::filter(MUN_O == origen_mun)
    }
    
    df %>%
      dplyr::group_by(ano) %>%
      dplyr::summarise(casos = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(ano)
  })
  
  output$destinos_mpio_top <- renderPlotly({
    df <- serie_origen_df()
    if (is.null(df) || nrow(df) == 0) {
      return(empty_plot("No hay casos de NDA para los filtros de ocurrencia seleccionados."))
    }
    
    df <- df %>%
      dplyr::mutate(
        casos_fmt = format(casos, big.mark = ".", decimal.mark = ","),
        label = paste0("Año: ", ano, "<br>Casos: ", casos_fmt)
      )
    
    plot_ly(
      data = df,
      x    = ~ano,
      y    = ~casos,
      type = "scatter",
      mode = "lines+markers",
      line   = list(color = BAR_COLOR),
      marker = list(size = 7, color = BAR_COLOR),
      text        = ~label,
      hoverinfo   = "text",
      hoverlabel  = list(align = "left")
    ) %>%
      layout(
        xaxis = list(
          title    = "",
          dtick    = 1,
          showgrid = FALSE
        ),
        yaxis = list(
          title    = "Casos",
          showgrid = TRUE,
          gridcolor = GRID_COLOR,
          gridwidth = 0.5
        ),
        margin = list(l = 60, r = 20, b = 40, t = 10),
        showlegend = FALSE
      )
  })
  
  # ---- BARRAS: distribución por sexo (Tab 1, ocurrencia) ----
  output$sexo_nna_barras <- renderPlotly({
    df <- nda_base_tab1() %>%
      dplyr::mutate(
        sexo_cat = dplyr::case_when(
          sexo_ni %in% c("Hombres","Mujeres") ~ sexo_ni,
          TRUE ~ "Sin dato"
        )
      ) %>%
      dplyr::group_by(sexo_cat) %>%
      dplyr::summarise(Casos = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(match(sexo_cat, c("Hombres","Mujeres","Sin dato")))
    
    if (nrow(df) == 0)
      return(empty_plot("No hay datos de sexo para los filtros seleccionados."))
    
    cols <- SEX_COLORS[df$sexo_cat]
    cols[is.na(cols)] <- "#cbd5e1"
    
    df <- df %>%
      dplyr::mutate(
        label = format(Casos, big.mark = ".", decimal.mark = ","),
        hover = paste0(sexo_cat, "<br>Casos: ", label)
      )
    
    plot_ly(
      df,
      x = ~Casos,
      y = ~sexo_cat,
      type = "bar",
      orientation = "h",
      marker = list(color = cols),
      text = ~label,
      textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(color = "white", size = 11),
      customdata = ~hover,
      hovertemplate = "%{customdata}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Casos", showgrid = FALSE),
        yaxis = list(
          title = "",
          showgrid = TRUE,
          gridcolor = GRID_COLOR,
          gridwidth = 0.5
        ),
        margin = list(l = 90, r = 10, b = 20, t = 10),
        showlegend = FALSE
      )
  })
  
  # ================= TAB 2 — Resultados clínicos y supervivencia (por ocurrencia) =================
  output$anio_b3_ui <- renderUI({
    yrs <- sort(unique(nda$ano))
    selectInput("f_anio_b3", "", choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  # 👉 Departamentos de ocurrencia sujetos al año seleccionado
  output$dep_o_b3_ui <- renderUI({
    req(input$f_anio_b3)
    deps <- nda %>%
      dplyr::filter(ano == input$f_anio_b3) %>%
      dplyr::distinct(DEP_O) %>%
      dplyr::filter(!is.na(DEP_O), DEP_O != "") %>%
      dplyr::arrange(DEP_O) %>%
      dplyr::pull(DEP_O)
    
    # <<< Atlántico por defecto en Tab 2 si está disponible
    default_dep <- if ("Atlántico" %in% deps) "Atlántico" else "Todos"
    
    selectInput("f_dep_o_b3", "",
                choices  = c("Todos", deps),
                selected = default_dep)
  })
  
  # 👉 Municipios de ocurrencia sujetos a año y departamento
  output$mun_o_b3_ui <- renderUI({
    req(input$f_anio_b3, input$f_dep_o_b3)
    
    if (input$f_dep_o_b3 == "Todos") {
      mm <- nda %>%
        dplyr::filter(ano == input$f_anio_b3) %>%
        dplyr::distinct(MUN_O) %>%
        dplyr::filter(!is.na(MUN_O), MUN_O != "") %>%
        dplyr::arrange(MUN_O)
      selectInput("f_mun_o_b3", "", choices = c("Todos", mm$MUN_O), selected = "Todos")
    } else {
      mm <- nda %>%
        dplyr::filter(ano == input$f_anio_b3, DEP_O == input$f_dep_o_b3) %>%
        dplyr::distinct(MUN_O) %>%
        dplyr::filter(!is.na(MUN_O), MUN_O != "") %>%
        dplyr::arrange(MUN_O)
      selectInput("f_mun_o_b3", "", choices = c("Todos", mm$MUN_O), selected = "Todos")
    }
  })
  
  output$sexo_b3_ui <- renderUI({
    selectInput("f_sexo_b3", "", choices = c("Todos","Hombres","Mujeres"), selected = "Todos")
  })
  
  # Base de Tab 2 SOLO por ocurrencia (año + filtros)
  base_b3 <- reactive({
    req(input$f_anio_b3)
    df <- nda %>% dplyr::filter(ano == input$f_anio_b3)
    if (!is.null(input$f_dep_o_b3) && input$f_dep_o_b3 != "Todos")
      df <- df %>% dplyr::filter(DEP_O == input$f_dep_o_b3)
    if (!is.null(input$f_mun_o_b3) && input$f_mun_o_b3 != "Todos")
      df <- df %>% dplyr::filter(MUN_O == input$f_mun_o_b3)
    if (!is.null(input$f_sexo_b3)  && input$f_sexo_b3  != "Todos")
      df <- df %>% dplyr::filter(sexo_ni == input$f_sexo_b3)
    df
  })
  
  # ---- Títulos storytelling TAB 2 ----
  output$ttl_serie_b3 <- renderText({
    "¿Cómo ha evolucionado anualmente la supervivencia y las defunciones asociadas al NDA?"
  })
  output$ttl_hist_b3 <- renderText({
    if (is.null(input$f_dep_o_b3) || input$f_dep_o_b3 == "Todos") {
      "Top 5 de departamentos con más casos, vivos y fallecidos por NDA"
    } else {
      "Top 5 de municipios con más casos, vivos y fallecidos por NDA"
    }
  })
  output$ttl_map_superv_b3 <- renderText({
    "¿Dónde es más alta la tasa de supervivencia (Vivos/(Vivos+Muertes))?"
  })
  
  # --- Serie supervivencia/defunciones (toda la serie histórica, por ocurrencia) ---
  output$serie_anual_tab3 <- renderPlotly({
    df0 <- nda %>%
      dplyr::filter(!is.na(PAC_HOS), PAC_HOS %in% c(1L, 2L))
    
    if (!is.null(input$f_dep_o_b3) && input$f_dep_o_b3 != "Todos") {
      df0 <- df0 %>% dplyr::filter(DEP_O == input$f_dep_o_b3)
    }
    if (!is.null(input$f_mun_o_b3) && input$f_mun_o_b3 != "Todos") {
      df0 <- df0 %>% dplyr::filter(MUN_O == input$f_mun_o_b3)
    }
    if (!is.null(input$f_sexo_b3) && input$f_sexo_b3 != "Todos") {
      df0 <- df0 %>% dplyr::filter(sexo_ni == input$f_sexo_b3)
    }
    
    if (is.null(df0) || nrow(df0) == 0)
      return(empty_plot("Sin datos para los filtros seleccionados."))
    
    dfm <- df0 %>%
      dplyr::mutate(ANO = suppressWarnings(as.integer(ano))) %>%
      dplyr::filter(!is.na(ANO)) %>%
      dplyr::group_by(ANO, PAC_HOS) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from  = PAC_HOS,
        values_from = n,
        values_fill = 0,
        names_prefix = "cat_"
      ) %>%
      dplyr::transmute(
        ANO,
        vivos   = cat_1,
        muertes = cat_2
      ) %>%
      dplyr::arrange(ANO) %>%
      dplyr::mutate(
        vivos_txt   = format(vivos, big.mark = ".", decimal.mark = ","),
        muertes_txt = format(muertes, big.mark = ".", decimal.mark = ","),
        ano_txt     = as.character(ANO)
      )
    
    plot_ly(dfm, x = ~ANO) %>%
      add_lines(
        y = ~vivos, name = "Vivos", yaxis = "y",
        line = list(color = BAR_COLOR),
        text = ~paste0("Año: ", ano_txt, "<br>Vivos: ", vivos_txt),
        hoverinfo = "text"
      ) %>%
      add_lines(
        y = ~muertes, name = "Defunciones", yaxis = "y2",
        line = list(color = "#ffb366"),
        text = ~paste0("Año: ", ano_txt, "<br>Muertes: ", muertes_txt),
        hoverinfo = "text"
      ) %>%
      layout(
        xaxis = list(
          title    = "",
          dtick    = 1,
          showgrid = FALSE
        ),
        yaxis  = list(
          title     = "Vivos",
          showgrid  = TRUE,
          gridcolor = GRID_COLOR,
          gridwidth = 0.5
        ),
        yaxis2 = list(
          title      = "Defunciones",
          overlaying = "y",
          side       = "right",
          showgrid   = FALSE
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2,
          yanchor = "top"
        ),
        margin = list(l = 50, r = 50, b = 80, t = 10)
      )
  })
  
  # ---------- Mapa de Supervivencia (Tab 2 por ocurrencia) ----------
  nivel_b3_map <- reactive({
    if (!is.null(input$f_dep_o_b3) && input$f_dep_o_b3 != "Todos") "mpios" else "deptos"
  })
  
  agg_superv_depto_b3 <- reactive({
    base_b3() %>%
      dplyr::filter(!is.na(PAC_HOS), PAC_HOS %in% c(1L,2L)) %>%
      dplyr::group_by(DEP_O) %>%
      dplyr::summarise(
        vivos   = sum(PAC_HOS == 1L),
        muertes = sum(PAC_HOS == 2L),
        n       = vivos + muertes,
        .groups = "drop"
      ) %>%
      dplyr::mutate(tasa = dplyr::if_else(n > 0, vivos / n, NA_real_))
  })
  
  agg_superv_mpio_b3 <- reactive({
    base_b3() %>%
      dplyr::filter(!is.na(PAC_HOS), PAC_HOS %in% c(1L,2L)) %>%
      dplyr::group_by(COD_DPTO2, DEP_O, MUN_O) %>%   # <-- incluye COD_DPTO2
      dplyr::summarise(
        vivos   = sum(PAC_HOS == 1L),
        muertes = sum(PAC_HOS == 2L),
        n       = vivos + muertes,
        .groups = "drop"
      ) %>%
      dplyr::mutate(tasa = dplyr::if_else(n > 0, vivos / n, NA_real_))
  })
  
  output$map_superv_b3 <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::setView(lng = -74.3, lat = 4.6, zoom = 5)
  })
  
  observe({
    titulo <- "Tasa de supervivencia"
    fmt_p  <- function(x) {
      ifelse(
        is.na(x),
        "s/d",
        scales::percent(x, accuracy = 0.1, decimal.mark = ",")
      )
    }
    fmt_n  <- function(x) scales::comma(x, big.mark = ".", decimal.mark = ",")
    
    if (nivel_b3_map() == "deptos") {
      dd <- agg_superv_depto_b3()
      
      shp <- dptos_sf %>%
        dplyr::left_join(
          dd,
          by = c("DEPARTAMENTO_N" = "DEP_O")
        ) %>%
        dplyr::mutate(
          nombre  = DEPARTAMENTO_N,
          vivos   = tidyr::replace_na(vivos, 0L),
          muertes = tidyr::replace_na(muertes, 0L),
          n       = tidyr::replace_na(n, 0L),
          etq     = paste0(
            "<b>", nombre, "</b><br>",
            titulo, ": ", fmt_p(tasa), "<br>",
            "Vivos: ", fmt_n(vivos), "<br>",
            "Defunciones: ", fmt_n(muertes)
          )
        )
      
      vals <- shp$tasa
      vals <- vals[is.finite(vals) & vals >= 0]
      if (!length(vals)) {
        bins_q <- c(0, 1)
      } else {
        qs   <- stats::quantile(vals, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
        minv <- 0
        maxv <- max(vals, na.rm = TRUE)
        bins_q <- unique(c(minv, as.numeric(qs), maxv))
        bins_q <- sort(bins_q)
      }
      
      pal <- leaflet::colorBin(
        MAP_COLORS,
        domain = shp$tasa,
        bins   = bins_q,
        na.color = "#f0f0f0"
      )
      
      leaflet::leafletProxy("map_superv_b3", data = shp) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addPolygons(
          layerId = ~COD_DPTO2,
          fillColor = ~pal(tasa),
          color = BORDER_UI, weight = 0.7, fillOpacity = 0.9,
          label = ~lapply(etq, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(
            color = BORDER_UI, weight = 2, bringToFront = TRUE
          )
        ) %>%
        leaflet::addLegend(
          "bottomright",
          pal = pal,
          values = ~tasa,
          title = titulo,
          labFormat = function(type, cuts) {
            label_bins_gt(cuts, digits = 1, big.mark = ".", decimal.mark = ",", suffix = " %", multiply = 100)
          }
        )
      
    } else {
      dep_sel <- input$f_dep_o_b3
      req(dep_sel, dep_sel != "Todos")
      dd <- agg_superv_mpio_b3()
      
      # código DANE del departamento seleccionado
      dep_cod <- dptos_sf$COD_DPTO2[match(dep_sel, dptos_sf$DEPARTAMENTO_N)]
      req(!is.na(dep_cod), nzchar(dep_cod))
      
      shp <- mpios_sf %>%
        dplyr::filter(COD_DPTO2 == dep_cod) %>%
        dplyr::left_join(
          dd,
          by = c("COD_DPTO2", "MUNICIPIO_N" = "MUN_O")
        ) %>%
        dplyr::mutate(
          nombre  = MUNICIPIO_N,
          vivos   = tidyr::replace_na(vivos, 0L),
          muertes = tidyr::replace_na(muertes, 0L),
          n       = tidyr::replace_na(n, 0L),
          etq     = paste0(
            "<b>", nombre, "</b><br>",
            titulo, ": ", fmt_p(tasa), "<br>",
            "Vivos: ", fmt_n(vivos), "<br>",
            "Defunciones: ", fmt_n(muertes)
          )
        )
      
      vals <- shp$tasa
      vals <- vals[is.finite(vals) & vals >= 0]
      if (!length(vals)) {
        bins_q <- c(0, 1)
      } else {
        qs   <- stats::quantile(vals, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
        minv <- 0
        maxv <- max(vals, na.rm = TRUE)
        bins_q <- unique(c(minv, as.numeric(qs), maxv))
        bins_q <- sort(bins_q)
      }
      
      pal <- leaflet::colorBin(
        MAP_COLORS,
        domain = shp$tasa,
        bins   = bins_q,
        na.color = "#f0f0f0"
      )
      bb <- sf::st_bbox(shp)
      
      leaflet::leafletProxy("map_superv_b3", data = shp) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addPolygons(
          layerId = ~COD_MUN5,
          fillColor = ~pal(tasa),
          color = BORDER_UI, weight = 0.4, fillOpacity = 0.9,
          label = ~lapply(etq, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(
            color = BORDER_UI, weight = 2, bringToFront = TRUE
          )
        ) %>%
        leaflet::addLegend(
          "bottomright",
          pal = pal,
          values = ~tasa,
          title = titulo,
          labFormat = function(type, cuts) {
            label_bins_gt(cuts, digits = 1, big.mark = ".", decimal.mark = ",", suffix = " %", multiply = 100)
          }
        ) %>%
        leaflet::fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  
  # Click en mapa Tab 2 → actualiza departamento de ocurrencia
  observeEvent(input$map_superv_b3_shape_click, {
    if (nivel_b3_map() == "deptos") {
      click <- input$map_superv_b3_shape_click
      req(click$id, input$f_anio_b3)
      cod <- sprintf("%02d", as.integer(click$id))
      nom <- dptos_sf$DEPARTAMENTO_N[match(cod, dptos_sf$COD_DPTO2)]
      deps_disp <- nda %>%
        dplyr::filter(ano == input$f_anio_b3) %>%
        dplyr::distinct(DEP_O) %>%
        dplyr::pull(DEP_O)
      if (!is.na(nom) && nzchar(nom) && (nom %in% deps_disp)) {
        updateSelectInput(session, "f_dep_o_b3", selected = nom)
      }
    }
  }, ignoreInit = TRUE)
  
  # ---------- Top 5 vivos/muertos/casos (Tab 2, por ocurrencia) ----------
  top10_b3 <- reactive({
    df <- base_b3() %>%
      dplyr::filter(!is.na(PAC_HOS), PAC_HOS %in% c(1L,2L))
    if (nrow(df) == 0) return(df[0,])
    
    if (is.null(input$f_dep_o_b3) || input$f_dep_o_b3 == "Todos") {
      agg <- df %>%
        dplyr::group_by(DEP_O) %>%
        dplyr::summarise(
          vivos   = sum(PAC_HOS == 1L),
          muertes = sum(PAC_HOS == 2L),
          casos   = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::filter(casos > 0) %>%
        dplyr::arrange(dplyr::desc(casos)) %>%
        dplyr::slice_head(n = 5) %>%
        dplyr::mutate(entidad = DEP_O)
    } else {
      agg <- df %>%
        dplyr::group_by(DEP_O, MUN_O) %>%
        dplyr::summarise(
          vivos   = sum(PAC_HOS == 1L),
          muertes = sum(PAC_HOS == 2L),
          casos   = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::filter(casos > 0) %>%
        dplyr::arrange(dplyr::desc(casos)) %>%
        dplyr::slice_head(n = 5) %>%
        dplyr::mutate(entidad = MUN_O)
    }
    agg
  })
  
  output$hist_edad_tab3 <- renderPlotly({
    agg <- top10_b3()
    if (is.null(agg) || nrow(agg) == 0)
      return(empty_plot("No hay casos con información de resultado hospitalario para los filtros seleccionados."))
    
    df_long <- agg %>%
      dplyr::select(entidad, vivos, muertes, casos) %>%
      tidyr::pivot_longer(
        cols = c("casos","vivos","muertes"),
        names_to = "tipo",
        values_to = "valor"
      ) %>%
      dplyr::mutate(
        tipo = factor(
          tipo,
          levels = c("casos","vivos","muertes"),
          labels = c("Casos NDA","Vivos","Defunciones")
        )
      )
    
    orden <- agg %>%
      dplyr::arrange(casos) %>%
      dplyr::pull(entidad)
    
    df_long <- df_long %>%
      dplyr::mutate(
        entidad = factor(entidad, levels = orden),
        label   = format(valor, big.mark = ".", decimal.mark = ","),
        hover   = paste0(
          as.character(tipo),
          "<br>Territorio de ocurrencia: ", entidad,
          "<br>Casos: ", label
        )
      )
    
    plot_ly(
      data = df_long,
      x = ~valor,
      y = ~entidad,
      color = ~tipo,
      type = "bar",
      orientation = "h",
      text = ~label,
      textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(color = "white", size = 11),
      customdata = ~hover,
      hovertemplate = "%{customdata}<extra></extra>",
      colors = c("#f97316", "#22c55e", "#ef4444")
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(
          title = "Casos",
          showgrid = FALSE
        ),
        yaxis = list(
          title = "",
          showgrid = TRUE,
          gridcolor = GRID_COLOR,
          gridwidth = 0.5
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2,
          yanchor = "top"
        ),
        margin = list(l = 120, r = 10, b = 80, t = 10)
      )
  })
  
}

shinyApp(ui, server)