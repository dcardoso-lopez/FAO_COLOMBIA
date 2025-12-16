# app_sninny_cuadrantes_tabs_segmentados.R
# -------------------------------------------------------------------

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(dplyr); library(htmltools); library(stringi); library(tibble)
  library(plotly); library(scales)
})

options(stringsAsFactors = FALSE)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---------------- Rutas ----------------
data_dir <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/FAO_COLOMBIA/03_Colombia/COLOMBIA/data"

# ---------------- Sistemas (cada código puede agrupar varias RDS) ----------------
sistemas_df <- tibble::tribble(
  ~sistema,                   ~archivos,
  "DANE_ECV",                 list("052_DANE_ECV.rds"),
  "DANE_POPULATION",          list("051_DANE_Proyecciones_P.rds"),
  "DNP-IDM",                  list("071_DNP_Terridata_IDM.rds"),
  "DNP_SISBEN",               list("031_DNP_SISBEN.rds"),
  "EVA_A",                    list("011_UPRA_EVA-A.rds"),
  "FINAGRO_CFA",              list("081_FINAGRO_CFA_fast.rds"),
  "HANSEN_COBERTURA_BOSQUE",  list("141_HANSEN_COBERTURA_NETA_TOTAL.rds"),
  "HANSEN_DEFORESTATION",     list("141_HANSEN_DEFORESTATION.rds"),
  # ICA_P = fusión de las 4 bases pecuarias
  "ICA_P", list(c(
    "101_ICA_CensoPecuario-Bovino.rds",
    "102_ICA_CensoPecuario-Porcino.rds",
    "103_ICA_CensoPecuario-BCOE.rds",
    "104_ICA_CensoPecuario-Aviar.rds"
  )),
  "NDA_APP",              list("023_INS_SIVIGILA-NDA.rds"),
  "NOAA_PRECIPITATION",   list("131_NOAA_Precipitación.rds"),
  "SIVIGILA_BPANI",       list("021_INS_SIVIGILA-BPAN.rds"),
  "SIVIGILA_ETA",         list("022_INS_SIVIGILA-ETA.rds"),
  "UPRA_APADT",           list("014_UPRA_APADT.rds"),
  "UPRA_FA",              list("013_UPRA_FA_Proporcion FA_Total municipal.rds")
) %>%
  mutate(
    rutas = lapply(archivos, function(v) file.path(data_dir, v))
  )

# ---------------- Indicadores (título, dimensión, sistema) ----------------
indicadores_df <- tibble::tribble(
  ~titulo,                                                                 ~dimension,        ~sistema,
  "Radar territorial de inseguridad alimentaria y consumo de ultra-procesados en los hogares", "Vulnerabilidad", "DANE_ECV",
  "Transición demográfica territorial",                                   "Estructura",      "DANE_POPULATION",
  "Capacidad institucional del territorio según el Índice de Desempeño Municipal (IDM)", "Estructura", "DNP-IDM",
  "Condiciones socio-económicas del territorio",                          "Vulnerabilidad",  "DNP_SISBEN",
  "Radiografía de los sistemas productivos agrícolas territoriales",      "Importancia",     "EVA_A",
  "Ruta del financiamiento rural",                                        "Vulnerabilidad",  "FINAGRO_CFA",
  "Cobertura de bosque en Colombia: evidencia territorial para priorizar la conservación", "Estructura", "HANSEN_COBERTURA_BOSQUE",
  "Pérdida de bosque en el territorio: focos críticos de deforestación.", "Vulnerabilidad",  "HANSEN_DEFORESTATION",
  "Inventarios pecuarios territoriales ICA para la gestión de políticas rurales", "Importancia", "ICA_P",
  "Desnutrición aguda infantil y brechas territoriales",                  "Vulnerabilidad",  "NDA_APP",
  "Radiografía de la lluvia para los Sistemas Agroalimentarios Territoriales (SAT)", "Vulnerabilidad", "NOAA_PRECIPITATION",
  "Gestión territorial del bajo peso al nacer",                           "Vulnerabilidad",  "SIVIGILA_BPANI",
  "Vigilancia territorial de Enfermedades Transmitidas por Alimentos",    "Vulnerabilidad",  "SIVIGILA_ETA",
  "Planificación productiva: Área potencial de riego o drenaje para las actividades agropecuarias", "Estructura", "UPRA_APADT",
  "Planificación productiva: Ordenamiento a partir de la frontera agropecuaria con enfoque territorial", "Estructura", "UPRA_FA"
)

# ---------------- Helpers ----------------

title_case_es <- function(x){
  if (is.null(x)) return(x)
  lower_words <- c(
    "a","ante","bajo","cabe","con","contra","de","del","desde","en","entre",
    "hacia","hasta","para","por","según","sin","so","sobre","tras","el","la",
    "los","las","un","una","unos","unas","y","e","o","u","ni","al"
  )
  vapply(as.character(x), function(s){
    if (is.na(s)) return(NA_character_)
    s <- stringi::stri_trim_both(s)
    if (s == "") return(s)
    s_low <- tolower(s)
    parts <- unlist(strsplit(s_low, "\\s+"))
    parts_tc <- stringi::stri_trans_totitle(parts, locale = "es")
    if (length(parts_tc) > 1){
      for (i in 2:length(parts_tc)){
        w_low <- tolower(parts_tc[i])
        if (w_low %in% lower_words) parts_tc[i] <- w_low
      }
    }
    paste(parts_tc, collapse = " ")
  }, FUN.VALUE = character(1))
}

choices_por_dimension <- function(dim){
  df <- indicadores_df %>% dplyr::filter(dimension == dim)
  if (nrow(df) == 0) {
    setNames("NINGUNO", "Sin sistemas asociados para esta dimensión")
  } else {
    setNames(df$sistema, df$titulo)
  }
}

lbl <- function(x) div(class = "filter-label", x)

selector_base <- function(input_id, dimension){
  selectInput(
    inputId = input_id,
    label   = NULL,
    choices = choices_por_dimension(dimension),
    selected = choices_por_dimension(dimension)[1]
  )
}

# --- EVA_A: cultivos industriales vs alimenticios ---

eva_info <- tryCatch({
  eva_path <- file.path(data_dir, "011_UPRA_EVA-A.rds")
  if (!file.exists(eva_path)) return(NULL)
  eva_df <- readRDS(eva_path)
  cult <- sort(unique(as.character(eva_df$cultivo)))
  industriales <- c(
    "Cacao","Café","Caña de Azúcar","Fique","Iraca","Olivo",
    "Otras oleaginosas","Otros cultivos tropicales tradicionales",
    "Palma de aceite","Sacha inchi"
  )
  tipo <- ifelse(cult %in% industriales, "Industrial", "Alimenticio")
  tibble(
    cultivo = cult,
    label   = title_case_es(cult),
    tipo    = tipo
  )
}, error = function(e) NULL)

eva_tipos_choices <- c(
  "Todos los cultivos"        = "Todos",
  "Cultivos alimenticios"     = "Alimenticio",
  "Cultivos industriales"     = "Industrial"
)

eva_cultivo_choices_fun <- function(tipo = "Todos"){
  if (is.null(eva_info) || nrow(eva_info) == 0){
    return(c("Todos" = "Todos"))
  }
  df <- eva_info
  if (!is.null(tipo) && tipo != "Todos"){
    df <- dplyr::filter(df, .data$tipo == tipo)
  }
  c("Todos" = "Todos",
    stats::setNames(df$cultivo, df$label))
}

eva_indicador_choices <- c(
  "Área sembrada (Ha)"   = "area_sembrada_ha",
  "Área cosechada (Ha)"  = "area_cosechada_ha",
  "Producción (Ton)"     = "produccion_t",
  "Rendimiento (Ton/Ha)" = "rendimiento_t_ha"
)

# ===================== Transición demográfica: opciones =====================
pob_var_choices <- c(
  "Población total"                 = "poblacion_total",
  "Población joven (0–14 años)"     = "poblacion_joven",
  "Población adulta mayor (65+)"    = "poblacion_mayor",
  "Razón de dependencia"            = "razon_dependencia"
)

# ===================== Hansen Cobertura de Bosque: opciones =================
hansen_metric_choices <- c(
  "Cobertura neta (Ha)" = "ha",
  "Cobertura neta (%)"  = "pct"
)

# =========================================================
# SERIES TEMPORALES / BARRAS NACIONALES (sin depto ni muni)
# =========================================================

# ----- Formato numérico ES -----
fmt_num_es <- function(x, digits = 1){
  scales::number(
    x,
    accuracy     = 10^-digits,
    big.mark     = ".",
    decimal.mark = ","
  )
}

fmt_km <- function(x){
  vapply(x, function(v){
    if (is.na(v)) return(NA_character_)
    av <- abs(v)
    if (av >= 1e6){
      paste0(fmt_num_es(v/1e6, digits = 1), "M")
    } else if (av >= 1e3){
      paste0(fmt_num_es(v/1e3, digits = 1), "K")
    } else {
      fmt_num_es(v, digits = 0)
    }
  }, character(1))
}

fmt_short <- function(x){
  ifelse(
    is.na(x), NA_character_,
    ifelse(
      abs(x) >= 1e6,
      paste0(fmt_num_es(x/1e6, digits = 1), "M"),
      ifelse(
        abs(x) >= 1e3,
        paste0(fmt_num_es(x/1e3, digits = 1), "K"),
        fmt_num_es(x, digits = 0)
      )
    )
  )
}

fmt_pct <- function(x, digits = 1){
  paste0(fmt_num_es(x, digits = digits), "%")
}

# ----- plot vacío -----
empty_ts_plot <- function(msg = "Sin datos para los filtros actuales."){
  plotly::plotly_empty(type = "scatter", mode = "markers") %>%
    plotly::layout(
      annotations = list(
        x = 0.5, y = 0.5, text = as.character(msg),
        showarrow = FALSE, xref = "paper", yref = "paper",
        font = list(size = 14)
      ),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      margin = list(l = 10, r = 10, b = 10, t = 10),
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# ----- Serie nacional DANE_POPULATION -----
pob_ts <- tryCatch({
  p_path <- file.path(data_dir, "051_DANE_Proyecciones_P.rds")
  if (!file.exists(p_path)) return(NULL)
  df <- readRDS(p_path)
  
  if (!"ano" %in% names(df)) return(NULL)
  if (!"edad" %in% names(df)) return(NULL)
  if (!"poblacion" %in% names(df) && "poblacion" %in% tolower(names(df))) {
    idx <- which(tolower(names(df)) == "poblacion")[1]
    names(df)[idx] <- "poblacion"
  }
  if (!"poblacion" %in% names(df)) return(NULL)
  
  suppressWarnings({
    df$ano       <- as.integer(df$ano)
    df$edad      <- as.integer(df$edad)
    df$poblacion <- as.numeric(df$poblacion)
  })
  
  df %>%
    dplyr::filter(!is.na(ano), !is.na(edad)) %>%
    dplyr::group_by(ano) %>%
    dplyr::summarise(
      poblacion_total  = sum(poblacion, na.rm = TRUE),
      poblacion_joven  = sum(poblacion[edad <= 14], na.rm = TRUE),
      poblacion_mayor  = sum(poblacion[edad >= 65], na.rm = TRUE),
      poblacion_activa = sum(poblacion[edad >= 15 & edad <= 64], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      razon_dependencia = dplyr::if_else(
        poblacion_activa > 0,
        (poblacion_joven + poblacion_mayor) / poblacion_activa,
        NA_real_
      )
    ) %>%
    dplyr::arrange(ano)
}, error = function(e) NULL)

make_pob_ts_plot <- function(var_code){
  if (is.null(pob_ts) || nrow(pob_ts) == 0) {
    return(empty_ts_plot("Sin datos demográficos disponibles."))
  }
  df <- pob_ts
  var_code <- var_code %||% "poblacion_total"
  
  if (!(var_code %in% names(df))) var_code <- "poblacion_total"
  
  if (var_code == "razon_dependencia") {
    df$valor <- df$razon_dependencia
    ylab     <- "Razón de dependencia (dependientes / activos)"
    custom_vals <- fmt_num_es(df$valor, digits = 2)
    
    rng      <- range(df$valor, na.rm = TRUE)
    tickvals <- pretty(rng, n = 6)
    ticktext <- fmt_num_es(tickvals, digits = 2)
  } else {
    df$valor <- df[[var_code]]
    ylab <- dplyr::case_when(
      var_code == "poblacion_total" ~ "Población total",
      var_code == "poblacion_joven" ~ "Población joven (0–14)",
      var_code == "poblacion_mayor" ~ "Población adulta mayor (65+)",
      TRUE                          ~ "Población"
    )
    custom_vals <- fmt_km(df$valor)
    
    rng      <- range(df$valor, na.rm = TRUE)
    tickvals <- pretty(rng, n = 6)
    tickvals <- tickvals[tickvals >= 0]
    ticktext <- fmt_km(tickvals)
  }
  
  plotly::plot_ly(
    data = df,
    x    = ~ano,
    y    = ~valor,
    type = "scatter",
    mode = "lines+markers",
    line   = list(color = "#2563eb", width = 2),
    marker = list(size = 6, color = "#2563eb"),
    customdata   = custom_vals,
    hovertemplate = if (var_code == "razon_dependencia") {
      "<b>Año:</b> %{x}<br><b>Razón de dependencia:</b> %{customdata}<extra></extra>"
    } else {
      "<b>Año:</b> %{x}<br><b>Población:</b> %{customdata}<extra></extra>"
    }
  ) %>%
    plotly::layout(
      xaxis = list(
        title = "",
        tickmode = "linear",
        dtick = 1,
        showgrid = FALSE
      ),
      yaxis = list(
        title    = ylab,
        tickvals = tickvals,
        ticktext = ticktext,
        showgrid = FALSE
      ),
      margin = list(l = 60, r = 20, t = 30, b = 50),
      hovermode = "x unified",
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# ----- Serie nacional Hansen Cobertura de Bosque -----
hansen_ts <- tryCatch({
  h_path <- file.path(data_dir, "141_HANSEN_COBERTURA_NETA_TOTAL.rds")
  if (!file.exists(h_path)) return(NULL)
  
  df <- readRDS(h_path)
  
  if (!"anio" %in% names(df)) {
    if ("ano" %in% names(df)) {
      df$anio <- as.integer(df$ano)
    } else {
      return(NULL)
    }
  }
  if (!"cobertura_ha" %in% names(df)) {
    if ("cobertura_neta_ha" %in% names(df)) {
      names(df)[names(df) == "cobertura_neta_ha"] <- "cobertura_ha"
    } else {
      return(NULL)
    }
  }
  if (!"base_ha_2000" %in% names(df)) {
    if (!"base_ha_2000" %in% names(df)) return(NULL)
  }
  
  suppressWarnings({
    df$anio         <- as.integer(df$anio)
    df$cobertura_ha <- as.numeric(df$cobertura_ha)
    df$base_ha_2000 <- as.numeric(df$base_ha_2000)
  })
  
  df %>%
    dplyr::filter(!is.na(anio)) %>%
    dplyr::group_by(anio) %>%
    dplyr::summarise(
      cobertura_ha = sum(cobertura_ha, na.rm = TRUE),
      base_ha_2000 = sum(base_ha_2000, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      cobertura_pct = dplyr::if_else(
        base_ha_2000 > 0,
        100 * cobertura_ha / base_ha_2000,
        NA_real_
      )
    ) %>%
    dplyr::arrange(anio)
}, error = function(e) NULL)

make_hansen_ts_plot <- function(metric_code){
  if (is.null(hansen_ts) || nrow(hansen_ts) == 0) {
    return(empty_ts_plot("Sin datos de cobertura de bosque disponibles."))
  }
  
  df <- hansen_ts
  metric_code <- metric_code %||% "pct"
  
  if (metric_code == "ha") {
    df$valor    <- df$cobertura_ha
    ylab        <- "Cobertura neta de bosque (Ha)"
    custom_vals <- fmt_short(df$valor)
    
    y_breaks   <- pretty(df$valor, n = 5)
    y_breaks   <- y_breaks[y_breaks >= 0]
    y_ticktext <- fmt_short(y_breaks)
    
    hover_tmpl <- "<b>Año:</b> %{x}<br><b>Hectáreas:</b> %{customdata}<extra></extra>"
  } else {
    df$valor    <- df$cobertura_pct
    ylab        <- "Cobertura neta de bosque (%)"
    custom_vals <- fmt_pct(df$valor, digits = 2)
    
    min_val <- max(0, floor(min(df$valor, na.rm = TRUE) / 5) * 5)
    max_val <- min(100, ceiling(max(df$valor, na.rm = TRUE) / 5) * 5)
    if (!is.finite(min_val) || !is.finite(max_val) || min_val >= max_val) {
      min_val <- 0; max_val <- 100
    }
    y_breaks   <- seq(min_val, max_val, by = 5)
    y_ticktext <- fmt_pct(y_breaks, digits = 0)
    
    hover_tmpl <- "<b>Año:</b> %{x}<br><b>Porcentaje:</b> %{customdata}<extra></extra>"
  }
  
  plotly::plot_ly(
    data = df,
    x    = ~anio,
    y    = ~valor,
    type = "scatter",
    mode = "lines+markers",
    line   = list(color = "#2E7D32", width = 2),
    marker = list(size = 6, color = "#2E7D32"),
    customdata   = custom_vals,
    hovertemplate = hover_tmpl
  ) %>%
    plotly::layout(
      xaxis = list(
        title = "",
        tickmode = "linear",
        dtick = 1,
        showgrid = FALSE
      ),
      yaxis = list(
        title    = ylab,
        tickvals = y_breaks,
        ticktext = y_ticktext,
        showgrid = FALSE
      ),
      margin = list(l = 60, r = 20, t = 30, b = 50),
      hovermode = "x unified",
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# ----- Serie nacional IDM (promedio nacional y por segmento/categoría) -----
idm_raw <- tryCatch({
  # Ruta principal dentro de COLOMBIA/data
  path_colombia <- file.path(data_dir, "071_DNP_Terridata_IDM.rds")
  # Ruta alternativa: la misma que usa app_idm.R
  path_sninny  <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/DNP-IDM/data/071_DNP_Terridata_IDM.rds"
  
  idm_path <- if (file.exists(path_colombia)) {
    path_colombia
  } else if (file.exists(path_sninny)) {
    path_sninny
  } else {
    return(NULL)
  }
  
  df <- readRDS(idm_path)
  nms <- names(df)
  
  pick_col_simple <- function(cands){
    for (pat in cands) {
      idx <- which(tolower(nms) == tolower(pat) |
                     grepl(pat, nms, ignore.case = TRUE))[1]
      if (!is.na(idx) && idx > 0) return(nms[idx])
    }
    NA_character_
  }
  
  col_ano <- pick_col_simple(c("ano","anio","year"))
  col_val <- pick_col_simple(c("valor","idm","indice"))
  # Intentamos detectar una columna de segmentación (cuadrantes, grupos, etc.)
  col_seg <- pick_col_simple(c("cuadrante","segmento","grupo","cluster","cuartil_idm"))
  
  if (is.na(col_ano) || is.na(col_val)) return(NULL)
  
  suppressWarnings({
    df[[col_ano]] <- as.integer(df[[col_ano]])
    df[[col_val]] <- as.numeric(df[[col_val]])
  })
  
  segmento <- if (!is.na(col_seg) && col_seg %in% names(df)) {
    as.character(df[[col_seg]])
  } else {
    NA_character_
  }
  
  tibble::tibble(
    ano        = df[[col_ano]],
    valor_bruto = df[[col_val]],
    segmento   = segmento
  ) %>%
    dplyr::filter(!is.na(ano), !is.na(valor_bruto))
}, error = function(e) NULL)

idm_seg_choices <- {
  if (is.null(idm_raw) || !nrow(idm_raw)) {
    c("Sin datos de IDM" = "__ALL__")
  } else if ("segmento" %in% names(idm_raw) && any(!is.na(idm_raw$segmento))) {
    seg_vals <- sort(unique(idm_raw$segmento[!is.na(idm_raw$segmento)]))
    c("Promedio nacional (todos los municipios)" = "__ALL__",
      stats::setNames(seg_vals, seg_vals))
  } else {
    c("Promedio nacional (todos los municipios)" = "__ALL__")
  }
}

make_idm_ts_plot <- function(segmento = "__ALL__"){
  if (is.null(idm_raw) || !nrow(idm_raw)) {
    return(empty_ts_plot("Sin datos del IDM disponibles."))
  }
  
  df <- idm_raw
  
  # Filtro por segmento / cuadrante si aplica
  if (!is.null(segmento) && segmento != "__ALL__" &&
      "segmento" %in% names(df)) {
    df <- df %>% dplyr::filter(.data$segmento == segmento)
  }
  
  if (!nrow(df)) {
    return(empty_ts_plot("Sin observaciones del IDM para el filtro seleccionado."))
  }
  
  # Promedio por año
  df <- df %>%
    dplyr::group_by(ano) %>%
    dplyr::summarise(
      valor = mean(valor_bruto, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (!nrow(df)) return(empty_ts_plot("Sin datos del IDM disponibles."))
  
  # Detectar si el índice está en 0–1 y convertir a % solo en ese caso
  max_val <- max(df$valor, na.rm = TRUE)
  is_pct_scale <- is.finite(max_val) && max_val <= 1.5
  
  if (is_pct_scale) {
    df$valor <- df$valor * 100
  }
  # si ya viene en 0–100, lo dejamos igual
  
  rng <- range(df$valor, na.rm = TRUE)
  if (!all(is.finite(rng))) rng <- c(0, 100)
  y_breaks <- pretty(rng, n = 6)
  y_breaks <- y_breaks[y_breaks >= 0 & y_breaks <= max(100, max(y_breaks, na.rm = TRUE))]
  if (length(y_breaks) == 0) y_breaks <- c(0, 25, 50, 75, 100)
  y_ticktext <- fmt_pct(y_breaks, digits = 0)
  
  custom_vals <- fmt_pct(df$valor, digits = 1)
  
  plotly::plot_ly(
    data = df,
    x    = ~ano,
    y    = ~valor,
    type = "scatter",
    mode = "lines+markers",
    line   = list(color = "#111827", width = 2),
    marker = list(size = 6, color = "#111827"),
    customdata   = custom_vals,
    hovertemplate = "<b>Año:</b> %{x}<br><b>IDM promedio:</b> %{customdata}<extra></extra>"
  ) %>%
    plotly::layout(
      xaxis = list(
        title    = "",
        tickmode = "linear",
        dtick   = 1,
        showgrid = FALSE
      ),
      yaxis = list(
        title    = "IDM promedio (%)",
        tickvals = y_breaks,
        ticktext = y_ticktext,
        showgrid = FALSE
      ),
      margin = list(l = 60, r = 20, t = 30, b = 50),
      hovermode = "x unified",
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# ----- Barras nacionales UPRA_FA: total ha vs ha en FA -----
upra_bar_stats <- tryCatch({
  upra_path <- file.path(data_dir, "013_UPRA_FA_Proporcion FA_Total municipal.rds")
  if (!file.exists(upra_path)) return(NULL)
  df <- readRDS(upra_path)
  
  nms <- names(df)
  pick_col_simple <- function(cands){
    for (pat in cands) {
      idx <- which(tolower(nms) == tolower(pat) |
                     grepl(pat, nms, ignore.case = TRUE))[1]
      if (length(idx) && !is.na(idx)) return(nms[idx])
    }
    NA_character_
  }
  col_ano      <- pick_col_simple(c("ano","anio","year"))
  col_area_fa  <- pick_col_simple(c("area_fa_ha","area_fa","fa_ha"))
  col_area_tot <- pick_col_simple(c("area_mpio_ha","area_total_ha","area_municipio"))
  
  if (is.na(col_ano) || is.na(col_area_fa) || is.na(col_area_tot)) return(NULL)
  
  suppressWarnings({
    df[[col_ano]]      <- as.integer(df[[col_ano]])
    df[[col_area_fa]]  <- as.numeric(df[[col_area_fa]])
    df[[col_area_tot]] <- as.numeric(df[[col_area_tot]])
  })
  
  df_agg <- df %>%
    dplyr::filter(!is.na(.data[[col_ano]])) %>%
    dplyr::group_by(.data[[col_ano]]) %>%
    dplyr::summarise(
      area_fa_ha  = sum(.data[[col_area_fa]],  na.rm = TRUE),
      area_tot_ha = sum(.data[[col_area_tot]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(area_tot_ha > 0) %>%
    dplyr::arrange(dplyr::desc(.data[[col_ano]]))
  
  if (!nrow(df_agg)) return(NULL)
  df_agg <- df_agg[1, ] # último año disponible
  df_agg$ano_ref <- df_agg[[col_ano]]
  df_agg
}, error = function(e) NULL)

make_upra_bar_plot <- function(){
  if (is.null(upra_bar_stats) || nrow(upra_bar_stats) == 0) {
    return(empty_ts_plot("Sin datos de Frontera Agropecuaria disponibles."))
  }
  
  st <- upra_bar_stats[1, ]
  vals <- c(st$area_tot_ha, st$area_fa_ha)
  cats <- c("Área total del territorio (ha)",
            "Área en Frontera Agropecuaria (ha)")
  
  share <- if (is.finite(st$area_tot_ha) && st$area_tot_ha > 0)
    st$area_fa_ha / st$area_tot_ha else NA_real_
  share_pct_txt <- if (is.finite(share)) fmt_pct(share * 100, digits = 1) else "NA"
  
  df_plot <- tibble(
    categoria = factor(cats, levels = cats),
    valor     = vals
  )
  
  txt_vals <- fmt_short(df_plot$valor)
  
  plotly::plot_ly(
    data = df_plot,
    x    = ~categoria,
    y    = ~valor,
    type = "bar",
    marker = list(color = c("#a5d8ff", "#0077b6")),
    text  = txt_vals,
    textposition = "inside",
    texttemplate = "%{text}",
    insidetextanchor = "middle",
    insidetextfont = list(
      family = "Inter SemiBold, Arial, sans-serif",
      size   = 12,
      color  = "white"
    ),
    hovertemplate = paste0(
      "<b>%{x}</b><br>",
      "Hectáreas: %{customdata}<extra></extra>"
    ),
    customdata = txt_vals
  ) %>%
    plotly::layout(
      title = list(
        text = paste0(
          "Distribución de tierras aptas (año ", st$ano_ref, ").  ",
          "La Frontera Agropecuaria equivale a ", share_pct_txt,
          " del territorio."
        ),
        x = 0.5
      ),
      xaxis = list(
        title    = "",
        showgrid = FALSE
      ),
      yaxis = list(
        title    = "Hectáreas",
        showgrid = FALSE
      ),
      margin = list(l = 60, r = 20, t = 60, b = 80),
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# ----- Barras nacionales UPRA_APADT: total ha vs ha potencial riego/drenaje -----
apadt_bar_stats <- tryCatch({
  apadt_path <- file.path(data_dir, "014_UPRA_APADT.rds")
  if (!file.exists(apadt_path)) return(NULL)
  df <- readRDS(apadt_path)
  
  nms <- names(df)
  pick_col_simple <- function(cands){
    for (pat in cands) {
      idx <- which(tolower(nms) == tolower(pat) |
                     grepl(pat, nms, ignore.case = TRUE))[1]
      if (length(idx) && !is.na(idx)) return(nms[idx])
    }
    NA_character_
  }
  col_ano       <- pick_col_simple(c("ano","anio","year"))
  col_area_pot  <- pick_col_simple(c("area_apadt_ha","area_pot_riego_ha","area_pot_dren_ha",
                                     "area_potencial_ha","area_potencial_riego_dren_ha"))
  col_area_tot  <- pick_col_simple(c("area_mpio_ha","area_total_ha","area_municipio"))
  
  if (is.na(col_ano) || is.na(col_area_pot) || is.na(col_area_tot)) return(NULL)
  
  suppressWarnings({
    df[[col_ano]]      <- as.integer(df[[col_ano]])
    df[[col_area_pot]] <- as.numeric(df[[col_area_pot]])
    df[[col_area_tot]] <- as.numeric(df[[col_area_tot]])
  })
  
  df_agg <- df %>%
    dplyr::filter(!is.na(.data[[col_ano]])) %>%
    dplyr::group_by(.data[[col_ano]]) %>%
    dplyr::summarise(
      area_pot_ha = sum(.data[[col_area_pot]], na.rm = TRUE),
      area_tot_ha = sum(.data[[col_area_tot]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(area_tot_ha > 0) %>%
    dplyr::arrange(dplyr::desc(.data[[col_ano]]))
  
  if (!nrow(df_agg)) return(NULL)
  df_agg <- df_agg[1, ] # último año disponible
  df_agg$ano_ref <- df_agg[[col_ano]]
  df_agg
}, error = function(e) NULL)

make_apadt_bar_plot <- function(){
  if (is.null(apadt_bar_stats) || nrow(apadt_bar_stats) == 0) {
    return(empty_ts_plot("Sin datos de área potencial de riego/drenaje disponibles."))
  }
  
  st <- apadt_bar_stats[1, ]
  vals <- c(st$area_tot_ha, st$area_pot_ha)
  cats <- c("Área total del territorio (ha)",
            "Área potencial de riego/drenaje (ha)")
  
  share <- if (is.finite(st$area_tot_ha) && st$area_tot_ha > 0)
    st$area_pot_ha / st$area_tot_ha else NA_real_
  share_pct_txt <- if (is.finite(share)) fmt_pct(share * 100, digits = 1) else "NA"
  
  df_plot <- tibble(
    categoria = factor(cats, levels = cats),
    valor     = vals
  )
  
  txt_vals <- fmt_short(df_plot$valor)
  
  plotly::plot_ly(
    data = df_plot,
    x    = ~categoria,
    y    = ~valor,
    type = "bar",
    marker = list(color = c("#ffe29a", "#f59e0b")),
    text  = txt_vals,
    textposition = "inside",
    texttemplate = "%{text}",
    insidetextanchor = "middle",
    insidetextfont = list(
      family = "Inter SemiBold, Arial, sans-serif",
      size   = 12,
      color  = "white"
    ),
    hovertemplate = paste0(
      "<b>%{x}</b><br>",
      "Hectáreas: %{customdata}<extra></extra>"
    ),
    customdata = txt_vals
  ) %>%
    plotly::layout(
      title = list(
        text = paste0(
          "Área potencial de riego o drenaje (año ", st$ano_ref, ").  ",
          "El área potencial equivale a ", share_pct_txt,
          " del territorio."
        ),
        x = 0.5
      ),
      xaxis = list(
        title    = "",
        showgrid = FALSE
      ),
      yaxis = list(
        title    = "Hectáreas",
        showgrid = FALSE
      ),
      margin = list(l = 60, r = 20, t = 60, b = 80),
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# ---------------- UI ----------------
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    primary = "#2563eb",
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight")
  ),
  tags$head(
    tags$style(HTML("
    :root{
      --ecv-bdr:#f57c00;
    }

    body{
      background:#ffffff;
      font-family:'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
    }

    .wrap{
      max-width:1200px;
      margin:0 auto;
      padding:18px 22px 36px;
    }

    .filters{
      background:#fff;
      border:1px solid var(--ecv-bdr);
      border-radius:16px;
      padding:10px 12px;
      margin-bottom:10px;
      box-shadow:0 2px 6px rgba(0,0,0,.03);
    }

    .filters-grid-1{
      display:grid;
      grid-template-columns:1fr;
      gap:10px;
      align-items:stretch;
    }

    .filters-grid-2{
      display:grid;
      grid-template-columns:1fr 1fr;
      gap:10px;
      align-items:end;
    }

    .filter-label{
      font-family:'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      font-size:13px;
      font-weight:500;
      letter-spacing:.2px;
      text-transform:none !important;
      color:#111827;
      margin-bottom:4px;
    }

    .filters .shiny-input-container{
      margin-bottom:0 !important;
      width:100% !important;
    }

    .filters .form-select{
      border:2px solid var(--ecv-bdr) !important;
      border-radius:10px !important;
      background-color:#fff !important;
      box-shadow:none !important;
      padding-top:0.25rem;
      padding-bottom:0.25rem;
      height:46px;
      min-height:46px;
      color:#111827;
    }

    .filters .form-select:focus{
      border-color:var(--ecv-bdr) !important;
      box-shadow:0 0 0 0.15rem rgba(245,124,0,.35) !important;
    }

    .card{
      background:#fff;
      border:1px solid var(--ecv-bdr);
      border-radius:18px;
      padding:10px 12px 12px;
      box-shadow:0 2px 10px rgba(0,0,0,.05);
      display:flex;
      flex-direction:column;
    }

    .card-title-small{
      font-weight:600;
      font-size:13px;
      text-transform:uppercase;
      letter-spacing:.05em;
      color:#6b7280;
      margin-bottom:4px;
    }

    .grid-2x2{
      display:grid;
      grid-template-columns:1fr 1fr;
      grid-auto-rows:minmax(360px, auto);
      gap:16px;
      align-items:stretch;
    }

    .placeholder-box{
      border:1.5px dashed #d1d5db;
      border-radius:12px;
      height:200px;
      min-height:180px;
      display:flex;
      align-items:center;
      justify-content:center;
      font-size:13px;
      color:#9ca3af;
      padding:8px;
      text-align:center;
      background:#f9fafb;
    }

    .nav-tabs .nav-link { font-weight:600; }
    .nav-tabs .nav-link.active { font-weight:700; }

    @media (max-width:900px){
      .grid-2x2{
        grid-template-columns:1fr;
      }
      .filters-grid-2{
        grid-template-columns:1fr;
      }
    }
  "))
  ),
  div(
    class = "wrap",
    h3("SNINNY — Tablero 2×2 con pestañas por dimensión"),
    
    div(
      class = "grid-2x2",
      
      # -------- Cuadrante 1 --------
      div(
        class = "card",
        div(class = "card-title-small", "Cuadrante 1"),
        tabsetPanel(
          id = "q1_tabs",
          tabPanel("Estructura",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Estructura analizamos?"),
                               selector_base("q1_estr_base", "Estructura")
                           ),
                           uiOutput("q1_estr_extra")
                       )
                   ),
                   plotlyOutput("q1_estr_plot", height = "360px")
          ),
          tabPanel("Dinámica",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Dinámica analizamos?"),
                               selector_base("q1_din_base", "Dinámica")
                           )
                       )
                   ),
                   div(class = "placeholder-box",
                       "Objeto visual vacío — Cuadrante 1 · Dinámica"
                   )
          ),
          tabPanel("Importancia",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Importancia analizamos?"),
                               selector_base("q1_imp_base", "Importancia")
                           ),
                           uiOutput("q1_imp_extra")
                       )
                   ),
                   div(class = "placeholder-box",
                       "Objeto visual vacío — Cuadrante 1 · Importancia"
                   )
          ),
          tabPanel("Vulnerabilidad",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Vulnerabilidad analizamos?"),
                               selector_base("q1_vul_base", "Vulnerabilidad")
                           )
                       )
                   ),
                   div(class = "placeholder-box",
                       "Objeto visual vacío — Cuadrante 1 · Vulnerabilidad"
                   )
          )
        )
      ),
      
      # -------- Cuadrante 2 --------
      div(
        class = "card",
        div(class = "card-title-small", "Cuadrante 2"),
        tabsetPanel(
          id = "q2_tabs",
          tabPanel("Estructura",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Estructura analizamos?"),
                               selector_base("q2_estr_base", "Estructura")
                           ),
                           uiOutput("q2_estr_extra")
                       )
                   ),
                   plotlyOutput("q2_estr_plot", height = "360px")
          ),
          tabPanel("Dinámica",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Dinámica analizamos?"),
                               selector_base("q2_din_base", "Dinámica")
                           )
                       )
                   ),
                   div(class = "placeholder-box",
                       "Objeto visual vacío — Cuadrante 2 · Dinámica"
                   )
          ),
          tabPanel("Importancia",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Importancia analizamos?"),
                               selector_base("q2_imp_base", "Importancia")
                           ),
                           uiOutput("q2_imp_extra")
                       )
                   ),
                   div(class = "placeholder-box",
                       "Objeto visual vacío — Cuadrante 2 · Importancia"
                   )
          ),
          tabPanel("Vulnerabilidad",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Vulnerabilidad analizamos?"),
                               selector_base("q2_vul_base", "Vulnerabilidad")
                           )
                       )
                   ),
                   div(class = "placeholder-box",
                       "Objeto visual vacío — Cuadrante 2 · Vulnerabilidad"
                   )
          )
        )
      ),
      
      # -------- Cuadrante 3 --------
      div(
        class = "card",
        div(class = "card-title-small", "Cuadrante 3"),
        tabsetPanel(
          id = "q3_tabs",
          tabPanel("Estructura",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Estructura analizamos?"),
                               selector_base("q3_estr_base", "Estructura")
                           ),
                           uiOutput("q3_estr_extra")
                       )
                   ),
                   plotlyOutput("q3_estr_plot", height = "360px")
          ),
          tabPanel("Dinámica",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Dinámica analizamos?"),
                               selector_base("q3_din_base", "Dinámica")
                           )
                       )
                   ),
                   div(class = "placeholder-box",
                       "Objeto visual vacío — Cuadrante 3 · Dinámica"
                   )
          ),
          tabPanel("Importancia",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Importancia analizamos?"),
                               selector_base("q3_imp_base", "Importancia")
                           ),
                           uiOutput("q3_imp_extra")
                       )
                   ),
                   div(class = "placeholder-box",
                       "Objeto visual vacío — Cuadrante 3 · Importancia"
                   )
          ),
          tabPanel("Vulnerabilidad",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Vulnerabilidad analizamos?"),
                               selector_base("q3_vul_base", "Vulnerabilidad")
                           )
                       )
                   ),
                   div(class = "placeholder-box",
                       "Objeto visual vacío — Cuadrante 3 · Vulnerabilidad"
                   )
          )
        )
      ),
      
      # -------- Cuadrante 4 --------
      div(
        class = "card",
        div(class = "card-title-small", "Cuadrante 4"),
        tabsetPanel(
          id = "q4_tabs",
          tabPanel("Estructura",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Estructura analizamos?"),
                               selector_base("q4_estr_base", "Estructura")
                           ),
                           uiOutput("q4_estr_extra")
                       )
                   ),
                   plotlyOutput("q4_estr_plot", height = "360px")
          ),
          tabPanel("Dinámica",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Dinámica analizamos?"),
                               selector_base("q4_din_base", "Dinámica")
                           )
                       )
                   ),
                   div(class = "placeholder-box",
                       "Objeto visual vacío — Cuadrante 4 · Dinámica"
                   )
          ),
          tabPanel("Importancia",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Importancia analizamos?"),
                               selector_base("q4_imp_base", "Importancia")
                           ),
                           uiOutput("q4_imp_extra")
                       )
                   ),
                   div(class = "placeholder-box",
                       "Objeto visual vacío — Cuadrante 4 · Importancia"
                   )
          ),
          tabPanel("Vulnerabilidad",
                   div(class = "filters",
                       div(class = "filters-grid-1",
                           div(lbl("¿Qué base de Vulnerabilidad analizamos?"),
                               selector_base("q4_vul_base", "Vulnerabilidad")
                           )
                       )
                   ),
                   div(class = "placeholder-box",
                       "Objeto visual vacío — Cuadrante 4 · Vulnerabilidad"
                   )
          )
        )
      )
    )
  )
)

# ---------------- SERVER ----------------
server <- function(input, output, session){
  
  # Helper EVA_A (Importancia) ----
  render_imp_extra <- function(base_input_id, output_id_prefix){
    output[[output_id_prefix]] <- renderUI({
      base_sel <- input[[base_input_id]]
      if (is.null(base_sel) || base_sel != "EVA_A") {
        return(NULL)
      }
      tagList(
        div(
          class = "filters-grid-2",
          div(
            lbl("¿Qué tipo de cultivo analizamos?"),
            selectInput(
              paste0(output_id_prefix, "_tipo"),
              label   = NULL,
              choices = eva_tipos_choices,
              selected = "Todos"
            )
          ),
          div(
            lbl("¿Cuál cultivo analizamos?"),
            selectInput(
              paste0(output_id_prefix, "_cultivo"),
              label   = NULL,
              choices = eva_cultivo_choices_fun("Todos"),
              selected = "Todos"
            )
          )
        ),
        div(
          lbl("Variable a considerar"),
          selectInput(
            paste0(output_id_prefix, "_indicador"),
            label   = NULL,
            choices = eva_indicador_choices,
            selected = "area_sembrada_ha"
          )
        )
      )
    })
  }
  
  render_imp_extra("q1_imp_base", "q1_imp_extra")
  render_imp_extra("q2_imp_base", "q2_imp_extra")
  render_imp_extra("q3_imp_base", "q3_imp_extra")
  render_imp_extra("q4_imp_base", "q4_imp_extra")
  
  setup_tipo_observer <- function(prefix){
    observeEvent(input[[paste0(prefix, "_tipo")]], {
      tipo_sel <- input[[paste0(prefix, "_tipo")]]
      if (is.null(tipo_sel)) return()
      updateSelectInput(
        session,
        inputId = paste0(prefix, "_cultivo"),
        choices = eva_cultivo_choices_fun(tipo_sel),
        selected = "Todos"
      )
    }, ignoreInit = TRUE)
  }
  
  setup_tipo_observer("q1_imp_extra")
  setup_tipo_observer("q2_imp_extra")
  setup_tipo_observer("q3_imp_extra")
  setup_tipo_observer("q4_imp_extra")
  
  # -------- Extra UI para Estructura (Transición demográfica / Cobertura bosque / IDM) ----
  render_estr_extra <- function(base_input_id, output_id_prefix){
    output[[output_id_prefix]] <- renderUI({
      base_sel <- input[[base_input_id]]
      if (is.null(base_sel)) return(NULL)
      
      if (base_sel == "DANE_POPULATION") {
        tagList(
          div(
            lbl("Variable demográfica a considerar"),
            selectInput(
              paste0(output_id_prefix, "_var"),
              label   = NULL,
              choices = pob_var_choices,
              selected = "poblacion_total"
            )
          )
        )
      } else if (base_sel == "HANSEN_COBERTURA_BOSQUE") {
        tagList(
          div(
            lbl("Indicador de cobertura de bosque"),
            selectInput(
              paste0(output_id_prefix, "_metric"),
              label   = NULL,
              choices = hansen_metric_choices,
              selected = "pct"
            )
          )
        )
      } else if (base_sel == "DNP-IDM") {
        # Filtro de segmento para la serie temporal del IDM
        tagList(
          div(
            lbl("Filtro para el IDM (segmento / cuadrante)"),
            selectInput(
              paste0(output_id_prefix, "_segmento"),
              label   = NULL,
              choices = idm_seg_choices,
              selected = "__ALL__"
            )
          )
        )
      } else {
        NULL
      }
    })
  }
  
  render_estr_extra("q1_estr_base", "q1_estr_extra")
  render_estr_extra("q2_estr_base", "q2_estr_extra")
  render_estr_extra("q3_estr_base", "q3_estr_extra")
  render_estr_extra("q4_estr_base", "q4_estr_extra")
  
  # -------- Series / barras por cuadrante (solo nacional / IDM filtrable) --------
  
  output$q1_estr_plot <- renderPlotly({
    base_sel <- input$q1_estr_base
    if (is.null(base_sel)) {
      return(empty_ts_plot("Objeto visual vacío — seleccione un sistema con serie temporal o barras."))
    }
    
    if (base_sel == "DANE_POPULATION") {
      var_sel <- input$q1_estr_extra_var
      make_pob_ts_plot(var_sel %||% "poblacion_total")
      
    } else if (base_sel == "HANSEN_COBERTURA_BOSQUE") {
      metric_sel <- input$q1_estr_extra_metric %||% "pct"
      make_hansen_ts_plot(metric_sel)
      
    } else if (base_sel == "DNP-IDM") {
      seg_sel <- input$q1_estr_extra_segmento %||% "__ALL__"
      make_idm_ts_plot(seg_sel)
      
    } else if (base_sel == "UPRA_FA") {
      make_upra_bar_plot()
      
    } else if (base_sel == "UPRA_APADT") {
      make_apadt_bar_plot()
      
    } else {
      empty_ts_plot("Objeto visual vacío — seleccione un sistema con serie temporal o barras.")
    }
  })
  
  output$q2_estr_plot <- renderPlotly({
    base_sel <- input$q2_estr_base
    if (is.null(base_sel)) {
      return(empty_ts_plot("Objeto visual vacío — seleccione un sistema con serie temporal o barras."))
    }
    
    if (base_sel == "DANE_POPULATION") {
      var_sel <- input$q2_estr_extra_var
      make_pob_ts_plot(var_sel %||% "poblacion_total")
      
    } else if (base_sel == "HANSEN_COBERTURA_BOSQUE") {
      metric_sel <- input$q2_estr_extra_metric %||% "pct"
      make_hansen_ts_plot(metric_sel)
      
    } else if (base_sel == "DNP-IDM") {
      seg_sel <- input$q2_estr_extra_segmento %||% "__ALL__"
      make_idm_ts_plot(seg_sel)
      
    } else if (base_sel == "UPRA_FA") {
      make_upra_bar_plot()
      
    } else if (base_sel == "UPRA_APADT") {
      make_apadt_bar_plot()
      
    } else {
      empty_ts_plot("Objeto visual vacío — seleccione un sistema con serie temporal o barras.")
    }
  })
  
  output$q3_estr_plot <- renderPlotly({
    base_sel <- input$q3_estr_base
    if (is.null(base_sel)) {
      return(empty_ts_plot("Objeto visual vacío — seleccione un sistema con serie temporal o barras."))
    }
    
    if (base_sel == "DANE_POPULATION") {
      var_sel <- input$q3_estr_extra_var
      make_pob_ts_plot(var_sel %||% "poblacion_total")
      
    } else if (base_sel == "HANSEN_COBERTURA_BOSQUE") {
      metric_sel <- input$q3_estr_extra_metric %||% "pct"
      make_hansen_ts_plot(metric_sel)
      
    } else if (base_sel == "DNP-IDM") {
      seg_sel <- input$q3_estr_extra_segmento %||% "__ALL__"
      make_idm_ts_plot(seg_sel)
      
    } else if (base_sel == "UPRA_FA") {
      make_upra_bar_plot()
      
    } else if (base_sel == "UPRA_APADT") {
      make_apadt_bar_plot()
      
    } else {
      empty_ts_plot("Objeto visual vacío — seleccione un sistema con serie temporal o barras.")
    }
  })
  
  output$q4_estr_plot <- renderPlotly({
    base_sel <- input$q4_estr_base
    if (is.null(base_sel)) {
      return(empty_ts_plot("Objeto visual vacío — seleccione un sistema con serie temporal o barras."))
    }
    
    if (base_sel == "DANE_POPULATION") {
      var_sel <- input$q4_estr_extra_var
      make_pob_ts_plot(var_sel %||% "poblacion_total")
      
    } else if (base_sel == "HANSEN_COBERTURA_BOSQUE") {
      metric_sel <- input$q4_estr_extra_metric %||% "pct"
      make_hansen_ts_plot(metric_sel)
      
    } else if (base_sel == "DNP-IDM") {
      seg_sel <- input$q4_estr_extra_segmento %||% "__ALL__"
      make_idm_ts_plot(seg_sel)
      
    } else if (base_sel == "UPRA_FA") {
      make_upra_bar_plot()
      
    } else if (base_sel == "UPRA_APADT") {
      make_apadt_bar_plot()
      
    } else {
      empty_ts_plot("Objeto visual vacío — seleccione un sistema con serie temporal o barras.")
    }
  })
  
}

shinyApp(ui, server)

