# app.R — COLOMBIA_ESTRUCTURA (APP COMPLETA)
# - ICA unificado (robusto aunque falten archivos)
# - IDM ajustado a tu estructura municipal (COD_DANE_DPTO_D + valor)
# - ✅ POBLACIÓN RECONFIGURADA: usa `quinquenio` para joven/activa/mayor y razón de dependencia
# - Rutas robustas (funciona con runApp() y source())
# -------------------------------------------------------------------

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(dplyr); library(tibble); library(stringi); library(htmltools)
  library(plotly); library(scales)
  library(sf); library(leaflet)
  library(DT)
  library(tidyr)
})

options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---------------- Rutas (robustas) ----------------
app_root <- tryCatch({
  of <- sys.frame(1)$ofile
  if (!is.null(of)) dirname(normalizePath(of, winslash = "/", mustWork = TRUE))
  else normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}, error = function(e){
  tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e2) getwd())
})

data_dir <- file.path(app_root, "data")

# =========================================================
# ALTURAS (plots/mapas)
# =========================================================
H_PLOT <- 650
H_MAP  <- 600

# =========================================================
# Colores globales UI + plots Pestaña 1
# =========================================================
COL_UI   <- "#99d5ec"
COL_PLOT <- "#1F77B4"

PLOTLY_HOVERLABEL <- list(
  bgcolor     = "#ffffff",
  bordercolor = "rgba(17,24,39,0.25)",
  font        = list(color = "#111827", size = 12)
)

# =========================================================
# Helpers robustos
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

pick_col_simple <- function(nms, cands){
  for (pat in cands) {
    idx <- which(tolower(nms) == tolower(pat) | grepl(pat, nms, ignore.case = TRUE))[1]
    if (length(idx) && !is.na(idx)) return(nms[idx])
  }
  NA_character_
}

pick_year_col <- function(nms){
  pick_col_simple(nms, c("ano","año","anio","year","anio_ref","anio_cosecha","año_ref"))
}

pick_dep_col <- function(nms){
  cands <- c(
    "dpto_ccdgo","cod_dpto","cod_depto","dpto_cod",
    "cod_dane_dpto","cod_dane_dep","cod_dane","codigo","cod_dep",
    "COD_DANE_DPTO_D","COD_DANE_DPTO","cod_dane_dpto_d"
  )
  pick_col_simple(nms, cands)
}

pick_dep_name_col <- function(nms){
  cands <- c(
    "dpto_cnmb","nom_dpto","nombre_dpto","nombre_depto","departamen","depto","nom_dep",
    "dpto_nom","DEPARTAMENTO_D","departamento"
  )
  pick_col_simple(nms, cands)
}

# =========================================================
# ✅ Helpers POBLACIÓN (quinquenio)
# =========================================================
pick_quinquenio_col <- function(nms){
  pick_col_simple(nms, c(
    "quinquenio","quinquenio_edad","grupo_edad","grupoedad","rango_edad","edad_quinquenio"
  ))
}

parse_quinquenio_bounds <- function(q){
  # Devuelve lower, upper (upper puede ser Inf)
  if (is.null(q)) return(list(lower = NA_real_, upper = NA_real_))
  q <- as.character(q)
  q <- stringi::stri_trim_both(q)
  q <- tolower(q)
  q <- gsub("\\s+", "", q)
  q <- gsub("años|anos", "", q)
  
  # Ej: "80ymas", "80ymás", "80+", "80mas"
  if (grepl("\\+", q) || grepl("ymas|ymás|mas|más", q)) {
    lo <- suppressWarnings(as.numeric(stringr::str_extract(q, "[0-9]+")))
    return(list(lower = lo, upper = Inf))
  }
  
  # Ej: "15-19"
  if (grepl("-", q)) {
    parts <- strsplit(q, "-", fixed = TRUE)[[1]]
    lo <- suppressWarnings(as.numeric(parts[1]))
    up <- suppressWarnings(as.numeric(parts[2]))
    return(list(lower = lo, upper = up))
  }
  
  # Fallback: un solo número
  lo <- suppressWarnings(as.numeric(stringr::str_extract(q, "[0-9]+")))
  list(lower = lo, upper = lo)
}

quinquenio_to_group <- function(q){
  b <- parse_quinquenio_bounds(q)
  lo <- b$lower; up <- b$upper
  if (is.na(lo)) return(NA_character_)
  if (is.finite(up) && up <= 14) return("joven")
  if (!is.finite(up) && lo >= 65) return("mayor")
  if (is.finite(up) && lo >= 65) return("mayor")
  if (is.finite(up) && lo >= 15 && up <= 64) return("activa")
  NA_character_
}

# =========================================================
# Formatos numéricos (ES) — 1 decimal + K/M
# =========================================================
fmt_num_es <- function(x, digits = 1){
  scales::number(x, accuracy = 10^-digits, big.mark = ".", decimal.mark = ",")
}

fmt_km_any <- function(x, digits = 1){
  vapply(x, function(v){
    if (is.na(v) || !is.finite(v)) return(NA_character_)
    av <- abs(v)
    if (av >= 1e6) {
      paste0(fmt_num_es(v/1e6, digits), "M")
    } else if (av >= 1e3) {
      paste0(fmt_num_es(v/1e3, digits), "K")
    } else {
      fmt_num_es(v, digits)
    }
  }, FUN.VALUE = character(1))
}

fmt_short <- function(x) fmt_km_any(x, digits = 1)
fmt_pct   <- function(x, digits = 1) paste0(fmt_num_es(x, digits), "%")

safe_growth <- function(curr, prev){
  if (is.na(curr) || is.na(prev) || !is.finite(curr) || !is.finite(prev) || prev == 0) return(NA_real_)
  (curr - prev) / prev
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

# =========================================================
# Ticks eje X
# - General: 2 en 2
# - SOLO demografía: 5 en 5
# =========================================================
year_ticks_2 <- function(years){
  yrs <- sort(unique(as.integer(years[is.finite(years)])))
  if (!length(yrs)) return(NULL)
  y_min <- min(yrs); y_max <- max(yrs)
  tick0 <- floor(y_min/2)*2
  ticks <- seq(tick0, y_max, by = 2)
  if (tail(ticks, 1) != y_max) ticks <- c(ticks, y_max)
  ticks
}

year_ticks_5 <- function(years){
  yrs <- sort(unique(as.integer(years[is.finite(years)])))
  if (!length(yrs)) return(NULL)
  y_min <- min(yrs); y_max <- max(yrs)
  tick0 <- floor(y_min/5)*5
  ticks <- seq(tick0, y_max, by = 5)
  if (tail(ticks, 1) != y_max) ticks <- c(ticks, y_max)
  ticks
}

# =========================================================
# Forzar color en Pestaña 1 (líneas/barras) a COL_PLOT
# =========================================================
force_plot_color <- function(p, col = COL_PLOT){
  if (is.null(p) || is.null(p$x) || is.null(p$x$data)) return(p)
  
  for (i in seq_along(p$x$data)) {
    tr <- p$x$data[[i]]
    
    if (!is.null(tr$type) && tr$type == "bar") {
      if (is.null(tr$marker)) tr$marker <- list()
      tr$marker$color <- col
    }
    
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

# =========================================================
# Sistemas + Indicadores (Estructura) - ICA COMO UNA SOLA BASE
# =========================================================
sistemas_df <- tibble::tribble(
  ~sistema,                   ~archivos,
  "DANE_POPULATION",          list("051_DANE_Proyecciones_P.rds"),
  "DNP-IDM",                  list("071_DNP_Terridata_IDM.rds"),
  "HANSEN_COBERTURA_BOSQUE",  list("141_HANSEN_COBERTURA_NETA_TOTAL.rds"),
  "UPRA_APADT",               list("014_UPRA_APADT.rds"),
  "UPRA_FA",                  list("013_UPRA_FA_Proporcion FA_Total municipal.rds"),
  "UPRA_EVA_A",               list("011_UPRA_EVA-A.rds"),
  "ICA_CENSO_PECUARIO",       list(
    "101_ICA_CensoPecuario-Bovino.rds",
    "102_ICA_CensoPecuario-Porcino.rds",
    "103_ICA_CensoPecuario-BCOE.rds",
    "104_ICA_CensoPecuario-Aviar.rds"
  )
) %>%
  mutate(rutas = lapply(archivos, function(v) file.path(data_dir, v)))

indicadores_df <- tibble::tribble(
  ~titulo,                                                                 ~dimension,   ~sistema,
  "Transición demográfica territorial",                                   "Estructura", "DANE_POPULATION",
  "Capacidad institucional del territorio según el Índice de Desempeño Municipal (IDM)", "Estructura", "DNP-IDM",
  "Cobertura de bosque en Colombia: evidencia territorial para priorizar la conservación", "Estructura", "HANSEN_COBERTURA_BOSQUE",
  "Planificación productiva: Área potencial de riego o drenaje para las actividades agropecuarias", "Estructura", "UPRA_APADT",
  "Planificación productiva: Ordenamiento a partir de la frontera agropecuaria con enfoque territorial", "Estructura", "UPRA_FA",
  "Condiciones productivas de los productos agroalimentarios",            "Estructura", "UPRA_EVA_A",
  "Inventarios pecuarios territoriales",                                  "Estructura", "ICA_CENSO_PECUARIO"
)

# =========================================================
# ORDEN DEL SCROLL (Estructura) — solicitado (ICA unificada)
# =========================================================
choices_por_dimension <- function(dim){
  
  orden_estructura <- c(
    "UPRA_FA",
    "UPRA_APADT",
    "HANSEN_COBERTURA_BOSQUE",
    "UPRA_EVA_A",
    "DANE_POPULATION",
    "DNP-IDM",
    "ICA_CENSO_PECUARIO"
  )
  
  df <- indicadores_df %>% dplyr::filter(dimension == dim)
  
  if (!nrow(df)) {
    return(setNames("NINGUNO", "Sin sistemas asociados para esta dimensión"))
  }
  
  if (dim == "Estructura") {
    df <- df %>%
      dplyr::mutate(.ord = match(sistema, orden_estructura)) %>%
      dplyr::arrange(.ord, titulo) %>%
      dplyr::select(-.ord)
  }
  
  setNames(df$sistema, df$titulo)
}

base_to_indicator_code <- function(sistema){
  switch(
    sistema,
    "DANE_POPULATION"         = "POBLACION",
    "DNP-IDM"                 = "IDM",
    "HANSEN_COBERTURA_BOSQUE" = "HANSEN",
    "UPRA_APADT"              = "UPRA_APADT",
    "UPRA_FA"                 = "UPRA_FA",
    "UPRA_EVA_A"              = "EVA",
    "ICA_CENSO_PECUARIO"      = "ICA",
    NA_character_
  )
}

# =========================================================
# Opciones de filtros
# =========================================================
pob_var_choices <- c(
  "Población total"              = "poblacion_total",
  "Población joven (0–14 años)"  = "poblacion_joven",
  "Población adulta mayor (65+)" = "poblacion_mayor",
  "Razón de dependencia"         = "razon_dependencia"
)

hansen_metric_choices <- c(
  "Cobertura neta (hectáreas)" = "ha",
  "Cobertura neta (%)"         = "pct"
)

eva_metric_choices <- c(
  "Área sembrada (hectáreas)"  = "area_sembrada_ha",
  "Área cosechada (hectáreas)" = "area_cosechada_ha",
  "Producción (t)"             = "produccion_t"
)

lbl <- function(x) div(class = "filter-label", x)

selector_base <- function(input_id, dimension){
  selectInput(
    inputId   = input_id,
    label     = NULL,
    choices   = choices_por_dimension(dimension),
    selected  = unname(choices_por_dimension(dimension)[1])
  )
}

# =========================================================
# Normalizador códigos departamento (2 dígitos)
# =========================================================
normalize_cod_dep <- function(x){
  x <- as.character(x)
  x <- stringi::stri_trim_both(x)
  x <- gsub("[^0-9]", "", x)
  x[x == ""] <- NA_character_
  stringi::stri_pad_left(x, 2, pad = "0")
}

# =========================================================
# ✅ ICA (CORREGIDO): siempre existe ica_long (aunque no haya archivos)
# =========================================================
ica_long <- tibble::tibble(
  ano = integer(),
  cod_dep = character(),
  animal = character(),
  valor = numeric()
)

read_ica_file <- function(file){
  p <- file.path(data_dir, file)
  if (!file.exists(p)) {
    message("ICA: no existe -> ", p)
    return(NULL)
  }
  
  df <- tryCatch(readRDS(p), error = function(e){
    message("ICA: error leyendo ", file, " -> ", e$message)
    NULL
  })
  if (is.null(df) || !nrow(df)) return(NULL)
  
  nms <- names(df)
  col_ano <- pick_year_col(nms)
  col_dep <- pick_dep_col(nms)
  
  if (is.na(col_ano) || is.na(col_dep)) {
    message("ICA: faltan columnas año/depto en ", file)
    return(NULL)
  }
  
  df$ano     <- suppressWarnings(as.integer(df[[col_ano]]))
  df$cod_dep <- normalize_cod_dep(df[[col_dep]])
  
  df
}

make_long_from_cols <- function(df, mapping_named){
  out <- lapply(seq_along(mapping_named), function(i){
    lab <- names(mapping_named)[i]
    col <- unname(mapping_named[i])
    
    if (!col %in% names(df)) return(NULL)
    
    tibble::tibble(
      ano = df$ano,
      cod_dep = df$cod_dep,
      animal = lab,
      valor = parse_num_co(df[[col]])
    )
  })
  
  dplyr::bind_rows(out) %>%
    dplyr::filter(!is.na(ano), !is.na(cod_dep))
}

df_bov  <- read_ica_file("101_ICA_CensoPecuario-Bovino.rds")
df_por  <- read_ica_file("102_ICA_CensoPecuario-Porcino.rds")
df_bcoe <- read_ica_file("103_ICA_CensoPecuario-BCOE.rds")
df_avi  <- read_ica_file("104_ICA_CensoPecuario-Aviar.rds")

parts <- list()

if (!is.null(df_bov)) {
  parts <- c(parts, list(make_long_from_cols(df_bov, c("Bovinos" = "total_bovinos"))))
}

if (!is.null(df_por)) {
  parts <- c(parts, list(make_long_from_cols(df_por, c("Porcinos" = "total_porcinos"))))
}

if (!is.null(df_bcoe)) {
  parts <- c(parts, list(make_long_from_cols(df_bcoe, c(
    "Búfalos"  = "total_bufalos",
    "Equinos"  = "total_equinos",
    "Caprinos" = "total_caprinos",
    "Ovinos"   = "total_ovinos"
  ))))
}

if (!is.null(df_avi)) {
  parts <- c(parts, list(make_long_from_cols(df_avi, c(
    "Aves (capacidad instalada)" = "total_aves_capacidad_instalada",
    "Aves (capacidad ocupada)"   = "total_aves_capacidad_ocupada",
    "Aves (traspatio)"           = "total_aves_traspatio"
  ))))
}

tmp <- dplyr::bind_rows(parts)
if (!is.null(tmp) && nrow(tmp)) {
  ica_long <- tmp %>%
    dplyr::group_by(ano, cod_dep, animal) %>%
    dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(animal, ano, cod_dep)
}

ica_animal_choices <- {
  if (is.null(ica_long) || !nrow(ica_long)) c("Sin datos ICA" = "__ALL__")
  else {
    vals <- unique(ica_long$animal)
    ord <- c("Bovinos","Porcinos",
             "Aves (capacidad instalada)","Aves (capacidad ocupada)","Aves (traspatio)",
             "Búfalos","Caprinos","Ovinos","Equinos")
    vals <- vals[order(match(vals, ord), vals, na.last = TRUE)]
    stats::setNames(vals, vals)
  }
}

make_ica_ts_plot_unified <- function(animal_sel = "Bovinos"){
  if (is.null(ica_long) || !nrow(ica_long)) return(empty_ts_plot("Sin datos del ICA disponibles."))
  animal_sel <- animal_sel %||% "Bovinos"
  
  df <- ica_long %>% dplyr::filter(animal == animal_sel)
  if (!nrow(df)) return(empty_ts_plot("Sin datos para el animal seleccionado."))
  
  ts_nacional <- df %>%
    dplyr::group_by(ano) %>%
    dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(ano)
  
  if (!nrow(ts_nacional) || all(is.na(ts_nacional$valor))) {
    return(empty_ts_plot("Sin datos válidos para el animal seleccionado."))
  }
  
  ticks_x <- year_ticks_2(ts_nacional$ano)
  
  plotly::plot_ly(
    data = ts_nacional, x = ~ano, y = ~valor,
    type = "scatter", mode = "lines+markers",
    line = list(width = 2),
    marker = list(size = 6),
    customdata = fmt_short(ts_nacional$valor),
    hovertemplate = paste0("<b>Año:</b> %{x}<br><b>", animal_sel, ":</b> %{customdata}<extra></extra>")
  ) %>%
    plotly::layout(
      title = NULL,
      xaxis = list(
        title = "",
        tickmode = "array", tickvals = ticks_x, ticktext = ticks_x,
        tickangle = 0, showgrid = FALSE, automargin = TRUE
      ),
      yaxis = list(
        title = paste0("Inventario (", animal_sel, ")"),
        showgrid = FALSE,
        automargin = TRUE
      ),
      margin = list(l = 70, r = 20, t = 10, b = 60),
      hovermode = "x unified",
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# =========================================================
# ✅ SERIES (NACIONAL) — DANE POBLACIÓN (desde quinquenio)
# =========================================================
pob_ts <- tryCatch({
  p_path <- file.path(data_dir, "051_DANE_Proyecciones_P.rds")
  if (!file.exists(p_path)) return(NULL)
  
  df <- readRDS(p_path)
  if (is.null(df) || !nrow(df)) return(NULL)
  
  nms <- names(df)
  col_ano  <- pick_year_col(nms)
  col_dep  <- pick_dep_col(nms)
  col_q    <- pick_quinquenio_col(nms)
  col_pob  <- pick_col_simple(nms, c("poblacion","pob","population"))
  
  if (any(is.na(c(col_ano, col_dep, col_q, col_pob)))) return(NULL)
  
  tmp <- tibble::tibble(
    ano        = suppressWarnings(as.integer(df[[col_ano]])),
    cod_dep    = normalize_cod_dep(df[[col_dep]]),
    quinquenio = stringi::stri_trim_both(as.character(df[[col_q]])),
    poblacion  = parse_num_co(df[[col_pob]])
  ) %>%
    dplyr::filter(!is.na(ano), !is.na(cod_dep), !is.na(quinquenio), quinquenio != "", !is.na(poblacion)) %>%
    dplyr::mutate(grupo = vapply(quinquenio, quinquenio_to_group, FUN.VALUE = character(1))) %>%
    dplyr::filter(!is.na(grupo))
  
  if (!nrow(tmp)) return(NULL)
  
  tmp %>%
    dplyr::group_by(ano) %>%
    dplyr::summarise(
      poblacion_joven  = sum(poblacion[grupo == "joven"],  na.rm = TRUE),
      poblacion_activa = sum(poblacion[grupo == "activa"], na.rm = TRUE),
      poblacion_mayor  = sum(poblacion[grupo == "mayor"],  na.rm = TRUE),
      poblacion_total  = poblacion_joven + poblacion_activa + poblacion_mayor,
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
  if (is.null(pob_ts) || !nrow(pob_ts)) return(empty_ts_plot("Sin datos demográficos disponibles."))
  
  df <- pob_ts
  var_code <- var_code %||% "poblacion_total"
  if (!(var_code %in% names(df))) var_code <- "poblacion_total"
  
  if (var_code == "razon_dependencia") {
    df$valor <- df$razon_dependencia * 100
    ylab <- "Razón de dependencia (%)"
    custom_vals <- fmt_pct(df$valor, 1)
    ticks <- pretty(range(df$valor, na.rm = TRUE), n = 6)
    ticktext <- fmt_pct(ticks, 1)
  } else {
    df$valor <- df[[var_code]]
    ylab <- "Población"
    custom_vals <- fmt_short(df$valor)
    ticks <- pretty(range(df$valor, na.rm = TRUE), n = 6); ticks <- ticks[ticks >= 0]
    ticktext <- fmt_short(ticks)
  }
  
  ticks_x <- year_ticks_5(df$ano)
  
  plotly::plot_ly(
    data = df, x = ~ano, y = ~valor,
    type = "scatter", mode = "lines+markers",
    line = list(width = 2),
    marker = list(size = 6),
    customdata = custom_vals,
    hovertemplate = "<b>Año:</b> %{x}<br><b>Valor:</b> %{customdata}<extra></extra>"
  ) %>%
    plotly::layout(
      title = NULL,
      xaxis = list(
        title = "",
        tickmode = "array", tickvals = ticks_x, ticktext = ticks_x,
        tickangle = 0, showgrid = FALSE, automargin = TRUE
      ),
      yaxis = list(title = ylab, tickvals = ticks, ticktext = ticktext, showgrid = FALSE, automargin = TRUE),
      margin = list(l = 70, r = 20, t = 10, b = 60),
      hovermode = "x unified",
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# =========================================================
# SERIES (NACIONAL) — HANSEN COBERTURA BOSQUE
# =========================================================
hansen_ts <- tryCatch({
  h_path <- file.path(data_dir, "141_HANSEN_COBERTURA_NETA_TOTAL.rds")
  if (!file.exists(h_path)) return(NULL)
  df <- readRDS(h_path)
  if (is.null(df) || !nrow(df)) return(NULL)
  
  nms <- names(df)
  col_ano  <- pick_year_col(nms)
  col_cob  <- if ("cobertura_ha" %in% nms) "cobertura_ha" else pick_col_simple(nms, c("cobertura_neta_ha","cobertura"))
  col_base <- pick_col_simple(nms, c("base_ha_2000","base2000","ha_2000"))
  if (any(is.na(c(col_ano, col_cob, col_base)))) return(NULL)
  
  suppressWarnings({
    df[[col_ano]]  <- as.integer(df[[col_ano]])
    df[[col_cob]]  <- parse_num_co(df[[col_cob]])
    df[[col_base]] <- parse_num_co(df[[col_base]])
  })
  
  df %>%
    dplyr::filter(!is.na(.data[[col_ano]])) %>%
    dplyr::group_by(anio = .data[[col_ano]]) %>%
    dplyr::summarise(
      cobertura_ha = sum(.data[[col_cob]], na.rm = TRUE),
      base_ha_2000 = sum(.data[[col_base]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      cobertura_pct = dplyr::if_else(base_ha_2000 > 0, 100 * cobertura_ha / base_ha_2000, NA_real_)
    ) %>%
    dplyr::arrange(anio)
}, error = function(e) NULL)

make_hansen_ts_plot <- function(metric_code){
  if (is.null(hansen_ts) || !nrow(hansen_ts)) return(empty_ts_plot("Sin datos de cobertura de bosque disponibles."))
  df <- hansen_ts
  metric_code <- metric_code %||% "pct"
  
  if (metric_code == "ha") {
    df$valor <- df$cobertura_ha
    ylab <- "Cobertura neta de bosque (hectáreas)"
    custom_vals <- fmt_short(df$valor)
    ticks <- pretty(range(df$valor, na.rm = TRUE), n = 6); ticks <- ticks[ticks >= 0]
    ticktext <- fmt_short(ticks)
  } else {
    df$valor <- df$cobertura_pct
    ylab <- "Cobertura neta de bosque (%)"
    custom_vals <- fmt_pct(df$valor, 1)
    ticks <- seq(0, 100, by = 5)
    ticktext <- fmt_pct(ticks, 1)
  }
  
  ticks_x <- year_ticks_2(df$anio)
  
  plotly::plot_ly(
    data = df, x = ~anio, y = ~valor,
    type = "scatter", mode = "lines+markers",
    line = list(width = 2),
    marker = list(size = 6),
    customdata = custom_vals,
    hovertemplate = "<b>Año:</b> %{x}<br><b>Valor:</b> %{customdata}<extra></extra>"
  ) %>%
    plotly::layout(
      title = NULL,
      xaxis = list(
        title = "",
        tickmode = "array", tickvals = ticks_x, ticktext = ticks_x,
        tickangle = 0, showgrid = FALSE, automargin = TRUE
      ),
      yaxis = list(title = ylab, tickvals = ticks, ticktext = ticktext, showgrid = FALSE, automargin = TRUE),
      margin = list(l = 70, r = 20, t = 10, b = 60),
      hovermode = "x unified",
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# =========================================================
# ✅ IDM — AJUSTADO A TU BASE MUNICIPAL (COD_DANE_DPTO_D + valor)
# =========================================================
idm_raw <- tibble::tibble(
  ano         = integer(),
  valor_bruto = numeric(),
  segmento    = character(),
  cod_dep     = character()
)

idm_raw <- tryCatch({
  p <- file.path(data_dir, "071_DNP_Terridata_IDM.rds")
  if (!file.exists(p)) {
    message("IDM: no existe -> ", p)
    return(idm_raw)
  }
  
  df <- readRDS(p)
  if (is.null(df) || !nrow(df)) return(idm_raw)
  
  nms <- names(df)
  
  col_ano <- pick_year_col(nms)                 # "ano"
  col_val <- pick_col_simple(nms, c("valor"))   # "valor"
  col_dep <- pick_dep_col(nms)                  # "COD_DANE_DPTO_D"
  
  if (any(is.na(c(col_ano, col_val, col_dep)))) {
    message("IDM: faltan columnas clave (ano/valor/depto).")
    return(idm_raw)
  }
  
  tibble::tibble(
    ano         = suppressWarnings(as.integer(df[[col_ano]])),
    valor_bruto = parse_num_co(df[[col_val]]),
    segmento    = NA_character_,
    cod_dep     = normalize_cod_dep(df[[col_dep]])
  ) %>%
    dplyr::filter(!is.na(ano), !is.na(valor_bruto), !is.na(cod_dep))
}, error = function(e){
  message("IDM: error leyendo/armando -> ", e$message)
  idm_raw
})

# En tu RDS no hay segmento/cuadrante -> dejamos solo promedio nacional
idm_seg_choices <- c("Promedio nacional" = "__ALL__")

make_idm_ts_plot <- function(segmento = "__ALL__"){
  if (is.null(idm_raw) || !nrow(idm_raw)) return(empty_ts_plot("Sin datos del IDM disponibles."))
  
  ts <- idm_raw %>%
    dplyr::group_by(ano) %>%
    dplyr::summarise(valor = mean(valor_bruto, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(ano)
  
  if (!nrow(ts) || all(is.na(ts$valor))) return(empty_ts_plot("Sin datos válidos para IDM."))
  
  ticks <- pretty(range(ts$valor, na.rm = TRUE), n = 6)
  ticks <- ticks[is.finite(ticks)]
  ticks_x <- year_ticks_2(ts$ano)
  
  plotly::plot_ly(
    data = ts, x = ~ano, y = ~valor,
    type = "scatter", mode = "lines+markers",
    line = list(width = 2),
    marker = list(size = 6),
    customdata = fmt_num_es(ts$valor, 2),
    hovertemplate = "<b>Año:</b> %{x}<br><b>IDM promedio:</b> %{customdata} puntos<extra></extra>"
  ) %>%
    plotly::layout(
      title = NULL,
      xaxis = list(
        title = "",
        tickmode = "array", tickvals = ticks_x, ticktext = ticks_x,
        tickangle = 0, showgrid = FALSE, automargin = TRUE
      ),
      yaxis = list(
        title = "Puntos (0–100)",
        tickvals = ticks,
        ticktext = fmt_num_es(ticks, 1),
        showgrid = FALSE,
        automargin = TRUE
      ),
      margin = list(l = 70, r = 20, t = 10, b = 60),
      hovermode = "x unified",
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# =========================================================
# UPRA_FA (nacional) — SOLO BARRAS
# =========================================================
upra_fa_ts <- tryCatch({
  p <- file.path(data_dir, "013_UPRA_FA_Proporcion FA_Total municipal.rds")
  if (!file.exists(p)) return(NULL)
  df <- readRDS(p)
  if (is.null(df) || !nrow(df)) return(NULL)
  nms <- names(df)
  
  col_ano      <- pick_year_col(nms)
  col_area_fa  <- pick_col_simple(nms, c("area_fa_ha","area_fa","fa_ha"))
  col_area_tot <- pick_col_simple(nms, c("area_mpio_ha","area_total_ha","area_municipio","area_mun_ha"))
  if (any(is.na(c(col_ano, col_area_fa, col_area_tot)))) return(NULL)
  
  suppressWarnings({
    df[[col_ano]]      <- as.integer(df[[col_ano]])
    df[[col_area_fa]]  <- parse_num_co(df[[col_area_fa]])
    df[[col_area_tot]] <- parse_num_co(df[[col_area_tot]])
  })
  
  df %>%
    dplyr::filter(!is.na(.data[[col_ano]])) %>%
    dplyr::group_by(ano = .data[[col_ano]]) %>%
    dplyr::summarise(
      area_fa_ha  = sum(.data[[col_area_fa]],  na.rm = TRUE),
      area_tot_ha = sum(.data[[col_area_tot]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      participacion = dplyr::if_else(area_tot_ha > 0, 100 * area_fa_ha / area_tot_ha, NA_real_)
    ) %>%
    dplyr::arrange(ano)
}, error = function(e) NULL)

make_upra_fa_bar_plot <- function(){
  if (is.null(upra_fa_ts) || !nrow(upra_fa_ts)) return(empty_ts_plot("Sin datos para barras."))
  uba <- upra_fa_ts
  last_year <- max(uba$ano, na.rm = TRUE)
  row <- uba %>% dplyr::filter(ano == last_year) %>% dplyr::slice(1)
  if (!nrow(row)) return(empty_ts_plot("Sin datos para barras."))
  
  dfb <- tibble::tibble(
    categoria = c("Área total (hectáreas)", "Área en frontera (hectáreas)"),
    valor     = c(row$area_tot_ha, row$area_fa_ha),
    lab       = fmt_short(c(row$area_tot_ha, row$area_fa_ha))
  )
  
  plotly::plot_ly(
    dfb, x = ~categoria, y = ~valor,
    type = "bar",
    text = ~lab, textposition = "auto",
    hovertemplate = "<b>%{x}</b><br><b>Valor:</b> %{text}<extra></extra>"
  ) %>%
    plotly::layout(
      title = NULL,
      xaxis = list(title = "", showgrid = FALSE, automargin = TRUE),
      yaxis = list(
        title = "hectáreas", showgrid = FALSE, automargin = TRUE,
        ticktext = fmt_short(pretty(dfb$valor)), tickvals = pretty(dfb$valor)
      ),
      margin = list(l = 70, r = 20, t = 10, b = 60),
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# =========================================================
# UPRA_APADT (nacional) — SOLO BARRAS
# =========================================================
upra_apadt_ts <- tryCatch({
  p <- file.path(data_dir, "014_UPRA_APADT.rds")
  if (!file.exists(p)) return(NULL)
  df <- readRDS(p)
  if (is.null(df) || !nrow(df)) return(NULL)
  nms <- names(df)
  
  col_ano      <- pick_year_col(nms)
  col_area_pot <- pick_col_simple(nms, c("area_apadt_ha","area_pot_riego_ha","area_pot_dren_ha",
                                         "area_potencial_ha","area_potencial_riego_dren_ha"))
  col_area_tot <- pick_col_simple(nms, c("area_mpio_ha","area_total_ha","area_municipio","area_mun_ha"))
  if (any(is.na(c(col_ano, col_area_pot, col_area_tot)))) return(NULL)
  
  suppressWarnings({
    df[[col_ano]]      <- as.integer(df[[col_ano]])
    df[[col_area_pot]] <- parse_num_co(df[[col_area_pot]])
    df[[col_area_tot]] <- parse_num_co(df[[col_area_tot]])
  })
  
  df %>%
    dplyr::filter(!is.na(.data[[col_ano]])) %>%
    dplyr::group_by(ano = .data[[col_ano]]) %>%
    dplyr::summarise(
      area_pot_ha = sum(.data[[col_area_pot]], na.rm = TRUE),
      area_tot_ha = sum(.data[[col_area_tot]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      participacion = dplyr::if_else(area_tot_ha > 0, 100 * area_pot_ha / area_tot_ha, NA_real_)
    ) %>%
    dplyr::arrange(ano)
}, error = function(e) NULL)

make_upra_apadt_bar_plot <- function(){
  if (is.null(upra_apadt_ts) || !nrow(upra_apadt_ts)) return(empty_ts_plot("Sin datos para barras."))
  uda <- upra_apadt_ts
  last_year <- max(uda$ano, na.rm = TRUE)
  row <- uda %>% dplyr::filter(ano == last_year) %>% dplyr::slice(1)
  if (!nrow(row)) return(empty_ts_plot("Sin datos para barras."))
  
  dfb <- tibble::tibble(
    categoria = c("Área total (hectáreas)", "Área potencial (hectáreas)"),
    valor     = c(row$area_tot_ha, row$area_pot_ha),
    lab       = fmt_short(c(row$area_tot_ha, row$area_pot_ha))
  )
  
  plotly::plot_ly(
    dfb, x = ~categoria, y = ~valor,
    type = "bar",
    text = ~lab, textposition = "auto",
    hovertemplate = "<b>%{x}</b><br><b>Valor:</b> %{text}<extra></extra>"
  ) %>%
    plotly::layout(
      title = NULL,
      xaxis = list(title = "", showgrid = FALSE, automargin = TRUE),
      yaxis = list(
        title = "hectáreas", showgrid = FALSE, automargin = TRUE,
        ticktext = fmt_short(pretty(dfb$valor)), tickvals = pretty(dfb$valor)
      ),
      margin = list(l = 70, r = 20, t = 10, b = 60),
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# =========================================================
# EVA-A — filtro por `cultivo` + serie dual
# =========================================================
eva_raw <- tryCatch({
  p <- file.path(data_dir, "011_UPRA_EVA-A.rds")
  if (!file.exists(p)) return(NULL)
  df <- readRDS(p)
  if (is.null(df) || !nrow(df)) return(NULL)
  
  nms <- names(df)
  nms_l <- tolower(nms)
  
  col_ano <- pick_year_col(nms)
  col_dep <- pick_dep_col(nms)
  
  if ("cultivo" %in% nms_l) col_cult <- nms[which(nms_l == "cultivo")[1]] else stop("EVA-A: no existe columna 'cultivo'.")
  
  col_a_s  <- pick_col_simple(nms, c("area_sembrada_ha","área_sembrada_ha","ha_sembrada","area_sembrada"))
  col_a_c  <- pick_col_simple(nms, c("area_cosechada_ha","área_cosechada_ha","ha_cosechada","area_cosechada"))
  col_prod <- pick_col_simple(nms, c("produccion_t","producción_t","produccion","producción","toneladas","t","produccion_ton"))
  
  if (any(is.na(c(col_ano, col_dep, col_a_s, col_a_c, col_prod)))) stop("EVA-A: faltan columnas (año/depto/áreas/producción).")
  
  cultivos_excluir <- c(
    "Cacao", "Café", "Caña de Azúcar", "Fique", "Iraca", "Olivo",
    "Otras oleaginosas",
    "Otros cultivos tropicales tradicionales",
    "Palma de aceite",
    "Sacha inchi"
  )
  
  tibble(
    ano               = as.integer(df[[col_ano]]),
    cod_dep           = normalize_cod_dep(df[[col_dep]]),
    cultivo           = stringi::stri_trim_both(as.character(df[[col_cult]])),
    area_sembrada_ha  = parse_num_co(df[[col_a_s]]),
    area_cosechada_ha = parse_num_co(df[[col_a_c]]),
    produccion_t      = parse_num_co(df[[col_prod]])
  ) %>%
    dplyr::filter(!is.na(ano), !is.na(cod_dep), !is.na(cultivo), cultivo != "") %>%
    dplyr::filter(!cultivo %in% cultivos_excluir)
}, error = function(e) {
  message("EVA-A error: ", e$message)
  NULL
})

eva_cultivo_choices <- {
  if (is.null(eva_raw) || !nrow(eva_raw)) c("Sin datos EVA-A" = "__ALL__")
  else {
    vals <- sort(unique(eva_raw$cultivo))
    c("Todos los cultivos" = "__ALL__", stats::setNames(vals, vals))
  }
}

make_eva_ts_plot_dual <- function(cultivo_sel = "__ALL__"){
  if (is.null(eva_raw) || !nrow(eva_raw)) return(empty_ts_plot("Sin datos de EVA-A disponibles."))
  
  df <- eva_raw
  cultivo_sel <- cultivo_sel %||% "__ALL__"
  
  if (!is.null(cultivo_sel) && cultivo_sel != "__ALL__") df <- df %>% dplyr::filter(.data$cultivo == cultivo_sel)
  if (!nrow(df)) return(empty_ts_plot("No hay datos para el cultivo seleccionado."))
  
  ts <- df %>%
    dplyr::group_by(ano) %>%
    dplyr::summarise(
      area_sembrada_ha  = sum(area_sembrada_ha,  na.rm = TRUE),
      area_cosechada_ha = sum(area_cosechada_ha, na.rm = TRUE),
      produccion_t      = sum(produccion_t,      na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(ano)
  
  y1_rng <- range(c(ts$area_sembrada_ha, ts$area_cosechada_ha), na.rm = TRUE)
  if (!all(is.finite(y1_rng))) y1_rng <- c(0, 1)
  y1_ticks <- pretty(y1_rng, n = 6); y1_ticks <- y1_ticks[y1_ticks >= 0]
  
  y2_rng <- range(ts$produccion_t, na.rm = TRUE)
  if (!all(is.finite(y2_rng))) y2_rng <- c(0, 1)
  y2_ticks <- pretty(y2_rng, n = 6); y2_ticks <- y2_ticks[y2_ticks >= 0]
  
  ticks_x <- year_ticks_2(ts$ano)
  
  plotly::plot_ly(data = ts, x = ~ano) %>%
    plotly::add_lines(
      y = ~area_sembrada_ha, name = "Área sembrada (hectáreas)",
      customdata = fmt_short(ts$area_sembrada_ha),
      hovertemplate = "<b>Año:</b> %{x}<br><b>Área sembrada:</b> %{customdata} ha<extra></extra>"
    ) %>%
    plotly::add_lines(
      y = ~area_cosechada_ha, name = "Área cosechada (hectáreas)",
      customdata = fmt_short(ts$area_cosechada_ha),
      hovertemplate = "<b>Año:</b> %{x}<br><b>Área cosechada:</b> %{customdata} ha<extra></extra>"
    ) %>%
    plotly::add_lines(
      y = ~produccion_t, name = "Producción (toneladas)",
      yaxis = "y2",
      customdata = fmt_short(ts$produccion_t),
      hovertemplate = "<b>Año:</b> %{x}<br><b>Producción:</b> %{customdata} t<extra></extra>"
    ) %>%
    plotly::layout(
      title = NULL,
      xaxis = list(
        title = "",
        tickmode = "array", tickvals = ticks_x, ticktext = ticks_x,
        tickangle = 0, showgrid = FALSE, automargin = TRUE
      ),
      yaxis = list(
        title = "Hectáreas",
        tickvals = y1_ticks, ticktext = fmt_short(y1_ticks),
        showgrid = FALSE, automargin = TRUE
      ),
      yaxis2 = list(
        title      = "Producción (toneladas)",
        tickvals   = y2_ticks,
        ticktext   = fmt_short(y2_ticks),
        overlaying = "y",
        side       = "right",
        showgrid   = FALSE
      ),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25, yanchor = "top"),
      margin = list(l = 70, r = 70, t = 10, b = 110),
      hovermode = "x unified",
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# =========================================================
# Tarjetas (UI)
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
# SHAPE departamental
# =========================================================
ruta_shp_dep <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")
shp_dep <- tryCatch({
  if (!file.exists(ruta_shp_dep)) return(NULL)
  sf::st_read(ruta_shp_dep, quiet = TRUE)
}, error = function(e) NULL)

dep_code_col_shp <- if (!is.null(shp_dep)) pick_dep_col(names(shp_dep)) else NA_character_
dep_name_col_shp <- if (!is.null(shp_dep)) pick_dep_name_col(names(shp_dep)) else NA_character_

if (!is.null(shp_dep)) {
  shp_dep <- sf::st_transform(shp_dep, 4326)
  shp_dep <- sf::st_simplify(shp_dep, dTolerance = 0.01, preserveTopology = TRUE)
}

# =========================================================
# Datos departamentales para mapas (agregados por año)
# =========================================================

# ✅ IDM a nivel departamental (promedio de municipios)
idm_dep_all <- tryCatch({
  if (is.null(idm_raw) || !nrow(idm_raw)) return(NULL)
  idm_raw %>%
    dplyr::group_by(ano, cod_dep) %>%
    dplyr::summarise(valor = mean(valor_bruto, na.rm = TRUE), .groups = "drop")
}, error = function(e) {
  message("IDM mapa: error -> ", e$message)
  NULL
})

hansen_dep_all <- tryCatch({
  h_path <- file.path(data_dir, "141_HANSEN_COBERTURA_NETA_TOTAL.rds")
  if (!file.exists(h_path)) return(NULL)
  df <- readRDS(h_path)
  nms <- names(df)
  
  col_ano  <- pick_year_col(nms)
  col_dep  <- pick_dep_col(nms)
  col_cob  <- if ("cobertura_ha" %in% nms) "cobertura_ha" else pick_col_simple(nms, c("cobertura_neta_ha","cobertura"))
  col_base <- pick_col_simple(nms, c("base_ha_2000","base2000","ha_2000"))
  if (any(is.na(c(col_ano, col_dep, col_cob, col_base)))) return(NULL)
  
  suppressWarnings({
    df[[col_ano]]  <- as.integer(df[[col_ano]])
    df[[col_cob]]  <- parse_num_co(df[[col_cob]])
    df[[col_base]] <- parse_num_co(df[[col_base]])
  })
  
  df %>%
    dplyr::filter(!is.na(.data[[col_ano]]), !is.na(.data[[col_dep]])) %>%
    dplyr::group_by(ano = .data[[col_ano]], cod_dep = normalize_cod_dep(.data[[col_dep]])) %>%
    dplyr::summarise(
      cobertura_ha = sum(.data[[col_cob]],  na.rm = TRUE),
      base_ha_2000 = sum(.data[[col_base]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      cobertura_pct = dplyr::if_else(base_ha_2000 > 0, 100 * cobertura_ha / base_ha_2000, NA_real_)
    )
}, error = function(e) NULL)

upra_fa_dep_all <- tryCatch({
  p <- file.path(data_dir, "013_UPRA_FA_Proporcion FA_Total municipal.rds")
  if (!file.exists(p)) return(NULL)
  df <- readRDS(p)
  nms <- names(df)
  
  col_ano      <- pick_year_col(nms)
  col_area_fa  <- pick_col_simple(nms, c("area_fa_ha","area_fa","fa_ha"))
  col_area_tot <- pick_col_simple(nms, c("area_mpio_ha","area_total_ha","area_municipio","area_mun_ha"))
  col_dep      <- pick_dep_col(nms)
  if (any(is.na(c(col_ano, col_area_fa, col_area_tot, col_dep)))) return(NULL)
  
  suppressWarnings({
    df[[col_ano]]      <- as.integer(df[[col_ano]])
    df[[col_area_fa]]  <- parse_num_co(df[[col_area_fa]])
    df[[col_area_tot]] <- parse_num_co(df[[col_area_tot]])
  })
  
  df %>%
    dplyr::filter(!is.na(.data[[col_ano]]), !is.na(.data[[col_dep]])) %>%
    dplyr::group_by(ano = .data[[col_ano]], cod_dep = normalize_cod_dep(.data[[col_dep]])) %>%
    dplyr::summarise(
      area_fa_ha  = sum(.data[[col_area_fa]], na.rm = TRUE),
      area_tot_ha = sum(.data[[col_area_tot]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(valor = dplyr::if_else(area_tot_ha > 0, 100 * area_fa_ha / area_tot_ha, NA_real_))
}, error = function(e) NULL)

apadt_dep_all <- tryCatch({
  p <- file.path(data_dir, "014_UPRA_APADT.rds")
  if (!file.exists(p)) return(NULL)
  df <- readRDS(p)
  nms <- names(df)
  
  col_ano      <- pick_year_col(nms)
  col_area_pot <- pick_col_simple(nms, c("area_apadt_ha","area_pot_riego_ha","area_pot_dren_ha",
                                         "area_potencial_ha","area_potencial_riego_dren_ha"))
  col_area_tot <- pick_col_simple(nms, c("area_mpio_ha","area_total_ha","area_municipio","area_mun_ha"))
  col_dep      <- pick_dep_col(nms)
  if (any(is.na(c(col_ano, col_area_pot, col_area_tot, col_dep)))) return(NULL)
  
  suppressWarnings({
    df[[col_ano]]      <- as.integer(df[[col_ano]])
    df[[col_area_pot]] <- parse_num_co(df[[col_area_pot]])
    df[[col_area_tot]] <- parse_num_co(df[[col_area_tot]])
  })
  
  df %>%
    dplyr::filter(!is.na(.data[[col_ano]]), !is.na(.data[[col_dep]])) %>%
    dplyr::group_by(ano = .data[[col_ano]], cod_dep = normalize_cod_dep(.data[[col_dep]])) %>%
    dplyr::summarise(
      area_pot_ha = sum(.data[[col_area_pot]], na.rm = TRUE),
      area_tot_ha = sum(.data[[col_area_tot]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(valor = dplyr::if_else(area_tot_ha > 0, 100 * area_pot_ha / area_tot_ha, NA_real_))
}, error = function(e) NULL)

# =========================================================
# ✅ POBLACIÓN departamental (desde quinquenio)
# =========================================================
pob_dep_all <- tryCatch({
  p <- file.path(data_dir, "051_DANE_Proyecciones_P.rds")
  if (!file.exists(p)) return(NULL)
  
  df <- readRDS(p)
  if (is.null(df) || !nrow(df)) return(NULL)
  
  nms <- names(df)
  col_ano  <- pick_year_col(nms)
  col_dep  <- pick_dep_col(nms)
  col_q    <- pick_quinquenio_col(nms)
  col_pob  <- pick_col_simple(nms, c("poblacion","pob","population"))
  if (any(is.na(c(col_ano, col_dep, col_q, col_pob)))) return(NULL)
  
  tmp <- tibble::tibble(
    ano        = suppressWarnings(as.integer(df[[col_ano]])),
    cod_dep    = normalize_cod_dep(df[[col_dep]]),
    quinquenio = stringi::stri_trim_both(as.character(df[[col_q]])),
    poblacion  = parse_num_co(df[[col_pob]])
  ) %>%
    dplyr::filter(!is.na(ano), !is.na(cod_dep), !is.na(quinquenio), quinquenio != "", !is.na(poblacion)) %>%
    dplyr::mutate(grupo = vapply(quinquenio, quinquenio_to_group, FUN.VALUE = character(1))) %>%
    dplyr::filter(!is.na(grupo))
  
  if (!nrow(tmp)) return(NULL)
  
  tmp %>%
    dplyr::group_by(ano, cod_dep) %>%
    dplyr::summarise(
      poblacion_joven  = sum(poblacion[grupo == "joven"],  na.rm = TRUE),
      poblacion_activa = sum(poblacion[grupo == "activa"], na.rm = TRUE),
      poblacion_mayor  = sum(poblacion[grupo == "mayor"],  na.rm = TRUE),
      poblacion_total  = poblacion_joven + poblacion_activa + poblacion_mayor,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      razon_dependencia = dplyr::if_else(
        poblacion_activa > 0,
        (poblacion_joven + poblacion_mayor) / poblacion_activa,
        NA_real_
      )
    )
}, error = function(e) NULL)

eva_dep_all <- tryCatch({
  if (is.null(eva_raw) || !nrow(eva_raw)) return(NULL)
  eva_raw %>%
    dplyr::group_by(ano, cod_dep, cultivo) %>%
    dplyr::summarise(
      area_sembrada_ha  = sum(area_sembrada_ha,  na.rm = TRUE),
      area_cosechada_ha = sum(area_cosechada_ha, na.rm = TRUE),
      produccion_t      = sum(produccion_t,      na.rm = TRUE),
      .groups = "drop"
    )
}, error = function(e) NULL)

ica_dep_all <- tryCatch({
  if (is.null(ica_long) || !nrow(ica_long)) return(NULL)
  ica_long %>%
    dplyr::group_by(ano, cod_dep, animal) %>%
    dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
}, error = function(e) NULL)

all_years_dep <- sort(unique(c(
  if (!is.null(pob_dep_all))    pob_dep_all$ano,
  if (!is.null(idm_dep_all))    idm_dep_all$ano,
  if (!is.null(hansen_dep_all)) hansen_dep_all$ano,
  if (!is.null(upra_fa_dep_all)) upra_fa_dep_all$ano,
  if (!is.null(apadt_dep_all))  apadt_dep_all$ano,
  if (!is.null(eva_dep_all))    eva_dep_all$ano,
  if (!is.null(ica_dep_all))    ica_dep_all$ano
)), na.last = NA)

default_fixed_year <- if (length(all_years_dep)) max(all_years_dep, na.rm = TRUE) else NA_integer_

# =========================================================
# UI
# =========================================================
default_base_estructura <- unname(choices_por_dimension("Estructura")[1])

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

    .filters{ background:#fff; border:1px solid var(--ecv-bdr); border-radius:16px; padding:14px 16px; margin-bottom:14px; box-shadow:0 2px 6px rgba(0,0,0,.03); }
    .filters-grid-1{ display:grid; grid-template-columns:1fr; gap:10px; align-items:stretch; }
    .filters-grid-2{ display:grid; grid-template-columns:1fr 1fr; gap:10px; align-items:end; }
    .filter-label{ font-size:13px; font-weight:500; color:#111827; margin-bottom:4px; word-wrap:break-word; overflow-wrap:break-word; }
    .filters .shiny-input-container{ margin-bottom:0 !important; width:100% !important; }
    .filters .form-select{ border:2px solid var(--ecv-bdr) !important; border-radius:10px !important; background:#fff !important; box-shadow:none !important; height:46px; min-height:46px; color:#111827; }
    .filters .form-select:focus{ border-color:var(--ecv-bdr) !important; box-shadow:0 0 0 0.15rem rgba(153,213,236,.55) !important; }

    .card{
      background:#fff; border:1px solid var(--ecv-bdr); border-radius:18px;
      padding:18px 18px 20px; box-shadow:0 2px 10px rgba(0,0,0,.05);
      display:flex; flex-direction:column;
      min-height:unset; height:auto;
    }

    .card-title-small{ font-weight:600; font-size:13px; text-transform:uppercase; letter-spacing:.05em; color:#6b7280; margin-bottom:8px; }
    .nav-tabs .nav-link{ font-weight:600; }
    .nav-tabs .nav-link.active{ font-weight:700; }

    .ts-grid{ display:grid; grid-template-columns: 2.15fr 1fr; gap:16px; align-items:stretch; }
    .ts-left{ min-height:unset; height:auto; display:flex; flex-direction:column; gap:10px; }
    .ts-right{ display:flex; flex-direction:column; gap:10px; }

    .kpi-card{ background:#fff; border:1px solid var(--ecv-bdr); border-radius:16px; padding:12px 14px; box-shadow:0 2px 6px rgba(0,0,0,.03); }
    .kpi-title{ font-weight:700; font-size:12px; letter-spacing:.06em; text-transform:uppercase; color:#6b7280; margin-bottom:8px; }
    .kpi-body{ font-size:13px; color:#111827; line-height:1.38; }
    .kpi-table{ display:flex; flex-direction:column; gap:6px; }
    .kpi-row{ display:flex; justify-content:space-between; align-items:baseline; border-top:1px dashed rgba(153,213,236,.55); padding-top:6px; }
    .kpi-row:first-child{ border-top:none; padding-top:0; }
    .kpi-k{ color:#374151; font-size:12px; }
    .kpi-v{ color:#111827; font-size:12px; font-weight:700; }

    .grid-2maps{ display:grid; grid-template-columns:1fr 1fr; gap:20px; align-items:stretch; }
    .map-panel{ display:flex; flex-direction:column; gap:10px; background:#fff; border:1px solid var(--ecv-bdr); border-radius:16px; padding:14px 14px 16px; box-shadow:0 2px 6px rgba(0,0,0,.03); }
    .map-panel .filters{ border:none; box-shadow:none; padding:0; margin:0 0 8px 0; }

    .filters-map{
      height:auto !important;
      min-height:unset !important;
      display:flex;
      flex-direction:column;
      justify-content:flex-start;
      overflow:visible;
    }

    .eva-dt-filters{
      display:grid; grid-template-columns:1fr 1fr; gap:10px;
      background:#fff; border:1px solid rgba(153,213,236,.70);
      border-radius:14px; padding:10px 12px;
    }

    @media (max-width:980px){
      .filters-grid-2{ grid-template-columns:1fr; }
      .grid-2maps{ grid-template-columns:1fr; }
      .filters-map{ height:auto; overflow:visible; }
      .ts-grid{ grid-template-columns:1fr; }
      .eva-dt-filters{ grid-template-columns:1fr; }
    }
  ")))
  ),
  div(
    class = "wrap",
    h3(""),
    div(
      class = "card",
      div(class = "card-title-small", ""),
      tabsetPanel(
        id = "tabs_estructura",
        
        tabPanel(
          title = "Indicadores a nivel nacional",
          div(
            class = "filters",
            div(
              class = "filters-grid-1",
              div(
                lbl("¿Cuál información de la dimensión de estructura quiere analizar?"),
                selector_base("estr_base1", "Estructura")
              ),
              uiOutput("estr_extra1")
            )
          ),
          div(
            class = "ts-grid",
            div(class = "ts-left", uiOutput("estr_left_ui")),
            div(class = "ts-right", uiOutput("estr_cards"))
          )
        ),
        
        tabPanel(
          title = "Comparativos departamentales",
          div(
            class = "filters",
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
            )
          ),
          div(
            class = "grid-2maps",
            
            div(
              class = "map-panel",
              div(class = "card-title-small", "INDICADOR 1"),
              div(
                class = "filters filters-map",
                div(
                  class = "filters-grid-1",
                  div(
                    lbl("¿Qué base de Estructura analizamos?"),
                    selector_base("estr_base2_map1", "Estructura")
                  ),
                  uiOutput("map1_extra"),
                  div(
                    class = "filters-grid-2",
                    div(lbl("¿Qué año analizamos?"), uiOutput("map1_year_ui"))
                  )
                )
              ),
              leafletOutput("map_idm_1", height = paste0(H_MAP, "px"))
            ),
            
            div(
              class = "map-panel",
              div(class = "card-title-small", "INDICADOR 2"),
              div(
                class = "filters filters-map",
                div(
                  class = "filters-grid-1",
                  div(
                    lbl("¿Qué base de Estructura analizamos?"),
                    selector_base("estr_base2_map2", "Estructura")
                  ),
                  uiOutput("map2_extra"),
                  div(
                    class = "filters-grid-2",
                    div(lbl("¿Qué año analizamos?"), uiOutput("map2_year_ui"))
                  )
                )
              ),
              leafletOutput("map_idm_2", height = paste0(H_MAP, "px"))
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
  
  # ----- Extra UI Pestaña 1 -----
  output$estr_extra1 <- renderUI({
    base_sel <- input$estr_base1
    if (is.null(base_sel)) return(NULL)
    
    if (base_sel == "DANE_POPULATION") {
      tagList(
        div(lbl("Indicador demográfico"),
            selectInput("estr_pob_var", label = NULL, choices = pob_var_choices, selected = "poblacion_total"))
      )
    } else if (base_sel == "HANSEN_COBERTURA_BOSQUE") {
      tagList(
        div(lbl("Indicador de cobertura de bosque"),
            selectInput("estr_hansen_metric", label = NULL, choices = hansen_metric_choices, selected = "pct"))
      )
    } else if (base_sel == "DNP-IDM") {
      tagList(
        div(lbl("Filtro para el IDM"),
            selectInput("estr_idm_segmento", label = NULL, choices = idm_seg_choices, selected = "__ALL__"))
      )
    } else if (base_sel == "UPRA_EVA_A") {
      tagList(
        div(lbl("Cultivo"),
            selectInput("estr_eva_cultivo", label = NULL, choices = eva_cultivo_choices, selected = "__ALL__"))
      )
    } else if (base_sel == "ICA_CENSO_PECUARIO") {
      tagList(
        div(lbl("Tipo de animal (ICA)"),
            selectInput("estr_ica_animal", label = NULL, choices = ica_animal_choices,
                        selected = names(ica_animal_choices)[1] %||% "Bovinos"))
      )
    } else {
      NULL
    }
  })
  
  # ----- UI del panel izquierdo (Pestaña 1) -----
  output$estr_left_ui <- renderUI({
    base_sel <- input$estr_base1 %||% default_base_estructura
    
    if (base_sel %in% c("UPRA_FA","UPRA_APADT")) {
      tagList(plotlyOutput("estr_bar", height = paste0(H_PLOT, "px")))
    } else if (base_sel == "UPRA_EVA_A") {
      tagList(
        plotlyOutput("estr_ts", height = paste0(H_PLOT, "px")),
        uiOutput("eva_dt_filters_ui"),
        DTOutput("eva_dt")
      )
    } else {
      plotlyOutput("estr_ts", height = paste0(H_PLOT, "px"))
    }
  })
  
  # ----- Plot principal (serie temporal) -----
  output$estr_ts <- renderPlotly({
    base_sel <- input$estr_base1 %||% default_base_estructura
    
    p <- if (base_sel == "DANE_POPULATION") {
      make_pob_ts_plot(input$estr_pob_var %||% "poblacion_total")
    } else if (base_sel == "HANSEN_COBERTURA_BOSQUE") {
      make_hansen_ts_plot(input$estr_hansen_metric %||% "pct")
    } else if (base_sel == "DNP-IDM") {
      make_idm_ts_plot(input$estr_idm_segmento %||% "__ALL__")
    } else if (base_sel == "UPRA_EVA_A") {
      make_eva_ts_plot_dual(input$estr_eva_cultivo %||% "__ALL__")
    } else if (base_sel == "ICA_CENSO_PECUARIO") {
      make_ica_ts_plot_unified(input$estr_ica_animal %||% names(ica_animal_choices)[1] %||% "Bovinos")
    } else if (base_sel %in% c("UPRA_FA","UPRA_APADT")) {
      empty_ts_plot("Este sistema se visualiza como barras (ver gráfico de barras).")
    } else {
      empty_ts_plot("Objeto visual vacío — seleccione un sistema con serie temporal.")
    }
    
    force_plot_color(p, COL_PLOT)
  })
  
  # ----- Plot barras (UPRA_FA / UPRA_APADT) -----
  output$estr_bar <- renderPlotly({
    base_sel <- input$estr_base1 %||% default_base_estructura
    
    p <- if (base_sel == "UPRA_FA") {
      make_upra_fa_bar_plot()
    } else if (base_sel == "UPRA_APADT") {
      make_upra_apadt_bar_plot()
    } else {
      empty_ts_plot(" ")
    }
    
    force_plot_color(p, COL_PLOT)
  })
  
  # =========================================================
  # EVA: filtros DT (Año y Cultivo) + tabla
  # =========================================================
  output$eva_dt_filters_ui <- renderUI({
    base_sel <- input$estr_base1 %||% default_base_estructura
    if (base_sel != "UPRA_EVA_A") return(NULL)
    if (is.null(eva_raw) || !nrow(eva_raw)) return(NULL)
    
    yrs <- sort(unique(eva_raw$ano))
    tagList(
      div(
        class = "eva-dt-filters",
        div(
          lbl("Año (tabla)"),
          selectInput("eva_dt_year", label = NULL,
                      choices = c("Todos los años"="__ALL__", stats::setNames(yrs, yrs)),
                      selected = "__ALL__")
        ),
        div(
          lbl("Cultivo (tabla)"),
          selectInput("eva_dt_cultivo", label = NULL,
                      choices = eva_cultivo_choices,
                      selected = "__ALL__")
        )
      )
    )
  })
  
  eva_dt_data <- reactive({
    if (is.null(eva_raw) || !nrow(eva_raw)) return(NULL)
    
    df <- eva_raw
    
    yr_sel  <- input$eva_dt_year %||% "__ALL__"
    cul_sel <- input$eva_dt_cultivo %||% "__ALL__"
    
    if (!is.null(yr_sel) && yr_sel != "__ALL__") df <- df %>% dplyr::filter(ano == as.integer(yr_sel))
    if (!is.null(cul_sel) && cul_sel != "__ALL__") df <- df %>% dplyr::filter(cultivo == cul_sel)
    
    if (!nrow(df)) return(NULL)
    
    df %>%
      dplyr::group_by(ano, cultivo) %>%
      dplyr::summarise(
        area_sembrada_ha  = sum(area_sembrada_ha,  na.rm = TRUE),
        area_cosechada_ha = sum(area_cosechada_ha, na.rm = TRUE),
        produccion_t      = sum(produccion_t,      na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(ano), dplyr::desc(produccion_t))
  })
  
  output$eva_dt <- DT::renderDT({
    base_sel <- input$estr_base1 %||% default_base_estructura
    if (base_sel != "UPRA_EVA_A") return(DT::datatable(data.frame()))
    
    df <- eva_dt_data()
    if (is.null(df) || !nrow(df)) {
      return(DT::datatable(data.frame(Mensaje = "Sin datos para los filtros seleccionados."), rownames = FALSE))
    }
    
    out <- df %>%
      dplyr::transmute(
        Año = ano,
        Cultivo = cultivo,
        `Área sembrada (ha)`  = fmt_short(area_sembrada_ha),
        `Área cosechada (ha)` = fmt_short(area_cosechada_ha),
        `Producción (t)`      = fmt_short(produccion_t)
      )
    
    DT::datatable(
      out,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 15, 25, 50),
        scrollX = TRUE,
        autoWidth = TRUE,
        order = list(list(0, "desc"))
      )
    )
  })
  
  # =========================================================
  # Tarjetas (storytelling formal + último año + crecimiento)
  # =========================================================
  current_summary <- reactive({
    base_sel <- input$estr_base1 %||% default_base_estructura
    
    out <- list(
      story = "No se dispone de información suficiente para construir la síntesis con los filtros actuales.",
      last_rows = list(list(k="Año", v=na_txt()), list(k="Valor", v=na_txt())),
      growth_rows = list(list(k="Año anterior", v=na_txt()), list(k="Tasa de crecimiento", v=na_txt()))
    )
    
    # ---- ICA (unificado) ----
    if (base_sel == "ICA_CENSO_PECUARIO") {
      if (is.null(ica_long) || !nrow(ica_long)) return(out)
      animal_sel <- input$estr_ica_animal %||% names(ica_animal_choices)[1] %||% "Bovinos"
      df <- ica_long %>% dplyr::filter(animal == animal_sel)
      if (!nrow(df)) return(out)
      
      ts <- df %>%
        dplyr::group_by(ano) %>%
        dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(ano)
      
      last_year <- max(ts$ano, na.rm = TRUE)
      last <- ts %>% dplyr::filter(ano == last_year) %>% dplyr::slice(1)
      prev <- ts %>% dplyr::filter(ano == (last_year - 1)) %>% dplyr::slice(1)
      
      val_last <- last$valor
      val_prev <- if (nrow(prev)) prev$valor else NA_real_
      gr <- safe_growth(val_last, val_prev)
      
      out$story <- sprintf(
        "La serie presenta la evolución nacional del censo pecuario (ICA) para %s. En %d, el inventario alcanza %s. La variación interanual es %s.",
        animal_sel, last_year, fmt_short(val_last),
        ifelse(is.na(gr), "NA", fmt_pct(100*gr, 1))
      )
      
      out$last_rows <- list(
        list(k="Año", v=as.character(last_year)),
        list(k=animal_sel, v=fmt_short(val_last))
      )
      out$growth_rows <- list(
        list(k="Año anterior", v=if (nrow(prev)) as.character(last_year-1) else na_txt()),
        list(k="Tasa de crecimiento", v=ifelse(is.na(gr), na_txt(), fmt_pct(100*gr, 1)))
      )
      return(out)
    }
    
    # ---- DEMOGRAFÍA ----
    if (base_sel == "DANE_POPULATION" && !is.null(pob_ts) && nrow(pob_ts)) {
      var_code <- input$estr_pob_var %||% "poblacion_total"
      df <- pob_ts
      last_year <- max(df$ano, na.rm = TRUE)
      last <- df %>% dplyr::filter(ano == last_year) %>% dplyr::slice(1)
      prev <- df %>% dplyr::filter(ano == (last_year - 1)) %>% dplyr::slice(1)
      
      if (var_code == "razon_dependencia") {
        val_last <- last$razon_dependencia * 100
        val_prev <- if (nrow(prev)) prev$razon_dependencia * 100 else NA_real_
        gr <- safe_growth(val_last, val_prev)
        
        out$story <- sprintf(
          "La serie presenta la evolución nacional de la razón de dependencia (en porcentaje). En %d, el indicador se ubica en %s. La variación frente al año inmediatamente anterior corresponde a %s.",
          last_year, fmt_pct(val_last, 1),
          ifelse(is.na(gr), "NA", fmt_pct(100*gr, 1))
        )
        out$last_rows <- list(list(k="Año", v=as.character(last_year)), list(k="Razón de dependencia", v=fmt_pct(val_last, 1)))
        out$growth_rows <- list(
          list(k="Año anterior", v=if (nrow(prev)) as.character(last_year-1) else na_txt()),
          list(k="Tasa de crecimiento", v=ifelse(is.na(gr), na_txt(), fmt_pct(100*gr, 1)))
        )
      } else {
        val_last <- last[[var_code]]
        val_prev <- if (nrow(prev)) prev[[var_code]] else NA_real_
        gr <- safe_growth(val_last, val_prev)
        
        out$story <- sprintf(
          "La serie presenta la trayectoria nacional del indicador. En %d, el valor alcanza %s. En comparación con %d, la variación corresponde a %s.",
          last_year, fmt_short(val_last),
          last_year-1,
          ifelse(is.na(gr), "NA", fmt_pct(100*gr, 1))
        )
        out$last_rows <- list(list(k="Año", v=as.character(last_year)), list(k="Valor", v=fmt_short(val_last)))
        out$growth_rows <- list(
          list(k="Año anterior", v=if (nrow(prev)) as.character(last_year-1) else na_txt()),
          list(k="Tasa de crecimiento", v=ifelse(is.na(gr), na_txt(), fmt_pct(100*gr, 1)))
        )
      }
      return(out)
    }
    
    # ---- HANSEN ----
    if (base_sel == "HANSEN_COBERTURA_BOSQUE" && !is.null(hansen_ts) && nrow(hansen_ts)) {
      metric <- input$estr_hansen_metric %||% "pct"
      df <- hansen_ts
      last_year <- max(df$anio, na.rm = TRUE)
      last <- df %>% dplyr::filter(anio == last_year) %>% dplyr::slice(1)
      prev <- df %>% dplyr::filter(anio == (last_year - 1)) %>% dplyr::slice(1)
      
      if (metric == "ha") {
        val_last <- last$cobertura_ha
        val_prev <- if (nrow(prev)) prev$cobertura_ha else NA_real_
        gr <- safe_growth(val_last, val_prev)
        out$story <- sprintf(
          "La serie consolida la cobertura neta de bosque a nivel nacional, expresada en hectáreas. En %d, la cobertura estimada es %s ha. La variación interanual es %s.",
          last_year, fmt_short(val_last),
          ifelse(is.na(gr), "NA", fmt_pct(100*gr, 1))
        )
        out$last_rows <- list(list(k="Año", v=as.character(last_year)), list(k="Cobertura (hectáreas)", v=fmt_short(val_last)))
      } else {
        val_last <- last$cobertura_pct
        val_prev <- if (nrow(prev)) prev$cobertura_pct else NA_real_
        gr <- safe_growth(val_last, val_prev)
        out$story <- sprintf(
          "La serie reporta la cobertura neta de bosque como porcentaje de la base considerada. En %d, el país se ubica en %s. La variación frente al año anterior es %s.",
          last_year, fmt_pct(val_last, 1),
          ifelse(is.na(gr), "NA", fmt_pct(100*gr, 1))
        )
        out$last_rows <- list(list(k="Año", v=as.character(last_year)), list(k="Cobertura (%)", v=fmt_pct(val_last, 1)))
      }
      
      out$growth_rows <- list(
        list(k="Año anterior", v=if (nrow(prev)) as.character(last_year-1) else na_txt()),
        list(k="Tasa de crecimiento", v=ifelse(is.na(gr), na_txt(), fmt_pct(100*gr, 1)))
      )
      return(out)
    }
    
    # ---- IDM ----
    if (base_sel == "DNP-IDM" && !is.null(idm_raw) && nrow(idm_raw)) {
      
      ts <- idm_raw %>%
        dplyr::group_by(ano) %>%
        dplyr::summarise(valor = mean(valor_bruto, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(ano)
      
      last_year <- max(ts$ano, na.rm = TRUE)
      last <- ts %>% dplyr::filter(ano == last_year) %>% dplyr::slice(1)
      prev <- ts %>% dplyr::filter(ano == (last_year - 1)) %>% dplyr::slice(1)
      
      val_last <- last$valor
      val_prev <- if (nrow(prev)) prev$valor else NA_real_
      gr <- safe_growth(val_last, val_prev)
      
      out$story <- sprintf(
        "La serie muestra el comportamiento del Índice de Desempeño Municipal (IDM) como promedio nacional (promedio simple municipal). En %d, el valor promedio es %s puntos; la variación interanual corresponde a %s.",
        last_year, fmt_num_es(val_last, 2),
        ifelse(is.na(gr), "NA", fmt_pct(100*gr, 1))
      )
      
      out$last_rows <- list(list(k="Año", v=as.character(last_year)), list(k="IDM promedio", v=fmt_num_es(val_last, 2)))
      out$growth_rows <- list(
        list(k="Año anterior", v=if (nrow(prev)) as.character(last_year-1) else na_txt()),
        list(k="Tasa de crecimiento", v=ifelse(is.na(gr), na_txt(), fmt_pct(100*gr, 1)))
      )
      return(out)
    }
    
    # ---- UPRA_FA ----
    if (base_sel == "UPRA_FA" && !is.null(upra_fa_ts) && nrow(upra_fa_ts)) {
      df <- upra_fa_ts
      last_year <- max(df$ano, na.rm = TRUE)
      last <- df %>% dplyr::filter(ano == last_year) %>% dplyr::slice(1)
      prev <- df %>% dplyr::filter(ano == (last_year - 1)) %>% dplyr::slice(1)
      
      gr_area <- safe_growth(last$area_fa_ha, if (nrow(prev)) prev$area_fa_ha else NA_real_)
      gr_part <- safe_growth(last$participacion, if (nrow(prev)) prev$participacion else NA_real_)
      
      out$story <- sprintf(
        "A nivel nacional, en %d la frontera agropecuaria suma %s ha sobre %s ha totales (participación: %s). La variación interanual es %s (área en frontera) y %s (participación).",
        last_year, fmt_short(last$area_fa_ha), fmt_short(last$area_tot_ha), fmt_pct(last$participacion, 1),
        ifelse(is.na(gr_area), "NA", fmt_pct(100*gr_area, 1)),
        ifelse(is.na(gr_part), "NA", fmt_pct(100*gr_part, 1))
      )
      
      out$last_rows <- list(
        list(k="Año", v=as.character(last_year)),
        list(k="Área total (hectáreas)", v=fmt_short(last$area_tot_ha)),
        list(k="Área en frontera (hectáreas)", v=fmt_short(last$area_fa_ha)),
        list(k="Participación", v=fmt_pct(last$participacion, 1))
      )
      out$growth_rows <- list(
        list(k="Año anterior", v=if (nrow(prev)) as.character(last_year-1) else na_txt()),
        list(k="Crec. área en frontera", v=ifelse(is.na(gr_area), na_txt(), fmt_pct(100*gr_area, 1))),
        list(k="Crec. participación", v=ifelse(is.na(gr_part), na_txt(), fmt_pct(100*gr_part, 1)))
      )
      return(out)
    }
    
    # ---- UPRA_APADT ----
    if (base_sel == "UPRA_APADT" && !is.null(upra_apadt_ts) && nrow(upra_apadt_ts)) {
      df <- upra_apadt_ts
      last_year <- max(df$ano, na.rm = TRUE)
      last <- df %>% dplyr::filter(ano == last_year) %>% dplyr::slice(1)
      prev <- df %>% dplyr::filter(ano == (last_year - 1)) %>% dplyr::slice(1)
      
      gr_area <- safe_growth(last$area_pot_ha, if (nrow(prev)) prev$area_pot_ha else NA_real_)
      
      out$story <- sprintf(
        "En %d, el área potencial (riego/drenaje) alcanza %s ha sobre %s ha totales. La variación interanual del área potencial es %s.",
        last_year, fmt_short(last$area_pot_ha), fmt_short(last$area_tot_ha),
        ifelse(is.na(gr_area), "NA", fmt_pct(100*gr_area, 1))
      )
      
      out$last_rows <- list(
        list(k="Año", v=as.character(last_year)),
        list(k="Área total (hectáreas)", v=fmt_short(last$area_tot_ha)),
        list(k="Área potencial (hectáreas)", v=fmt_short(last$area_pot_ha))
      )
      out$growth_rows <- list(
        list(k="Año anterior", v=if (nrow(prev)) as.character(last_year-1) else na_txt()),
        list(k="Crec. área potencial", v=ifelse(is.na(gr_area), na_txt(), fmt_pct(100*gr_area, 1)))
      )
      return(out)
    }
    
    # ---- EVA ----
    if (base_sel == "UPRA_EVA_A" && !is.null(eva_raw) && nrow(eva_raw)) {
      cultivo_sel <- input$estr_eva_cultivo %||% "__ALL__"
      df <- eva_raw
      if (!is.null(cultivo_sel) && cultivo_sel != "__ALL__") df <- df %>% dplyr::filter(cultivo == cultivo_sel)
      if (!nrow(df)) return(out)
      
      ts <- df %>%
        dplyr::group_by(ano) %>%
        dplyr::summarise(
          area_sembrada_ha  = sum(area_sembrada_ha,  na.rm = TRUE),
          area_cosechada_ha = sum(area_cosechada_ha, na.rm = TRUE),
          produccion_t      = sum(produccion_t,      na.rm = TRUE),
          .groups = "drop"
        ) %>% dplyr::arrange(ano)
      
      last_year <- max(ts$ano, na.rm = TRUE)
      last <- ts %>% dplyr::filter(ano == last_year) %>% dplyr::slice(1)
      prev <- ts %>% dplyr::filter(ano == (last_year - 1)) %>% dplyr::slice(1)
      
      gr_p <- safe_growth(last$produccion_t, if (nrow(prev)) prev$produccion_t else NA_real_)
      cul_txt <- if (cultivo_sel == "__ALL__") "el conjunto de cultivos" else cultivo_sel
      
      out$story <- sprintf(
        "Para %s, en %d la producción total alcanza %s t (área sembrada: %s ha; área cosechada: %s ha). La variación interanual de la producción es %s.",
        cul_txt, last_year,
        fmt_short(last$produccion_t), fmt_short(last$area_sembrada_ha), fmt_short(last$area_cosechada_ha),
        ifelse(is.na(gr_p), "NA", fmt_pct(100*gr_p, 1))
      )
      
      out$last_rows <- list(
        list(k="Año", v=as.character(last_year)),
        list(k="Área sembrada (hectáreas)", v=fmt_short(last$area_sembrada_ha)),
        list(k="Área cosechada (hectáreas)", v=fmt_short(last$area_cosechada_ha)),
        list(k="Producción (toneladas)", v=fmt_short(last$produccion_t))
      )
      out$growth_rows <- list(
        list(k="Año anterior", v=if (nrow(prev)) as.character(last_year-1) else na_txt()),
        list(k="Crec. producción", v=ifelse(is.na(gr_p), na_txt(), fmt_pct(100*gr_p, 1)))
      )
      return(out)
    }
    
    out
  })
  
  output$estr_cards <- renderUI({
    s <- current_summary()
    tagList(
      card_box("Lectura analítica", div(s$story)),
      card_box("Último año (niveles)", kv_table(s$last_rows)),
      card_box("Variación interanual (último año vs. anterior)", kv_table(s$growth_rows))
    )
  })
  
  # =======================================================
  # Pestaña 2 — filtros año fijo
  # =======================================================
  output$fixed_year_ui <- renderUI({
    lock <- input$lock_years %||% FALSE
    if (!isTRUE(lock)) return(tags$span("Active el año fijo para usar este filtro.", style = "font-size:12px;color:#9ca3af;"))
    yrs <- all_years_dep
    if (!length(yrs)) return(tags$span("Sin años disponibles.", style = "font-size:12px;color:#9ca3af;"))
    selectInput("fixed_year", label = NULL, choices = yrs, selected = default_fixed_year)
  })
  
  get_dep_data <- function(ind_code){
    switch(ind_code,
           "POBLACION"  = pob_dep_all,
           "IDM"        = idm_dep_all,
           "HANSEN"     = hansen_dep_all,
           "UPRA_FA"    = upra_fa_dep_all,
           "UPRA_APADT" = apadt_dep_all,
           "EVA"        = eva_dep_all,
           "ICA"        = ica_dep_all,
           NULL
    )
  }
  
  render_extra_map <- function(base_input_id, output_id){
    output[[output_id]] <- renderUI({
      base_sel <- input[[base_input_id]]
      if (is.null(base_sel)) return(NULL)
      
      if (base_sel == "DANE_POPULATION") {
        div(
          lbl("Variable demográfica"),
          selectInput(paste0(output_id, "_var"), label = NULL, choices = pob_var_choices, selected = "poblacion_total")
        )
      } else if (base_sel == "HANSEN_COBERTURA_BOSQUE") {
        div(
          lbl("Indicador de cobertura de bosque"),
          selectInput(paste0(output_id, "_metric"), label = NULL, choices = hansen_metric_choices, selected = "pct")
        )
      } else if (base_sel == "UPRA_EVA_A") {
        tagList(
          div(
            lbl("Métrica (Condiciones productivas)"),
            selectInput(paste0(output_id, "_eva_metric"), label = NULL, choices = eva_metric_choices, selected = "produccion_t")
          ),
          div(
            lbl("Cultivo"),
            selectInput(paste0(output_id, "_eva_cultivo"), label = NULL, choices = eva_cultivo_choices, selected = "__ALL__")
          )
        )
      } else if (base_sel == "ICA_CENSO_PECUARIO") {
        div(
          lbl("Tipo de animal (ICA)"),
          selectInput(paste0(output_id, "_ica_animal"), label = NULL, choices = ica_animal_choices,
                      selected = names(ica_animal_choices)[1] %||% "Bovinos")
        )
      } else {
        NULL
      }
    })
  }
  render_extra_map("estr_base2_map1", "map1_extra")
  render_extra_map("estr_base2_map2", "map2_extra")
  
  output$map1_year_ui <- renderUI({
    lock <- input$lock_years %||% FALSE
    if (isTRUE(lock)) return(tags$span("Año controlado por el filtro global.", style = "font-size:12px;color:#6b7280;"))
    
    base_sel <- input$estr_base2_map1 %||% default_base_estructura
    ind_sel  <- base_to_indicator_code(base_sel)
    df <- get_dep_data(ind_sel)
    if (is.null(df) || !nrow(df)) return(tags$span("Sin años disponibles para esta base", style="font-size:12px;color:#9ca3af;"))
    
    yrs <- sort(unique(df$ano))
    selectInput("map1_year", label = NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  output$map2_year_ui <- renderUI({
    lock <- input$lock_years %||% FALSE
    if (isTRUE(lock)) return(tags$span("Año controlado por el filtro global.", style = "font-size:12px;color:#6b7280;"))
    
    base_sel <- input$estr_base2_map2 %||% default_base_estructura
    ind_sel  <- base_to_indicator_code(base_sel)
    df <- get_dep_data(ind_sel)
    if (is.null(df) || !nrow(df)) return(tags$span("Sin años disponibles para esta base", style="font-size:12px;color:#9ca3af;"))
    
    yrs <- sort(unique(df$ano))
    selectInput("map2_year", label = NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  map1_data <- reactive({
    base_sel <- input$estr_base2_map1 %||% default_base_estructura
    lock     <- input$lock_years %||% FALSE
    yr       <- if (isTRUE(lock)) input$fixed_year else input$map1_year
    ind_sel  <- base_to_indicator_code(base_sel)
    
    df <- get_dep_data(ind_sel)
    if (is.null(df) || is.null(yr)) return(NULL)
    df <- df %>% dplyr::filter(ano == as.integer(yr))
    
    if (ind_sel == "POBLACION") {
      var_sel <- input$map1_extra_var %||% "poblacion_total"
      df <- df %>% dplyr::mutate(valor = dplyr::case_when(
        var_sel == "poblacion_total"   ~ poblacion_total,
        var_sel == "poblacion_joven"   ~ poblacion_joven,
        var_sel == "poblacion_mayor"   ~ poblacion_mayor,
        var_sel == "razon_dependencia" ~ razon_dependencia * 100,
        TRUE                           ~ poblacion_total
      ))
    } else if (ind_sel == "HANSEN") {
      metric_sel <- input$map1_extra_metric %||% "pct"
      df$valor <- if (metric_sel == "ha") df$cobertura_ha else df$cobertura_pct
    } else if (ind_sel == "EVA") {
      met <- input$map1_extra_eva_metric %||% "produccion_t"
      cul <- input$map1_extra_eva_cultivo %||% "__ALL__"
      if (cul != "__ALL__") df <- df %>% dplyr::filter(cultivo == cul)
      if (!nrow(df)) return(NULL)
      df <- df %>% dplyr::mutate(valor = .data[[met]])
    } else if (ind_sel == "ICA") {
      ani <- input$map1_extra_ica_animal %||% names(ica_animal_choices)[1] %||% "Bovinos"
      df <- df %>% dplyr::filter(animal == ani)
      if (!nrow(df)) return(NULL)
    }
    # IDM ya viene como df$valor
    
    if (!nrow(df) || is.null(shp_dep) || is.na(dep_code_col_shp)) return(NULL)
    shp <- shp_dep
    shp$cod_dep_join <- normalize_cod_dep(shp[[dep_code_col_shp]])
    
    df$cod_dep_join  <- normalize_cod_dep(df$cod_dep)
    dplyr::left_join(shp, df, by = "cod_dep_join")
  })
  
  map2_data <- reactive({
    base_sel <- input$estr_base2_map2 %||% default_base_estructura
    lock     <- input$lock_years %||% FALSE
    yr       <- if (isTRUE(lock)) input$fixed_year else input$map2_year
    ind_sel  <- base_to_indicator_code(base_sel)
    
    df <- get_dep_data(ind_sel)
    if (is.null(df) || is.null(yr)) return(NULL)
    df <- df %>% dplyr::filter(ano == as.integer(yr))
    
    if (ind_sel == "POBLACION") {
      var_sel <- input$map2_extra_var %||% "poblacion_total"
      df <- df %>% dplyr::mutate(valor = dplyr::case_when(
        var_sel == "poblacion_total"   ~ poblacion_total,
        var_sel == "poblacion_joven"   ~ poblacion_joven,
        var_sel == "poblacion_mayor"   ~ poblacion_mayor,
        var_sel == "razon_dependencia" ~ razon_dependencia * 100,
        TRUE                           ~ poblacion_total
      ))
    } else if (ind_sel == "HANSEN") {
      metric_sel <- input$map2_extra_metric %||% "pct"
      df$valor <- if (metric_sel == "ha") df$cobertura_ha else df$cobertura_pct
    } else if (ind_sel == "EVA") {
      met <- input$map2_extra_eva_metric %||% "produccion_t"
      cul <- input$map2_extra_eva_cultivo %||% "__ALL__"
      if (cul != "__ALL__") df <- df %>% dplyr::filter(cultivo == cul)
      if (!nrow(df)) return(NULL)
      df <- df %>% dplyr::mutate(valor = .data[[met]])
    } else if (ind_sel == "ICA") {
      ani <- input$map2_extra_ica_animal %||% names(ica_animal_choices)[1] %||% "Bovinos"
      df <- df %>% dplyr::filter(animal == ani)
      if (!nrow(df)) return(NULL)
    }
    # IDM ya viene como df$valor
    
    if (!nrow(df) || is.null(shp_dep) || is.na(dep_code_col_shp)) return(NULL)
    shp <- shp_dep
    shp$cod_dep_join <- normalize_cod_dep(shp[[dep_code_col_shp]])
    
    df$cod_dep_join  <- normalize_cod_dep(df$cod_dep)
    dplyr::left_join(shp, df, by = "cod_dep_join")
  })
  
  output$map_idm_1 <- leaflet::renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(lng = -74, lat = 4.5, zoom = 5)
  })
  output$map_idm_2 <- leaflet::renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(lng = -74, lat = 4.5, zoom = 5)
  })
  
  # =========================================================
  # Leyenda: desde el 2do rango poner ">A–B"
  # Porcentajes con símbolo "%"
  # =========================================================
  make_bin_labels <- function(bins, value_type = c("percent","number")){
    value_type <- match.arg(value_type)
    bins <- sort(unique(as.numeric(bins)))
    if (length(bins) < 2) return(character(0))
    
    f <- if (value_type == "percent") function(z) fmt_pct(z, 1) else function(z) fmt_short(z)
    
    labs <- vapply(seq_len(length(bins)-1), function(i){
      a <- bins[i]; b <- bins[i+1]
      if (i == 1) paste0(f(a), "–", f(b)) else paste0(">", f(a), "–", f(b))
    }, FUN.VALUE = character(1))
    labs
  }
  
  update_dep_leaflet <- function(map_id, md, palette_name,
                                 legend_title,
                                 value_type = c("percent","number")){
    value_type <- match.arg(value_type)
    
    if (is.null(md) || !"valor" %in% names(md)) {
      leaflet::leafletProxy(map_id) %>% leaflet::clearShapes() %>% leaflet::clearControls()
      return(invisible(NULL))
    }
    
    vals <- md$valor
    vals <- vals[is.finite(vals)]
    if (!length(vals)) {
      leaflet::leafletProxy(map_id) %>% leaflet::clearShapes() %>% leaflet::clearControls()
      return(invisible(NULL))
    }
    
    qs <- stats::quantile(vals, probs = seq(0, 1, length.out = 5), na.rm = TRUE)
    bins <- sort(unique(as.numeric(qs)))
    if (length(bins) < 3) {
      bins <- sort(unique(pretty(range(vals, na.rm = TRUE), n = 5)))
    }
    if (length(bins) < 3) {
      rr <- range(vals, na.rm = TRUE)
      bins <- sort(unique(c(rr[1], mean(rr), rr[2])))
    }
    
    pal <- leaflet::colorBin(palette = palette_name, domain = vals, bins = bins, na.color = "#f5f5f5")
    
    dep_name <- if (!is.na(dep_name_col_shp) && dep_name_col_shp %in% names(md)) {
      as.character(md[[dep_name_col_shp]])
    } else {
      as.character(md$cod_dep_join)
    }
    
    lab_vals <- if (value_type == "percent") fmt_pct(md$valor, 1) else fmt_short(md$valor)
    labels <- sprintf("<strong>%s</strong><br/>Valor: %s", dep_name, lab_vals) %>% lapply(htmltools::HTML)
    
    legend_labels <- make_bin_labels(bins, value_type)
    mids <- (bins[-1] + bins[-length(bins)]) / 2
    legend_cols <- pal(mids)
    
    leaflet::leafletProxy(map_id, data = md) %>%
      leaflet::clearShapes() %>%
      leaflet::clearControls() %>%
      leaflet::addPolygons(
        fillColor   = ~pal(valor),
        weight      = 1,
        opacity     = 1,
        color       = "#ffffff",
        fillOpacity = 0.8,
        highlightOptions = leaflet::highlightOptions(
          weight = 2, color = "#000000", fillOpacity = 0.9, bringToFront = TRUE
        ),
        label = labels
      ) %>%
      leaflet::addLegend(
        colors   = legend_cols,
        labels   = legend_labels,
        opacity  = 0.7,
        title    = legend_title,
        position = "bottomright"
      ) %>%
      leaflet::setView(lng = -74, lat = 4.5, zoom = 5)
  }
  
  observe({
    md       <- map1_data()
    base_sel <- input$estr_base2_map1 %||% default_base_estructura
    ind_sel  <- base_to_indicator_code(base_sel)
    
    var_sel <- input$map1_extra_var %||% "poblacion_total"
    metric1 <- input$map1_extra_metric %||% "pct"
    
    tipo_val <- if (ind_sel %in% c("UPRA_FA","UPRA_APADT") ||
                    (ind_sel == "HANSEN" && metric1 == "pct") ||
                    (ind_sel == "POBLACION" && var_sel == "razon_dependencia")) "percent" else "number"
    
    legend_title <- if (ind_sel == "IDM") "Puntos (0–100)" else if (tipo_val == "percent") "Porcentaje" else "Cantidad"
    
    update_dep_leaflet("map_idm_1", md, palette_name = "Greens",
                       legend_title = legend_title, value_type = tipo_val)
  })
  
  observe({
    md       <- map2_data()
    base_sel <- input$estr_base2_map2 %||% default_base_estructura
    ind_sel  <- base_to_indicator_code(base_sel)
    
    var_sel <- input$map2_extra_var %||% "poblacion_total"
    metric2 <- input$map2_extra_metric %||% "pct"
    
    tipo_val <- if (ind_sel %in% c("UPRA_FA","UPRA_APADT") ||
                    (ind_sel == "HANSEN" && metric2 == "pct") ||
                    (ind_sel == "POBLACION" && var_sel == "razon_dependencia")) "percent" else "number"
    
    legend_title <- if (ind_sel == "IDM") "Puntos (0–100)" else if (tipo_val == "percent") "Porcentaje" else "Cantidad"
    
    update_dep_leaflet("map_idm_2", md, palette_name = "Blues",
                       legend_title = legend_title, value_type = tipo_val)
  })
}

# ✅ Forzar retorno de shiny.appobj (evita "did not return a shiny.appobj object")
app <- shinyApp(ui, server)
app
