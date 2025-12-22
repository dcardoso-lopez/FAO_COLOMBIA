# app_sninny_estructura_multi_bases_con_finagro.R
# (MODIFICADO — POBLACIÓN por quinquenio + labels mapas con DEPARTAMENTO_D)
# (AJUSTE ÚNICO ADICIONAL — FINAGRO: ahora carga 081_FINAGRO_CFA y usa variables de la imagen)
# -------------------------------------------------------------------

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(dplyr); library(tibble); library(stringi); library(htmltools)
  library(plotly); library(scales)
  library(sf); library(leaflet)
  library(DT)
  library(tidyr)
  library(readr)
  library(haven)
  library(stringr)
})

options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---------------- Rutas ----------------
data_dir <- "data"

# =========================================================
# ALTURAS
# =========================================================
H_PLOT <- 650
H_MAP  <- 600

# =========================================================
# Colores globales
# =========================================================
COL_UI   <- "#ffb366"
COL_PLOT <- "#FF8C00"

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

safe_chr <- function(x){
  if (inherits(x, "haven_labelled")) as.character(haven::as_factor(x, levels = "labels"))
  else as.character(x)
}

pick_col_simple <- function(nms, cands){
  for (pat in cands) {
    idx <- which(tolower(nms) == tolower(pat) | grepl(pat, nms, ignore.case = TRUE))[1]
    if (length(idx) && !is.na(idx)) return(nms[idx])
  }
  NA_character_
}

pick_year_col <- function(nms) pick_col_simple(nms, c("ano","año","anio","year"))
pick_dep_col  <- function(nms) pick_col_simple(nms, c("cod_dane_dpto_d","cod_dane_dpto","cod_dpto","cod_depto","dpto_ccdgo","COD_DPTO2","cod_dpto2"))
pick_dep_name_col <- function(nms) pick_col_simple(nms, c("departamento_d","departamento","nom_dpto","depto","NOM_DPTO","DEPARTAMENTO_D"))

fmt_num_es <- function(x, digits = 1){
  scales::number(x, accuracy = 10^-digits, big.mark = ".", decimal.mark = ",")
}
fmt_pct <- function(x, digits = 1) paste0(fmt_num_es(x, digits), "%")

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

year_ticks_5 <- function(years){
  yrs <- sort(unique(as.integer(years[is.finite(years)])))
  if (!length(yrs)) return(NULL)
  y_min <- min(yrs); y_max <- max(yrs)
  tick0 <- floor(y_min/5)*5
  ticks <- seq(tick0, y_max, by = 5)
  if (tail(ticks, 1) != y_max) ticks <- c(ticks, y_max)
  ticks
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

norm_dep2 <- function(x){
  x <- gsub("\\D","",as.character(x)); x[nchar(x)==0] <- NA
  stringi::stri_pad_left(x,2,"0")
}

# =========================================================
# Sistemas + Indicadores (Estructura)
# =========================================================
sistemas_df <- tibble::tribble(
  ~sistema,              ~archivos,
  "DANE_POPULATION",     list("051_DANE_Proyecciones_P.rds"),
  "DANE_ECV",            list("052_DANE_ECV.rds"),
  "INS_SIVIGILA_BPAN",   list("021_INS_SIVIGILA-BPAN.rds"),
  "INS_SIVIGILA_ETA",    list("022_INS_SIVIGILA-ETA.rds"),
  "INS_SIVIGILA_NDA",    list("023_INS_SIVIGILA-NDA.rds"),
  "DNP_SISBEN",          list("031_DNP_SISBEN.rds"),
  "NOAA_PRECIPITACION",  list("131_NOAA_Precipitación.rds"),
  "HANSEN_DEFORESTATION",list("141_HANSEN_DEFORESTATION.rds"),
  # (CAMBIO FINAGRO) antes: 081_FINAGRO_CFA_fast.rds
  "FINAGRO_CFA_FAST",    list("081_FINAGRO_CFA.rds")
) %>%
  mutate(rutas = lapply(archivos, function(v) file.path(data_dir, v)))

indicadores_df <- tibble::tribble(
  ~titulo,                                                               ~dimension,   ~sistema,
  "Transición demográfica territorial — Razón de dependencia",           "Estructura", "DANE_POPULATION",
  "Condiciones de inseguridad alimentaria y consumo de ultra-procesados en los hogares", "Estructura", "DANE_ECV",
  "Enfermedades Transmitidas por Alimentos",                             "Estructura", "INS_SIVIGILA_ETA",
  "Bajo peso al nacer",                                                  "Estructura", "INS_SIVIGILA_BPAN",
  "Desnutrición aguda infantil",                                         "Estructura", "INS_SIVIGILA_NDA",
  "Condiciones socio-económicas de la población",                        "Estructura", "DNP_SISBEN",
  "Lluvia en el territorio",                                             "Estructura", "NOAA_PRECIPITACION",
  "Pérdida de bosque en el territorio",                                  "Estructura", "HANSEN_DEFORESTATION",
  "Financiamiento rural y agropecuario",                                 "Estructura", "FINAGRO_CFA_FAST"
)

choices_por_dimension <- function(dim){
  orden_estructura <- c(
    "DANE_POPULATION",
    "DANE_ECV",
    "INS_SIVIGILA_ETA",
    "INS_SIVIGILA_BPAN",
    "INS_SIVIGILA_NDA",
    "DNP_SISBEN",
    "NOAA_PRECIPITACION",
    "HANSEN_DEFORESTATION",
    "FINAGRO_CFA_FAST"
  )
  df <- indicadores_df %>% dplyr::filter(dimension == dim)
  if (!nrow(df)) return(setNames("NINGUNO", "Sin sistemas asociados para esta dimensión"))
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
    "DANE_POPULATION"      = "DEP_RATIO",
    "DANE_ECV"             = "ECV_FIES",
    "INS_SIVIGILA_BPAN"    = "BPAN",
    "INS_SIVIGILA_ETA"     = "ETA",
    "INS_SIVIGILA_NDA"     = "NDA",
    "DNP_SISBEN"           = "SISBEN",
    "NOAA_PRECIPITACION"   = "PRECIP",
    "HANSEN_DEFORESTATION" = "HANSEN",
    "FINAGRO_CFA_FAST"     = "FINAGRO",
    NA_character_
  )
}

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
  shp_dep$join_code <- if (!is.na(dep_code_col_shp)) as.character(shp_dep[[dep_code_col_shp]]) else NA_character_
  shp_dep$join_name <- if (!is.na(dep_name_col_shp)) stringi::stri_trans_general(as.character(shp_dep[[dep_name_col_shp]]), "Latin-ASCII") else NA_character_
  shp_dep$join_name <- stringi::stri_trim_both(toupper(shp_dep$join_name))
}

join_dep_shp <- function(df, shp, df_key_code = NULL, df_key_name = NULL){
  if (is.null(df) || !nrow(df) || is.null(shp)) return(NULL)
  
  df2 <- df
  
  if (!is.null(df_key_code) && df_key_code %in% names(df2)) {
    df2$join_code <- as.character(df2[[df_key_code]])
  } else {
    df2$join_code <- NA_character_
  }
  
  if (!is.null(df_key_name) && df_key_name %in% names(df2)) {
    nm <- as.character(df2[[df_key_name]])
    nm <- stringi::stri_trans_general(nm, "Latin-ASCII")
    df2$join_name <- stringi::stri_trim_both(toupper(nm))
  } else {
    df2$join_name <- NA_character_
  }
  
  out_code <- suppressWarnings(dplyr::left_join(shp, df2, by = c("join_code" = "join_code")))
  ok_code  <- if ("valor" %in% names(out_code)) mean(!is.na(out_code$valor)) else 0
  out <- if (is.finite(ok_code) && ok_code >= 0.20) out_code else suppressWarnings(dplyr::left_join(shp, df2, by = c("join_name" = "join_name")))
  
  # Asegurar que exista DEPARTAMENTO_D para labels
  if (!("DEPARTAMENTO_D" %in% names(out))) {
    if ("dep_nom" %in% names(out)) out$DEPARTAMENTO_D <- out$dep_nom
    else if ("join_name" %in% names(out)) out$DEPARTAMENTO_D <- out$join_name
    else out$DEPARTAMENTO_D <- out$join_code
  }
  out
}

# =========================================================
# 1) DANE POPULATION — razón de dependencia (serie + dep)
#    AJUSTADO a tu estructura: ano, COD_DANE_DPTO_D, DEPARTAMENTO_D, quinquenio, poblacion
# =========================================================
parse_quinquenio_bounds <- function(q){
  q <- as.character(q)
  q <- stringi::stri_trim_both(q)
  q <- gsub("\\s+", "", q)
  
  if (grepl("\\+$", q)) {
    low <- suppressWarnings(as.integer(gsub("\\+$", "", q)))
    return(list(low = low, high = Inf))
  }
  if (grepl("^[0-9]+-[0-9]+$", q)) {
    parts <- strsplit(q, "-", fixed = TRUE)[[1]]
    low  <- suppressWarnings(as.integer(parts[1]))
    high <- suppressWarnings(as.integer(parts[2]))
    return(list(low = low, high = high))
  }
  list(low = NA_integer_, high = NA_real_)
}

classify_age_bucket <- function(low, high){
  if (is.finite(high) && !is.na(high) && high <= 14) return("joven")
  if (is.finite(low)  && !is.na(low)  && low >= 65)  return("mayor")
  if (is.finite(low) && !is.na(low) && is.finite(high) && !is.na(high) && low >= 15 && high <= 64) return("activa")
  NA_character_
}

pob_raw <- tryCatch({
  p_path <- file.path(data_dir, "051_DANE_Proyecciones_P.rds")
  if (!file.exists(p_path)) {
    message("DANE_POPULATION: no existe -> ", p_path)
    NULL
  } else {
    readRDS(p_path)
  }
}, error = function(e) {
  message("DANE_POPULATION: error -> ", conditionMessage(e))
  NULL
})

pob_ts <- tryCatch({
  if (is.null(pob_raw) || !nrow(pob_raw)) return(NULL)
  
  nms <- names(pob_raw)
  col_ano  <- pick_col_simple(nms, c("ano","año","anio","year"))
  col_dep  <- pick_col_simple(nms, c("COD_DANE_DPTO_D","cod_dane_dpto_d","COD_DPTO2","cod_dpto2","cod_dpto","cod_depto"))
  col_nom  <- pick_col_simple(nms, c("DEPARTAMENTO_D","departamento_d","departamento","nom_dpto","depto"))
  col_q    <- pick_col_simple(nms, c("quinquenio","quinquenio_edad","grupo_edad","edad_grupo"))
  col_pob  <- pick_col_simple(nms, c("poblacion","pob","population"))
  
  if (any(is.na(c(col_ano, col_dep, col_q, col_pob)))) return(NULL)
  
  df <- pob_raw %>%
    transmute(
      ano             = suppressWarnings(as.integer(parse_num_co(.data[[col_ano]]))),
      COD_DANE_DPTO_D  = norm_dep2(.data[[col_dep]]),
      DEPARTAMENTO_D   = if (!is.na(col_nom)) safe_chr(.data[[col_nom]]) else NA_character_,
      quinquenio       = safe_chr(.data[[col_q]]),
      poblacion        = parse_num_co(.data[[col_pob]])
    ) %>%
    filter(!is.na(ano), !is.na(COD_DANE_DPTO_D), nzchar(COD_DANE_DPTO_D),
           is.finite(poblacion), poblacion >= 0, !is.na(quinquenio), nzchar(quinquenio))
  
  if (!nrow(df)) return(NULL)
  
  bnds <- lapply(df$quinquenio, parse_quinquenio_bounds)
  df$age_low  <- vapply(bnds, `[[`, integer(1), "low")
  df$age_high <- vapply(bnds, function(x) as.numeric(x$high), numeric(1))
  df$bucket   <- mapply(classify_age_bucket, df$age_low, df$age_high)
  
  df <- df %>% filter(!is.na(bucket))
  if (!nrow(df)) return(NULL)
  
  df %>%
    group_by(ano, bucket) %>%
    summarise(pob = sum(poblacion, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = bucket, values_from = pob, values_fill = 0) %>%
    mutate(
      poblacion_joven  = joven %||% 0,
      poblacion_mayor  = mayor %||% 0,
      poblacion_activa = activa %||% 0,
      razon_dependencia_pct = if_else(
        poblacion_activa > 0,
        100 * (poblacion_joven + poblacion_mayor) / poblacion_activa,
        NA_real_
      )
    ) %>%
    transmute(
      ano,
      poblacion_joven,
      poblacion_mayor,
      poblacion_activa,
      razon_dependencia_pct
    ) %>%
    arrange(ano)
  
}, error = function(e){
  message("DANE_POPULATION (ts): error -> ", conditionMessage(e))
  NULL
})

pob_dep_all <- tryCatch({
  if (is.null(pob_raw) || !nrow(pob_raw)) return(NULL)
  
  nms <- names(pob_raw)
  col_ano  <- pick_col_simple(nms, c("ano","año","anio","year"))
  col_dep  <- pick_col_simple(nms, c("COD_DANE_DPTO_D","cod_dane_dpto_d","COD_DPTO2","cod_dpto2","cod_dpto","cod_depto"))
  col_nom  <- pick_col_simple(nms, c("DEPARTAMENTO_D","departamento_d","departamento","nom_dpto","depto"))
  col_q    <- pick_col_simple(nms, c("quinquenio","quinquenio_edad","grupo_edad","edad_grupo"))
  col_pob  <- pick_col_simple(nms, c("poblacion","pob","population"))
  
  if (any(is.na(c(col_ano, col_dep, col_q, col_pob)))) return(NULL)
  
  df <- pob_raw %>%
    transmute(
      ano             = suppressWarnings(as.integer(parse_num_co(.data[[col_ano]]))),
      COD_DANE_DPTO_D  = norm_dep2(.data[[col_dep]]),
      DEPARTAMENTO_D   = if (!is.na(col_nom)) safe_chr(.data[[col_nom]]) else NA_character_,
      quinquenio       = safe_chr(.data[[col_q]]),
      poblacion        = parse_num_co(.data[[col_pob]])
    ) %>%
    filter(!is.na(ano), !is.na(COD_DANE_DPTO_D), nzchar(COD_DANE_DPTO_D),
           is.finite(poblacion), poblacion >= 0, !is.na(quinquenio), nzchar(quinquenio))
  
  if (!nrow(df)) return(NULL)
  
  bnds <- lapply(df$quinquenio, parse_quinquenio_bounds)
  df$age_low  <- vapply(bnds, `[[`, integer(1), "low")
  df$age_high <- vapply(bnds, function(x) as.numeric(x$high), numeric(1))
  df$bucket   <- mapply(classify_age_bucket, df$age_low, df$age_high)
  
  df <- df %>% filter(!is.na(bucket))
  if (!nrow(df)) return(NULL)
  
  df %>%
    group_by(ano, COD_DANE_DPTO_D, DEPARTAMENTO_D, bucket) %>%
    summarise(pob = sum(poblacion, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = bucket, values_from = pob, values_fill = 0) %>%
    mutate(
      joven  = joven %||% 0,
      mayor  = mayor %||% 0,
      activa = activa %||% 0,
      valor = if_else(activa > 0, 100 * (joven + mayor) / activa, NA_real_)
    ) %>%
    transmute(
      ano     = ano,
      cod_dep = COD_DANE_DPTO_D,
      dep_nom = DEPARTAMENTO_D,
      DEPARTAMENTO_D = DEPARTAMENTO_D,
      valor   = valor
    ) %>%
    arrange(ano, cod_dep)
  
}, error = function(e){
  message("DANE_POPULATION (dep): error -> ", conditionMessage(e))
  NULL
})

make_dep_ratio_ts_plot <- function(){
  if (is.null(pob_ts) || !nrow(pob_ts)) return(empty_ts_plot("Sin datos demográficos disponibles."))
  df <- pob_ts %>% mutate(valor = razon_dependencia_pct)
  
  ticks_x <- year_ticks_5(df$ano)
  
  plotly::plot_ly(
    data = df, x = ~ano, y = ~valor,
    type = "scatter", mode = "lines+markers",
    line = list(width = 2, color = COL_PLOT),
    marker = list(size = 6, color = COL_PLOT),
    customdata = fmt_pct(df$valor, 1),
    hovertemplate = "<b>Año:</b> %{x}<br><b>Razón de dependencia:</b> %{customdata}<extra></extra>"
  ) %>%
    plotly::layout(
      xaxis = list(title = "", tickmode = "array", tickvals = ticks_x, ticktext = ticks_x, showgrid = FALSE, automargin = TRUE),
      yaxis = list(title = "Razón de dependencia (%)", showgrid = FALSE, automargin = TRUE),
      margin = list(l = 70, r = 20, t = 10, b = 60),
      hovermode = "x unified",
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# =========================================================
# 2) SIVIGILA — BPAN / NDA (conteo) y ETA (sum total_enf)
# =========================================================
load_sivigila <- function(file_name){
  tryCatch({
    p <- file.path(data_dir, file_name)
    if (!file.exists(p)) return(NULL)
    df <- readRDS(p)
    if (is.null(df) || !nrow(df)) return(NULL)
    df
  }, error = function(e) NULL)
}

bpan_raw <- load_sivigila("021_INS_SIVIGILA-BPAN.rds")
eta_raw  <- load_sivigila("022_INS_SIVIGILA-ETA.rds")
nda_raw  <- load_sivigila("023_INS_SIVIGILA-NDA.rds")

prep_sivigila_counts_dep <- function(df){
  if (is.null(df) || !nrow(df)) return(NULL)
  nms <- names(df)
  col_ano <- pick_year_col(nms)
  col_dep <- pick_dep_col(nms)
  col_depname <- pick_dep_name_col(nms)
  if (is.na(col_ano)) return(NULL)
  
  ano <- suppressWarnings(as.integer(df[[col_ano]]))
  cod <- if (!is.na(col_dep)) norm_dep2(df[[col_dep]]) else NA_character_
  nam <- if (!is.na(col_depname)) safe_chr(df[[col_depname]]) else NA_character_
  
  tibble(ano = ano, cod_dep = cod, dep_nom = nam) %>%
    filter(!is.na(ano)) %>%
    group_by(ano, cod_dep, dep_nom) %>%
    summarise(valor = dplyr::n(), .groups = "drop")
}

prep_sivigila_eta_dep <- function(df){
  if (is.null(df) || !nrow(df)) return(NULL)
  nms <- names(df)
  col_ano <- pick_year_col(nms)
  col_dep <- pick_dep_col(nms)
  col_depname <- pick_dep_name_col(nms)
  col_enf <- pick_col_simple(nms, c("total_enf","TOTAL_ENF","enfermos","total_enfermos"))
  if (is.na(col_ano)) return(NULL)
  
  ano <- suppressWarnings(as.integer(df[[col_ano]]))
  cod <- if (!is.na(col_dep)) norm_dep2(df[[col_dep]]) else NA_character_
  nam <- if (!is.na(col_depname)) safe_chr(df[[col_depname]]) else NA_character_
  enf <- if (!is.na(col_enf)) parse_num_co(df[[col_enf]]) else NA_real_
  
  tibble(ano = ano, cod_dep = cod, dep_nom = nam, total_enf = enf) %>%
    filter(!is.na(ano)) %>%
    group_by(ano, cod_dep, dep_nom) %>%
    summarise(valor = sum(total_enf, na.rm = TRUE), .groups = "drop")
}

bpan_dep_all <- prep_sivigila_counts_dep(bpan_raw)
nda_dep_all  <- prep_sivigila_counts_dep(nda_raw)
eta_dep_all  <- prep_sivigila_eta_dep(eta_raw)

bpan_ts <- if (!is.null(bpan_dep_all) && nrow(bpan_dep_all)) bpan_dep_all %>% group_by(ano) %>% summarise(valor = sum(valor, na.rm = TRUE), .groups="drop") %>% arrange(ano) else NULL
nda_ts  <- if (!is.null(nda_dep_all)  && nrow(nda_dep_all))  nda_dep_all  %>% group_by(ano) %>% summarise(valor = sum(valor, na.rm = TRUE), .groups="drop") %>% arrange(ano) else NULL
eta_ts  <- if (!is.null(eta_dep_all)  && nrow(eta_dep_all))  eta_dep_all  %>% group_by(ano) %>% summarise(valor = sum(valor, na.rm = TRUE), .groups="drop") %>% arrange(ano) else NULL

make_sivigila_ts_plot <- function(which_one = c("BPAN","NDA","ETA")){
  which_one <- match.arg(which_one)
  df <- switch(which_one, "BPAN" = bpan_ts, "NDA" = nda_ts, "ETA" = eta_ts, NULL)
  if (is.null(df) || !nrow(df)) return(empty_ts_plot("Sin datos disponibles."))
  
  ticks_x <- year_ticks_2(df$ano)
  ylab <- if (which_one == "ETA") "Total enfermos" else "Casos"
  
  plotly::plot_ly(
    data = df, x = ~ano, y = ~valor,
    type = "scatter", mode = "lines+markers",
    line = list(width = 2, color = COL_PLOT),
    marker = list(size = 6, color = COL_PLOT),
    customdata = fmt_short(df$valor),
    hovertemplate = "<b>Año:</b> %{x}<br><b>Valor:</b> %{customdata}<extra></extra>"
  ) %>%
    plotly::layout(
      xaxis = list(title = "", tickmode = "array", tickvals = ticks_x, ticktext = ticks_x, showgrid = FALSE, automargin = TRUE),
      yaxis = list(title = ylab, showgrid = FALSE, automargin = TRUE),
      margin = list(l = 70, r = 20, t = 10, b = 60),
      hovermode = "x unified",
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# =========================================================
# 3) SISBEN — prevalencia i# usando agregados (Nw_hogares + i# ya sumados por grupo)
# =========================================================
sisben_i_labels <- c(
  i1  = "Bajo logro educativo",
  i2  = "Analfabetismo",
  i3  = "Inasistencia escolar",
  i4  = "Rezago escolar",
  i5  = "Barreras a cuidado primera infancia",
  i6  = "Trabajo infantil",
  i7  = "Desempleo de larga duración",
  i8  = "Trabajo informal",
  i9  = "Sin aseguramiento en salud",
  i10 = "Barreras acceso a salud",
  i11 = "Sin fuente de agua mejorada",
  i12 = "Eliminación inadecuada de excretas",
  i13 = "Pisos inadecuados",
  i14 = "Paredes exteriores inadecuados",
  i15 = "Hacinamiento crítico"
)

sisben_raw <- tryCatch({
  p <- file.path(data_dir, "031_DNP_SISBEN.rds")
  if (!file.exists(p)) return(NULL)
  df <- readRDS(p)
  if (is.null(df) || !nrow(df)) return(NULL)
  df
}, error = function(e) NULL)

prep_sisben_agg_preval_long <- function(df){
  if (is.null(df) || !nrow(df)) return(NULL)
  
  nms <- names(df)
  col_ano <- pick_year_col(nms)
  col_w   <- pick_col_simple(nms, c("Nw_hogares","nw_hogares","nw","peso","weight","ponderador"))
  col_dep_code <- pick_dep_col(nms)
  col_dep_name <- pick_dep_name_col(nms)
  col_grp <- pick_col_simple(nms, c("grupo","group","categoria","categoría","segmento","nivel","grupo_sisben"))
  
  if (is.na(col_ano) || is.na(col_w)) return(NULL)
  
  priv_cols <- intersect(nms, paste0("i", 1:15))
  if (!length(priv_cols)) return(NULL)
  
  base <- df %>%
    mutate(
      .ano = suppressWarnings(as.integer(parse_num_co(.data[[col_ano]]))),
      .w   = parse_num_co(.data[[col_w]]),
      .dep_code = if (!is.na(col_dep_code)) norm_dep2(.data[[col_dep_code]]) else NA_character_,
      .dep_name = if (!is.na(col_dep_name)) safe_chr(.data[[col_dep_name]]) else NA_character_,
      .grp      = if (!is.na(col_grp)) safe_chr(.data[[col_grp]]) else "TOTAL"
    ) %>%
    filter(!is.na(.ano), is.finite(.w), .w > 0)
  
  if (!nrow(base)) return(NULL)
  
  long <- base %>%
    select(.ano, .w, .dep_code, .dep_name, .grp, all_of(priv_cols)) %>%
    pivot_longer(cols = all_of(priv_cols), names_to = "metric", values_to = "i_val") %>%
    mutate(
      i_num = parse_num_co(i_val),
      i_cap = pmin(pmax(i_num, 0), .w)
    )
  
  long %>%
    group_by(
      ano = .ano,
      cod_dep = .dep_code,
      dep_nom = .dep_name,
      grupo   = .grp,
      metric
    ) %>%
    summarise(
      denom = sum(.w, na.rm = TRUE),
      numer = sum(i_cap, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      valor = if_else(denom > 0, 100 * numer / denom, NA_real_),
      DEPARTAMENTO_D = dep_nom
    ) %>%
    filter(is.finite(valor)) %>%
    arrange(metric, ano, cod_dep, dep_nom, grupo)
}

sisben_prev_long <- prep_sisben_agg_preval_long(sisben_raw)

sisben_metric_choices <- {
  if (is.null(sisben_prev_long) || !nrow(sisben_prev_long)) c("Sin datos Sisbén" = "i1")
  else {
    mets <- sort(unique(sisben_prev_long$metric))
    labs <- ifelse(mets %in% names(sisben_i_labels), sisben_i_labels[mets], mets)
    stats::setNames(mets, labs)
  }
}

sisben_group_choices <- {
  if (is.null(sisben_prev_long) || !nrow(sisben_prev_long) || !"grupo" %in% names(sisben_prev_long)) {
    c("Todos" = "Todos")
  } else {
    grs <- sort(unique(na.omit(as.character(sisben_prev_long$grupo))))
    c("Todos" = "Todos", stats::setNames(grs, grs))
  }
}

make_sisben_ts_plot <- function(metric_sel = "i1", grupo_sel = "Todos"){
  if (is.null(sisben_prev_long) || !nrow(sisben_prev_long)) return(empty_ts_plot("Sin datos Sisbén disponibles."))
  metric_sel <- metric_sel %||% "i1"
  grupo_sel  <- grupo_sel  %||% "Todos"
  
  dd <- sisben_prev_long %>% filter(metric == metric_sel)
  if ("grupo" %in% names(dd) && !is.null(grupo_sel) && grupo_sel != "Todos") dd <- dd %>% filter(grupo == grupo_sel)
  if (!nrow(dd)) return(empty_ts_plot("Sin datos para la privación (y grupo) seleccionados."))
  
  ts <- dd %>%
    group_by(ano) %>%
    summarise(
      denom = sum(denom, na.rm = TRUE),
      numer = sum(numer, na.rm = TRUE),
      valor = if_else(denom > 0, 100 * numer / denom, NA_real_),
      .groups = "drop"
    ) %>%
    arrange(ano)
  
  ticks_x <- year_ticks_2(ts$ano)
  ticks_y <- seq(0, 100, by = 5)
  
  nm <- if (metric_sel %in% names(sisben_i_labels)) sisben_i_labels[[metric_sel]] else metric_sel
  
  plotly::plot_ly(
    data = ts, x = ~ano, y = ~valor,
    type = "scatter", mode = "lines+markers",
    line = list(width = 2, color = COL_PLOT),
    marker = list(size = 6, color = COL_PLOT),
    customdata = fmt_pct(ts$valor, 1),
    hovertemplate = "<b>Año:</b> %{x}<br><b>Prevalencia:</b> %{customdata}<extra></extra>"
  ) %>%
    plotly::layout(
      xaxis = list(title = "", tickmode = "array", tickvals = ticks_x, ticktext = ticks_x, showgrid = FALSE, automargin = TRUE),
      yaxis = list(title = paste0(nm, " (%)"), tickvals = ticks_y, ticktext = fmt_pct(ticks_y, 1), showgrid = FALSE, automargin = TRUE, rangemode = "tozero"),
      margin = list(l = 70, r = 20, t = 10, b = 60),
      hovermode = "x unified",
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# =========================================================
# 4) ECV — FIES-8 (prevalencia ponderada) — SOLO indicador
# =========================================================
mk_event_fies <- function(v){
  vv <- suppressWarnings(as.integer(parse_num_co(v)))
  ifelse(vv %in% c(1,2), as.numeric(vv == 1L), NA_real_)
}

col_or_na_int <- function(df, candidates){
  n  <- nrow(df)
  nm <- tolower(names(df))
  cand <- tolower(candidates)
  for (c in cand){
    hit <- which(nm == c)
    if (length(hit)) return(suppressWarnings(as.integer(parse_num_co(df[[hit[1]]]))))
  }
  nm_norm   <- gsub("_|\\s", "", nm)
  cand_norm <- unique(gsub("_|\\s", "", cand))
  for (c in cand_norm){
    hit <- which(nm_norm == c)
    if (length(hit)) return(suppressWarnings(as.integer(parse_num_co(df[[hit[1]]]))))
  }
  rep(NA_integer_, n)
}

inds_fies <- c(
  "Se preocupó por no tener suficientes alimentos" = "p_suficiente_a",
  "No pudo comer alimentos saludables y nutritivos" = "np_comer_a_saludables",
  "Consumió poca variedad de alimentos"             = "c_poca_variedad",
  "Saltó comidas (desayuno/almuerzo/cena)"         = "salto_comidas",
  "Comió menos de lo que pensaba debía comer"      = "comio_menos_delopensado",
  "El hogar se quedó sin alimentos"                = "hogar_sin_alimentos",
  "Tuvo hambre pero no comió"                      = "hambre_pero_sin_comida",
  "Un día entero sin comer"                        = "no_comer_dia_entero"
)

ecv_raw <- tryCatch({
  p <- file.path(data_dir, "052_DANE_ECV.rds")
  if (!file.exists(p)) return(NULL)
  readRDS(p)
}, error = function(e) NULL)

ecv_fies_agg_dep <- NULL
ecv_fies_agg_nat <- NULL

if (!is.null(ecv_raw) && nrow(ecv_raw)) {
  
  names(ecv_raw) <- tolower(names(ecv_raw))
  
  pick1 <- function(nms, prefer, pattern) {
    if (!is.na(prefer) && prefer %in% nms) prefer else nms[grepl(pattern, nms, ignore.case = TRUE)][1]
  }
  
  col_ano     <- pick1(names(ecv_raw), "anio", "^a(n|ñ)o$|year|anio")
  col_dep_cod <- pick1(names(ecv_raw), "cod_dane_dpto_d", "cod.*dane.*dpto|dpto.*(ccdgo|cod)|cod.*dep")
  col_dep_nom <- pick1(names(ecv_raw), "departamento_d", "^departa|depto")
  col_w       <- pick1(names(ecv_raw), "fex_c.x", "^fex|factor|pondera|expans")
  
  p_suficiente_a          <- col_or_na_int(ecv_raw, c("p3516s1","p3516_s1","p_suficiente_a"))
  np_comer_a_saludables   <- col_or_na_int(ecv_raw, c("p3516s2","p3516_s2","np_comer_a_saludables"))
  c_poca_variedad         <- col_or_na_int(ecv_raw, c("p3516s3","p3516_s3","c_poca_variedad"))
  salto_comidas           <- col_or_na_int(ecv_raw, c("p3516s4","p3516_s4","salto_comidas"))
  comio_menos_delopensado <- col_or_na_int(ecv_raw, c("p3516s5","p3516_s5","comio_menos_delopensado"))
  hogar_sin_alimentos     <- col_or_na_int(ecv_raw, c("p3516s6","p3516_s6","hogar_sin_alimentos"))
  hambre_pero_sin_comida  <- col_or_na_int(ecv_raw, c("p3516s7","p3516_s7","hambre_pero_sin_comida"))
  no_comer_dia_entero     <- col_or_na_int(ecv_raw, c("p3516s8","p3516_s8","no_comer_dia_entero"))
  
  ecv <- tibble::tibble(
    anio           = suppressWarnings(as.integer(parse_num_co(ecv_raw[[col_ano]]))),
    COD_DANE_DPTO2 = norm_dep2(ecv_raw[[col_dep_cod]]),
    DEPARTAMENTO   = as.character(ecv_raw[[col_dep_nom]]),
    fexp           = suppressWarnings(as.numeric(parse_num_co(ecv_raw[[col_w]]))),
    p_suficiente_a          = p_suficiente_a,
    np_comer_a_saludables   = np_comer_a_saludables,
    c_poca_variedad         = c_poca_variedad,
    salto_comidas           = salto_comidas,
    comio_menos_delopensado = comio_menos_delopensado,
    hogar_sin_alimentos     = hogar_sin_alimentos,
    hambre_pero_sin_comida  = hambre_pero_sin_comida,
    no_comer_dia_entero     = no_comer_dia_entero
  ) %>%
    dplyr::filter(!is.na(anio), is.finite(fexp), fexp > 0, !is.na(COD_DANE_DPTO2), nzchar(COD_DANE_DPTO2))
  
  fies_vars <- unname(inds_fies)
  
  ecv_long <- ecv %>%
    tidyr::pivot_longer(cols = all_of(fies_vars), names_to = "metric", values_to = "raw_v") %>%
    mutate(evento = mk_event_fies(raw_v)) %>%
    select(anio, COD_DANE_DPTO2, DEPARTAMENTO, fexp, metric, evento)
  
  ecv_fies_agg_dep <- ecv_long %>%
    group_by(
      ano     = anio,
      cod_dep = COD_DANE_DPTO2,
      dep_nom = DEPARTAMENTO,
      metric
    ) %>%
    summarise(
      numer = sum(fexp * if_else(is.na(evento), 0, evento), na.rm = TRUE),
      denom = sum(fexp[!is.na(evento)], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      valor = if_else(denom > 0, 100 * numer / denom, NA_real_),
      DEPARTAMENTO_D = dep_nom
    )
  
  ecv_fies_agg_nat <- ecv_long %>%
    group_by(ano = anio, metric) %>%
    summarise(
      numer = sum(fexp * if_else(is.na(evento), 0, evento), na.rm = TRUE),
      denom = sum(fexp[!is.na(evento)], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(valor = if_else(denom > 0, 100 * numer / denom, NA_real_))
}

make_ecv_fies_ts_plot <- function(metric_sel){
  if (is.null(ecv_fies_agg_nat) || !nrow(ecv_fies_agg_nat))
    return(empty_ts_plot("Sin datos ECV (FIES) disponibles."))
  
  metric_sel <- metric_sel %||% unname(inds_fies)[1]
  
  ts <- ecv_fies_agg_nat %>%
    filter(metric == metric_sel) %>%
    arrange(ano)
  
  if (!nrow(ts))
    return(empty_ts_plot("Sin datos para el indicador FIES seleccionado."))
  
  ticks_x <- year_ticks_2(ts$ano)
  ticks_y <- seq(0, 100, by = 5)
  
  plotly::plot_ly(
    data = ts, x = ~ano, y = ~valor,
    type = "scatter", mode = "lines+markers",
    line = list(width = 2, color = COL_PLOT),
    marker = list(size = 6, color = COL_PLOT),
    customdata = fmt_pct(ts$valor, 1),
    hovertemplate = "<b>Año:</b> %{x}<br><b>Prevalencia:</b> %{customdata}<extra></extra>"
  ) %>%
    plotly::layout(
      xaxis = list(title = "", tickmode = "array", tickvals = ticks_x, ticktext = ticks_x, showgrid = FALSE, automargin = TRUE),
      yaxis = list(title = "Prevalencia (%)", tickvals = ticks_y, ticktext = fmt_pct(ticks_y, 1), showgrid = FALSE, automargin = TRUE, rangemode = "tozero"),
      margin = list(l = 70, r = 20, t = 10, b = 60),
      hovermode = "x unified",
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# =========================================================
# 5) NOAA — Precipitación (mm³) — anual nacional + dep
# =========================================================
noaa_raw <- tryCatch({
  p <- file.path(data_dir, "131_NOAA_Precipitación.rds")
  if (!file.exists(p)) return(NULL)
  df <- readRDS(p)
  if (is.null(df) || !nrow(df)) return(NULL)
  df
}, error = function(e) NULL)

noaa_ts <- NULL
noaa_dep_all <- NULL

if (!is.null(noaa_raw)) {
  names(noaa_raw) <- tolower(names(noaa_raw))
  
  find_col <- function(patterns) {
    for (pat in patterns) {
      col <- names(noaa_raw)[grepl(pat, names(noaa_raw), ignore.case = TRUE)]
      if (length(col) > 0) return(col[1])
    }
    return(NA_character_)
  }
  
  col_ano <- find_col(c("^ano$","^anio$","year"))
  col_precip <- find_col(c("precip_mm","precipitacion","precip","valor"))
  col_dep_code <- find_col(c("cod_dane_dpto","cod_dane_dpto_d","dpto_cod","cod_dpto","cod_depto","cod_dpto2","cod_dane_dpto2"))
  col_dep_name <- find_col(c("departamento","departamento_d","nom_dpto","departamen"))
  
  if (!is.na(col_ano) && !is.na(col_precip)) {
    df <- noaa_raw %>%
      mutate(
        ano = as.integer(.[[col_ano]]),
        valor = parse_num_co(.[[col_precip]]),
        cod_dep = if (!is.na(col_dep_code)) norm_dep2(.[[col_dep_code]]) else NA_character_,
        dep_nom = if (!is.na(col_dep_name)) safe_chr(.[[col_dep_name]]) else NA_character_
      ) %>%
      filter(!is.na(ano), !is.na(valor), valor >= 0) %>%
      mutate(
        dep_nom = stringi::stri_trans_general(dep_nom, "Latin-ASCII"),
        dep_nom = stringi::stri_trim_both(toupper(dep_nom))
      )
    
    noaa_dep_all <- df %>%
      filter(!is.na(cod_dep), nzchar(cod_dep)) %>%
      group_by(ano, cod_dep, dep_nom) %>%
      summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
      mutate(DEPARTAMENTO_D = dep_nom)
    
    noaa_ts <- noaa_dep_all %>%
      group_by(ano) %>%
      summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
      arrange(ano)
  }
}

make_noaa_ts_plot <- function(){
  if (is.null(noaa_ts) || !nrow(noaa_ts))
    return(empty_ts_plot("Sin datos NOAA disponibles."))
  
  df <- noaa_ts
  ticks_x <- year_ticks_5(df$ano)
  
  plotly::plot_ly(
    data = df, x = ~ano, y = ~valor,
    type = "scatter", mode = "lines+markers",
    line = list(width = 2, color = COL_PLOT),
    marker = list(size = 6, color = COL_PLOT),
    customdata = fmt_short(df$valor),
    hovertemplate = "<b>Año:</b> %{x}<br><b>Precipitación:</b> %{customdata} mm³<extra></extra>"
  ) %>%
    plotly::layout(
      xaxis = list(title = "", tickmode = "array", tickvals = ticks_x, ticktext = ticks_x, showgrid = FALSE, automargin = TRUE),
      yaxis = list(title = "Precipitación (mm³)", showgrid = FALSE, automargin = TRUE, rangemode = "tozero"),
      margin = list(l = 70, r = 20, t = 10, b = 60),
      hovermode = "x unified",
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# =========================================================
# 6) HANSEN — Deforestación (ha) — TS + dep
# =========================================================
hansen_raw <- tryCatch({
  p <- file.path(data_dir, "141_HANSEN_DEFORESTATION.rds")
  if (!file.exists(p)) return(NULL)
  df <- readRDS(p)
  if (is.null(df) || !nrow(df)) return(NULL)
  df
}, error = function(e) NULL)

hansen_ts <- NULL
hansen_dep_all <- NULL

if (!is.null(hansen_raw)) {
  needed <- c("ano", "COD_DANE_DPTO_D", "DEPARTAMENTO_D", "has")
  miss <- setdiff(needed, names(hansen_raw))
  if (length(miss) == 0) {
    df <- hansen_raw %>%
      mutate(
        ano     = as.integer(parse_num_co(ano)),
        valor   = parse_num_co(has),
        cod_dep = norm_dep2(COD_DANE_DPTO_D),
        dep_nom = safe_chr(DEPARTAMENTO_D)
      ) %>%
      filter(!is.na(ano), is.finite(valor), valor >= 0, !is.na(cod_dep), nzchar(cod_dep)) %>%
      mutate(
        dep_nom = stringi::stri_trans_general(dep_nom, "Latin-ASCII"),
        dep_nom = stringi::stri_trim_both(toupper(dep_nom))
      )
    
    hansen_dep_all <- df %>%
      group_by(ano, cod_dep, dep_nom) %>%
      summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
      mutate(DEPARTAMENTO_D = dep_nom)
    
    hansen_ts <- hansen_dep_all %>%
      group_by(ano) %>%
      summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
      arrange(ano)
  }
}

make_hansen_ts_plot <- function(){
  if (is.null(hansen_ts) || !nrow(hansen_ts))
    return(empty_ts_plot("Sin datos Hansen disponibles."))
  
  df <- hansen_ts
  ticks_x <- year_ticks_5(df$ano)
  
  plotly::plot_ly(
    data = df, x = ~ano, y = ~valor,
    type = "scatter", mode = "lines+markers",
    line = list(width = 2, color = COL_PLOT),
    marker = list(size = 6, color = COL_PLOT),
    customdata = fmt_short(df$valor),
    hovertemplate = "<b>Año:</b> %{x}<br><b>Deforestación:</b> %{customdata} ha<extra></extra>"
  ) %>%
    plotly::layout(
      xaxis = list(title = "", tickmode = "array", tickvals = ticks_x, ticktext = ticks_x, showgrid = FALSE, automargin = TRUE),
      yaxis = list(title = "Deforestación (ha)", showgrid = FALSE, automargin = TRUE, rangemode = "tozero"),
      margin = list(l = 70, r = 20, t = 10, b = 60),
      hovermode = "x unified",
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# =========================================================
# 7) FINAGRO — 081_FINAGRO_CFA (según variables de la imagen)
#    columnas esperadas: COD_DANE_DPTO_D, DEPARTAMENTO_D, TIPO_PRODUCTOR,
#    SEXO, LINEA_CREDITO, ESLABON_CADENA, ano, mes, VALOR_CREDITO, NUMERO_CREDITO
# =========================================================
finagro_raw <- tryCatch({
  p <- file.path(data_dir, "081_FINAGRO_CFA.rds")
  if (!file.exists(p)) return(NULL)
  df <- readRDS(p)
  if (is.null(df) || !nrow(df)) return(NULL)
  df
}, error = function(e) NULL)

finagro_df <- NULL

ipp_tbl <- data.frame(
  ano = c(2010:2025),
  IPP = c(
    0.8851, 0.9633, 0.9625, 0.9502, 0.9778, 1.0158, 1.0707, 1.0801,
    1.1356, 1.1848, 1.1758, 1.3756, 1.7822, 1.7923, 1.7989, 1.8585
  )
)

if (!is.null(finagro_raw)) {
  nms <- names(finagro_raw)
  
  col_ano   <- pick_year_col(nms)  # "ano"
  col_mes   <- pick_col_simple(nms, c("mes","MES","month"))
  col_dep   <- pick_col_simple(nms, c("COD_DANE_DPTO_D","cod_dane_dpto_d","COD_DPTO2","cod_dpto2","cod_dpto","cod_depto"))
  col_depnm <- pick_col_simple(nms, c("DEPARTAMENTO_D","departamento_d","DEPARTAMENTO","departamento","NOM_DPTO","nom_dpto"))
  
  col_val   <- pick_col_simple(nms, c("VALOR_CREDITO","valor_credito","VALOR_CREDITO_REAL","valor_credito_real"))
  col_num   <- pick_col_simple(nms, c("NUMERO_CREDITO","numero_credito","N_CREDITOS","creditos","CREDITOS"))
  
  # Variables según la imagen
  col_tipo  <- pick_col_simple(nms, c("TIPO_PRODUCTOR","tipo_productor","TIPO_PERSONA","tipo_persona"))
  col_sexo  <- pick_col_simple(nms, c("SEXO","sexo","SEXO2","sexo2"))
  col_lin   <- pick_col_simple(nms, c("LINEA_CREDITO","linea_credito","LINEA","linea"))
  col_esl   <- pick_col_simple(nms, c("ESLABON_CADENA","eslabon_cadena","ESLABON","eslabon"))
  
  if (!is.na(col_ano) && !is.na(col_val) && !is.na(col_num)) {
    df <- finagro_raw %>%
      mutate(
        ano = as.integer(parse_num_co(.data[[col_ano]])),
        mes = if (!is.na(col_mes)) as.integer(parse_num_co(.data[[col_mes]])) else NA_integer_,
        VALOR_CREDITO  = parse_num_co(.data[[col_val]]),
        NUMERO_CREDITO = parse_num_co(.data[[col_num]]),
        COD_DPTO2      = if (!is.na(col_dep)) norm_dep2(.data[[col_dep]]) else NA_character_,
        DEPARTAMENTO   = if (!is.na(col_depnm)) safe_chr(.data[[col_depnm]]) else NA_character_,
        TIPO_PERSONA   = if (!is.na(col_tipo)) safe_chr(.data[[col_tipo]]) else NA_character_,
        SEXO2          = if (!is.na(col_sexo)) safe_chr(.data[[col_sexo]]) else NA_character_,
        LINEA_CREDITO  = if (!is.na(col_lin))  safe_chr(.data[[col_lin]])  else NA_character_,
        ESLABON_CADENA = if (!is.na(col_esl))  safe_chr(.data[[col_esl]])  else NA_character_
      ) %>%
      filter(!is.na(ano), is.finite(VALOR_CREDITO), is.finite(NUMERO_CREDITO))
    
    df <- df %>%
      left_join(ipp_tbl, by = "ano") %>%
      mutate(
        IPP = ifelse(is.na(IPP), 1, IPP),
        VALOR_CREDITO_REAL = VALOR_CREDITO / IPP
      )
    
    df <- df %>%
      mutate(
        DEPARTAMENTO = stringi::stri_trans_general(DEPARTAMENTO, "Latin-ASCII"),
        DEPARTAMENTO = stringi::stri_trim_both(toupper(DEPARTAMENTO)),
        DEPARTAMENTO_D = DEPARTAMENTO
      )
    
    finagro_df <- df
  }
}

finagro_choices <- list(
  tipo    = c("Todos"),
  sexo    = c("Todos"),
  linea   = c("Todos"),
  eslabon = c("Todos")
)
if (!is.null(finagro_df) && nrow(finagro_df)) {
  finagro_choices$tipo    <- c("Todos", sort(unique(na.omit(finagro_df$TIPO_PERSONA))))
  finagro_choices$sexo    <- c("Todos", sort(unique(na.omit(finagro_df$SEXO2))))
  finagro_choices$linea   <- c("Todos", sort(unique(na.omit(finagro_df$LINEA_CREDITO))))
  finagro_choices$eslabon <- c("Todos", sort(unique(na.omit(finagro_df$ESLABON_CADENA))))
}

finagro_apply_filters <- function(df, tipo, sexo, linea, eslabon){
  if (is.null(df) || !nrow(df)) return(df)
  if (!is.null(tipo)   && tipo   != "Todos") df <- df %>% filter(TIPO_PERSONA == tipo)
  if (!is.null(sexo)   && sexo   != "Todos") df <- df %>% filter(SEXO2 == sexo)
  if (!is.null(linea)  && linea  != "Todos") df <- df %>% filter(LINEA_CREDITO == linea)
  if (!is.null(eslabon)&& eslabon!= "Todos") df <- df %>% filter(ESLABON_CADENA == eslabon)
  df
}

make_finagro_dual_ts_plot <- function(df){
  if (is.null(df) || !nrow(df)) return(empty_ts_plot("Sin datos FINAGRO para los filtros seleccionados."))
  
  ts <- df %>%
    group_by(ano) %>%
    summarise(
      monto_real = sum(VALOR_CREDITO_REAL, na.rm = TRUE) / 1e9,
      creditos   = sum(NUMERO_CREDITO, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(ano)
  
  if (!nrow(ts)) return(empty_ts_plot("Sin serie temporal FINAGRO."))
  
  ticks_x <- year_ticks_2(ts$ano)
  
  plot_ly(ts, x = ~ano) %>%
    add_trace(
      y = ~monto_real,
      type = "scatter", mode = "lines+markers",
      name = "Monto real (Mil M)",
      yaxis = "y1",
      line = list(width = 2, color = COL_PLOT),
      marker = list(size = 6, color = COL_PLOT),
      hovertemplate = "<b>Año:</b> %{x}<br><b>Monto real:</b> %{y:.2f} Mil M<extra></extra>"
    ) %>%
    add_trace(
      y = ~creditos,
      type = "scatter", mode = "lines+markers",
      name = "Número de créditos",
      yaxis = "y2",
      line = list(width = 2, dash = "dot", color = COL_PLOT),
      marker = list(size = 6, color = COL_PLOT),
      hovertemplate = "<b>Año:</b> %{x}<br><b> Número de operaciones:</b> %{y:,}<extra></extra>"
    ) %>%
    layout(
      xaxis = list(title = "", tickmode = "array", tickvals = ticks_x, ticktext = ticks_x, showgrid = FALSE, automargin = TRUE),
      yaxis = list(title = "Monto real (Miles de millones)", showgrid = FALSE, automargin = TRUE, rangemode = "tozero"),
      yaxis2 = list(title = "Número de operaciones ", overlaying = "y", side = "right", showgrid = FALSE, automargin = TRUE, rangemode = "tozero"),
      legend = list(orientation = "h", x = 0, y = -0.22, xanchor = "left", yanchor = "top"),
      margin = list(l = 70, r = 70, t = 10, b = 95),
      hovermode = "x unified",
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# =========================================================
# Datos departamentales precomputados (para mapas)
# =========================================================
get_dep_data <- function(ind_code){
  switch(ind_code,
         "DEP_RATIO" = pob_dep_all,
         "ECV_FIES"  = ecv_fies_agg_dep,
         "BPAN"      = bpan_dep_all,
         "ETA"       = eta_dep_all,
         "NDA"       = nda_dep_all,
         "SISBEN"    = sisben_prev_long,
         "PRECIP"    = noaa_dep_all,
         "HANSEN"    = hansen_dep_all,
         "FINAGRO"   = NULL,
         NULL)
}

all_years_dep <- sort(unique(c(
  if (!is.null(pob_dep_all))      pob_dep_all$ano,
  if (!is.null(ecv_fies_agg_dep)) ecv_fies_agg_dep$ano,
  if (!is.null(bpan_dep_all))     bpan_dep_all$ano,
  if (!is.null(eta_dep_all))      eta_dep_all$ano,
  if (!is.null(nda_dep_all))      nda_dep_all$ano,
  if (!is.null(sisben_prev_long)) sisben_prev_long$ano,
  if (!is.null(noaa_dep_all))     noaa_dep_all$ano,
  if (!is.null(hansen_dep_all))   hansen_dep_all$ano,
  if (!is.null(finagro_df))       finagro_df$ano
)), na.last = NA)

default_fixed_year <- if (length(all_years_dep)) max(all_years_dep, na.rm = TRUE) else NA_integer_

# =========================================================
# Tarjetas
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
    .filters-grid-3{ display:grid; grid-template-columns:1fr 1fr 1fr; gap:10px; align-items:end; }
    .filters-grid-4{ display:grid; grid-template-columns:1fr 1fr; gap:10px; align-items:end; }
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
    .map-panel{ display:flex; flex-direction:column; gap:10px; background:#fff; border:1px solid var(--ecv-bdr); border-radius:16px; padding:16px 16px 18px; box-shadow:0 2px 6px rgba(0,0,0,.03); }
    .map-panel .filters{ border:none; box-shadow:none; padding:0; margin:0 0 8px 0; }

    .filters-map{ height:auto; display:flex; flex-direction:column; justify-content:flex-start; overflow:visible; }

    @media (max-width:980px){
      .filters-grid-2{ grid-template-columns:1fr; }
      .filters-grid-3{ grid-template-columns:1fr; }
      .grid-2maps{ grid-template-columns:1fr; }
      .filters-map{ height:auto; overflow:visible; }
      .ts-grid{ grid-template-columns:1fr; }
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
              div(lbl("¿Cuál información de la dimensión de vulnerabilidad quiere analizar?"),
                  selector_base("estr_base1", "Estructura")),
              uiOutput("estr_extra1")
            )
          ),
          div(
            class = "ts-grid",
            div(class = "ts-left", plotlyOutput("estr_ts", height = paste0(H_PLOT, "px"))),
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
                  div(lbl("¿Qué base de la dimensión de vulnerabilidad analizamos?"), selector_base("estr_base2_map1", "Estructura")),
                  uiOutput("map1_extra"),
                  div(class = "filters-grid-2", div(lbl("¿Qué año analizamos?"), uiOutput("map1_year_ui")))
                )
              ),
              leafletOutput("map_1", height = paste0(H_MAP, "px"))
            ),
            
            div(
              class = "map-panel",
              div(class = "card-title-small", "INDICADOR 2"),
              div(
                class = "filters filters-map",
                div(
                  class = "filters-grid-1",
                  div(lbl("¿Qué base de la dimensión de vulnerabilidad analizamos?"), selector_base("estr_base2_map2", "Estructura")),
                  uiOutput("map2_extra"),
                  div(class = "filters-grid-2", div(lbl("¿Qué año analizamos?"), uiOutput("map2_year_ui")))
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
  
  # ----- Extra UI Pestaña 1 -----
  output$estr_extra1 <- renderUI({
    base_sel <- input$estr_base1
    if (is.null(base_sel)) return(NULL)
    
    if (base_sel == "DNP_SISBEN") {
      tagList(
        div(lbl("Tipología de privación"),
            selectInput("estr_sisben_metric", label = NULL,
                        choices = sisben_metric_choices,
                        selected = unname(sisben_metric_choices)[1] %||% "i1")),
        div(lbl("Grupo (A/B/C/D)"),
            selectInput("estr_sisben_grupo", label = NULL,
                        choices = sisben_group_choices,
                        selected = "Todos"))
      )
    } else if (base_sel == "DANE_ECV") {
      tagList(
        div(lbl("Indicador FIES-8"),
            selectInput("estr_ecv_fies_metric", label = NULL,
                        choices = inds_fies,
                        selected = unname(inds_fies)[1]))
      )
    } else if (base_sel == "FINAGRO_CFA_FAST") {
      # (CAMBIO FINAGRO) agrega ESLABON_CADENA
      tagList(
        div(lbl("Tipo de productor"),
            selectInput("estr_fin_tipo", NULL, choices = finagro_choices$tipo, selected = "Todos")),
        div(lbl("Sexo"),
            selectInput("estr_fin_sexo", NULL, choices = finagro_choices$sexo, selected = "Todos")),
        div(lbl("Línea de crédito"),
            selectInput("estr_fin_linea", NULL, choices = finagro_choices$linea, selected = "Todos")),
        div(lbl("Eslabón de la cadena"),
            selectInput("estr_fin_eslabon", NULL, choices = finagro_choices$eslabon, selected = "Todos"))
      )
    } else {
      NULL
    }
  })
  
  # ----- Plot principal (serie temporal) -----
  output$estr_ts <- renderPlotly({
    base_sel <- input$estr_base1 %||% default_base_estructura
    
    p <- if (base_sel == "DANE_POPULATION") {
      make_dep_ratio_ts_plot()
    } else if (base_sel == "DANE_ECV") {
      make_ecv_fies_ts_plot(metric_sel = input$estr_ecv_fies_metric %||% unname(inds_fies)[1])
    } else if (base_sel == "INS_SIVIGILA_BPAN") {
      make_sivigila_ts_plot("BPAN")
    } else if (base_sel == "INS_SIVIGILA_ETA") {
      make_sivigila_ts_plot("ETA")
    } else if (base_sel == "INS_SIVIGILA_NDA") {
      make_sivigila_ts_plot("NDA")
    } else if (base_sel == "DNP_SISBEN") {
      make_sisben_ts_plot(
        metric_sel = input$estr_sisben_metric %||% "i1",
        grupo_sel  = input$estr_sisben_grupo %||% "Todos"
      )
    } else if (base_sel == "NOAA_PRECIPITACION") {
      make_noaa_ts_plot()
    } else if (base_sel == "HANSEN_DEFORESTATION") {
      make_hansen_ts_plot()
    } else if (base_sel == "FINAGRO_CFA_FAST") {
      df <- finagro_apply_filters(
        finagro_df,
        tipo    = input$estr_fin_tipo %||% "Todos",
        sexo    = input$estr_fin_sexo %||% "Todos",
        linea   = input$estr_fin_linea %||% "Todos",
        eslabon = input$estr_fin_eslabon %||% "Todos"
      )
      make_finagro_dual_ts_plot(df)
    } else {
      empty_ts_plot("Seleccione una base.")
    }
    
    p
  })
  
  # =========================================================
  # Tarjetas (calcular para TODAS las bases)
  # =========================================================
  summary_from_ts <- function(ts, year_col = "ano", value_col = "valor",
                              titulo = "Indicador",
                              value_type = c("percent","number"),
                              unit = ""){
    value_type <- match.arg(value_type)
    
    out <- list(
      story = "No se dispone de información suficiente para construir la síntesis con los filtros actuales.",
      last_rows = list(list(k="Año", v=na_txt()), list(k="Valor", v=na_txt())),
      growth_rows = list(list(k="Año anterior", v=na_txt()), list(k="Variación", v=na_txt()))
    )
    
    if (is.null(ts) || !nrow(ts) || !(year_col %in% names(ts)) || !(value_col %in% names(ts))) return(out)
    
    ts2 <- ts %>%
      dplyr::mutate(
        .y = suppressWarnings(as.integer(.data[[year_col]])),
        .v = suppressWarnings(as.numeric(.data[[value_col]]))
      ) %>%
      dplyr::filter(is.finite(.y), is.finite(.v)) %>%
      dplyr::arrange(.y)
    
    if (!nrow(ts2)) return(out)
    
    last_year <- max(ts2$.y, na.rm = TRUE)
    last_val  <- ts2 %>% dplyr::filter(.y == last_year) %>% dplyr::slice(1) %>% dplyr::pull(.v)
    
    prev_val <- ts2 %>% dplyr::filter(.y == (last_year - 1)) %>% dplyr::slice(1) %>% dplyr::pull(.v)
    if (!length(prev_val)) prev_val <- NA_real_
    
    g <- safe_growth(last_val, prev_val)
    
    fmt_val <- function(v){
      if (!is.finite(v)) return(na_txt())
      if (value_type == "percent") return(fmt_pct(v, 1))
      paste0(fmt_short(v), unit)
    }
    
    out$story <- sprintf(
      "%s: en %d el valor fue %s. Variación interanual (%d vs %d): %s.",
      titulo,
      last_year,
      as.character(fmt_val(last_val)),
      last_year,
      last_year - 1,
      ifelse(is.na(g), "NA", fmt_pct(100*g, 1))
    )
    
    out$last_rows <- list(
      list(k="Año", v=as.character(last_year)),
      list(k="Valor", v=as.character(fmt_val(last_val)))
    )
    
    out$growth_rows <- list(
      list(k="Año anterior", v=if (is.finite(prev_val)) as.character(last_year - 1) else na_txt()),
      list(k="Variación", v=ifelse(is.na(g), na_txt(), fmt_pct(100*g, 1)))
    )
    
    out
  }
  
  current_summary <- reactive({
    base_sel <- input$estr_base1 %||% default_base_estructura
    
    # FINAGRO (dual: monto + créditos)
    if (base_sel == "FINAGRO_CFA_FAST") {
      df <- finagro_apply_filters(
        finagro_df,
        tipo    = input$estr_fin_tipo %||% "Todos",
        sexo    = input$estr_fin_sexo %||% "Todos",
        linea   = input$estr_fin_linea %||% "Todos",
        eslabon = input$estr_fin_eslabon %||% "Todos"
      )
      if (is.null(df) || !nrow(df)) {
        return(list(
          story = "Sin datos FINAGRO para los filtros actuales.",
          last_rows = list(list(k="Año", v=na_txt()), list(k="Monto real", v=na_txt()), list(k="Créditos", v=na_txt())),
          growth_rows = list(list(k="Año anterior", v=na_txt()), list(k="Δ Monto", v=na_txt()), list(k="Δ Créditos", v=na_txt()))
        ))
      }
      
      ts <- df %>%
        group_by(ano) %>%
        summarise(
          monto_real = sum(VALOR_CREDITO_REAL, na.rm = TRUE) / 1e9,
          creditos   = sum(NUMERO_CREDITO, na.rm = TRUE),
          .groups = "drop"
        ) %>% arrange(ano)
      
      last_year <- max(ts$ano, na.rm = TRUE)
      last <- ts %>% filter(ano == last_year) %>% slice(1)
      prev <- ts %>% filter(ano == (last_year - 1)) %>% slice(1)
      
      m_last <- last$monto_real
      c_last <- last$creditos
      m_prev <- if (nrow(prev)) prev$monto_real else NA_real_
      c_prev <- if (nrow(prev)) prev$creditos   else NA_real_
      gm <- safe_growth(m_last, m_prev)
      gc <- safe_growth(c_last, c_prev)
      
      return(list(
        story = sprintf(
          "FINAGRO (crédito agropecuario): en %d el monto real fue %s Mil M y se registraron %s créditos. Variación interanual: monto %s y créditos %s.",
          last_year,
          fmt_num_es(m_last, 2),
          fmt_num_es(c_last, 0),
          ifelse(is.na(gm), "NA", fmt_pct(100*gm, 1)),
          ifelse(is.na(gc), "NA", fmt_pct(100*gc, 1))
        ),
        last_rows = list(
          list(k="Año", v=as.character(last_year)),
          list(k="Monto real", v=paste0(fmt_num_es(m_last, 2), " Mil M")),
          list(k="Créditos", v=fmt_num_es(c_last, 0))
        ),
        growth_rows = list(
          list(k="Año anterior", v=if (nrow(prev)) as.character(last_year-1) else na_txt()),
          list(k="Δ Monto", v=ifelse(is.na(gm), na_txt(), fmt_pct(100*gm, 1))),
          list(k="Δ Créditos", v=ifelse(is.na(gc), na_txt(), fmt_pct(100*gc, 1)))
        )
      ))
    }
    
    # DANE POPULATION
    if (base_sel == "DANE_POPULATION") {
      ts <- pob_ts %>% mutate(valor = razon_dependencia_pct)
      return(summary_from_ts(ts, "ano", "valor",
                             titulo = "Razón de dependencia",
                             value_type = "percent"))
    }
    
    # ECV (FIES)
    if (base_sel == "DANE_ECV") {
      met <- input$estr_ecv_fies_metric %||% unname(inds_fies)[1]
      ts <- ecv_fies_agg_nat %>% filter(metric == met) %>% arrange(ano)
      titulo <- names(inds_fies)[match(met, unname(inds_fies))] %||% "FIES-8"
      return(summary_from_ts(ts, "ano", "valor",
                             titulo = paste0("ECV — ", titulo),
                             value_type = "percent"))
    }
    
    # SIVIGILA
    if (base_sel == "INS_SIVIGILA_BPAN")
      return(summary_from_ts(bpan_ts, "ano", "valor", "SIVIGILA — Bajo peso al nacer (registros)", "number"))
    if (base_sel == "INS_SIVIGILA_NDA")
      return(summary_from_ts(nda_ts,  "ano", "valor", "SIVIGILA — Desnutrición aguda infantil (registros)", "number"))
    if (base_sel == "INS_SIVIGILA_ETA")
      return(summary_from_ts(eta_ts,  "ano", "valor", "SIVIGILA — ETA (total enfermos)", "number"))
    
    # SISBEN (según privación + grupo)
    if (base_sel == "DNP_SISBEN") {
      met <- input$estr_sisben_metric %||% "i1"
      grp <- input$estr_sisben_grupo %||% "Todos"
      nm  <- if (met %in% names(sisben_i_labels)) sisben_i_labels[[met]] else met
      
      dd <- sisben_prev_long %>% filter(metric == met)
      if ("grupo" %in% names(dd) && grp != "Todos") dd <- dd %>% filter(grupo == grp)
      
      ts <- dd %>%
        group_by(ano) %>%
        summarise(
          denom = sum(denom, na.rm = TRUE),
          numer = sum(numer, na.rm = TRUE),
          valor = if_else(denom > 0, 100 * numer / denom, NA_real_),
          .groups = "drop"
        ) %>% arrange(ano)
      
      return(summary_from_ts(ts, "ano", "valor",
                             titulo = paste0("Sisbén — ", nm, " (prevalencia)"),
                             value_type = "percent"))
    }
    
    # NOAA
    if (base_sel == "NOAA_PRECIPITACION") {
      return(summary_from_ts(noaa_ts, "ano", "valor",
                             titulo = "NOAA — Precipitación anual",
                             value_type = "number"))
    }
    
    # HANSEN
    if (base_sel == "HANSEN_DEFORESTATION") {
      return(summary_from_ts(hansen_ts, "ano", "valor",
                             titulo = "HANSEN — Deforestación anual",
                             value_type = "number",
                             unit = " ha"))
    }
    
    summary_from_ts(NULL)
  })
  
  output$estr_cards <- renderUI({
    s <- current_summary()
    tagList(
      card_box("Lectura analítica", div(s$story)),
      card_box("Último período (niveles)", kv_table(s$last_rows)),
      card_box("Variación respecto al período anterior", kv_table(s$growth_rows))
    )
  })
  
  # =======================================================
  # Pestaña 2 — filtro año fijo
  # =======================================================
  output$fixed_year_ui <- renderUI({
    lock <- input$lock_years %||% FALSE
    if (!isTRUE(lock)) return(tags$span("Active el año fijo para usar este filtro.", style = "font-size:12px;color:#9ca3af;"))
    yrs <- all_years_dep
    if (!length(yrs)) return(tags$span("Sin años disponibles.", style = "font-size:12px;color:#9ca3af;"))
    selectInput("fixed_year", label = NULL, choices = yrs, selected = default_fixed_year)
  })
  
  render_extra_map <- function(base_input_id, output_id, prefix){
    output[[output_id]] <- renderUI({
      base_sel <- input[[base_input_id]]
      if (is.null(base_sel)) return(NULL)
      
      if (base_sel == "DNP_SISBEN") {
        tagList(
          div(
            lbl("Tipología de privación"),
            selectInput(paste0(prefix, "_sisben_metric"), label = NULL,
                        choices = sisben_metric_choices,
                        selected = unname(sisben_metric_choices)[1] %||% "i1")
          ),
          div(
            lbl("Grupo (A/B/C/D)"),
            selectInput(paste0(prefix, "_sisben_grupo"), label = NULL,
                        choices = sisben_group_choices,
                        selected = "Todos")
          )
        )
      } else if (base_sel == "DANE_ECV") {
        div(
          lbl("Indicador FIES-8"),
          selectInput(paste0(prefix, "_ecv_fies_metric"), label = NULL,
                      choices = inds_fies,
                      selected = unname(inds_fies)[1])
        )
      } else if (base_sel == "FINAGRO_CFA_FAST") {
        # (CAMBIO FINAGRO) agrega ESLABON_CADENA para mapas
        tagList(
          div(
            lbl("Métrica para el mapa"),
            selectInput(paste0(prefix, "_fin_metric"), NULL,
                        choices = c("Monto real del crédito"="monto", "Número de créditos"="creditos"),
                        selected = "monto")
          ),
          div(
            lbl("Tipo de productor"),
            selectInput(paste0(prefix, "_fin_tipo"), NULL,
                        choices = finagro_choices$tipo, selected = "Todos")
          ),
          div(
            lbl("Sexo"),
            selectInput(paste0(prefix, "_fin_sexo"), NULL,
                        choices = finagro_choices$sexo, selected = "Todos")
          ),
          div(
            lbl("Línea de crédito"),
            selectInput(paste0(prefix, "_fin_linea"), NULL,
                        choices = finagro_choices$linea, selected = "Todos")
          ),
          div(
            lbl("Eslabón de la cadena"),
            selectInput(paste0(prefix, "_fin_eslabon"), NULL,
                        choices = finagro_choices$eslabon, selected = "Todos")
          )
        )
      } else {
        NULL
      }
    })
  }
  render_extra_map("estr_base2_map1", "map1_extra", "map1")
  render_extra_map("estr_base2_map2", "map2_extra", "map2")
  
  output$map1_year_ui <- renderUI({
    lock <- input$lock_years %||% FALSE
    if (isTRUE(lock)) return(tags$span("Año controlado por el filtro global.", style = "font-size:12px;color:#6b7280;"))
    
    base_sel <- input$estr_base2_map1 %||% default_base_estructura
    ind_sel  <- base_to_indicator_code(base_sel)
    
    if (ind_sel == "FINAGRO") {
      if (is.null(finagro_df) || !nrow(finagro_df)) return(tags$span("Sin años FINAGRO disponibles.", style="font-size:12px;color:#9ca3af;"))
      yrs <- sort(unique(finagro_df$ano))
      return(selectInput("map1_year", label = NULL, choices = yrs, selected = max(yrs, na.rm = TRUE)))
    }
    
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
    
    if (ind_sel == "FINAGRO") {
      if (is.null(finagro_df) || !nrow(finagro_df)) return(tags$span("Sin años FINAGRO disponibles.", style="font-size:12px;color:#9ca3af;"))
      yrs <- sort(unique(finagro_df$ano))
      return(selectInput("map2_year", label = NULL, choices = yrs, selected = max(yrs, na.rm = TRUE)))
    }
    
    df <- get_dep_data(ind_sel)
    if (is.null(df) || !nrow(df)) return(tags$span("Sin años disponibles para esta base", style="font-size:12px;color:#9ca3af;"))
    
    yrs <- sort(unique(df$ano))
    selectInput("map2_year", label = NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  build_map_sf <- function(base_sel, year_val, prefix){
    ind_sel <- base_to_indicator_code(base_sel)
    if (is.null(year_val) || is.null(shp_dep)) return(NULL)
    
    if (ind_sel == "FINAGRO") {
      if (is.null(finagro_df) || !nrow(finagro_df)) return(NULL)
      met <- input[[paste0(prefix, "_fin_metric")]] %||% "monto"
      
      df <- finagro_apply_filters(
        finagro_df,
        tipo    = input[[paste0(prefix, "_fin_tipo")]] %||% "Todos",
        sexo    = input[[paste0(prefix, "_fin_sexo")]] %||% "Todos",
        linea   = input[[paste0(prefix, "_fin_linea")]] %||% "Todos",
        eslabon = input[[paste0(prefix, "_fin_eslabon")]] %||% "Todos"
      )
      
      dd <- df %>%
        filter(ano == as.integer(year_val), !is.na(COD_DPTO2), nzchar(COD_DPTO2)) %>%
        group_by(cod_dep = COD_DPTO2, dep_nom = DEPARTAMENTO_D) %>%
        summarise(
          monto = sum(VALOR_CREDITO_REAL, na.rm = TRUE),
          creditos = sum(NUMERO_CREDITO, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(valor = if (met == "monto") monto else creditos,
               DEPARTAMENTO_D = dep_nom) %>%
        select(cod_dep, dep_nom, DEPARTAMENTO_D, valor)
      
      return(join_dep_shp(dd, shp_dep, df_key_code = "cod_dep", df_key_name = "dep_nom"))
    }
    
    df <- get_dep_data(ind_sel)
    if (is.null(df) || !nrow(df)) return(NULL)
    
    dd <- df %>% filter(ano == as.integer(year_val))
    if (!nrow(dd)) return(NULL)
    
    if (ind_sel == "SISBEN") {
      met <- input[[paste0(prefix, "_sisben_metric")]] %||% "i1"
      grp <- input[[paste0(prefix, "_sisben_grupo")]]  %||% "Todos"
      
      dd <- dd %>% filter(metric == met)
      if (!nrow(dd)) return(NULL)
      
      if ("grupo" %in% names(dd) && grp != "Todos") dd <- dd %>% filter(grupo == grp)
      if (!nrow(dd)) return(NULL)
      
      dd <- dd %>%
        group_by(cod_dep, dep_nom) %>%
        summarise(
          denom = sum(denom, na.rm = TRUE),
          numer = sum(numer, na.rm = TRUE),
          valor = if_else(denom > 0, 100 * numer / denom, NA_real_),
          .groups = "drop"
        ) %>%
        mutate(DEPARTAMENTO_D = dep_nom)
    } else if (ind_sel == "ECV_FIES") {
      met <- input[[paste0(prefix, "_ecv_fies_metric")]] %||% unname(inds_fies)[1]
      dd <- dd %>% filter(metric == met)
      dd <- dd %>%
        group_by(cod_dep, dep_nom) %>%
        summarise(
          numer = sum(numer, na.rm = TRUE),
          denom = sum(denom, na.rm = TRUE),
          valor = if_else(denom > 0, 100 * numer / denom, NA_real_),
          .groups = "drop"
        ) %>%
        mutate(DEPARTAMENTO_D = dep_nom)
    } else {
      dd <- dd %>%
        group_by(cod_dep, dep_nom) %>%
        summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
        mutate(DEPARTAMENTO_D = dep_nom)
    }
    
    join_dep_shp(dd, shp_dep, df_key_code = "cod_dep", df_key_name = "dep_nom")
  }
  
  map1_data <- reactive({
    base_sel <- input$estr_base2_map1 %||% default_base_estructura
    lock     <- input$lock_years %||% FALSE
    yr       <- if (isTRUE(lock)) input$fixed_year else input$map1_year
    build_map_sf(base_sel, yr, "map1")
  })
  
  map2_data <- reactive({
    base_sel <- input$estr_base2_map2 %||% default_base_estructura
    lock     <- input$lock_years %||% FALSE
    yr       <- if (isTRUE(lock)) input$fixed_year else input$map2_year
    build_map_sf(base_sel, yr, "map2")
  })
  
  output$map_1 <- leaflet::renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(lng = -74, lat = 4.5, zoom = 5)
  })
  output$map_2 <- leaflet::renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(lng = -74, lat = 4.5, zoom = 5)
  })
  
  make_bin_labels <- function(bins, value_type = c("percent","number")){
    value_type <- match.arg(value_type)
    if (length(bins) < 2) return(character(0))
    
    fmtb <- function(v){
      if (value_type == "percent") fmt_pct(v, 1) else fmt_num_es(v, 1)
    }
    
    labs <- character(length(bins) - 1)
    for (i in seq_len(length(bins) - 1)) {
      a <- fmtb(bins[i]); b <- fmtb(bins[i + 1])
      core <- paste0(a, " - ", b)
      labs[i] <- if (i >= 2) paste0("> ", core) else core
    }
    labs
  }
  
  # =======================================================
  # (MOD) Labels: SIEMPRE usar DEPARTAMENTO_D
  # =======================================================
  update_dep_leaflet <- function(map_id, md, palette_name, legend_title, value_type = c("percent","number"), unit_suffix = ""){
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
    bins <- unique(qs)
    if (length(bins) < 3) bins <- pretty(range(vals, na.rm = TRUE), n = 4)
    
    pal <- leaflet::colorBin(palette = palette_name, domain = vals, bins = bins, na.color = "#f5f5f5")
    
    # NOMBRE: DEPARTAMENTO_D (siempre que exista)
    dep_name <- if ("DEPARTAMENTO_D" %in% names(md)) as.character(md$DEPARTAMENTO_D) else NA_character_
    if (all(is.na(dep_name) | !nzchar(dep_name))) {
      dep_name <- if ("dep_nom" %in% names(md)) as.character(md$dep_nom) else md$join_name
    }
    if (all(is.na(dep_name) | !nzchar(dep_name))) dep_name <- md$join_code
    
    dep_name <- ifelse(is.na(dep_name) | !nzchar(dep_name), md$join_code, dep_name)
    
    lab_vals <- if (value_type == "percent") fmt_pct(md$valor, 1) else fmt_short(md$valor)
    labels <- sprintf("<strong>%s</strong><br/>Valor: %s%s", dep_name, lab_vals, unit_suffix) %>% lapply(htmltools::HTML)
    
    leg_labels <- make_bin_labels(bins, value_type)
    mid_vals   <- (bins[-length(bins)] + bins[-1]) / 2
    leg_cols   <- pal(mid_vals)
    
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
        colors   = leg_cols,
        labels   = leg_labels,
        opacity  = 0.7,
        title    = legend_title,
        position = "bottomright"
      ) %>%
      leaflet::setView(lng = -74, lat = 4.5, zoom = 5)
  }
  
  observe({
    base_sel <- input$estr_base2_map1 %||% default_base_estructura
    ind_sel  <- base_to_indicator_code(base_sel)
    md       <- map1_data()
    
    tipo_val <- if (ind_sel %in% c("DEP_RATIO","SISBEN","ECV_FIES")) "percent" else "number"
    legend_title <- if (tipo_val == "percent") "Porcentaje" else "Cantidad"
    
    if (ind_sel == "FINAGRO") {
      met <- input$map1_fin_metric %||% "monto"
      if (met == "monto")   legend_title <- "Monto real (COP)"
      if (met == "creditos")legend_title <- "Número de créditos"
    }
    
    update_dep_leaflet("map_1", md, palette_name = "Greens", legend_title = legend_title, value_type = tipo_val, unit_suffix = "")
  })
  
  observe({
    base_sel <- input$estr_base2_map2 %||% default_base_estructura
    ind_sel  <- base_to_indicator_code(base_sel)
    md       <- map2_data()
    
    tipo_val <- if (ind_sel %in% c("DEP_RATIO","SISBEN","ECV_FIES")) "percent" else "number"
    legend_title <- if (tipo_val == "percent") "Porcentaje" else "Cantidad"
    
    if (ind_sel == "FINAGRO") {
      met <- input$map2_fin_metric %||% "monto"
      if (met == "monto")   legend_title <- "Monto real (COP)"
      if (met == "creditos")legend_title <- "Número de créditos"
    }
    
    update_dep_leaflet("map_2", md, palette_name = "Blues", legend_title = legend_title, value_type = tipo_val, unit_suffix = "")
  })
}

shinyApp(ui, server)
