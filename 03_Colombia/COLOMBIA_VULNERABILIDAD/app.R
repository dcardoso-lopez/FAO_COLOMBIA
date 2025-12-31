# app_sninny_estructura_multi_bases_con_crecimiento_y_SIPSA_IHH.R
# -------------------------------------------------------------------
# ✅ Incluye indicador de crecimiento poblacional (30 años)
# ✅ Incluye SIPSA Abastecimiento OD: 041_DANE_SIPSA-Abast_od.rds
#    - Serie temporal: IHH nacional (según Origen/Destino + Grupo)
#    - Mapas: IHH por departamento (según Origen/Destino + Grupo)
# ✅ SIN filtro de alimento para SIPSA
# ✅ Tooltips/labels de mapas: muestran nombre del departamento (Caldas, Antioquia, etc.)
# ✅ NUEVO (pedido): en TODOS los mapas (excepto SIPSA) la etiqueta usa el nombre tal cual viene
#    en la base usada (DEPARTAMENTO_D o DEPARTAMENTO_O). No se fuerza Title Case.
# ✅ NUEVO (pedido HOY): en el mapa SIPSA (departamento) las etiquetas van en MAYÚSCULAS.
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
pick_dep_name_col_o <- function(nms) pick_col_simple(nms, c("departamento_o","DEPARTAMENTO_O","depto_o","nom_dpto_o","origen_dpto","DEPARTAMENTO_ORIGEN"))

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

norm_txt_upper <- function(x){
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringi::stri_trim_both(toupper(x))
  x
}
title_case_simple <- function(x){
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(stringr::str_squish(x))
  stringi::stri_trans_totitle(x)
}
trim_safe <- function(x){
  x <- as.character(x)
  x <- stringi::stri_replace_all_fixed(x, "\u00A0", " ")
  stringi::stri_trim_both(x)
}

# =========================================================
# Sistemas + Indicadores (Estructura)
# =========================================================
sistemas_df <- tibble::tribble(
  ~sistema,              ~archivos,
  "DANE_POPULATION",     list("051_DANE_Proyecciones_P.rds"),
  "DANE_POP_CRECIMIENTO",list("051_DANE_Proyecciones_P.rds"),
  "DANE_ECV",            list("052_DANE_ECV.rds"),
  "DANE_SIPSA_ABAST_OD", list("041_DANE_SIPSA-Abast_od.rds"),
  "INS_SIVIGILA_BPAN",   list("021_INS_SIVIGILA-BPAN.rds"),
  "INS_SIVIGILA_ETA",    list("022_INS_SIVIGILA-ETA.rds"),
  "INS_SIVIGILA_NDA",    list("023_INS_SIVIGILA-NDA.rds"),
  "DNP_SISBEN",          list("031_DNP_SISBEN.rds"),
  "NOAA_PRECIPITACION",  list("131_NOAA_Precipitación.rds"),
  "HANSEN_DEFORESTATION",list("141_HANSEN_DEFORESTATION.rds")
) %>%
  mutate(rutas = lapply(archivos, function(v) file.path(data_dir, v)))

indicadores_df <- tibble::tribble(
  ~titulo,                                                               ~dimension,   ~sistema,
  "Transición demográfica territorial — Razón de dependencia",           "Estructura", "DANE_POPULATION",
  "Crecimiento poblacional promedio anual (30 años)",                    "Estructura", "DANE_POP_CRECIMIENTO",
  "Condiciones de inseguridad alimentaria y consumo de ultra-procesados en los hogares", "Estructura", "DANE_ECV",
  "Abastecimiento de alimentos (SIPSA) — Concentración (IHH) por dpto",   "Estructura", "DANE_SIPSA_ABAST_OD",
  "Enfermedades Transmitidas por Alimentos",                             "Estructura", "INS_SIVIGILA_ETA",
  "Bajo peso al nacer",                                                  "Estructura", "INS_SIVIGILA_BPAN",
  "Desnutrición aguda infantil",                                         "Estructura", "INS_SIVIGILA_NDA",
  "Condiciones socio-económicas de la población",                        "Estructura", "DNP_SISBEN",
  "Lluvia en el territorio",                                             "Estructura", "NOAA_PRECIPITACION",
  "Pérdida de bosque en el territorio",                                  "Estructura", "HANSEN_DEFORESTATION"
)

choices_por_dimension <- function(dim){
  orden_estructura <- c(
    "DANE_POPULATION",
    "DANE_POP_CRECIMIENTO",
    "DANE_ECV",
    "DANE_SIPSA_ABAST_OD",
    "INS_SIVIGILA_ETA",
    "INS_SIVIGILA_BPAN",
    "INS_SIVIGILA_NDA",
    "DNP_SISBEN",
    "NOAA_PRECIPITACION",
    "HANSEN_DEFORESTATION"
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
    "DANE_POP_CRECIMIENTO" = "DEP_CRECIMIENTO",
    "DANE_ECV"             = "ECV_FIES",
    "DANE_SIPSA_ABAST_OD"  = "SIPSA_IHH",
    "INS_SIVIGILA_BPAN"    = "BPAN",
    "INS_SIVIGILA_ETA"     = "ETA",
    "INS_SIVIGILA_NDA"     = "NDA",
    "DNP_SISBEN"           = "SISBEN",
    "NOAA_PRECIPITACION"   = "PRECIP",
    "HANSEN_DEFORESTATION" = "HANSEN",
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
  shp_dep <- sf::st_simplify(shp_dep, dTolerance = 0.001, preserveTopology = TRUE)
  
  shp_dep$join_code <- if (!is.na(dep_code_col_shp)) as.character(shp_dep[[dep_code_col_shp]]) else NA_character_
  shp_dep$join_name <- if (!is.na(dep_name_col_shp)) norm_txt_upper(shp_dep[[dep_name_col_shp]]) else NA_character_
  
  # (solo fallback)
  shp_dep$DEP_LABEL <- if (!is.na(dep_name_col_shp)) safe_chr(shp_dep[[dep_name_col_shp]]) else shp_dep$join_code
}

# =========================================================
# JOIN: conservar etiqueta tal cual base (DEPARTAMENTO_D/DEPARTAMENTO_O) excepto SIPSA (que ya trae DEP_LABEL)
# =========================================================
join_dep_shp <- function(df, shp, df_key_code = NULL, df_key_name = NULL){
  if (is.null(df) || !nrow(df) || is.null(shp)) return(NULL)
  
  df2 <- df
  
  # join_code por código
  if (!is.null(df_key_code) && df_key_code %in% names(df2)) {
    df2$join_code <- as.character(df2[[df_key_code]])
  } else {
    df2$join_code <- NA_character_
  }
  
  # join_name (solo para fallback) EN MAYÚSCULAS NORMALIZADAS
  if (!is.null(df_key_name) && df_key_name %in% names(df2)) {
    nm <- as.character(df2[[df_key_name]])
    nm <- stringi::stri_trans_general(nm, "Latin-ASCII")
    df2$join_name <- stringi::stri_trim_both(toupper(nm))
  } else if ("dep_nom" %in% names(df2)) {
    nm <- as.character(df2$dep_nom)
    nm <- stringi::stri_trans_general(nm, "Latin-ASCII")
    df2$join_name <- stringi::stri_trim_both(toupper(nm))
  } else if ("DEPARTAMENTO_D" %in% names(df2)) {
    nm <- as.character(df2$DEPARTAMENTO_D)
    nm <- stringi::stri_trans_general(nm, "Latin-ASCII")
    df2$join_name <- stringi::stri_trim_both(toupper(nm))
  } else if ("DEPARTAMENTO_O" %in% names(df2)) {
    nm <- as.character(df2$DEPARTAMENTO_O)
    nm <- stringi::stri_trans_general(nm, "Latin-ASCII")
    df2$join_name <- stringi::stri_trim_both(toupper(nm))
  } else {
    df2$join_name <- NA_character_
  }
  
  # ✅ etiqueta "tal cual base" (preferencia)
  # (si ya viene DEP_LABEL desde SIPSA, se respeta; si no, toma DEPARTAMENTO_D/DEPARTAMENTO_O/dep_nom)
  if ("DEP_LABEL" %in% names(df2)) {
    df2$DEP_LABEL_SRC <- trim_safe(df2$DEP_LABEL)
  } else if ("DEPARTAMENTO_D" %in% names(df2)) {
    df2$DEP_LABEL_SRC <- trim_safe(df2$DEPARTAMENTO_D)
  } else if ("DEPARTAMENTO_O" %in% names(df2)) {
    df2$DEP_LABEL_SRC <- trim_safe(df2$DEPARTAMENTO_O)
  } else if ("dep_nom" %in% names(df2)) {
    df2$DEP_LABEL_SRC <- trim_safe(df2$dep_nom)
  } else if (!is.null(df_key_name) && df_key_name %in% names(df2)) {
    df2$DEP_LABEL_SRC <- trim_safe(df2[[df_key_name]])
  } else {
    df2$DEP_LABEL_SRC <- NA_character_
  }
  
  out_code <- suppressWarnings(dplyr::left_join(shp, df2, by = c("join_code" = "join_code")))
  ok_code  <- if ("valor" %in% names(out_code)) mean(!is.na(out_code$valor)) else 0
  out <- if (is.finite(ok_code) && ok_code >= 0.20) out_code else suppressWarnings(dplyr::left_join(shp, df2, by = c("join_name" = "join_name")))
  
  # ✅ garantizar variable principal (para tu regla)
  if (!("DEPARTAMENTO_D" %in% names(out))) {
    if ("dep_nom" %in% names(out)) {
      out$DEPARTAMENTO_D <- out$dep_nom
    } else if ("join_name" %in% names(out)) {
      out$DEPARTAMENTO_D <- out$join_name
    } else if ("NOM_DPTO" %in% names(out)) {
      out$DEPARTAMENTO_D <- out$NOM_DPTO
    } else if ("DPTO_CNMBR" %in% names(out)) {
      out$DEPARTAMENTO_D <- out$DPTO_CNMBR
    } else {
      out$DEPARTAMENTO_D <- out$join_code
    }
  }
  
  # ✅ DEP_LABEL final: usar lo que venga de la BASE (DEP_LABEL_SRC) si existe
  out$DEP_LABEL <- if ("DEP_LABEL_SRC" %in% names(out)) out$DEP_LABEL_SRC else NA_character_
  out$DEP_LABEL <- ifelse(is.na(out$DEP_LABEL) | !nzchar(out$DEP_LABEL),
                          if ("DEP_LABEL" %in% names(shp)) as.character(out$DEP_LABEL) else NA_character_,
                          out$DEP_LABEL)
  
  # fallback a DEPARTAMENTO_D / join_code
  out$DEP_LABEL <- ifelse(is.na(out$DEP_LABEL) | !nzchar(out$DEP_LABEL), as.character(out$DEPARTAMENTO_D), out$DEP_LABEL)
  out$DEP_LABEL <- ifelse(is.na(out$DEP_LABEL) | !nzchar(out$DEP_LABEL), as.character(out$join_code), out$DEP_LABEL)
  
  out
}

# =========================================================
# 1) DANE POPULATION — razón de dependencia (serie + dep)
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
      DEP_LABEL = DEPARTAMENTO_D,  # ✅ etiqueta tal cual base
      valor   = valor
    ) %>%
    arrange(ano, cod_dep)
  
}, error = function(e){
  message("DANE_POPULATION (dep): error -> ", conditionMessage(e))
  NULL
})

# =========================================================
# NUEVO: Crecimiento poblacional promedio anual (30 años)
# =========================================================
L <- 30L

pob_crecimiento_30 <- tryCatch({
  if (is.null(pob_raw) || !nrow(pob_raw)) return(NULL)
  
  nms <- names(pob_raw)
  col_ano  <- pick_col_simple(nms, c("ano","año","anio","year"))
  col_dep  <- pick_col_simple(nms, c("COD_DANE_DPTO_D","cod_dane_dpto_d","COD_DPTO2","cod_dpto2","cod_dpto","cod_depto"))
  col_nom  <- pick_col_simple(nms, c("DEPARTAMENTO_D","departamento_d","departamento","nom_dpto","depto"))
  col_pob  <- pick_col_simple(nms, c("poblacion","pob","population"))
  
  if (any(is.na(c(col_ano, col_dep, col_pob)))) return(NULL)
  
  df_total <- pob_raw %>%
    transmute(
      ano             = suppressWarnings(as.integer(parse_num_co(.data[[col_ano]]))),
      COD_DANE_DPTO_D = norm_dep2(.data[[col_dep]]),
      DEPARTAMENTO_D  = if (!is.na(col_nom)) safe_chr(.data[[col_nom]]) else NA_character_,
      poblacion       = parse_num_co(.data[[col_pob]])
    ) %>%
    filter(!is.na(ano), !is.na(COD_DANE_DPTO_D), nzchar(COD_DANE_DPTO_D),
           is.finite(poblacion), poblacion >= 0) %>%
    group_by(ano, COD_DANE_DPTO_D, DEPARTAMENTO_D) %>%
    summarise(poblacion = sum(poblacion, na.rm = TRUE), .groups = "drop")
  
  if (!nrow(df_total)) return(NULL)
  
  df_total %>%
    group_by(COD_DANE_DPTO_D, DEPARTAMENTO_D) %>%
    arrange(ano) %>%
    mutate(
      poblacion_tminusL = dplyr::lag(poblacion, n = L),
      g_aprox = dplyr::if_else(
        !is.na(poblacion_tminusL) & poblacion_tminusL > 0,
        (poblacion / poblacion_tminusL - 1) / L,
        NA_real_
      )
    ) %>%
    filter(!is.na(g_aprox)) %>%
    ungroup() %>%
    transmute(
      ano = ano,
      cod_dep = COD_DANE_DPTO_D,
      dep_nom = DEPARTAMENTO_D,
      DEPARTAMENTO_D = DEPARTAMENTO_D,
      DEP_LABEL = DEPARTAMENTO_D,  # ✅ etiqueta tal cual base
      valor = g_aprox * 100
    ) %>%
    arrange(cod_dep, ano)
  
}, error = function(e){
  message("DANE_POPULATION (crecimiento 30 años): error -> ", conditionMessage(e))
  NULL
})

pob_crecimiento_ts <- tryCatch({
  if (is.null(pob_raw) || !nrow(pob_raw)) return(NULL)
  
  nms <- names(pob_raw)
  col_ano  <- pick_col_simple(nms, c("ano","año","anio","year"))
  col_pob  <- pick_col_simple(nms, c("poblacion","pob","population"))
  if (any(is.na(c(col_ano, col_pob)))) return(NULL)
  
  df_total <- pob_raw %>%
    transmute(
      ano       = suppressWarnings(as.integer(parse_num_co(.data[[col_ano]]))),
      poblacion = parse_num_co(.data[[col_pob]])
    ) %>%
    filter(!is.na(ano), is.finite(poblacion), poblacion >= 0) %>%
    group_by(ano) %>%
    summarise(poblacion = sum(poblacion, na.rm = TRUE), .groups = "drop") %>%
    arrange(ano)
  
  if (!nrow(df_total)) return(NULL)
  
  df_total %>%
    mutate(
      poblacion_tminusL = dplyr::lag(poblacion, n = L),
      g_aprox = dplyr::if_else(
        !is.na(poblacion_tminusL) & poblacion_tminusL > 0,
        (poblacion / poblacion_tminusL - 1) / L,
        NA_real_
      )
    ) %>%
    filter(!is.na(g_aprox)) %>%
    transmute(ano = ano, valor = g_aprox * 100) %>%
    arrange(ano)
  
}, error = function(e){
  message("DANE_POPULATION (crecimiento ts): error -> ", conditionMessage(e))
  NULL
})

make_pob_crecimiento_ts_plot <- function(){
  if (is.null(pob_crecimiento_ts) || !nrow(pob_crecimiento_ts))
    return(empty_ts_plot("Sin datos para estimar la serie de crecimiento poblacional."))
  
  df <- pob_crecimiento_ts
  ticks_x <- year_ticks_5(df$ano)
  
  plotly::plot_ly(
    data = df, x = ~ano, y = ~valor,
    type = "scatter", mode = "lines+markers",
    line = list(width = 2, color = COL_PLOT),
    marker = list(size = 6, color = COL_PLOT),
    customdata = fmt_pct(df$valor, 2),
    hovertemplate = paste0(
      "<b>Año t:</b> %{x}<br>",
      "<b>Rezago (L):</b> ", L, " años<br>",
      "<b>Crec. promedio anual:</b> %{customdata}<extra></extra>"
    )
  ) %>%
    plotly::layout(
      xaxis = list(title = "", tickmode = "array", tickvals = ticks_x, ticktext = ticks_x, showgrid = FALSE, automargin = TRUE),
      yaxis = list(title = "Crecimiento promedio anual (%)", showgrid = FALSE, automargin = TRUE),
      margin = list(l = 70, r = 20, t = 10, b = 60),
      hovermode = "x unified",
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

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
# SIPSA ABAST OD — IHH (HHI)
# =========================================================
sipsa_raw <- tryCatch({
  p <- file.path(data_dir, "041_DANE_SIPSA-Abast_od.rds")
  if (!file.exists(p)) return(NULL)
  df <- readRDS(p)
  if (is.null(df) || !nrow(df)) return(NULL)
  df
}, error = function(e) NULL)

sipsa_long <- NULL
sipsa_years <- integer(0)
sipsa_group_levels <- character(0)

if (!is.null(sipsa_raw) && nrow(sipsa_raw)) {
  nms <- names(sipsa_raw)
  
  col_ano <- pick_year_col(nms)
  
  col_cod_o <- pick_col_simple(nms, c("cod_dane_dpto_o","cod_dane_dep_o","cod_dep_o","cod_dpto_o","cod_origen","cod_dane_origen","COD_DANE_DPTO_O","COD_DANE_DEP_O"))
  col_nom_o <- pick_col_simple(nms, c("departamento_o","depto_o","nom_dpto_o","origen_dpto","DEPARTAMENTO_O","DEPARTAMENTO_ORIGEN"))
  col_cod_d <- pick_col_simple(nms, c("cod_dane_dpto_d","cod_dane_dep_d","cod_dep_d","cod_dpto_d","cod_destino","cod_dane_destino","COD_DANE_DPTO_D","COD_DANE_DEP_D"))
  col_nom_d <- pick_col_simple(nms, c("departamento_d","depto_d","nom_dpto_d","destino_dpto","DEPARTAMENTO_D","DEPARTAMENTO_DESTINO"))
  
  col_grupo <- pick_col_simple(nms, c("grupo","grupo_alimentos","grupo_alimento","GRUPO"))
  col_ton   <- pick_col_simple(nms, c("toneladas","ton","ton_total","cantidad_ton","q_ton","TON","TONELADAS","valor_ton"))
  
  if (!any(is.na(c(col_ano, col_cod_o, col_cod_d, col_ton)))) {
    sipsa_long <- sipsa_raw %>%
      transmute(
        ano   = suppressWarnings(as.integer(parse_num_co(.data[[col_ano]]))),
        cod_o = norm_dep2(.data[[col_cod_o]]),
        nom_o = if (!is.na(col_nom_o)) safe_chr(.data[[col_nom_o]]) else NA_character_,
        cod_d = norm_dep2(.data[[col_cod_d]]),
        nom_d = if (!is.na(col_nom_d)) safe_chr(.data[[col_nom_d]]) else NA_character_,
        grupo = if (!is.na(col_grupo)) safe_chr(.data[[col_grupo]]) else "TOTAL",
        ton   = parse_num_co(.data[[col_ton]])
      ) %>%
      filter(is.finite(ano), !is.na(cod_o), nzchar(cod_o), !is.na(cod_d), nzchar(cod_d),
             is.finite(ton), ton >= 0) %>%
      mutate(
        # (SIPSA mantiene su lógica actual)
        nom_o = norm_txt_upper(nom_o),
        nom_d = norm_txt_upper(nom_d),
        grupo = stringi::stri_trim_both(toupper(grupo))
      )
    
    if (!is.null(sipsa_long) && nrow(sipsa_long)) {
      sipsa_years <- sort(unique(sipsa_long$ano))
      sipsa_group_levels <- sort(unique(na.omit(as.character(sipsa_long$grupo))))
      if (!length(sipsa_group_levels)) sipsa_group_levels <- "TOTAL"
    }
  }
}

sipsa_group_choices <- {
  if (!length(sipsa_group_levels)) c("Todos" = "Todos")
  else c("Todos" = "Todos", stats::setNames(sipsa_group_levels, sipsa_group_levels))
}

calc_hhi <- function(x){
  x <- x[is.finite(x) & x >= 0]
  if (!length(x)) return(NA_real_)
  s <- sum(x)
  if (!is.finite(s) || s <= 0) return(NA_real_)
  p <- x / s
  10000 * sum(p^2, na.rm = TRUE)
}

sipsa_ts_nacional <- function(dir = c("Origen","Destino"), grupo = "Todos"){
  dir <- match.arg(dir)
  if (is.null(sipsa_long) || !nrow(sipsa_long)) return(NULL)
  dd <- sipsa_long
  if (!is.null(grupo) && grupo != "Todos") dd <- dd %>% filter(grupo == grupo)
  if (!nrow(dd)) return(NULL)
  
  if (dir == "Origen") {
    by_year_dep <- dd %>% group_by(ano, cod_dep = cod_o) %>% summarise(ton = sum(ton, na.rm = TRUE), .groups="drop")
  } else {
    by_year_dep <- dd %>% group_by(ano, cod_dep = cod_d) %>% summarise(ton = sum(ton, na.rm = TRUE), .groups="drop")
  }
  
  by_year_dep %>%
    group_by(ano) %>%
    summarise(valor = calc_hhi(ton), .groups="drop") %>%
    arrange(ano)
}

make_sipsa_ihh_ts_plot <- function(dir = "Origen", grupo = "Todos"){
  ts <- sipsa_ts_nacional(dir = dir, grupo = grupo)
  if (is.null(ts) || !nrow(ts)) return(empty_ts_plot("Sin datos SIPSA para los filtros seleccionados."))
  
  ticks_x <- year_ticks_2(ts$ano)
  
  plotly::plot_ly(
    data = ts, x = ~ano, y = ~valor,
    type = "scatter", mode = "lines+markers",
    line = list(width = 2, color = COL_PLOT),
    marker = list(size = 6, color = COL_PLOT),
    customdata = fmt_num_es(ts$valor, 0),
    hovertemplate = paste0(
      "<b>Año:</b> %{x}<br>",
      "<b>Rol:</b> ", dir, "<br>",
      "<b>IHH (0–10.000):</b> %{customdata}<extra></extra>"
    )
  ) %>%
    plotly::layout(
      xaxis = list(title = "", tickmode = "array", tickvals = ticks_x, ticktext = ticks_x, showgrid = FALSE, automargin = TRUE),
      yaxis = list(title = "IHH (0–10.000)", showgrid = FALSE, automargin = TRUE, rangemode = "tozero"),
      margin = list(l = 70, r = 20, t = 10, b = 60),
      hovermode = "x unified",
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

sipsa_map_ihh_by_dep <- function(year_val, dir = c("Origen","Destino"), grupo = "Todos"){
  dir <- match.arg(dir)
  if (is.null(sipsa_long) || !nrow(sipsa_long)) return(NULL)
  if (is.null(year_val) || !is.finite(as.integer(year_val))) return(NULL)
  
  dd <- sipsa_long %>% filter(ano == as.integer(year_val))
  if (!is.null(grupo) && grupo != "Todos") dd <- dd %>% filter(grupo == grupo)
  if (!nrow(dd)) return(NULL)
  
  if (dir == "Origen") {
    by_pair <- dd %>%
      group_by(cod_dep = cod_o, dep_nom = nom_o, counter = cod_d) %>%
      summarise(ton = sum(ton, na.rm = TRUE), .groups="drop")
  } else {
    by_pair <- dd %>%
      group_by(cod_dep = cod_d, dep_nom = nom_d, counter = cod_o) %>%
      summarise(ton = sum(ton, na.rm = TRUE), .groups="drop")
  }
  
  by_pair %>%
    group_by(cod_dep, dep_nom) %>%
    summarise(valor = calc_hhi(ton), .groups="drop") %>%
    mutate(
      DEPARTAMENTO_D = dep_nom,
      DEP_LABEL = dep_nom  # ✅ SIPSA se queda con su etiqueta (ya viene en MAYÚSCULA)
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
    summarise(valor = dplyr::n(), .groups = "drop") %>%
    mutate(
      DEPARTAMENTO_D = dep_nom,
      DEP_LABEL      = dep_nom # ✅ etiqueta tal cual base
    )
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
    summarise(valor = sum(total_enf, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      DEPARTAMENTO_D = dep_nom,
      DEP_LABEL      = dep_nom # ✅ etiqueta tal cual base
    )
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
# 3) SISBEN — prevalencia IPM / hogares
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
      DEPARTAMENTO_D = dep_nom,
      DEP_LABEL      = dep_nom # ✅ etiqueta tal cual base
    ) %>%
    filter(is.finite(denom)) %>%
    arrange(metric, ano, cod_dep, dep_nom, grupo)
}

prep_sisben_hogares_long <- function(df){
  if (is.null(df) || !nrow(df)) return(NULL)
  
  nms <- names(df)
  col_ano <- pick_year_col(nms)
  col_w   <- pick_col_simple(nms, c("Nw_hogares","nw_hogares","nw","peso","weight","ponderador"))
  col_dep_code <- pick_dep_col(nms)
  col_dep_name <- pick_dep_name_col(nms)
  col_grp <- pick_col_simple(nms, c("grupo","group","categoria","categoría","segmento","nivel","grupo_sisben"))
  
  if (is.na(col_ano) || is.na(col_w)) return(NULL)
  
  out <- df %>%
    transmute(
      ano     = suppressWarnings(as.integer(parse_num_co(.data[[col_ano]]))),
      cod_dep = if (!is.na(col_dep_code)) norm_dep2(.data[[col_dep_code]]) else NA_character_,
      dep_nom = if (!is.na(col_dep_name)) safe_chr(.data[[col_dep_name]]) else NA_character_,
      grupo   = if (!is.na(col_grp)) safe_chr(.data[[col_grp]]) else "TOTAL",
      hogares = parse_num_co(.data[[col_w]])
    ) %>%
    filter(!is.na(ano), is.finite(hogares), hogares > 0) %>%
    mutate(
      grupo   = toupper(stringi::stri_trim_both(grupo))
    )
  
  out %>%
    group_by(ano, cod_dep, dep_nom, grupo) %>%
    summarise(hogares = sum(hogares, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      DEPARTAMENTO_D = dep_nom,
      DEP_LABEL      = dep_nom # ✅ etiqueta tal cual base
    )
}

sisben_prev_long <- prep_sisben_agg_preval_long(sisben_raw)
sisben_hog_long  <- prep_sisben_hogares_long(sisben_raw)

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

make_sisben_ts_plot <- function(view_sel = c("prevalencia","hogares"),
                                metric_sel = "i1",
                                grupo_sel = "Todos"){
  view_sel <- match.arg(view_sel)
  
  if (view_sel == "hogares") {
    
    if (is.null(sisben_hog_long) || !nrow(sisben_hog_long))
      return(empty_ts_plot("Sin datos de hogares por grupo."))
    
    dd <- sisben_hog_long
    if (grupo_sel != "Todos") dd <- dd %>% filter(grupo == grupo_sel)
    
    dd <- dd %>%
      group_by(ano, grupo) %>%
      summarise(hogares = sum(hogares, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(ano), is.finite(hogares))
    
    if (!nrow(dd)) return(empty_ts_plot("Sin datos de hogares por grupo."))
    
    dd_base <- dd %>% filter(grupo != "TOTAL")
    
    if (grupo_sel == "Todos") {
      total <- dd_base %>%
        group_by(ano) %>%
        summarise(hogares = sum(hogares, na.rm = TRUE), .groups = "drop") %>%
        mutate(grupo = "TOTAL")
      ts <- bind_rows(dd_base, total) %>% arrange(ano, grupo)
    } else {
      ts <- dd %>% arrange(ano, grupo)
    }
    
    ticks_x <- year_ticks_2(ts$ano)
    
    plotly::plot_ly(
      data = ts,
      x = ~ano, y = ~hogares,
      type = "scatter", mode = "lines+markers",
      split = ~grupo,
      customdata = fmt_short(ts$hogares),
      hovertemplate = "<b>Año:</b> %{x}<br><b>Grupo:</b> %{fullData.name}<br><b>Hogares:</b> %{customdata}<extra></extra>"
    ) %>%
      plotly::layout(
        xaxis = list(title = "", tickmode = "array", tickvals = ticks_x, ticktext = ticks_x, showgrid = FALSE, automargin = TRUE),
        yaxis = list(title = "Hogares ponderados", showgrid = FALSE, automargin = TRUE, rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = -0.22, xanchor = "left", yanchor = "top"),
        margin = list(l = 70, r = 20, t = 10, b = 95),
        hovermode = "x unified",
        hoverlabel = PLOTLY_HOVERLABEL,
        plot_bgcolor  = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
    
  } else {
    
    if (is.null(sisben_prev_long) || !nrow(sisben_prev_long))
      return(empty_ts_plot("Sin datos Sisbén disponibles."))
    
    metric_sel <- metric_sel %||% "i1"
    nm <- if (metric_sel %in% names(sisben_i_labels)) sisben_i_labels[[metric_sel]] else metric_sel
    
    dd <- sisben_prev_long %>% filter(metric == metric_sel)
    if (!nrow(dd)) return(empty_ts_plot("Sin datos para la privación seleccionada."))
    
    if (grupo_sel != "Todos") dd <- dd %>% filter(grupo == grupo_sel)
    
    if (grupo_sel == "Todos") {
      ts_g <- dd %>%
        group_by(ano, grupo) %>%
        summarise(
          denom = sum(denom, na.rm = TRUE),
          numer = sum(numer, na.rm = TRUE),
          valor = if_else(denom > 0, 100 * numer / denom, NA_real_),
          .groups = "drop"
        ) %>% filter(is.finite(valor))
      
      ts_t <- dd %>%
        group_by(ano) %>%
        summarise(
          denom = sum(denom, na.rm = TRUE),
          numer = sum(numer, na.rm = TRUE),
          valor = if_else(denom > 0, 100 * numer / denom, NA_real_),
          .groups = "drop"
        ) %>% filter(is.finite(valor)) %>% mutate(grupo = "TOTAL")
      
      ts <- bind_rows(ts_g, ts_t) %>% arrange(ano, grupo)
    } else {
      ts <- dd %>%
        group_by(ano, grupo) %>%
        summarise(
          denom = sum(denom, na.rm = TRUE),
          numer = sum(numer, na.rm = TRUE),
          valor = if_else(denom > 0, 100 * numer / denom, NA_real_),
          .groups = "drop"
        ) %>% filter(is.finite(valor))
    }
    
    if (!nrow(ts)) return(empty_ts_plot("Sin serie temporal para la prevalencia por grupo."))
    
    ticks_x <- year_ticks_2(ts$ano)
    ticks_y <- seq(0, 100, by = 5)
    
    plotly::plot_ly(
      data = ts,
      x = ~ano, y = ~valor,
      type = "scatter", mode = "lines+markers",
      split = ~grupo,
      customdata = fmt_pct(ts$valor, 1),
      hovertemplate = paste0(
        "<b>Año:</b> %{x}<br>",
        "<b>Grupo:</b> %{fullData.name}<br>",
        "<b>Prevalencia:</b> %{customdata}",
        "<extra></extra>"
      )
    ) %>%
      plotly::layout(
        xaxis = list(title = "", tickmode = "array", tickvals = ticks_x, ticktext = ticks_x, showgrid = FALSE, automargin = TRUE),
        yaxis = list(title = paste0(nm, " (%)"), tickvals = ticks_y, ticktext = fmt_pct(ticks_y, 1),
                     showgrid = FALSE, automargin = TRUE, rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = -0.22, xanchor = "left", yanchor = "top"),
        margin = list(l = 70, r = 20, t = 10, b = 95),
        hovermode = "x unified",
        hoverlabel = PLOTLY_HOVERLABEL,
        plot_bgcolor  = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  }
}

# =========================================================
# 4) ECV — FIES-8
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
      DEPARTAMENTO_D = dep_nom,
      DEP_LABEL      = dep_nom # ✅ etiqueta tal cual base
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
# 5) NOAA — Precipitación
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
      filter(!is.na(ano), !is.na(valor), valor >= 0)
    
    noaa_dep_all <- df %>%
      filter(!is.na(cod_dep), nzchar(cod_dep)) %>%
      group_by(ano, cod_dep, dep_nom) %>%
      summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        DEPARTAMENTO_D = dep_nom,
        DEP_LABEL      = dep_nom # ✅ etiqueta tal cual base
      )
    
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
# 6) HANSEN — Deforestación
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
      filter(!is.na(ano), is.finite(valor), valor >= 0, !is.na(cod_dep), nzchar(cod_dep))
    
    hansen_dep_all <- df %>%
      group_by(ano, cod_dep, dep_nom) %>%
      summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        DEPARTAMENTO_D = dep_nom,
        DEP_LABEL      = dep_nom # ✅ etiqueta tal cual base
      )
    
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
# Datos departamentales precomputados (para mapas)
# =========================================================
get_dep_data <- function(ind_code){
  switch(ind_code,
         "DEP_RATIO"       = pob_dep_all,
         "DEP_CRECIMIENTO" = pob_crecimiento_30,
         "ECV_FIES"        = ecv_fies_agg_dep,
         "BPAN"            = bpan_dep_all,
         "ETA"             = eta_dep_all,
         "NDA"             = nda_dep_all,
         "SISBEN"          = sisben_prev_long,
         "PRECIP"          = noaa_dep_all,
         "HANSEN"          = hansen_dep_all,
         "SIPSA_IHH"       = NULL,
         NULL)
}

all_years_dep <- sort(unique(c(
  if (!is.null(pob_dep_all))         pob_dep_all$ano,
  if (!is.null(pob_crecimiento_30))  pob_crecimiento_30$ano,
  if (!is.null(ecv_fies_agg_dep))    ecv_fies_agg_dep$ano,
  if (!is.null(bpan_dep_all))        bpan_dep_all$ano,
  if (!is.null(eta_dep_all))         eta_dep_all$ano,
  if (!is.null(nda_dep_all))         nda_dep_all$ano,
  if (!is.null(sisben_prev_long))    sisben_prev_long$ano,
  if (!is.null(sisben_hog_long))     sisben_hog_long$ano,
  if (!is.null(noaa_dep_all))        noaa_dep_all$ano,
  if (!is.null(hansen_dep_all))      hansen_dep_all$ano,
  if (length(sipsa_years))           sipsa_years
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
  
  output$estr_extra1 <- renderUI({
    base_sel <- input$estr_base1
    if (is.null(base_sel)) return(NULL)
    
    if (base_sel == "DNP_SISBEN") {
      
      tagList(
        div(
          lbl("¿Qué quiere ver en Sisbén?"),
          selectInput(
            "estr_sisben_view", label = NULL,
            choices = c(
              "Prevalencia (privaciones IPM)" = "prevalencia",
              "Número de hogares ponderados por grupo" = "hogares"
            ),
            selected = "prevalencia"
          )
        ),
        
        conditionalPanel(
          condition = "input.estr_sisben_view == 'prevalencia'",
          div(
            lbl("Tipología de privación"),
            selectInput(
              "estr_sisben_metric", label = NULL,
              choices = sisben_metric_choices,
              selected = unname(sisben_metric_choices)[1] %||% "i1"
            )
          )
        ),
        
        div(
          lbl("Grupo SISBÉN (A/B/C/D/TOTAL)"),
          selectInput(
            "estr_sisben_grupo", label = NULL,
            choices = sisben_group_choices,
            selected = "Todos"
          )
        )
      )
      
    } else if (base_sel == "DANE_ECV") {
      
      tagList(
        div(lbl("Indicador FIES-8"),
            selectInput("estr_ecv_fies_metric", label = NULL,
                        choices = inds_fies,
                        selected = unname(inds_fies)[1]))
      )
      
    } else if (base_sel == "DANE_SIPSA_ABAST_OD") {
      
      tagList(
        div(
          lbl("Rol del departamento"),
          selectInput(
            "estr_sipsa_dir", label = NULL,
            choices = c("Origen" = "Origen", "Destino" = "Destino"),
            selected = "Origen"
          )
        ),
        div(
          lbl("Grupo"),
          selectInput(
            "estr_sipsa_grupo", label = NULL,
            choices = sipsa_group_choices,
            selected = "Todos"
          )
        )
      )
      
    } else {
      NULL
    }
  })
  
  output$estr_ts <- renderPlotly({
    base_sel <- input$estr_base1 %||% default_base_estructura
    
    p <- if (base_sel == "DANE_POPULATION") {
      make_dep_ratio_ts_plot()
    } else if (base_sel == "DANE_POP_CRECIMIENTO") {
      make_pob_crecimiento_ts_plot()
    } else if (base_sel == "DANE_ECV") {
      make_ecv_fies_ts_plot(metric_sel = input$estr_ecv_fies_metric %||% unname(inds_fies)[1])
    } else if (base_sel == "DANE_SIPSA_ABAST_OD") {
      make_sipsa_ihh_ts_plot(
        dir   = input$estr_sipsa_dir %||% "Origen",
        grupo = input$estr_sipsa_grupo %||% "Todos"
      )
    } else if (base_sel == "INS_SIVIGILA_BPAN") {
      make_sivigila_ts_plot("BPAN")
    } else if (base_sel == "INS_SIVIGILA_ETA") {
      make_sivigila_ts_plot("ETA")
    } else if (base_sel == "INS_SIVIGILA_NDA") {
      make_sivigila_ts_plot("NDA")
    } else if (base_sel == "DNP_SISBEN") {
      
      view_sel  <- input$estr_sisben_view %||% "prevalencia"
      met_sel   <- input$estr_sisben_metric %||% "i1"
      grupo_sel <- input$estr_sisben_grupo %||% "Todos"
      
      make_sisben_ts_plot(
        view_sel   = view_sel,
        metric_sel = met_sel,
        grupo_sel  = grupo_sel
      )
      
    } else if (base_sel == "NOAA_PRECIPITACION") {
      make_noaa_ts_plot()
    } else if (base_sel == "HANSEN_DEFORESTATION") {
      make_hansen_ts_plot()
    } else {
      empty_ts_plot("Seleccione una base.")
    }
    
    p
  })
  
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
    
    if (base_sel == "DANE_SIPSA_ABAST_OD") {
      ts <- sipsa_ts_nacional(
        dir   = input$estr_sipsa_dir %||% "Origen",
        grupo = input$estr_sipsa_grupo %||% "Todos"
      )
      return(summary_from_ts(ts, "ano", "valor",
                             titulo = "SIPSA — IHH (0–10.000)",
                             value_type = "number"))
    }
    
    if (base_sel == "DANE_POP_CRECIMIENTO") {
      ts <- pob_crecimiento_ts
      return(summary_from_ts(ts, "ano", "valor",
                             titulo = "Crecimiento poblacional promedio anual (30 años)",
                             value_type = "percent"))
    }
    
    if (base_sel == "DANE_POPULATION") {
      ts <- pob_ts %>% mutate(valor = razon_dependencia_pct)
      return(summary_from_ts(ts, "ano", "valor",
                             titulo = "Razón de dependencia",
                             value_type = "percent"))
    }
    
    if (base_sel == "DANE_ECV") {
      met <- input$estr_ecv_fies_metric %||% unname(inds_fies)[1]
      ts <- ecv_fies_agg_nat %>% filter(metric == met) %>% arrange(ano)
      titulo <- names(inds_fies)[match(met, unname(inds_fies))] %||% "FIES-8"
      return(summary_from_ts(ts, "ano", "valor",
                             titulo = paste0("ECV — ", titulo),
                             value_type = "percent"))
    }
    
    if (base_sel == "INS_SIVIGILA_BPAN")
      return(summary_from_ts(bpan_ts, "ano", "valor", "SIVIGILA — Bajo peso al nacer (registros)", "number"))
    if (base_sel == "INS_SIVIGILA_NDA")
      return(summary_from_ts(nda_ts,  "ano", "valor", "SIVIGILA — Desnutrición aguda infantil (registros)", "number"))
    if (base_sel == "INS_SIVIGILA_ETA")
      return(summary_from_ts(eta_ts,  "ano", "valor", "SIVIGILA — ETA (total enfermos)", "number"))
    
    if (base_sel == "DNP_SISBEN") {
      
      view_sel  <- input$estr_sisben_view %||% "prevalencia"
      grupo_sel <- input$estr_sisben_grupo %||% "Todos"
      
      if (view_sel == "hogares") {
        
        if (is.null(sisben_hog_long) || !nrow(sisben_hog_long)) {
          return(list(
            story = "Sin datos Sisbén para hogares.",
            last_rows = list(list(k="Año", v=na_txt()), list(k="Hogares", v=na_txt())),
            growth_rows = list(list(k="Año anterior", v=na_txt()), list(k="Variación", v=na_txt()))
          ))
        }
        
        dd <- sisben_hog_long
        if (grupo_sel != "Todos") dd <- dd %>% filter(grupo == grupo_sel)
        
        if (grupo_sel == "Todos") {
          ts <- dd %>%
            group_by(ano) %>%
            summarise(valor = sum(hogares, na.rm = TRUE), .groups = "drop") %>%
            arrange(ano)
          titulo <- "Sisbén — Hogares ponderados (TOTAL)"
        } else {
          ts <- dd %>%
            filter(grupo == grupo_sel) %>%
            group_by(ano) %>%
            summarise(valor = sum(hogares, na.rm = TRUE), .groups = "drop") %>%
            arrange(ano)
          titulo <- paste0("Sisbén — Hogares ponderados (Grupo ", grupo_sel, ")")
        }
        
        return(summary_from_ts(
          ts, "ano", "valor",
          titulo = titulo,
          value_type = "number",
          unit = " hogares"
        ))
        
      } else {
        
        met <- input$estr_sisben_metric %||% "i1"
        nm  <- if (met %in% names(sisben_i_labels)) sisben_i_labels[[met]] else met
        
        dd <- sisben_prev_long %>% filter(metric == met)
        if (grupo_sel != "Todos") dd <- dd %>% filter(grupo == grupo_sel)
        
        if (grupo_sel == "Todos") {
          ts <- dd %>%
            group_by(ano) %>%
            summarise(
              denom = sum(denom, na.rm = TRUE),
              numer = sum(numer, na.rm = TRUE),
              valor = if_else(denom > 0, 100 * numer / denom, NA_real_),
              .groups = "drop"
            ) %>% arrange(ano)
          titulo <- paste0("Sisbén — ", nm, " (prevalencia, TOTAL)")
        } else {
          ts <- dd %>%
            filter(grupo == grupo_sel) %>%
            group_by(ano) %>%
            summarise(
              denom = sum(denom, na.rm = TRUE),
              numer = sum(numer, na.rm = TRUE),
              valor = if_else(denom > 0, 100 * numer / denom, NA_real_),
              .groups = "drop"
            ) %>% arrange(ano)
          titulo <- paste0("Sisbén — ", nm, " (prevalencia, Grupo ", grupo_sel, ")")
        }
        
        return(summary_from_ts(ts, "ano", "valor",
                               titulo = titulo,
                               value_type = "percent"))
      }
    }
    
    if (base_sel == "NOAA_PRECIPITACION") {
      return(summary_from_ts(noaa_ts, "ano", "valor",
                             titulo = "NOAA — Precipitación anual",
                             value_type = "number"))
    }
    
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
            lbl("¿Qué quiere ver en Sisbén?"),
            selectInput(
              paste0(prefix, "_sisben_view"), label = NULL,
              choices = c(
                "Prevalencia (privaciones IPM)" = "prevalencia",
                "Número de hogares ponderados por grupo" = "hogares"
              ),
              selected = "prevalencia"
            )
          ),
          conditionalPanel(
            condition = paste0("input.", prefix, "_sisben_view == 'prevalencia'"),
            div(
              lbl("Tipología de privación"),
              selectInput(paste0(prefix, "_sisben_metric"), label = NULL,
                          choices = sisben_metric_choices,
                          selected = unname(sisben_metric_choices)[1] %||% "i1")
            )
          ),
          div(
            lbl("Grupo (A/B/C/D/TOTAL)"),
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
      } else if (base_sel == "DANE_SIPSA_ABAST_OD") {
        tagList(
          div(
            lbl("Rol del departamento"),
            selectInput(
              paste0(prefix, "_sipsa_dir"), label = NULL,
              choices = c("Origen" = "Origen", "Destino" = "Destino"),
              selected = "Origen"
            )
          ),
          div(
            lbl("Grupo"),
            selectInput(
              paste0(prefix, "_sipsa_grupo"), label = NULL,
              choices = sipsa_group_choices,
              selected = "Todos"
            )
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
    
    if (ind_sel == "SISBEN") {
      view_sel <- input$map1_sisben_view %||% "prevalencia"
      df <- if (view_sel == "hogares") sisben_hog_long else sisben_prev_long
      yrs <- sort(unique(df$ano))
      if (!length(yrs)) return(tags$span("Sin años disponibles para esta base", style="font-size:12px;color:#9ca3af;"))
      selectInput("map1_year", label = NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
    } else if (ind_sel == "SIPSA_IHH") {
      yrs <- sipsa_years
      if (!length(yrs)) return(tags$span("Sin años disponibles para SIPSA", style="font-size:12px;color:#9ca3af;"))
      selectInput("map1_year", label = NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
    } else {
      df <- get_dep_data(ind_sel)
      if (is.null(df) || !nrow(df)) return(tags$span("Sin años disponibles para esta base", style="font-size:12px;color:#9ca3af;"))
      yrs <- sort(unique(df$ano))
      selectInput("map1_year", label = NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
    }
  })
  
  output$map2_year_ui <- renderUI({
    lock <- input$lock_years %||% FALSE
    if (isTRUE(lock)) return(tags$span("Año controlado por el filtro global.", style = "font-size:12px;color:#6b7280;"))
    
    base_sel <- input$estr_base2_map2 %||% default_base_estructura
    ind_sel  <- base_to_indicator_code(base_sel)
    
    if (ind_sel == "SISBEN") {
      view_sel <- input$map2_sisben_view %||% "prevalencia"
      df <- if (view_sel == "hogares") sisben_hog_long else sisben_prev_long
      yrs <- sort(unique(df$ano))
      if (!length(yrs)) return(tags$span("Sin años disponibles para esta base", style="font-size:12px;color:#9ca3af;"))
      selectInput("map2_year", label = NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
    } else if (ind_sel == "SIPSA_IHH") {
      yrs <- sipsa_years
      if (!length(yrs)) return(tags$span("Sin años disponibles para SIPSA", style="font-size:12px;color:#9ca3af;"))
      selectInput("map2_year", label = NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
    } else {
      df <- get_dep_data(ind_sel)
      if (is.null(df) || !nrow(df)) return(tags$span("Sin años disponibles para esta base", style="font-size:12px;color:#9ca3af;"))
      yrs <- sort(unique(df$ano))
      selectInput("map2_year", label = NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
    }
  })
  
  build_map_sf <- function(base_sel, year_val, prefix){
    ind_sel <- base_to_indicator_code(base_sel)
    if (is.null(year_val) || is.null(shp_dep)) return(NULL)
    
    if (ind_sel == "SISBEN") {
      view_sel <- input[[paste0(prefix, "_sisben_view")]] %||% "prevalencia"
      
      if (view_sel == "hogares") {
        if (is.null(sisben_hog_long) || !nrow(sisben_hog_long)) return(NULL)
        
        dd <- sisben_hog_long %>% filter(ano == as.integer(year_val))
        grp <- input[[paste0(prefix, "_sisben_grupo")]] %||% "Todos"
        if (grp != "Todos") dd <- dd %>% filter(grupo == grp)
        if (!nrow(dd)) return(NULL)
        
        dd <- dd %>%
          group_by(cod_dep, dep_nom) %>%
          summarise(valor = sum(hogares, na.rm = TRUE), .groups = "drop") %>%
          mutate(
            DEPARTAMENTO_D = dep_nom,
            DEP_LABEL      = dep_nom
          )
        
      } else {
        if (is.null(sisben_prev_long) || !nrow(sisben_prev_long)) return(NULL)
        
        dd <- sisben_prev_long %>% filter(ano == as.integer(year_val))
        if (!nrow(dd)) return(NULL)
        
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
          mutate(
            DEPARTAMENTO_D = dep_nom,
            DEP_LABEL      = dep_nom
          )
      }
      
    } else if (ind_sel == "ECV_FIES") {
      if (is.null(ecv_fies_agg_dep) || !nrow(ecv_fies_agg_dep)) return(NULL)
      
      dd <- ecv_fies_agg_dep %>% filter(ano == as.integer(year_val))
      if (!nrow(dd)) return(NULL)
      
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
        mutate(
          DEPARTAMENTO_D = dep_nom,
          DEP_LABEL      = dep_nom
        )
      
    } else if (ind_sel == "DEP_CRECIMIENTO") {
      if (is.null(pob_crecimiento_30) || !nrow(pob_crecimiento_30)) return(NULL)
      dd <- pob_crecimiento_30 %>% filter(ano == as.integer(year_val)) %>%
        mutate(
          DEPARTAMENTO_D = dep_nom,
          DEP_LABEL      = dep_nom
        )
      
    } else if (ind_sel == "SIPSA_IHH") {
      dir   <- input[[paste0(prefix, "_sipsa_dir")]]   %||% "Origen"
      grupo <- input[[paste0(prefix, "_sipsa_grupo")]] %||% "Todos"
      dd <- sipsa_map_ihh_by_dep(year_val = year_val, dir = dir, grupo = grupo)
      if (is.null(dd) || !nrow(dd)) return(NULL)
      
    } else {
      df <- get_dep_data(ind_sel)
      if (is.null(df) || !nrow(df)) return(NULL)
      
      dd <- df %>% filter(ano == as.integer(year_val))
      if (!nrow(dd)) return(NULL)
      
      dd <- dd %>%
        group_by(cod_dep, dep_nom) %>%
        summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          DEPARTAMENTO_D = dep_nom,
          DEP_LABEL      = dep_nom
        )
    }
    
    if (is.null(dd) || !nrow(dd)) return(NULL)
    
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
  
  # ✅ label_mode: "asis" (usar tal cual base) o "titlecase" (solo para SIPSA)
  update_dep_leaflet <- function(map_id, md, palette_name, legend_title,
                                 value_type = c("percent","number"),
                                 unit_suffix = "",
                                 label_mode = c("asis","titlecase")){
    value_type <- match.arg(value_type)
    label_mode <- match.arg(label_mode)
    
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
    
    # ✅ Prioridad: DEP_LABEL (ya es el nombre de la base), luego DEPARTAMENTO_D, luego DEPARTAMENTO_O
    dep_name <- if ("DEP_LABEL" %in% names(md)) as.character(md$DEP_LABEL) else NA_character_
    if (all(is.na(dep_name) | !nzchar(dep_name))) {
      dep_name <- if ("DEPARTAMENTO_D" %in% names(md)) as.character(md$DEPARTAMENTO_D) else NA_character_
    }
    if (all(is.na(dep_name) | !nzchar(dep_name))) {
      dep_name <- if ("DEPARTAMENTO_O" %in% names(md)) as.character(md$DEPARTAMENTO_O) else NA_character_
    }
    if (all(is.na(dep_name) | !nzchar(dep_name))) {
      dep_name <- if ("join_name" %in% names(md)) as.character(md$join_name) else md$join_code
    }
    
    dep_name <- trim_safe(dep_name)
    
    # ✅ SIPSA (pedido HOY): MAYÚSCULAS en etiquetas
    # (solo se aplica cuando label_mode == "titlecase", que es la excepción usada por SIPSA)
    if (label_mode == "titlecase") {
      dep_name <- norm_txt_upper(dep_name)
    }
    
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
    
    tipo_val <- "number"
    legend_title <- "Cantidad"
    label_mode <- "asis"
    
    if (ind_sel == "DEP_CRECIMIENTO") {
      tipo_val <- "percent"
      legend_title <- "Crecimiento anual (%)"
    } else if (ind_sel == "SIPSA_IHH") {
      tipo_val <- "number"
      dir   <- input$map1_sipsa_dir %||% "Origen"
      grp   <- input$map1_sipsa_grupo %||% "Todos"
      legend_title <- paste0("IHH (", dir, if (grp != "Todos") paste0(" / ", grp) else "", ")")
      label_mode <- "titlecase"  # ✅ excepción SIPSA (AHORA: MAYÚSCULAS)
    } else if (ind_sel == "SISBEN") {
      view_sel  <- input$map1_sisben_view %||% "prevalencia"
      grupo_sel <- input$map1_sisben_grupo %||% "Todos"
      
      tipo_val <- if (view_sel == "prevalencia") "percent" else "number"
      
      if (view_sel == "prevalencia") {
        legend_title <- if (grupo_sel == "Todos") "Prevalencia (%)" else paste0("Prevalencia - Grupo ", grupo_sel, " (%)")
      } else {
        legend_title <- if (grupo_sel == "Todos") "Hogares (Total)" else paste0("Hogares - Grupo ", grupo_sel)
      }
    } else if (ind_sel %in% c("DEP_RATIO","ECV_FIES")) {
      tipo_val <- "percent"
      legend_title <- "Porcentaje"
    }
    
    update_dep_leaflet("map_1", md, palette_name = "Greens",
                       legend_title = legend_title,
                       value_type = tipo_val, unit_suffix = "",
                       label_mode = label_mode)
  })
  
  observe({
    base_sel <- input$estr_base2_map2 %||% default_base_estructura
    ind_sel  <- base_to_indicator_code(base_sel)
    md       <- map2_data()
    
    tipo_val <- "number"
    legend_title <- "Cantidad"
    label_mode <- "asis"
    
    if (ind_sel == "DEP_CRECIMIENTO") {
      tipo_val <- "percent"
      legend_title <- "Crecimiento anual (%)"
    } else if (ind_sel == "SIPSA_IHH") {
      tipo_val <- "number"
      dir   <- input$map2_sipsa_dir %||% "Origen"
      grp   <- input$map2_sipsa_grupo %||% "Todos"
      legend_title <- paste0("IHH (", dir, if (grp != "Todos") paste0(" / ", grp) else "", ")")
      label_mode <- "titlecase"  # ✅ excepción SIPSA (AHORA: MAYÚSCULAS)
    } else if (ind_sel == "SISBEN") {
      view_sel  <- input$map2_sisben_view %||% "prevalencia"
      grupo_sel <- input$map2_sisben_grupo %||% "Todos"
      
      tipo_val <- if (view_sel == "prevalencia") "percent" else "number"
      
      if (view_sel == "prevalencia") {
        legend_title <- if (grupo_sel == "Todos") "Prevalencia (%)" else paste0("Prevalencia - Grupo ", grupo_sel, " (%)")
      } else {
        legend_title <- if (grupo_sel == "Todos") "Hogares (Total)" else paste0("Hogares - Grupo ", grupo_sel)
      }
    } else if (ind_sel %in% c("DEP_RATIO","ECV_FIES")) {
      tipo_val <- "percent"
      legend_title <- "Porcentaje"
    }
    
    update_dep_leaflet("map_2", md, palette_name = "Blues",
                       legend_title = legend_title,
                       value_type = tipo_val, unit_suffix = "",
                       label_mode = label_mode)
  })
}

shinyApp(ui, server)


