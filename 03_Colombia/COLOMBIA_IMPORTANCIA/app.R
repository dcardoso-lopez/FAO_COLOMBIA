# app_sninny_EVA_y_FINAGRO_con_selector_base.R
# -------------------------------------------------------------------
# Incluye:
# - UPRA_EVA_A (sistema existente)
# - FINAGRO_CFA_FAST (nuevo sistema con montos y créditos)
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

# ---------------- Rutas ----------------
data_dir <- "data"

# =========================================================
# ALTURAS (plots/mapas)
# =========================================================
H_PLOT <- 650
H_MAP  <- 600

# =========================================================
# Colores globales UI + plots
# =========================================================
COL_UI   <- "#a1d99b"
COL_PLOT <- "#008f38"

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

# Función para normalizar códigos de departamento a 2 dígitos
norm_dep2 <- function(x){
  x <- gsub("\\D","",as.character(x))
  x[nchar(x)==0] <- NA
  stringi::stri_pad_left(x,2,"0")
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

fmt_num_es <- function(x, digits = 1){
  scales::number(x, accuracy = 10^-digits, big.mark = ".", decimal.mark = ",")
}

fmt_km_any <- function(x, digits = 1){
  vapply(x, function(v){
    if (is.na(v) || !is.finite(v)) return(NA_character_)
    av <- abs(v)
    if (av >= 1e6) paste0(fmt_num_es(v/1e6, digits), "M")
    else if (av >= 1e3) paste0(fmt_num_es(v/1e3, digits), "K")
    else fmt_num_es(v, digits)
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

year_ticks_2 <- function(years){
  yrs <- sort(unique(as.integer(years[is.finite(years)])))
  if (!length(yrs)) return(NULL)
  y_min <- min(yrs); y_max <- max(yrs)
  tick0 <- floor(y_min/2)*2
  ticks <- seq(tick0, y_max, by = 2)
  if (tail(ticks, 1) != y_max) ticks <- c(ticks, y_max)
  ticks
}

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

lbl <- function(x) div(class = "filter-label", x)

# =========================================================
# ✅ Helpers para LEYENDA tipo "A–B" y desde el 2do ">A–B"
# + formato CO con K/M a 1 decimal
# =========================================================
make_bin_labels_co <- function(bins){
  bins <- sort(unique(as.numeric(bins)))
  if (length(bins) < 2) return(character(0))
  
  vapply(seq_len(length(bins) - 1), function(i){
    a <- bins[i]; b <- bins[i + 1]
    if (i == 1) paste0(fmt_short(a), "–", fmt_short(b))
    else        paste0(">", fmt_short(a), "–", fmt_short(b))
  }, FUN.VALUE = character(1))
}

# =========================================================
# SELECTOR BASE (EVA + FINAGRO)
# =========================================================
base_choices <- c(
  "Condiciones productivas de los productos agroindustriales" = "UPRA_EVA_A",
  "Financiamiento rural y agropecuario" = "FINAGRO_CFA_FAST"
)
default_base <- unname(base_choices[1])

# =========================================================
# EVA-A — Carga + filtro SOLO cultivos objetivo
# =========================================================
eva_metric_choices <- c(
  "Área sembrada (hectáreas)"  = "area_sembrada_ha",
  "Área cosechada (hectáreas)" = "area_cosechada_ha",
  "Producción (toneladas)"     = "produccion_t"
)

cultivos_objetivo <- c(
  "Cacao", "Café", "Caña de Azúcar", "Fique", "Iraca", "Olivo",
  "Otras oleaginosas",
  "Otros cultivos tropicales tradicionales",
  "Palma de aceite",
  "Sacha inchi"
)

norm_txt <- function(x){
  x <- stringi::stri_trim_both(as.character(x))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  tolower(x)
}

eva_raw <- tryCatch({
  p <- file.path(data_dir, "011_UPRA_EVA-A.rds")
  if (!file.exists(p)) stop("No existe: 011_UPRA_EVA-A.rds")
  df <- readRDS(p)
  if (is.null(df) || !nrow(df)) stop("EVA-A vacío")
  
  nms <- names(df); nms_l <- tolower(nms)
  col_ano <- pick_year_col(nms)
  col_dep <- pick_dep_col(nms)
  if ("cultivo" %in% nms_l) col_cult <- nms[which(nms_l == "cultivo")[1]] else stop("EVA-A: no existe columna 'cultivo'.")
  col_a_s  <- pick_col_simple(nms, c("area_sembrada_ha","área_sembrada_ha","ha_sembrada","area_sembrada"))
  col_a_c  <- pick_col_simple(nms, c("area_cosechada_ha","área_cosechada_ha","ha_cosechada","area_cosechada"))
  col_prod <- pick_col_simple(nms, c("produccion_t","producción_t","produccion","producción","toneladas","t","produccion_ton"))
  if (any(is.na(c(col_ano, col_dep, col_a_s, col_a_c, col_prod)))) stop("EVA-A: faltan columnas clave (año/depto/áreas/producción).")
  
  tibble(
    ano               = as.integer(df[[col_ano]]),
    cod_dep           = stringi::stri_trim_both(as.character(df[[col_dep]])),
    cultivo           = stringi::stri_trim_both(as.character(df[[col_cult]])),
    area_sembrada_ha  = parse_num_co(df[[col_a_s]]),
    area_cosechada_ha = parse_num_co(df[[col_a_c]]),
    produccion_t      = parse_num_co(df[[col_prod]])
  ) %>%
    filter(!is.na(ano), !is.na(cod_dep), !is.na(cultivo), cultivo != "") %>%
    mutate(.cultivo_norm = norm_txt(cultivo)) %>%
    filter(.cultivo_norm %in% norm_txt(cultivos_objetivo)) %>%
    select(-.cultivo_norm)
  
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

# =========================================================
# EVA nacional (serie dual)
# =========================================================
make_eva_ts_plot_dual <- function(cultivo_sel = "__ALL__"){
  if (is.null(eva_raw) || !nrow(eva_raw)) return(empty_ts_plot("Sin datos de EVA-A disponibles."))
  
  df <- eva_raw
  if (!is.null(cultivo_sel) && cultivo_sel != "__ALL__") df <- df %>% filter(cultivo == cultivo_sel)
  if (!nrow(df)) return(empty_ts_plot("No hay datos para el cultivo seleccionado."))
  
  ts <- df %>%
    group_by(ano) %>%
    summarise(
      area_sembrada_ha  = sum(area_sembrada_ha,  na.rm = TRUE),
      area_cosechada_ha = sum(area_cosechada_ha, na.rm = TRUE),
      produccion_t      = sum(produccion_t,      na.rm = TRUE),
      .groups = "drop"
    ) %>% arrange(ano)
  
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
      yaxis = list(title = "Hectáreas", showgrid = FALSE, automargin = TRUE),
      yaxis2 = list(
        title      = "Producción (toneladas)",
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
# EVA departamental (para mapas)
# =========================================================
eva_dep_all <- tryCatch({
  if (is.null(eva_raw) || !nrow(eva_raw)) return(NULL)
  eva_raw %>%
    group_by(ano, cod_dep, cultivo) %>%
    summarise(
      area_sembrada_ha  = sum(area_sembrada_ha,  na.rm = TRUE),
      area_cosechada_ha = sum(area_cosechada_ha, na.rm = TRUE),
      produccion_t      = sum(produccion_t,      na.rm = TRUE),
      .groups = "drop"
    )
}, error = function(e) NULL)

# =========================================================
# FINAGRO — 081_FINAGRO_CFA
# =========================================================
finagro_raw <- tryCatch({
  p <- file.path(data_dir, "081_FINAGRO_CFA.rds")
  if (!file.exists(p)) return(NULL)
  df <- readRDS(p)
  if (is.null(df) || !nrow(df)) return(NULL)
  df
}, error = function(e) NULL)

# Tabla de IPP para ajuste del valor del crédito
ipp_tbl <- data.frame(
  ano = c(2010:2025),
  IPP = c(
    0.8851, 0.9633, 0.9625, 0.9502, 0.9778, 1.0158, 1.0707, 1.0801,
    1.1356, 1.1848, 1.1758, 1.3756, 1.7822, 1.7923, 1.7989, 1.8585
  )
)

# Procesar datos FINAGRO
finagro_df <- NULL
finagro_choices <- list(
  tipo    = c("Todos"),
  sexo    = c("Todos"),
  linea   = c("Todos"),
  eslabon = c("Todos")
)

if (!is.null(finagro_raw)) {
  nms <- names(finagro_raw)
  
  col_ano   <- pick_year_col(nms)
  col_mes   <- pick_col_simple(nms, c("mes","MES","month"))
  col_dep   <- pick_col_simple(nms, c("COD_DANE_DPTO_D","cod_dane_dpto_d","COD_DPTO2","cod_dpto2","cod_dpto","cod_depto"))
  col_depnm <- pick_col_simple(nms, c("DEPARTAMENTO_D","departamento_d","DEPARTAMENTO","departamento","NOM_DPTO","nom_dpto"))
  
  col_val   <- pick_col_simple(nms, c("VALOR_CREDITO","valor_credito","VALOR_CREDITO_REAL","valor_credito_real"))
  col_num   <- pick_col_simple(nms, c("NUMERO_CREDITO","numero_credito","N_CREDITOS","creditos","CREDITOS"))
  
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
    
    finagro_choices$tipo    <- c("Todos", sort(unique(na.omit(finagro_df$TIPO_PERSONA))))
    finagro_choices$sexo    <- c("Todos", sort(unique(na.omit(finagro_df$SEXO2))))
    finagro_choices$linea   <- c("Todos", sort(unique(na.omit(finagro_df$LINEA_CREDITO))))
    finagro_choices$eslabon <- c("Todos", sort(unique(na.omit(finagro_df$ESLABON_CADENA))))
  }
}

# Funciones para aplicar filtros FINAGRO
finagro_apply_filters <- function(df, tipo, sexo, linea, eslabon){
  if (is.null(df) || !nrow(df)) return(df)
  if (!is.null(tipo)   && tipo   != "Todos") df <- df %>% filter(TIPO_PERSONA == tipo)
  if (!is.null(sexo)   && sexo   != "Todos") df <- df %>% filter(SEXO2 == sexo)
  if (!is.null(linea)  && linea  != "Todos") df <- df %>% filter(LINEA_CREDITO == linea)
  if (!is.null(eslabon)&& eslabon!= "Todos") df <- df %>% filter(ESLABON_CADENA == eslabon)
  df
}

# Función para crear gráfica dual de FINAGRO
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
  
  plotly::plot_ly(ts, x = ~ano) %>%
    plotly::add_trace(
      y = ~monto_real,
      type = "scatter", mode = "lines+markers",
      name = "Monto real (Mil M)",
      yaxis = "y1",
      line = list(width = 2),
      marker = list(size = 6),
      hovertemplate = "<b>Año:</b> %{x}<br><b>Monto real:</b> %{y:.2f} Mil M<extra></extra>"
    ) %>%
    plotly::add_trace(
      y = ~creditos,
      type = "scatter", mode = "lines+markers",
      name = "Número de créditos",
      yaxis = "y2",
      line = list(width = 2, dash = "dot"),
      marker = list(size = 6),
      hovertemplate = "<b>Año:</b> %{x}<br><b> Número de operaciones:</b> %{y:,}<extra></extra>"
    ) %>%
    plotly::layout(
      title = NULL,
      xaxis = list(
        title = "", tickmode = "array", tickvals = ticks_x, ticktext = ticks_x,
        showgrid = FALSE, automargin = TRUE
      ),
      yaxis = list(title = "Monto real (Miles de millones)", showgrid = FALSE, automargin = TRUE, rangemode = "tozero"),
      yaxis2 = list(title = "Número de operaciones ", overlaying = "y", side = "right", showgrid = FALSE, automargin = TRUE, rangemode = "tozero"),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25, yanchor = "top"),
      margin = list(l = 70, r = 70, t = 10, b = 110),
      hovermode = "x unified",
      hoverlabel = PLOTLY_HOVERLABEL,
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

# Datos departamentales para FINAGRO (para mapas)
finagro_dep_all <- tryCatch({
  if (is.null(finagro_df) || !nrow(finagro_df)) return(NULL)
  
  finagro_df %>%
    filter(!is.na(COD_DPTO2), nzchar(COD_DPTO2)) %>%
    group_by(ano, cod_dep = COD_DPTO2, dep_nom = DEPARTAMENTO_D) %>%
    summarise(
      monto_real = sum(VALOR_CREDITO_REAL, na.rm = TRUE),
      creditos   = sum(NUMERO_CREDITO, na.rm = TRUE),
      .groups = "drop"
    )
}, error = function(e) NULL)

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
# UI
# =========================================================
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
    .filters .form-select:focus{ border-color:var(--ecv-bdr) !important; box-shadow:0 0 0 0.15rem rgba(161,217,155,.55) !important; }

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
    .kpi-row{ display:flex; justify-content:space-between; align-items:baseline; border-top:1px dashed rgba(161,217,155,.55); padding-top:6px; }
    .kpi-row:first-child{ border-top:none; padding-top:0; }
    .kpi-k{ color:#374151; font-size:12px; }
    .kpi-v{ color:#111827; font-size:12px; font-weight:700; }

    .grid-2maps{ display:grid; grid-template-columns:1fr 1fr; gap:20px; align-items:stretch; }
    .map-panel{ display:flex; flex-direction:column; gap:10px; background:#fff; border:1px solid var(--ecv-bdr); border-radius:16px; padding:16px 16px 18px; box-shadow:0 2px 6px rgba(0,0,0,.03); }
    .map-panel .filters{ border:none; box-shadow:none; padding:0; margin:0 0 8px 0; }

    /* ✅ elimina el espacio gigante arriba del mapa */
    .filters-map{ height:auto !important; min-height:unset !important; overflow:visible; }

    .eva-dt-filters{
      display:grid; grid-template-columns:1fr 1fr; gap:10px;
      background:#fff; border:1px solid rgba(161,217,155,.70);
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
      tabsetPanel(
        id = "tabs_eva",
        
        tabPanel(
          title = "Indicadores a nivel nacional",
          div(
            class = "filters",
            div(
              class = "filters-grid-1",
              div(
                lbl("¿Cuál información de la dimensión de importancia quiere analizar?"),
                selectInput("base_ts", label = NULL, choices = base_choices, selected = default_base)
              ),
              uiOutput("extra_ts")
            )
          ),
          div(
            class = "ts-grid",
            div(
              class = "ts-left",
              plotlyOutput("eva_ts_plot", height = paste0(H_PLOT, "px")),
              uiOutput("data_table_filters_ui"),
              DTOutput("data_table")
            ),
            div(class = "ts-right", uiOutput("summary_cards"))
          )
        ),
        
        tabPanel(
          title = "Comparativos departamentales",
          div(
            class = "filters",
            div(
              class = "filters-grid-2",
              div(lbl("¿Usar un año fijo para ambos mapas?"),
                  checkboxInput("lock_years", label = NULL, value = FALSE)),
              div(lbl("Año fijo (cuando está activado)"),
                  uiOutput("fixed_year_ui"))
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
                    lbl("¿Cuál información de la dimensión de importancia quiere analizar?"),
                    selectInput("map1_base", label = NULL, choices = base_choices, selected = default_base)
                  ),
                  uiOutput("map1_extra"),
                  div(class = "filters-grid-2",
                      div(lbl("¿Qué año analizamos?"), uiOutput("map1_year_ui")))
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
                  div(
                    lbl("¿Qué base analizamos?"),
                    selectInput("map2_base", label = NULL, choices = base_choices, selected = default_base)
                  ),
                  uiOutput("map2_extra"),
                  div(class = "filters-grid-2",
                      div(lbl("¿Qué año analizamos?"), uiOutput("map2_year_ui")))
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
  
  # ----- Extra UI Pestaña 1 (series temporales) -----
  output$extra_ts <- renderUI({
    base_sel <- input$base_ts %||% default_base
    
    if (base_sel == "UPRA_EVA_A") {
      div(
        lbl("Cultivo"),
        selectInput("eva_cultivo_ts", label = NULL, choices = eva_cultivo_choices, selected = "__ALL__")
      )
    } else if (base_sel == "FINAGRO_CFA_FAST") {
      tagList(
        div(lbl("Tipo de productor"),
            selectInput("fin_tipo", NULL, choices = finagro_choices$tipo, selected = "Todos")),
        div(lbl("Sexo"),
            selectInput("fin_sexo", NULL, choices = finagro_choices$sexo, selected = "Todos")),
        div(lbl("Línea de crédito"),
            selectInput("fin_linea", NULL, choices = finagro_choices$linea, selected = "Todos")),
        div(lbl("Eslabón de la cadena"),
            selectInput("fin_eslabon", NULL, choices = finagro_choices$eslabon, selected = "Todos"))
      )
    } else {
      NULL
    }
  })
  
  # ----- Plot principal (serie temporal) -----
  output$eva_ts_plot <- renderPlotly({
    base_sel <- input$base_ts %||% default_base
    
    p <- if (base_sel == "UPRA_EVA_A") {
      make_eva_ts_plot_dual(input$eva_cultivo_ts %||% "__ALL__")
    } else if (base_sel == "FINAGRO_CFA_FAST") {
      df <- finagro_apply_filters(
        finagro_df,
        tipo    = input$fin_tipo %||% "Todos",
        sexo    = input$fin_sexo %||% "Todos",
        linea   = input$fin_linea %||% "Todos",
        eslabon = input$fin_eslabon %||% "Todos"
      )
      make_finagro_dual_ts_plot(df)
    } else {
      empty_ts_plot("Base no implementada.")
    }
    
    force_plot_color(p, COL_PLOT)
  })
  
  # ----- Filtros para tabla de datos -----
  output$data_table_filters_ui <- renderUI({
    base_sel <- input$base_ts %||% default_base
    
    if (base_sel == "UPRA_EVA_A") {
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
    } else if (base_sel == "FINAGRO_CFA_FAST") {
      if (is.null(finagro_df) || !nrow(finagro_df)) return(NULL)
      yrs <- sort(unique(finagro_df$ano))
      tagList(
        div(
          class = "eva-dt-filters",
          div(
            lbl("Año (tabla)"),
            selectInput("fin_dt_year", label = NULL,
                        choices = c("Todos los años"="__ALL__", stats::setNames(yrs, yrs)),
                        selected = "__ALL__")
          ),
          div(
            lbl("Agrupación"),
            selectInput("fin_dt_group", label = NULL,
                        choices = c("Por departamento", "Por tipo de productor", "Por sexo"),
                        selected = "Por departamento")
          )
        )
      )
    } else {
      NULL
    }
  })
  
  # ----- Datos para tabla -----
  data_table_data <- reactive({
    base_sel <- input$base_ts %||% default_base
    
    if (base_sel == "UPRA_EVA_A") {
      if (is.null(eva_raw) || !nrow(eva_raw)) return(NULL)
      df <- eva_raw
      yr_sel  <- input$eva_dt_year %||% "__ALL__"
      cul_sel <- input$eva_dt_cultivo %||% "__ALL__"
      if (!is.null(yr_sel) && yr_sel != "__ALL__") df <- df %>% filter(ano == as.integer(yr_sel))
      if (!is.null(cul_sel) && cul_sel != "__ALL__") df <- df %>% filter(cultivo == cul_sel)
      if (!nrow(df)) return(NULL)
      
      df %>%
        group_by(ano, cultivo) %>%
        summarise(
          area_sembrada_ha  = sum(area_sembrada_ha,  na.rm = TRUE),
          area_cosechada_ha = sum(area_cosechada_ha, na.rm = TRUE),
          produccion_t      = sum(produccion_t,      na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(ano), desc(produccion_t))
      
    } else if (base_sel == "FINAGRO_CFA_FAST") {
      if (is.null(finagro_df) || !nrow(finagro_df)) return(NULL)
      
      df <- finagro_apply_filters(
        finagro_df,
        tipo    = input$fin_tipo %||% "Todos",
        sexo    = input$fin_sexo %||% "Todos",
        linea   = input$fin_linea %||% "Todos",
        eslabon = input$fin_eslabon %||% "Todos"
      )
      
      yr_sel <- input$fin_dt_year %||% "__ALL__"
      if (!is.null(yr_sel) && yr_sel != "__ALL__") df <- df %>% filter(ano == as.integer(yr_sel))
      
      group_sel <- input$fin_dt_group %||% "Por departamento"
      
      if (group_sel == "Por departamento") {
        df <- df %>%
          filter(!is.na(COD_DPTO2), nzchar(COD_DPTO2)) %>%
          group_by(ano, cod_dep = COD_DPTO2, dep_nom = DEPARTAMENTO_D) %>%
          summarise(
            monto_real = sum(VALOR_CREDITO_REAL, na.rm = TRUE) / 1e9,
            creditos   = sum(NUMERO_CREDITO, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(desc(ano), desc(monto_real))
      } else if (group_sel == "Por tipo de productor") {
        df <- df %>%
          filter(!is.na(TIPO_PERSONA), nzchar(TIPO_PERSONA)) %>%
          group_by(ano, tipo = TIPO_PERSONA) %>%
          summarise(
            monto_real = sum(VALOR_CREDITO_REAL, na.rm = TRUE) / 1e9,
            creditos   = sum(NUMERO_CREDITO, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(desc(ano), desc(monto_real))
      } else if (group_sel == "Por sexo") {
        df <- df %>%
          filter(!is.na(SEXO2), nzchar(SEXO2)) %>%
          group_by(ano, sexo = SEXO2) %>%
          summarise(
            monto_real = sum(VALOR_CREDITO_REAL, na.rm = TRUE) / 1e9,
            creditos   = sum(NUMERO_CREDITO, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(desc(ano), desc(monto_real))
      }
      
      df
    } else {
      NULL
    }
  })
  
  # ----- Renderizar tabla de datos -----
  output$data_table <- DT::renderDT({
    base_sel <- input$base_ts %||% default_base
    df <- data_table_data()
    
    if (is.null(df) || !nrow(df)) {
      return(DT::datatable(data.frame(Mensaje = "Sin datos para los filtros seleccionados."),
                           rownames = FALSE))
    }
    
    if (base_sel == "UPRA_EVA_A") {
      out <- df %>%
        transmute(
          Año = ano,
          Cultivo = cultivo,
          `Área sembrada (ha)`  = fmt_short(area_sembrada_ha),
          `Área cosechada (ha)` = fmt_short(area_cosechada_ha),
          `Producción (t)`      = fmt_short(produccion_t)
        )
    } else if (base_sel == "FINAGRO_CFA_FAST") {
      group_sel <- input$fin_dt_group %||% "Por departamento"
      
      if (group_sel == "Por departamento") {
        out <- df %>%
          transmute(
            Año = ano,
            Departamento = dep_nom,
            `Código` = cod_dep,
            `Monto real (Mil M)` = sprintf("%.2f", monto_real),
            `Número de créditos` = fmt_short(creditos)
          )
      } else if (group_sel == "Por tipo de productor") {
        out <- df %>%
          transmute(
            Año = ano,
            `Tipo de productor` = tipo,
            `Monto real (Mil M)` = sprintf("%.2f", monto_real),
            `Número de créditos` = fmt_short(creditos)
          )
      } else if (group_sel == "Por sexo") {
        out <- df %>%
          transmute(
            Año = ano,
            Sexo = sexo,
            `Monto real (Mil M)` = sprintf("%.2f", monto_real),
            `Número de créditos` = fmt_short(creditos)
          )
      }
    } else {
      out <- df
    }
    
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
  
  # ----- Resumen para tarjetas -----
  current_summary <- reactive({
    base_sel <- input$base_ts %||% default_base
    
    if (base_sel == "UPRA_EVA_A") {
      out <- list(
        story = "No se dispone de información suficiente para construir la síntesis con los filtros actuales.",
        last_rows = list(list(k="Año", v=na_txt()), list(k="Producción", v=na_txt())),
        growth_rows = list(list(k="Año anterior", v=na_txt()), list(k="Crec. producción", v=na_txt()))
      )
      if (is.null(eva_raw) || !nrow(eva_raw)) return(out)
      
      cultivo_sel <- input$eva_cultivo_ts %||% "__ALL__"
      df <- eva_raw
      if (cultivo_sel != "__ALL__") df <- df %>% filter(cultivo == cultivo_sel)
      if (!nrow(df)) return(out)
      
      ts <- df %>%
        group_by(ano) %>%
        summarise(
          area_sembrada_ha  = sum(area_sembrada_ha,  na.rm = TRUE),
          area_cosechada_ha = sum(area_cosechada_ha, na.rm = TRUE),
          produccion_t      = sum(produccion_t,      na.rm = TRUE),
          .groups = "drop"
        ) %>% arrange(ano)
      
      last_year <- max(ts$ano, na.rm = TRUE)
      last <- ts %>% filter(ano == last_year) %>% slice(1)
      prev <- ts %>% filter(ano == (last_year - 1)) %>% slice(1)
      
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
      
    } else if (base_sel == "FINAGRO_CFA_FAST") {
      df <- finagro_apply_filters(
        finagro_df,
        tipo    = input$fin_tipo %||% "Todos",
        sexo    = input$fin_sexo %||% "Todos",
        linea   = input$fin_linea %||% "Todos",
        eslabon = input$fin_eslabon %||% "Todos"
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
    
    list(
      story = "Base no implementada.",
      last_rows = list(list(k="", v=na_txt())),
      growth_rows = list(list(k="", v=na_txt()))
    )
  })
  
  # ----- Tarjetas de resumen -----
  output$summary_cards <- renderUI({
    s <- current_summary()
    tagList(
      card_box("Lectura analítica", div(s$story)),
      card_box("Último año (niveles)", kv_table(s$last_rows)),
      card_box("Variación interanual (último año vs. anterior)", kv_table(s$growth_rows))
    )
  })
  
  # =========================================================
  # ✅ Extra UI para mapas (FIX del error de parseo)
  # =========================================================
  render_extra_map <- function(output_id, prefix){
    output[[output_id]] <- renderUI({
      base_sel <- input[[paste0(prefix, "_base")]] %||% default_base
      
      if (base_sel == "UPRA_EVA_A") {
        tagList(
          div(lbl("Indicadores a considerar"),
              selectInput(paste0(prefix, "_metric"), label = NULL,
                          choices = eva_metric_choices, selected = "produccion_t")),
          div(lbl("Cultivos"),
              selectInput(paste0(prefix, "_cultivo"), label = NULL,
                          choices = eva_cultivo_choices, selected = "__ALL__"))
        )
      } else if (base_sel == "FINAGRO_CFA_FAST") {
        tagList(
          div(
            lbl("Métrica para el mapa"),
            selectInput(paste0(prefix, "_fin_metric"), label = NULL,
                        choices = c("Monto real del crédito"="monto", "Número de créditos"="creditos"),
                        selected = "monto")
          ),
          div(
            lbl("Tipo de productor"),
            selectInput(paste0(prefix, "_fin_tipo"), label = NULL,
                        choices = finagro_choices$tipo, selected = "Todos")
          ),
          div(
            lbl("Sexo"),
            selectInput(paste0(prefix, "_fin_sexo"), label = NULL,
                        choices = finagro_choices$sexo, selected = "Todos")
          ),
          div(
            lbl("Línea de crédito"),
            selectInput(paste0(prefix, "_fin_linea"), label = NULL,
                        choices = finagro_choices$linea, selected = "Todos")
          ),
          div(
            lbl("Eslabón de la cadena"),
            selectInput(paste0(prefix, "_fin_eslabon"), label = NULL,
                        choices = finagro_choices$eslabon, selected = "Todos")
          )
        )
      } else {
        NULL
      }
    })
  }
  
  render_extra_map("map1_extra", "map1")
  render_extra_map("map2_extra", "map2")
  
  # ----- Obtener todos los años disponibles -----
  all_years <- sort(unique(c(
    if (!is.null(eva_dep_all)) eva_dep_all$ano,
    if (!is.null(finagro_dep_all)) finagro_dep_all$ano
  )))
  default_fixed_year <- if (length(all_years)) max(all_years, na.rm = TRUE) else NA_integer_
  
  # ----- Año fijo para mapas -----
  output$fixed_year_ui <- renderUI({
    lock <- input$lock_years %||% FALSE
    if (!isTRUE(lock)) return(tags$span("Active el año fijo para usar este filtro.", style = "font-size:12px;color:#9ca3af;"))
    if (!length(all_years)) return(tags$span("Sin años disponibles.", style = "font-size:12px;color:#9ca3af;"))
    selectInput("fixed_year", label = NULL, choices = all_years, selected = default_fixed_year)
  })
  
  output$map1_year_ui <- renderUI({
    lock <- input$lock_years %||% FALSE
    if (isTRUE(lock)) return(tags$span("Año controlado por el filtro global.", style = "font-size:12px;color:#6b7280;"))
    if (!length(all_years)) return(tags$span("Sin años disponibles.", style="font-size:12px;color:#9ca3af;"))
    selectInput("map1_year", label = NULL, choices = all_years, selected = default_fixed_year)
  })
  
  output$map2_year_ui <- renderUI({
    lock <- input$lock_years %||% FALSE
    if (isTRUE(lock)) return(tags$span("Año controlado por el filtro global.", style = "font-size:12px;color:#6b7280;"))
    if (!length(all_years)) return(tags$span("Sin años disponibles.", style="font-size:12px;color:#9ca3af;"))
    selectInput("map2_year", label = NULL, choices = all_years, selected = default_fixed_year)
  })
  
  # ----- Construir datos para mapas -----
  build_map_data <- function(base_sel, year_sel, prefix){
    if (is.null(shp_dep) || is.na(dep_code_col_shp)) return(NULL)
    if (is.null(year_sel) || is.na(year_sel)) return(NULL)
    
    year_int <- as.integer(year_sel)
    
    if (base_sel == "UPRA_EVA_A") {
      if (is.null(eva_dep_all) || !nrow(eva_dep_all)) return(NULL)
      
      metric_sel  <- input[[paste0(prefix, "_metric")]]  %||% "produccion_t"
      cultivo_sel <- input[[paste0(prefix, "_cultivo")]] %||% "__ALL__"
      
      df <- eva_dep_all %>% filter(ano == year_int)
      if (!nrow(df)) return(NULL)
      
      if (cultivo_sel != "__ALL__") {
        df <- df %>% filter(cultivo == cultivo_sel)
        if (!nrow(df)) return(NULL)
      }
      
      df <- df %>%
        group_by(ano, cod_dep) %>%
        summarise(valor = sum(.data[[metric_sel]], na.rm = TRUE), .groups = "drop")
      
    } else if (base_sel == "FINAGRO_CFA_FAST") {
      if (is.null(finagro_df) || !nrow(finagro_df)) return(NULL)
      
      df <- finagro_apply_filters(
        finagro_df,
        tipo    = input[[paste0(prefix, "_fin_tipo")]]   %||% "Todos",
        sexo    = input[[paste0(prefix, "_fin_sexo")]]   %||% "Todos",
        linea   = input[[paste0(prefix, "_fin_linea")]]  %||% "Todos",
        eslabon = input[[paste0(prefix, "_fin_eslabon")]]%||% "Todos"
      )
      
      df <- df %>%
        filter(ano == year_int, !is.na(COD_DPTO2), nzchar(COD_DPTO2)) %>%
        group_by(cod_dep = COD_DPTO2, dep_nom = DEPARTAMENTO_D) %>%
        summarise(
          monto    = sum(VALOR_CREDITO_REAL, na.rm = TRUE),
          creditos = sum(NUMERO_CREDITO, na.rm = TRUE),
          .groups = "drop"
        )
      
      metric_sel <- input[[paste0(prefix, "_fin_metric")]] %||% "monto"
      df <- df %>% mutate(valor = if (metric_sel == "monto") monto else creditos)
      
    } else {
      return(NULL)
    }
    
    if (!nrow(df)) return(NULL)
    
    shp <- shp_dep
    shp$cod_dep_join <- as.character(shp[[dep_code_col_shp]])
    df$cod_dep_join  <- as.character(df$cod_dep)
    dplyr::left_join(shp, df, by = "cod_dep_join")
  }
  
  map1_data <- reactive({
    base_sel <- input$map1_base %||% default_base
    yr <- if (isTRUE(input$lock_years %||% FALSE)) input$fixed_year else input$map1_year
    build_map_data(base_sel, yr, "map1")
  })
  
  map2_data <- reactive({
    base_sel <- input$map2_base %||% default_base
    yr <- if (isTRUE(input$lock_years %||% FALSE)) input$fixed_year else input$map2_year
    build_map_data(base_sel, yr, "map2")
  })
  
  # ----- Inicializar mapas -----
  output$map_1 <- leaflet::renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(lng = -74, lat = 4.5, zoom = 5)
  })
  output$map_2 <- leaflet::renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(lng = -74, lat = 4.5, zoom = 5)
  })
  
  # ----- Actualizar mapas -----
  update_dep_leaflet <- function(map_id, md, palette_name, legend_title){
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
    if (length(bins) < 3) bins <- sort(unique(pretty(range(vals, na.rm = TRUE), n = 5)))
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
    
    labels <- sprintf("<strong>%s</strong><br/>Valor: %s", dep_name, fmt_short(md$valor)) %>%
      lapply(htmltools::HTML)
    
    legend_labels <- make_bin_labels_co(bins)
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
  
  # ----- Observadores para actualizar mapas -----
  observe({
    md <- map1_data()
    base_sel <- input$map1_base %||% default_base
    
    if (base_sel == "UPRA_EVA_A") {
      metric_sel <- input$map1_metric %||% "produccion_t"
      legend_title <- switch(metric_sel,
                             "area_sembrada_ha"  = "Área sembrada (hectáreas)",
                             "area_cosechada_ha" = "Área cosechada (hectáreas)",
                             "produccion_t"      = "Producción (toneladas)",
                             "Valor")
    } else if (base_sel == "FINAGRO_CFA_FAST") {
      metric_sel <- input$map1_fin_metric %||% "monto"
      legend_title <- if (metric_sel == "monto") "Monto real (COP)" else "Número de créditos"
    } else {
      legend_title <- "Valor"
    }
    
    update_dep_leaflet("map_1", md, palette_name = "Greens", legend_title = legend_title)
  })
  
  observe({
    md <- map2_data()
    base_sel <- input$map2_base %||% default_base
    
    if (base_sel == "UPRA_EVA_A") {
      metric_sel <- input$map2_metric %||% "produccion_t"
      legend_title <- switch(metric_sel,
                             "area_sembrada_ha"  = "Área sembrada (hectáreas)",
                             "area_cosechada_ha" = "Área cosechada (hectáreas)",
                             "produccion_t"      = "Producción (toneladas)",
                             "Valor")
    } else if (base_sel == "FINAGRO_CFA_FAST") {
      metric_sel <- input$map2_fin_metric %||% "monto"
      legend_title <- if (metric_sel == "monto") "Monto real (COP)" else "Número de créditos"
    } else {
      legend_title <- "Valor"
    }
    
    update_dep_leaflet("map_2", md, palette_name = "Blues", legend_title = legend_title)
  })
}

shinyApp(ui, server)
