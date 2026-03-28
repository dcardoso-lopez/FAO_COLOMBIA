# app.R — ECV Consumo (c_ = binarios, f_ = frecuencias, FIES-8)
# ============================================================================
# MODIFICADA:
# + Botones estilo ICA / IDM
# + Exportación robusta de PNG (htmlwidgets + webshot2 + retry)
# + Un solo botón "Descargar informe (PDF)" para TODO el tablero
# + El PDF genera y muestra TODOS los objetos visuales:
#   - FIES: mapa + serie + ranking
#   - ULTRA: mapa + serie + ranking
# + Renderiza Informe_descargable.Rmd desde la raíz
# + NO depende de CSV para el informe
# + FIX: el PDF usa snapshot de inputs, para evitar reactividad interrumpida
# ============================================================================

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(dplyr); library(tidyr); library(readr); library(stringi); library(scales)
  library(leaflet); library(sf); library(htmltools); library(plotly); library(haven); library(stringr)
  library(htmlwidgets); library(webshot2)
  library(rmarkdown); library(ragg); library(ggplot2)
})

options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

# ---------------- Utils base ----------------
`%||%` <- function(x,y) if (is.null(x)||length(x)==0) y else x

# ---------------- Rutas ----------------
get_app_root <- function(){
  normalizePath(shiny::getShinyOption("appDir") %||% getwd(), winslash = "/", mustWork = FALSE)
}

app_root <- get_app_root()
data_dir <- file.path(app_root, "data")
ruta_ecv     <- file.path(data_dir, "052_DANE_ECV.rds")
ruta_shp_dep <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")
ruta_shp_mun <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")

EXPORT_DIR <- file.path(app_root, "Descargas")
dir.create(EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)

ruta_rmd <- file.path(app_root, "Informe_descargable.Rmd")

# PNG export config
PNG_VWIDTH_MAP   <- 2400
PNG_VHEIGHT_MAP  <- 1700
PNG_VWIDTH_PLOT  <- 1800
PNG_VHEIGHT_TS   <- 900
PNG_VHEIGHT_BAR  <- 1000
PNG_DELAY_MAP    <- 1.3
PNG_DELAY_PLOT   <- 0.9

# Nombres fijos para FIES
IMG_FIES_MAP  <- file.path(EXPORT_DIR, "ecv_fies_mapa.png")
IMG_FIES_TS   <- file.path(EXPORT_DIR, "ecv_fies_serie.png")
IMG_FIES_BAR  <- file.path(EXPORT_DIR, "ecv_fies_ranking.png")

# Nombres fijos para ULTRA
IMG_ULTRA_MAP <- file.path(EXPORT_DIR, "ecv_ultra_mapa.png")
IMG_ULTRA_TS  <- file.path(EXPORT_DIR, "ecv_ultra_serie.png")
IMG_ULTRA_BAR <- file.path(EXPORT_DIR, "ecv_ultra_ranking.png")

if (!file.exists(ruta_ecv)) stop("No se encuentra el archivo: ", ruta_ecv)

chk <- function(shp){
  b <- sub("\\.shp$","",shp)
  req <- paste0(b,c(".shp",".dbf",".shx",".prj"))
  req[!file.exists(req)]
}
if (length(chk(ruta_shp_dep))) stop("Faltan partes del SHP deptos en /data/shp (shp/dbf/shx/prj)")

# ---------------- Utils ----------------
norm_txt <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
norm_cmp <- function(x) tolower(stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII"))

plain_txt <- function(x){
  trimws(as.character(x))
}

safe_first <- function(x, default = "?"){
  x <- x[!is.na(x)]
  if (length(x) == 0) default else x[1]
}

to_title <- function(x){
  s <- stringi::stri_trans_tolower(as.character(x), locale = "es")
  min_words <- c("y","e","o","u","de","del","la","las","el","los",
                 "en","por","para","con","a","al")
  vapply(s, function(z){
    if (is.na(z)) return(NA_character_)
    z <- trimws(z)
    if (!nzchar(z)) return("")
    parts <- unlist(strsplit(z, "\\s+"))
    if (!length(parts)) return("")
    parts_out <- character(length(parts))
    for (i in seq_along(parts)){
      w <- parts[i]
      if (!nzchar(w)) {
        parts_out[i] <- w
      } else if (i > 1 && w %in% min_words) {
        parts_out[i] <- w
      } else {
        parts_out[i] <- stringi::stri_trans_totitle(
          w,
          opts_brkiter = stringi::stri_opts_brkiter(type="word")
        )
      }
    }
    paste(parts_out, collapse = " ")
  }, FUN.VALUE = character(1))
}

shorten_depto_lbl <- function(x){
  out <- as.character(x)
  nm  <- norm_cmp(out)
  target <- "archipielago de san andres, providencia y santa catalina"
  out[nm == target] <- "San Andrés y Providencia"
  
  target_bog1 <- "bogota, d.c."
  target_bog2 <- "bogota d.c."
  out[nm %in% c(target_bog1, target_bog2)] <- "Bogotá D.C."
  
  out
}

lbl <- function(x) div(class = "filter-label", x)

safe_as_char <- function(x){
  if (inherits(x, "haven_labelled")) as.character(haven::as_factor(x, levels = "labels", ordered = FALSE))
  else as.character(x)
}
safe_as_num <- function(x){
  if (inherits(x, "haven_labelled")) suppressWarnings(as.numeric(haven::zap_labels(x)))
  else if (is.numeric(x)) as.numeric(x)
  else suppressWarnings(as.numeric(readr::parse_number(as.character(x))))
}
numish <- function(x) safe_as_num(x)

w_prop <- function(x, w){
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * x[ok]) / sum(w[ok])
}

fmt_num_co <- function(x, digits = 1){
  ifelse(
    is.na(x) | !is.finite(x),
    NA_character_,
    scales::number(
      x,
      accuracy     = 10^(-digits),
      big.mark     = ".",
      decimal.mark = ","
    )
  )
}
fmt_pct1_co <- function(x){
  out <- fmt_num_co(x, digits = 1)
  ifelse(is.na(out), NA_character_, paste0(out, "%"))
}

norm_dep2 <- function(x){
  x <- gsub("\\D","",as.character(x)); x[nchar(x)==0] <- NA
  stringi::stri_pad_left(x,2,"0")
}
norm_mun5 <- function(x){
  x <- gsub("\\D","",as.character(x)); x[nchar(x)==0] <- NA
  stringi::stri_pad_left(x,5,"0")
}

COL_BAR   <-  "#e6550d"
COLS_MAP  <- c("#ffe0cc", "#fa8916", "#e6550d", "#9c4a00")

make_pal_fixed <- function(values, colors = COLS_MAP){
  vals <- suppressWarnings(as.numeric(values))
  vals <- vals[is.finite(vals)]
  n <- length(colors)
  if (length(vals) == 0) {
    bins <- c(0,1)
    pal <- leaflet::colorBin(
      palette = colors, domain = c(0,1), bins = bins,
      na.color = "#f0f0f0", right = FALSE
    )
    attr(pal, "bins") <- bins
    return(pal)
  }
  rng <- range(vals, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2])) {
    bins <- c(0, 1)
  } else if (abs(diff(rng)) < .Machine$double.eps * 10) {
    v <- rng[1]; bins <- c(v - 0.5, v + 0.5)
  } else {
    qs <- suppressWarnings(quantile(vals, probs = seq(0, 1, length.out = n + 1),
                                    na.rm = TRUE, type = 7))
    bins <- unique(as.numeric(qs))
    if (length(bins) < 2) bins <- seq(rng[1], rng[2], length.out = n + 1)
    eps <- max(1e-12, diff(range(bins))/1e9)
    for (i in 2:length(bins)) if (bins[i] <= bins[i-1]) bins[i] <- bins[i-1] + eps
  }
  pal <- leaflet::colorBin(
    palette = colors, domain = vals, bins = bins,
    na.color = "#f0f0f0", right = FALSE
  )
  attr(pal, "bins") <- bins
  pal
}

legend_fmt_pct1 <- function(type = "numeric", cuts, p) {
  n <- length(cuts)
  if (n < 2) return("")
  labs <- character(n - 1)
  for (i in seq_len(n - 1)) {
    a <- cuts[i]
    b <- cuts[i + 1]
    pa <- fmt_num_co(a, digits = 1)
    pb <- fmt_num_co(b, digits = 1)
    if (i == 1) {
      labs[i] <- paste0(pa, " – ", pb, " %")
    } else {
      labs[i] <- paste0("> ", pa, " – ", pb, " %")
    }
  }
  labs
}

fit_bounds_proxy <- function(proxy, geom){
  if (is.null(geom) || nrow(geom)==0) return(invisible(proxy))
  bb <- sf::st_bbox(geom)
  leaflet::fitBounds(proxy, bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
}

col_or_na_int <- function(df, candidates){
  n  <- nrow(df)
  nm <- tolower(names(df))
  cand <- tolower(candidates)
  for (c in cand){
    hit <- which(nm == c)
    if (length(hit)) {
      return(suppressWarnings(as.integer(numish(df[[hit[1]]]))))
    }
  }
  nm_norm   <- gsub("_|\\s", "", nm)
  cand_norm <- unique(gsub("_|\\s", "", cand))
  for (c in cand_norm){
    hit <- which(nm_norm == c)
    if (length(hit)) {
      return(suppressWarnings(as.integer(numish(df[[hit[1]]]))))
    }
  }
  rep(NA_integer_, n)
}

format_breaks_list <- function(breaks, percent = FALSE){
  if (length(breaks) < 2)
    return("<li>Sin información suficiente para segmentar</li>")
  paste(
    lapply(seq_len(length(breaks) - 1), function(i){
      a <- breaks[i]; b <- breaks[i+1]
      if (percent) {
        pa <- fmt_pct1_co(a)
        pb <- fmt_pct1_co(b)
      } else {
        pa <- fmt_num_co(a, digits = 1)
        pb <- fmt_num_co(b, digits = 1)
      }
      if (i == 1) {
        sprintf("<li>%s – %s</li>", pa, pb)
      } else {
        sprintf("<li>&gt; %s – %s</li>", pa, pb)
      }
    }),
    collapse = "\n"
  )
}
build_info_html <- function(breaks, percent = FALSE){
  htmltools::HTML(sprintf(
    '<div class="info-title">ℹ️&nbsp;Cómo se segmentan los cortes</div>
       <div class="info-text">Los colores se calculan con cuartiles del indicador mostrado en esta vista.</div>
       <ul class="info-list">%s</ul>',
    format_breaks_list(breaks, percent)
  ))
}

zoom_from_bbox <- function(bb){
  w <- abs(as.numeric(bb["xmax"] - bb["xmin"]))
  h <- abs(as.numeric(bb["ymax"] - bb["ymin"]))
  span <- max(w, h)
  if (!is.finite(span)) return(6)
  if (span < 0.10) return(12)
  if (span < 0.20) return(11)
  if (span < 0.35) return(10)
  if (span < 0.80) return(9)
  if (span < 1.50) return(8)
  if (span < 3.00) return(7)
  6
}

save_widget_png <- function(widget, out_png, vwidth, vheight, delay = 1){
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  
  tmp_dir  <- tempfile("wshot_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  lib_dir  <- file.path(tmp_dir, "lib")
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
  tmp_html <- file.path(tmp_dir, "widget.html")
  
  htmlwidgets::saveWidget(widget, file = tmp_html, selfcontained = FALSE, libdir = lib_dir)
  
  webshot2::webshot(
    url     = tmp_html,
    file    = out_png,
    vwidth  = vwidth,
    vheight = vheight,
    delay   = delay
  )
  
  file.exists(out_png) &&
    is.finite(file.info(out_png)$size) &&
    file.info(out_png)$size > 0
}

save_widget_png_retry <- function(widget, out_png, vwidth, vheight, delay_base = 1){
  delays <- c(delay_base, delay_base + 1.5, delay_base + 3)
  for (d in delays){
    ok <- tryCatch(
      save_widget_png(widget, out_png, vwidth = vwidth, vheight = vheight, delay = d),
      error = function(e) FALSE
    )
    if (isTRUE(ok)) return(TRUE)
  }
  FALSE
}

# ---------------- Cargar base ----------------
ecv_raw <- readRDS(ruta_ecv)
names(ecv_raw) <- tolower(names(ecv_raw))

pick1 <- function(nms, prefer, pattern) {
  if (prefer %in% nms) prefer else nms[grepl(pattern, nms, ignore.case = TRUE)][1]
}

col_ano     <- pick1(names(ecv_raw), "anio", "^a(n|ñ)o$|year")
col_dep_cod <- pick1(names(ecv_raw), "cod_dane_dpto_d", "cod.*dane.*dpto|dpto.*(ccdgo|cod)|cod.*dep")
col_dep_nom <- pick1(names(ecv_raw), "departamento_d", "^departa")
col_mun_cod <- pick1(names(ecv_raw), "cod_dane_mp_d", "cod.*dane.*(mpio|mun)|mpio.*(ccdgo|cod)|cod.*muni")
col_mun_nom <- pick1(names(ecv_raw), "municipio_d", "^muni|municipio")
col_clase   <- pick1(names(ecv_raw), "clase", "^clase$|^area$|urb|rur")
col_sexo_txt<- if ("sexo" %in% names(ecv_raw)) "sexo" else NA_character_
col_sexo    <- if (!is.na(col_sexo_txt)) col_sexo_txt else pick1(names(ecv_raw), "p6020", "^p?6020|sexo")
col_edad    <- pick1(names(ecv_raw), "p6040", "^p?6040|edad")
col_w       <- pick1(names(ecv_raw), "fex_c.x", "^fex|factor|pondera|expans")

need_cols <- c(col_ano,col_dep_cod,col_dep_nom,col_sexo,col_edad,col_w)
if (any(is.na(need_cols))) stop("Faltan columnas clave en ECV (anio, cod_dane_dpto_d, departamento_d, sexo/p6020, p6040, factor).")

col_c_azuc <- names(ecv_raw)[grepl("^c_.*azucar", names(ecv_raw))]
col_c_paq  <- names(ecv_raw)[grepl("^c_.*paq|^c_.*paque", names(ecv_raw))]
col_c_azuc <- if (length(col_c_azuc)) col_c_azuc[1] else NA_character_
col_c_paq  <- if (length(col_c_paq))  col_c_paq[1]  else NA_character_

col_f_azuc <- names(ecv_raw)[grepl("^f_.*azucar", names(ecv_raw))]
col_f_paq  <- names(ecv_raw)[grepl("^f_.*paq|^f_.*paque", names(ecv_raw))]
col_f_azuc <- if (length(col_f_azuc)) col_f_azuc[1] else NA_character_
col_f_paq  <- if (length(col_f_paq))  col_f_paq[1]  else NA_character_

mk_clase_exact <- function(v){
  if (all(is.na(v))) return(rep(NA_character_, length(v)))
  vnum <- suppressWarnings(as.integer(numish(v)))
  out  <- ifelse(vnum == 1, "Cabecera municipal",
                 ifelse(vnum == 2, "Centros Poblados y Rural Disperso", NA_character_))
  need_txt <- is.na(out)
  if (any(need_txt)) {
    txt <- tolower(norm_txt(safe_as_char(v[need_txt])))
    lbl2 <- rep(NA_character_, length(txt))
    lbl2[grepl("cabecera", txt)] <- "Cabecera municipal"
    lbl2[grepl("centro|centros|poblado|poblados|rural|disperso", txt)] <- "Centros Poblados y Rural Disperso"
    out[need_txt] <- lbl2
  }
  out
}

mk_sexo_lbl <- function(col_val){
  if (!is.null(col_sexo_txt) && !is.na(col_sexo_txt)) {
    s <- tolower(norm_txt(safe_as_char(col_val)))
    ifelse(grepl("^hombre|^hombres|^m$", s), "Hombre",
           ifelse(grepl("^mujer|^mujeres|^f$", s), "Mujer", "Sin dato"))
  } else {
    v <- numish(col_val)
    dplyr::case_when(v==1 ~ "Hombre", v==2 ~ "Mujer", TRUE ~ "Sin dato")
  }
}

ecv <- tibble::tibble(
  anio           = suppressWarnings(as.integer(numish(ecv_raw[[col_ano]]))),
  COD_DANE_DPTO2 = norm_dep2(ecv_raw[[col_dep_cod]]),
  DEPARTAMENTO   = plain_txt(ecv_raw[[col_dep_nom]]),
  COD_DANE_MPIO2 = if (!is.na(col_mun_cod)) norm_mun5(ecv_raw[[col_mun_cod]]) else NA_character_,
  MUNICIPIO      = if (!is.na(col_mun_nom)) plain_txt(ecv_raw[[col_mun_nom]]) else NA_character_,
  CLASE_LBL      = if (!is.na(col_clase)) mk_clase_exact(ecv_raw[[col_clase]]) else NA_character_,
  p6020_lbl      = mk_sexo_lbl(ecv_raw[[col_sexo]]),
  p6040          = numish(ecv_raw[[col_edad]]),
  fexp           = suppressWarnings(as.numeric(numish(ecv_raw[[col_w]]))),
  c_azucaradas   = if (!is.na(col_c_azuc)) suppressWarnings(as.integer(numish(ecv_raw[[col_c_azuc]]))) else NA_integer_,
  c_paquetes     = if (!is.na(col_c_paq))  suppressWarnings(as.integer(numish(ecv_raw[[col_c_paq]])))  else NA_integer_,
  f_azucaradas   = if (!is.na(col_f_azuc)) suppressWarnings(as.integer(numish(ecv_raw[[col_f_azuc]]))) else NA_integer_,
  f_paquetes     = if (!is.na(col_f_paq))  suppressWarnings(as.integer(numish(ecv_raw[[col_f_paq]])))  else NA_integer_,
  p_suficiente_a          = col_or_na_int(ecv_raw, c("p3516s1","p3516_s1","p_suficiente_a")),
  np_comer_a_saludables   = col_or_na_int(ecv_raw, c("p3516s2","p3516_s2","np_comer_a_saludables")),
  c_poca_variedad         = col_or_na_int(ecv_raw, c("p3516s3","p3516_s3","c_poca_variedad")),
  salto_comidas           = col_or_na_int(ecv_raw, c("p3516s4","p3516_s4","salto_comidas")),
  comio_menos_delopensado = col_or_na_int(ecv_raw, c("p3516s5","p3516_s5","comio_menos_delopensado")),
  hogar_sin_alimentos     = col_or_na_int(ecv_raw, c("p3516s6","p3516_s6","hogar_sin_alimentos")),
  hambre_pero_sin_comida  = col_or_na_int(ecv_raw, c("p3516s7","p3516_s7","hambre_pero_sin_comida")),
  no_comer_dia_entero     = col_or_na_int(ecv_raw, c("p3516s8","p3516_s8","no_comer_dia_entero"))
) %>%
  mutate(
    edad_grupo = cut(
      p6040,
      breaks = c(-Inf,11,17,26,40,59,Inf),
      labels = c("0–11","12–17","18–26","27–40","41–59","60+"),
      right  = TRUE
    )
  )

pad_or_na <- function(x, width){
  x <- as.character(x)
  ifelse(is.na(x) | !nzchar(x), NA_character_, stringr::str_pad(x, width, pad = "0"))
}

ecv <- ecv %>%
  mutate(
    CODMUN = pad_or_na(COD_DANE_MPIO2, 5),
    DPTO2  = dplyr::coalesce(
      pad_or_na(COD_DANE_DPTO2, 2),
      substr(ifelse(is.na(CODMUN), "", CODMUN), 1, 2)
    )
  ) %>%
  dplyr::filter(DEPARTAMENTO == "ATLÁNTICO")

inds_binarios <- c(
  "¿Consume bebidas azucaradas?" = "c_azucaradas",
  "¿Consume alimentos de paquete?" = "c_paquetes"
)
inds_aplican_12mas <- c("c_azucaradas", "f_azucaradas", "c_paquetes", "f_paquetes")

freq_labels <- c(
  "Todos los días de la semana (dos o más veces al día)",
  "Todos los días de la semana (una vez al día)",
  "Cuatro a seis veces a la semana",
  "Dos o tres veces a la semana",
  "Una vez a la semana",
  "Menos de una vez por semana"
)

freq_ind_map <- tibble::tibble(
  var       = rep(c("f_azucaradas","f_paquetes"), each=6),
  code      = rep(1:6, times=2),
  label_base= rep(c("Bebidas azucaradas", "Paquetes"), each=6),
  label_cat = rep(freq_labels, times=2)
) %>%
  mutate(
    key   = paste(var, code, sep = ":"),
    label = label_cat
  )

inds_fies <- c(
  "Se preocupó por no tener suficientes alimentos" = "p_suficiente_a",
  "No pudo comer alimentos saludables y nutritivos" = "np_comer_a_saludables",
  "Consumió poca variedad de alimentos"             = "c_poca_variedad",
  "Saltó comidas (desayuno/almuerzo/cena)"          = "salto_comidas",
  "Comió menos de lo que pensaba debía comer"       = "comio_menos_delopensado",
  "El hogar se quedó sin alimentos"                 = "hogar_sin_alimentos",
  "Tuvo hambre pero no comió"                       = "hambre_pero_sin_comida",
  "Un día entero sin comer"                         = "no_comer_dia_entero"
)

get_fies_phrase_tail <- function(key){
  switch(
    key,
    "p_suficiente_a"          = "se preocuparon por no tener suficientes alimentos",
    "np_comer_a_saludables"   = "no pudieron comer alimentos saludables y nutritivos",
    "c_poca_variedad"         = "consumieron poca variedad de alimentos",
    "salto_comidas"           = "saltaron comidas (desayuno, almuerzo o cena)",
    "comio_menos_delopensado" = "comieron menos de lo que pensaban que debían comer",
    "hogar_sin_alimentos"     = "se quedaron sin alimentos",
    "hambre_pero_sin_comida"  = "tuvieron hambre pero no comieron",
    "no_comer_dia_entero"     = "pasaron un día entero sin comer",
    NULL
  )
}

mk_event_fies <- function(d, ind_var){
  v <- suppressWarnings(as.integer(numish(d[[ind_var]])))
  ifelse(v %in% c(1, 2), as.numeric(v == 1L), NA_real_)
}

dep_sf <- sf::st_read(ruta_shp_dep, quiet=TRUE) %>%
  dplyr::mutate(
    COD_DPTO2 = dplyr::coalesce(
      if ("DPTO_CCDGO" %in% names(.)) sprintf("%02d", suppressWarnings(as.integer(.data$DPTO_CCDGO))) else NA_character_,
      if ("COD_DEPTO"   %in% names(.)) sprintf("%02d", suppressWarnings(as.integer(.data$COD_DEPTO))) else NA_character_
    ),
    DEPTO_N = dplyr::coalesce(
      as.character(.$DEPARTAMENTO_D %||% NA),
      as.character(.$DPTO_CNMBR %||% NA),
      as.character(.$NOMBRE_DEPTO %||% COD_DPTO2)
    )
  ) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid()

mun_sf <- NULL
if (file.exists(ruta_shp_mun) && length(chk(ruta_shp_mun))==0) {
  muni_name_cands <- c("MUNICIPIO_D","MPIO_CNMBR","NOMBRE_MPIO","NOMBRE_MUNICIP","MUNICIPIO","NOMBRE")
  dpto2_cands     <- c("DPTO_CCDGO","COD_DEPTO","DPTO","CODIGO_DEPTO","DPTO_COD")
  mun_raw <- sf::st_read(ruta_shp_mun, quiet = TRUE)
  stopifnot("MPIO_CDPMP" %in% names(mun_raw))
  muni_name_col <- muni_name_cands[muni_name_cands %in% names(mun_raw)][1]
  dpto2_col     <- dpto2_cands[dpto2_cands %in% names(mun_raw)][1]
  mun_sf <- mun_raw %>%
    dplyr::mutate(
      CODMUN      = stringr::str_pad(as.character(.data[["MPIO_CDPMP"]]), 5, pad="0"),
      DPTO2       = if (!is.na(dpto2_col)) stringr::str_pad(as.character(.data[[dpto2_col]]), 2, pad="0") else substr(CODMUN, 1, 2),
      MUNICIPIO_D = if (!is.na(muni_name_col)) as.character(.data[[muni_name_col]]) else CODMUN
    ) %>%
    sf::st_transform(4326) %>%
    sf::st_make_valid()
}

lookup_freq <- function(key){
  row <- freq_ind_map[freq_ind_map$key == key, ]
  if (nrow(row)==0) return(list(var="f_azucaradas", code=1, label=""))
  list(var = row$var[[1]], code = row$code[[1]], label = row$label[[1]])
}

mk_event_freq_single <- function(d, key){
  lk <- lookup_freq(key)
  vcode <- suppressWarnings(as.integer(numish(d[[lk$var]])))
  vcode[!vcode %in% 1:6] <- NA_integer_
  ifelse(is.na(vcode), NA_real_, ifelse(vcode == lk$code, 1, 0))
}

apply_common_filters <- function(d, sexo, edad, ind_var, clase=NULL){
  if (!is.null(sexo) && length(sexo) && !("Todos" %in% sexo)) {
    d <- d %>% dplyr::filter(p6020_lbl %in% sexo)
  }
  if (!is.null(edad) && length(edad)) {
    d <- d %>% dplyr::filter(edad_grupo %in% edad)
  }
  if (!is.null(clase) && length(clase) && !("Todos" %in% clase)) {
    d <- d %>% dplyr::filter(CLASE_LBL %in% clase | is.na(CLASE_LBL))
  }
  if (!is.null(ind_var) && ind_var %in% inds_aplican_12mas) {
    d <- d %>% dplyr::filter(p6040 >= 12 | is.na(p6040))
  }
  d %>% dplyr::filter(is.finite(fexp), fexp>0, !is.na(DEPARTAMENTO), nzchar(DEPARTAMENTO))
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

    .wrap{
      max-width:1200px;
      margin:0 auto;
      padding:18px 22px 36px;
    }

    .filters{
      background:#fff;
      border:1px solid var(--ecv-bdr);
      border-radius:16px;
      padding:16px 18px;
      margin-bottom:16px;
      box-shadow:0 2px 10px rgba(0,0,0,.05);
    }

    .filters-grid-6{
      display:grid;
      grid-template-columns:repeat(auto-fit, minmax(190px, 1fr));
      gap:16px;
      align-items:stretch;
    }
    .filters-grid-2{
      display:grid;
      grid-template-columns:repeat(auto-fit, minmax(220px, 1fr));
      gap:14px;
      margin-top:10px;
      align-items:stretch;
    }

    .filters .filters-grid-6 > div,
    .filters .filters-grid-2 > div{
      display:flex;
      flex-direction:column;
    }

    .filter-label{
      font-family:'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      font-size:14px;
      font-weight:500;
      letter-spacing:.2px;
      text-transform:none !important;
      color:#111827;
      margin-bottom:6px;
    }

    .filters .shiny-input-container{
      margin-bottom:0 !important;
    }

    .card{
      background:#fff;
      border:1px solid var(--ecv-bdr);
      border-radius:18px;
      padding:16px;
      box-shadow:0 2px 10px rgba(0,0,0,.05);
    }

    .card-title{
      font-weight:700;
      font-size:16px;
      margin-bottom:0;
    }

    .card-header-row{
      display:flex;
      justify-content:space-between;
      align-items:center;
      gap:8px;
      margin-bottom:8px;
    }

    .btn-download{
      background:#ffffff !important;
      border:1px solid var(--ecv-bdr) !important;
      color:#374151 !important;
      font-weight:700 !important;
      border-radius:12px !important;
      padding:6px 10px !important;
      font-size:12px !important;
      line-height:1.2;
      box-shadow:none !important;
    }
    .btn-download:hover{
      background:#ffffff !important;
      color:#374151 !important;
      box-shadow:none !important;
    }

    .grid-2{
      display:grid;
      grid-template-columns:1.35fr 1fr;
      grid-template-rows:520px 340px;
      gap:16px;
      align-items:stretch;
    }
    .cell-left-1{grid-column:1;grid-row:1;}
    .cell-left-2{grid-column:1;grid-row:2;}
    .cell-right-span{grid-column:2;grid-row:1 / span 2;}

    .card.stretch{
      height:100%;
      display:flex;
      flex-direction:column;
    }
    .fill{
      flex:1;
      min-height:0;
    }
    .fill > .html-widget,
    .fill > .leaflet,
    .fill > div,
    .plotly.html-widget{
      height:100% !important;
    }

    .filters .form-select,
    .filters .form-control{
      border:2px solid var(--ecv-bdr) !important;
      border-radius:10px !important;
      background-color:#fff !important;
      box-shadow:none !important;
      padding-top:0.375rem;
      padding-bottom:0.375rem;
      height:56px;
      min-height:56px;
      color:#111827;
    }

    .filters .selectize-control.single .selectize-input,
    .filters .selectize-control.multi .selectize-input{
      border:2px solid var(--ecv-bdr) !important;
      border-radius:10px !important;
      box-shadow:none !important;
      background-color:#fff !important;
      min-height:56px;
      height:56px;
      padding-top:8px;
      padding-bottom:8px;
      color:#111827;
      display:flex;
      align-items:center;
    }
    .filters .selectize-dropdown{
      border:2px solid var(--ecv-bdr) !important;
      border-radius:10px !important;
    }

    .filters .bootstrap-select{
      border:none !important;
      box-shadow:none !important;
    }
    .filters .bootstrap-select > .dropdown-toggle,
    .filters .bootstrap-select .btn{
      border:2px solid var(--ecv-bdr) !important;
      border-radius:10px !important;
      background-color:#fff !important;
      color:#111827 !important;
      box-shadow:none !important;
      padding-top:0.375rem !important;
      padding-bottom:0.375rem !important;
      height:56px;
      min-height:56px;
      display:flex;
      align-items:center;
      justify-content:space-between;
    }

    .filters .form-select:focus,
    .filters .form-control:focus,
    .filters .selectize-control.single .selectize-input.input-active,
    .filters .selectize-control.multi .selectize-input.input-active,
    .filters .bootstrap-select > .dropdown-toggle:focus,
    .filters .bootstrap-select .btn:focus{
      border-color:var(--ecv-bdr) !important;
      box-shadow:0 0 0 0.15rem rgba(245,124,0,.35) !important;
    }

    .info-quantiles-icon{
      margin-left:8px;
      font-size:14px;
      cursor:pointer;
      color:#4b5563;
    }
    .info-title{font-weight:700;margin-bottom:6px;}
    .info-text{font-size:12px;color:#4b5563;margin-bottom:6px;}
    .info-list{margin:0;padding-left:18px;font-size:12px;}

    .footer-actions{
      margin-top:10px;
      display:flex;
      justify-content:flex-end;
      gap:8px;
      padding:6px 6px 0;
      flex-wrap:wrap;
    }

    @media (max-width:1200px){
      .grid-2{
        grid-auto-rows:360px;
      }
    }
    @media (max-width:800px){
      .grid-2{
        grid-template-columns:1fr;
        grid-auto-rows:auto;
      }
      .cell-left-1,
      .cell-left-2,
      .cell-right-span{
        grid-column:auto;
        grid-row:auto;
      }
    }
  ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateQuantilesTooltipFies', function(message) {
        var el = document.getElementById('info_quantiles_fies');
        if (!el) return;
        el.setAttribute('data-bs-toggle', 'popover');
        el.setAttribute('data-bs-html', 'true');
        el.setAttribute('data-bs-content', message.html || '');
        el.setAttribute('data-bs-placement', 'bottom');
        el.setAttribute('data-bs-offset', '0,8');
        if (typeof bootstrap !== 'undefined' && bootstrap.Popover) {
          var existing = bootstrap.Popover.getInstance(el);
          if (existing) existing.dispose();
          new bootstrap.Popover(el);
        }
      });
      Shiny.addCustomMessageHandler('updateQuantilesTooltipUltra', function(message) {
        var el = document.getElementById('info_quantiles_ultra');
        if (!el) return;
        el.setAttribute('data-bs-toggle', 'popover');
        el.setAttribute('data-bs-html', 'true');
        el.setAttribute('data-bs-content', message.html || '');
        el.setAttribute('data-bs-placement', 'bottom');
        el.setAttribute('data-bs-offset', '0,8');
        if (typeof bootstrap !== 'undefined' && bootstrap.Popover) {
          var existing = bootstrap.Popover.getInstance(el);
          if (existing) existing.dispose();
          new bootstrap.Popover(el);
        }
      });

      $(document).on('shown.bs.tab', 'button[data-bs-toggle=\"tab\"], a[data-bs-toggle=\"tab\"]', function(e) {
        ['info_quantiles_fies','info_quantiles_ultra'].forEach(function(id){
          var el = document.getElementById(id);
          if (!el || typeof bootstrap === 'undefined' || !bootstrap.Popover) return;
          var instance = bootstrap.Popover.getInstance(el);
          if (instance) { instance.hide(); }
        });
      });
    "))
  ),
  div(
    class = "wrap",
    h3(""),
    tabsetPanel(
      type = "tabs", id = "tabs",
      
      tabPanel(
        "Condición de inseguridad alimentaria",
        
        div(
          class = "filters",
          div(
            class = "filters-grid-6",
            div(lbl("¿Qué año analizamos?"),  uiOutput("anio_ui3")),
            div(lbl("¿En qué departamento?"), uiOutput("dep_ui3")),
            div(
              lbl("¿Área urbana o rural?"),
              pickerInput(
                "f_clase3", NULL,
                multiple = FALSE,
                choices  = c(
                  "Todos" = "Todos",
                  "Cabecera municipal" = "Cabecera municipal",
                  "Centros Poblados y Rural Disperso" = "Centros Poblados y Rural Disperso"
                ),
                selected = "Todos",
                options  = list(`actions-box` = FALSE)
              )
            ),
            div(
              lbl("¿Hombres o mujeres?"),
              pickerInput(
                "f_sexo3", NULL,
                multiple = FALSE,
                choices  = c(
                  "Todos"  = "Todos",
                  "Hombre" = "Hombre",
                  "Mujer"  = "Mujer"
                ),
                selected = "Todos",
                options  = list(`actions-box` = FALSE)
              )
            ),
            div(
              lbl("Indicador del FIES"),
              selectInput(
                "f_ind_fies", NULL,
                choices  = inds_fies,
                selected = "p_suficiente_a"
              )
            )
          )
        ),
        
        div(
          class = "grid-2",
          div(
            class = "card stretch cell-left-1",
            div(
              class = "card-header-row",
              div(
                style = "display:flex;align-items:center;gap:6px;",
                uiOutput("titulo_mapa_fies"),
                tags$span(id = "info_quantiles_fies", "ℹ️", class = "info-quantiles-icon")
              ),
              downloadButton(
                "dl_mapa_fies",
                label = "Descargar PNG",
                class  = "btn-download"
              )
            ),
            div(class = "fill", leafletOutput("mapa_fies", height = "100%"))
          ),
          div(
            class = "card stretch cell-left-2",
            div(
              class = "card-header-row",
              uiOutput("titulo_ts_fies"),
              downloadButton(
                "dl_ts_fies",
                label = "Descargar PNG",
                class  = "btn-download"
              )
            ),
            htmlOutput("ts_label_fies"),
            div(class = "fill", plotlyOutput("ts_prev_fies", height = "100%"))
          ),
          div(
            class = "card stretch cell-right-span",
            div(
              class = "card-header-row",
              uiOutput("titulo_barras_fies"),
              downloadButton(
                "dl_bars_fies",
                label = "Descargar PNG",
                class  = "btn-download"
              )
            ),
            div(class = "fill", plotlyOutput("bars_all_fies", height = "100%"))
          )
        ),
        
        div(
          class = "footer-actions",
          downloadButton(
            "dl_csv_fies",
            label = "Descargar CSV",
            class = "btn-download"
          ),
          downloadButton(
            "dl_reporte_pdf",
            label = "Descargar informe (PDF)",
            class = "btn-download"
          )
        )
      ),
      
      tabPanel(
        "Consumo y frecuencia de snacks y gaseosas",
        
        div(
          class = "filters",
          div(
            class = "filters-grid-6",
            div(lbl("¿Qué año analizamos?"),  uiOutput("anio_ui")),
            div(lbl("¿En qué departamento?"), uiOutput("dep_ui")),
            div(
              lbl("¿Área urbana o rural?"),
              pickerInput(
                "f_clase", NULL,
                multiple = FALSE,
                choices  = c(
                  "Todos" = "Todos",
                  "Cabecera municipal" = "Cabecera municipal",
                  "Centros Poblados y Rural Disperso" = "Centros Poblados y Rural Disperso"
                ),
                selected = "Todos",
                options  = list(`actions-box` = FALSE)
              )
            ),
            div(
              lbl("¿Hombres o mujeres?"),
              pickerInput(
                "f_sexo", NULL,
                multiple = FALSE,
                choices  = c(
                  "Todos"  = "Todos",
                  "Hombre" = "Hombre",
                  "Mujer"  = "Mujer"
                ),
                selected = "Todos",
                options  = list(`actions-box` = FALSE)
              )
            ),
            div(
              lbl("Variable a considerar"),
              selectInput(
                "f_ind_bin", NULL,
                choices  = inds_binarios,
                selected = "c_azucaradas"
              )
            )
          ),
          div(
            class = "filters-grid-2",
            div(
              lbl("Tipo de indicador"),
              radioButtons(
                "f_view_ultra", NULL,
                choices = c(
                  "Presencia de consumo" = "bin",
                  "Frecuencia de consumo" = "freq"
                ),
                selected = "bin",
                inline   = TRUE
              )
            ),
            div(
              conditionalPanel(
                "input.f_view_ultra == 'freq'",
                tagList(
                  lbl("Filtro detallado"),
                  selectInput(
                    "f_ind_freq_key", NULL,
                    choices = NULL
                  )
                )
              )
            )
          )
        ),
        
        div(
          class = "grid-2",
          div(
            class = "card stretch cell-left-1",
            div(
              class = "card-header-row",
              div(
                style = "display:flex;align-items:center;gap:6px;",
                uiOutput("titulo_mapa_ultra"),
                tags$span(id = "info_quantiles_ultra", "ℹ️", class = "info-quantiles-icon")
              ),
              downloadButton(
                "dl_mapa_ultra",
                label = "Descargar PNG",
                class  = "btn-download"
              )
            ),
            div(class = "fill", leafletOutput("mapa_ultra", height = "100%"))
          ),
          div(
            class = "card stretch cell-left-2",
            div(
              class = "card-header-row",
              uiOutput("titulo_ts_ultra"),
              downloadButton(
                "dl_ts_ultra",
                label = "Descargar PNG",
                class  = "btn-download"
              )
            ),
            htmlOutput("ts_label_ultra"),
            div(class = "fill", plotlyOutput("ts_prev_ultra", height = "100%"))
          ),
          div(
            class = "card stretch cell-right-span",
            div(
              class = "card-header-row",
              uiOutput("titulo_barras_ultra"),
              downloadButton(
                "dl_bars_ultra",
                label = "Descargar PNG",
                class  = "btn-download"
              )
            ),
            div(class = "fill", plotlyOutput("bars_all_ultra", height = "100%"))
          )
        ),
        
        div(
          class = "footer-actions",
          downloadButton(
            "dl_reporte_pdf2",
            label = "Descargar informe (PDF)",
            class = "btn-download"
          )
        )
      )
    )
  )
)

# ---------------- SERVER ----------------
server <- function(input, output, session){
  
  yrs_avail <- ecv %>%
    dplyr::filter(is.finite(fexp), fexp>0, !is.na(anio)) %>%
    dplyr::distinct(anio) %>% dplyr::arrange(anio) %>% dplyr::pull(anio)
  yr_latest <- max(yrs_avail, na.rm=TRUE)
  
  output$anio_ui  <- renderUI({ selectInput("anio",  NULL, choices=yrs_avail, selected=yr_latest) })
  output$anio_ui2 <- renderUI({ selectInput("anio2", NULL, choices=yrs_avail, selected=yr_latest) })
  output$anio_ui3 <- renderUI({ selectInput("anio3", NULL, choices=yrs_avail, selected=yr_latest) })
  
  mk_dep_choices <- function(){
    ecv %>%
      dplyr::filter(!is.na(DEPARTAMENTO), nzchar(DEPARTAMENTO)) %>%
      dplyr::distinct(DEPARTAMENTO) %>%
      dplyr::arrange(DEPARTAMENTO) %>%
      dplyr::pull(DEPARTAMENTO)
  }
  
  output$dep_ui <- renderUI({
    vals <- mk_dep_choices()
    selectInput(
      "f_dep", NULL,
      choices  = stats::setNames(vals, to_title(vals)),
      selected = vals[1]
    )
  })
  
  output$dep_ui2 <- renderUI({
    vals <- mk_dep_choices()
    selectInput(
      "f_dep2", NULL,
      choices  = stats::setNames(vals, to_title(vals)),
      selected = vals[1]
    )
  })
  
  output$dep_ui3 <- renderUI({
    vals <- mk_dep_choices()
    selectInput(
      "f_dep3", NULL,
      choices  = stats::setNames(vals, to_title(vals)),
      selected = vals[1]
    )
  })
  
  observe({
    ind <- input$f_ind_bin
    if (is.null(ind)) return()
    if (ind == "c_azucaradas") {
      opts <- freq_ind_map %>% dplyr::filter(var == "f_azucaradas")
    } else {
      opts <- freq_ind_map %>% dplyr::filter(var == "f_paquetes")
    }
    choices <- stats::setNames(as.list(opts$key), opts$label)
    sel <- if (!is.null(input$f_ind_freq_key) && input$f_ind_freq_key %in% opts$key) input$f_ind_freq_key else opts$key[1]
    updateSelectInput(session, "f_ind_freq_key", choices = choices, selected = sel)
  })
  
  # ---------------- FIES ----------------
  fies_lbl_info <- reactive({
    key <- input$f_ind_fies
    if (is.null(key)) {
      return(list(
        label = "este indicador del FIES",
        tail  = "presentan este tipo de inseguridad alimentaria"
      ))
    }
    lblx <- names(inds_fies)[inds_fies == key][1]
    if (is.na(lblx) || !nzchar(lblx)) lblx <- "este indicador del FIES"
    tail <- get_fies_phrase_tail(key)
    if (is.null(tail) || !nzchar(tail)) {
      tail <- "presentan este tipo de inseguridad alimentaria"
    }
    list(label = lblx, tail = tail)
  })
  
  output$titulo_mapa_fies <- renderUI({
    info <- fies_lbl_info()
    txt  <- sprintf("¿Cuáles departamentos concentran más hogares que %s?", info$tail)
    tags$div(class = "card-title", txt)
  })
  
  output$titulo_barras_fies <- renderUI({
    info <- fies_lbl_info()
    txt  <- sprintf("¿Qué departamentos tienen mayor prevalencia de hogares que %s?", info$tail)
    tags$div(class = "card-title", txt)
  })
  
  output$titulo_ts_fies <- renderUI({
    info <- fies_lbl_info()
    txt  <- sprintf("¿Cómo ha evolucionado en el tiempo la prevalencia de hogares que %s?", info$tail)
    tags$div(class = "card-title", txt)
  })
  
  base_anio_fies <- reactive({
    req(input$anio3, input$f_ind_fies)
    d <- ecv %>% dplyr::filter(anio == as.integer(input$anio3))
    if (!is.null(input$f_dep3) && input$f_dep3 != "Todos") d <- d %>% dplyr::filter(DEPARTAMENTO == input$f_dep3)
    apply_common_filters(d, input$f_sexo3, NULL, NULL, clase = input$f_clase3)
  })
  
  base_ts_fies <- reactive({
    req(input$f_ind_fies)
    d <- ecv
    apply_common_filters(d, input$f_sexo3, NULL, NULL, clase = input$f_clase3)
  })
  
  output$mapa_fies <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(-74.3, 4.6, 5)
  })
  
  observe({
    d <- base_anio_fies(); req(nrow(d) > 0)
    ev      <- mk_event_fies(d, input$f_ind_fies)
    dep_sel <- input$f_dep3 %||% "Todos"
    prx     <- leafletProxy("mapa_fies") %>% clearShapes() %>% clearControls()
    
    dd <- d %>%
      dplyr::mutate(evento = as.numeric(ev)) %>%
      dplyr::group_by(COD_DANE_DPTO2, DEPARTAMENTO) %>%
      dplyr::summarise(prev = w_prop(evento, fexp) * 100, .groups = "drop")
    
    shp <- dep_sf %>%
      dplyr::left_join(dd, by = c("COD_DPTO2" = "COD_DANE_DPTO2")) %>%
      dplyr::mutate(
        DEPARTAMENTO = dplyr::coalesce(DEPARTAMENTO, DEPTO_N),
        DEPTO_TC     = shorten_depto_lbl(to_title(DEPARTAMENTO)),
        etq          = paste0(
          "<b>", DEPTO_TC, "</b><br>Prevalencia: ",
          ifelse(is.finite(prev), fmt_pct1_co(prev), "Sin dato")
        )
      )
    
    pal  <- make_pal_fixed(shp$prev, COLS_MAP)
    bins <- attr(pal, "bins")
    info_html <- build_info_html(bins, percent = TRUE)
    session$sendCustomMessage(
      type    = "updateQuantilesTooltipFies",
      message = list(html = as.character(info_html))
    )
    
    if (sum(is.finite(shp$prev)) == 0) {
      prx %>% addPolygons(
        data  = shp, layerId = ~COD_DPTO2,
        fillColor = "#f0f0f0",
        color = "#666", weight = .7, fillOpacity = .7,
        label = ~lapply(etq, HTML)
      )
    } else {
      prx %>% addPolygons(
        data  = shp, layerId = ~COD_DPTO2,
        fillColor = ~pal(prev),
        color = "#666", weight = .7, fillOpacity = .9,
        label = ~lapply(etq, HTML),
        highlightOptions = highlightOptions(color="black", weight=2, bringToFront=TRUE)
      ) %>%
        leaflet::addLegend(
          "bottomright",
          pal      = pal,
          values   = shp$prev,
          title    = "Prevalencia (%)",
          opacity  = .9,
          labFormat = legend_fmt_pct1
        )
    }
    
    if (!is.null(dep_sel) && dep_sel != "Todos") {
      dep_row <- dep_sf %>% dplyr::filter(norm_cmp(DEPTO_N) == norm_cmp(dep_sel))
      if (nrow(dep_row) > 0) {
        prx %>% addPolygons(
          data  = dep_row, fill = FALSE,
          color = "#111", weight = 2.2, opacity = 1
        )
        fit_bounds_proxy(prx, dep_row)
      }
    }
  })
  
  bars_all_fies_plot <- reactive({
    d <- base_anio_fies(); req(nrow(d) > 0)
    ev <- mk_event_fies(d, input$f_ind_fies)
    dd <- d %>%
      dplyr::mutate(evento = as.numeric(ev)) %>%
      dplyr::group_by(DEPARTAMENTO) %>%
      dplyr::summarise(prev = w_prop(evento, fexp) * 100, .groups = "drop") %>%
      dplyr::filter(is.finite(prev)) %>%
      dplyr::arrange(dplyr::desc(prev)) %>%
      dplyr::mutate(
        Departamento_lbl = shorten_depto_lbl(to_title(DEPARTAMENTO)),
        Departamento     = factor(Departamento_lbl, levels = rev(Departamento_lbl)),
        prev_lab         = fmt_num_co(prev, digits = 1)
      )
    
    plot_ly(
      dd,
      x = ~prev, y = ~Departamento,
      type = "bar", orientation = "h",
      text = ~prev_lab,
      textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(color = "white", size=14),
      hovertemplate = "<b>%{y}</b><br>Prevalencia: %{text}%<extra></extra>",
      marker = list(color = COL_BAR)
    ) %>%
      layout(
        xaxis = list(title = "Prevalencia (%)", zeroline = FALSE),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 140, r = 10, t = 10, b = 40),
        paper_bgcolor = "#ffffff",
        plot_bgcolor  = "#ffffff"
      )
  })
  output$bars_all_fies <- renderPlotly({ bars_all_fies_plot() })
  
  ts_prev_fies_plot <- reactive({
    d <- base_ts_fies(); req(nrow(d) > 0)
    ev <- mk_event_fies(d, input$f_ind_fies)
    dt <- d %>%
      dplyr::mutate(evento = as.numeric(ev)) %>%
      dplyr::group_by(anio) %>%
      dplyr::summarise(prev = w_prop(evento, fexp) * 100, .groups = "drop") %>%
      dplyr::filter(!is.na(anio), is.finite(prev)) %>%
      dplyr::arrange(anio) %>%
      dplyr::mutate(prev_txt = fmt_pct1_co(prev))
    
    plot_ly(
      dt, x = ~anio, y = ~prev,
      type = "scatter", mode = "lines+markers",
      text = ~prev_txt,
      hovertemplate = "Año: %{x}<br>Promedio: %{text}<extra></extra>",
      line   = list(color = COL_BAR),
      marker = list(color = COL_BAR)
    ) %>%
      layout(
        xaxis = list(
          title    = "",
          dtick    = 1,
          tickmode = "linear",
          showgrid = FALSE
        ),
        yaxis = list(
          title     = "Prevalencia (%)",
          rangemode = "tozero",
          showgrid  = TRUE,
          gridcolor = "#e5e7eb"
        ),
        margin = list(l = 60, r = 10, t = 10, b = 40),
        paper_bgcolor = "#ffffff",
        plot_bgcolor  = "#ffffff"
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })
  output$ts_prev_fies <- renderPlotly({ ts_prev_fies_plot() })
  output$ts_label_fies <- renderUI({ NULL })
  
  # ---------------- ULTRA ----------------
  mk_event_bin <- function(d, ind){
    v <- suppressWarnings(as.integer(numish(d[[ind]])))
    ifelse(is.na(v), NA_real_, as.numeric(v == 1L))
  }
  
  base_anio_bin <- reactive({
    req(input$anio)
    d <- ecv %>% dplyr::filter(anio==as.integer(input$anio))
    if (!is.null(input$f_dep) && input$f_dep!="Todos") d <- d %>% dplyr::filter(DEPARTAMENTO==input$f_dep)
    apply_common_filters(d, input$f_sexo, NULL, input$f_ind_bin, clase=input$f_clase)
  })
  
  base_ts_bin <- reactive({
    d <- ecv
    apply_common_filters(d, input$f_sexo, NULL, input$f_ind_bin, clase=input$f_clase)
  })
  
  base_anio_freq <- reactive({
    req(input$anio, input$f_ind_freq_key, input$f_ind_bin)
    ind_var <- lookup_freq(input$f_ind_freq_key)$var
    d <- ecv %>% dplyr::filter(anio == as.integer(input$anio))
    if (!is.null(input$f_dep) && input$f_dep!="Todos") d <- d %>% dplyr::filter(DEPARTAMENTO==input$f_dep)
    d <- apply_common_filters(d, input$f_sexo, NULL, ind_var, clase=input$f_clase)
    yes_code <- 1L
    c_var <- if (ind_var == "f_azucaradas") "c_azucaradas" else "c_paquetes"
    v_c <- suppressWarnings(as.integer(numish(d[[c_var]])))
    d <- d[is.na(v_c) | v_c == yes_code, , drop = FALSE]
    d
  })
  
  base_ts_freq <- reactive({
    req(input$f_ind_freq_key, input$f_ind_bin)
    ind_var <- lookup_freq(input$f_ind_freq_key)$var
    d <- ecv
    d <- apply_common_filters(d, input$f_sexo, NULL, ind_var, clase=input$f_clase)
    yes_code <- 1L
    c_var <- if (ind_var == "f_azucaradas") "c_azucaradas" else "c_paquetes"
    v_c <- suppressWarnings(as.integer(numish(d[[c_var]])))
    d <- d[is.na(v_c) | v_c == yes_code, , drop = FALSE]
    d
  })
  
  ultra_mode <- reactive(input$f_view_ultra %||% "bin")
  
  output$titulo_mapa_ultra <- renderUI({
    modo <- ultra_mode()
    txt <- if (modo == "bin") {
      "¿Cuáles departamentos concentran más consumo de estos alimentos?"
    } else {
      "¿Cuáles son los departamentos con mayor porcentaje de hogares para esta frecuencia?"
    }
    tags$div(class = "card-title", txt)
  })
  
  output$titulo_ts_ultra <- renderUI({
    modo <- ultra_mode()
    txt <- if (modo == "bin") {
      "¿Cómo ha evolucionado la prevalencia de consumo?"
    } else {
      "¿Cómo ha evolucionado el consumo de los hogares para esta frecuencia?"
    }
    tags$div(class = "card-title", txt)
  })
  
  output$titulo_barras_ultra <- renderUI({
    modo <- ultra_mode()
    txt <- if (modo == "bin") {
      "¿Qué departamentos tienen mayor prevalencia al consumo de estos alimentos?"
    } else {
      "¿Qué departamentos tienen mayor porcentaje para esta frecuencia de consumo?"
    }
    tags$div(class = "card-title", txt)
  })
  
  output$mapa_ultra <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(-74.3, 4.6, 5)
  })
  
  observe({
    modo    <- ultra_mode()
    dep_sel <- input$f_dep %||% "Todos"
    prx     <- leafletProxy("mapa_ultra") %>% clearShapes() %>% clearControls()
    
    if (modo == "bin") {
      d  <- base_anio_bin();  req(nrow(d) > 0)
      ev <- mk_event_bin(d, input$f_ind_bin %||% "c_azucaradas")
    } else {
      d  <- base_anio_freq(); req(nrow(d) > 0)
      ev <- mk_event_freq_single(d, input$f_ind_freq_key)
    }
    
    dd <- d %>%
      dplyr::mutate(evento = as.numeric(ev)) %>%
      dplyr::group_by(COD_DANE_DPTO2, DEPARTAMENTO) %>%
      dplyr::summarise(prev = w_prop(evento, fexp) * 100, .groups = "drop")
    
    shp <- dep_sf %>%
      dplyr::left_join(dd, by = c("COD_DPTO2" = "COD_DANE_DPTO2")) %>%
      dplyr::mutate(
        DEPARTAMENTO = dplyr::coalesce(DEPARTAMENTO, DEPTO_N),
        DEPTO_TC     = shorten_depto_lbl(to_title(DEPARTAMENTO)),
        etq          = paste0(
          "<b>", DEPTO_TC, "</b><br>Prevalencia: ",
          ifelse(is.finite(prev), fmt_pct1_co(prev), "Sin dato")
        )
      )
    
    pal  <- make_pal_fixed(shp$prev, COLS_MAP)
    bins <- attr(pal, "bins")
    info_html <- build_info_html(bins, percent = TRUE)
    session$sendCustomMessage(
      type    = "updateQuantilesTooltipUltra",
      message = list(html = as.character(info_html))
    )
    
    lab_legend <- if (modo == "bin") "Prevalencia (%)" else "Porcentaje (%)"
    
    if (sum(is.finite(shp$prev)) == 0) {
      prx %>% addPolygons(
        data  = shp, layerId = ~COD_DPTO2,
        fillColor = "#f0f0f0",
        color = "#666", weight = .7, fillOpacity = .7,
        label = ~lapply(etq, HTML)
      )
    } else {
      prx %>% addPolygons(
        data  = shp, layerId = ~COD_DPTO2,
        fillColor = ~pal(prev),
        color = "#666", weight = .7, fillOpacity = .9,
        label = ~lapply(etq, HTML),
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) %>%
        leaflet::addLegend(
          "bottomright",
          pal      = pal,
          values   = shp$prev,
          title    = lab_legend,
          opacity  = .9,
          labFormat = legend_fmt_pct1
        )
    }
    
    if (!is.null(dep_sel) && dep_sel != "Todos") {
      dep_row <- dep_sf %>% dplyr::filter(norm_cmp(DEPTO_N) == norm_cmp(dep_sel))
      if (nrow(dep_row) > 0) {
        prx %>% addPolygons(
          data  = dep_row, fill = FALSE,
          color = "#111", weight = 2.2, opacity = 1
        )
        fit_bounds_proxy(prx, dep_row)
      }
    }
  })
  
  ts_prev_ultra_plot <- reactive({
    modo <- ultra_mode()
    if (modo == "bin") {
      d  <- base_ts_bin();  req(nrow(d) > 0)
      ev <- mk_event_bin(d, input$f_ind_bin %||% "c_azucaradas")
    } else {
      d  <- base_ts_freq(); req(nrow(d) > 0)
      ev <- mk_event_freq_single(d, input$f_ind_freq_key)
    }
    
    dt <- d %>%
      dplyr::mutate(evento = as.numeric(ev)) %>%
      dplyr::group_by(anio) %>%
      dplyr::summarise(prev = w_prop(evento, fexp) * 100, .groups = "drop") %>%
      dplyr::filter(!is.na(anio), is.finite(prev)) %>%
      dplyr::arrange(anio) %>%
      dplyr::mutate(prev_txt = fmt_pct1_co(prev))
    
    lab_y <- if (modo == "bin") "Prevalencia (%)" else "Porcentaje (%)"
    
    plot_ly(
      dt, x = ~anio, y = ~prev,
      type = "scatter", mode = "lines+markers",
      text = ~prev_txt,
      hovertemplate = "Año: %{x}<br>Promedio: %{text}<extra></extra>",
      line   = list(color = COL_BAR),
      marker = list(color = COL_BAR)
    ) %>%
      layout(
        xaxis = list(
          title    = "",
          dtick    = 1,
          tickmode = "linear",
          showgrid = FALSE
        ),
        yaxis = list(
          title     = lab_y,
          rangemode = "tozero",
          showgrid  = TRUE,
          gridcolor = "#e5e7eb"
        ),
        margin = list(l = 60, r = 10, t = 10, b = 40),
        paper_bgcolor = "#ffffff",
        plot_bgcolor  = "#ffffff"
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })
  
  output$ts_prev_ultra <- renderPlotly({ ts_prev_ultra_plot() })
  
  output$ts_label_ultra <- renderUI({
    modo <- ultra_mode()
    if (modo == "bin") {
      HTML('<div style="color:#6b7280;font-size:12px;margin-bottom:4px"></div>')
    } else {
      lk <- lookup_freq(input$f_ind_freq_key)
      HTML(sprintf(
        '<div style="color:#6b7280;font-size:12px;margin-bottom:4px"><br/><span style="font-weight:600"></span></div>',
        htmlEscape(lk$label)
      ))
    }
  })
  
  bars_all_ultra_plot <- reactive({
    modo <- ultra_mode()
    if (modo == "bin") {
      d  <- base_anio_bin();  req(nrow(d) > 0)
      ev <- mk_event_bin(d, input$f_ind_bin %||% "c_azucaradas")
    } else {
      d  <- base_anio_freq(); req(nrow(d) > 0)
      ev <- mk_event_freq_single(d, input$f_ind_freq_key)
    }
    
    dd <- d %>%
      dplyr::mutate(evento = as.numeric(ev)) %>%
      dplyr::group_by(DEPARTAMENTO) %>%
      dplyr::summarise(prev = w_prop(evento, fexp) * 100, .groups = "drop") %>%
      dplyr::filter(is.finite(prev)) %>%
      dplyr::arrange(dplyr::desc(prev)) %>%
      dplyr::mutate(
        Departamento_lbl = shorten_depto_lbl(to_title(DEPARTAMENTO)),
        Departamento     = factor(Departamento_lbl, levels = rev(Departamento_lbl)),
        prev_lab         = fmt_num_co(prev, digits = 1)
      )
    
    lab_x <- if (modo == "bin") "Prevalencia (%)" else "Porcentaje (%)"
    
    plot_ly(
      dd,
      x = ~prev, y = ~Departamento,
      type = "bar", orientation = "h",
      text = ~prev_lab,
      textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(color = "white", size=14),
      hovertemplate = "<b>%{y}</b><br>Valor: %{text}%<extra></extra>",
      marker = list(color = COL_BAR)
    ) %>%
      layout(
        xaxis = list(title = lab_x, zeroline = FALSE),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 140, r = 10, t = 10, b = 40),
        paper_bgcolor = "#ffffff",
        plot_bgcolor  = "#ffffff"
      )
  })
  
  output$bars_all_ultra <- renderPlotly({ bars_all_ultra_plot() })
  
  # ---------------- SNAPSHOT PARA PDF ----------------
  snapshot_inputs_pdf <- function(){
    list(
      anio3       = isolate(input$anio3),
      f_dep3      = isolate(input$f_dep3),
      f_clase3    = isolate(input$f_clase3),
      f_sexo3     = isolate(input$f_sexo3),
      f_ind_fies  = isolate(input$f_ind_fies),
      
      anio           = isolate(input$anio),
      f_dep          = isolate(input$f_dep),
      f_clase        = isolate(input$f_clase),
      f_sexo         = isolate(input$f_sexo),
      f_ind_bin      = isolate(input$f_ind_bin),
      f_ind_freq_key = isolate(input$f_ind_freq_key),
      f_view_ultra   = isolate(input$f_view_ultra)
    )
  }
  
  base_anio_fies_snapshot <- function(snap){
    req(snap$anio3, snap$f_ind_fies)
    d <- ecv %>% dplyr::filter(anio == as.integer(snap$anio3))
    if (!is.null(snap$f_dep3) && snap$f_dep3 != "Todos") {
      d <- d %>% dplyr::filter(DEPARTAMENTO == snap$f_dep3)
    }
    apply_common_filters(d, snap$f_sexo3, NULL, NULL, clase = snap$f_clase3)
  }
  
  base_ts_fies_snapshot <- function(snap){
    req(snap$f_ind_fies)
    d <- ecv
    apply_common_filters(d, snap$f_sexo3, NULL, NULL, clase = snap$f_clase3)
  }
  
  ultra_mode_snapshot <- function(snap){
    snap$f_view_ultra %||% "bin"
  }
  
  mk_event_bin_snapshot <- function(d, ind){
    v <- suppressWarnings(as.integer(numish(d[[ind]])))
    ifelse(is.na(v), NA_real_, as.numeric(v == 1L))
  }
  
  base_anio_bin_snapshot <- function(snap){
    req(snap$anio)
    d <- ecv %>% dplyr::filter(anio == as.integer(snap$anio))
    if (!is.null(snap$f_dep) && snap$f_dep != "Todos") {
      d <- d %>% dplyr::filter(DEPARTAMENTO == snap$f_dep)
    }
    apply_common_filters(d, snap$f_sexo, NULL, snap$f_ind_bin, clase = snap$f_clase)
  }
  
  base_ts_bin_snapshot <- function(snap){
    d <- ecv
    apply_common_filters(d, snap$f_sexo, NULL, snap$f_ind_bin, clase = snap$f_clase)
  }
  
  base_anio_freq_snapshot <- function(snap){
    req(snap$anio, snap$f_ind_freq_key, snap$f_ind_bin)
    ind_var <- lookup_freq(snap$f_ind_freq_key)$var
    
    d <- ecv %>% dplyr::filter(anio == as.integer(snap$anio))
    if (!is.null(snap$f_dep) && snap$f_dep != "Todos") {
      d <- d %>% dplyr::filter(DEPARTAMENTO == snap$f_dep)
    }
    
    d <- apply_common_filters(d, snap$f_sexo, NULL, ind_var, clase = snap$f_clase)
    
    yes_code <- 1L
    c_var <- if (ind_var == "f_azucaradas") "c_azucaradas" else "c_paquetes"
    v_c <- suppressWarnings(as.integer(numish(d[[c_var]])))
    d <- d[is.na(v_c) | v_c == yes_code, , drop = FALSE]
    d
  }
  
  base_ts_freq_snapshot <- function(snap){
    req(snap$f_ind_freq_key, snap$f_ind_bin)
    ind_var <- lookup_freq(snap$f_ind_freq_key)$var
    
    d <- ecv
    d <- apply_common_filters(d, snap$f_sexo, NULL, ind_var, clase = snap$f_clase)
    
    yes_code <- 1L
    c_var <- if (ind_var == "f_azucaradas") "c_azucaradas" else "c_paquetes"
    v_c <- suppressWarnings(as.integer(numish(d[[c_var]])))
    d <- d[is.na(v_c) | v_c == yes_code, , drop = FALSE]
    d
  }
  
  # ---------------- BUILDERS SNAPSHOT PARA PDF ----------------
  build_map_widget_export_fies_snapshot <- function(snap){
    d <- base_anio_fies_snapshot(snap); req(nrow(d) > 0)
    ev <- mk_event_fies(d, snap$f_ind_fies)
    
    dd <- d %>%
      dplyr::mutate(evento = as.numeric(ev)) %>%
      dplyr::group_by(COD_DANE_DPTO2) %>%
      dplyr::summarise(prev = w_prop(evento, fexp) * 100, .groups = "drop")
    
    shp <- dep_sf %>%
      dplyr::left_join(dd, by = c("COD_DPTO2" = "COD_DANE_DPTO2"))
    
    dep_sel <- snap$f_dep3 %||% "Todos"
    if (!is.null(dep_sel) && dep_sel != "Todos") {
      g <- dep_sf %>% dplyr::filter(norm_cmp(DEPTO_N) == norm_cmp(dep_sel))
      bb <- if (nrow(g) > 0) sf::st_bbox(g) else sf::st_bbox(dep_sf)
    } else {
      bb <- sf::st_bbox(dep_sf)
    }
    
    pal  <- make_pal_fixed(shp$prev, COLS_MAP)
    lng  <- mean(c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"])))
    lat  <- mean(c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"])))
    zoom <- zoom_from_bbox(bb)
    
    leaflet(options = leafletOptions(minZoom = 5, maxZoom = 12, zoomSnap = 0.25)) %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(crossOrigin = TRUE)
      ) %>%
      setView(lng = lng, lat = lat, zoom = zoom) %>%
      addPolygons(
        data = shp,
        fillColor = ~pal(prev),
        weight = 0.7,
        color = "#666",
        fillOpacity = 0.9
      ) %>%
      addLegend(
        position = "bottomright",
        pal      = pal,
        values   = shp$prev,
        title    = "Prevalencia (%)",
        opacity  = 0.9,
        labFormat = legend_fmt_pct1
      ) %>%
      htmlwidgets::onRender("function(el,x){ this.zoomControl.setPosition('topright'); }")
  }
  
  build_ts_fies_plot_snapshot <- function(snap){
    d <- base_ts_fies_snapshot(snap); req(nrow(d) > 0)
    ev <- mk_event_fies(d, snap$f_ind_fies)
    
    dt <- d %>%
      dplyr::mutate(evento = as.numeric(ev)) %>%
      dplyr::group_by(anio) %>%
      dplyr::summarise(prev = w_prop(evento, fexp) * 100, .groups = "drop") %>%
      dplyr::filter(!is.na(anio), is.finite(prev)) %>%
      dplyr::arrange(anio) %>%
      dplyr::mutate(prev_txt = fmt_pct1_co(prev))
    
    plot_ly(
      dt, x = ~anio, y = ~prev,
      type = "scatter", mode = "lines+markers",
      text = ~prev_txt,
      hovertemplate = "Año: %{x}<br>Promedio: %{text}<extra></extra>",
      line   = list(color = COL_BAR),
      marker = list(color = COL_BAR)
    ) %>%
      layout(
        xaxis = list(title = "", dtick = 1, tickmode = "linear", showgrid = FALSE),
        yaxis = list(title = "Prevalencia (%)", rangemode = "tozero", showgrid = TRUE, gridcolor = "#e5e7eb"),
        margin = list(l = 60, r = 10, t = 10, b = 40),
        paper_bgcolor = "#ffffff",
        plot_bgcolor  = "#ffffff"
      ) %>%
      plotly::config(displayModeBar = FALSE)
  }
  
  build_bars_fies_plot_snapshot <- function(snap){
    d <- base_anio_fies_snapshot(snap); req(nrow(d) > 0)
    ev <- mk_event_fies(d, snap$f_ind_fies)
    
    dd <- d %>%
      dplyr::mutate(evento = as.numeric(ev)) %>%
      dplyr::group_by(DEPARTAMENTO) %>%
      dplyr::summarise(prev = w_prop(evento, fexp) * 100, .groups = "drop") %>%
      dplyr::filter(is.finite(prev)) %>%
      dplyr::arrange(dplyr::desc(prev)) %>%
      dplyr::mutate(
        Departamento_lbl = shorten_depto_lbl(to_title(DEPARTAMENTO)),
        Departamento     = factor(Departamento_lbl, levels = rev(Departamento_lbl)),
        prev_lab         = fmt_num_co(prev, digits = 1)
      )
    
    plot_ly(
      dd,
      x = ~prev, y = ~Departamento,
      type = "bar", orientation = "h",
      text = ~prev_lab,
      textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(color = "white", size=14),
      hovertemplate = "<b>%{y}</b><br>Prevalencia: %{text}%<extra></extra>",
      marker = list(color = COL_BAR)
    ) %>%
      layout(
        xaxis = list(title = "Prevalencia (%)", zeroline = FALSE),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 140, r = 10, t = 10, b = 40),
        paper_bgcolor = "#ffffff",
        plot_bgcolor  = "#ffffff"
      )
  }
  
  build_map_widget_export_ultra_snapshot <- function(snap){
    modo <- ultra_mode_snapshot(snap)
    
    if (modo == "bin") {
      d  <- base_anio_bin_snapshot(snap); req(nrow(d) > 0)
      ev <- mk_event_bin_snapshot(d, snap$f_ind_bin %||% "c_azucaradas")
      lab_legend <- "Prevalencia (%)"
    } else {
      d  <- base_anio_freq_snapshot(snap); req(nrow(d) > 0)
      ev <- mk_event_freq_single(d, snap$f_ind_freq_key)
      lab_legend <- "Porcentaje (%)"
    }
    
    dd <- d %>%
      dplyr::mutate(evento = as.numeric(ev)) %>%
      dplyr::group_by(COD_DANE_DPTO2) %>%
      dplyr::summarise(prev = w_prop(evento, fexp) * 100, .groups = "drop")
    
    shp <- dep_sf %>%
      dplyr::left_join(dd, by = c("COD_DPTO2" = "COD_DANE_DPTO2"))
    
    dep_sel <- snap$f_dep %||% "Todos"
    if (!is.null(dep_sel) && dep_sel != "Todos") {
      g <- dep_sf %>% dplyr::filter(norm_cmp(DEPTO_N) == norm_cmp(dep_sel))
      bb <- if (nrow(g) > 0) sf::st_bbox(g) else sf::st_bbox(dep_sf)
    } else {
      bb <- sf::st_bbox(dep_sf)
    }
    
    pal  <- make_pal_fixed(shp$prev, COLS_MAP)
    lng  <- mean(c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"])))
    lat  <- mean(c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"])))
    zoom <- zoom_from_bbox(bb)
    
    leaflet(options = leafletOptions(minZoom = 5, maxZoom = 12, zoomSnap = 0.25)) %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(crossOrigin = TRUE)
      ) %>%
      setView(lng = lng, lat = lat, zoom = zoom) %>%
      addPolygons(
        data = shp,
        fillColor = ~pal(prev),
        weight = 0.7,
        color = "#666",
        fillOpacity = 0.9
      ) %>%
      addLegend(
        position = "bottomright",
        pal      = pal,
        values   = shp$prev,
        title    = lab_legend,
        opacity  = 0.9,
        labFormat = legend_fmt_pct1
      ) %>%
      htmlwidgets::onRender("function(el,x){ this.zoomControl.setPosition('topright'); }")
  }
  
  build_ts_ultra_plot_snapshot <- function(snap){
    modo <- ultra_mode_snapshot(snap)
    if (modo == "bin") {
      d  <- base_ts_bin_snapshot(snap); req(nrow(d) > 0)
      ev <- mk_event_bin_snapshot(d, snap$f_ind_bin %||% "c_azucaradas")
      lab_y <- "Prevalencia (%)"
    } else {
      d  <- base_ts_freq_snapshot(snap); req(nrow(d) > 0)
      ev <- mk_event_freq_single(d, snap$f_ind_freq_key)
      lab_y <- "Porcentaje (%)"
    }
    
    dt <- d %>%
      dplyr::mutate(evento = as.numeric(ev)) %>%
      dplyr::group_by(anio) %>%
      dplyr::summarise(prev = w_prop(evento, fexp) * 100, .groups = "drop") %>%
      dplyr::filter(!is.na(anio), is.finite(prev)) %>%
      dplyr::arrange(anio) %>%
      dplyr::mutate(prev_txt = fmt_pct1_co(prev))
    
    plot_ly(
      dt, x = ~anio, y = ~prev,
      type = "scatter", mode = "lines+markers",
      text = ~prev_txt,
      hovertemplate = "Año: %{x}<br>Promedio: %{text}<extra></extra>",
      line   = list(color = COL_BAR),
      marker = list(color = COL_BAR)
    ) %>%
      layout(
        xaxis = list(title = "", dtick = 1, tickmode = "linear", showgrid = FALSE),
        yaxis = list(title = lab_y, rangemode = "tozero", showgrid = TRUE, gridcolor = "#e5e7eb"),
        margin = list(l = 60, r = 10, t = 10, b = 40),
        paper_bgcolor = "#ffffff",
        plot_bgcolor  = "#ffffff"
      ) %>%
      plotly::config(displayModeBar = FALSE)
  }
  
  build_bars_ultra_plot_snapshot <- function(snap){
    modo <- ultra_mode_snapshot(snap)
    if (modo == "bin") {
      d  <- base_anio_bin_snapshot(snap); req(nrow(d) > 0)
      ev <- mk_event_bin_snapshot(d, snap$f_ind_bin %||% "c_azucaradas")
      lab_x <- "Prevalencia (%)"
    } else {
      d  <- base_anio_freq_snapshot(snap); req(nrow(d) > 0)
      ev <- mk_event_freq_single(d, snap$f_ind_freq_key)
      lab_x <- "Porcentaje (%)"
    }
    
    dd <- d %>%
      dplyr::mutate(evento = as.numeric(ev)) %>%
      dplyr::group_by(DEPARTAMENTO) %>%
      dplyr::summarise(prev = w_prop(evento, fexp) * 100, .groups = "drop") %>%
      dplyr::filter(is.finite(prev)) %>%
      dplyr::arrange(dplyr::desc(prev)) %>%
      dplyr::mutate(
        Departamento_lbl = shorten_depto_lbl(to_title(DEPARTAMENTO)),
        Departamento     = factor(Departamento_lbl, levels = rev(Departamento_lbl)),
        prev_lab         = fmt_num_co(prev, digits = 1)
      )
    
    plot_ly(
      dd,
      x = ~prev, y = ~Departamento,
      type = "bar", orientation = "h",
      text = ~prev_lab,
      textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(color = "white", size=14),
      hovertemplate = "<b>%{y}</b><br>Valor: %{text}%<extra></extra>",
      marker = list(color = COL_BAR)
    ) %>%
      layout(
        xaxis = list(title = lab_x, zeroline = FALSE),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 140, r = 10, t = 10, b = 40),
        paper_bgcolor = "#ffffff",
        plot_bgcolor  = "#ffffff"
      )
  }
  
  # DESCARGAS PNG INDIVIDUALES
  output$dl_ts_fies <- downloadHandler(
    filename = function() sprintf("serie_fies_%s.png", Sys.Date()),
    content = function(file){
      ok <- save_widget_png_retry(
        as_widget(ts_prev_fies_plot()), file,
        vwidth = PNG_VWIDTH_PLOT, vheight = PNG_VHEIGHT_TS, delay_base = PNG_DELAY_PLOT
      )
      if (!ok) stop("No se pudo generar el PNG de la serie FIES.")
    }
  )
  
  output$dl_bars_fies <- downloadHandler(
    filename = function() sprintf("ranking_fies_%s.png", Sys.Date()),
    content = function(file){
      ok <- save_widget_png_retry(
        as_widget(bars_all_fies_plot()), file,
        vwidth = PNG_VWIDTH_PLOT, vheight = PNG_VHEIGHT_BAR, delay_base = PNG_DELAY_PLOT
      )
      if (!ok) stop("No se pudo generar el PNG del ranking FIES.")
    }
  )
  
  output$dl_csv_fies <- downloadHandler(
    filename = function() sprintf("datos_fies_%s.csv", Sys.Date()),
    content = function(file){
      d <- base_anio_fies(); req(nrow(d) > 0)
      ev <- mk_event_fies(d, input$f_ind_fies)
      
      dd <- d %>%
        dplyr::mutate(evento = as.numeric(ev)) %>%
        dplyr::group_by(DEPARTAMENTO) %>%
        dplyr::summarise(
          prevalencia = w_prop(evento, fexp) * 100,
          .groups = "drop"
        ) %>%
        dplyr::filter(is.finite(prevalencia)) %>%
        dplyr::arrange(dplyr::desc(prevalencia)) %>%
        dplyr::mutate(
          Departamento = shorten_depto_lbl(to_title(DEPARTAMENTO))
        ) %>%
        dplyr::select(Departamento, prevalencia)
      
      readr::write_csv(dd, file)
    }
  )
  
  output$dl_ts_ultra <- downloadHandler(
    filename = function() sprintf("serie_ultraprocesados_%s.png", Sys.Date()),
    content = function(file){
      ok <- save_widget_png_retry(
        as_widget(ts_prev_ultra_plot()), file,
        vwidth = PNG_VWIDTH_PLOT, vheight = PNG_VHEIGHT_TS, delay_base = PNG_DELAY_PLOT
      )
      if (!ok) stop("No se pudo generar el PNG de la serie Ultra.")
    }
  )
  
  output$dl_bars_ultra <- downloadHandler(
    filename = function() sprintf("ranking_ultraprocesados_%s.png", Sys.Date()),
    content = function(file){
      ok <- save_widget_png_retry(
        as_widget(bars_all_ultra_plot()), file,
        vwidth = PNG_VWIDTH_PLOT, vheight = PNG_VHEIGHT_BAR, delay_base = PNG_DELAY_PLOT
      )
      if (!ok) stop("No se pudo generar el PNG del ranking Ultra.")
    }
  )
  
  output$dl_mapa_fies <- downloadHandler(
    filename = function() sprintf("mapa_fies_%s.png", Sys.Date()),
    content = function(file){
      snap <- snapshot_inputs_pdf()
      ok <- save_widget_png_retry(
        build_map_widget_export_fies_snapshot(snap), file,
        vwidth = PNG_VWIDTH_MAP, vheight = PNG_VHEIGHT_MAP, delay_base = PNG_DELAY_MAP
      )
      if (!ok) stop("No se pudo generar el PNG del mapa FIES.")
    }
  )
  
  output$dl_mapa_ultra <- downloadHandler(
    filename = function() sprintf("mapa_ultraprocesados_%s.png", Sys.Date()),
    content = function(file){
      snap <- snapshot_inputs_pdf()
      ok <- save_widget_png_retry(
        build_map_widget_export_ultra_snapshot(snap), file,
        vwidth = PNG_VWIDTH_MAP, vheight = PNG_VHEIGHT_MAP, delay_base = PNG_DELAY_MAP
      )
      if (!ok) stop("No se pudo generar el PNG del mapa Ultra.")
    }
  )
  
  # INFORME DEL TABLERO COMPLETO
  render_informe_completo <- function(file){
    
    if (!file.exists(ruta_rmd)) {
      stop("No encuentro Informe_descargable.Rmd en la raíz del proyecto.")
    }
    
    snap <- snapshot_inputs_pdf()
    
    ok_fm <- save_widget_png_retry(
      build_map_widget_export_fies_snapshot(snap), IMG_FIES_MAP,
      vwidth = PNG_VWIDTH_MAP, vheight = PNG_VHEIGHT_MAP, delay_base = PNG_DELAY_MAP
    )
    ok_fs <- save_widget_png_retry(
      as_widget(build_ts_fies_plot_snapshot(snap)), IMG_FIES_TS,
      vwidth = PNG_VWIDTH_PLOT, vheight = PNG_VHEIGHT_TS, delay_base = PNG_DELAY_PLOT
    )
    ok_fb <- save_widget_png_retry(
      as_widget(build_bars_fies_plot_snapshot(snap)), IMG_FIES_BAR,
      vwidth = PNG_VWIDTH_PLOT, vheight = PNG_VHEIGHT_BAR, delay_base = PNG_DELAY_PLOT
    )
    
    ok_um <- save_widget_png_retry(
      build_map_widget_export_ultra_snapshot(snap), IMG_ULTRA_MAP,
      vwidth = PNG_VWIDTH_MAP, vheight = PNG_VHEIGHT_MAP, delay_base = PNG_DELAY_MAP
    )
    ok_us <- save_widget_png_retry(
      as_widget(build_ts_ultra_plot_snapshot(snap)), IMG_ULTRA_TS,
      vwidth = PNG_VWIDTH_PLOT, vheight = PNG_VHEIGHT_TS, delay_base = PNG_DELAY_PLOT
    )
    ok_ub <- save_widget_png_retry(
      as_widget(build_bars_ultra_plot_snapshot(snap)), IMG_ULTRA_BAR,
      vwidth = PNG_VWIDTH_PLOT, vheight = PNG_VHEIGHT_BAR, delay_base = PNG_DELAY_PLOT
    )
    
    if (!ok_fm) stop("No se pudo generar Descargas/ecv_fies_mapa.png")
    if (!ok_fs) stop("No se pudo generar Descargas/ecv_fies_serie.png")
    if (!ok_fb) stop("No se pudo generar Descargas/ecv_fies_ranking.png")
    if (!ok_um) stop("No se pudo generar Descargas/ecv_ultra_mapa.png")
    if (!ok_us) stop("No se pudo generar Descargas/ecv_ultra_serie.png")
    if (!ok_ub) stop("No se pudo generar Descargas/ecv_ultra_ranking.png")
    
    ind_fies_lbl <- safe_first(names(inds_fies)[inds_fies == (snap$f_ind_fies %||% "p_suficiente_a")],
                               snap$f_ind_fies %||% "p_suficiente_a")
    
    filtros_tbl_fies <- data.frame(
      Parametro = c("Año", "Departamento", "Área", "Sexo", "Indicador"),
      Valor = c(
        as.character(snap$anio3 %||% ""),
        as.character(snap$f_dep3 %||% ""),
        as.character(snap$f_clase3 %||% ""),
        as.character(snap$f_sexo3 %||% ""),
        as.character(ind_fies_lbl)
      ),
      stringsAsFactors = FALSE
    )
    
    modo_now <- ultra_mode_snapshot(snap)
    ind_ultra_lbl <- if (modo_now == "bin") {
      safe_first(names(inds_binarios)[inds_binarios == (snap$f_ind_bin %||% "c_azucaradas")],
                 snap$f_ind_bin %||% "c_azucaradas")
    } else {
      lookup_freq(snap$f_ind_freq_key)$label %||% "Frecuencia"
    }
    
    filtros_tbl_ultra <- data.frame(
      Parametro = c("Año", "Departamento", "Área", "Sexo", "Modo", "Indicador"),
      Valor = c(
        as.character(snap$anio %||% ""),
        as.character(snap$f_dep %||% ""),
        as.character(snap$f_clase %||% ""),
        as.character(snap$f_sexo %||% ""),
        ifelse(modo_now == "bin", "Presencia de consumo", "Frecuencia de consumo"),
        as.character(ind_ultra_lbl)
      ),
      stringsAsFactors = FALSE
    )
    
    logo_src <- file.path(app_root, "www", "LOGO_PLATEA.png")
    if (!file.exists(logo_src)) {
      logo_src2 <- file.path(app_root, "WWW", "LOGO_PLATEA.png")
      logo_src  <- if (file.exists(logo_src2)) logo_src2 else NA_character_
    }
    logo_dst <- file.path(EXPORT_DIR, "LOGO_PLATEA.png")
    if (!is.na(logo_src) && file.exists(logo_src)) file.copy(logo_src, logo_dst, overwrite = TRUE)
    
    rmarkdown::render(
      input         = ruta_rmd,
      output_format = "pdf_document",
      output_file   = basename(file),
      output_dir    = dirname(file),
      quiet         = TRUE,
      params        = list(
        app_root      = app_root,
        export_dir    = "Descargas",
        filtros_fies  = filtros_tbl_fies,
        filtros_ultra = filtros_tbl_ultra,
        
        img_fies_map      = basename(IMG_FIES_MAP),
        img_fies_serie    = basename(IMG_FIES_TS),
        img_fies_ranking  = basename(IMG_FIES_BAR),
        
        img_ultra_map     = basename(IMG_ULTRA_MAP),
        img_ultra_serie   = basename(IMG_ULTRA_TS),
        img_ultra_ranking = basename(IMG_ULTRA_BAR),
        
        csv_filtrado = NULL
      ),
      knit_root_dir = app_root,
      envir         = new.env(parent = globalenv())
    )
  }
  
  output$dl_reporte_pdf <- downloadHandler(
    filename = function(){
      paste0("Informe_descargable_ECV_tablero_completo_", Sys.Date(), ".pdf")
    },
    content = function(file){
      render_informe_completo(file)
    },
    contentType = "application/pdf"
  )
  
  output$dl_reporte_pdf2 <- downloadHandler(
    filename = function(){
      paste0("Informe_descargable_ECV_tablero_completo_", Sys.Date(), ".pdf")
    },
    content = function(file){
      render_informe_completo(file)
    },
    contentType = "application/pdf"
  )
}

shinyApp(ui, server)