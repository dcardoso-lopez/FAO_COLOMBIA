# app.R — ECV Consumo (c_ = binarios, f_ = frecuencias, FIES-8)
# Vista por departamentos y, si hay departamento elegido, vista por municipios del dpto (join por CODMUN).

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(dplyr); library(tidyr); library(readr); library(stringi); library(scales)
  library(leaflet); library(sf); library(htmltools); library(plotly); library(haven); library(stringr)
})
options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

# ---------------- Rutas ----------------
app_root <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
data_dir <- file.path(app_root, "data")
ruta_ecv     <- file.path(data_dir, "052_DANE_ECV.rds")
ruta_shp_dep <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")
ruta_shp_mun <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")

if (!file.exists(ruta_ecv)) stop("No se encuentra el archivo: ", ruta_ecv)
chk <- function(shp){
  b <- sub("\\.shp$","",shp)
  req <- paste0(b,c(".shp",".dbf",".shx",".prj"))
  req[!file.exists(req)]
}
if (length(chk(ruta_shp_dep))) stop("Faltan partes del SHP deptos en /data/shp (shp/dbf/shx/prj)")

# ---------------- Utils ----------------
`%||%` <- function(x,y) if (is.null(x)||length(x)==0) y else x
norm_txt <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
norm_cmp <- function(x) tolower(stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII"))

# Title Case robusto (muestra) — NO cambia los valores internos
to_title <- function(x){
  stringi::stri_trans_totitle(
    stringi::stri_trans_tolower(as.character(x), locale = "es"),
    opts_brkiter = stringi::stri_opts_brkiter(type="word")
  )
}

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
  ok <- is.finite(x) & is.finite(w) & w>0
  if(!any(ok)) return(NA_real_)
  sum(w[ok]*x[ok]) / sum(w[ok])
}
norm_dep2 <- function(x){
  x <- gsub("\\D","",as.character(x)); x[nchar(x)==0] <- NA
  stringi::stri_pad_left(x,2,"0")
}
norm_mun5 <- function(x){
  x <- gsub("\\D","",as.character(x)); x[nchar(x)==0] <- NA
  stringi::stri_pad_left(x,5,"0")
}

# ---- Colores corporativos ----
COL_BAR   <- "#f57c00"
COLS_MAP  <- c("#9c4a00", "#e6550d", "#fa8916", "#fa8916", "#ffe0cc")

# Paleta robusta para evitar "breaks no únicos"
make_pal_fixed <- function(values, colors = COLS_MAP){
  vals <- suppressWarnings(as.numeric(values))
  vals <- vals[is.finite(vals)]
  n <- length(colors)
  if (length(vals) == 0) {
    return(leaflet::colorBin(palette = colors, domain = c(0,1), bins = c(0,1), na.color = "#f0f0f0", right = FALSE))
  }
  rng <- range(vals, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2])) {
    bins <- c(0, 1)
  } else if (abs(diff(rng)) < .Machine$double.eps * 10) {
    v <- rng[1]; bins <- c(v - 0.5, v + 0.5)
  } else {
    qs <- suppressWarnings(quantile(vals, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE, type = 7))
    bins <- unique(as.numeric(qs))
    if (length(bins) < 2) bins <- seq(rng[1], rng[2], length.out = n + 1)
    eps <- max(1e-12, diff(range(bins))/1e9)
    for (i in 2:length(bins)) if (bins[i] <= bins[i-1]) bins[i] <- bins[i-1] + eps
  }
  leaflet::colorBin(palette = colors, domain = vals, bins = bins, na.color = "#f0f0f0", right = FALSE)
}

fit_bounds_proxy <- function(proxy, geom){
  if (is.null(geom) || nrow(geom)==0) return(invisible(proxy))
  bb <- sf::st_bbox(geom)
  leaflet::fitBounds(proxy, bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
}

# --- Helper robusto: devuelve columna INT; si no existe, NA del tamaño del df ---
col_or_na_int <- function(df, candidates){
  n  <- nrow(df)
  nm <- tolower(names(df))
  cand <- tolower(candidates)
  for (c in cand){
    hit <- which(nm == c)
    if (length(hit)) return(suppressWarnings(as.integer(numish(df[[hit[1]]]))))
  }
  nm_norm   <- gsub("_|\\s", "", nm)
  cand_norm <- unique(gsub("_|\\s", "", cand))
  for (c in cand_norm){
    hit <- which(nm_norm == c)
    if (length(hit)) return(suppressWarnings(as.integer(numish(df[[hit[1]]]))))
  }
  rep(NA_integer_, n)
}

# ---------------- Cargar base ----------------
ecv_raw <- readRDS(ruta_ecv)
names(ecv_raw) <- tolower(names(ecv_raw))

# Campos mínimos (robustos en nombre)
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

need <- c(col_ano,col_dep_cod,col_dep_nom,col_sexo,col_edad,col_w)
if (any(is.na(need))) stop("Faltan columnas clave en ECV (anio, cod_dane_dpto_d, departamento_d, sexo/p6020, p6040, factor).")

# Detectar variables c_ y f_
col_c_azuc <- names(ecv_raw)[grepl("^c_.*azucar", names(ecv_raw))]
col_c_paq  <- names(ecv_raw)[grepl("^c_.*paq|^c_.*paque", names(ecv_raw))]
col_c_azuc <- if (length(col_c_azuc)) col_c_azuc[1] else NA_character_
col_c_paq  <- if (length(col_c_paq))  col_c_paq[1]  else NA_character_

col_f_azuc <- names(ecv_raw)[grepl("^f_.*azucar", names(ecv_raw))]
col_f_paq  <- names(ecv_raw)[grepl("^f_.*paq|^f_.*paque", names(ecv_raw))]
col_f_azuc <- if (length(col_f_azuc)) col_f_azuc[1] else NA_character_
col_f_paq  <- if (length(col_f_paq))  col_f_paq[1]  else NA_character_

# CLASE exacta (1 = Cabecera; 2 = Centros poblados y rural disperso)
mk_clase_exact <- function(v){
  if (all(is.na(v))) return(rep(NA_character_, length(v)))
  vnum <- suppressWarnings(as.integer(numish(v)))
  out  <- ifelse(vnum == 1, "Cabecera municipal",
                 ifelse(vnum == 2, "Centros Poblados y Rural Disperso", NA_character_))
  need_txt <- is.na(out)
  if (any(need_txt)) {
    txt <- tolower(norm_txt(safe_as_char(v[need_txt])))
    lbl <- rep(NA_character_, length(txt))
    lbl[grepl("cabecera", txt)] <- "Cabecera municipal"
    lbl[grepl("centro|centros|poblado|poblados|rural|disperso", txt)] <- "Centros Poblados y Rural Disperso"
    out[need_txt] <- lbl
  }
  out
}

# Sexo: aceptar p6020 (1/2) o texto "Hombre/Mujer"
mk_sexo_lbl <- function(col_val){
  if (!is.null(col_sexo_txt) && !is.na(col_sexo_txt)) {
    s <- tolower(norm_txt(safe_as_char(col_val)))
    out <- ifelse(grepl("^hombre|^hombres|^m$", s), "Hombre",
                  ifelse(grepl("^mujer|^mujeres|^f$", s), "Mujer", "Sin dato"))
    return(out)
  } else {
    v <- numish(col_val)
    return(dplyr::case_when(v==1 ~ "Hombre", v==2 ~ "Mujer", TRUE ~ "Sin dato"))
  }
}

# Estructura base
ecv <- tibble::tibble(
  anio           = suppressWarnings(as.integer(numish(ecv_raw[[col_ano]]))),
  COD_DANE_DPTO2 = norm_dep2(ecv_raw[[col_dep_cod]]),
  DEPARTAMENTO   = norm_txt(ecv_raw[[col_dep_nom]]),
  COD_DANE_MPIO2 = if (!is.na(col_mun_cod)) norm_mun5(ecv_raw[[col_mun_cod]]) else NA_character_,
  MUNICIPIO      = if (!is.na(col_mun_nom)) norm_txt(ecv_raw[[col_mun_nom]]) else NA_character_,
  CLASE_LBL      = if (!is.na(col_clase)) mk_clase_exact(ecv_raw[[col_clase]]) else NA_character_,
  p6020_lbl      = mk_sexo_lbl(ecv_raw[[col_sexo]]),
  p6040          = numish(ecv_raw[[col_edad]]),
  fexp           = suppressWarnings(as.numeric(numish(ecv_raw[[col_w]]))),
  c_azucaradas   = if (!is.na(col_c_azuc)) suppressWarnings(as.integer(numish(ecv_raw[[col_c_azuc]]))) else NA_integer_,
  c_paquetes     = if (!is.na(col_c_paq))  suppressWarnings(as.integer(numish(ecv_raw[[col_c_paq]])))  else NA_integer_,
  f_azucaradas   = if (!is.na(col_f_azuc)) suppressWarnings(as.integer(numish(ecv_raw[[col_f_azuc]]))) else NA_integer_,
  f_paquetes     = if (!is.na(col_f_paq))  suppressWarnings(as.integer(numish(ecv_raw[[col_f_paq]])))  else NA_integer_,
  
  # ---- FIES-8 (robusto a nombres alternos) ----
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
    edad_grupo = cut(p6040, breaks=c(-Inf,11,17,26,40,59,Inf),
                     labels=c("0–11","12–17","18–26","27–40","41–59","60+"), right=TRUE)
  )

# ========= Alinear claves (VERSIÓN VECTORIZADA) =========
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
  )

# ---------------- Indicadores ----------------
inds_binarios <- c(
  "¿Consume bebidas azucaradas?" = "c_azucaradas",
  "¿Consume alimentos de paquete?" = "c_paquetes"
)
inds_aplican_12mas <- c("c_azucaradas", "f_azucaradas", "c_paquetes", "f_paquetes")

freq_labels <- c(
  "1 Todos los días de la semana (dos o más veces al día)",
  "2 Todos los días de la semana (una vez al día)",
  "3 Cuatro a seis veces a la semana",
  "4 Dos o tres veces a la semana",
  "5 Una vez a la semana",
  "6 Menos de una vez por semana"
)

freq_ind_map <- tibble::tibble(
  var  = rep(c("f_azucaradas","f_paquetes"), each=6),
  code = rep(1:6, times=2),
  label_base = rep(c("Bebidas azucaradas", "Paquetes"), each=6),
  label_cat  = rep(freq_labels, times=2)
) %>%
  mutate(
    key   = paste(var, code, sep=":"),
    label = paste0(label_base, " — ", label_cat)
  )
freq_choices_ind <- setNames(freq_ind_map$key, freq_ind_map$label)

# -------- FIES-8 --------
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
fies_choices <- c("1 — Sí" = 1, "2 — No" = 2)
mk_event_fies <- function(d, ind_var, yesno){
  v <- suppressWarnings(as.integer(numish(d[[ind_var]])))
  ifelse(v %in% c(1,2), as.numeric(v == as.integer(yesno)), NA_real_)
}

# ---------------- Shapes ----------------
dep_sf <- sf::st_read(ruta_shp_dep, quiet=TRUE) %>%
  dplyr::mutate(
    COD_DPTO2 = dplyr::coalesce(
      if ("DPTO_CCDGO" %in% names(.)) sprintf("%02d", suppressWarnings(as.integer(.data$DPTO_CCDGO))) else NA_character_,
      if ("COD_DEPTO"   %in% names(.)) sprintf("%02d", suppressWarnings(as.integer(.data$COD_DEPTO))) else NA_character_
    ),
    DEPTO_N = dplyr::coalesce(
      as.character(.$DEPARTAMENTO_D %||% NA),
      as.character(.$DPTO_CNMBR %||% NA),
      as.character(.$NOMBRE_DEPTO %||% COD_DPTO2))
  ) %>% sf::st_transform(4326) %>% sf::st_make_valid()

mun_sf <- NULL
if (file.exists(ruta_shp_mun) && length(chk(ruta_shp_mun))==0) {
  muni_name_cands <- c("MUNICIPIO_D","MPIO_CNMBR","NOMBRE_MPIO","NOMBRE_MUNICIP","MUNICIPIO","NOMBRE")
  dpto2_cands     <- c("DPTO_CCDGO","COD_DPTO","DPTO","CODIGO_DEPTO","DPTO_COD")
  
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
    sf::st_transform(4326) %>% sf::st_make_valid()
}

# ---------------- UI ----------------
ui <- fluidPage(
  theme = bs_theme(version=5, primary="#2563eb",
                   base_font=bslib::font_google("Inter"),
                   heading_font=bslib::font_google("Inter Tight")),
  tags$head(tags$style(HTML("
    .wrap{max-width:1200px;margin:0 auto;padding:16px 20px 32px;}
    .filters{
      background:#fff;border:1px solid #ffb366;border-radius:16px;
      padding:14px 16px;margin-bottom:12px;box-shadow:0 2px 10px rgba(0,0,0,.05)
    }
    .filters-grid-6{display:grid;grid-template-columns:repeat(6,minmax(170px,1fr));gap:12px}
    .filters-grid-2{display:grid;grid-template-columns:repeat(2,minmax(170px,1fr));gap:12px;margin-top:8px}
    .filter-label{font-size:12px;font-weight:600;letter-spacing:.4px;text-transform:uppercase;color:#6b7280;margin-bottom:6px}
    .grid-2{display:grid;grid-template-columns:1fr 1fr;gap:12px;align-items:start}
    .card{
      background:#fff;border:1px solid #ffb366;border-radius:16px;padding:14px;
      box-shadow:0 2px 10px rgba(0,0,0,.05)
    }
    .card-title{font-weight:700;font-size:16px;margin-bottom:8px}
    .kpi{display:flex;gap:16px;flex-wrap:wrap}
    .kpi .item{background:#f9fafb;border:1px solid #ffb366;border-radius:12px;padding:10px 12px;min-width:180px}
    .kpi .item b{display:block;font-size:20px}
    @media (max-width:1200px){ .filters-grid-6{grid-template-columns:repeat(3,minmax(160px,1fr));} }
    @media (max-width:800px){ .filters-grid-6{grid-template-columns:repeat(2,minmax(150px,1fr));}
                              .filters-grid-2{grid-template-columns:1fr;} }
  "))),
  div(class="wrap",
      h3("ECV — Historias de (in)seguridad alimentaria y consumo de ultra-procesados"),
      tabsetPanel(type="tabs", id="tabs",
                  
                  # -------- TAB 1 (PRIMERO): FIES con títulos storytelling --------
                  tabPanel("¿Quiénes la padecen la inseguridad alimentaria? (FIES-8)",
                           div(class="filters",
                               div(class="filters-grid-6",
                                   div(div(class="filter-label","¿Qué año miramos?"), uiOutput("anio_ui3")),
                                   div(div(class="filter-label","¿Dónde? (departamento)"), uiOutput("dep_ui3")),
                                   div(div(class="filter-label","¿Algún municipio en particular?"), uiOutput("mun_ui3")),
                                   div(div(class="filter-label","¿Área urbana o rural?"),
                                       pickerInput("f_clase3", NULL, multiple=TRUE,
                                                   choices=c("Cabecera municipal","Centros Poblados y Rural Disperso"),
                                                   selected=c("Cabecera municipal","Centros Poblados y Rural Disperso"),
                                                   options=list(`actions-box`=TRUE))),
                                   div(div(class="filter-label","¿Quiénes? (sexo)"),
                                       pickerInput("f_sexo3", NULL, multiple=TRUE,
                                                   choices=c("Hombre","Mujer"), selected=c("Hombre","Mujer"),
                                                   options=list(`actions-box`=TRUE))),
                                   div(div(class="filter-label","¿Qué edades?"),
                                       pickerInput("f_edad3", NULL, multiple=TRUE,
                                                   choices=levels(ecv$edad_grupo), selected=levels(ecv$edad_grupo),
                                                   options=list(`actions-box`=TRUE)))
                               ),
                               div(class="filters-grid-2",
                                   div(div(class="filter-label","Ítem FIES (la historia)"),
                                       selectInput("f_ind_fies", NULL, choices = inds_fies,
                                                   selected = "p_suficiente_a")),
                                   div(div(class="filter-label","Categoría a seguir"),
                                       selectInput("f_cat_fies", NULL, choices = c("1 — Sí"=1,"2 — No"=2), selected = 1))
                               )
                           ),
                           div(id="kpi_card_fies", class="card",
                               div(class="card-title","¿Cuántas personas lo vivieron?"),
                               htmlOutput("kpis_fies")),
                           div(class="grid-2",
                               div(
                                 div(class="card",
                                     div(class="card-title","¿Dónde se siente más?"),
                                     leafletOutput("mapa_fies", height = 320)),
                                 div(class="card", style="margin-top:12px",
                                     div(class="card-title","¿Cómo ha cambiado en el tiempo?"),
                                     div(style="margin-bottom:6px;",
                                         prettySwitch("ts_scope_fies", "Usar departamento seleccionado en la serie", FALSE, status="primary")),
                                     htmlOutput("ts_label_fies"),
                                     plotlyOutput("ts_prev_fies", height = 300))
                               ),
                               div(class="card",
                                   div(class="card-title","¿Quiénes lideran la prevalencia?"),
                                   plotlyOutput("bars_all_fies", height = "720px"))
                           )
                  ),
                  
                  # -------- TAB 2: BINARIOS (storytelling) --------
                  tabPanel("¿Qué tan presentes están los ultra-procesados?",
                           div(class="filters",
                               div(class="filters-grid-6",
                                   div(div(class="filter-label","¿Qué año miramos?"), uiOutput("anio_ui")),
                                   div(div(class="filter-label","¿Dónde? (departamento)"), uiOutput("dep_ui")),
                                   div(div(class="filter-label","¿Algún municipio en particular?"), uiOutput("mun_ui")),
                                   div(div(class="filter-label","¿Área urbana o rural?"),
                                       pickerInput("f_clase", NULL, multiple=TRUE,
                                                   choices=c("Cabecera municipal","Centros Poblados y Rural Disperso"),
                                                   selected=c("Cabecera municipal","Centros Poblados y Rural Disperso"),
                                                   options=list(`actions-box`=TRUE))),
                                   div(div(class="filter-label","¿Quiénes? (sexo)"),
                                       pickerInput("f_sexo", NULL, multiple=TRUE,
                                                   choices=c("Hombre","Mujer"), selected=c("Hombre","Mujer"),
                                                   options=list(`actions-box`=TRUE))),
                                   div(div(class="filter-label","¿Qué edades?"),
                                       pickerInput("f_edad", NULL, multiple=TRUE,
                                                   choices=levels(ecv$edad_grupo), selected=levels(ecv$edad_grupo),
                                                   options=list(`actions-box`=TRUE)))
                               ),
                               div(class="filters-grid-2",
                                   div(div(class="filter-label","¿Qué indicador quieres ver?"),
                                       selectInput("f_ind_bin", NULL, choices = inds_binarios, selected="c_azucaradas")),
                                   div(div(class="filter-label","¿Seguimos el “Sí” o el “No”?"), uiOutput("cat_ui_bin"))
                               )
                           ),
                           div(id="kpi_card_bin", class="card",
                               div(class="card-title","¿Qué tan común es a nivel nacional?"),
                               htmlOutput("kpis_bin")),
                           div(class="grid-2",
                               div(
                                 div(class="card",
                                     div(class="card-title","¿Dónde se consume más?"),
                                     leafletOutput("mapa_bin", height = 320)),
                                 div(class="card", style="margin-top:12px",
                                     div(class="card-title","¿Cómo ha cambiado el consumo?"),
                                     div(style="margin-bottom:6px;",
                                         prettySwitch("ts_scope_bin", "Usar departamento seleccionado en la serie", FALSE, status="primary")),
                                     htmlOutput("ts_label_bin"),
                                     plotlyOutput("ts_prev_bin", height = 300))
                               ),
                               div(class="card",
                                   div(class="card-title","¿Qué departamentos consumen más?"),
                                   plotlyOutput("bars_all_bin", height = "720px"))
                           )
                  ),
                  
                  # -------- TAB 3: FRECUENCIAS (storytelling) --------
                  tabPanel("¿Con qué frecuencia sucede?",
                           div(class="filters",
                               div(class="filters-grid-6",
                                   div(div(class="filter-label","¿Qué año miramos?"), uiOutput("anio_ui2")),
                                   div(div(class="filter-label","¿Dónde? (departamento)"), uiOutput("dep_ui2")),
                                   div(div(class="filter-label","¿Algún municipio en particular?"), uiOutput("mun_ui2")),
                                   div(div(class="filter-label","¿Área urbana o rural?"),
                                       pickerInput("f_clase2", NULL, multiple=TRUE,
                                                   choices=c("Cabecera municipal","Centros Poblados y Rural Disperso"),
                                                   selected=c("Cabecera municipal","Centros Poblados y Rural Disperso"),
                                                   options=list(`actions-box`=TRUE))),
                                   div(div(class="filter-label","¿Quiénes? (sexo)"),
                                       pickerInput("f_sexo2", NULL, multiple=TRUE,
                                                   choices=c("Hombre","Mujer"), selected=c("Hombre","Mujer"),
                                                   options=list(`actions-box`=TRUE))),
                                   div(div(class="filter-label","¿Qué edades?"),
                                       pickerInput("f_edad2", NULL, multiple=TRUE,
                                                   choices=levels(ecv$edad_grupo), selected=levels(ecv$edad_grupo),
                                                   options=list(`actions-box`=TRUE)))
                               ),
                               div(class="filters-grid-2",
                                   div(div(class="filter-label","Elige una categoría exacta (p. ej., “Una vez a la semana”)"),
                                       selectInput("f_ind_freq_key", NULL, choices = freq_choices_ind,
                                                   selected = freq_ind_map$key[1])),
                                   div()
                               )
                           ),
                           div(id="kpi_card_freq", class="card",
                               div(class="card-title","¿Qué proporción reporta esa frecuencia?"),
                               htmlOutput("kpis_freq")),
                           div(class="grid-2",
                               div(
                                 div(class="card",
                                     div(class="card-title","¿Dónde pasa más seguido?"),
                                     leafletOutput("mapa_freq", height = 320)),
                                 div(class="card", style="margin-top:12px",
                                     div(class="card-title","¿Cómo evoluciona esa frecuencia?"),
                                     div(style="margin-bottom:6px;",
                                         prettySwitch("ts_scope_freq", "Usar departamento seleccionado en la serie", FALSE, status="primary")),
                                     htmlOutput("ts_label_freq"),
                                     plotlyOutput("ts_prev_freq", height = 300))
                               ),
                               div(class="card",
                                   div(class="card-title","¿Qué departamentos destacan?"),
                                   plotlyOutput("bars_all_freq", height = "720px"))
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
    ecv %>% dplyr::filter(!is.na(DEPARTAMENTO), nzchar(DEPARTAMENTO)) %>%
      dplyr::distinct(DEPARTAMENTO) %>% dplyr::arrange(DEPARTAMENTO) %>% dplyr::pull(DEPARTAMENTO)
  }
  # Mostrar en Title Case, conservar valores originales
  output$dep_ui  <- renderUI({ vals <- mk_dep_choices(); selectInput("f_dep",  NULL, choices=c("Todos"="Todos", stats::setNames(vals, to_title(vals))), selected="Todos") })
  output$dep_ui2 <- renderUI({ vals <- mk_dep_choices(); selectInput("f_dep2", NULL, choices=c("Todos"="Todos", stats::setNames(vals, to_title(vals))), selected="Todos") })
  output$dep_ui3 <- renderUI({ vals <- mk_dep_choices(); selectInput("f_dep3", NULL, choices=c("Todos"="Todos", stats::setNames(vals, to_title(vals))), selected="Todos") })
  
  mk_mun_choices <- function(dep_sel){
    d <- ecv
    if (!is.null(dep_sel) && dep_sel!="Todos") d <- d %>% dplyr::filter(DEPARTAMENTO==dep_sel)
    d %>% dplyr::filter(!is.na(MUNICIPIO), nzchar(MUNICIPIO)) %>%
      dplyr::distinct(MUNICIPIO) %>% dplyr::arrange(MUNICIPIO) %>% dplyr::pull(MUNICIPIO)
  }
  output$mun_ui  <- renderUI({
    vals <- mk_mun_choices(input$f_dep %||% "Todos")
    selectInput("f_mun", NULL, choices = c("Todos"="Todos", stats::setNames(vals, to_title(vals))), selected = "Todos")
  })
  output$mun_ui2 <- renderUI({
    vals <- mk_mun_choices(input$f_dep2 %||% "Todos")
    selectInput("f_mun2", NULL, choices = c("Todos"="Todos", stats::setNames(vals, to_title(vals))), selected = "Todos")
  })
  output$mun_ui3 <- renderUI({
    vals <- mk_mun_choices(input$f_dep3 %||% "Todos")
    selectInput("f_mun3", NULL, choices = c("Todos"="Todos", stats::setNames(vals, to_title(vals))), selected = "Todos")
  })
  
  output$cat_ui_bin <- renderUI({
    ind <- input$f_ind_bin; if (is.null(ind)) return(NULL)
    ch <- c("1 — Sí consume"=1,"2 — No consume"=2)
    if (ind=="c_azucaradas") selectInput("f_bin_c_azuc", NULL, choices=ch, selected=1)
    else                      selectInput("f_bin_c_paq",  NULL, choices=ch, selected=1)
  })
  
  apply_common_filters <- function(d, sexo, edad, ind_var, clase=NULL){
    if (!is.null(sexo) && length(sexo)) d <- d %>% dplyr::filter(p6020_lbl %in% sexo)
    if (!is.null(edad) && length(edad)) d <- d %>% dplyr::filter(edad_grupo %in% edad)
    if (!is.null(clase) && length(clase)) d <- d %>% dplyr::filter(CLASE_LBL %in% clase | is.na(CLASE_LBL))
    if (!is.null(ind_var) && ind_var %in% inds_aplican_12mas) d <- d %>% dplyr::filter(p6040 >= 12 | is.na(p6040))
    d %>% dplyr::filter(is.finite(fexp), fexp>0, !is.na(DEPARTAMENTO), nzchar(DEPARTAMENTO))
  }
  
  mk_event_bin <- function(d, ind){
    yes <- ifelse(ind=="c_azucaradas", input$f_bin_c_azuc %||% 1, input$f_bin_c_paq %||% 1)
    v <- suppressWarnings(as.integer(numish(d[[ind]])))
    ifelse(is.na(v), NA_real_, as.numeric(v == as.integer(yes)))
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
  
  # ===================== TAB FIES (primero) =====================
  base_anio_fies <- reactive({
    req(input$anio3, input$f_ind_fies, input$f_cat_fies)
    d <- ecv %>% filter(anio == as.integer(input$anio3))
    if (!is.null(input$f_dep3) && input$f_dep3 != "Todos") d <- d %>% filter(DEPARTAMENTO == input$f_dep3)
    if (!is.null(input$f_mun3) && input$f_mun3 != "Todos") d <- d %>% filter(MUNICIPIO == input$f_mun3)
    apply_common_filters(d, input$f_sexo3, input$f_edad3, NULL, clase = input$f_clase3)
  })
  base_ts_fies <- reactive({
    req(input$f_ind_fies, input$f_cat_fies)
    d <- ecv
    use_dep <- isTRUE(input$ts_scope_fies) && !is.null(input$f_dep3) && input$f_dep3 != "Todos"
    if (use_dep) d <- d %>% filter(DEPARTAMENTO == input$f_dep3)
    apply_common_filters(d, input$f_sexo3, input$f_edad3, NULL, clase = input$f_clase3)
  })
  output$kpis_fies <- renderUI({
    d <- base_anio_fies(); req(nrow(d) > 0)
    ev <- mk_event_fies(d, input$f_ind_fies, input$f_cat_fies)
    prev <- w_prop(as.numeric(ev), d$fexp) * 100
    n    <- sum(is.finite(ev) & is.finite(d$fexp) & d$fexp > 0)
    pob  <- sum(d$fexp[is.finite(d$fexp) & d$fexp > 0], na.rm=TRUE)
    HTML(sprintf('<div class="kpi">
      <div class="item"><span>Prevalencia ponderada</span><b>%s</b></div>
      <div class="item"><span>n válidos</span><b>%s</b></div>
      <div class="item"><span>Personas ponderadas</span><b>%s</b></div>
      <div class="item"><span>Año</span><b>%s</b></div></div>',
                 ifelse(is.finite(prev), sprintf('%.1f%%', prev), 'NA'),
                 comma(n), comma(round(pob)), input$anio3))
  })
  output$mapa_fies <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(-74.3, 4.6, 5)
  })
  observe({
    d <- base_anio_fies(); req(nrow(d) > 0)
    ev <- mk_event_fies(d, input$f_ind_fies, input$f_cat_fies)
    dep_sel <- input$f_dep3 %||% "Todos"
    mun_sel <- input$f_mun3 %||% "Todos"
    prx <- leafletProxy("mapa_fies") %>% clearShapes() %>% clearControls()
    
    if (is.null(dep_sel) || dep_sel == "Todos" || is.null(mun_sf)) {
      dd <- d %>% mutate(evento = as.numeric(ev)) %>%
        group_by(COD_DANE_DPTO2, DEPARTAMENTO) %>%
        summarise(prev = w_prop(evento, fexp) * 100, .groups = "drop")
      shp <- dep_sf %>% left_join(dd, by = c("COD_DPTO2" = "COD_DANE_DPTO2")) %>%
        mutate(DEPARTAMENTO = coalesce(DEPARTAMENTO, DEPTO_N),
               DEPTO_TC = to_title(DEPARTAMENTO),
               etq = paste0("<b>", DEPTO_TC, "</b><br>Prevalencia: ",
                            ifelse(is.finite(prev), sprintf('%.1f%%', prev), "NA")))
      if (sum(is.finite(shp$prev)) == 0) {
        prx %>% addPolygons(data = shp, layerId = ~COD_DPTO2, fillColor = "#f0f0f0",
                            color = "#666", weight = .7, fillOpacity = .7, label = ~lapply(etq, HTML))
      } else {
        pal <- make_pal_fixed(shp$prev, COLS_MAP)
        prx %>% addPolygons(data = shp, layerId = ~COD_DPTO2, fillColor = ~pal(prev),
                            color = "#666", weight = .7, fillOpacity = .9, label = ~lapply(etq, HTML),
                            highlightOptions = highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
          addLegend("bottomright", pal = pal, values = shp$prev, title = "Prevalencia (%)", opacity = .9)
      }
      return(invisible(NULL))
    }
    
    dep_row  <- dep_sf %>% filter(norm_cmp(DEPTO_N) == norm_cmp(dep_sel))
    dep_code <- if (nrow(dep_row)) dep_row$COD_DPTO2[1] else NA_character_
    
    dd_mpio <- d %>% filter(DEPARTAMENTO == dep_sel) %>%
      mutate(evento = as.numeric(ev)) %>%
      group_by(CODMUN, MUNICIPIO) %>%
      summarise(prev = w_prop(evento, fexp) * 100,
                MUNICIPIO = dplyr::first(na.omit(MUNICIPIO)), .groups = "drop")
    mun_dep <- mun_sf %>% filter(DPTO2 == dep_code)
    
    shp_m <- mun_dep %>%
      left_join(dd_mpio, by = "CODMUN") %>%
      mutate(MPIO_N = coalesce(MUNICIPIO_D, MUNICIPIO, CODMUN),
             MPIO_TC = to_title(MPIO_N),
             etq    = paste0("<b>", MPIO_TC, "</b><br>Prevalencia: ",
                             ifelse(is.finite(prev), sprintf('%.1f%%', prev), "NA")))
    
    if (sum(is.finite(shp_m$prev)) == 0) {
      prx %>% addPolygons(data = shp_m, layerId = ~CODMUN, fillColor = "#f0f0f0",
                          color = "#666", weight = .6, fillOpacity = .7, label = ~lapply(etq, HTML))
    } else {
      pal <- make_pal_fixed(shp_m$prev, COLS_MAP)
      prx %>% addPolygons(data = shp_m, layerId = ~CODMUN, fillColor = ~pal(prev),
                          color = "#666", weight = .6, fillOpacity = .9, label = ~lapply(etq, HTML),
                          highlightOptions = highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        addLegend("bottomright", pal = pal, values = shp_m$prev,
                  title = paste0("Prevalencia (%) — ", htmlEscape(to_title(dep_sel))), opacity = .9)
    }
    if (nrow(dep_row)>0) {
      prx %>% addPolygons(data = dep_row, fill = FALSE, color = "#111", weight = 2.2, opacity = 1)
      fit_bounds_proxy(prx, dep_row)
    }
    if (!is.null(mun_sel) && mun_sel != "Todos") {
      mun_row <- shp_m %>% filter(norm_cmp(MPIO_N) == norm_cmp(mun_sel))
      if (nrow(mun_row) > 0) {
        prx %>% addPolygons(data = mun_row, fillColor = "#ffffff", fillOpacity = .2,
                            color = "#1f2937", weight = 3, opacity = 1, dashArray = "4",
                            label = ~MPIO_TC)
        cen <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(mun_row)))[1,]
        prx %>% addCircleMarkers(lng = cen[1], lat = cen[2], radius = 5, weight = 2, fillOpacity = .9,
                                 label = mun_row$MPIO_TC[1])
        fit_bounds_proxy(prx, mun_row)
      }
    }
  })
  output$bars_all_fies <- renderPlotly({
    d <- base_anio_fies(); req(nrow(d) > 0)
    ev <- mk_event_fies(d, input$f_ind_fies, input$f_cat_fies)
    dd <- d %>% mutate(evento = as.numeric(ev)) %>%
      group_by(DEPARTAMENTO) %>%
      summarise(prev = w_prop(evento, fexp) * 100, .groups = "drop") %>%
      filter(is.finite(prev)) %>% arrange(desc(prev)) %>%
      mutate(Departamento_lbl = to_title(DEPARTAMENTO),
             Departamento = factor(Departamento_lbl, levels = rev(Departamento_lbl)),
             prev_lab = sprintf('%.1f', prev))
    plot_ly(dd, x = ~prev, y = ~Departamento, type = "bar", orientation = "h",
            text = ~prev_lab, textposition = "auto",
            hovertemplate = "<b>%{y}</b><br>Prevalencia: %{x:.1f}%<extra></extra>",
            marker = list(color = COL_BAR)) %>%
      layout(xaxis = list(title = "Prevalencia (%)", zeroline = FALSE),
             yaxis = list(title = "", automargin = TRUE),
             margin = list(l = 140, r = 10, t = 10, b = 40))
  })
  output$ts_label_fies <- renderUI({
    use_dep <- isTRUE(input$ts_scope_fies) && !identical(input$f_dep3, "Todos")
    item <- names(inds_fies)[inds_fies == (input$f_ind_fies %||% "p_suficiente_a")]
    catv <- if (as.integer(input$f_cat_fies %||% 1) == 1) "— Seguimos el \"Sí\"" else "— Seguimos el \"No\""
    dep_txt <- if (use_dep) paste0("Alcance de la serie: Departamento — ", htmlEscape(to_title(input$f_dep3))) else "Alcance de la serie: Nacional"
    HTML(sprintf('<div style="color:#6b7280;font-size:12px;margin-bottom:4px">%s<br/><span style="font-weight:600">%s %s</span></div>',
                 dep_txt, htmlEscape(item), htmlEscape(catv)))
  })
  output$ts_prev_fies <- renderPlotly({
    d <- base_ts_fies(); req(nrow(d) > 0)
    ev <- mk_event_fies(d, input$f_ind_fies, input$f_cat_fies)
    dt <- d %>% mutate(evento = as.numeric(ev)) %>%
      group_by(anio) %>% summarise(prev = w_prop(evento, fexp) * 100, .groups = "drop") %>%
      filter(!is.na(anio), is.finite(prev)) %>% arrange(anio)
    plot_ly(dt, x = ~anio, y = ~prev, type = "scatter", mode = "lines+markers",
            text = ~sprintf('%.1f%%', prev),
            hovertemplate = "Año: %{x}<br>Promedio: %{y:.1f}%%<extra></extra>",
            line = list(color = COL_BAR), marker = list(color = COL_BAR)) %>%
      layout(xaxis = list(title = "Año", dtick = 1, tickmode = "linear"),
             yaxis = list(title = "Prevalencia ponderada (%)", rangemode = "tozero"),
             margin = list(l = 60, r = 10, t = 10, b = 40))
  })
  # ===================== FIN TAB FIES =====================
  
  # -------- TAB 2: BINARIOS --------
  base_anio_bin <- reactive({
    req(input$anio)
    d <- ecv %>% dplyr::filter(anio==as.integer(input$anio))
    if (!is.null(input$f_dep) && input$f_dep!="Todos") d <- d %>% dplyr::filter(DEPARTAMENTO==input$f_dep)
    if (!is.null(input$f_mun) && input$f_mun!="Todos") d <- d %>% dplyr::filter(MUNICIPIO==input$f_mun)
    apply_common_filters(d, input$f_sexo, input$f_edad, input$f_ind_bin, clase=input$f_clase)
  })
  base_ts_bin <- reactive({
    d <- ecv
    use_dep <- isTRUE(input$ts_scope_bin) && !is.null(input$f_dep) && input$f_dep!="Todos"
    if (use_dep) d <- d %>% dplyr::filter(DEPARTAMENTO==input$f_dep)
    apply_common_filters(d, input$f_sexo, input$f_edad, input$f_ind_bin, clase=input$f_clase)
  })
  output$kpis_bin <- renderUI({
    d <- base_anio_bin(); req(nrow(d)>0)
    ev <- mk_event_bin(d, input$f_ind_bin %||% "c_azucaradas")
    prev <- w_prop(as.numeric(ev), d$fexp)*100
    n    <- sum(is.finite(ev)&is.finite(d$fexp)&d$fexp>0)
    pob  <- sum(d$fexp[is.finite(d$fexp)&d$fexp>0], na.rm=TRUE)
    HTML(sprintf('<div class="kpi">
      <div class="item"><span>Prevalencia ponderada</span><b>%s</b></div>
      <div class="item"><span>n válidos</span><b>%s</b></div>
      <div class="item"><span>Personas ponderadas</span><b>%s</b></div>
      <div class="item"><span>Año</span><b>%s</b></div></div>',
                 ifelse(is.finite(prev), sprintf("%.1f%%",prev), "NA"),
                 comma(n), comma(round(pob)), input$anio))
  })
  output$mapa_bin <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(-74.3,4.6,5)
  })
  observe({
    d <- base_anio_bin(); req(nrow(d) > 0)
    ev <- mk_event_bin(d, input$f_ind_bin %||% "c_azucaradas")
    dep_sel <- input$f_dep %||% "Todos"
    mun_sel <- input$f_mun %||% "Todos"
    prx <- leafletProxy("mapa_bin") %>% clearShapes() %>% clearControls()
    
    if (is.null(dep_sel) || dep_sel=="Todos" || is.null(mun_sf)) {
      dd <- d %>% mutate(evento = as.numeric(ev)) %>%
        group_by(COD_DANE_DPTO2, DEPARTAMENTO) %>%
        summarise(prev = w_prop(evento,fexp)*100, .groups="drop")
      shp <- dep_sf %>% left_join(dd, by=c("COD_DPTO2"="COD_DANE_DPTO2")) %>%
        mutate(DEPARTAMENTO=coalesce(DEPARTAMENTO, DEPTO_N),
               DEPTO_TC = to_title(DEPARTAMENTO),
               etq=paste0("<b>",DEPTO_TC,"</b><br>Prevalencia: ",
                          ifelse(is.finite(prev),sprintf('%.1f%%',prev),"NA")))
      if (sum(is.finite(shp$prev))==0){
        prx %>% addPolygons(data=shp, layerId=~COD_DPTO2, fillColor="#f0f0f0",
                            color="#666", weight=.7, fillOpacity=.7, label=~lapply(etq, HTML))
      } else {
        pal <- make_pal_fixed(shp$prev, COLS_MAP)
        prx %>% addPolygons(data=shp, layerId=~COD_DPTO2, fillColor=~pal(prev),
                            color="#666", weight=.7, fillOpacity=.9, label=~lapply(etq, HTML),
                            highlightOptions=highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
          addLegend("bottomright", pal=pal, values=shp$prev, title="Prevalencia (%)", opacity=.9)
      }
      return(invisible(NULL))
    }
    
    dep_row  <- dep_sf %>% filter(norm_cmp(DEPTO_N)==norm_cmp(dep_sel))
    dep_code <- if (nrow(dep_row)) dep_row$COD_DPTO2[1] else NA_character_
    
    dd_mpio <- d %>% filter(DEPARTAMENTO==dep_sel) %>%
      mutate(evento=as.numeric(ev)) %>%
      group_by(CODMUN, MUNICIPIO) %>%
      summarise(prev=w_prop(evento,fexp)*100,
                MUNICIPIO = dplyr::first(na.omit(MUNICIPIO)), .groups="drop")
    mun_dep <- mun_sf %>% filter(DPTO2 == dep_code)
    
    shp_m <- mun_dep %>%
      left_join(dd_mpio, by = "CODMUN") %>%
      mutate(
        MPIO_N = coalesce(MUNICIPIO_D, MUNICIPIO, CODMUN),
        MPIO_TC = to_title(MPIO_N),
        etq    = paste0("<b>",MPIO_TC,"</b><br>Prevalencia: ",
                        ifelse(is.finite(prev),sprintf('%.1f%%',prev),"NA"))
      )
    
    if (sum(is.finite(shp_m$prev))==0){
      prx %>% addPolygons(data=shp_m, layerId=~CODMUN, fillColor="#f0f0f0",
                          color="#666", weight=.6, fillOpacity=.7, label=~lapply(etq, HTML))
    } else {
      pal <- make_pal_fixed(shp_m$prev, COLS_MAP)
      prx %>% addPolygons(data=shp_m, layerId=~CODMUN, fillColor=~pal(prev),
                          color="#666", weight=.6, fillOpacity=.9, label=~lapply(etq, HTML),
                          highlightOptions=highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        addLegend("bottomright", pal=pal, values=shp_m$prev,
                  title=paste0("Prevalencia (%) — ", htmlEscape(to_title(dep_sel))), opacity=.9)
    }
    if (nrow(dep_row)>0){
      prx %>% addPolygons(data=dep_row, fill=FALSE, color="#111", weight=2.2, opacity=1)
      fit_bounds_proxy(prx, dep_row)
    }
    if (!is.null(mun_sel) && mun_sel!="Todos"){
      mun_row <- shp_m %>% filter(norm_cmp(MPIO_N)==norm_cmp(mun_sel))
      if (nrow(mun_row)>0){
        prx %>% addPolygons(data=mun_row, fillColor="#ffffff", fillOpacity=.2,
                            color="#1f2937", weight=3, opacity=1, dashArray="4",
                            label=~MPIO_TC)
        cen <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(mun_row)))[1,]
        prx %>% addCircleMarkers(lng=cen[1], lat=cen[2], radius=5, weight=2, fillOpacity=.9,
                                 label=mun_row$MPIO_TC[1])
        fit_bounds_proxy(prx, mun_row)
      }
    }
  })
  output$bars_all_bin <- renderPlotly({
    d <- base_anio_bin(); req(nrow(d)>0)
    ev <- mk_event_bin(d, input$f_ind_bin %||% "c_azucaradas")
    dd <- d %>% mutate(evento=as.numeric(ev)) %>%
      group_by(DEPARTAMENTO) %>%
      summarise(prev=w_prop(evento,fexp)*100, .groups="drop") %>%
      filter(is.finite(prev)) %>% arrange(desc(prev)) %>%
      mutate(Departamento_lbl = to_title(DEPARTAMENTO),
             Departamento=factor(Departamento_lbl, levels=rev(Departamento_lbl)),
             prev_lab=sprintf("%.1f",prev))
    plot_ly(dd, x=~prev, y=~Departamento, type="bar", orientation="h",
            text=~prev_lab, textposition="auto",
            hovertemplate="<b>%{y}</b><br>Prevalencia: %{x:.1f}%<extra></extra>",
            marker=list(color=COL_BAR)) %>%
      layout(xaxis=list(title="Prevalencia (%)", zeroline=FALSE),
             yaxis=list(title="", automargin=TRUE),
             margin=list(l=140,r=10,t=10,b=40))
  })
  output$ts_label_bin <- renderUI({
    use_dep <- isTRUE(input$ts_scope_bin) && !identical(input$f_dep,"Todos")
    HTML(sprintf('<div style="color:#6b7280;font-size:12px;margin-bottom:4px">%s</div>',
                 if (use_dep) paste0("Alcance de la serie: Departamento — ", htmlEscape(to_title(input$f_dep)))
                 else "Alcance de la serie: Nacional"))
  })
  output$ts_prev_bin <- renderPlotly({
    d <- base_ts_bin(); req(nrow(d)>0)
    ev <- mk_event_bin(d, input$f_ind_bin %||% "c_azucaradas")
    dt <- d %>% mutate(evento=as.numeric(ev)) %>%
      group_by(anio) %>% summarise(prev=w_prop(evento,fexp)*100, .groups="drop") %>%
      filter(!is.na(anio), is.finite(prev)) %>% arrange(anio)
    plot_ly(dt, x=~anio, y=~prev, type="scatter", mode="lines+markers",
            text=~sprintf('%.1f%%',prev),
            hovertemplate="Año: %{x}<br>Promedio: %{y:.1f}%%<extra></extra>",
            line=list(color=COL_BAR), marker=list(color=COL_BAR)) %>%
      layout(xaxis=list(title="Año", dtick=1, tickmode="linear"),
             yaxis=list(title="Prevalencia ponderada (%)", rangemode="tozero"),
             margin=list(l=60,r=10,t=10,b=40))
  })
  
  # -------- TAB 3: FRECUENCIAS --------
  base_anio_freq <- reactive({
    req(input$anio2, input$f_ind_freq_key)
    ind_var <- lookup_freq(input$f_ind_freq_key)$var
    d <- ecv %>% filter(anio==as.integer(input$anio2))
    if (!is.null(input$f_dep2) && input$f_dep2!="Todos") d <- d %>% filter(DEPARTAMENTO==input$f_dep2)
    if (!is.null(input$f_mun2) && input$f_mun2!="Todos") d <- d %>% filter(MUNICIPIO==input$f_mun2)
    apply_common_filters(d, input$f_sexo2, input$f_edad2, ind_var, clase=input$f_clase2)
  })
  base_ts_freq <- reactive({
    req(input$f_ind_freq_key)
    ind_var <- lookup_freq(input$f_ind_freq_key)$var
    d <- ecv
    use_dep <- isTRUE(input$ts_scope_freq) && !is.null(input$f_dep2) && input$f_dep2!="Todos"
    if (use_dep) d <- d %>% filter(DEPARTAMENTO==input$f_dep2)
    apply_common_filters(d, input$f_sexo2, input$f_edad2, ind_var, clase=input$f_clase2)
  })
  output$kpis_freq <- renderUI({
    d <- base_anio_freq(); req(nrow(d)>0)
    ev <- mk_event_freq_single(d, input$f_ind_freq_key)
    prev <- w_prop(as.numeric(ev), d$fexp)*100
    n    <- sum(is.finite(ev)&is.finite(d$fexp)&d$fexp>0)
    pob  <- sum(d$fexp[is.finite(d$fexp)&d$fexp>0], na.rm=TRUE)
    HTML(sprintf('<div class="kpi">
      <div class="item"><span>Prevalencia ponderada</span><b>%s</b></div>
      <div class="item"><span>n válidos</span><b>%s</b></div>
      <div class="item"><span>Personas ponderadas</span><b>%s</b></div>
      <div class="item"><span>Año</span><b>%s</b></div></div>',
                 ifelse(is.finite(prev), sprintf("%.1f%%",prev), "NA"),
                 comma(n), comma(round(pob)), input$anio2))
  })
  output$mapa_freq <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(-74.3,4.6,5)
  })
  observe({
    d <- base_anio_freq(); req(nrow(d) > 0)
    ev <- mk_event_freq_single(d, input$f_ind_freq_key)
    dep_sel <- input$f_dep2 %||% "Todos"
    mun_sel <- input$f_mun2 %||% "Todos"
    prx <- leafletProxy("mapa_freq") %>% clearShapes() %>% clearControls()
    
    if (is.null(dep_sel) || dep_sel=="Todos" || is.null(mun_sf)) {
      dd <- d %>% mutate(evento=as.numeric(ev)) %>%
        group_by(COD_DANE_DPTO2, DEPARTAMENTO) %>%
        summarise(prev=w_prop(evento,fexp)*100, .groups="drop")
      shp <- dep_sf %>% left_join(dd, by=c("COD_DPTO2"="COD_DANE_DPTO2")) %>%
        mutate(DEPARTAMENTO=coalesce(DEPARTAMENTO, DEPTO_N),
               DEPTO_TC = to_title(DEPARTAMENTO),
               etq=paste0("<b>",DEPTO_TC,"</b><br>Prevalencia: ",
                          ifelse(is.finite(prev),sprintf('%.1f%%',prev),"NA")))
      if (sum(is.finite(shp$prev))==0){
        prx %>% addPolygons(data=shp, layerId=~COD_DPTO2, fillColor="#f0f0f0",
                            color="#666", weight=.7, fillOpacity=.7, label=~lapply(etq, HTML))
      } else {
        pal <- make_pal_fixed(shp$prev, COLS_MAP)
        prx %>% addPolygons(data=shp, layerId=~COD_DPTO2, fillColor=~pal(prev),
                            color="#666", weight=.7, fillOpacity=.9, label=~lapply(etq, HTML),
                            highlightOptions=highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
          addLegend("bottomright", pal=pal, values=shp$prev, title="Prevalencia (%)", opacity=.9)
      }
      return(invisible(NULL))
    }
    
    dep_row  <- dep_sf %>% filter(norm_cmp(DEPTO_N)==norm_cmp(dep_sel))
    dep_code <- if (nrow(dep_row)) dep_row$COD_DPTO2[1] else NA_character_
    
    dd_mpio <- d %>% filter(DEPARTAMENTO==dep_sel) %>%
      mutate(evento=as.numeric(ev)) %>%
      group_by(CODMUN, MUNICIPIO) %>%
      summarise(prev=w_prop(evento,fexp)*100,
                MUNICIPIO = dplyr::first(na.omit(MUNICIPIO)), .groups="drop")
    mun_dep <- mun_sf %>% filter(DPTO2 == dep_code)
    
    shp_m <- mun_dep %>%
      left_join(dd_mpio, by = "CODMUN") %>%
      mutate(
        MPIO_N = coalesce(MUNICIPIO_D, MUNICIPIO, CODMUN),
        MPIO_TC = to_title(MPIO_N),
        etq    = paste0("<b>",MPIO_TC,"</b><br>Prevalencia: ",
                        ifelse(is.finite(prev),sprintf('%.1f%%',prev),"NA"))
      )
    
    if (sum(is.finite(shp_m$prev))==0){
      prx %>% addPolygons(data=shp_m, layerId=~CODMUN, fillColor="#f0f0f0",
                          color="#666", weight=.6, fillOpacity=.7, label=~lapply(etq, HTML))
    } else {
      pal <- make_pal_fixed(shp_m$prev, COLS_MAP)
      prx %>% addPolygons(data=shp_m, layerId=~CODMUN, fillColor=~pal(prev),
                          color="#666", weight=.6, fillOpacity=.9, label=~lapply(etq, HTML),
                          highlightOptions=highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        addLegend("bottomright", pal=pal, values=shp_m$prev,
                  title=paste0("Prevalencia (%) — ", htmlEscape(to_title(dep_sel))), opacity=.9)
    }
    if (nrow(dep_row)>0){
      prx %>% addPolygons(data=dep_row, fill=FALSE, color="#111", weight=2.2, opacity=1)
      fit_bounds_proxy(prx, dep_row)
    }
    if (!is.null(mun_sel) && mun_sel!="Todos"){
      mun_row <- shp_m %>% filter(norm_cmp(MPIO_N)==norm_cmp(mun_sel))
      if (nrow(mun_row)>0){
        prx %>% addPolygons(data=mun_row, fillColor="#ffffff", fillOpacity=.2,
                            color="#1f2937", weight=3, opacity=1, dashArray="4",
                            label=~MPIO_TC)
        cen <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(mun_row)))[1,]
        prx %>% addCircleMarkers(lng=cen[1], lat=cen[2], radius=5, weight=2, fillOpacity=.9,
                                 label=mun_row$MPIO_TC[1])
        fit_bounds_proxy(prx, mun_row)
      }
    }
  })
  output$bars_all_freq <- renderPlotly({
    d <- base_anio_freq(); req(nrow(d)>0)
    ev <- mk_event_freq_single(d, input$f_ind_freq_key)
    dd <- d %>% mutate(evento=as.numeric(ev)) %>%
      group_by(DEPARTAMENTO) %>%
      summarise(prev=w_prop(evento,fexp)*100, .groups="drop") %>%
      filter(is.finite(prev)) %>% arrange(desc(prev)) %>%
      mutate(Departamento_lbl = to_title(DEPARTAMENTO),
             Departamento=factor(Departamento_lbl, levels=rev(Departamento_lbl)),
             prev_lab=sprintf("%.1f",prev))
    plot_ly(dd, x=~prev, y=~Departamento, type="bar", orientation="h",
            text=~prev_lab, textposition="auto",
            hovertemplate="<b>%{y}</b><br>Prevalencia: %{x:.1f}%<extra></extra>",
            marker=list(color=COL_BAR)) %>%
      layout(xaxis=list(title="Prevalencia (%)", zeroline=FALSE),
             yaxis=list(title="", automargin=TRUE),
             margin=list(l=140,r=10,t=10,b=40))
  })
  output$ts_label_freq <- renderUI({
    use_dep <- isTRUE(input$ts_scope_freq) && !identical(input$f_dep2,"Todos")
    lk <- lookup_freq(input$f_ind_freq_key)
    HTML(sprintf('<div style="color:#6b7280;font-size:12px;margin-bottom:4px">%s<br/><span style="font-weight:600">%s</span></div>',
                 if (use_dep) paste0("Alcance de la serie: Departamento — ", htmlEscape(to_title(input$f_dep2)))
                 else "Alcance de la serie: Nacional",
                 htmlEscape(lk$label)))
  })
  output$ts_prev_freq <- renderPlotly({
    d <- base_ts_freq(); req(nrow(d)>0)
    ev <- mk_event_freq_single(d, input$f_ind_freq_key)
    dt <- d %>% mutate(evento=as.numeric(ev)) %>%
      group_by(anio) %>% summarise(prev=w_prop(evento,fexp)*100, .groups="drop") %>%
      filter(!is.na(anio), is.finite(prev)) %>% arrange(anio)
    plot_ly(dt, x=~anio, y=~prev, type="scatter", mode="lines+markers",
            text=~sprintf('%.1f%%',prev),
            hovertemplate="Año: %{x}<br>Promedio: %{y:.1f}%%<extra></extra>",
            line=list(color=COL_BAR), marker=list(color=COL_BAR)) %>%
      layout(xaxis=list(title="Año", dtick=1, tickmode="linear"),
             yaxis=list(title="Prevalencia ponderada (%)", rangemode="tozero"),
             margin=list(l=60,r=10,t=10,b=40))
  })
}

shinyApp(ui, server)




