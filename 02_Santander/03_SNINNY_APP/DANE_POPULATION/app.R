# =========================================================
# app_poblacion_dashboard_v2.R — Proyecciones DANE (pirámide con decenios)
# =========================================================
suppressWarnings({
  library(shiny); library(dplyr); library(tidyr); library(ggplot2); library(plotly)
  library(scales); library(bslib); library(stringi)
})

options(stringsAsFactors = FALSE, scipen = 999)

# ---------- Ruta y carga ----------
data_dir <- "data"
pob_path <- file.path(data_dir, "051_DANE_Proyecciones_P.rds")
stopifnot(file.exists(pob_path))
pob <- readRDS(pob_path)

# ---------- Normalización mínima ----------
if (!"DEPARTAMENTO_D" %in% names(pob) && "DEPARTAMENTO" %in% names(pob)) pob$DEPARTAMENTO_D <- pob$DEPARTAMENTO
if (!"MUNICIPIO_D"   %in% names(pob) && "MUNICIPIO"   %in% names(pob)) pob$MUNICIPIO_D   <- pob$MUNICIPIO

suppressWarnings({
  if (!is.numeric(pob$ano))       pob$ano       <- as.integer(pob$ano)
  if ("quinquenio" %in% names(pob) && !is.numeric(pob$quinquenio)) pob$quinquenio <- as.character(pob$quinquenio)
  if (!is.numeric(pob$poblacion)) pob$poblacion <- as.numeric(pob$poblacion)
})

pob <- pob %>% dplyr::filter(DEPARTAMENTO_D == "SANTANDER")

# ---------- Helpers ----------
fmt_comma <- function(x) comma(x, big.mark = ".", decimal.mark = ",")

fmt_km <- function(x){
  vapply(x, function(v){
    if (is.na(v)) return(NA_character_)
    av <- abs(v)
    if (av >= 1e6){
      paste0(scales::number(av/1e6, accuracy = 0.1, big.mark = ".", decimal.mark = ","), "M")
    } else if (av >= 1e3){
      paste0(scales::number(av/1e3, accuracy = 0.1, big.mark = ".", decimal.mark = ","), "K")
    } else {
      scales::number(av, accuracy = 1, big.mark = ".", decimal.mark = ",")
    }
  }, character(1))
}

quinquenio_to_edad_inicio <- function(quinquenio) {
  procesar_un_valor <- function(q) {
    suppressWarnings({
      if (is.na(q)) return(NA)
      if (is.character(q)) {
        if (grepl("\\+", q)) return(as.numeric(gsub("\\+", "", q)))
        return(as.numeric(sub("-.*", "", q)))
      } else if (is.numeric(q)) {
        return(q)
      }
      return(NA)
    })
  }
  sapply(quinquenio, procesar_un_valor, USE.NAMES = FALSE)
}

quinquenio_to_decenio <- function(quinquenio) {
  edad_inicio <- quinquenio_to_edad_inicio(quinquenio)
  decenio <- cut(
    edad_inicio,
    breaks = c(0, seq(10, 80, by = 10), Inf),
    labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"),
    right = FALSE,
    include.lowest = TRUE
  )
  as.character(decenio)
}

title_case_es <- function(x){
  x <- tolower(as.character(x))
  stringi::stri_trans_totitle(x, opts_brkiter = list(type = "word"))
}

empty_plot <- function(txt = "Sin datos para los filtros actuales."){
  plotly::plotly_empty(type = "scatter", mode = "markers") %>%
    plotly::layout(
      annotations = list(
        x = 0.5, y = 0.5, text = as.character(txt),
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

empty_ggplot <- function(txt = "Sin datos para los filtros actuales."){
  ggplot() + annotate("text", x = 0, y = 0, label = txt, size = 5) + theme_void()
}

# ---------- Opciones filtros ----------
anos <- sort(unique(na.omit(pob$ano)))

deps_raw    <- sort(unique(na.omit(pob$DEPARTAMENTO_D)))
deps_lab    <- title_case_es(deps_raw)
dep_choices <- setNames(c("Todos", deps_raw), c("Todos", deps_lab))

mun_raw_all     <- sort(unique(na.omit(pob$MUNICIPIO_D)))
mun_lab_all     <- title_case_es(mun_raw_all)
mun_choices_all <- setNames(c("Todos", mun_raw_all), c("Todos", mun_lab_all))

clase_raw     <- sort(unique(na.omit(pob$clase)))
clase_lab     <- title_case_es(clase_raw)
clase_val     <- c("Todas", clase_raw)
clase_choices <- setNames(clase_val, c("Todas", clase_lab))

sexo_raw     <- sort(unique(na.omit(pob$sexo)))
sexo_lab     <- title_case_es(sexo_raw)
sexo_val     <- c("Ambos", sexo_raw)
sexo_choices <- setNames(sexo_val, c("Ambos", sexo_lab))

L <- 30L

sanitize_filename <- function(x){
  x <- as.character(x)
  x <- gsub("[/\\\\:*?\"<>|]", "_", x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("__+", "_", x)
  trimws(x)
}

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  theme = bs_theme(
    version = 5, primary = "#2563eb",
    base_font = font_google("Inter"), heading_font = font_google("Inter Tight"),
    "border-radius" = "0.9rem", "font-size-base" = "0.95rem"
  ),
  tags$head(
    tags$style(HTML("
      .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
      .filters{
        background:#fff; border:1.5px solid #99d5ec; border-radius:16px;
        padding:14px 16px; margin-bottom:16px; box-shadow:0 4px 14px rgba(153,213,236,.35)
      }
      .card{
        background:#fff; border:1.5px solid #99d5ec; border-radius:16px; padding:12px;
        box-shadow:0 2px 10px rgba(153,213,236,.35);
        margin-bottom:0;
      }
      .card-kpi{display:flex;flex-direction:column;justify-content:space-between;min-height:120px;}
      .filters-grid{display:grid;grid-template-columns:repeat(5,minmax(160px,1fr));gap:12px}
      .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}
      .filter-label{
        font-family: 'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-size:14px;font-weight:500;letter-spacing:.4px;color:#000000;margin-bottom:6px;
      }
      .kpi{font-weight:800;font-size:28px;color:#111827; line-height:1.2;}
      .kpi-sub{font-size:12px;color:#6b7280;margin-top:-4px}

      .form-control, .form-select{
        border:1.5px solid #99d5ec !important;
        border-radius:10px !important;
        box-shadow:none !important;
      }
      .form-control:focus, .form-select:focus{
        border-color:#99d5ec !important;
        box-shadow:0 0 0 .2rem rgba(153,213,236,.55) !important;
      }

      .selectize-control{padding:0 !important;border:none !important;box-shadow:none !important;background:transparent !important;}
      .selectize-control.single .selectize-input{
        border:1.5px solid #99d5ec !important;border-radius:10px !important;box-shadow:none !important;
        background-color:#fff !important;padding-top:4px;padding-bottom:4px;
      }
      .selectize-dropdown{border:1.5px solid #99d5ec !important;border-radius:10px !important;box-shadow:none !important;}
      .section-row{margin-top:16px;}

      .csv-footer{
        display:flex;justify-content:flex-end;align-items:center;gap:10px;
        margin-top:14px;padding-bottom:6px;
      }
    ")),
    tags$script(HTML("
      (function() {
        function initDepPopover() {
          var el = document.getElementById('info_dep');
          if (!el || typeof bootstrap === 'undefined' || !bootstrap.Popover) {
            setTimeout(initDepPopover, 300);
            return;
          }
          var content =
            '<div style=\"font-weight:600;margin-bottom:4px;\">Razón de dependencia</div>' +
            '<div style=\"font-size:12px;\">' +
            'Relación entre población dependiente (0–14 y 65+) y población en edad de trabajar (15–64).<br>' +
            'Un valor de 0,5 significa 50 dependientes por cada 100 personas en edad de trabajar.' +
            '</div>';
          el.setAttribute('data-bs-toggle','popover');
          el.setAttribute('data-bs-html','true');
          el.setAttribute('data-bs-placement','bottom');
          el.setAttribute('data-bs-offset','0,8');
          el.setAttribute('data-bs-content', content);
          new bootstrap.Popover(el);
        }
        if (document.readyState !== 'loading') initDepPopover();
        else document.addEventListener('DOMContentLoaded', initDepPopover);
      })();
    "))
  ),
  
  div(class="wrap",
      div(class="filters",
          div(class="filters-grid",
              div(class="filter", div(class="filter-label","¿Qué año analizamos?"),
                  selectInput("f_ano", NULL, choices = anos, selected = max(anos, na.rm = TRUE))),
              div(class="filter", div(class="filter-label","¿En qué departamento?"),
                  selectInput("f_dep", NULL, choices = dep_choices, selected = "SANTANDER")),
              div(class="filter", div(class="filter-label","¿Algún municipio en particular?"),
                  selectizeInput("f_mun", NULL, choices = mun_choices_all, selected = "Todos",
                                 options = list(placeholder = "Seleccione municipio..."))),
              div(class="filter", div(class="filter-label","¿Área urbana o rural?"),
                  selectInput("f_clase", NULL, choices = clase_choices, selected = "Todas")),
              div(class="filter", div(class="filter-label","¿Hombres o mujeres?"),
                  selectInput("f_sexo", NULL, choices = sexo_choices, selected = "Ambos"))
          )
      ),
      
      fluidRow(class = "section-row",
               column(3, div(class="card card-kpi",
                             div(class="card-title","Población total"),
                             div(class="kpi", textOutput("kpi_pob")),
                             div(class="kpi-sub"," ")
               )),
               column(3, div(class="card card-kpi",
                             div(class="card-title","% Jóvenes (0–14)"),
                             div(class="kpi", textOutput("kpi_jovenes")),
                             div(class="kpi-sub","Participación del total de personas")
               )),
               column(3, div(class="card card-kpi",
                             div(class="card-title","% Adultos mayores (65+)"),
                             div(class="kpi", textOutput("kpi_mayores")),
                             div(class="kpi-sub","Participación del total de personas")
               )),
               column(3, div(class="card card-kpi",
                             div(class = "card-title",
                                 HTML('Razón de dependencia <span id=\"info_dep\" class=\"info-quantiles-icon\">ℹ️</span>')),
                             div(class="kpi", textOutput("kpi_dep")),
                             div(class="kpi-sub"," ")
               ))
      ),
      
      fluidRow(class = "section-row",
               column(6, div(class="card",
                             div(style="display:flex;justify-content:space-between;align-items:center;",
                                 div(class="card-title","¿Cómo ha cambiado el número de habitantes a lo largo del tiempo?"),
                                 downloadButton("dl_hist_png", "Descargar .png", class = "btn btn-sm btn-outline-primary")
                             ),
                             plotlyOutput("plot_hist", height = 420)
               )),
               column(6, div(class="card",
                             div(style="display:flex;justify-content:space-between;align-items:center;",
                                 div(class="card-title","¿Cómo se distribuye la población del territorio por grupos de edad y sexo?"),
                                 downloadButton("dl_piramide_png", "Descargar .png", class = "btn btn-sm btn-outline-primary")
                             ),
                             plotlyOutput("plot_piramide", height = 420)
               ))
      ),
      
      fluidRow(class = "section-row",
               column(12, div(class="card",
                              div(style="display:flex;justify-content:space-between;align-items:center;",
                                  div(class="card-title","¿Cómo ha variado la población, en promedio anual, durante los últimos 30 años?"),
                                  downloadButton("dl_cagr30_png", "Descargar .png", class = "btn btn-sm btn-outline-primary")
                              ),
                              plotlyOutput("plot_cagr30", height = 320)
               ))
      ),
      
      div(class="csv-footer",
          downloadButton("dl_base_csv", "Descargar base (.csv)", class = "btn btn-sm btn-outline-primary"),
          downloadButton("dl_reporte_pdf", "Descargar informe (.pdf)", class = "btn btn-sm btn-outline-primary")
      )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  
  observeEvent(input$f_dep, {
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      ch_choices <- mun_choices_all
    } else {
      ch_raw <- pob %>%
        filter(DEPARTAMENTO_D == input$f_dep) %>%
        distinct(MUNICIPIO_D) %>%
        arrange(MUNICIPIO_D) %>%
        pull(MUNICIPIO_D)
      
      ch_lab     <- title_case_es(ch_raw)
      ch_choices <- setNames(c("Todos", ch_raw), c("Todos", ch_lab))
    }
    
    updateSelectizeInput(session, "f_mun", choices = ch_choices, selected = "Todos", server = TRUE)
  }, ignoreInit = TRUE)
  
  base_filtrada <- reactive({
    df <- pob
    if (!is.null(input$f_dep)   && input$f_dep   != "Todos") df <- df %>% filter(DEPARTAMENTO_D == input$f_dep)
    if (!is.null(input$f_mun)   && input$f_mun   != "Todos") df <- df %>% filter(MUNICIPIO_D   == input$f_mun)
    if (!is.null(input$f_clase) && input$f_clase != "Todas") df <- df %>% filter(clase         == input$f_clase)
    if (!is.null(input$f_sexo)  && input$f_sexo  != "Ambos") df <- df %>% filter(sexo          == input$f_sexo)
    df
  })
  
  serie_poblacion <- reactive({
    base_filtrada() %>%
      group_by(ano) %>%
      summarise(poblacion = sum(poblacion, na.rm = TRUE), .groups = "drop") %>%
      arrange(ano)
  })
  
  base_ano <- reactive({
    df <- base_filtrada()
    if (is.null(input$f_ano)) return(df[0, ])
    df %>% filter(ano == input$f_ano)
  })
  
  observe({
    df  <- serie_poblacion()
    yrs <- sort(unique(na.omit(df$ano)))
    if (length(yrs)) {
      sel <- if (!is.null(input$f_ano) && input$f_ano %in% yrs) input$f_ano else max(yrs)
      updateSelectInput(session, "f_ano", choices = yrs, selected = sel)
    }
  })
  
  output$kpi_pob <- renderText({
    df <- base_ano()
    fmt_comma(sum(df$poblacion, na.rm = TRUE))
  })
  
  output$kpi_jovenes <- renderText({
    df  <- base_ano()
    tot <- sum(df$poblacion, na.rm = TRUE)
    
    if ("quinquenio" %in% names(df)) {
      df <- df %>% mutate(edad_inicio = quinquenio_to_edad_inicio(quinquenio))
      j   <- sum(df$poblacion[!is.na(df$edad_inicio) & df$edad_inicio <= 14], na.rm = TRUE)
    } else j <- 0
    
    prop <- if (tot > 0) 100 * j / tot else 0
    paste0(scales::number(prop, accuracy = 0.1, decimal.mark = ",", big.mark = "."), "%")
  })
  
  output$kpi_mayores <- renderText({
    df  <- base_ano()
    tot <- sum(df$poblacion, na.rm = TRUE)
    
    if ("quinquenio" %in% names(df)) {
      df <- df %>% mutate(edad_inicio = quinquenio_to_edad_inicio(quinquenio))
      may <- sum(df$poblacion[!is.na(df$edad_inicio) & df$edad_inicio >= 65], na.rm = TRUE)
    } else may <- 0
    
    prop <- if (tot > 0) 100 * may / tot else 0
    paste0(scales::number(prop, accuracy = 0.1, decimal.mark = ",", big.mark = "."), "%")
  })
  
  output$kpi_dep <- renderText({
    df  <- base_ano()
    
    if ("quinquenio" %in% names(df)) {
      df <- df %>% mutate(edad_inicio = quinquenio_to_edad_inicio(quinquenio))
      j   <- sum(df$poblacion[!is.na(df$edad_inicio) & df$edad_inicio <= 14], na.rm = TRUE)
      m   <- sum(df$poblacion[!is.na(df$edad_inicio) & df$edad_inicio >= 65], na.rm = TRUE)
      act <- sum(df$poblacion[!is.na(df$edad_inicio) & df$edad_inicio >= 15 & df$edad_inicio <= 64], na.rm = TRUE)
    } else { j <- 0; m <- 0; act <- 1 }
    
    ratio <- if (act > 0) (j + m) / act else 0
    scales::number(ratio, accuracy = 0.01, decimal.mark = ",", big.mark = ".")
  })
  
  # ================= ggplots base =================
  gg_hist <- reactive({
    df <- serie_poblacion()
    if (nrow(df) == 0) return(empty_ggplot("Sin datos para la evolución histórica con los filtros actuales."))
    
    ggplot(df, aes(
      x = ano, y = poblacion,
      text = paste0("Año: ", ano, "<br>Población: ", fmt_km(poblacion))
    )) +
      geom_line(color = "#0a83ff", linewidth = 1.2) +
      geom_point(color = "#0a83ff", size = 2) +
      scale_y_continuous(labels = fmt_km) +
      labs(x = NULL, y = "Población") +
      theme_minimal() +
      theme(
        panel.background   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#e5e7eb"),
        panel.grid.minor.y = element_blank(),
        axis.line.x        = element_line(colour = "#111827"),
        axis.line.y        = element_line(colour = "#111827")
      )
  })
  
  gg_piramide <- reactive({
    df <- base_ano()
    if (!"quinquenio" %in% names(df)) return(empty_ggplot("No se encontró variable quinquenio para la pirámide."))
    
    df <- df %>%
      mutate(
        decenio = quinquenio_to_decenio(quinquenio),
        sexo_lc  = tolower(trimws(as.character(sexo))),
        sexo_cat = dplyr::case_when(
          sexo_lc %in% c("hombre","hombres","m","masculino","masculinos") ~ "Hombres",
          sexo_lc %in% c("mujer","mujeres","f","femenino","femeninos")     ~ "Mujeres",
          TRUE ~ "Otros"
        ),
        sexo_cat = factor(sexo_cat, levels = c("Hombres","Mujeres","Otros"))
      ) %>%
      filter(sexo_cat %in% c("Hombres","Mujeres")) %>%
      group_by(sexo_cat, decenio) %>%
      summarise(pob = sum(poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(pob_plot = ifelse(sexo_cat == "Hombres", -pob, pob))
    
    if (nrow(df) == 0) return(empty_ggplot("Sin datos para la pirámide con los filtros actuales."))
    
    decenios_orden <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
    df$decenio <- factor(df$decenio, levels = decenios_orden)
    
    max_abs    <- max(abs(df$pob_plot), na.rm = TRUE)
    raw_breaks <- pretty(c(-max_abs, max_abs), n = 5)
    max_tick   <- max(abs(raw_breaks))
    breaks_y   <- pretty(c(-max_tick, max_tick), n = 5)
    
    ggplot(df, aes(
      x = decenio, y = pob_plot, fill = sexo_cat,
      text = paste0("Grupo: ", decenio, "<br>Sexo: ", sexo_cat, "<br>Población: ", fmt_km(pob))
    )) +
      geom_col(width = 0.9) +
      coord_flip() +
      scale_y_continuous(
        breaks = breaks_y,
        labels = function(x) fmt_km(abs(x)),
        limits = range(breaks_y),
        expand = expansion(mult = 0.02)
      ) +
      scale_fill_manual(values = c("Hombres"="#0a83ff","Mujeres"="#f57c00"), name = "Sexo", drop = TRUE) +
      labs(x = "Grupos etarios (decenios)", y = "") +
      theme_minimal() +
      theme(
        panel.background   = element_blank(),
        panel.grid.major.x = element_line(colour = "#e5e7eb"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line          = element_line(colour = "#111827"),
        axis.text.y        = element_text(size = 10)
      )
  })
  
  gg_cagr30 <- reactive({
    df_tot <- serie_poblacion()
    if (nrow(df_tot) == 0) return(empty_ggplot("Sin datos suficientes para estimar la serie de crecimiento."))
    
    df_join <- df_tot %>%
      dplyr::rename(poblacion_t = poblacion) %>%
      dplyr::left_join(
        df_tot %>% dplyr::transmute(ano = ano + L, poblacion_tminusL = poblacion),
        by = "ano"
      ) %>%
      dplyr::mutate(
        g_aprox = dplyr::if_else(
          !is.na(poblacion_tminusL) & poblacion_tminusL > 0,
          (poblacion_t / poblacion_tminusL - 1) / L,
          NA_real_
        )
      ) %>%
      dplyr::filter(!is.na(g_aprox)) %>%
      dplyr::arrange(ano)
    
    if (nrow(df_join) == 0) return(empty_ggplot(sprintf("No hay pares t y t-%d disponibles con los filtros actuales.", L)))
    
    ggplot(df_join, aes(
      x = ano, y = g_aprox, group = 1,
      text = paste0(
        "Año t: ", ano,
        "<br>Rezago (L): ", L, " años",
        "<br>Crec. promedio anual aprox.: ", scales::percent(g_aprox, accuracy = 0.1, big.mark = ".", decimal.mark = ",")
      )
    )) +
      geom_line(color = "#0a83ff", linewidth = 1.2) +
      geom_point(color = "#0a83ff", size = 2) +
      scale_x_continuous(breaks = unique(df_join$ano)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1, big.mark = ".", decimal.mark = ",")) +
      labs(x = NULL, y = "Crecimiento promedio anual") +
      theme_minimal() +
      theme(
        panel.background   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#e5e7eb"),
        panel.grid.minor.y = element_blank(),
        axis.line.x        = element_line(colour = "#111827"),
        axis.line.y        = element_line(colour = "#111827")
      )
  })
  
  # ================= Plotly outputs =================
  output$plot_hist <- renderPlotly({
    ggplotly(gg_hist(), tooltip = "text") %>%
      layout(margin = list(l = 60, r = 20, t = 30, b = 40),
             xaxis = list(showgrid = FALSE, zeroline = FALSE),
             yaxis = list(showgrid = TRUE,  zeroline = FALSE),
             plot_bgcolor  = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)")
  })
  
  output$plot_piramide <- renderPlotly({
    ggplotly(gg_piramide(), tooltip = "text") %>%
      layout(margin = list(l = 60, r = 20, t = 30, b = 50),
             xaxis = list(showgrid = FALSE, zeroline = FALSE),
             yaxis = list(showgrid = TRUE,  zeroline = FALSE),
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.10, yanchor = "top"),
             plot_bgcolor  = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)")
  })
  
  output$plot_cagr30 <- renderPlotly({
    ggplotly(gg_cagr30(), tooltip = "text") %>%
      layout(margin = list(l = 60, r = 20, t = 30, b = 40),
             xaxis = list(showgrid = FALSE, zeroline = FALSE),
             yaxis = list(showgrid = TRUE,  zeroline = FALSE),
             plot_bgcolor  = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)")
  })
  
  # ================= Descargas PNG =================
  output$dl_hist_png <- downloadHandler(
    filename = function(){
      paste0("hist_poblacion_", sanitize_filename(input$f_dep), "_", sanitize_filename(input$f_mun), "_",
             sanitize_filename(input$f_clase), "_", sanitize_filename(input$f_sexo), ".png")
    },
    content = function(file){
      ggsave(file, plot = gg_hist(), width = 10, height = 6, dpi = 300, bg = "white")
    }
  )
  
  output$dl_piramide_png <- downloadHandler(
    filename = function(){
      paste0("piramide_", input$f_ano, "_", sanitize_filename(input$f_dep), "_", sanitize_filename(input$f_mun), "_",
             sanitize_filename(input$f_clase), "_", sanitize_filename(input$f_sexo), ".png")
    },
    content = function(file){
      ggsave(file, plot = gg_piramide(), width = 10, height = 6, dpi = 300, bg = "white")
    }
  )
  
  output$dl_cagr30_png <- downloadHandler(
    filename = function(){
      paste0("crec_prom_anual_L", L, "_", sanitize_filename(input$f_dep), "_", sanitize_filename(input$f_mun), "_",
             sanitize_filename(input$f_clase), "_", sanitize_filename(input$f_sexo), ".png")
    },
    content = function(file){
      ggsave(file, plot = gg_cagr30(), width = 12, height = 4.5, dpi = 300, bg = "white")
    }
  )
  
  # ================= Descarga CSV =================
  output$dl_base_csv <- downloadHandler(
    filename = function(){
      paste0("base_poblacion_", Sys.Date(), "_", sanitize_filename(input$f_dep), "_", sanitize_filename(input$f_mun), "_",
             sanitize_filename(input$f_clase), "_", sanitize_filename(input$f_sexo), ".csv")
    },
    content = function(file){
      df <- base_filtrada()
      utils::write.csv2(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # ================= (FIX) PDF: render de Informe_descargable.Rmd usando getwd()/Descargas =================
  output$dl_reporte_pdf <- downloadHandler(
    filename = function(){
      paste0(
        "Informe_poblacion_", Sys.Date(), "_",
        sanitize_filename(input$f_dep), "_",
        sanitize_filename(input$f_mun), "_",
        sanitize_filename(input$f_clase), "_",
        sanitize_filename(input$f_sexo), ".pdf"
      )
    },
    content = function(file){
      
      if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("Falta el paquete rmarkdown")
      if (!requireNamespace("knitr", quietly = TRUE))     stop("Falta el paquete knitr")
      
      rmd_origen <- "Informe_descargable.Rmd"
      if (!file.exists(rmd_origen)) stop("No se encontró el archivo: Informe_descargable.Rmd")
      
      # 1) Carpeta REAL (persistente)
      app_root <- getwd()
      export_dir_name <- "Descargas"
      export_path <- file.path(app_root, export_dir_name)
      dir.create(export_path, recursive = TRUE, showWarnings = FALSE)
      
      # 2) Guardar PNGs livianos en ./Descargas/
      suppressWarnings({
        ggplot2::ggsave(file.path(export_path, "hist.png"),
                        plot = gg_hist(), width = 10, height = 6, dpi = 140, bg = "white", device = "png")
        ggplot2::ggsave(file.path(export_path, "piramide.png"),
                        plot = gg_piramide(), width = 10, height = 6, dpi = 140, bg = "white", device = "png")
        ggplot2::ggsave(file.path(export_path, "cagr30.png"),
                        plot = gg_cagr30(), width = 12, height = 4.5, dpi = 140, bg = "white", device = "png")
      })
      
      # CSV filtrado opcional
      csv_path <- file.path(export_path, "base_filtrada.csv")
      utils::write.csv2(base_filtrada(), csv_path, row.names = FALSE, fileEncoding = "UTF-8")
      
      # tabla filtros
      filtros_tbl <- data.frame(
        Filtro = c("Año", "Departamento", "Municipio", "Clase", "Sexo"),
        Valor  = c(
          as.character(input$f_ano),
          as.character(input$f_dep),
          as.character(input$f_mun),
          as.character(input$f_clase),
          as.character(input$f_sexo)
        ),
        stringsAsFactors = FALSE
      )
      
      # 3) Render (PDF en temporal, pero app_root real)
      td <- tempfile("informe_pob_")
      dir.create(td, recursive = TRUE, showWarnings = FALSE)
      
      out_pdf <- rmarkdown::render(
        input = rmd_origen,
        output_format = "pdf_document",
        output_file = "Informe_descargable.pdf",
        output_dir = td,
        quiet = TRUE,
        params = list(
          app_root     = app_root,
          export_dir   = export_dir_name,
          filtros      = filtros_tbl,
          
          anio         = input$f_ano,
          dep          = input$f_dep,
          mpio         = input$f_mun,
          clase        = input$f_clase,
          sexo         = input$f_sexo,
          
          img_hist     = "hist.png",
          img_piramide = "piramide.png",
          img_cagr30   = "cagr30.png",
          
          img_map      = "hist.png",
          img_top10    = "piramide.png",
          
          csv_filtrado = csv_path,
          
          logo_path    = if (file.exists(file.path(app_root, "www", "LOGO_PLATEA.png")))
            file.path(app_root, "www", "LOGO_PLATEA.png") else NULL
        ),
        envir = new.env(parent = globalenv())
      )
      
      file.copy(out_pdf, file, overwrite = TRUE)
    }
  )
}

# =========================================================
# RUN
# =========================================================
shinyApp(ui, server)