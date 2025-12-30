#!/usr/bin/env Rscript
# ============================================================================
# Script de instalación de dependencias para FAO_COLOMBIA en AWS RServer
# ============================================================================
# 
# Uso: Rscript install_dependencies.R
# 
# Este script instala automáticamente todos los paquetes R necesarios
# para ejecutar las aplicaciones Shiny del proyecto FAO_COLOMBIA
#
# Requisitos previos:
# - Instalar dependencias de sistema (ver DEPLOYMENT_ANALYSIS.md)
# - R 4.0 o superior
# - Acceso a CRAN (internet)

options(repos = "https://cran.r-project.org")

# Paquetes requeridos
paquetes_requeridos <- c(
  # Aplicaciones web y UI
  "shiny",
  "bslib", 
  "shinyWidgets",
  "shinyjs",
  
  # Manipulación de datos
  "dplyr",
  "tidyr",
  "readr",
  "tibble",
  "haven",
  
  # Visualización
  "ggplot2",
  "plotly",
  "leaflet",
  "scales",
  "networkD3",
  "ragg",
  "kableExtra",
  
  # Datos geoespaciales
  "sf",
  "bsicons",
  
  # HTML y tablas
  "htmltools",
  "htmlwidgets",
  "DT",
  "webshot2",
  
  # Procesamiento de texto
  "stringi",
  "stringr"
)

# Colores para output
color_verde <- "\033[0;32m"
color_rojo <- "\033[0;31m"
color_amarillo <- "\033[1;33m"
color_azul <- "\033[0;34m"
color_reset <- "\033[0m"

# Función para imprimir con color
print_status <- function(paquete, status, mensaje = "") {
  if (status == "OK") {
    cat(sprintf("%s✓%s %-20s %s\n", color_verde, color_reset, paquete, mensaje))
  } else if (status == "INSTALANDO") {
    cat(sprintf("%s●%s %-20s %s\n", color_azul, color_reset, paquete, mensaje))
  } else if (status == "ADVERTENCIA") {
    cat(sprintf("%s⚠%s %-20s %s\n", color_amarillo, color_reset, paquete, mensaje))
  } else {
    cat(sprintf("%s✗%s %-20s %s\n", color_rojo, color_reset, paquete, mensaje))
  }
}

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  Instalador de dependencias R para FAO_COLOMBIA en AWS        ║\n")
cat("║  Versión 1.0 - Diciembre 2025                                ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# Información del sistema
cat(sprintf("Sistema: %s\n", Sys.info()["sysname"]))
cat(sprintf("Versión de R: %s\n", paste(R.version$major, R.version$minor, sep = ".")))
cat(sprintf("Número de núcleos disponibles: %d\n", parallel::detectCores()))
cat(sprintf("Repositorio CRAN: %s\n\n", getOption("repos")[1]))

# Verificar paquetes instalados
cat(sprintf("%s=== VERIFICACIÓN INICIAL ===%s\n", color_azul, color_reset))
paquetes_instalados <- sapply(paquetes_requeridos, function(x) {
  require(x, character.only = TRUE, quietly = TRUE)
})

paquetes_faltantes <- names(paquetes_instalados)[!paquetes_instalados]

if (length(paquetes_faltantes) == 0) {
  cat("✓ Todos los paquetes ya están instalados.\n\n")
  cat(sprintf("%s=== VERIFICACIÓN FINAL ===%s\n", color_azul, color_reset))
  
  for (pkg in paquetes_requeridos) {
    print_status(pkg, "OK")
  }
  
  cat(sprintf("\n%s✓ Instalación completada exitosamente.%s\n\n", color_verde, color_reset))
  quit(status = 0)
}

cat(sprintf("Paquetes a instalar: %d\n", length(paquetes_faltantes)))
cat(sprintf("Paquetes ya instalados: %d\n\n", sum(paquetes_instalados)))

# Instalar paquetes faltantes
cat(sprintf("%s=== INSTALANDO PAQUETES ===%s\n\n", color_azul, color_reset))

for (pkg in paquetes_faltantes) {
  print_status(pkg, "INSTALANDO")
  
  resultado <- tryCatch({
    install.packages(pkg,
                    repos = "https://cran.r-project.org",
                    Ncpus = parallel::detectCores(),
                    type = "source",
                    dependencies = TRUE,
                    quiet = TRUE)
    TRUE
  }, warning = function(w) {
    cat(sprintf("\n  %sAdvertencia: %s%s\n", color_amarillo, w$message, color_reset))
    TRUE
  }, error = function(e) {
    cat(sprintf("\n  %sError: %s%s\n", color_rojo, e$message, color_reset))
    FALSE
  })
  
  if (resultado) {
    print_status(pkg, "OK", "Instalado correctamente")
  } else {
    print_status(pkg, "ERROR", "Falló la instalación")
  }
}

# Verificación final
cat(sprintf("\n%s=== VERIFICACIÓN FINAL ===%s\n\n", color_azul, color_reset))

errores <- c()
advertencias <- c()

for (pkg in paquetes_requeridos) {
  resultado <- tryCatch({
    require(pkg, character.only = TRUE, quietly = TRUE)
  }, error = function(e) {
    FALSE
  })
  
  # Intentar cargar nuevamente en caso de fallo anterior
  if (!resultado) {
    resultado <- tryCatch({
      library(pkg, character.only = TRUE, quietly = TRUE)
    }, error = function(e) {
      FALSE
    })
  }
  
  if (resultado) {
    print_status(pkg, "OK")
  } else {
    print_status(pkg, "ERROR")
    errores <- c(errores, pkg)
  }
}

# Resumen final
cat(sprintf("\n%s════════════════════════════════════════════════════════════════%s\n", 
            color_azul, color_reset))

if (length(errores) == 0) {
  cat(sprintf("%s✓ INSTALACIÓN COMPLETADA EXITOSAMENTE%s\n", color_verde, color_reset))
  cat(sprintf("Todos los %d paquetes están correctamente instalados.\n", length(paquetes_requeridos)))
  cat(sprintf("El sistema está listo para ejecutar las aplicaciones Shiny de FAO_COLOMBIA.\n"))
  cat(sprintf("%s════════════════════════════════════════════════════════════════%s\n\n", 
              color_azul, color_reset))
  quit(status = 0)
} else {
  cat(sprintf("%s✗ INSTALACIÓN INCOMPLETA%s\n", color_rojo, color_reset))
  cat(sprintf("Paquetes con problemas: %s\n", paste(errores, collapse = ", ")))
  cat(sprintf("Intentar resolver manualmente:\n"))
  for (err in errores) {
    cat(sprintf("  install.packages('%s', dependencies = TRUE)\n", err))
  }
  cat(sprintf("%s════════════════════════════════════════════════════════════════%s\n\n", 
              color_azul, color_reset))
  quit(status = 1)
}
