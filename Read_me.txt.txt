Read_me.txt

Estos son los paquetes necesarios para correr cada base de datos

pkgs <- c("shiny","bslib","shinyWidgets","leaflet","sf","dplyr","tidyr",
          "scales","htmltools","plotly","stringi","readr","tibble",
          "shinyjs","DT","ggplot2","webshot2","htmlwidgets","ragg",
          "glue","networkD3","stringr")
suppressWarnings(invisible(sapply(pkgs, require, character.only = TRUE)))
