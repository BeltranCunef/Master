# Instalacion de paquetes

instalar <- function(pkg) if (!pkg %in% installed.packages()) install.packages(pkg)

instalar("ggplot2")
instalar("shiny")
instalar("shinythemes")
instalar("DT")
instalar("plotly")
