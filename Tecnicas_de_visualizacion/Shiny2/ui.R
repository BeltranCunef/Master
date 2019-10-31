library(shiny)
library(ggplot2)

shinyUI(
  fluidPage(
    titlePanel(
      "Ejercicio"
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput("VariableX", 
                    label = ("Elige la variable X"), 
                    choices = list("Tamaño de motor" = "displ", "Cilindrada" = "cyl",
                                   "Consumo en ciudad" = "cty", 
                                   "Consumo en autopista" = "hwy"), 
                    selected = "displ"),
        selectInput("VariableY", 
                    label = ("Elige la variable Y"), 
                    choices = list("Tamaño de motor" = "displ", "Cilindrada" = "cyl",
                                   "Consumo en ciudad" = "cty", 
                                   "Consumo en autopista" = "hwy"), 
                    selected = "cyl")       
      ),
      mainPanel(
        textOutput("miOutPut"),
        plotOutput("miPlot")
      )
    )
  )
  
)