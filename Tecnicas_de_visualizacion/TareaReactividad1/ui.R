#############################
##
##
##
#############################

library(shiny)
library(datasets)
library(ggplot2)

data("mpg")
data("mtcars")
data("cars")
data("anscombe")
data("trees")
data("Titanic")

shinyUI(
  fluidPage(
    titlePanel("Tarea Reactividad")
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "data", label = "Data Set a elegir",
        choices = list(
          "MPG" = "mpg",
          "MTCARS" = "mtcars",
          "CARS" = "cars",
          "ANSCOMBE" = "anscombe",
          "TREES" = "trees",
          "TITANIC" = "Titanic"
        ), 
        selected = "mpg"
      ),
      selectInput(
        "mostrar", label = "Información a mostrar",
        choices = list(
          "Mostrar summary" = "summary",
          "Mostrar las primeras filas" = "head",
          "Mostrar las últimas filas" = "tail"
        ),
        selected = "summary"
      ),
      actionButton(
        "pintar", label = "Mostrar resultado"
      )
    ),
    mainPanel(
      verbatimTextOutput("resultado")
    )
  )
)