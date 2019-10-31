#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Distribuciones"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel( 
            sliderInput("tamMuestra",
                        label = "Tamaño muestral",
                        min = 5,
                        max = 500,
                        value = 100),
            selectInput("distribucion",
                        label = "Tipo de distribución",
                        choices = list("Gaussiana" = "rnorm", "Uniforme" = "uniform"),
                        selected = "norm"),
            actionButton("pintar",
                        label = "Representar función")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) { # isolate sirve para aislar los input 

    output$distPlot <- renderPlot({
       input$pintar
       rdistribucion <- ifelse(isolate(input$distribucion) == "rnorm", rnorm, runif)
       hist(rdistribucion(isolate(input$tamMuestra)),
            main = "Distribucion",
            col = "pink")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
