########################
# Autor: Beltrán Aller López
# Fecha: 30/10/2019
#
########################

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Shiny4"),

    # Sidebar with a selectInput and 2 conditionalPanel and both of them have one numericInput 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "selector",
                label = h3("Seleccionar distribución"),
                choices = list(
                    "Poisson" = "poisson",
                    "Gamma" = "gamma"
                ),
                selected = "gamma"
            ),
            # la funcion de un conditionalPanel es que aparezca algo o no en funcion de la condicion dada
            conditionalPanel(
                condition = "input.selector == 'poisson'",
                numericInput(
                    "lambda",
                    label = h3("lambda"),
                    value = 5
                )
            ),
            conditionalPanel(
                condition = "input.selector == 'gamma'",
                numericInput(
                    "shape",
                    label = h3("shape"),
                    value = 5
                ),
                numericInput(
                    "rate",
                    label = h3("rate"),
                    value = 5
                )
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput(
              "miOutput"
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$miOutput <- renderPlot({
      distribucion <- if (input$selector == "poisson") {
          rpois(1000, input$lambda)
      } 
      else(
          rgamma(1000, input$shape, input$rate)
      )
    
      hist(distribucion, 
           main = "Histograma",
           col = "red")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
