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
      # uiOutput sirve para inyectar interfaces desde el servidor
      uiOutput(
        "parametros"
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
  output$parametros <- renderUI({
    if (input$selector == "poisson") {
      numericInput(
        "lambda",
        label = "Lambda",
        value = 1
      )
    }
    else if (input$selector == "gamma") {
      # en un renderUi si quieres devolver varias opciones hay que usar una taglist
      tagList(
        numericInput(
          "shape",
          label = "Shape",
          value = 1
        ),
        numericInput(
          "rate",
          label = "Rate",
          value = 1
        ) 
      )
    }
  })
  output$miOutput <- renderPlot({
    
    # esto se pone para comprobar que los inputs existen
    # notese que estan en un renderUi, por lo que tardan algo en ponerse
    # un req si descubre un null para la ejecucion
    # solo son necesarios cuando hay un renderUi o cuando haya algun input que al iniciar el shiny
    # sea un valor nulo
    
    distribucion <- 
      if (input$selector == "poisson") {
        req(input$lambda)
        rpois(1000, input$lambda)
      } 
      else if (input$selector == "gamma") {
        req(input$shape)
        req(input$rate)
        rgamma(1000, input$shape, input$rate)
      }
    
    hist(distribucion, 
         main = "Histograma",
         col = "red")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
