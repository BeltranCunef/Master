library(shiny)
library(ggplot2)

shinyServer(function(input, output){
  output$miOutPut <- renderText(paste0("Este es el gráfico de: ", input$VariableX, " y ", input$VariableY))
  output$miPlot <- renderPlot({
    ggplot(mpg, aes_string( x = input$VariableX, y = input$VariableY)) + geom_point()
  })
})
  
  
