########################
##
##
##
########################

shinyServer(function(input, output){
  output$resultado <- renderText(
    input$pintar,
    informacion <- ifelse(isolate(input$mostrar) == "summary", as.caharacter(summary), 
                      ifelse(isolate(input$mostrar) == "head", as.character(head), as.character(tail))),
    informacion(isolate(input$data))
  )
})