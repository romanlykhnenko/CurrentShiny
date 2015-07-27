
library(shiny)
library(fOptions)
source("code1.R")

shinyServer(function(input, output) {
  
  
  output$plotBM <- renderPlot({ 
    BS_price(input$strike)
  })
  
})