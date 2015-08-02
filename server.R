
library(shiny)
library(fOptions)
source("code1.R")
source("2.R")

shinyServer(function(input, output) {
  
  
  output$plotBM <- renderPlot({ 
    BS_price(input$strike)
  })
  
})