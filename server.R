
library(shiny)
library(fOptions)
source("code1.R")
source("class.R")
#source("2.R")

shinyServer(function(input, output){
  
  output$BSprice <- renderPrint({ BSPrice(BSworld(input$stock, input$strike,
                                                  input$maturity, input$rate, input$vola,
                                                  input$type))  })
  
  output$plotBM <- renderPlot({ 
    BS_price(input$strike)
  })
  
  })