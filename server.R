
library(shiny)
library(fOptions)
source("code1.R")
source("class.R")
#source("2.R")

shinyServer(function(input, output){
  
  output$BScallprise <- renderPrint({ BScallPrice(BSworld(100,input$strike, input$maturity,
                                                          input$rate, 0.02))  })
  
  output$plotBM <- renderPlot({ 
    BS_price(input$strike)
  })
  
  })