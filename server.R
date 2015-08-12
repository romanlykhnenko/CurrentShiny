
library(shiny)
library(fOptions)
source("code1.R")
source("class.R")
#source("2.R")

shinyServer(function(input, output){
  
  output$BScallprise <- renderPrint({ BScallPrice(BSworld(input$stock,input$strike, input$maturity,
                                                          input$rate, input$vola))  })
  
  output$plotBM <- renderPlot({ 
    BS_price(input$strike)
  })
  
  })