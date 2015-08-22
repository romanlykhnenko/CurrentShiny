# load packages
library(dplyr)
library(ggplot2)
library(shiny)
library(fOptions)

source("class.R")
source("funImpVola.R")
source("functions.R")



# VSTOXX index read from csv file
vstoxxIndex <- read.csv("Data/vstoxx_index.csv")

# Futures on  VSTOXX read from csv file
vstoxxFutures <- read.csv("Data/vstoxx_futures.csv")

# Options on VSTOXX read from csv file
vstoxxOptions <- read.csv("Data/vstoxx_options.csv")



shinyServer(function(input, output){
  
  output$BSprice <- renderPrint({ BSPrice(BSworld(input$stock, input$strike,
                                                  input$maturity, input$rate, input$vola,
                                                  input$type))  })
  
  
  
  output$dateOption <- renderPrint({ input$date })
  
  plotData <- reactive({
     impVola(input$date, vstoxxOptions)
    
  })
    
  output$plotImplVola <- renderPlot({ 
    ggplot(plotData(), aes(STRIKE, ImpVol, group = MATURITY,
                         colour = MATURITY)) + geom_line()
  })
  
  output$plotBM <- renderPlot({ 
    BS_price(input$strike)
  })
  
  })