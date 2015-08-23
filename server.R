# load packages
library(dplyr)
library(ggplot2)
library(shiny)
library(fOptions)

source("class.R")
source("functions.R")
source("funImpVola.R")

# VSTOXX index read from csv file
vstoxxIndex <- read.csv("Data/vstoxx_index.csv")

# Futures on  VSTOXX read from csv file
vstoxxFutures <- read.csv("Data/vstoxx_futures.csv")

# Options on VSTOXX read from csv file
vstoxxOptions <- read.csv("Data/vstoxx_options.csv")



shinyServer(function(input, output){
  
  # option calculator
  output$BSprice <- renderPrint({ print(BSworld(input$stock, input$strike,
                                                  input$maturity, input$rate, input$vola,
                                                  input$type))  })
  
  
  # used to filter option data with regard to date provided by a user
   tradingDate <- reactive({
     input$date
   })
   
   plotData <- reactive({
     impVola(tradingDate(), vstoxxOptions, vstoxxIndex )
   })
  
  # plot implied volatilities(for all maturities) for a provided date
  output$plotImplVola <- renderPlot({ 
    ggplot(plotData(), aes(STRIKE, ImpVol, group = MATURITY,
                         colour = MATURITY)) + geom_line()
 })
  
  
  # plot observed from the market  option prices (for all maturities) 
  # for a date specified by user
  output$plotObsPrices <- renderPlot({ 
    ggplot(plotData(), aes(STRIKE, PRICE, group = MATURITY,
                           colour = MATURITY)) + geom_line()
  })
  
  
  
  output$plotBM <- renderPlot({ 
    BS_price(input$strike)
  })
  
  })