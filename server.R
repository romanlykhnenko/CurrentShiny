# load packages
library(dplyr)
library(ggplot2)
library(shiny)
library(fOptions)

source("class.R")
source("functions.R")
source("funImpVola.R")
source("plotGreeks.R")

# VSTOXX index read from csv file
vstoxxIndex <- read.csv("Data/vstoxx_index.csv")

# Futures on  VSTOXX read from csv file
vstoxxFutures <- read.csv("Data/vstoxx_futures.csv")

# Options on VSTOXX read from csv file
vstoxxOptions <- read.csv("Data/vstoxx_options.csv")



shinyServer(function(input, output){ 
  
  # option calculator: print method for class BSworld 
  output$BSprice <- renderPrint({ print(BSworld(input$stock, input$strike, 
                                                input$maturity, input$rate, 
                                                input$vola, input$type))  })
  # data to plot Delta of option 
  dataPlotDelta <- reactive({
    plotDelta(input$percent, input$stock, input$strike, input$maturity, input$rate, 
              input$vola, input$type)
  })
  

  # data to plot Gamma of option
  dataPlotGamma <- reactive({
    plotGamma(input$percent, input$stock, input$strike, input$maturity, input$rate, 
              input$vola, input$type)
  })
  

  # data to plot Vega of option 
  dataPlotVega <- reactive({
    plotVega(input$percent, input$stock, input$strike, input$maturity, input$rate, 
              input$vola, input$type)
  })
  
  
  # data to plot Rho of option
  dataPlotRho <- reactive({
    plotRho(input$percent, input$stock, input$strike, input$maturity, input$rate, 
              input$vola, input$type)
  })
  
  
  # select which Greek must be showed
  output$plotGreek <- renderPlot({ 
    if (input$greekType == "Delta") {
      ggplot(dataPlotDelta(), aes(underlying, Delta)) + geom_line(size = 1)
    } else if (input$greekType == "Gamma") {
      ggplot(dataPlotGamma(), aes(underlying, Gamma)) + geom_line(size = 1)
    } else if (input$greekType == "Vega") {
      ggplot(dataPlotVega(), aes(underlying, Vega)) + geom_line(size = 1)
    } else if (input$greekType == "Rho") {
      ggplot(dataPlotRho(), aes(underlying, Rho)) + geom_line(size = 1)
    }
  }) 
  
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
                         colour = MATURITY)) + geom_line(size = 1)
 })
  
  
  # plot observed from the market  option prices (for all maturities) 
  # for a date specified by user
  output$plotObsPrices <- renderPlot({ 
    ggplot(plotData(), aes(STRIKE, PRICE, group = MATURITY,
                           colour = MATURITY)) + geom_line(size = 1)
  })
  
  
  
  })