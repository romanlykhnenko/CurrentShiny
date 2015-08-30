# load packages
library(dplyr)
library(ggplot2)
library(shiny)
library(fOptions)

# VSTOXX index read from csv file
vstoxxIndex <- read.csv("Data/vstoxx_index.csv")

# Options on VSTOXX read from csv file
vstoxxOptions <- read.csv("Data/vstoxx_options.csv")

# load files to be used
source("class.R")
source("functions.R")
source("funImpVola.R")
source("plotGreeks.R")


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
   
  # data to plot implied volat. and option prices for a given data
   plotData <- reactive({
     impVola(tradingDate(), vstoxxOptions, vstoxxIndex )
   })
  
  # plot implied volatilities(for all maturities) for a provided date
  output$plotImplVola <- renderPlot({ 
    ggplot(plotData(), aes(STRIKE, ImpVol, group = MATURITY,
                         colour = MATURITY)) + 
      geom_line(size = 1) 
#       labs(x="Strike", y="Implied volatility") +
#       ggtitle("Implied volatility as a function of strike") +
#       theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", 
#                                       face="bold", size=25, hjust=0.5))
 })
  
  
  # plot observed from the market option prices (for all maturities) 
  # for a date specified by user
  output$plotObsPrices <- renderPlot({ 
    ggplot(plotData(), aes(STRIKE, PRICE, group = MATURITY,
                           colour = MATURITY)) + geom_line(size = 1)
  })
  
  # select descriptive plot to show
  output$descriptivePlot <- renderPlot({
    if ((input$optionType == "call")&(input$selectPlot == "Strike")){
      ggplot(StrikeValueCall(vstoxxOptions), aes(STRIKE, meanCallValueStrike)) + 
        geom_line()
    } else if ((input$optionType == "call")&(input$selectPlot == "maturity")){
      ggplot(ttmValueCall(vstoxxOptions), aes(TTM, meanCallValueTTM)) + 
        geom_line()
    } else if ((input$optionType == "put")&(input$selectPlot == "Strike")){
      ggplot(StrikeValuePut(vstoxxOptions), aes(STRIKE, meanPutValueStrike)) + 
        geom_line()
    } else if ((input$optionType == "put")&(input$selectPlot == "maturity")){
      ggplot(ttmValuePut(vstoxxOptions), aes(TTM, meanPutValueTTM)) + 
        geom_line()
      }
  })
  

  })