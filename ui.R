

library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Stock price and option price"),
  
  sliderInput("strike","Select strike price", 50, 150, value = 100),
  
  plotOutput("plotBM")
))
