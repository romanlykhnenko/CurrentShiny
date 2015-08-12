

library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Stock price and option price"),
  
  sliderInput("strike","Select strike price", 50, 150, value = 100),
  
  sliderInput("maturity","Select maturity", 0.2, 2, value = 0.7),
  
  sliderInput("rate","Select risk free rate", 0.01, 0.05, value = 0.02),
  
  verbatimTextOutput("BScallprise"),
  
  plotOutput("plotBM")
))
