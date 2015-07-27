

library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Stock price and option price"),
  
  plotOutput("plotBM")
))
