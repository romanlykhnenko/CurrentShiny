

library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Select parameters of your option and get a Black-Scholes price"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput("stock","Select stock price", 50, 150, value = 100),
      
      sliderInput("strike","Select strike price", 50, 150, value = 100),
      
      sliderInput("maturity","Select maturity", 0.2, 2, value = 0.7),
      
      sliderInput("rate","Select risk free rate", 0.01, 0.05, value = 0.02),
      
      sliderInput("vola","Select volatility", 0.1, 2, value = 0.2)
      
    ),
    
    mainPanel(
      h1("Price of your call"),
      verbatimTextOutput("BScallprise"))
  ),
  
  

  
  plotOutput("plotBM")
))
