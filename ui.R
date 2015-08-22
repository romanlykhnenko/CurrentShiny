

library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Part 1: option calculator"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("type", label = h3("Select type of the option"),
                  choices = list("Call option" = "call", "Put option" = "put")),
      
      sliderInput("stock","Select stock price", 50, 150, value = 100),
      
      sliderInput("strike","Select strike price", 50, 150, value = 100),
      
      sliderInput("maturity","Select maturity", 0.2, 2, value = 0.7),
      
      sliderInput("rate","Select risk free rate", 0.01, 0.05, value = 0.02),
      
      sliderInput("vola","Select volatility", 0.1, 2, value = 0.2)
      
    ),
    
    mainPanel(
      h1("Price of your option"),
      verbatimTextOutput("BSprice"))
  ),
  
  titlePanel("Part 2: real data"),
  
  # input of the date to select option trading day
  dateInput("date", label = h3("Date input"), value = "2014-03-31"),
  
  fluidRow(column(3, verbatimTextOutput("dateOption"))),
  
  plotOutput("plotImplVola"),
  
  plotOutput("plotBM")
))
