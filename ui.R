
shinyUI(navbarPage("Option Pricing",
        # part 1: Option Calculator      
        tabPanel("Option Calculator", 
      
            sidebarLayout(
              
              sidebarPanel(
                # input option characteristics
                selectInput("type", label = h3("Select type of the option"),
                            choices = list("Call option" = "call", "Put option" = "put")
                            ),
                
                sliderInput("stock","Select stock price", 50, 150, value = 100),
                
                sliderInput("strike","Select strike price", 50, 150, value = 100),
                
                sliderInput("maturity","Select maturity", 0.2, 2, value = 0.7),
                
                sliderInput("rate","Select risk free rate", 0.01, 0.05, value = 0.02),
                
                sliderInput("vola","Select volatility", 0.1, 2, value = 0.2),
                
                radioButtons("greekType", "Select one of Greeks to plot",
                               c("Delta", "Gamma","Vega", "Rho")),
                
                sliderInput("percent","rescale x-axis", 1, 100, value = 50)
                
              ),
              
              mainPanel(
                h1("Properties of your option"),
                verbatimTextOutput("BSprice"), # output option price and Greeks
                plotOutput("plotGreek") # output plot of one of the Greeks
                )
            )
        ),
            
   # part 2: VSTOXX Data    
   tabPanel("VSTOXX Data",
          
            selectInput("optionType", label = h3("Select type of the option"),
                        choices = list("Call option" = "call", "Put option" = "put")
                        ),
            
            radioButtons("selectPlot", "Select type of the plot",
                         c("Strike", "maturity")
                         ),
            
            plotOutput("descriptivePlot"), # plot price of option averaged accross
                                           # all dates
            
            # input of the date to select option data with regard to this date
            dateInput('date', label = h3("Date input"), value ='2014-03-27', 
                      min = '2014-01-02', max = '2014-03-31', format = "yyyy-mm-dd"),
            
            plotOutput("plotImplVola"), # plot volatility smiles 
            
            plotOutput("plotObsPrices") # plot option prices observed from market
            
            )
  
)) 
