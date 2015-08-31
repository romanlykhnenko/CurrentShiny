
# plot gamma of option  when stock price is being varied #######################

plotGamma <- function(n, S0, K, T, r, sigma, type){
  
  #  input:
  #        n: percentage of initial stock price, used to control the width 
  #           of the interval to calculate Gamma
  #
  #       S0:     initial stock
  # 
  #       K :     strike price
  # 
  #       T :     maturity date (in year fractions)
  # 
  #       r :     constant risk-free short rate
  # 
  #       sigma : volatility 
  #       
  #       type:   call or put
  #
  #
  # output: data frame with 2 columns, the 1st is stock prices, the second
  #         values of Vega for a corresponding stock price
  #        
  
  # calculate gamma for a given stock price
  gammaValue <- function(stock){
    
    out <- BSgamma(BSworld(stock, K, T, r, sigma, type))$gamma
    
    out
    
  }
  # sequence of stock values used for calculation of Gamma
  S0 <- seq(S0 - n*(S0/100), S0 + n*(S0/100), length = 10000)
  
  # calculated Gammas
  gammaValues <- sapply(S0, gammaValue)
  
  out <- data.frame(S0, gammaValues)
  colnames(out) <- c("underlying", "Gamma")
  
  out
  
}



# plot delta when stock price is being varied ##################################
plotDelta <- function(n, S0, K, T, r, sigma, type){
  
  #  input:
  #        n: percentage of initial stock price, used to control the width 
  #           of the interval to calculate Delta
  #
  #       S0:     initial stock
  # 
  #       K :     strike price
  # 
  #       T :     maturity date (in year fractions)
  # 
  #       r :     constant risk-free short rate
  # 
  #       sigma : volatility 
  #       
  #       type:   call or put
  #
  #
  # output: data frame with 2 columns, the 1st is stock prices, the second
  #         values of Delta for a corresponding stock price
  #        
  
  # calculate Delta for a given stock price
  deltaValue <- function(stock){
    
    out <- BSdelta(BSworld(stock, K, T, r, sigma, type))$delta
    
    out
    
  }
  # sequence of stock values used for calculation of Gamma
  S0 <- seq(S0 - n*(S0/100), S0 + n*(S0/100), length = 1000)
  
  # calculated Deltas
  deltaValues <- sapply(S0, deltaValue)
  
  out <- data.frame(S0, deltaValues)
  colnames(out) <- c("underlying", "Delta")
  
  out
  
} 



# plot rho when stock price is being varied ##################################
plotRho <- function(n, S0, K, T, r, sigma, type){
  
  #  input:
  #        n: percentage of initial stock price, used to control the width 
  #           of the interval to calculate Rho
  #
  #       S0:     initial stock
  # 
  #       K :     strike price
  # 
  #       T :     maturity date (in year fractions)
  # 
  #       r :     constant risk-free short rate
  # 
  #       sigma : volatility 
  #       
  #       type:   call or put
  #
  #
  # output: data frame with 2 columns, the 1st is stock prices, the second
  #         values of Rho for a corresponding stock price
  #        
  
  # calculate Rho for a given stock price
  rhoValue <- function(stock){
    
    out <- BSrho(BSworld(stock, K, T, r, sigma, type))$rho
    
    out
    
  }
  # sequence of stock values used for calculation of Rho
  S0 <- seq(S0 - n*(S0/100), S0 + n*(S0/100), length = 1000)
  
  # calculated Rho
  rhoValues <- sapply(S0, rhoValue)
  
  out <- data.frame(S0, rhoValues)
  colnames(out) <- c("underlying", "Rho")
  
  out
  
}



# plot Vega when stock price is being varied ##################################
plotVega <- function(n, S0, K, T, r, sigma, type){
  
  #  input:
  #        n: percentage of initial stock price, used to control the width 
  #           of the interval to calculate Vega
  #
  #       S0:     initial stock
  # 
  #       K :     strike price
  # 
  #       T :     maturity date (in year fractions)
  # 
  #       r :     constant risk-free short rate
  # 
  #       sigma : volatility 
  #       
  #       type:   call or put
  #
  #
  # output: data frame with 2 columns, the 1st is stock prices, the second
  #         values of Vega for a corresponding stock price
  #        
  
  
  # calculate Vega for a given stock price
  vegaValue <- function(stock){
    
    out <- BSvega(BSworld(stock, K, T, r, sigma, type))$vega
    
    out
    
  }
  # sequence of stock values used for calculation of Vega
  S0 <- seq(S0 - n*(S0/100), S0 + n*(S0/100), length = 1000)
  
  # calculated Vega
  vegaValues <- sapply(S0, vegaValue)
  
  out <- data.frame(S0, vegaValues)
  colnames(out) <- c("underlying", "Vega")
  
  out
  
}










