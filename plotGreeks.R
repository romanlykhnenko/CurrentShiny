
# plot gamma of option  when stock price is being varied #######################

plotGamma <- function(n, S0, K, T, r, sigma, type, C = 100){
  
 # n is used to control the width of the interval used as X-axis in plot
  
  gammaValue <- function(stock){
    
    out <- BSgamma(BSworld(stock, K, T, r, sigma, type, C = 100))$gamma
    
    out
    
  }
  
  S0 <- seq(S0 - n*(S0/100), S0 + n*(S0/100), length = 10000)
  
  gammaValues <- sapply(S0, gammaValue)
  
  out <- data.frame(S0, gammaValues)
  colnames(out) <- c("underlying", "Gamma")
  
  out
  
}

# t1 <- plotGamma(50, 100, 105, 1, 0.05, 0.02, "call")
# ggplot(t1, aes(underlying, Gamma)) + geom_line(size = 1)
################################################################################

# plot delta when stock price is being varied ##################################
plotDelta <- function(n, S0, K, T, r, sigma, type, C = 100){
  
  # n is used to control the width of the interval used as X-axis in plot
  
  deltaValue <- function(stock){
    
    out <- BSdelta(BSworld(stock, K, T, r, sigma, type, C = 100))$delta
    
    out
    
  }
  
  S0 <- seq(S0 - n*(S0/100), S0 + n*(S0/100), length = 1000)
  
  deltaValues <- sapply(S0, deltaValue)
  
  out <- data.frame(S0, deltaValues)
  colnames(out) <- c("underlying", "Delta")
  
  out
  
}

#t1 <- plotDelta(50, 100, 105, 1, 0.05, 0.02, "call")
#ggplot(t1, aes(underlying, Delta)) + geom_line(size = 1)
################################################################################


# plot rho when stock price is being varied ##################################
plotRho <- function(n, S0, K, T, r, sigma, type, C = 100){
  
  # n is used to control the width of the interval used as X-axis in plot
  
  rhoValue <- function(stock){
    
    out <- BSrho(BSworld(stock, K, T, r, sigma, type, C = 100))$rho
    
    out
    
  }
  
  S0 <- seq(S0 - n*(S0/100), S0 + n*(S0/100), length = 1000)
  
  rhoValues <- sapply(S0, rhoValue)
  
  out <- data.frame(S0, rhoValues)
  colnames(out) <- c("underlying", "Rho")
  
  out
  
}

# t1 <- plotRho(50, 100, 105, 1, 0.05, 0.02, "call")
# ggplot(t1, aes(underlying, Rho)) + geom_line(size = 1)
################################################################################

# plot Vega when stock price is being varied ##################################
plotVega <- function(n, S0, K, T, r, sigma, type, C = 100){
  
  # n is used to control the width of the interval used as X-axis in plot
  
  vegaValue <- function(stock){
    
    out <- BSvega(BSworld(stock, K, T, r, sigma, type, C = 100))$vega
    
    out
    
  }
  
  S0 <- seq(S0 - n*(S0/100), S0 + n*(S0/100), length = 1000)
  
  vegaValues <- sapply(S0, vegaValue)
  
  out <- data.frame(S0, vegaValues)
  colnames(out) <- c("underlying", "Vega")
  
  out
  
}

# t1 <- plotVega(50, 100, 105, 1, 0.05, 0.02, "call")
# ggplot(t1, aes(underlying, Vega)) + geom_line(size = 1)
################################################################################








