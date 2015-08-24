
plotDelta <- function(n, S0, K, T, r, sigma, type, C = 100){
  
 # n is used to control the width of the interval used as X-axis in plot
  
  deltaValue <- function(stock){
    
    out <- BSdelta(BSworld(stock, K, T, r, sigma, type, C = 100))$delta
    
    out
    
  }
  
  S0 <- seq(S0-n*(S0/100), S0+n*(S0/100), length = 1000)
  
  deltaValues <- sapply(S0, deltaValue)
  
  out <- data.frame(S0, deltaValues)
  colnames(out) <- c("underlying", "Delta")
  
  out
  
}

#t1 <- plotDelta(50, 100, 105, 1, 0.05, 0.02, "call")
#ggplot(t1, aes(underlying, Delta)) + geom_line(size = 1)


