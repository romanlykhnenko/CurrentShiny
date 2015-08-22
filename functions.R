


# function: bsCallPrice
# 
# Valuation of European call option in BSM model.
# Analytical formula.
#
# Args:
#
#       S0:     initial stock price
# 
#       K :     strike price
# 
#       T :     maturity date (in year fractions)
# 
#       r :     constant risk-free short rate
# 
#       sigma : volatility 
# 
# returns:
#
#       value : present value of the European call option

bsCallPrice <- function(S0, K, T, r, sigma) {
  
  d1 = (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  d2 = (log(S0 / K) + (r - 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  value = (S0 * pnorm(d1) - K * exp(-r * T) * pnorm(d2))
  
  return(value)
}


bsVega <- function(S0, K, T, r, sigma) {
  
  d1 = (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  vega = S0 * dnorm(d1) * sqrt(T)
  
  return(vega)
  
}

bsCallImpVol <- function(S0, K, T, r, C0, sigmaEst, it=100) {
  
  for( i in 1:it){
    sigmaEst <- sigmaEst - ((bsCallPrice(S0, K, T, r, sigmaEst) - C0)
                            / bsVega(S0, K, T, r, sigmaEst))
  }
  
  return(sigmaEst)
}

