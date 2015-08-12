################################################################################
# this file contains a set of function
#
#
################################################################################

# fun1 : 

# library(fOptions)
# 
# # Valuation of the call price for each value of the underlying
# Call    = GBSOption(TypeFlag = "c", S = St, X = K, Time = tau, r = r, b = 0, sigma = si)  
# Callp   = attr(Call, "price")

################################################################################
# bs_call
# 
# Valuation of European call option in BSM model.
# Analytical formula.
#
# Args:
#
#       S0:     initial stock
# 
#       K :     strike price
# 
#       T :     maturity date (in year fractions)
# 
#       r :     constant risk-free short rate
# 
#       sigma : volatility factor in diffusion term
# 
# returns:
#
#       value : present value of the European call option
################################################################################

bs_call_price <- function(S0, K, T, r, sigma) {

  d1 = (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  d2 = (log(S0 / K) + (r - 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  value = (S0 * pnorm(d1) - K * exp(-r * T) * pnorm(d2))
  
  return(value)
}


# Vega of European option in BSM model.
# 
# Parameters
# ==========
#   S0 : float
# initial stock/index level
# K : float
# strike price
# T : float
# maturity date (in year fractions)
# r : float
# constant risk-free short rate
# sigma : float
# volatility factor in diffusion term
# 
# Returns
# =======
#   vega : float
# partial derivative of BSM formula with respect
# to sigma, i.e. Vega

bs_vega <- function(S0, K, T, r, sigma) {
  
  d1 = (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  vega = S0 * pnorm(d1) * sqrt(T)
  
  return(vega)

}



# Implied volatility of European call option in BSM model
# 
# Parameters
# ==========
#   S0 : float
# initial stock/index level
# K : float
# strike price
# T : float
# maturity date (in year fractions)
# r : float
# constant risk-free short rate
# sigma_est : float
# estimate of impl. volatility
# it : integer
# number of iterations
# 
# Returns
# =======
#   simga_est : float
# numerically estimated implied volatility


bs_call_imp_vol <- function(S0, K, T, r, C0, sigmaEst, it=100) {

  for( i in 1:it){
    sigmaEst <- sigmaEst - ((bs_call_price(S0, K, T, r, sigmaEst) - C0)
                  / bs_vega(S0, K, T, r, sigmaEst))
  }
    
  return(sigmaEst)
}


# performance ##################################################################

S0 <- 100
K <- 105
T <- 1
r <- 0.05
sigmaEst <- 0.23
# sigma <- sigmaEst
 C0 <- 10
# 
# bs_call_price(S0, K, T, r, sigma)
# 
# bs_vega(S0, K, T, r, sigma)
# 
bs_call_imp_vol(S0, K, T, r, sigmaEst, C0)

GBSVolatility(price = C0, TypeFlag = "c", S = S0, 
              X = K, Time = T, r = r, b = 0)


#
install.packages("plot3D")
library(plot3D)



par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 1))
R <- 3
r <- 2
strike <- seq(100, 110,length.out=50)
maturity <- seq(0.5, 2,length.out=50)
callPrice <- seq(30, 40, length.out = 50)
r = 0.05
z = GBSVolatility(price = callPrice, TypeFlag = "c", S = 100, 
                  X = strike, Time = maturity, r = r, b = r)




M <- mesh(strike, maturity)

alpha <- M$x
beta <- M$y


surf3D(x = M$x,
       y = M$y,
       z = GBSVolatility(price = callPrice, TypeFlag = "c", S = 100, 
                         X = strike, Time = maturity, r = r, b = 0) ,
       colkey=FALSE,
       bty="b2",
       main="Half of a Torus")


# binomial trees

CRRBinomialTreeOption(TypeFlag = "ce", S = 900, X = 950,
                       Time = 1/4, r = 0.02, b = 0.02, sigma = 0.22, n = 3)@price

CRRTree <- BinomialTreeOption(TypeFlag = "ce", S = 900, X = 950,
                              Time = 1/4, r = 0.02, b = 0.02, sigma = 0.22, n = 3)

BinomialTreePlot(CRRTree, dy = 1, xlab = "Time steps",
                   ylab = "Number of up steps", xlim = c(0,4))

title(main = "Call Option Tree")

# compare BS price and BinTrees price

# page 93

# greeks
# for straddle and other strategies

# impied volatility smile
























