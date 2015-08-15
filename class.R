# Valuation of call options in BS model


# load libraries
library(fOptions)

# constructor for the class BSworld ###########################################

BSworld <- function(S0, K, T, r, sigma, type) {
  
    # Arguments of the function (attributes of the class BSworld)
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
    # returns:
    #
    #       instance of the class BSworld with values of attributes specified by
    #       arguments of the function.
  
  
  # add input check
  
  instanceBSworld <-list(S0 = S0, K = K, T = T, r = r, sigma = sigma, type = type)
  class(instanceBSworld) <- "BSworld"
  instanceBSworld
  
}


obj1 <- BSworld(100,105, 1, 0.05, 0.02, "call")

# method 1: BScallPrice

# reserve the name of the function, and use UseMethod command to tell R to 
# search for the correct function
BSPrice <- function(someClass) {
  
  UseMethod("BSPrice", someClass)
  
}

# definition of the method BScallPrice for the class BSworld
BSPrice.BSworld <- function(instance.of.BSworld) {
  
  # get all attributes of the class BSworld using given instance of this class
  S0 <- instance.of.BSworld$S0
  K <- instance.of.BSworld$K
  T <- instance.of.BSworld$T
  r <- instance.of.BSworld$r
  sigma <- instance.of.BSworld$sigma
  type <- instance.of.BSworld$type
  
  d1 <- (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  d2 <- (log(S0 / K) + (r - 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  valueCall <- (S0 * pnorm(d1) - K * exp(-r * T) * pnorm(d2))
  
  valuePut <- K*exp(-r * T)*pnorm(-d2) - S0*pnorm(-d1)
  
  valueOut <- ifelse(type == "call", valueCall, valuePut)
  
  return(list(type = type, price = valueOut)) # make better summary
  
}

BSPrice(obj1)
# BScallPrice(2)


# method 2: BSvega


# reserve the name of the function, and use UseMethod command to tell R to 
# search for the correct function
BSvega <- function(someClass) {
  
  UseMethod("BSvega", someClass)
  
}


BSvega.BSworld <- function(instance.of.BSworld) {
  
  # get all attributes of the class BSworld using given instance of this class
  S0 <- instance.of.BSworld$S0
  K <- instance.of.BSworld$K
  T <- instance.of.BSworld$T
  r <- instance.of.BSworld$r
  sigma <- instance.of.BSworld$sigma
  
  d1 = (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  vega = S0 * pnorm(d1) * sqrt(T)
  
  return(vega)
  
}

# BSvega(obj1)


# method3: BScallImpVol.BSworld

# reserve the name of the function, and use UseMethod command to tell R to 
# search for the correct function
BScallImpVol <- function(someClass) {
  
  UseMethod("BScallImpVol", someClass)
  
}


BScallImpVol.BSworld <- function(instance.of.BSworld) {
  
  # get all attributes of the class BSworld using given instance of this class
  S0 <- instance.of.BSworld$S0
  K <- instance.of.BSworld$K
  T <- instance.of.BSworld$T
  r <- instance.of.BSworld$r
  sigma <- instance.of.BSworld$sigma
  
  implVol <- GBSVolatility(price = C0, TypeFlag = "c", S = S0, X = K, Time = T, r = r, b = 0)
  
  return(implVol)
}


# BScallImpVol(obj1)

# create instance of the class BSworld

# obj1 <- BSworld(100,105, 1, 0.05, 0.02)
