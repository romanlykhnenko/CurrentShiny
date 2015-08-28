# Valuation of options in BS model


# load libraries
library(fOptions)

# constructor for the class BSworld ###########################################

BSworld <- function(S0, K, T, r, sigma, type, C = 100) {
  
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
    #       C:      option price observed from the market(if not specified
    #               then 100 by default)
    # 
    # returns:
    #
    #       instance of the class BSworld with values of attributes specified by
    #       arguments of the function.
  
  
  # add input check
  
  instanceBSworld <-list(S0 = S0, K = K, T = T, r = r, sigma = sigma, 
                         type = type, C = C)
  class(instanceBSworld) <- "BSworld"
  instanceBSworld
  
}


# obj1 <- BSworld(100, 105, 1, 0.05, 0.02, "call")


# method 1: BSPrice

# reserve the name of the function, and use UseMethod command to tell R to 
# search for the correct function
BSPrice <- function(someClass) {
  
  UseMethod("BSPrice", someClass)
  
}

# definition of the method BSPrice for the class BSworld
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

# BSPrice(obj1)
# BScallPrice(2)

# method: delta

BSdelta <- function(someClass) {
  
  UseMethod("BSdelta", someClass)
  
}

BSdelta.BSworld <- function(instance.of.BSworld) {
  
  # get all attributes of the class BSworld using given instance of this class
  S0 <- instance.of.BSworld$S0
  K <- instance.of.BSworld$K
  T <- instance.of.BSworld$T
  r <- instance.of.BSworld$r
  sigma <- instance.of.BSworld$sigma
  type <- instance.of.BSworld$type
  
  d1 = (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  deltaCall <- pnorm(d1)
  
  deltaPut <- -pnorm(-d1)
  
  valueOut <- ifelse(type == "call", deltaCall, deltaPut)
  
  return(list(type = type, delta = valueOut)) # make better summary
  
}

# BSdelta(obj1)


# method: gamma

BSgamma <- function(someClass) {
  
  UseMethod("BSgamma", someClass)
  
}

BSgamma.BSworld <- function(instance.of.BSworld) {
  
  # get all attributes of the class BSworld using given instance of this class
  S0 <- instance.of.BSworld$S0
  K <- instance.of.BSworld$K
  T <- instance.of.BSworld$T
  r <- instance.of.BSworld$r
  sigma <- instance.of.BSworld$sigma
  type <- instance.of.BSworld$type
  
  d1 = (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  gammaCall <- dnorm(d1)/(S0*sigma*sqrt(T))
  
  gammaPut <- dnorm(d1)/(S0*sigma*sqrt(T))
  
  valueOut <- ifelse(type == "call", gammaCall, gammaPut)
  
  return(list(type = type, gamma = valueOut)) # make better summary
  
}

# BSgamma(obj1)


# method: rho

BSrho <- function(someClass) {
  
  UseMethod("BSrho", someClass)
  
}

BSrho.BSworld <- function(instance.of.BSworld) {
  
  # get all attributes of the class BSworld using given instance of this class
  S0 <- instance.of.BSworld$S0
  K <- instance.of.BSworld$K
  T <- instance.of.BSworld$T
  r <- instance.of.BSworld$r
  sigma <- instance.of.BSworld$sigma
  type <- instance.of.BSworld$type
  
  d2 <- (log(S0 / K) + (r - 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  rhoCall <- T*K*exp(-r * T)*pnorm(d2)
  
  rhoPut <- -T*K*exp(-r * T)*pnorm(-d2)
  
  valueOut <- ifelse(type == "call", rhoCall, rhoPut)
  
  return(list(type = type, rho = valueOut)) # make better summary
  
}

# BSrho(obj1)


# method: BSvega


# reserve the name of the function, and use UseMethod command to tell R to 
# search for the correct function
BSvega <- function(someClass) {
  
  UseMethod("BSvega", someClass)
  
}


BSvega.BSworld <- function(instance.of.BSworld){
  
  # get all attributes of the class BSworld using given instance of this class
  S0 <- instance.of.BSworld$S0
  K <- instance.of.BSworld$K
  T <- instance.of.BSworld$T
  r <- instance.of.BSworld$r
  sigma <- instance.of.BSworld$sigma
  type <- instance.of.BSworld$type
  
  d1 = (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  vega = S0 * dnorm(d1) * sqrt(T)
  
  return(list(type = type, vega = vega))
  
  }

# BSvega(obj1)


# method: BSimpVol.BSworld

# reserve the name of the function, and use UseMethod command to tell R to 
# search for the correct function
BSimpVol <- function(someClass) {
  
  UseMethod("BSimpVol", someClass)
  
}


BSimpVol.BSworld <- function(instance.of.BSworld){
  
  # get all attributes of the class BSworld using given instance of this class
  S0 <- instance.of.BSworld$S0
  K <- instance.of.BSworld$K
  T <- instance.of.BSworld$T
  r <- instance.of.BSworld$r
  sigma <- instance.of.BSworld$sigma
  C <- instance.of.BSworld$C
  
  CallImplVol <- GBSVolatility(price = C, TypeFlag = "c", S = S0, X = K,
                               Time = T, r = r, b = r)
  
  PutImplVol <- GBSVolatility(price = C, TypeFlag = "p", S = S0, X = K,
                              Time = T, r = r, b = r)
  
  valueOut <- ifelse(type == "call", CallImplVol, PutImplVol)
  
  return(list(type = type, implVol = valueOut))

  }


# BScallImpVol(obj1)
# obj1$sigma
# create instance of the class BSworld

# obj1 <- BSworld(100,105, 1, 0.05, 0.02)


# print method for the class BSworld

print.BSworld <- function(instance.of.BSworld) {
  
  # compute option characteristic for a object specified by user
  
  optionType <- instance.of.BSworld$type
  
  price <- BSPrice(instance.of.BSworld)[[2]]
  
  delta <- BSdelta(instance.of.BSworld)[[2]]
    
  gamma <- BSgamma(instance.of.BSworld)[[2]]
  
  rho   <- BSrho(instance.of.BSworld)[[2]]
  
  vega  <- BSvega(instance.of.BSworld)[[2]]
  
  # print procedure
  
  cat("Type of the option:\n")
  cat(paste(optionType, "\n\n"))
  
  cat("Price of the option based on Black-Scholes formula:\n")
  cat(paste(price, "\n\n"))
  
  cat("Delta:\n")
  cat(paste(delta, "\n\n"))
  
  cat("Gamma:\n")
  cat(paste(gamma, "\n\n"))
  
  cat("Rho:\n") 
  cat(paste(rho,"\n\n")) 
  
  cat("Vega:\n") 
  cat(paste(vega, "\n"))
  
} 

# print(obj1)





