

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
    #       
    # 
    # returns:
    #
    #       instance of the class BSworld with values of attributes specified by
    #       arguments of the function.
  
  
  
  instanceBSworld <-list(S0 = S0, K = K, T = T, r = r, sigma = sigma, 
                         type = type)
  class(instanceBSworld) <- "BSworld"
  instanceBSworld
  
}


# obj1 <- BSworld(100, 105, 1, 0.05, 0.02, "call")


# method: BSPrice ##############################################################
#
# input: 
#        instance of the class BSworld
#
# output:
# 
#        option price based on Black-Scholes formula

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
  
  # call price using Black-Scholes formula
  valueCall <- (S0 * pnorm(d1) - K * exp(-r * T) * pnorm(d2))
  
  # put price using Black-Scholes formula
  valuePut <- K*exp(-r * T)*pnorm(-d2) - S0*pnorm(-d1)
  
  # based on type of the option select output price
  valueOut <- ifelse(type == "call", valueCall, valuePut)
  
  return(list(type = type, price = valueOut)) 
  
}

# BSPrice(obj1)
# BScallPrice(2)

# method: BSdelta ##############################################################
#
# input: 
#        instance of the class BSworld
#
# output:
# 
#        value of Delta


# reserve the name of the function, and use UseMethod command to tell R to 
# search for the correct function
BSdelta <- function(someClass) {
  
  UseMethod("BSdelta", someClass)
  
}

# definition of the method BSdelta for the class BSworld
BSdelta.BSworld <- function(instance.of.BSworld) {
  
  # get all attributes of the class BSworld using given instance of this class
  S0 <- instance.of.BSworld$S0
  K <- instance.of.BSworld$K
  T <- instance.of.BSworld$T
  r <- instance.of.BSworld$r
  sigma <- instance.of.BSworld$sigma
  type <- instance.of.BSworld$type
  
  d1 = (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  # calculate Delta for put 
  deltaCall <- pnorm(d1)
  
  # calculate Delta for call
  deltaPut <- -pnorm(-d1)
  
  # based on type of the option select output 
  valueOut <- ifelse(type == "call", deltaCall, deltaPut)
  
  return(list(type = type, delta = valueOut)) 
  
}

# BSdelta(obj1)


# method: BSgamma ##############################################################
#
# input: 
#        instance of the class BSworld
#
# output:
# 
#        value of Gamma


# reserve the name of the function, and use UseMethod command to tell R to 
# search for the correct function
BSgamma <- function(someClass) {
  
  UseMethod("BSgamma", someClass)
  
}

# definition of the method BSgamma for the class BSworld
BSgamma.BSworld <- function(instance.of.BSworld) {
  
  # get all attributes of the class BSworld using given instance of this class
  S0 <- instance.of.BSworld$S0
  K <- instance.of.BSworld$K
  T <- instance.of.BSworld$T
  r <- instance.of.BSworld$r
  sigma <- instance.of.BSworld$sigma
  type <- instance.of.BSworld$type
  
  d1 = (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  # value of Gamma for call
  gammaCall <- dnorm(d1)/(S0*sigma*sqrt(T))
  
  # value of Gamma for put
  gammaPut <- dnorm(d1)/(S0*sigma*sqrt(T))
  
  # based on type of the option select output
  valueOut <- ifelse(type == "call", gammaCall, gammaPut)
  
  return(list(type = type, gamma = valueOut)) # make better summary
  
}

# BSgamma(obj1)


# method: rho ##################################################################
#
# input: 
#        instance of the class BSworld
#
# output:
# 
#        value of Rho

# reserve the name of the function, and use UseMethod command to tell R to 
# search for the correct function
BSrho <- function(someClass) {
  
  UseMethod("BSrho", someClass)
  
}

# definition of the method BSrho for the class BSworld
BSrho.BSworld <- function(instance.of.BSworld) {
  
  # get all attributes of the class BSworld using given instance of this class
  S0 <- instance.of.BSworld$S0
  K <- instance.of.BSworld$K
  T <- instance.of.BSworld$T
  r <- instance.of.BSworld$r
  sigma <- instance.of.BSworld$sigma
  type <- instance.of.BSworld$type
  
  d2 <- (log(S0 / K) + (r - 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  # value of Rho for call
  rhoCall <- T*K*exp(-r * T)*pnorm(d2)
  
  # value of Rho for put
  rhoPut <- -T*K*exp(-r * T)*pnorm(-d2)
  
  # based on type of the option select output
  valueOut <- ifelse(type == "call", rhoCall, rhoPut)
  
  return(list(type = type, rho = valueOut)) # make better summary
  
}

# BSrho(obj1)


# method: BSvega ###############################################################
#
# input: 
#        instance of the class BSworld
#
# output:
# 
#        value of Vega


# reserve the name of the function, and use UseMethod command to tell R to 
# search for the correct function
BSvega <- function(someClass) {
  
  UseMethod("BSvega", someClass)
  
}

# definition of the method BSvega for the class BSworld
BSvega.BSworld <- function(instance.of.BSworld){
  
  # get all attributes of the class BSworld using given instance of this class
  S0 <- instance.of.BSworld$S0
  K <- instance.of.BSworld$K
  T <- instance.of.BSworld$T
  r <- instance.of.BSworld$r
  sigma <- instance.of.BSworld$sigma
  type <- instance.of.BSworld$type
  
  d1 = (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  # value of Vega
  vega = S0 * dnorm(d1) * sqrt(T)
  
  return(list(type = type, vega = vega))
  
  }

# BSvega(obj1)

# print method for the class BSworld ###########################################

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





