# Downloading Option Chain Data from Google Finance in R

# quantmod #####################################################################
install.packages("XML")
install.packages("quantmod") #Install the quantmod library
library("quantmod")
library(XML)

AAPL.OPT <- getOptionChain("AAPL", NULL)

################################################################################

AAPL.OPT[[1]]



# installs RCurl and jsonlite packages. will prompt you to select mirror for download
install.packages("RCurl")
install.packages("jsonlite")

library(RCurl)
library(jsonlite)

getOptionQuote <- function(symbol){
  output = list()
  url = paste('http://www.google.com/finance/option_chain?q=', symbol, '&output=json', sep = "")
  x = getURL(url)
  fix = fixJSON(x)
  json = fromJSON(fix)
  numExp = dim(json$expirations)[1]
  for(i in 1:numExp){
    # download each expirations data
    y = json$expirations[i,]$y
    m = json$expirations[i,]$m
    d = json$expirations[i,]$d
    expName = paste(y, m, d, sep = "_")
    if (i > 1){
      url = paste('http://www.google.com/finance/option_chain?q=', symbol, '&output=json&expy=', y, '&expm=', m, '&expd=', d, sep = "")
      json = fromJSON(fixJSON(getURL(url)))
    }
    output[[paste(expName, "calls", sep = "_")]] = json$calls
    output[[paste(expName, "puts", sep = "_")]] = json$puts
  }
  return(output)
}

fixJSON <- function(json_str){
  stuff = c('cid','cp','s','cs','vol','expiry','underlying_id','underlying_price',
            'p','c','oi','e','b','strike','a','name','puts','calls','expirations',
            'y','m','d')
  
  for(i in 1:length(stuff)){
    replacement1 = paste(',"', stuff[i], '":', sep = "")
    replacement2 = paste('\\{"', stuff[i], '":', sep = "")
    regex1 = paste(',', stuff[i], ':', sep = "")
    regex2 = paste('\\{', stuff[i], ':', sep = "")
    json_str = gsub(regex1, replacement1, json_str)
    json_str = gsub(regex2, replacement2, json_str)
  }
  return(json_str)
}

aapl_opt = getOptionQuote("AAPL")


################################################################################

library(RCurl)
library(jsonlite)
library(plyr)

fixJSON <- function(json){
    gsub('([^,{:]+):', '"\1":', json)
  }

URL1 = 'http://www.google.com/finance/option_chain?q=%s&output=json'
URL2 = 'http://www.google.com/finance/option_chain?q=%s&output=json&expy=%d&expm=%d&expd=%d'

getOptionQuotes <- function(symbol){
   url = sprintf(URL1, symbol)
      
   chain = fromJSON(fixJSON(getURL(url)))
       #
   options = mlply(chain$expirations, function(y, m, d) {
              url = sprintf(URL2, symbol, y, m, d)
              expiry = fromJSON(fixJSON(getURL(url)))
               #
              expiry$calls$type = "Call"
              expiry$puts$type  = "Put"
                 #
              prices = rbind(expiry$calls, expiry$puts)
                   #
              prices$expiry = sprintf("%4d-%02d-%02d", y, m, d)
              prices$underlying.price = expiry$underlying_price
                     #
              prices
    })
         #
    options = cbind(data.frame(symbol), rbind.fill(options))
           #
    names(options) = c("price", "bid", "ask", "open.interest")
             #
    for (col in c("strike", "price", "bid", "ask")) options[, col] = as.numeric(options[, col])
    options[, "open.interest"] = suppressWarnings(as.integer(options[, "open.interest"]))
               #
    options[, c(1, 16, 15, 6, 10, 11, 17, 14, 12)]
}

AAPL = getOptionQuotes("AAPL")














