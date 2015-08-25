# load packages
library(dplyr)
library(ggplot2)

################################################################################
# VSTOXX index read from csv file
vstoxxIndex <- read.csv("Data/vstoxx_index.csv")

# Futures on  VSTOXX read from csv file
vstoxxFutures <- read.csv("Data/vstoxx_futures.csv")

# Options on VSTOXX read from csv file
vstoxxOptions <- read.csv("Data/vstoxx_options.csv")

# add file data preparation: coerce to required formats etc., 
# add time to maturity

################################################################################

# select trading date 
dateOption = as.Date("2014-03-31")

  
# take Options traded on 2014-03-31
vstoxxOptions3103 <- dplyr::filter(vstoxxOptions, 
                                   as.Date(DATE, format = "%Y-%m-%d ") == 
                                     dateOption)

# add time to maturity (in years) as a column
vstoxxOptions3103 = mutate(vstoxxOptions3103, 
                           TTM = as.numeric((as.Date(MATURITY, format = "%Y-%m-%d ")
                                             - as.Date(DATE, format = "%Y-%m-%d ") ) /360))


# select all distinct maturities
# distinct(select(vstoxxOptions3103, MATURITY))

# select all distinct strikes
# distinct(select(vstoxxOptions3103, STRIKE))

# select only call options
vstoxxOptions3103call = dplyr::filter(vstoxxOptions3103, TYPE == 'C')

# select only put options
vstoxxOptions3103put = dplyr::filter(vstoxxOptions3103, TYPE == 'P')

# VSTOXX value
V0 <- vstoxxIndex %>%
  dplyr::filter(as.Date(Date) == dateOption ) %>%
  select(V2TX)

# coerce dataframe to a number
V0 <- as.numeric(as.vector(V0))


# short rate
r  <- 0.01

# add column with implied volatilities
vstoxxOptions3103call = mutate(vstoxxOptions3103call,
                               ImpVol = bsCallImpVol(V0, STRIKE, TTM, r, 
                                                     PRICE, 2, 100))

# remove rows with NA --- leads to lose of data that's why different plots
na.omit(vstoxxOptions3103call)

# select options with non-zero maturity
plotData <- dplyr::filter(vstoxxOptions3103call, ImpVol > 0)

# coerce to factor
plotData$MATURITY <- factor(plotData$MATURITY)

# plot implied volatilities for different maturities
ggplot(plotData, aes(STRIKE, ImpVol, group = MATURITY,
                     colour = MATURITY)) + geom_line()

# plot prices of options for different maturities
ggplot(plotData, aes(STRIKE, PRICE, group = MATURITY,
                     colour = MATURITY)) + geom_line()

# descriptive statistics
plotData %>%
  group_by(MATURITY, STRIKE)%>%
  select(MATURITY, STRIKE, PRICE, ImpVol)%>%
  summarise(sum(PRICE), sum(ImpVol))


################################################################################

vstoxxOptions$DATE <- as.Date(vstoxxOptions$DATE, "%Y-%m-%d ")

vstoxxOptions$MATURITY <- as.Date(vstoxxOptions$MATURITY, "%Y-%m-%d ")

# add time to maturity (in years) as a column
vstoxxOptions = mutate(vstoxxOptions, 
                           TTM = as.numeric((as.Date(MATURITY, format = "%Y-%m-%d ")
                                             - as.Date(DATE, format = "%Y-%m-%d ") ) /360))

# quick overview of the data
summary(vstoxxOptions)
str(vstoxxOptions)
factor(vstoxxOptions$TTM)
factor(vstoxxOptions$MATURITY)
factor(vstoxxOptions$STRIKE)

# mistakes in data, since TTM can not be negative
filter(vstoxxOptions, TTM < 0)

# select call options

# group by STRIKE
StrikeValueCall <- vstoxxOptions %>%
  dplyr::filter(TTM >= 0) %>%
  dplyr::filter(TYPE == 'C' ) %>%
  group_by(STRIKE) %>%
  summarise(meanCallValueStrike = mean(PRICE))

# plot: strike vs mean value of the options accros strikes
ggplot(StrikeValueCall, aes(STRIKE, meanCallValueStrike)) + geom_line()

# groupy by TTM

ttmValueCall <- vstoxxOptions %>%
  filter(TTM >= 0) %>%
  filter(TYPE == 'C' ) %>%
  group_by(TTM) %>%
  summarise(meanCallValueTTM = mean(PRICE))

# plot: strike vs mean value of the options accros strikes
ggplot(ttmValueCall, aes(TTM, meanCallValueTTM)) + geom_line()

# select put options

# group by STRIKE
StrikeValuePut <- vstoxxOptions %>%
  dplyr::filter(TTM >= 0) %>%
  dplyr::filter(TYPE == 'P' ) %>%
  group_by(STRIKE) %>%
  summarise(meanPutValueStrike = mean(PRICE))

# plot: strike vs mean value of the options accros strikes
ggplot(StrikeValuePut, aes(STRIKE, meanPutValueStrike)) + geom_line()

###
cbind(StrikeValuePut, StrikeValueCall)
##

# groupy by TTM

ttmValuePut <- vstoxxOptions %>% 
  filter(TTM >= 0) %>%
  filter(TYPE == 'P' ) %>%
  group_by(TTM) %>%
  summarise(meanPutValueTTM = mean(PRICE))

# plot: strike vs mean value of the options accros strikes
ggplot(ttmValuePut, aes(TTM, meanPutValueTTM)) + geom_line()

################################################################################

# split all option data with regard to date of trading
l1 <- split(vstoxxOptions, vstoxxOptions$DATE)

# count number of maturities for each trading date
l3 <- lapply(l1, function(list){ length(levels(factor(list$MATURITY)))})
l33 <- sapply(l1, function(list){ length(levels(factor(list$MATURITY)))})


function(list){ levels(factor(list$MATURITY))}

levels(factor(l1[[1]]$MATURITY))

summary(l1[[1]])

l2 <- lapply(l1, summary) 
l2[[1]]
  
  








