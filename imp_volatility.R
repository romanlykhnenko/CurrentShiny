# load packages
library(dplyr)
library(ggplot2)

# VSTOXX index read from csv file
vstoxxIndex <- read.csv("vstoxx_index.csv")

# Futures on  VSTOXX read from csv file
vstoxxFutures <- read.csv("vstoxx_futures.csv")

# Options on VSTOXX read from csv file
vstoxxOptions <- read.csv("vstoxx_options.csv")

################################################################################
# take Options traded on 31.03.2014
vstoxxOptions3103 <- subset(vstoxxOptions, 
                            strptime(vstoxxOptions$DATE, 
                                     format = "%Y-%m-%d ") == "2014-03-31")

# add time to maturity (in years) as a column
vstoxxOptions3103 = mutate(vstoxxOptions3103, 
                           TTM = as.numeric((strptime(MATURITY, format = "%Y-%m-%d ")
                                             - strptime(DATE, format = "%Y-%m-%d ") ) /360))


# select all distinct maturities
distinct(select(vstoxxOptions3103, MATURITY))

# select all distinct strikes
distinct(select(vstoxxOptions3103, STRIKE))

# select only call options
vstoxxOptions3103call = filter(vstoxxOptions3103, TYPE == 'C')

# select only put options
vstoxxOptions3103put = filter(vstoxxOptions3103, TYPE == 'P')

# VSTOXX value
V0 <- 17.6639 

# short rate
r  <- 0.01

# add column with implied volatilities
vstoxxOptions3103call = mutate(vstoxxOptions3103call,
                               ImpVol = bsCallImpVol(V0, STRIKE, TTM, r, 
                                                        PRICE, 2, 100))

# remove rows with NA
na.omit(vstoxxOptions3103call)

# select options with non-zero maturity
plotData <- filter(vstoxxOptions3103call, ImpVol > 0)

# plot implied volatilities for different maturities
ggplot(plotData, aes(STRIKE, ImpVol, group = MATURITY,
       colour = MATURITY)) + geom_line()





