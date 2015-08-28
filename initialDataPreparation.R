library(dplyr)

vstoxxOptions$DATE <- as.Date(vstoxxOptions$DATE, "%Y-%m-%d ")

vstoxxOptions$MATURITY <- as.Date(vstoxxOptions$MATURITY, "%Y-%m-%d ")

# add time to maturity (in years) as a column
vstoxxOptions <- mutate(vstoxxOptions, 
                       TTM = as.numeric((as.Date(MATURITY, format = "%Y-%m-%d ")
                                         - as.Date(DATE, format = "%Y-%m-%d ") ) /360))

# mistakes in data, since TTM can not be negative
# dplyr::filter(vstoxxOptions, TTM >= 0)

