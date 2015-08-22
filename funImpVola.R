
impVola <- function(dateOption, vstoxxOptions){
  
  # dateOption must be in date format
  
  # select options traded only on the date provided by user
  vstoxxOptions3103 <- dplyr::filter(vstoxxOptions, 
                              as.Date(strptime(vstoxxOptions$DATE, 
                                       format = "%Y-%m-%d ")) == dateOption)
  
  # add time to maturity (in years) as a column
  vstoxxOptions3103 = mutate(vstoxxOptions3103, 
                             TTM = as.numeric((strptime(MATURITY, format = "%Y-%m-%d ")
                                               - strptime(DATE, format = "%Y-%m-%d ") ) /360))
  
  
  # select all distinct maturities
  # distinct(select(vstoxxOptions3103, MATURITY))
  
  # select all distinct strikes
  # distinct(select(vstoxxOptions3103, STRIKE))
  
  # select only call options
  vstoxxOptions3103call <- vstoxxOptions3103[vstoxxOptions3103$TYPE =="C", ]
  
  # select only put options
  # vstoxxOptions3103put = filter(vstoxxOptions3103, TYPE == 'P')
  
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
  plotData <- vstoxxOptions3103call[vstoxxOptions3103call$ImpVol > 0, ]
    
  return(plotData)
  
}

