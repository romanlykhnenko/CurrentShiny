


dataPreparation <- function(){ 
  
  # VSTOXX index read from csv file
  vstoxxIndex <- read.csv("Data/vstoxx_index.csv")
  
  # Futures on  VSTOXX read from csv file
  #vstoxxFutures <- read.csv("Data/vstoxx_futures.csv")
  
  # Options on VSTOXX read from csv file
  vstoxxOptions <- read.csv("Data/vstoxx_options.csv")
  
  # coerce to Date format
  vstoxxOptions$DATE <- as.Date(vstoxxOptions$DATE, "%Y-%m-%d ")
  
  # coerce to Date format
  vstoxxOptions$MATURITY <- as.Date(vstoxxOptions$MATURITY, "%Y-%m-%d ") 
  
  out <- list(options = vstoxxOptions, index = vstoxxIndex)
  return(out)

# # add time to maturity (in years) as a column
# vstoxxOptions <- mutate(vstoxxOptions, 
#                        TTM = as.numeric((as.Date(MATURITY, format = "%Y-%m-%d ")
#                                          - as.Date(DATE, format = "%Y-%m-%d ") ) /360))

# mistakes in data, since TTM can not be negative
# dplyr::filter(vstoxxOptions, TTM >= 0)
}

