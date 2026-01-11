library(quantmod)
library(xts)

fetch_data <- function(assets, start_date, end_date) {
  # Create an environment to store data
  data_env <- new.env()
  
  # Download data
  getSymbols(assets, src = "yahoo", from = start_date, to = end_date, env = data_env)
  
  # Extract Adjusted Close prices and merge into one xts object
  prices_list <- lapply(assets, function(sym) {
    # Handle case where download failed or data is missing
    if (exists(sym, envir = data_env)) {
      Ad(get(sym, envir = data_env))
    } else {
      warning(paste("Could not fetch data for", sym))
      NULL
    }
  })
  
  # Merge and remove NULLs
  prices <- do.call(merge, prices_list[!sapply(prices_list, is.null)])
  colnames(prices) <- assets
  
  # Calculate daily returns
  returns <- na.omit(Return.calculate(prices, method = "discrete"))
  
  return(returns)
}
