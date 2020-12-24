
daily_returns <- function(ticker, base_year, glb_env = TRUE) {
  # Obtain stock price data from Yahoo! Finance
  stock <- quantmod::getSymbols(ticker, src = "yahoo", auto.assign = FALSE)
  
  # Remove missing values
  stock <- stats::na.omit(stock)
  
  # Keep only adjusted closing stock prices
  stock <- stock[, 6]
  
  # Confine our observations to begin at the base year and end at the last available trading day
  horizon <- base::paste0(as.character(base_year), "/", as.character(Sys.Date()))
  stock <- stock[horizon]

  # Calculate monthly arithmetic returns
  data <- quantmod::periodReturn(stock, period = "daily", type = "arithmetic")
  
  # If glb_env == TRUE then assign global environment variable, else print the data
  if (glb_env == TRUE) {
    base::assign(paste0(ticker, "_daily"), data, envir = .GlobalEnv)
  } else {
    base::writeLines(ticker)
    base::return(data)
  }

}


monthly_returns <- function(ticker, base_year, glb_env = TRUE) {
  # Obtain stock price data from Yahoo! Finance
  stock <- quantmod::getSymbols(ticker, src = "yahoo", auto.assign = FALSE)
 
   # Remove missing values
  stock <- stats::na.omit(stock)
  
  # Keep only adjusted closing stock prices
  stock <- stock[, 6]

  # Confine our observations to begin at the base year and end at the last available trading day
  horizon <- base::paste0(as.character(base_year), "/", as.character(Sys.Date()))
  stock <- stock[horizon]

  # Calculate monthly arithmetic returns
  data <- quantmod::periodReturn(stock, period = "monthly", type = "arithmetic")
  
  # If glb_env == TRUE then assign global environment variable, else print the data
  if (glb_env == TRUE) {
    base::assign(ticker, data, envir = .GlobalEnv)
  } else {
    base::writeLines(ticker)
    base::return(data)
  }
}

yearly_returns <- function(ticker, base_year, glb_env = TRUE) {
  # Obtain stock price data from Yahoo! Finance
  stock <- quantmod::getSymbols(ticker, src = "yahoo", auto.assign = FALSE)
  
  # Remove missing values
  stock <- stats::na.omit(stock)
  
  # Keep only adjusted closing stock prices
  stock <- stock[, 6]
  
  # Confine our observations to begin at the base year and end at the last available trading day
  horizon <- base::paste0(as.character(base_year), "/", as.character(Sys.Date()))
  stock <- stock[horizon]
  
  # Calculate monthly arithmetic returns
  data <- quantmod::periodReturn(stock, period = "yearly", type = "arithmetic")
  
  # If glb_env == TRUE then assign global environment variable, else print the data
  if (glb_env == TRUE) {
    base::assign(paste0(ticker, "_yearly"), data, envir = .GlobalEnv)
  } else {
    base::writeLines(ticker)
    base::return(data)
  }
}
