library(quantmod)
library(moments)
#pull stock price via quantmod
#compute log returns for specified ticker on yfinance when we pass the ticker name
LogReturn <- function(ticker = "SPY") {
  ticker_data <-getSymbols(ticker, src="yahoo", auto.assign = FALSE)
  ticker_price <- Cl(ticker_data)
  ticker_lr <- diff(log(ticker_price)) |> na.omit()
  
  output <- list(
    ticker = ticker,
    price = ticker_price,
    log_return = ticker_lr
    )
  return(output)
}

#----------------------------------------------------------
#Plot log returns over time = T
#----------------------------------------------------------
Plot_Returns <- function(log_return) {
  
  plot(
    index(log_return),
    as.numeric(log_return),
    type = "l",
    main = "Log Returns",
    xlab = "Time",
    ylab = "Log Return"
  )
   abline(h = 0, col = "lightgrey", lwd = 2)
}

#----------------------------------------------------------
#General summary for log returns
#----------------------------------------------------------
LR_Summary <- function(log_return) {
  r <- as.numeric(log_return)
  output <- data.frame(
    mean = mean(r),
    sd = sd(r),
    skewness = moments::skewness(r),
    kurtosis = moments::kurtosis(r),
    min_max = c(min(r),max(r)),
    T = length(r)
  )
  return(output)
}

