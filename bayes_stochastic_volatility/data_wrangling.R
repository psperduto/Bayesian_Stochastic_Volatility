library(quantmod)
library(moments)
# Quantmod is a library that lets us pull data through yahoo finance

getLogReturn <- function(stockName = "SPY", diagonisticInformation = FALSE) {
  
  getSymbols(stockName, src="yahoo")
  STOCK_price <- Cl(WTI)
  STOCK_log_return <- diff(log(SPY_price)) |> na.omit()
  
  output <- list(Price = STOCK_price, 
                 LogReturn = STOCK_log_return)
  
  if (diagonisticInformation == TRUE) {
    plot(STOCK_log_return)
    
    #plot the log_returns against a normal model
    hist(STOCK_log_return,
         breaks = 50,
         probability = TRUE,
         xlim = c(-0.1,0.1),
         main = paste("Histogram of log returns of",stockName))
    
    mu <- mean(STOCK_log_return)
    sigma <- sd(STOCK_log_return)
    
    curve(dnorm(x, mean = mu, sd = sigma),
          from = -0.1,
          to = 0.1,
          col = "red",
          lwd = 2,
          add = TRUE)
    
    acf(STOCK_log_return)
    acf(STOCK_log_return^2)
    
    
    qqnorm(STOCK_log_return)
    qqline(STOCK_log_return)
    
    
    kurtosis(STOCK_log_return)
    skewness(STOCK_log_return)    
  }
  
  return(output)
}

