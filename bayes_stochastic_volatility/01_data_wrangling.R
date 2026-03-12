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
#Fit normal model to log returns

fit_normal <- function(log_return) {
  log_return <- as.numeric(log_return)
  T <- length(log_return)
  
  mu_hat <- mean(log_return)
  sigma_hat <- sd(log_return)

  log_likelihood <- sum(dnorm(log_return, mean = mu_hat,sd = sigma_hat, log = TRUE))
  
  output <- list(
    model = "Normal",
    mu = mu_hat,
    sigma = sigma_hat,
    degree_of_freedom = NA,
    log_likelihood = log_likelihood,
    T = T
  )
  return(output)
}

#!! the cauchy does not have finite moments so we must aproximate this distriubtion's maximum likelihood
fit_cauchy <- function(log_return) {
  log_return <- as.numeric(log_return)
  
  T <- length(log_return)
  location_hat <- median(log_return)
  scale_hat <- IQR(log_return) / 2
  log_likelihood <- sum(dcauchy(log_return, location = location_hat, scale = scale_hat, log = TRUE))
  output <- list(
    model = "Cauchy Aproxx",
    mu = location_hat,
    sigma = scale_hat,
    degree_of_freedom = NA,
    log_likelihood = log_likelihood,
    T=T
    )
  return(output)
}

#fit the t-dsit to log_returns
fit_t <- function(log_return) {
  log_return <- as.numeric(log_return)
  
  T <- length(log_return)
  mu_hat <- mean(log_return)
  sigma_hat <- sd(log_return)
  kurtosis_hat <- kurtosis(log_return)
  degree_of_freedom_hat <- 4 + 6/(kurtosis_hat - 3)
   
  log_likelihood <- sum(
     dt((log_return - mu_hat) / sigma_hat, 
        df = degree_of_freedom_hat,
        log = TRUE) - log(sigma_hat))
  
   output <- list(
     model = "T-Distribution",
     mu = mu_hat,
     sigma = sigma_hat,
     degree_of_freedom = degree_of_freedom_hat,
     log_likelihood = log_likelihood,
     T = T
   )
   return(output)
}
#collect all fitted distributions collectively for single log_return variable
Fit_Distributions <- function(log_return) {
  list(
    Normal = fit_normal(log_return),
    Cauchy = fit_cauchy(log_return),
    T_Distribution = fit_t(log_return)
  )
}

Distribution_Summary <- function(fit_models) {
  
  output <- data.frame(
    model = c(
      fit_models$Normal$model,
      fit_models$Cauchy$model,
      fit_models$T_Distribution$model
    ),
    mu = c(
      fit_models$Normal$mu,
      fit_models$Cauchy$mu,
      fit_models$T_Distribution$mu
    ),
    sigma = c(
      fit_models$Normal$sigma,
      fit_models$Cauchy$sigma,
      fit_models$T_Distribution$sigma
    ),
    log_likelihood = c(
      fit_models$Normal$log_likelihood,
      fit_models$Cauchy$log_likelihood,
      fit_models$T_Distribution$log_likelihood
    )
  )
  return(output)
}
Plot_Models <- function(log_return, fit_models) {
  log_return <- as.numeric(log_return)
  xlim <- as.numeric(quantile(log_return, c(0.005,0.995)))
  h <- hist(log_return, breaks = 60, probability = TRUE, plot = FALSE)
  ylim <- c(0, max(h$density) * 1.5)
  
  hist(
    log_return,
    breaks = 60, 
    probability = TRUE,
    xlim = xlim,
    ylim = ylim,
    main = "Fitted Log Returns",
    xlab = "Log Returns"
  )
  curve(
    dnorm(
      x,
      mean = fit_models$Normal$mu,
      sd = fit_models$Normal$sigma),
    from = xlim[1],
    to = xlim[2],
    col = "blue",
    lwd = 2,
    add = TRUE
    )
  curve(
    dcauchy(
      x,
      location = fit_models$Cauchy$mu,
      scale = fit_models$Cauchy$sigma),
    from = xlim[1],
    to = xlim[2],
    col = "green",
    lwd = 2,
    add = TRUE
  )
  curve(
    dt(
      (x - fit_models$T_Distribution$mu)/ fit_models$T_Distribution$sigma,
      df = fit_models$T_Distribution$degree_of_freedom) / fit_models$T_Distribution$sigma,
    from = xlim[1],
    to = xlim[2],
    col = "red",
    lwd = 2,
    add = TRUE
    )
  legend(
    "topright",
    c("Normal", "Cauchy", "T-Distribution"),
    col = c("blue", "green" ,"red"),
    lwd = 2,
    bty = "n"
  )
}

#Function for QQ plots of log returns
QQplot_returns <- function(log_return) {
  log_return <- as.numeric(log_return)
  
  qqnorm(
    log_return,
    main = " Q-Q Log Returns",
    pch = 20,
    cex = 0.5
  )
  qqline(log_return, lwd =2, col = "blue")
}
