library(rugarch)

#----------------------------------------------------------
#!!Auto regressive plots to check assumptions for AR(1) & Garch
#----------------------------------------------------------
ACF_Diagnostics <- function(log_return) {
  
  r <- as.numeric(log_return)
  par(mfrow = c(1,2))
  acf(r,main = "ACF of Log Returns")
  acf(r^2, main = "ACF of squared Log Returns")
  par(mfrow = c(1,1))
  
}

#----------------------------------------------------------
#plot absolute value of log returns
#----------------------------------------------------------
Plot_Absolute_Returns <- function(log_return) {
  par(mfrow = c(1,2))
  plot(
    index(log_return),
    abs(as.numeric(log_return)),
    type = "l",
    main = "Absolute Log Returns",
    xlab = "T",
    ylab = "abs(Log Return)"
  )
  plot(
    index(log_return^2),
    abs(as.numeric(log_return^2)),
    type = "l",
    main = "Squared Log Returns",
    xlab = "T",
    ylab = "Squared Log Return"
  )
  par(mfrow = c(1,1))
}


#Fit Normal Garch:----------------------------------------------------------
#setting the type of autoregressive model to standard symetric
#garchOrder = c(1,1) is defined for finance situations as in Jacquier et al.
#armaOrder c(0,0) specifie sthat our means independent of time/index, ie mean is constant
#----------------------------------------------------------
Fit_Garch_Norm <- function(log_return) {
  r <- as.numeric(log_return)
  
  specifacations <- ugarchspec(
    variance.model = list(
      model = "sGARCH",
      garchOrder = c(1,1)
    ),
    mean.model = list(
      armaOrder = c(0,0),
      include.mean = FALSE
    ),
    distribution.model = "norm"
  )
  
  garch_fit_norm <- ugarchfit(
    spec = specifacations,
    data =r
  )
  output <- list(
    model = "GARCH Normal Model",
    spec = specifacations,
    fit = garch_fit_norm,
    log_likelihood = likelihood(garch_fit_norm),
    n_par = length(coef(garch_fit_norm)),
    sigma_t = sigma(garch_fit_norm),
    residuals = residuals(garch_fit_norm, standardize = TRUE)
    )
  return(output)
}

#----------------------------------------------------------
# Plot_GARCH_Volatility
# Plot fitted conditional volatility from a GARCH model
#----------------------------------------------------------
Plot_GARCH_Volatility <- function(garch_model, log_return) {
  
  sigma_t <- sigma(garch_model$fit)
  t <- index(log_return)[1:length(sigma_t)]
  
  plot(
    t,
    sigma_t,
    type = "l",
    main = paste(garch_model$model, "Conditional Volatility"),
    xlab = "Time",
    ylab = expression(hat(sigma)[t])
  )
  
}

#----------------------------------------------------------
#plot standardized residuals
#----------------------------------------------------------
Plot_GARCH_Residuals <- function(garch_model, log_return) {
  z_t <- garch_model$residuals
  t <- index(log_return)[1:length(z_t)]
  
  plot(
    t,
    z_t,
    type = "l",
    main = paste(garch_model$model, "Standardized Residuals"),
    xlab = "Time",
    ylab = "Residual"
  )
}
