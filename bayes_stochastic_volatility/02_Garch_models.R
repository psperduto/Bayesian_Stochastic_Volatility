library(MASS)
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
#Fit Normal Garch
#----------------------------------------------------------
Fit_Garch_Norm <- function(log_return) {
  r <- as.numeric(log_return)
  
  Garch_Normal_Specifacations <- ugarchspec(
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
    spec = Garch_Normal_Specifacations,
    data =r
  )
  output <- list(
    model = "GARCH Normal Model",
    spec = Garch_Normal_Specifacations,
    fit = garch_fit_norm,
    log_likelihood = likelihood(garch_fit_norm)
  )
  return(output)
}

# Plot_GARCH_Volatility
# Plot fitted conditional volatility from a GARCH model

Plot_GARCH_Volatility <- function(garch_model, log_return) {
  
  sigma_t <- sigma(garch_model$fit)
  t <- index(log_return)
  
  plot(
    t,
    sigma_t,
    type = "l",
    main = paste(garch_model$model, "Conditional Volatility"),
    xlab = "Time",
    ylab = expression(hat(sigma)[t])
  )
  
}


# garch analysis, AR1 model
garch_fit_norm <- ugarchfit(
  spec = garch_spec_norm,
  data = SPY_log_return
)
garch_fit_norm
#The Garch by default assumes normal spread but we can specify t
garch_spec_t <- ugarchspec(
  variance.model = list(model="sGARCH", garchOrder=c(1,1)),
  mean.model = list(armaOrder=c(0,0), include.mean=TRUE),
  distribution.model = "std"
)

garch_fit_t <- ugarchfit(
  spec = garch_spec_t,
  data = SPY_log_return
)

garch_fit_t

#plot garch model
garch_vol <- sigma(garch_fit_t)

plot(garch_vol,
     main="Estimated Volatility (GARCH-t)",

     col="blue")
