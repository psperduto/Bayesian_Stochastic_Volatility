library(MASS)
library(rugarch)



# Lets works from SPY
#We already know log return volatility isnt normal.
normalModelLikelihood <- function(logReturns) {
  mu_hat <- mean(logReturns)
  sigma_hat <- sd(logReturns)
  loglik_norm <- sum(dnorm(logReturns,
                           mean = mu_hat,
                           sd = sigma_hat,
                           log = TRUE))
  return(loglik_norm)
}

tModelLikelihood <- function(logReturns) {
  fit_t <- fitdistr(logReturns, "t")
  
  mu_t <- fit_t$estimate["m"]
  sigma_t <- fit_t$estimate["s"]
  nu_t <- fit_t$estimate["df"]
  
  loglik_t <- sum(dt((logReturns - mu_t)/sigma_t,
                     df = nu_t,
                     log = TRUE) - log(sigma_t))
  return(loglik_t)
}

cauchyModelLikelihood <- function(logReturns) {
  fit_cauchy <- fitdistr(logReturns, "cauchy")
  
  mu_c <- fit_cauchy$estimate["location"]
  gamma_c <- fit_cauchy$estimate["scale"]
  
  loglik_cauchy <- sum(dcauchy(logReturns,
                               location = mu_c,
                               scale = gamma_c,
                               log = TRUE))
  return(loglik_cauchy)
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
