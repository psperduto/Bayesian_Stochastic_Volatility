# Lets works from SPY
#We already know log return volatility isnt normal.
mu_hat <- mean(SPY_log_return)
sigma_hat <- sd(SPY_log_return)
print(mu_hat)
print(sigma_hat)
loglik_norm <- sum(dnorm(SPY_log_return,
                         mean = mu_hat,
                         sd = sigma_hat,
                         log = TRUE))
loglik_norm


library(MASS)
fit_t <- fitdistr(SPY_log_return, "t")

fit_t
mu_t <- fit_t$estimate["m"]
sigma_t <- fit_t$estimate["s"]
nu_t <- fit_t$estimate["df"]

loglik_t <- sum(dt((SPY_log_return - mu_t)/sigma_t,
                   df = nu_t,
                   log = TRUE) - log(sigma_t))

fit_cauchy <- fitdistr(SPY_log_return, "cauchy")

mu_c <- fit_cauchy$estimate["location"]
gamma_c <- fit_cauchy$estimate["scale"]

loglik_cauchy <- sum(dcauchy(SPY_log_return,
                             location = mu_c,
                             scale = gamma_c,
                             log = TRUE))

loglik_cauchy
#load package to run a garch analysis
library(rugarch)
garch_spec_norm <- ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0),
                    include.mean = TRUE),
  distribution.model = "norm"
)
#fit the garch model
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