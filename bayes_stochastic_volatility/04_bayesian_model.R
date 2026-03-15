library(rjags)
library(coda)

#----------------------------------------------------------
#prepare and store data for JAGS
#----------------------------------------------------------
Returns_Trim <- function(log_returns, obs) {
  y <- as.numeric(tail(log_returns, obs))
  T <- length(y)
  output <- list(
    y = y,
    T = T
  )
  return(output)
}

#----------------------------------------------------------
# store JAGS data list
#----------------------------------------------------------
SV_Data_list <- function(log_returns, obs) {
  data_list <- Returns_Trim(log_returns, obs)
  return(data_list)
}


#----------------------------------------------------------
#Store the model string for JAGS
#----------------------------------------------------------
sv_model_string <- "
model {
  for (t in 1:T) {
    y[t] ~ dnorm(0, tau_y[t])
    tau_y[t] <- exp(-h[t])
  }
  
  h[1] ~ dnorm(mu, tau_h0)
  tau_h0 <- (1 - phi * phi) * tau_eta

  for (t in 2:T) {
    h[t] ~ dnorm(mu + phi * (h[t-1] - mu), tau_eta)
  }

  mu ~ dnorm(0, 0.01)
  phi ~ dunif(-0.999, 0.999)

  tau_eta ~ dgamma(2.5, 0.025)
  sigma2_eta <- 1 / tau_eta
  sigma_eta <- sqrt(sigma2_eta)
}
"

#----------------------------------------------------------
#initialize starting values
#----------------------------------------------------------
init_fun <- function(y,T) {
  list(
    mu = log(var(y)),
    phi = 0.95,
    tau_eta = 50,
    h = rep(log(var(y)), T)
  )
}

#----------------------------------------------------------
#Fit JAGS model
#----------------------------------------------------------
Fit_SV_JAGS <- function(log_returns, obs) {
  jags_data <- SV_Data_list(log_returns, obs)
  params_basic <- c("mu", "phi", "sigma_eta", "sigma2_eta")
  
  sv_jags <- jags.model(
    textConnection(sv_model_string),
    data = jags_data,
    inits = function() init_fun(jags_data$y, jags_data$T),
    n.chains = 3,
    n.adapt = 1000
  )
  
  update(sv_jags, n.iter = 5000)
  
  sv_samples_basic <- coda.samples(
    model = sv_jags,
    variable.names = params_basic,
    n.iter = 10000,
    thin = 10
  )
  
  output <- list(
    model = sv_jags,
    samples = sv_samples_basic,
    data = jags_data
  )
  
  return(output)
}

#----------------------------------------------------------
#
#----------------------------------------------------------
params_full <- c("mu","phi","sigma_eta","h")
sv_samples_full <- coda.samples(
  sv_jags,
  variable.names=params_full,
  n.iter=5000,
  thin=20
)
sv_mat <- as.matrix(sv_samples_full)
dim(sv_mat)

h_cols <- grep("^h\\[", colnames(sv_mat))
length(h_cols)
head(colnames(sv_mat)[h_cols])
tail(colnames(sv_mat)[h_cols])

h_post_mean <- colMeans(sv_mat[, h_cols])
head(h_post_mean)

sv_vol <- exp(h_post_mean / 2)
head(sv_vol)

plot(sv_vol,
     type = "l",
     col = "red",
     lwd = 1.5,
     main = "Posterior Mean Volatility (SV-JAGS)",
     ylab = "Volatility",
     xlab = "Time")


garch_vol <- rugarch::sigma(garch_fit_t)
garch_vol_use <- tail(as.numeric(garch_vol), length(sv_vol))
plot(garch_vol_use,
     type = "l",
     col = "blue",
     lwd = 1.5,
     main = "GARCH-t vs Bayesian SV Volatility",
     ylab = "Volatility",
     xlab = "Time")

lines(sv_vol, col = "red", lwd = 1.5)

legend("topright",
       legend = c("GARCH-t", "SV-JAGS"),
       col = c("blue", "red"),
       lty = 1,
       lwd = 1.5)

