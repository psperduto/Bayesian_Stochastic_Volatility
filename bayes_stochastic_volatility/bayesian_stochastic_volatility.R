library(rjags)
#shorten the data to a more manageable amount
y <- SPY_log_return[1:2000]
y <- as.numeric(na.omit(y))
T <- length(y)

jags_data <- list(
  y = y, 
  T = T
)

sv_model_string <- "
model {

  # Observation equation
  for (t in 1:T) {
    y[t] ~ dnorm(0, tau_y[t])
    tau_y[t] <- exp(-h[t])
  }

  # Initial state
  h[1] ~ dnorm(mu, tau_h0)
  tau_h0 <- (1 - phi * phi) * tau_eta

  # State equation
  for (t in 2:T) {
    h[t] ~ dnorm(mu + phi * (h[t-1] - mu), tau_eta)
  }

  # Priors
  mu ~ dnorm(0, 0.01)
  phi ~ dunif(-0.999, 0.999)

  tau_eta ~ dgamma(2.5, 0.025)
  sigma2_eta <- 1 / tau_eta
  sigma_eta <- sqrt(sigma2_eta)
}
"
init_fun <- function() {
  list(
    mu = log(var(y)),
    phi = 0.95,
    tau_eta = 50,
    h = rep(log(var(y)), T)
  )
}
params_basic <- c("mu", "phi", "sigma_eta", "sigma2_eta")
sv_jags <- jags.model(
  textConnection(sv_model_string),
  data = jags_data,
  inits = init_fun,
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

summary(sv_samples_basic)

plot(sv_samples_basic)
gelman.diag(sv_samples_basic)
effectiveSize(sv_samples_basic)

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

