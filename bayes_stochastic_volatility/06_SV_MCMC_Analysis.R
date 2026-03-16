#----------------------------------------------------------
# Extract estimated h values from sv_fit
#----------------------------------------------------------
SV_h_Draws <- function(sv_fit) {
  
  sv_draws <- as.matrix(sv_fit$samples)
  h_cols <- startsWith(colnames(sv_draws), "h[")
  h_draws <- sv_draws[, h_cols]
  
  return(h_draws)
}

#----------------------------------------------------------
#calculate volatility at draw y_t from simulated h_t
#----------------------------------------------------------
SV_Volatility_Draws <- function(sv_fit) {
  h_draws <- SV_h_Draws(sv_fit)
  volatility_draws <- exp(h_draws / 2)
  return(volatility_draws)
}

#----------------------------------------------------------
#Volatility path from the posteriors
#----------------------------------------------------------
SV_Volatility_Path <- function(sv_fit) {
  volatility_draws <- SV_Volatility_Draws(sv_fit)
  volatility_path <- colMeans(volatility_draws)
  return(volatility_path)
}

#----------------------------------------------------------
#Credible intervals
#----------------------------------------------------------
SV_Volatility_Bands <- function(sv_fit) {
  volatility_draws <- SV_Volatility_Draws(sv_fit)
  
  vol_mean  <- apply(volatility_draws, 2, mean)
  vol_lower <- apply(volatility_draws, 2, quantile, probs = 0.025)
  vol_upper <- apply(volatility_draws, 2, quantile, probs = 0.975)
  output <- list(
    mean = vol_mean,
    lower = vol_lower,
    upper = vol_upper
  )
  return(output)
}

#----------------------------------------------------------
#plot posterior volatility paths
#----------------------------------------------------------
SV_Volatility_Plot <- function(sv_fit) {
  volatility_path <- SV_Volatility_Path(sv_fit)
  plot(
    volatility_path,
    type = "l",
    main = "Estimated Volatility",
    ylab = "Volatility"
  )
}

#----------------------------------------------------------
#Plot Volatility vs abs(returns)
#----------------------------------------------------------
SV_V_AR_Plot <- function(sv_fit) {
  returns <- as.numeric(sv_fit$data$y)
  volatility_path <- SV_Volatility_Path(sv_fit)
  
  plot(
    abs(returns),
    type = "l",
    main = "SV MCMC vs Absolute Returns",
    ylab = "magnitude",
    col = "darkgrey"
  )
  lines(volatility_path, col = 'darkred', lwd =2)
}

#----------------------------------------------------------
#Parameter posterior density plots
#----------------------------------------------------------
SV_Posterior_Density_Plots <- function(sv_fit) {
  
  sv_mcmc <- sv_fit$samples
  
  densplot(sv_mcmc[, "mu"],
           main = "Posterior Distribution of mu")
  densplot(sv_mcmc[, "phi"],
           main = "Posterior Distribution of Volatility Persistence")
  densplot(sv_mcmc[, "sigma_eta"],
           main = "Posterior Distribution of Volatility Innovation")
  densplot(sv_mcmc[, "sigma2_eta"],
           main = "Posterior Distribution of sigma2_eta")
}

