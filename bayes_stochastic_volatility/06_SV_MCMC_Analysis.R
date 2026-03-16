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
