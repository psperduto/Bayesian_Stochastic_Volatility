#----------------------------------------------------------
# 08_Forecasting.R
# Rolling one-step-ahead forecast comparison:
# Bayesian Stochastic Volatility vs Normal GARCH
#----------------------------------------------------------

#----------------------------------------------------------
# Load required source files
#----------------------------------------------------------
source("03_Garch_Models.R")
source("04_Bayesian_Model.R")

library(rugarch)

#----------------------------------------------------------
# Extract posterior draws from MCMC simulation
#----------------------------------------------------------
SV_Extract_Posterior_Draws <- function(sv_fit) {
  draws_matrix <- as.matrix(sv_fit$samples)
  
  output <- list(
    draws_matrix = draws_matrix,
    parameter_names = colnames(draws_matrix)
  )
  
  return(output)
}

#----------------------------------------------------------
# Extract last latent state draws from posterior simulation
#----------------------------------------------------------
SV_Last_Latent_Draws <- function(sv_fit) {
  posterior_draws <- SV_Extract_Posterior_Draws(sv_fit)
  draws_matrix <- posterior_draws$draws_matrix
  parameter_names <- posterior_draws$parameter_names
  
  h_names <- parameter_names[grepl("^h\\[", parameter_names)]
  last_h_name <- tail(h_names, 1)
  last_h_draws <- draws_matrix[, last_h_name]
  
  output <- list(
    last_h_draws = last_h_draws,
    last_h_name = last_h_name
  )
  
  return(output)
}

#----------------------------------------------------------
# Extract scalar parameter draws from posterior simulation
#----------------------------------------------------------
SV_Parameter_Draws <- function(sv_fit) {
  posterior_draws <- SV_Extract_Posterior_Draws(sv_fit)
  draws_matrix <- posterior_draws$draws_matrix
  
  mu_draws <- draws_matrix[, "mu"]
  phi_draws <- draws_matrix[, "phi"]
  sigma_eta_draws <- draws_matrix[, "sigma_eta"]
  
  output <- list(
    mu_draws = mu_draws,
    phi_draws = phi_draws,
    sigma_eta_draws = sigma_eta_draws
  )
  
  return(output)
}

#----------------------------------------------------------
# One-step-ahead SV forecast from current training sample
#----------------------------------------------------------
SV_One_Step_Forecast <- function(train_returns) {
  obs <- length(train_returns)
  sv_fit <- Run_SV_JAGS_Fast(log_returns = train_returns, obs = obs)
  
  last_h <- SV_Last_Latent_Draws(sv_fit)
  params <- SV_Parameter_Draws(sv_fit)
  
  last_h_draws <- last_h$last_h_draws
  mu_draws <- params$mu_draws
  phi_draws <- params$phi_draws
  sigma_eta_draws <- params$sigma_eta_draws
  
  n_draws <- length(last_h_draws)
  
  h_next_draws <- numeric(n_draws)
  vol_next_draws <- numeric(n_draws)
  
  for (i in 1:n_draws) {
    h_next_draws[i] <- mu_draws[i] +
      phi_draws[i] * (last_h_draws[i] - mu_draws[i]) +
      rnorm(1, mean = 0, sd = sigma_eta_draws[i])
    
    vol_next_draws[i] <- exp(h_next_draws[i] / 2)
  }
  
  output <- list(
    mean_forecast = mean(vol_next_draws),
    median_forecast = median(vol_next_draws),
    lower_forecast = as.numeric(quantile(vol_next_draws, probs = 0.025)),
    upper_forecast = as.numeric(quantile(vol_next_draws, probs = 0.975)),
    vol_next_draws = vol_next_draws,
    sv_fit = sv_fit
  )
  
  return(output)
}

#----------------------------------------------------------
# One-step-ahead GARCH forecast from current training sample
#----------------------------------------------------------
GARCH_One_Step_Forecast <- function(train_returns) {
  garch_norm_fit <- Fit_Garch_Norm(train_returns)
  garch_forecast <- ugarchforecast(garch_norm_fit$fit, n.ahead = 1)
  
  garch_vol_forecast <- as.numeric(sigma(garch_forecast))[1]
  
  output <- list(
    garch_vol_forecast = garch_vol_forecast,
    garch_norm_fit = garch_norm_fit
  )
  
  return(output)
}

#----------------------------------------------------------
# Rolling one-step-ahead SV forecasts
#----------------------------------------------------------
SV_Rolling_One_Step_Forecast <- function(log_return, test_size = 200, window_size = 500) {
  T <- length(log_return)
  train_end <- T - test_size
  
  mean_forecast <- numeric(test_size)
  median_forecast <- numeric(test_size)
  lower_forecast <- numeric(test_size)
  upper_forecast <- numeric(test_size)
  
  for (i in 1:test_size) {
    current_end <- train_end + i - 1
    current_start <- max(1, current_end - window_size + 1)
    current_train <- log_return[current_start:current_end]
    
    sv_one_step <- SV_One_Step_Forecast(current_train)
    
    mean_forecast[i] <- sv_one_step$mean_forecast
    median_forecast[i] <- sv_one_step$median_forecast
    lower_forecast[i] <- sv_one_step$lower_forecast
    upper_forecast[i] <- sv_one_step$upper_forecast
    
    cat("SV rolling forecast step", i, "of", test_size, "\n")
  }
  
  output <- list(
    mean_forecast = mean_forecast,
    median_forecast = median_forecast,
    lower_forecast = lower_forecast,
    upper_forecast = upper_forecast,
    test_size = test_size,
    window_size = window_size
  )
  
  return(output)
}


#----------------------------------------------------------
# Rolling one-step-ahead GARCH forecasts with moving window
#----------------------------------------------------------
GARCH_Rolling_One_Step_Forecast <- function(log_return, test_size = 200, window_size = 500) {
  T <- length(log_return)
  train_end <- T - test_size
  
  garch_vol_forecast <- numeric(test_size)
  
  for (i in 1:test_size) {
    current_end <- train_end + i - 1
    current_start <- max(1, current_end - window_size + 1)
    current_train <- log_return[current_start:current_end]
    
    garch_one_step <- GARCH_One_Step_Forecast(current_train)
    
    garch_vol_forecast[i] <- garch_one_step$garch_vol_forecast
    
    cat("GARCH rolling forecast step", i, "of", test_size, "\n")
  }
  
  output <- list(
    garch_vol_forecast = garch_vol_forecast,
    test_size = test_size,
    window_size = window_size
  )
  
  return(output)
}

#----------------------------------------------------------
# Compute rolling forecast error metrics
#----------------------------------------------------------
Rolling_Forecast_Error_Metrics <- function(test_returns, sv_roll, garch_roll) {
  test_proxy <- abs(test_returns)
  
  sv_errors <- test_proxy - sv_roll$mean_forecast
  garch_errors <- test_proxy - garch_roll$garch_vol_forecast
  
  sv_mse <- mean(sv_errors^2)
  garch_mse <- mean(garch_errors^2)
  
  sv_mae <- mean(abs(sv_errors))
  garch_mae <- mean(abs(garch_errors))
  
  output <- list(
    test_proxy = test_proxy,
    sv_mse = sv_mse,
    garch_mse = garch_mse,
    sv_mae = sv_mae,
    garch_mae = garch_mae
  )
  
  return(output)
}

#----------------------------------------------------------
# Plot rolling forecast comparison
#----------------------------------------------------------
Rolling_Forecast_Comparison_Plot <- function(test_returns, sv_roll, garch_roll) {
  test_proxy <- abs(test_returns)
  forecast_index <- 1:length(test_proxy)
  
  plot(
    forecast_index, test_proxy,
    type = "l",
    lwd = 1.5,
    xlab = "Forecast Step",
    ylab = "Volatility",
    main = "Rolling One-Step-Ahead Forecast Comparison"
  )
  
  lines(forecast_index, sv_roll$mean_forecast, lwd = 2, col = "blue")
  lines(forecast_index, sv_roll$lower_forecast, lty = 2, col = "blue")
  lines(forecast_index, sv_roll$upper_forecast, lty = 2, col = "blue")
  lines(forecast_index, garch_roll$garch_vol_forecast, lwd = 2, col = "red")
  
  legend(
    "topright",
    legend = c("Proxy |r_t|", "SV Mean Forecast", "SV 95% Bands", "GARCH Forecast"),
    col = c("black", "blue", "blue", "red"),
    lty = c(1, 1, 2, 1),
    lwd = c(1.5, 2, 1, 2),
    bty = "n"
  )
}

#----------------------------------------------------------
# Master rolling forecast pipeline
#----------------------------------------------------------
Run_Rolling_Forecast_Comparison <- function(log_return, test_size = 200, window_size = 500) {
  T <- length(log_return)
  
  test_returns <- log_return[(T - test_size + 1):T]
  
  sv_roll <- SV_Rolling_One_Step_Forecast(
    log_return = log_return,
    test_size = test_size,
    window_size = window_size
  )
  
  garch_roll <- GARCH_Rolling_One_Step_Forecast(
    log_return = log_return,
    test_size = test_size,
    window_size = window_size
  )
  
  error_metrics <- Rolling_Forecast_Error_Metrics(
    test_returns = test_returns,
    sv_roll = sv_roll,
    garch_roll = garch_roll
  )
  
  output <- list(
    test_returns = test_returns,
    sv_roll = sv_roll,
    garch_roll = garch_roll,
    error_metrics = error_metrics,
    test_size = test_size,
    window_size = window_size
  )
  
  return(output)
}