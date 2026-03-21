
library(quantmod)
library(rugarch)
library(rjags)
library(coda)

source("01_Data_Wrangling.R")
source("03_Garch_Models.R")
source("04_Bayesian_Model.R")
source("08_Forecasting.R")

spy_data <- LogReturn("SPY")
log_return <- spy_data$log_return

forecast_results <- Run_Rolling_Forecast_Comparison(
  log_return = log_return,
  test_size = 200,
  window_size = 500
)

forecast_results$error_metrics

Rolling_Forecast_Comparison_Plot(
  test_returns = forecast_results$test_returns,
  sv_roll = forecast_results$sv_roll,
  garch_roll = forecast_results$garch_roll
)