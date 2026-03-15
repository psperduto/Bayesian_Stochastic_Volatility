#SCATCH FROM CHAT DONT KEEP!

#==========================================================
# Run SV Pipeline.R
# Master script up through Bayesian SV estimation
#==========================================================

rm(list = ls())

#----------------------------------------------------------
# Load required libraries
#----------------------------------------------------------
library(quantmod)
library(moments)
library(MASS)
library(rugarch)
library(rjags)
library(coda)

#----------------------------------------------------------
# Source project files
#----------------------------------------------------------
source("01_Data_Wrangling.R")
source("02_Traditional_Models.R")
source("03_Garch_Models.R")
source("04_Bayesian_Model.R")

#----------------------------------------------------------
# Set project inputs
#----------------------------------------------------------
ticker <- "SPY"
obs <- 1000

#----------------------------------------------------------
# Step 1: Pull data and compute log returns
#----------------------------------------------------------
data_output <- LogReturn(ticker = ticker)

price <- data_output$price
log_return <- data_output$log_return

cat("Data loaded successfully.\n")
cat("Ticker:", ticker, "\n")
cat("Number of returns:", length(log_return), "\n\n")

#----------------------------------------------------------
# Step 2: Fit traditional distribution models
#----------------------------------------------------------
normal_fit <- Fit_Normal(log_return = log_return)
cauchy_fit <- Fit_Cauchy(log_return = log_return)
t_fit <- Fit_Student_T(log_return = log_return)

traditional_fits <- list(
  Normal = normal_fit,
  Cauchy = cauchy_fit,
  T_Distribution = t_fit
)

traditional_summary <- Distribution_Summary(traditional_fits)

cat("Traditional models fitted.\n\n")

#----------------------------------------------------------
# Step 3: Fit GARCH models
#----------------------------------------------------------
garch_norm_fit <- Fit_Garch_Norm(log_return = log_return)
garch_t_fit <- Fit_Garch_t(log_return = log_return)

cat("GARCH models fitted.\n\n")

#----------------------------------------------------------
# Step 4: Fit Bayesian SV model
#----------------------------------------------------------
sv_fit <- Run_SV_JAGS(
  log_returns = log_return,
  obs = obs
)

sv_jags <- sv_fit$model
sv_samples_basic <- sv_fit$samples
sv_jags_data <- sv_fit$data

cat("Bayesian SV model fitted.\n\n")

#----------------------------------------------------------
# Step 5: Posterior summary
#----------------------------------------------------------
sv_summary <- summary(sv_samples_basic)

cat("Posterior summary created.\n\n")

#----------------------------------------------------------
# Step 6: Collect outputs
#----------------------------------------------------------
sv_pipeline_output <- list(
  inputs = list(
    ticker = ticker,
    obs = obs
  ),
  
  data = list(
    price = price,
    log_return = log_return
  ),
  
  traditional_models = list(
    fits = traditional_fits,
    summary = traditional_summary
  ),
  
  garch_models = list(
    garch_normal = garch_norm_fit,
    garch_t = garch_t_fit
  ),
  
  sv_model = list(
    model = sv_jags,
    samples = sv_samples_basic,
    summary = sv_summary,
    data = sv_jags_data
  )
)

cat("SV pipeline completed successfully through Bayesian estimation.\n")