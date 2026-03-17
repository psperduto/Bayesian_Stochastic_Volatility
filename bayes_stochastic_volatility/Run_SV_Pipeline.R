#SCATCH FROM CHAT DONT KEEP!

#==========================================================
# Run SV Pipeline.R
# Master script up through Bayesian SV estimation
#==========================================================
set.seed(2323)
rm(list = ls())

#----------------------------------------------------------
# Load libraries
#----------------------------------------------------------
library(quantmod)
library(moments)
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
source("05_SV_Diagnostics.R")
source("06_SV_MCMC_Analysis.R")
source("07_Model_Comparison.R")

#----------------------------------------------------------
# Pull data
#----------------------------------------------------------
spy_data <- LogReturn("SPY")

price <- spy_data$price
log_return <- spy_data$log_return

#----------------------------------------------------------
# Fit traditional models
#----------------------------------------------------------
normal_fit <- Fit_Normal(log_return)
t_fit <- Fit_Student_T(log_return)
cauchy_fit <- Fit_Cauchy(log_return)

#----------------------------------------------------------
# Fit GARCH models
#----------------------------------------------------------
garch_norm_fit <- Fit_Garch_Norm(log_return)
garch_t_fit <- Fit_Garch_t(log_return)

#----------------------------------------------------------
# Fit Bayesian SV model
#----------------------------------------------------------
sv_fit <- Run_SV_JAGS(log_return, obs = 1000)

#----------------------------------------------------------
# MCMC diagnostics
#----------------------------------------------------------
sv_summary <- SV_MCMC_Summary(sv_fit)
sv_ess <- SV_MCMC_Effective_Size(sv_fit)
sv_gelman <- SV_MCMC_Gelman(sv_fit)

#----------------------------------------------------------
# SV analysis
#----------------------------------------------------------
h_draws <- SV_h_Draws(sv_fit)
vol_draws <- SV_Volatility_Draws(sv_fit)
vol_path <- SV_Volatility_Path(sv_fit)
vol_bands <- SV_Volatility_Bands(sv_fit)

#----------------------------------------------------------
# Model comparison
#----------------------------------------------------------
sv_garch_error <- SV_vs_GARCH_Error(sv_fit, garch_norm_fit)

#----------------------------------------------------------
# Plots (optional run)
#----------------------------------------------------------
SV_MCMC_TP(sv_fit)
SV_MCMC_ACF(sv_fit)

SV_Posterior_Density_Plots(sv_fit)
SV_Volatility_Plot(sv_fit)
SV_V_AR_Plot(sv_fit)

SV_vs_GARCH(sv_fit, garch_norm_fit)
SV_vs_Returns(sv_fit, garch_norm_fit)

#----------------------------------------------------------
# Store output
#----------------------------------------------------------
SV_Pipeline_Output <- list(
  price = price,
  log_return = log_return,
  normal_fit = normal_fit,
  t_fit = t_fit,
  cauchy_fit = cauchy_fit,
  garch_norm_fit = garch_norm_fit,
  garch_t_fit = garch_t_fit,
  sv_fit = sv_fit,
  sv_summary = sv_summary,
  sv_ess = sv_ess,
  sv_gelman = sv_gelman,
  volatility_path = vol_path,
  volatility_bands = vol_bands,
  sv_garch_error = sv_garch_error
)