#SCATCH FROM CHAT DONT KEEP!
#ACF plot for one monitored MCMC parameter
#example: param = "phi"
#----------------------------------------------------------
Plot_SV_MCMC_ACF <- function(sv_fit, param, lag.max = 50) {
  acf(
    as.matrix(sv_fit$samples)[, param],
    lag.max = lag.max,
    main = paste("ACF of", param, "MCMC Samples")
  )
}

#----------------------------------------------------------
#ACF plots for all monitored MCMC parameters
#----------------------------------------------------------
Plot_SV_MCMC_ACF_All <- function(sv_fit, lag.max = 50) {
  sample_mat <- as.matrix(sv_fit$samples)
  
  par(mfrow = c(2, 2))
  
  acf(sample_mat[, "mu"], lag.max = lag.max, main = "ACF of mu")
  acf(sample_mat[, "phi"], lag.max = lag.max, main = "ACF of phi")
  acf(sample_mat[, "sigma2_eta"], lag.max = lag.max, main = "ACF of sigma2_eta")
  acf(sample_mat[, "sigma_eta"], lag.max = lag.max, main = "ACF of sigma_eta")
  
  par(mfrow = c(1, 1))
}

#----------------------------------------------------------
#ACF plot for posterior mean latent log-volatility h[t]
#----------------------------------------------------------
Plot_SV_h_ACF <- function(h_post, lag.max = 50) {
  acf(
    h_post,
    lag.max = lag.max,
    main = "ACF of Posterior Mean Log Volatility"
  )
}

#----------------------------------------------------------
#ACF plot for posterior volatility path
#----------------------------------------------------------
Plot_SV_Volatility_ACF <- function(sv_vol, lag.max = 50) {
  acf(
    sv_vol,
    lag.max = lag.max,
    main = "ACF of Posterior Volatility"
  )
}

