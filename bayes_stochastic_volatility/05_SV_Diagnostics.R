library(coda)


#----------------------------------------------------------
#store mcmc object
#----------------------------------------------------------
SV_MCMC_Object <- function(sv_fit) {
  sv_mcmc <- sv_fit$samples
  return(sv_mcmc)
}


#----------------------------------------------------------
#SV MCMC Summary info
#----------------------------------------------------------
SV_MCMC_Summary <- function(sv_fit) {
  sv_mcmc <- SV_MCMC_Object(sv_fit)
  summary_MCMC <- summary(sv_mcmc)
  return(summary_MCMC)
}

#----------------------------------------------------------
#Effective sample sizes
#----------------------------------------------------------
SV_Effective_Size <- function(sv_fit) {
  sv_mcmc <- SV_MCMC_Object(sv_fit)
  effective_ss <- effectiveSize(sv_mcmc)
  return(effective_ss)
}

#----------------------------------------------------------
#From Gelman and Rubin (1992), to check for convergence
#----------------------------------------------------------
SV_Gelman <- function(sv_fit) {
  sv_mcmc <- SV_MCMC_Object(sv_fit)
  SV_MCMC_Gelman <- gelman.diag(sv_mcmc)
  return(SV_MCMC_Gelman)
}

#----------------------------------------------------------
#Sv MCMC trace plots
#----------------------------------------------------------
SV_MCMC_TP <- function(sv_fit) {
    sv_mcmc <- SV_MCMC_Object(sv_fit)
    traceplot(sv_mcmc[, c("mu","phi","sigma_eta","sigma2_eta")])
}

#----------------------------------------------------------
#SV MCMC ACF Plots
#----------------------------------------------------------
SV_MCMC_ACF <- function(sv_fit) {
  sv_mcmc <- SV_MCMC_Object(sv_fit)
  autocorr.plot(sv_mcmc[,c("mu","phi","sigma_eta","sigma2_eta")])
}
