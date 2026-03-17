SV_vs_GARCH <- function(sv_fit, garch_fit) {
  sv_vol <- SV_Volatility_Path(sv_fit)
  garch_vol <- tail(as.numeric(sigma(garch_fit$fit)), length(sv_vol))
  
  plot(sv_vol, type="l", col="red", lwd=2,
       main="SV vs GARCH Volatility", ylab="Volatility")
  lines(garch_vol, col="blue", lwd=2)
  
  legend("topright",
         legend=c("SV","GARCH"),
         col=c("red","blue"),
         lwd=2)
}




SV_vs_Returns <- function(sv_fit, garch_fit) {
  returns <- as.numeric(sv_fit$data$y)
  sv_vol <- SV_Volatility_Path(sv_fit)
  garch_vol <- tail(as.numeric(sigma(garch_fit$fit)), length(sv_vol))
  
  plot(abs(returns), type="l", col="grey",
       main="Volatility vs Returns")
  lines(sv_vol, col="red", lwd=2)
  lines(garch_vol, col="blue", lwd=2)
}



SV_vs_GARCH_Error <- function(sv_fit, garch_fit) {
  returns <- as.numeric(sv_fit$data$y)
  sv_vol <- SV_Volatility_Path(sv_fit)
  garch_vol <- tail(as.numeric(sigma(garch_fit$fit)), length(sv_vol))
  
  realized <- abs(returns)
  
  c(
    SV_MSE = mean((realized - sv_vol)^2),
    GARCH_MSE = mean((realized - garch_vol)^2)
  )
}