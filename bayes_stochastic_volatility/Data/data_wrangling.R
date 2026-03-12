library(quantmod)
# Quantmod is a library that lets us pull data through yahoo finance
getSymbols("SPY", src="yahoo")
SPY_price <- Cl(SPY)
SPY_log_return <- diff(log(SPY_price))
SPY <- na.omit(SPY_log_return)
plot(SPY_log_return)

mu <- mean(SPY_log_return)
sigma <- sd(SPY_log_return)
#plot the log_returns against a normal model
hist(SPY_log_return,
     breaks = 50,
     probability = TRUE,
     xlim = c(-0.1,0.1))
mu <- mean(SPY_log_return)
sigma <- sd(SPY_log_return)
curve(dnorm(x, mean = mu, sd = sigma),
      from = -0.1,
      to = 0.1,
      col = "red",
      lwd = 2,
      add = TRUE)
SPY_log_return <- na.omit(SPY_log_return)
acf(SPY_log_return)
acf(SPY_log_return^2)


qqnorm(SPY_log_return)
qqline(SPY_log_return)

library(moments)
kurtosis(SPY_log_return)
skewness(SPY_log_return)
#very important see that we have an excess kurtosis 17-3 =14.