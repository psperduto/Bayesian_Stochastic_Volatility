# Stochastic Volatility Modeling: Bayesian Framework

## Motivation

Financial modeling often involves estimating quantities that are not directly observed in return data. One of the main ones is volatility. It plays an important role in risk management, pricing, and trading decisions.

Return data usually shows volatility clustering and persistence. Periods of high volatility tend to follow each other, and the same is true for low volatility. This means volatility is not constant and changes over time.

Since volatility is not directly observed, it makes sense to model it as a latent process and estimate it from the data. This leads to stochastic volatility models. Bayesian methods provide a way to estimate this process and account for uncertainty.

## Traditional methodology

In practice, the Generalized Autoregressive Conditional Heteroskedasticity (GARCH) framework is widely used as a simple and effective way to estimate time-varying volatility.

The GARCH model treats volatility as a deterministic function of past returns and past volatility. At each time \( t \), volatility is computed using previous observations.

An alternative is to treat volatility as a latent stochastic process that evolves over time rather than a deterministic quantity.

## General approach

Following the hierarchical model proposed in "Bayesian Analysis of Stochastic Volatility Models" (1994) by Jacquier et al., we implement a stochastic volatility model where the latent log-volatility follows an AR(1) process.

We estimate the model using Markov Chain Monte Carlo (MCMC) methods, including Metropolis-based sampling, to obtain posterior distributions for both the model parameters and the latent volatility process.

Using these estimates, we construct a rolling forecasting framework to predict next-day volatility over a 200-day window. These forecasts are compared against GARCH-based models to evaluate relative performance.

## The pipeline

We begin by retrieving financial data using the `quantmod` and `yfinance` libraries and computing log returns for a chosen asset.

The return series exhibits near-zero mean with little autocorrelation, while volatility remains highly persistent. We first fit standard GARCH and t-GARCH models to serve as benchmarks.

We then fit the Bayesian stochastic volatility model using JAGS, sampling from the posterior distribution of both the AR(1) parameters and the latent volatility \( h_t \).

Finally, we generate out-of-sample volatility forecasts over a rolling 200-day window and compare performance across models using relative error and efficiency measures.