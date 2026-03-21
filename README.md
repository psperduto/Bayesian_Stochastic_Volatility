# Stochastic Volatility Modeling Bayesian Framework

## Motivation

Financial modeling often involves estimating quantities that are not directly observed in return data. One of the main ones is volatility. It plays an important role in risk management, pricing, and trading decisions.

Return data usually shows volatility clustering and persistence. Periods of high volatility tend to follow each other, and the same is true for low volatility. This means volatility is not constant and changes over time.

Since volatility is not directly observed, it makes sense to model it as a latent process and estimate it from the data. This leads to stochastic volatility models. Bayesian methods provide a way to estimate this process and account for uncertainty.

## Traditional methodology

In practice the Generalized Autoregressive Condition Heteroskedasticity (GARCH) framework is widely accepted as a simple and effective way to model estimate this latent volatility. 

The Garch model is a deterministic way to calculate the volatility at a time, T_i, provided with the returns of the T_j, where j<i.

The alternative to this is to treat this volatility parameter as a latent stochastic process for which can be modeled with a framework.

## General approach

Following the hierarchical model proposed in the paper "Bayesian Analysis of Stochastic Volatility Models" (1994) by Jacquier we build a model following the AR(1) framework, to simulate, estimate, and forecast these volatility densities conditioned all the observed prior returns.

Utilizing Markov Chain Monte Carlo simulations and metropolis algorithms we can efficiently simulate the posterior densities and create credible intervals to which we may compare with more traditional models.

From these methods we then build a rolling forecasting pipeline estimating the volatility of the next day over a 200 day period for our Garch and Bayesian models to compare relative error.

## The pipeline

In this pipeline we display how normal Gaussian distributions fails to capture the heavy tail characteristics of financial returns data, and why more complex models are necessary.

Pulling data via the quantmod library and yfinance efficiently pull the returns data of a specified ticker name, and transform into a log_return object.

These log returns exhibit nice qualities most importantly a near zero uncorrelated mean but a very highly correlated volatility. Following the data retrival we then fit the classic Garch and t-Garch deterministic models to model the latent volatility.

Utilizing the JAGS library and following the paper by Jacquier we then sequentially simulate the posterior densities of our AR(1) parameters and the latent volatility (h_t) from the conditional distributions.

From these fit Garch and SV bayesian models we forecast the ticker volatility over the last 200 day window and compare relative errors and efficieny.
