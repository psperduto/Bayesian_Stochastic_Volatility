# Stochastic Volatility Modeling Bayesian Framework

## Motivation

Financial modeling often involves estimating quantities that are not directly observed in return data. One of the main ones is volatility. It plays an important role in risk management, pricing, and trading decisions.

Return data usually shows volatility clustering and persistence. Periods of high volatility tend to follow each other, and the same is true for low volatility. This means volatility is not constant and changes over time.

Since volatility is not directly observed, it makes sense to model it as a latent process and estimate it from the data. This leads to stochastic volatility models. Bayesian methods provide a way to estimate this process and account for uncertainty.