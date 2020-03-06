## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

## example of gamma GLM fit
library(MASS)
types <- rbinom(100, 1, 0.6)  # create types (0 or 1)
times <- rgamma(100, 3, 0.2+0.1*types) # gamma r.v.s w/rate 0.2 (0.3 for types==1)
model.fit <- glm(times ~ types, family=Gamma())
summary(model.fit)  # show linear model for 1/mu = rate/m = rate/3
## moment estimator of m, preferred by McCullagh and Nelder
m.hat1 <- 1/summary(model.fit)$dispersion
## common estimator of m
m.hat2 <- summary(model.fit)$df.residual/summary(model.fit)$deviance
## MLE estimator of m, implemented in MASS
m.hat3 <- gamma.shape(model.fit)

## example of survival analysis
## suppose bonds mature at 15 years so times > 15 are censored
library(survival)
library(flexsurv)
defaulted <- times < 15   # TRUE => data not censored
times.censored <- pmin(times, 15)

## Fit gamma parameters
## Note that covariate coefficients are modeling rate
## anc (ancillary) parameter allows modeling shape parameter
model.fit <- flexsurvreg(Surv(times, defaulted) ~ types, dist="gamma")
model.fit  # show parameter estimates
