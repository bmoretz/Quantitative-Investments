## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(MASS)

## simulation for PE exercise
## Values are from Harte (2017)
n.years <- 12
periods.per.year <- 4
dtime <- 1/periods.per.year
times <- seq(dtime, n.years, by=dtime)
mgmt.fee <- 0.02
invest.time.end <- max(times)
capital0 <- 100
fees <- capital0*mgmt.fee*dtime
investment0 <- capital0 - fees

rf <- 0.05

## drift/OU reversion parameters
#pe.alpha <- 0.04
mu.mkt <- 0.11
mu.delta <- 0.41
mu.nu <- 0.08
theta.pi <- 0.16; kappa.pi <- 0.42
pi0 <- theta.pi

## covariance parameters; some correlations differ from Harte (2017)
pe.beta <- 1.3
vol.delta <- 0.21*sqrt(dtime); vol.nu <- 0.11*sqrt(dtime); vol.pi <- 0.16*sqrt(dtime)
vol.mkt.sys <- 0.15*sqrt(dtime); vol.mkt.idio <- 0.35*sqrt(dtime)
corr.delta <- -0.3; corr.nu <- 0.8; corr.pi <- -0.3

cov.delta <- corr.delta*vol.delta*vol.mkt.sys
cov.nu    <- corr.nu*vol.nu*vol.mkt.sys
cov.pi    <- corr.pi*vol.pi*vol.mkt.sys
cov.mtx <- matrix(c(vol.delta^2,        0,        0,     cov.delta,       0,
                    0,           vol.nu^2,        0,        cov.nu,       0,
                    0,                  0, vol.pi^2,        cov.pi,       0,
                    cov.delta,     cov.nu,   cov.pi, vol.mkt.sys^2,       0,
                    0,                  0,        0,             0, vol.mkt.idio^2),
                  nrow=5)

## generate random variates
B.rvs <- mvrnorm(mu=rep(0, 5), Sigma=cov.mtx, n=n.years*periods.per.year)
colnames(B.rvs) <- c("B.delta", "B.nu", "B.pi", "B.mkt", "B.epsilon")

delta <- pmin(pmax(mu.delta*dtime + B.rvs[,"B.delta"], 0), 1)
nu <- pmin(pmax(mu.nu*times + B.rvs[,"B.nu"], 0), 1)
pi.ou <- c(pi0)
for (i in 2:length(times)) {
    pi.ou[i] <- pi.ou[i-1] + kappa.pi*(theta.pi - pi.ou[i-1])*dtime + B.rvs[i-1,"B.pi"]
}
dmkt <- mu.mkt*dtime + pe.beta*B.rvs[,"B.mkt"] + B.rvs[,"B.epsilon"]

## simulate the cashflows and value
drawdowns <- c(delta[1]*investment0)
rcvd.distn <- c(0)
value <- c(0)
for (i in 2:length(times)) {
    d.drawdowns <- delta[i]*(investment0 - drawdowns[i-1])*(times[i] < invest.time.end)
    drawdowns[i] <- drawdowns[i-1] + d.drawdowns
    d.rcvd.distn <- nu[i]*value[i-1]
    if (i < length(times)) {
        rcvd.distn[i] <- rcvd.distn[i-1] + d.rcvd.distn
        dvalue <- value[i-1]*dmkt[i-1] + d.drawdowns - d.rcvd.distn
    } else {
        rcvd.distn[i] <- rcvd.distn[i-1] + value[i-1]
        dvalue <- -value[i-1]
    }
    value[i] <- value[i-1] + dvalue
}
