## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

rf <- 0.02  # 2% interest rates
num.sims <- 100000 # 100,000 simulations
s0 <- 90  # underlier price at t=0
s.bar <- 95
K <- 102  # strike price
sigma <- 0.25  # volatility of underlier log-returns
T <- 2  # 2 years = option expiry
tau <- 1  # mid-life of one-year bond
lambda <- 2  # mean reversion rate; 2 => half-life = ln(2)/2 = 0.35 years

## arithmetic Brownian motion; normal underlier price
w1 <- rnorm(num.sims) # simulated W_T values
sigma.abm <- sigma*s0  # rescale volatility for initial prife level
s.sim.abm <- s0 + rf*T + sigma.abm*sqrt(T)*w1
opt.payoff.abm <- pmax(s.sim.abm - K, 0)
opt.value.abm <- mean(opt.payoff.abm)*exp(-rf*T)

## geometric Brownian motion; normal underlier log-returns
w2 <- rnorm(num.sims) # simulated W_T values
s.sim.gbm <- s0*exp((rf-sigma^2/2)*T + sigma*sqrt(T)*w2)
opt.payoff.gbm <- pmax(s.sim.gbm - K, 0)
opt.value.gbm <- mean(opt.payoff.gbm)*exp(-rf*T)

## Ornstein-Uhlenbeck process for underlier price
w3 <- rnorm(num.sims) # simulated W_T values
s.sim.ou <- s.bar + exp(-lambda*T)*(s0-s.bar) + sigma*s.bar*
        sqrt((1-exp(-2*lambda*T))/(2*lambda))*w3
opt.payoff.ou <- pmax(s.sim.ou - K, 0)
opt.value.ou <- mean(opt.payoff.ou)*exp(-rf*T)

## Brownian bridge; two-year bond maturing at 100
## option is struck at 102 in year 1
## FYI: vol of 0.25 is very high for a two-year bond
w4 <- rnorm(num.sims) # simulated W_tau values
s.bar.bb <- s0 + (100-s0)*tau/T
s.sim.bb <- s.bar.bb + sigma*s.bar.bb*sqrt(tau/T*(T-tau)/T)*w4
opt.payoff.bb <- pmax(s.sim.bb - K, 0)
opt.value.bb <- mean(opt.payoff.bb)*exp(-rf*tau)
