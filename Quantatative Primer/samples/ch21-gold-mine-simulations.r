## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

oz.per.year <- 10000
rf <- 0.03  # 3% interest rates
wacc <- 0.08  # 8% weighted average cost of capital
num.sims <- 100000 # 100,000 simulations
s0 <- 1300  # gold price at t=0
K <- 1000  # cost of extraction
sigma <- 0.3  # volatility of gold log-returns
lambda <- 1  # rate of mean reversion; 1 => half-life = ln(2) = 0.69 years

## Annual extraction decisions at start of each year (t=0,...,9)
T.list <- 1:9  # 1--9 year options; years 2:8 are 1 year fwd
s.bar.list <- c(1300, rep(1200, 8))  # 1300/oz, then 1200/oz

# gold price follows geometric Brownian motion
sum.val <- (s0-K)/(1+wacc)  # time t=0 decision
for (i in 1:9) {
    T <- T.list[i]
    z <- rnorm(num.sims) # simulated Z_T values
    s.sim <- s0*exp((rf-sigma^2/2)*T+sigma*sqrt(T)*z)
    if (T>1) s.sim <- s.sim*s.bar.list[i]/s0
    opt.terminal.val.sim <- pmax(s.sim - K, 0)
    opt.value <- mean(opt.terminal.val.sim)*exp(-rf*T)
    sum.val <- sum.val + opt.value/(1+wacc)
}
print(sum.val*oz.per.year)

## gold price follows Ornstein-Uhlenbeck mean reversion
sum.val <- (s0-K)/(1+wacc)
for (i in 1:9) {
    T <- T.list[i]
    s.bar <- s.bar.list[i]
    s.meanrevert.sim <- s.bar + exp(-lambda*T)*(s0-s.bar) + sigma*s0*
        sqrt((1-exp(-2*lambda*T))/(2*lambda))*z
    opt.terminal.val.meanrevert.sim <- pmax(s.meanrevert.sim - K, 0)
    opt.meanrevert.value <- mean(opt.terminal.val.meanrevert.sim)*
        exp(-rf*T)
    sum.val <- sum.val + opt.meanrevert.value/(1+wacc)
}
print(sum.val*oz.per.year)

## Quarterly extract decisions at start of each quarter
T.list <- (1:39)/4 # 1--39 quarter options
s.bar.list <- c(rep(1300, 4), 1275, 1250, 1225, rep(1200, 32))

# gold price follows geometric Brownian motion
sum.val <- (s0-K)/(1+wacc)^0.25
for (i in 1:39) {
    T <- T.list[i]
    z <- rnorm(num.sims) # simulated Z_T values
    s.sim <- s0*exp(sigma*sqrt(T)*z)
    opt.terminal.val.sim <- pmax(s.sim - K, 0)
    opt.value <- mean(opt.terminal.val.sim)*exp(-rf*T)
    sum.val <- sum.val + opt.value/(1+wacc)^0.25
}
print(sum.val*oz.per.year/4)

# gold price follows Ornstein-Uhlenbeck mean reversion
sum.val <- (s0-K)/(1+wacc)^0.25
for (i in 1:39) {
    T <- T.list[i]
    s.bar <- s.bar.list[i]
    s.meanrevert.sim <- s.bar + exp(-lambda*T)*(s0-s.bar) + sigma*s0*
        sqrt((1-exp(-2*lambda*T))/(2*lambda))*z
    opt.terminal.val.meanrevert.sim <- pmax(s.meanrevert.sim - K, 0)
    opt.meanrevert.value <- mean(opt.terminal.val.meanrevert.sim)*exp(-rf*T)
    sum.val <- sum.val + opt.meanrevert.value/(1+wacc)^0.25
}
print(sum.val*oz.per.year/4)
