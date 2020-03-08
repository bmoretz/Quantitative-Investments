## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

bs.call <- function(S,K,rf,T,sigma) {
    d1 <- (log(S/K)+(rf+sigma^2/2)*T)/(sigma*sqrt(T))
    S*pnorm(d1)-K*exp(-rf*T)*pnorm(d1-sigma*sqrt(T))
}

oz.per.year <- 10000
rf <- 0.03  # 3% interest rates
wacc <- 0.08  # 8% weighted average cost of capital
s0 <- 1300
s0.lt <- 1200
K <- 1000
sigma <- 0.3

## annual real options approach, no mean reversion
oz.per.year*((s0-K)/(1+wacc) + bs.call(s0,K,rf,1,sigma)/(1+wacc) +
             sum(bs.call(s0.lt,K,rf,1:8,sigma)/(1+wacc)*exp(-rf)))

## annual real options, assuming mean reversion (approximation)
oz.per.year*((s0-K)/(1+wacc) + bs.call(s0,K,rf,1,sigma)/(1+wacc) +
             sum(bs.call(1200,1000,0.03,1,0.3)/(1+wacc)*exp(-rf*(1:8))))

## quarterly real options, no mean reversion
oz.per.year/4*((s0-K)/(1+wacc)^0.25
    + sum(bs.call(1300,1000,0.03,(1:4)/4,0.3)/(1+wacc)^0.25)
    + sum(bs.call(1300-25*(1:3),1000,0.03,0.25,0.3)/(1+wacc)^0.25*exp(-rf*(5:7)/4))
    + sum(bs.call(1200,1000,0.03,(1:32)/4,0.3)/(1+wacc)^0.25*exp(-rf*(8:39)/4)))

## quarterly real options, assuming mean reversion (approximation)
oz.per.year/4*((s0-K)/(1+wacc)^0.25
    + sum(bs.call(1300,1000,0.03,0.25,0.3)/(1+wacc)^0.25*exp(-rf*(0:3)/4))
    + sum(bs.call(1300-25*(1:4),1000,0.03,0.25,0.3)/(1+wacc)^0.25*exp(-rf*(4:7)/4))
    + sum(bs.call(1200,1000,0.03,0.25,0.3)/(1+wacc)^0.25*exp(-rf*(8:39)/4)))
