## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

y <- seq(0.5, 8.5, by=0.01)   # range of yields
d6m <- 1/(1+y/2/100)   # 6-month discount factor
c1 <- 4.5  # annual coupon
b1 <- c1/2*(d6m+d6m^2+d6m^3+d6m^4) + 100*d6m^4  # 2Y bond
b2 <- c1/(y/100)*(1-d6m^10) + 100*d6m^10        # 5Y bond
b3 <- c1/(y/100)*(1-d6m^20) + 100*d6m^20        # 10Y bond
b4 <- c1/(y/100)*(1-d6m^40) + 100*d6m^40        # 20Y bond

plot(y,b4, type='l', xlim=c(1,8), lty=1, lwd=2, col="purple")
lines(y, b3, lty=2, lwd=2, col="magenta")
lines(y, b2, lty=3, lwd=2, col="red")
lines(y, b1, lty=4, lwd=2, col="orange")
abline(h=100, lty=1, lwd=1, col="gray")
legend.text <- c("20Y", "10Y", "5Y", "2Y")
legend("topright", legend.text, lty=1:4, lwd=2, col=c("purple","magenta","red","orange"))
