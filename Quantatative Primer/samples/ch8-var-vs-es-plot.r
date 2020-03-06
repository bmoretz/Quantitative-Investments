## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

alpha <- 0.05  # probability mass in the loss tail
t.df <- 5      # degrees of freedom for the t-distribution

# Calculate VaR (inverse CDF) and expected shortfall (numeric integration)
value.at.risk <- qt(alpha, df=t.df)
t5.mean.integrand <- function(x) { x*dt(x,t.df)}
ex.shortfall <- 1/alpha*integrate(t5.mean.integrand, lower=-Inf,
                                  upper=value.at.risk)[["value"]]

curve(dt(x, t.df), xlim=c(-4,4), lwd=2)  # plot the t-distribution

# Setup for drawing a filled-in area under the curve
x.seq <- seq(-5, value.at.risk, 0.01)
y.seq <- dt(x.seq, t.df)
cord.x <- c(-5, x.seq, value.at.risk)
cord.y <- c(0, y.seq, 0)
polygon(cord.x, cord.y, col="plum1")  # and draw the filled-in area

abline(v=value.at.risk, lty=2, lwd=2, col="red")  # add a vertical line at the VaR
abline(v=ex.shortfall, lty=4, lwd=2, col="deepskyblue")   # add a vertical line at the ES
abline(h=0)
text(x=-2.015, y=0.05, "5%-VaR", pos=4)  # Add text notes near...
text(x=-2.890, y=0.01, "5%-ES", pos=4)   # ...the vertical lines
