## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

x <- seq(-5,5, 0.01)  # sequence of values to plot a standardized distribution

# Plot normal, logistic, and standardized t_5 densities; add a legend to look nice.
plot(x, dnorm(x), type='l', ylim=c(0,0.5), lty=1, lwd=2, col="cyan", xlab="", ylab="", main="")
lines(x, dlogis(x, scale=sqrt(3)/pi), lty=2, lwd=2)
lines(x, dt(x/sqrt((5-2)/5), df=5)/sqrt((5-2)/5), lty=4, lwd=2, col="red")
legend.text <- c(expression(paste('t'[5], "  ", kappa, "=9")),
                 expression(paste("logistic  ", kappa,"=4.2")),
                 expression(paste("normal  ", kappa, "=3")))
legend("topright", legend.text, lty=c(4,2,1), lwd=2, col=c("red", "black", "cyan"))

# Then plot logs of densities to show the tail behavior
plot(x, log(dnorm(x)), type='l', lty=1, lwd=2, col="cyan", xlab="", ylab="", main="")
lines(x, log(dlogis(x, scale=sqrt(3)/pi)), type='l', lty=2, lwd=2)
lines(x, log(dt(x/sqrt((5-2)/5), df=5)/sqrt((5-2)/5)), type='l', lty=4, lwd=2, col="red")
legend.text <- c(expression(paste('t'[5], "  ", kappa, "=9")),
                 expression(paste("logistic  ", kappa,"=4.2")),
                 expression(paste("normal  ", kappa, "=3")))
legend("topright", legend.text, lty=c(4,2,1), lwd=2, col=c("red", "black", "cyan"))
