## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

## B = Bloomberg Barclays Aggregate Bond index ETF (AGG), from Yahoo Finance
expret.bonds <- 0.0269
vol.bonds <- 0.0292
## E = S&P 500 index ETF (SPY), from Yahoo Finance
vol.stocks <- 0.1027
expret.stocks <- 0.1076
rf <- 0.01  ## F = risk-free rate (approximately)

wt.bonds <- seq(-0.5,1.5,0.005)  ## explore bond weights: short 50% to long 150%
## create expected portfolio returns for weights
expret.port <- wt.bonds*expret.bonds + (1-wt.bonds)*expret.stocks

## plot and set up axes
plot.default(NULL, type='l', xlim=c(0,0.15), ylim=c(-0.01, 0.15))
abline(h=0, lty=2, col="gray80")
lines(c(0,0), c(0.08,-0.025), lty=2, col="gray80")

## iterate through correlations from 1 to -1 in steps of -0.25
## find the portfolio volatility and then plot the curve
## RAINBOW-y for visibility and fabulous pride
line.colors <- c("purple","magenta", "red", "orange", "yellow",
                 "chartreuse", "aquamarine", "cyan", "blue")
corrs.bonds.stocks <- seq(1, -1, -0.25)
legend.keys <-  c()
for (i in 1:length(corrs.bonds.stocks)) {
    corr.bs <- corrs.bonds.stocks[i]
    var.port.tmp <- wt.bonds^2*vol.bonds^2 + (1-wt.bonds)^2*vol.stocks^2 +
        2*wt.bonds*(1-wt.bonds)*corr.bs*vol.bonds*vol.stocks
    vol.port.tmp <- sqrt(var.port.tmp)
    lines(vol.port.tmp, expret.port, lty=i, lwd=2, col=line.colors[i])
    legend.keys <- c(legend.keys, bquote(rho==~.(corr.bs)))
}
## Add a legend so we know which curves are for which correlations
legend.text <- as.expression(legend.keys)
legend("topleft", legend.text, lty=1:length(legend.text), cex=0.8, lwd=2, col=line.colors)

## show where F, B, and E are
points(0,0.01,lwd=2)
text(0.005, 0.01, "F")
points(vol.bonds, expret.bonds,lwd=2)
text(vol.bonds+0.005, expret.bonds, "B")
points(vol.stocks, expret.stocks,lwd=2)
text(vol.stocks, expret.stocks-0.01, "E")
