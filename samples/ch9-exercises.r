## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(MASS)
library(xts)
library(quantmod)
library(Quandl)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)

## risk-free rate (for example purposes)
rf <- 0.01

etf.symbols <- c("SPY", "IWM", "AGG", "FEZ", "ACWI", "IYR")
adj.close <- 6  # 6th field is adjusted close
etf.prices <- getSymbols(etf.symbols[1], source="yahoo",
                         auto.assign=FALSE, return.class="xts")[,adj.close]
for (i in 2:length(etf.symbols)) {
    etf.tmp <- getSymbols(etf.symbols[i], source="yahoo",
                          auto.assign=FALSE, return.class="xts")[,adj.close]
    etf.prices <- cbind(etf.prices, etf.tmp)
}
colnames(etf.prices) <- etf.symbols
etf.rets <- diff(log(etf.prices))["2012/"]

commodity.symbols <- c("WTI", "Natgas", "AU", "CU", "Corn")
settle <- "Settle"  # settle field is labeled
commodity.tickers <- c("CHRIS/CME_CL1", "CHRIS/CME_NG1", "CHRIS/CME_GC1",
                       "CHRIS/CME_HG1", "CHRIS/CME_C1")
commodity.prices <- Quandl(commodity.tickers[1], type="xts")[,settle]
for (i in 2:length(commodity.symbols)) {
    commodity.tmp <- Quandl(commodity.tickers[i], type="xts")[,settle]
    commodity.prices <- cbind(commodity.prices, commodity.tmp)
}
colnames(commodity.prices) <- commodity.symbols

all.returns.tmp <- diff(log(cbind(etf.prices,commodity.prices)))["2012/"]
all.returns <- na.omit(all.returns.tmp)

## set up portfolio with objective and constraints
n.assets <- length(colnames(all.returns))
port.spec <- portfolio.spec(assets = colnames(all.returns))
port.spec <- add.objective(portfolio=port.spec, type="risk", name="StdDev")
port.spec <- add.objective(portfolio=port.spec, type="return", name="mean")
port.spec <- add.constraint(portfolio=port.spec, type="full_investment")
port.spec <- add.constraint(portfolio=port.spec, type="box", min=-1, max=1)

## map out the efficient frontier (for variance risk)
eff.frontier <- create.EfficientFrontier(R=all.returns, portfolio=port.spec,
                                         n.portfolios=100, type="mean-StdDev")

## daily Sharpe ratio
sharpe.ratios <- (eff.frontier$frontier[,"mean"]-rf/250)/eff.frontier$frontier[,"StdDev"]
max.sharpe.ratio <- sharpe.ratios[sharpe.ratios == max(sharpe.ratios)]
optimal.port.name <- names(max.sharpe.ratio)
optimal.mean <- eff.frontier$frontier[optimal.port.name,"mean"]
optimal.sd <- eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 252
print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio*sqrt(n.trading.days.per.year)))
print(sprintf("Optimal E(port return): %f", optimal.mean*n.trading.days.per.year))
print(sprintf("Optimal sd(port return): %f", optimal.sd*sqrt(n.trading.days.per.year)))
print("Optimal weights")
print(eff.frontier$frontier[optimal.port.name,(1:n.assets)+3])
