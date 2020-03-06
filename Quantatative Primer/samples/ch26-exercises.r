## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(xts)
library(Quandl)
library(quantmod)

fit.bubblear1 <- function(prcs) {
    coef(lm.fit(cbind(Intercept = 1, prcs[,-1]), prcs[,1]))[2]
}

btc.prices <- Quandl("BITFINEX/BTCUSD", type="xts")[,"Mid"]
btc.prices$lagMid <- lag(btc.prices$Mid)
fits.btc.ar <- rollapply(na.omit(btc.prices), 90, fit.bubblear1, align="right", by.column=FALSE)

spx.prices <- Quandl("CHRIS/CME_SP1", type="xts")[,"Settle"]
spx.prices$lagSettle <- lag(spx.prices$Settle)
fits.spx.ar <- rollapply(na.omit(spx.prices), 90, fit.bubblear1, align="right", by.column=FALSE)

ust.tickers <- c("FRED/DGS3MO", "FRED/DGS2", "FRED/DGS10", "FRED/DGS30")
ust <- Quandl(ust.tickers, type="xts")/100
colnames(ust) <- c("UST3M", "UST2Y", "UST10Y", "UST30Y")

eugb.tickers <-c("BUNDESBANK/BBK01_WT3210", # 6M
                 "BUNDESBANK/BBK01_WT3213", # 2Y
                 "BUNDESBANK/BBK01_WT3229", # 10Y
                 "BUNDESBANK/BBK01_WT3500") # 30Y
eugb <- Quandl(eugb.tickers, type="xts")/100
colnames(eugb) <- c("EUGB6M", "EUGB2Y", "EUGB10Y", "EUGB30Y")

eonia.6m <- Quandl("BUNDESBANK/BBK01_ST0304", type="xts")/100

ted <- Quandl("FRED/TEDRATE", type="xts")/100
bed <- eonia.6m - eugb$EUGB6M

index.tickers <- c("^SPX", "^STOXX50E")
adj.close <- 6  # 6th field is adjusted close
spx.prices <- getSymbols("^SPX", source="yahoo", auto.assign=FALSE,
                         return.class="xts")[,adj.close]
estox.prices <- getSymbols("^STOXX50E", source="yahoo", auto.assign=FALSE,
                           return.class="xts")[,adj.close]
spx.rets <- diff(log(spx.prices))
estox.rets <- diff(log(estox.prices))

