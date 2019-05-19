library(xts)
library(Quandl)
library(quantmod)

# FX Rates
# Download data for BitFinEx
# Grab the (bid and ask) between USD and BTC levels.

btcusd.tickers <- c("BITFINEX/BTCUSD")
btcusd.raw <- Quandl(cg.tickers, type = "xts")