library(xts)
library(Quandl)
library(quantmod)

# Commodities
# Download data for metals on the London Metal Exchange.
# Grab the buyer and seller prices for "cash" buyer and seller (bid and ask) for
# copper, aluminum, and tin.

com.tickers <- c("LME/PR_CU", "LME/PR_AL", "LME/PR_TN")
com.raw <- Quandl(cg.tickers, type = "xts")