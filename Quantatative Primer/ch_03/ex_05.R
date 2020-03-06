library(xts)
library(Quandl)
library(quantmod)

# Equities
# Look at short volumn vs. stock price for three names.
# AMD, RGR and TSLA.
# Download short volume and adjusted close prices from yahoo.

eqty.tickers <- c("FINRA/FNSQ_AMD", "FINRA/FNSQ_RGR", "FINRA/FNSQ_TSLA")
eqty.raw <- Quandl(eqty.tickers, type = "xts")

