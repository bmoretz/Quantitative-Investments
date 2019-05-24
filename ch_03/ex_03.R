library(xts)
library(Quandl)
library(quantmod)

# Equity Indices
# Download data for equity index futures on the Hong Kong and Montreal Exchanges.
# Grab the (bid and ask) for the Hang Seng and TSX 60.

eqtidx.tickers <- c("CHRIS/HKEX_HSI1", "CHRIS/MX_SXM1")
eqtidx.raw <- Quandl(eqtidx.tickers, type = "xts")
