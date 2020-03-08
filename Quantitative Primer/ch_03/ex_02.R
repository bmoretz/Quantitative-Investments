library(xts)
library(Quandl)
library(quantmod)

# Commodities
# Download data for metals on the London Metal Exchange.
# Grab the buyer and seller prices for "cash" buyer and seller (bid and ask) for
# copper, aluminum, and tin.

com.tickers <- c("LME/PR_CU")
com.raw <- Quandl(com.tickers, type = "xts")

com.data <- com.raw[, grepl('Bid Price', colnames(com.raw)) | grepl('Ask Price', colnames(com.raw))]

colnames(com.data) <- c("CGF1.bid", "CGF1.ask", "CGB1.bid", "CGB1.ask")

head(com.data)

com.data <- com.data[com.data[, 1] != 0 & com.data[, 2] != 0 & com.data[, 3] != 0 & com.data[, 4] != 0]

summary(com.data)