library(xts)
library(Quandl)
library(quantmod)

# Fixed Income
# Download data for Canadian government bond futures, grab the bid/ask for the five and ten year
# bond futures. Some of these will be zero, exclude them from the final data set.
cg.tickers <- c("CHRIS/MX_CGF1", "CHRIS/MX_CGB1")
cg.raw <- Quandl(cg.tickers, type = "xts")

cg.data <- cg.raw[, grepl('Bid Price', colnames(cg.raw)) | grepl('Ask Price', colnames(cg.raw))]

colnames(cg.data) <- c("CFG1.bid", "CFG1.ask", "CGB1.bid", "CGB1.ask")


cg.data[which(cg.data != 0)]
