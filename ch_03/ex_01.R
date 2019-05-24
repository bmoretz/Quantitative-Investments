library(xts)
library(Quandl)
library(quantmod)

# Fixed Income
# Download data for Canadian government bond futures, grab the bid/ask for the five and ten year
# bond futures. Some of these will be zero, exclude them from the final data set.
cg.tickers <- c("CHRIS/MX_CGF1", "CHRIS/MX_CGB1")
cg.raw <- Quandl(cg.tickers, type = "xts")

cg.data <- cg.raw[, grepl('Bid Price', colnames(cg.raw)) | grepl('Ask Price', colnames(cg.raw))]

colnames(cg.data) <- c("CGF1.bid", "CGF1.ask", "CGB1.bid", "CGB1.ask")

head(cg.data)

cg.data <- cg.data[cg.data[, 1] != 0 & cg.data[, 2] != 0 & cg.data[, 3] != 0 & cg.data[, 4] != 0]

summary(cg.data)

cg.data$CFG1.spead <- cg.data$CGF1.ask - cg.data$CGF1.bid
cg.data$CGB1.spead <- cg.data$CGB1.ask - cg.data$CGB1.bid

for (v in colnames(cg.data)) {
  plot.tmp <- plot(cg.data[, v], main = v)
  print(plot.tmp)
  invisible(readline(prompt = "Press [enter] to continue"))
}