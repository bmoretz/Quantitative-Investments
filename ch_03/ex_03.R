library(xts)
library(Quandl)
library(quantmod)

# Equity Indices
# Download data for equity index futures on the Hong Kong and Montreal Exchanges.
# Grab the (bid and ask) for the Hang Seng and TSX 60.

eqtidx.tickers <- c("CHRIS/HKEX_HSI1", "CHRIS/MX_SXM1")
eqtidx.raw <- Quandl(eqtidx.tickers, type = "xts")

head(eqtidx.raw)

eqtidx.data <- eqtidx.raw[, grepl('- Bid', colnames(eqtidx.raw)) | grepl('- Ask ', colnames(eqtidx.raw))]

colnames(eqtidx.data) <- c("HSI.bid", "HSI.ask", "TSX.bid", "TSX.ask")

eqtidx.data <- eqtidx.data[eqtidx.data[, 1] != 0 & eqtidx.data[, 2] != 0 & eqtidx.data[, 3] != 0 & eqtidx.data[, 4] != 0]

summary(eqtidx.data)

eqtidx.data$HSI.spead <- eqtidx.data$HSI.ask - eqtidx.data$HSI.bid
eqtidx.data$TSX.spead <- eqtidx.data$TSX.ask - eqtidx.data$TSX.bid

for (v in colnames(eqtidx.data)) {
  plot.tmp <- plot(eqtidx.data[, v], main = v)
  print(plot.tmp)
  invisible(readline(prompt = "Press [enter] to continue"))
}