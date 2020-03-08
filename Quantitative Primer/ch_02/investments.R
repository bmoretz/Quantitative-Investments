library(xts)
library(Quandl)
library(quantmod)

# Get constant-maturity (US) Treasuries

ust.tickers <- c("FRED/DGS3MO", "FRED/DGS2", "FRED/DGS10", "FRED/DGS30")
ust.raw <- Quandl(ust.tickers, type = "xts")
ust.colnames <- c("UST3M", "UST2Y", "UST10Y", "UST30YY")
colnames(ust.raw) <- ust.colnames

corpbond.raw <- Quandl("FED/RIMLPBAAR_N_B", type = "xts")
colnames(corpbond.raw) <- c("USBaaCorp")

re.tickers <- c("CHRIS/CME_JR1", "NLY", "EQR", "CAR-UN.TO")
re.raw <- Quandl(re.tickers[1], type = "xts")[, "Settle"]

# Sadly, there is a data error: a few days reported 10x price
re.raw[re.raw > 1000] <- re.raw[re.raw > 2000] / 10

for (j in 2:length(re.tickers)) {
  tmp <- getSymbols(re.tickers[j], src = "yahoo", env = NULL)
  adj.col <- last(colnames(tmp))
  re.raw <- cbind(re.raw, tmp[, adj.col])
}

colnames(re.raw) <- c("DJUSRE", "NLY", "EqRes", "CdnApt")

commodities.tickers <- c("CHRIS/CME_GI1", "CHRIS/CME_C1", "CHRIS/CME_S1", "CHRIS/CME_HG1",
                         "CHRIS/SHFE_RB1", "CHRIS/CME_CL1", "CHRIS/CME_NG1")
commodities.raw <- Quandl(commodities.tickers[1], type = "xts")[, "Settle"]

for (j in 2:length(commodities.tickers)) {
  tmp <- Quandl(commodities.tickers[j], type = "xts")[, "Settle"]
  commodities.raw <- cbind(commodities.raw, tmp)
}
colnames(commodities.raw) <- c("GSCI", "Corn", "Soybeans", "Copper", "Rebar",
                               "WTI", "USNatgas")

eqtidx.tickers <- c("^GSPC", "^RUT", "^GSPTSE", "^MXX", "^FTSE", "^STOXX50E",
                    "^SSMI", "^HSI", "^STI", "^N225")
tmp <- getSymbols(eqtidx.tickers[1], src = "yahoo", env = NULL)
adj.col <- last(colnames(tmp))
eqtidx.raw <- tmp[, adj.col]
for (j in 2:length(eqtidx.tickers)) {
  tmp <- getSymbols(eqtidx.tickers[j], src = "yahoo", env = NULL)
  adj.col <- last(colnames(tmp))
  eqtidx.raw <- cbind(eqtidx.raw, tmp[, adj.col])
}
colnames(eqtidx.raw) <- c("SP500", "R2000", "TSX", "IPC", "FTSE100", "ESTX50",
                          "SMI", "HangSeng", "STI", "NK225")

fx.tickers <- c("CHRIS/CME_JY1", "CHRIS/CME_EC1", "CHRIS/CME_CD1", "CHRIS/ICE_AR1")
fx.raw <- Quandl(fx.tickers[1], type = "xts")[, "Settle"]
for (j in 2:length(fx.tickers)) {
  tmp <- Quandl(fx.tickers[j], type = "xts")[, "Settle"]
  fx.raw <- cbind(fx.raw, tmp)
}
colnames(fx.raw) <- c("USDJPY", "USDEUR", "USDCAD", "AUDNZD")

alldata.full <- cbind(ust.raw, corpbond.raw, re.raw, commodities.raw,
                      eqtidx.raw, fx.raw)
alldata <- alldata.full["20140101/20180515"]

summary(alldata)

for (v in colnames(alldata)) {
  plot.tmp <- plot(alldata[, v], main = v)
  print(plot.tmp)
  invisible(readline(prompt="Press [enter] to continue"))
}