library(xts)
library(Quandl)
library(quantmod)

# Get constant-maturity (US) Treasuries

ust.tickers <- c("FRED/DGS3MO", "FRED/DGS2", "FRED/GDS10", "FRED/DGS30")
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

