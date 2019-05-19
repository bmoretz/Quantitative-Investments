## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(xts)
library(Quandl)

## Get 6 contracts of the crude curve
crude.tickers <- c("CHRIS/CME_CL1", "CHRIS/CME_CL2", "CHRIS/CME_CL3",
                 "CHRIS/CME_CL4", "CHRIS/CME_CL5", "CHRIS/CME_CL6")
settle.field <- "Settle"
crude.prices <- Quandl(crude.tickers[1], type="xts")[,settle.field]
for (i in 2:length(crude.tickers)) {
    crude.tmp <- Quandl(crude.tickers[i], type="xts")[,settle.field]
    crude.prices <- cbind(crude.prices, crude.tmp)
}
colnames(crude.prices) <- paste("CL", 1:6, sep="")

crude.returns <- na.omit(diff(log(crude.prices)))
pc.crude <- princomp(crude.returns)
summary(pc.crude)
loadings(pc.crude)

## Get 6 contracts of the natgas curve
natgas.tickers <- c("CHRIS/CME_NG1", "CHRIS/CME_NG2", "CHRIS/CME_NG3",
                 "CHRIS/CME_NG4", "CHRIS/CME_NG5", "CHRIS/CME_NG6")
settle.field <- "Settle"
natgas.prices <- Quandl(natgas.tickers[1], type="xts")[,settle.field]
for (i in 2:length(natgas.tickers)) {
    natgas.tmp <- Quandl(natgas.tickers[i], type="xts")[,settle.field]
    natgas.prices <- cbind(natgas.prices, natgas.tmp)
}
colnames(natgas.prices) <- paste("NG", 1:6, sep="")

natgas.returns <- na.omit(diff(log(natgas.prices)))
pc.natgas <- princomp(natgas.returns)
summary(pc.natgas)
loadings(pc.natgas)
