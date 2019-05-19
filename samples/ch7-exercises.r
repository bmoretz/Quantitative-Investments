## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(Quandl)
library(xts)
library(quantmod)
library(PerformanceAnalytics)

# Example of reading in CMTs from Quandl
# Name columns so we know what each holds after joining them together
ust.tickers <- c("FRED/DGS3MO", "FRED/DGS2", "FRED/DGS10", "FRED/DGS30")
ust.raw <- Quandl(ust.tickers, type="xts")/100
colnames(ust.raw) <- c("T3M.yld", "T2Y.yld", "T10Y.yld", "T30Y.yld")

# This is a way to get approximate returns for these bonds.
# Later on, you will learn about duration and why we can do this. 
ust.yieldchanges <- diff(ust.raw)
colnames(ust.yieldchanges) <- c("T3M", "T2Y", "T10Y", "T30Y")
ust <- ust.yieldchanges
ust$T3M  <- -0.25*ust.yieldchanges$T3M
ust$T2Y  <- -1.98*ust.yieldchanges$T2Y
ust$T10Y <- -8.72*ust.yieldchanges$T10Y
ust$T30Y <- -19.2*ust.yieldchanges$T30Y

# Get Eurodollar futures (settlement) prices and create log-returns.
ed1.raw <- Quandl("CHRIS/CME_ED1", type="xts")[,"Settle"]
ed1 <- diff(log(ed1.raw))
colnames(ed1) <- c("ED1")

# Get S&P 500 prices (just adjusted close); then create log-returns.
# Do similarly for the Russell 2000, and other stocks.
adj.close <- 6  # 6th field is adjusted close
spx.raw <- getSymbols("^GSPC", source="yahoo", auto.assign=FALSE, return.class="xts")[,adj.close]
colnames(spx.raw) <- c("SPX.prc")
spx <- diff(log(spx.raw))
colnames(spx) <- c("SPX")

# Join all of the datasets together: US Treasuries, Eurodollars,
# S&P 500, Russell 2000, and group 1 and group 2 stocks.
# Then trim them down so the dates are consistent.
alldata.full <- cbind(ust.raw, ust, ed1, ed24, spx.raw, spx,
                      rut.raw, rut, yourticker.raw, yourticker)
alldata <- alldata.full["20130513/20170516"]

# Calculate annual volatilities like so:
apply(alldata, 2, sd)*sqrt(250)

# skewness and kurtosis are independent of time; no need to scale them
skewness(alldata, method="moment")
kurtosis(alldata, method="moment")
