## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(xts)
library(quantmod)
library(Quandl)

# Get TED spread
ted <- Quandl("FRED/TEDRATE", type="xts")/100
colnames(ted) <- c("TED")

# Get S&P 500, Russell 2000, and ETF returns
adj.close <- 6  # 6th field is adjusted close
equity.tickers <- c("^GSPC","^RUT","SPY","IWM","UN","UL","RELX","RENX","BHP","BBL")
prices <- getSymbols(equity.tickers[1], source="yahoo", auto.assign=FALSE,
                         return.class="xts")[,adj.close]
for (i in 2:length(equity.tickers)) {
  prices.tmp <- getSymbols(equity.tickers[i], source="yahoo",
                         auto.assign=FALSE, return.class="xts")[,adj.close]
  prices <- cbind(prices, prices.tmp)
}
equity.names <- c("SPX","RUT","SPY","IWM","UN","UL","RELX","RENX","BHP","BBL")
colnames(prices) <- equity.names

# compute differences

# Now we join all of the datasets together
alldata.full <- cbind(ted, differences)["2007/"]
