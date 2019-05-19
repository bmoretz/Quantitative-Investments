## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(xts)
library(Quandl)
library(quantmod)

## Get risk-free rates
rf.tickers <- c("FRED/DGS3MO", "BOC/V39065", "BUNDESBANK/BBK01_WT3210",
                "SNB/RENDOBLID", "RBA/F17_0", "MOFJ/INTEREST_RATE_JAPAN")
rf.columns <- c(1,1,1,1,2,1)
rf.ids <- c("USD3M", "CAD3M","EUR6M","CHF1Y","AUD3M","JPY1Y","HKD3M")
## loop through tickers and columns
rf <- Quandl(rf.tickers[1], type="xts")[,rf.columns[1]]/100
for (i in 2:length(rf.tickers)) {
    rf.tmp <- Quandl(rf.tickers[i], type="xts")[,rf.columns[i]]/100
    rf <- cbind(rf, rf.tmp)
}

## Getting HKMA data is not straightforward, so...
## ...these are from the Hong Kong Monetary Authority
hkma.exfund.3M.rates <-
    c(0.09,0.10,0.10,0.11,0.26,0.62,0.27,0.18,0.24,0.25,0.30,0.28, # 2010
      0.16,0.19,0.20,0.11,0.13,0.06,0.07,0.10,0.11,0.10,0.17,0.22, # 2011
      0.18,0.11,0.10,0.12,0.10,0.08,0.15,0.17,0.23,0.09,0.07,0.05, # 2012
      0.11,0.08,0.06,0.08,0.09,0.15,0.16,0.15,0.17,0.13,0.12,0.11, # 2013
      0.14,0.14,0.14,0.10,0.07,0.09,0.07,0.04,0.09,0.03,0.03,0.04, # 2014
      0.03,0.01,0.02,0.01,0.01,0.00,0.01,-0.01,-0.01,0.00,-0.01,0.04, # 2015
      0.24,0.07,0.07,0.06,0.20,0.17,0.28,0.30,0.31,0.30,0.28,0.67, # 2016
      0.54,0.41,0.27,0.40,0.29,0.31,0.35,0.28,0.45,0.85,0.75,1.02, # 2017
      0.66,0.56) # 2018
rf.subset <- rf["201001/201802"]
hkma.3M <- xts(hkma.exfund.3M.rates,
               order.by = index(rf.subset[xts:::startof(rf.subset, "months")]))
rf <- cbind(rf, hkma.3M)
rf <- na.locf(rf)
colnames(rf) <- rf.ids

# Get indices and stock prices
adj.close <- 6  # 6th field is adjusted close
equity.tickers <- c("^GSPC","^GSPTSE","^STOXX50E","^SSMI","^AORD","^HSI","^N225",
                    "CTC-A.TO","F","HSBC","PFE","SVNDY","TM","EDF.PA","NOVN.VX",
                    "SIE.DE","VOW.DE","BRG.AX","MQG.AX","J36.SI","0019.HK")
equity.ids <- c("SPX","TSXCOMP","ESX50","SMI","AORD","HS","N225",
                "CATIRE","FORD","HSBC","PFIZER","SEVENANDI","TOY","EDF","NOVARTIS",
                "SIEMENS","VW","BREVILLE","MACQUARIE","JARDINES","SWIRE")
prices <- getSymbols(equity.tickers[1], source="yahoo", auto.assign=FALSE,
                         return.class="xts")[,adj.close]
for (i in 2:length(equity.tickers)) {
  prices.tmp <- getSymbols(equity.tickers[i], source="yahoo",
                         auto.assign=FALSE, return.class="xts")[,adj.close]
  prices <- cbind(prices, prices.tmp)
}
## We often get errors here since international stocks are not always
## traded on our home days, so there are some NAs.  No worries; fill
## forward and returns will be zero on non-trading days.  (Might be a
## minor bias on volatility estimates; we will probably survive.
prices <- na.locf(prices)
colnames(prices) <- equity.ids
returns <- diff(log(prices))

## Now we join all of the datasets together and trim to recent
alldata <- cbind(rf, returns)["2010/"]

## create excess returns
equity.names.xs <- paste(equity.names, ".xs", sep="")
## now in a for loop subtract off a daily risk-free rate, FOR EXAMPLE:
alldata$SPX.xs <- alldata$SPX - alldata$T3M/250
## Only oddity: use HKD rf for Jardines.

# If your ticker were DAL and you wanted to model returns
# (not excess returns) using ESTOX and SMI, you would do like so:
simple.wrong.model <- lm(DAL ~ ESTOX + SMI, data=alldata)
summary(simple.wrong.model)
# NOTE that this is not the model you are supposed to do
