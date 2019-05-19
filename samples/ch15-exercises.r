## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(xts)
library(quantmod)
library(Quandl)
library(rugarch)

# get CMT USTs: 3M, 2Y, 10Y, 30Y

## Download Fama-French data
french.base <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp"
factor.file <- "F-F_Research_Data_Factors_daily_CSV.zip"
french.url <- paste(french.base, factor.file, sep="/")
temp.file <- tempfile()
download.file(french.url, destfile=temp.file)
ff.tmp <- read.csv(unz(temp.file, "F-F_Research_Data_Factors_daily.CSV"),
                   header = TRUE, skip = 3)
unlink(temp.file)
# remove obnoxious last line, scale percentages, create xts object
ff.tmp <- ff.tmp[-length(ff.tmp[,1]),]
ff.data <- as.xts(ff.tmp[,c("SMB","HML")],
                  order.by=as.POSIXct(ff.tmp[[1]], format="%Y%m%d"))

## Download Carhart (momentum) data; skip absurd number of comment lines
factor.file <- "F-F_Momentum_Factor_daily_CSV.zip"
french.url <- paste(french.base, factor.file, sep="/")
temp.file <- tempfile()
download.file(french.url, destfile=temp.file)
umd.tmp <- read.csv(unz(temp.file, "F-F_Momentum_Factor_daily.CSV"),
                    header = TRUE, skip = 13)
unlink(temp.file)
# remove obnoxious last line, scale percentages, create xts object
umd.tmp <- umd.tmp[-length(umd.tmp[,1]),]
umd.data <- as.xts(umd.tmp[,c("Mom")], order.by=as.POSIXct(umd.tmp[[1]], format="%Y%m%d"))
colnames(umd.data) <- c("UMD")

### Handle monthly data
# For expected CPI and realized CPI, only get the first column of
# data... like so:
exinfl <- Quandl("FRBC/EXIN", type="xts")[,1]
colnames(exinfl) <- c("EXINFL")

cpi <- Quandl("FRBC/USINFL", type="xts")[,1]
colnames(cpi) <- c("CPI")

excpi <- cpi*(1+exinfl)  # expected CPI in twelve months
cpi.surprise <- log(cpi) - log(lag(excpi, 12))  # % CPI surprise
colnames(cpi.surprise) <- c("INFLSURP")

# For industrial production, compute log-returns (% changes)
indprod <- Quandl("FRED/INDPRO", type="xts")
colnames(indprod) <- c("INDPROD")
indprod.logret <- diff(log(indprod))

# Get index and stock prices; create returns

# Now we join all of the datasets together
alldata.full <- cbind(ust, exinfl, cpi.surprise, ltcorpbond, indprod, ff.data, umd.data, returns)

# For monthly data: Last Observation Carried Forward (until new number)
alldata <- na.locf(alldata.full)["2010/"]

# create excess returns for indices, stocks

# Handy way to compute a function for each column
apply(alldata, 2, mean, na.rm=TRUE)  # "2" = by columns; "1" = by rows

# If your ticker were DAL and you wanted to model returns (not excess
# returns) using HML and SMB, you would do like so:
hml.wrong.model <- lm(DAL ~ HML + SMB, data=alldata)
summary(hml.wrong.model)
# NOTE that this is not a model you are supposed to do for the homework

# Now do GARCH-in-mean models
gim.spec <- ugarchspec(variance.model=list(model="sGARCH", archm=TRUE, archpow=2),
                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE))
garch.in.mean.spx <- ugarchfit(data=alldata$SPX.xs, spec=gim.spec)
show(garch.in.mean.spx)
