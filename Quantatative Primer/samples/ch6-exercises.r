## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(xts)
library(Quandl)

# Grab constant-maturity US Treasuries
ust.tickers <- c("FRED/DGS3MO", "FRED/DGS2", "FRED/DGS5", "FRED/DGS10", "FRED/DGS30")
ust <- Quandl(ust.tickers, type="xts")/100
ust.colnames <- c("T3M", "T2Y", "T5Y", "T10Y", "T30Y")
colnames(ust) <- ust.colnames

# Grab inflation-indexed US Treasuries
tips.yields <- c("TIPSY02", "TIPSY05", "TIPSY10")
tips <- Quandl("FED/TIPSY", type="xts")[,tips.yields]/100

# expected inflation and CPI are only available monthly...
# For expected inflation and CPI, only get the first column of data... like so:
exinfl <- Quandl("FRBC/EXIN", type="xts")[,1]
colnames(exinfl) <- c("EXINFL")
cpi <- Quandl("FRBC/USINFL", type="xts")[,1]
colnames(cpi) <- c("CPI")

# Calculate inflation and surprise by projecting CPI for one year ahead
# The surprise is how much the CPI differed from what was
# projected a year earlier
infl.yoy <- log(cpi) - log(lag(cpi, 12))
colnames(infl.yoy) <- c("INFL.YOY")
infl.mom <- (log(cpi) - log(lag(cpi)))*12
colnames(infl.mom) <- c("INFL.MOM")
excpi <- cpi*(1+exinfl)  # expected CPI in twelve months
cpi.surprise <- log(cpi) - log(lag(excpi, 12))  # % CPI surprise
colnames(cpi.surprise) <- c("INFLSURP")

# combine the data and carry monthly observations forward
inflation.tmp <- cbind(ust, tips, infl.yoy, infl.mom, exinfl, cpi, excpi, cpi.surprise)["1999/"]
inflation.data <- na.locf(inflation.tmp)

# backward Hodrick-Prescott filter function
hpbackfilter <- function(y, lambda) {
    n <- length(y)
    I <- diag(1, nrow = n)
    # build the curvature matrix
    K <- matrix(0, nrow=n-2, ncol=n)
    for (i in 1:(n-2)) {
        K[i,i:(i+2)] = c(1,-2,1)
    }
    # now invert and multiply by the data
    hat.matrix <- solve(I+2*lambda*t(K)%*%K)
    hat.matrix %*% y
}
lambda.monthly <- 129600 # for monthly data
tau <- hpbackfilter(cpi, lambda.monthly)
