## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(xts)

## get put/call ratio data from CBOE
## skip the first two lines
index.pcr.file <- "http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/indexpc.csv"
putcall.tmp <- read.csv(index.pcr.file, skip=2)
putcall <- xts(putcall.tmp$P.C.Ratio,
               order.by=as.POSIXct(putcall.tmp$DATE, format="%m/%d/%Y"))
colnames(putcall) <- "ratio"
putcall$change <- diff(putcall$ratio)
putcall$dayofweek <- .indexwday(putcall)
putcall$dayofmonth <- day(putcall)
putcall$monday <- putcall$dayofweek == 1
putcall$friday <- putcall$dayofweek == 5
putcall$monthstart <- putcall$dayofmonth <= 3
putcall$monthend <- putcall$dayofmonth >= 28
