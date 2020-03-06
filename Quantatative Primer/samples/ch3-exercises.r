## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

## create spread like this:
alldata$CGB1.spread <- alldata$CGB1.ask - alldata$CGB1.bid
## create fractional spreads, like so:
alldata$CGB1.fracspread <- log(alldata$CGB1.ask) - log(alldata$CGB1.bid)
