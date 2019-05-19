## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

# The fields in an 11Ac1-5 execution quality report
field.names <- c("Participant", "MktCenter", "CCYYMM", "Symbol", "Immediacy",
                 "Size", "NumCoveredOrds", "ShrsCoveredOrds", "CxldShrsCoveredOrds",
                 "FillShrsCoveredOrds", "AwayShrsCoveredOrds", "Shrs0to9s",
                 "Shrs10to29s", "Shrs30to59s", "Shrs60to299s", "Shrs5to30m",
                 "AvgRealizedSpread", "AvgEffectiveSpread", "ShrsPriceImprove",
                 "ShrWtdPriceImproved", "ShrWtdPriceImproveTime",
                 "ShrsFilledAtQuote", "ShrWtdAtQuoteTime", "ShrsFilledOutsideQuote",
                 "ShrWtdOutsideQuotePriceDiff", "ShrWtdOutsideQuoteTime")

# These give the meaning of coded fields in the report
field.participants <- list(A="Amex", B="BSE", M="CHX", C="CSE", T="NASD",
                           N="NYSE", P="PCX", X="Phlx")

field.sizes <- list("21"="100-499", "22"="500-1999", "23"="2000-4999", "24"="5000+")
field.immediacy <- list("11"="Market", "12"="Marketable limit", "13"="Inside limit",
                        "14"="At-quote limit", "15"="Near-quote limit")

files2grab <- c("m200204.zip", "M200604.ZIP", "M201004.ZIP", "M201404.ZIP", "M201804.ZIP")

for (zipfile in files2grab) {
  temp <- tempfile()
  download.file(paste("ftp://doe.chx.com/", zipfile, sep=""), temp)
  file.names <- unzip(temp, list=TRUE)
  exdata <- read.csv(unz(temp, file.names[1,"Name"]), header=FALSE, sep="|")
  colnames(exdata) <- field.names
  unlink(temp)

  # do some calculations across subgroups and store results
  # You can subset the data like so:
  marketableorders.idx <- exdata$Immediacy == 11 | exdata$Immediacy == 12
  mean(exdata$AvgRealizedSpread[marketableorders.idx])
}
