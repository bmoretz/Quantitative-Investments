## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
setwd("your/working/directory/goes/here")
# Download the NBER-CES data and save it to a working directory
# Then, read in the CSV file
mfgdata <- read.csv("naics5811.csv")

# Convert columns which should be categorical variables from text
# Also pull out NAICS sectors
mfgdata$naicsind <- as.factor(mfgdata$naics)
mfgdata$naicssector <- as.factor(substr(mfgdata$naics, 1, 2))
mfgdata$yearfactor <- as.factor(mfgdata$year)

# Initialize percentage changes for value-added and investment
# Then, go through all the industries and calculate the log-return
# (percentage change) for each industry's value-added and investment.
# Also save last period's (lagged) percentage change in value-added.
mfgdata$varet <- 0
mfgdata$invret <- 0
for (ind in levels(mfgdata$naicsind)) {
    temp <- mfgdata[mfgdata$naicsind == ind,]
    nobs <- dim(temp)[1]
    # add one into logs to guard against zeros
    varet <- c(NA, log(temp$vadd[2:nobs]+1) - log(temp$vadd[1:(nobs-1)])+1)
    invret <- c(NA, log(temp$invest[2:nobs]+1) - log(temp$invest[1:(nobs-1)])+1)
    mfgdata[mfgdata$naicsind == ind,"varet"] <- varet
    mfgdata[mfgdata$naicsind == ind,"lagvaret"] <- c(NA, varet[1:(nobs-1)])
    mfgdata[mfgdata$naicsind == ind,"invret"] <- invret
}

# Some data may have been missing. Build indices to remove
# missing data for procedures which do not handle it.
usable.idx <- !is.na(mfgdata$invret)
usablear.idx <- !is.na(mfgdata$laginvret)

# Fit the model, summarize the data, and plot residuals
simple.model <- lm(invret ~ varet, data=mfgdata, weight=cap)
summary(simple.model)
plot(mfgdata$year[usable.idx], residuals(simple.model), pch=".")
grid(col="gray")

# Do likewise for the year-slope model
yearslope.model <- lm(invret ~ varet + varet:yearfactor,
                      weight=cap, data=mfgdata)
summary(yearslope.model)
plot(mfgdata$year[usable.idx], residuals(yearslope.model), pch=".")
grid(col="gray")

# And do this again for a model with a lagged value-added component
yearslopear.model <- lm(invret ~ varet + lagvaret + varet:yearfactor,
                        data=mfgdata, weight=cap)
summary(yearslopear.model)
plot(mfgdata$year[usablear.idx], residuals(yearslopear.model), pch=".")
grid(col="gray")
