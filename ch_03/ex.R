# 1.)
# Create a spread column for each of the fixed income, commodity, equity index and FX instruments
# plot the spreads, all over the same period, and comment on trends, unsual events and commonalities

## create spread like this:
alldata$CGB1.spread <- alldata$CGB1.ask - alldata$CGB1.bid

# 2.)
# Create fractional spreads, like so:

## create fractional spreads, like so:
alldata$CGB1.fracspread <- log(alldata$CGB1.ask) - log(alldata$CGB1.bid)
# and plot these spreads as in the prior question.
# comment on trends, unsual events and commonalities

# 3.)
# Look at the summaries for the spreads and fractional spreads.
# Which seem to be better behaved: arithmetic or log-spreads? Why?

# 4.)
# Plot the short volumn and adjusted close for the equities, making sure all plots are for the same time scale.
# Do you notice any relationships between the short volume and price plots?