## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

principal.start <- 30e6  # pool principal
n.peryr <- 12  # monthly = 12 payments per year
T <- 30   # 30-year mortgage
r <- 0.0625  # 6.25%
t <- 1:(n.peryr*T)

## calculate level monthly payment
pmt.level <- principal.start*r/(n.peryr*(1-1/(1+r/n.peryr)^(n.peryr*T)))

## 100% PSA prepayment: rate up to 6% annual at 30 months
psa.speed <- 100  # 100% PSA
max.rate <- psa.speed/100*0.06
seasoned.month <- 30  # at 30 months, PSA says mortgages are seasoned
prepay.rate <- c(seq(max.rate/seasoned.month, max.rate, max.rate/seasoned.month),
                 rep(max.rate, n.peryr*T-seasoned.month))
prepay.monthly.survival <- (1-prepay.rate)^(1/n.peryr)
prepay.monthly.rate <- 1-prepay.monthly.survival

## In production code, this would be more elegant; however,
## a for loop shows the intuition better than other approaches
num.pmts <- n.peryr*T
principal.left <- c(principal.start, rep(0,num.pmts-1))
pmt.perperiod <- c(pmt.level, rep(0,num.pmts-1))
pmt.interest <- rep(0, num.pmts)
pmt.principal <- rep(0, num.pmts)
pmt.prepays <- rep(0, num.pmts)
for (i in 1:num.pmts) {
    pmt.interest[i] <- principal.left[i]*r/n.peryr
    pmt.principal[i] <- pmt.perperiod[i] - pmt.interest[i]
    if (pmt.principal[i] > principal.left[i]) {
        ## do not repay more than principal remaining
        pmt.principal[i] <- principal.left[i]
        ## stop the loop; pool is paid off!
        break
    } else {
        ## do not prepay more than principal remaining after normal payment
        pmt.prepays[i] <- (principal.left[i]-pmt.principal[i])*prepay.monthly.rate[i]
        if ((pmt.principal[i] + pmt.prepays[i]) > principal.left[i]) {
            pmt.prepays[i] <- principal.left[i] - pmt.principal[i]
            ## stop the loop; pool is paid off!
            break
        }
    }
    if (i < num.pmts) {
        principal.left[i+1] <- principal.left[i] - pmt.principal[i] - pmt.prepays[i]
        pmt.perperiod[i+1] <- pmt.perperiod[i]*prepay.monthly.survival[i]
    }
}

areaplot(t,data.frame(pmt.interest, pmt.principal, pmt.prepays),
         ylim=c(0,pmt.level+max(pmt.prepays)), col=c("red","blue","orange"),
         border=FALSE)
