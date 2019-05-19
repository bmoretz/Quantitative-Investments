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

## tranche info
attach.pts <- list(a=0.22, b=0.14, c=0.05, z=0)
principal <- list(a=24e6,b=3e6,c=3e6,z=0)
interestbase <- list(a=(1-attach.pts$a)*principal.start,
                     b=(attach.pts$a-attach.pts$b)*principal.start,
                     c=(attach.pts$b-attach.pts$c)*principal.start,
                     z=(attach.pts$c-attach.pts$z)*principal.start)

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
### Use fullword.variables for pool payments...
num.pmts <- n.peryr*T
principal.left <- c(principal.start, rep(0,num.pmts-1))
pmt.perperiod <- c(pmt.level, rep(0,num.pmts-1))
pmt.interest <- rep(0, num.pmts)
pmt.principal <- rep(0, num.pmts)
pmt.prepays <- rep(0, num.pmts)
### ... and use abbr.vars (abbreviated) for tranche payments
pmt.prin.a <- pmt.prin.b <- pmt.prin.c <- rep(0, num.pmts)
pmt.int.a <- pmt.int.b <- pmt.int.c <- pmt.int.z <- rep(0, num.pmts)
stop.loop <- FALSE
for (i in 1:num.pmts) {
    ## determine pool cashflows
    pmt.interest[i] <- principal.left[i]*r/n.peryr
    pmt.principal[i] <- pmt.perperiod[i] - pmt.interest[i]
    if (pmt.principal[i] > principal.left[i]) {
        ## do not repay more than principal remaining
        pmt.principal[i] <- principal.left[i]
        ## stop the loop; pool is paid off!
        stop.loop <- TRUE
    } else {
        ## do not prepay more than principal remaining after normal payment
        pmt.prepays[i] <- (principal.left[i]-pmt.principal[i])*prepay.monthly.rate[i]
        if ((pmt.principal[i] + pmt.prepays[i]) > principal.left[i]) {
            pmt.prepays[i] <- principal.left[i] - pmt.principal[i]
            ## stop the loop; pool is paid off!
            stop.loop <- TRUE
        }
    }
    ## allocate payments to tranches
    prin.to.allocate <- pmt.principal[i]+pmt.prepays[i]
    prinleft.a <- principal$a - sum(pmt.prin.a)
    prinleft.b <- principal$b - sum(pmt.prin.b)
    prinleft.c <- principal$c - sum(pmt.prin.c)
    ## Scale interest payments by fraction not repaid*interestbase
    ## Remaining interest will get allocated to principal repayment
    ## and then deferrred interest on the z tranche
    pmt.int.a[i] <- prinleft.a/principal$a*interestbase$a*r/n.peryr
    pmt.int.b[i] <- prinleft.b/principal$b*interestbase$b*r/n.peryr
    pmt.int.c[i] <- prinleft.c/principal$c*interestbase$c*r/n.peryr
    prin.to.allocate <- prin.to.allocate + pmt.interest[i] -
        pmt.int.a[i] - pmt.int.b[i] - pmt.int.c[i]
    ## A is first in line for principal (Andropov is the name to say)
    if (prinleft.a > 0) {
        pmt.prin.a[i] <- min(prinleft.a, prin.to.allocate)
        prin.to.allocate <- prin.to.allocate - pmt.prin.a[i]
    }
    ## The letter B is next in line (for principal, Brezhnev's dead so he'll do just fine)
    if (prinleft.b > 0 & prin.to.allocate > 0) {
        pmt.prin.b[i] <- min(prinleft.b, prin.to.allocate)
        prin.to.allocate <- prin.to.allocate - pmt.prin.b[i]
    }
    ## The letter C is next for principal (C is for Chernenko)
    if (prinleft.c > 0 & prin.to.allocate > 0) {
        pmt.prin.c[i] <- min(prinleft.c, prin.to.allocate)
        prin.to.allocate <- prin.to.allocate - pmt.prin.c[i]
    }
    ## Finally, if any cashflows are left, they go to the Z tranche
    if (prin.to.allocate > 0)
        pmt.int.z[i] <- prin.to.allocate

    ## prepare for next iteration of loop
    if (i < num.pmts) {
        principal.left[i+1] <- principal.left[i] - pmt.principal[i] - pmt.prepays[i]
        pmt.perperiod[i+1] <- pmt.perperiod[i]*prepay.monthly.survival[i]
    }
    
    if (stop.loop)
        break
}

areaplot(t,data.frame(pmt.int.c,pmt.int.b,pmt.int.a,
                      pmt.prin.a,pmt.prin.b,pmt.prin.c,pmt.int.z),
         col=c("green","yellow","lightblue","blue","orange","darkgreen","red"),
         ylab="", border=FALSE)
text(100, 150000, "A", cex=2, col="white")
text(210, 60000, "B", cex=2)
text(255, 40000, "C", cex=2, col="white")
text(310, 20000, "Z", cex=2)
