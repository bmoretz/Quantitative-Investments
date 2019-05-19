## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

## No prepayments
principal <- 300000  # mortgage principal
n.peryr <- 12  # monthly = 12 payments per year
T <- 30   # 30-year mortgage
r <- 0.0625  # 6.25%
t <- 1:(n.peryr*T)
pmt.level <- principal*r/(n.peryr*(1-1/(1+r/n.peryr)^(n.peryr*T)))
pmt.principal <- pmt.level/(1+r/n.peryr)^(n.peryr*T-t+1)
pmt.interest <- pmt.level-pmt.principal

areaplot(t, data.frame(pmt.interest, pmt.principal), col=c("red","blue"),
         ylim=c(0,pmt.level), border=FALSE)

## With prepayments
principal.start <- 300e3  # mortgage principal
n.peryr <- 12  # monthly = 12 payments per year
T <- 30   # 30-year mortgage
r <- 0.0625  # 6.25%
t <- 1:(n.peryr*T)
## calculate level monthly payment
pmt.level <- principal.start*r/(n.peryr*(1-1/(1+r/n.peryr)^(n.peryr*T)))

## Prepay an extra 5\% each payment
pmt.prepay.plan <- 0.05*pmt.level

## In production code, this would be more elegant; however,
## a for loop shows the intuition better than other approaches
num.pmts <- n.peryr*T
principal.left <- c(principal.start, rep(0,num.pmts-1))
pmt.interest <- pmt.principal <- pmt.prepays <- rep(0, num.pmts)
for (i in 1:num.pmts) {
    pmt.interest[i] <- principal.left[i]*r/n.peryr
    pmt.principal[i] <- pmt.level - pmt.interest[i]
    if (pmt.principal[i] > principal.left[i]) {
        ## do not repay more than principal remaining
        pmt.principal[i] <- principal.left[i]
        ## stop the loop; mortgage is paid off
        break
    } else {
        ## do not prepay more than principal remaining after normal payment
        pmt.prepays[i] <- pmt.prepay.plan
        if ((pmt.principal[i] + pmt.prepays[i]) > principal.left[i]) {
            pmt.prepays[i] <- principal.left[i] - pmt.principal[i]
            ## stop the loop; mortgage is paid off
            break
        }
    }
    if (i < num.pmts) {
        principal.left[i+1] <- principal.left[i] - pmt.principal[i] - pmt.prepays[i]
    }
}

areaplot(t,data.frame(pmt.interest, pmt.principal, pmt.prepays),
         ylim=c(0,pmt.level+max(pmt.prepays)), col=c("red","blue","orange"),
         border=FALSE)
