## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(EQL)

# calculate moments to pass to Edgeworth expansion
mean.obs <- mean(x.obs)
var.obs <- var(x.obs)
skew.obs <- skewness(x.obs, method="moment")*n.obs/(n.obs-2)
exkurt.obs <- kurtosis(x.obs, method="moment")*n.obs/(n.obs-3)-3

# create the Edgeworth expansion. Since we have moments, we
# do this for a "sum" with 1 term.
edge.expand <- edgeworth(x, 1, rho3=skew.obs, rho4=exkurt.obs,
                         mu=mean.obs, sigma2=var.obs, type="sum")

# without EQL, we would have to do this:
dedge <- function(x) {
    z <- (x-mean.obs)/sqrt(var.obs)
    dnorm(z)*(1 + skew.obs*(z^3-3*z)/6 + exkurt.obs*(z^4-6*z^2+3)/24
              + skew.obs^2*(z^6-15*z^4+45*z^2-15)/72)/sqrt(var.obs)
}
