## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

## empirical
alpha <- 0.05  # probability mass in the loss tail
# sort the data; grab the lowest alpha fraction of the data
sorted.x.obs <- sort(x.obs)
frac.idx <- length(sorted.x.obs)*alpha
left.idx <- floor(frac.idx)

# VaR is linearly interpolated between points
rfrac <- 1-(frac.idx-left.idx)
VaR.emp <- sorted.x.obs[left.idx:(left.idx+1)] %*% c(1-rfrac, rfrac)

# ES is just the average of the observed data in the tail
es.emp <- mean(sorted.x.obs[1:left.idx])

## parametric
VaR.norm <- mean.obs + sqrt(var.obs)*qnorm(alpha)
es.norm <- mean.obs - sqrt(var.obs)*dnorm(qnorm(alpha))/alpha

## from kernel density estimate
# create the kernel density estimate
kde <- density(x.obs, kernel="gaussian", bw="SJ")

# now create numerical CDF
int.kde <- cumsum(kde$y)/sum(kde$y)

# Find where alpha is in loss tail
left.idx <- findInterval(alpha, int.kde)

# linearly interpolate to find VaR
rfrac <- (alpha-int.kde[left.idx])/(int.kde[left.idx+1]-int.kde[left.idx])
VaR.kde <- kde$x[left.idx:(left.idx+1)] %*% c(1-rfrac, rfrac)

# numerically integrate kernel density estimate to get ES (a bit hacky)
es.kde <- kde$x[1:left.idx] %*% kde$y[1:left.idx]/sum(kde$y)/alpha

## from Edgeworth expansion - exact
# this function returns the "CDF" of the Edgeworth expansion
int.left.edge <- function(q, mu, sigma, skew=0, exkurt=0) {
  z <- (q-mu)/sigma
  pnorm(z) - dnorm(z)*(skew*(z^2+1)/6 + exkurt*(z^3-3*z)/24
                                      + skew^2*(z^5-10*z^3+15*z)/72)
}

# returns the "conditional expectation" of the Edgeworth expansion
exp.left.edge <- function(q, mu, sigma, skew=0, exkurt=0) {
  z <- (q-mu)/sigma
  mu - sigma/alpha*dnorm(z)*(1 + skew*(z^3)/6 + exkurt*(z^4-2*z^2-1)/24
                               + skew^2*(z^6-9*z^4+9*z^2+3)/72)
}

# Find the point where the area under the left tail equals alpha
# Do this by shifting the integral down by alpha to find where it
# crosses zero (i.e. where the integral equals alpha)
int.left.edge.shifted <- function(q) {
  int.left.edge(q, mean.obs, sqrt(var.obs), skew.obs, exkurt.obs) - alpha
}
find.VaR <- uniroot(int.left.edge.shifted, check.conv = TRUE,
                    lower=-1, upper=mean.obs)

# The VaR is that point; the ES is the integral up to that point
VaR.edge.exact <-find.VaR$root
es.edge.exact <- exp.left.edge(VaR.edge.exact, mean.obs, sqrt(var.obs),
                               skew.obs, exkurt.obs)

## from Edgeworth expansion - numeric
# Alternately, just use numeric integration on the Edgeworth
# expansion points we got from the moments

# create numeric "CDF"
int.edge <- cumsum(edge.expand$approx)/sum(edge.expand$approx)

# linearly interpolate to find VaR
left.idx <- which(int.edge>alpha)[1] - 1
rfrac <- (alpha-int.edge[left.idx])/(int.edge[left.idx+1]-int.edge[left.idx])
VaR.edge.num <- edge.expand$y[left.idx:(left.idx+1)] %*% c(1-rfrac, rfrac)

# numerically integrate Edgeworth expansion to get ES
es.edge.num <- edge.expand$y[1:left.idx] %*% edge.expand$approx[1:left.idx]/
    sum(edge.expand$approx)/alpha

## from Cornish-Fisher expansion
VaR.cornfish <- VaR(x.obs, p=0.05, method="modified", mu=mean.obs, sigma=var.obs,
                    m3=skew.obs, m4=exkurt.obs)
es.cornfish <- ES(x.obs, p=0.05, method="modified", mu=mean.obs, sigma=var.obs,
                  m3=skew.obs, m4=exkurt.obs)

## from EVT
library(evir)

#evir package looks at upper tail, so flip returns
x.neg.obs <- -x.obs

# First, work with the generalized extreme value distribution
gev.model <- gev(x.neg.obs, block=23)
q.gev <- function(p) {
  -1*qgev(1-p, xi=gev.model$par.ests[["xi"]],
               mu=gev.model$par.ests[["mu"]],
               sigma=gev.model$par.ests[["sigma"]])
}
VaR.gev <- q.gev(alpha)  # use evir function
# a bit hacky: numerically integrate CDF to get ES
es.gev <- mean(q.gev(seq(0.001, alpha, 0.001)))

# Next, work with the generalized Pareto distribution
gpd.model <- gpd(x.neg.obs, nextremes=5)
riskmeasures(gpd.model, c(0.95))

## comparison
# now create the exact CDF
t.df <- 5
p.t <- function(q) {
  z1 <- (q+0.05)/desired.sd*t5.sd
  z2 <- (q-0.05)/desired.sd*t5.sd
  pt(z1, df=5)*3/5 + pt(z2, df=5)*2/5
}

# shift CDF down to find the point where alpha is in the loss tail
p.t.quantile <- uniroot(function(q) p.t(q)-alpha, check.conv=TRUE,
                        lower=-1, upper=mean.obs)
VaR.true <- p.t.quantile$root

# function finds ES for a t-distribution to left of quantile q
es.t <- function(q) {
  -dt(qt(q, df=t.df), df=t.df)/q*(t.df-2+qt(q, df=t.df)^2)/(t.df-1)
}

# ES calculation is crusty: we found point for alpha in loss tail.
# Now find ES for both t-distributions; then find average of them
q1 <- pt((VaR.true+0.05)/desired.sd*t5.sd, df=t.df)
q2 <- pt((VaR.true-0.05)/desired.sd*t5.sd, df=t.df)
es.true <- (3/5*es.t(q1) + 2/5*es.t(q2))*desired.sd/t5.sd - 0.01
