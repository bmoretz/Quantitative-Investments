## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

t5.sd <- sqrt(5/3) # scaling factor to standardize t_5 distribution
desired.sd <- 0.02
scale.true <- desired.sd/t5.sd # scaling factor to get the desired sd

# left.frac of the random numbers will come from the left distribution
# the rest will come from the right distribution
left.frac <- 3/5
n.obs <- 50
x.obs <- c(rt(n.obs*left.frac, df=5)*scale.true-0.05,
           rt(n.obs*(1-left.frac), df=5)*scale.true+0.05)

# create values of the true density
x <- seq(-0.15, 0.15, by=0.001)
true.density <- left.frac*dt((x+0.05)/scale.true,df=5)/scale.true +
    (1-left.frac)*dt((x-0.05)/scale.true, df=5)/scale.true
y.max = max(true.density)

# plot the kernel density estimator; then add the true density
plot(density(x.obs, kernel="gaussian", bw="SJ"), main="",
     xlim=c(-0.15,0.15), ylim=c(0,y.max), lwd=2, col="red")
lines(x, true.density, lty=2, lwd=2)
rug(x.obs)  # show the observations as ticks at plot bottom
