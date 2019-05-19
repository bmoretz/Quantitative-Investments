## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

price.tree <- function(numsteps, under.tree, opt.payoffs,
                       rf, delta.t, ups=0, downs=0) {
    if (numsteps > 0) {
        up.value <- price.tree(numsteps-1, under.tree, opt.payoffs,
                               rf, delta.t, ups+1, downs)
        down.value <- price.tree(numsteps-1, under.tree, opt.payoffs,
                                 rf, delta.t, ups, downs+1)
    } else {
        # We have reached a leaf; return the option payoff
        return(opt.payoffs[downs+1])
    }
    under.up.value <- under.tree[downs+1,ups+1+1]
    under.down.value <- under.tree[downs+1+1,ups+1]
    H <- (up.value-down.value)/(under.up.value-under.down.value)
    B <- (under.up.value*down.value - under.down.value*up.value)/
        (under.up.value - under.down.value)*exp(-rf*delta.t)
    H*under.tree[downs+1,ups+1] + B
}

rf <- 0.03  # 3% risk-free rates
sigma <- 0.45  # 45% volatility
T <- 1.25  # 15-month option
n.steps <- 16  # number of steps total
delta.t <- T/n.steps  # interpolated time step size
u <- exp(sigma*sqrt(delta.t))  # up move
d <- exp(-sigma*sqrt(delta.t))  # down move
s0 <- 100  # initial stock price
K <- 80  # strike price

## This creates a full matrix with the tree -- and steps beyond the tree
## in the lower-right triangle of the matrix.  Since this is an easy way
## to create the tree, we will just ignore the lower-right triangle.
underlier.tree <- s0*d^(0:n.steps)%*%t(u^(0:n.steps))
option.payoffs <- pmax(s0*u^(n.steps:0)*d^(0:n.steps) - K, 0)
price.tree(numsteps=n.steps, underlier.tree, option.payoffs, rf, delta.t)
