## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

## Check residual plots for patterns
model.ibm <- lm(logret.ibm ~ logret.factor1, data=bigdataset)
res.ibm <- residuals(model.ibm)
plot(model.ibm$fitted, res.ibm)  # plot residuals vs fitted values
plot(bigdataset$date, res.ibm)  # plot residuals versus date

## Test for Autocorrelations
library{lmtest}
model.ibm <- lm(logret.ibm ~ logret.factor1, data=bigdataset)
res.ibm <- residuals(model.ibm)
dwtest(res.ibm ~ 1)

## Test for Cross-Correlations
model.ibm <- lm(logret.ibm ~ logret.factor1, data=bigdataset)
res.ibm <- residuals(model.ibm)
model.aapl <- lm(logret.aapl ~ logret.factor1, data=bigdataset)
res.aapl <- residuals(model.aapl)
# Compute Pearson and Fisher tests
cor.test(res.ibm, res.aapl, method="pearson")

## Check for Influential Points
model.ibm <- lm(logret.ibm ~ logret.factor1, data=bigdataset)
res.student.ibm <- rstudent(model.ibm)
plot(res.student.ibm)

cooks.distances.ibm <- cooks.distance(model.ibm)
plot(cooks.distances.ibm)

# Compute Pena's sensitivities
p <- length(coef(model.ibm))
Q <- qr.Q(model.ibm$qr)  # the Q of the QR decomposition
H <- Q %*% t(Q)  # the hat matrix
h <- diag(H)
S <- H^2 %*% rstudent(model.ibm)^2/(p*h*(1-h))

## Check for Multicollinearity
cor.coeff <- cov2cor(vcov(model.ibm))
cor.coeff                     # coefficient correlation matrix
eigen(cor.coeff)              # compute eigenvectors, eigenvalues
det(cor.coeff)                # determinant
kappa(cor.coeff, exact=TRUE)  # condition number
