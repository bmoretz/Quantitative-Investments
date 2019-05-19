## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

pca.usd <- prcomp(~ T3M + T6M + T1Y + T2Y, data=UST,
                  scale=FALSE, center=FALSE)
pca.usd <- prcomp(yc.usd, scale=FALSE, center=FALSE)
pca.usd$rotation  # eigenvectors
pca.usd$sdev^2    # eigenvalues

