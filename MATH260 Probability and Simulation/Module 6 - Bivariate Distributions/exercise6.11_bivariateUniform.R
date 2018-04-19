
# jointXY = 1, 0 <= x <= 1, and 0 <= y <= 1
# joint bivariate uniform distribution.

n=10^5
X = runif(n=n, min=0, max=1)
Y = runif(n=n, min=0, max=1)
head(X)

probBothBetweenHalf <- sum((X <= 0.5) & (Y <= 0.5))/n
probBothBetweenHalf
