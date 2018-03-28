
p = 1/2
n = 21
improve <- seq(1, 21, by=1)

Px <- dbinom(improve, size=n, prob=p)
Px[11] # in chart, is 0.12730
Px[12] # in chart is 12
Px[7] # in chart is 0.650

# P(X > x) (so is calculating P(X >= x + 1))
PGTx <- pbinom(improve, size=n, prob=p, lower.tail = FALSE)
# is calculating 1 - P(X <= x) = P(X > x) = P(X >= x + 1)
PGTx.2 <- 1 - pbinom(improve, size=n, prob=p, lower.tail=TRUE) # is same as above
PGTx - PGTx.2 # errors are very small, almost zero so they are the same

PLTx <- 1 - PGTx

results <- cbind(improve, Px, PLTx, PGTx)
dimnames(results) <- list(NULL, c("x", "P(X = x)", "P(X <= x)", "P(X > x)"))
head(results, 10)
