# MODEL: Y = B0 + B1x1 + B2x2 + B3x3 + B4x4 + e

library(gdata)
yieldData <- read.xls("example1174data.xlsx", header=TRUE)

yield.lm <- lm(y ~ x1 + x2 + x3 + x4, data=yieldData)
summary(yield.lm)

# Solving as matrix
X = cbind(rep(1,16), yieldData$x1, yieldData$x2, yieldData$x3, yieldData$x4)
X
Y = yieldData$y
betahat = solve(t(X) %*% X) %*% (t(X) %*% Y); betahat
yield.lm$coef # check

SSE.C = t(Y) %*% Y - t(betahat) %*% (t(X) %*% Y); SSE.C
anova(yield.lm)


# Nested F-test: H0: B2 = B3 = B4 = 0
summary(yield.lm)

X.reduced = cbind(rep(1,16), yieldData$x1)
betahat.reduced = solve(t(X.reduced) %*% X.reduced) %*% (t(X.reduced) %*% Y); betahat.reduced

SSE.R = t(Y) %*% Y - t(betahat.reduced) %*% (t(X.reduced) %*% Y); SSE.R

yield.reduced.lm = lm(y ~ x1, data=yieldData) # reduced model
anova(yield.reduced.lm)

# F-test
k = 4
g = 1
n = nrow(yieldData)
Fstat = ((SSE.R - SSE.C) / (k-g)) / (SSE.C / (n-k-1)); Fstat
p.value = pf(Fstat, df1=k-g, df2=n-k-1, lower.tail=F) # F-test is always one-sided.
p.value # cannot reject the null, seems no evidence that complete model is better. 

anova(yield.reduced.lm, yield.lm)
# yay same F-statistic