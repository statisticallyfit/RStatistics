# Chi Square test Goodness of fit
fruit <- c(32, 28, 16, 14, 10)
chisq.test(fruit) -> res
res
res$p.value
res$statistic
res$parameter
res$method
res$data.name
O <- res$observed 
E <- res$expected
res$residuals
res$stdres
# manually
chi.stat <- sum((O-E)^2/E)
pchisq(chi.stat, df=4, lower.tail=F)

# Retired executives
exec <- c(122, 85, 76, 17)
p <- c(0.38, 0.32, 0.23, 0.07)
res <- chisq.test(exec, p=p)
res$expected
qchisq(0.90, df=3)

# Firearm
fire <- c(68, 27, 5)
p <- c(0.74, 0.16, 0.10)
chisq.test(fire, p=p)
