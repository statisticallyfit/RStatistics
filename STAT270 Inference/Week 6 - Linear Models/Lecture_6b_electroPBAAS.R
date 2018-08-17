LC_ISE_nitric = c(120, 77, 435, 3973, 309, 38, 30, 291)
AAS_nitric = c(190, 140, 1270, 11800, 710, 60, 40, 360)
log10_LC = log10(LC_ISE_nitric)
log10_AAS = log10(AAS_nitric) 

data = data.frame(log10_AAS=log10_AAS, log10_LC= log10_LC)

leadlog.lm <- lm(log10_LC ~ log10_AAS, data=data)
summary(leadlog.lm)

# H0: B1 = 1
# Ha: B1 != 1
b1.hat <- summary(leadlog.lm)$coef[2]; b1.hat
se = (summary(leadlog.lm))$coef[2,2]
t = (b1.hat - 1)/se; t
n = nrow(data); n
p.value = 2*pt(t, df=n-2, lower.tail=TRUE); p.value

# 95% CI for the slope
crit = qt(0.025, df=n-2) * -1; crit
lower = b1.hat - crit * se; lower
upper = b1.hat + crit * se; upper

# For a 1 unit increase in AAS, we expect ISE to increase by 0.85 units, and we are 95%
# confident that for 1 unit increase in AAS, ISE increases on average between 0.74, 0.96. 


leadlog.nointercept.lm <- lm(log10_LC ~ log10_AAS - 1, data=data)
summary(leadlog.nointercept.lm)



# ------ Matrix example: y = XB + e, B = (X^t X)^-1 (X^Ty)

x = cbind(rep(1,5), c(12, 3,4,5,6)); x
y= c(20, 6, 9, 11, 14); y

invX = inv(t(x) %*% x)
beta.hat = solve(t(x) %*% x) %*% (t(x) %*% y)
beta.hat

# fitted values
y.fit = x %*% beta.hat; y.fit

model = lm(y ~ x[,2])
model$fit


# residuals observed - fit: y - y.hat
e = y - y.fit; e
model$residuals


# Estimated standard deviation:
# SSE = Y^T Y - B^T(X^T y)
n = nrow(x); n
k = ncol(x) - 1; k
sse = t(y) %*% y - t(beta.hat) %*% (t(x) %*% y); sse
s = sqrt(sse / (n-k-1)); s

summary(model) # same residual standard error 1.573

# Variance-covariance matrix
varhat.as.matrix = matrix(rep(s^2, 2*2), nrow=2); varhat.as.matrix
varcov.mat = varhat.as.matrix * solve(t(x) %*% x); varcov.mat

a.vector.x.star = c(1,3)
y.hat.pred = a.vector.x.star %*% beta.hat; y.hat.pred
# stderror: V(aT B.hat)
stderr.mean.ci = sqrt(s^2 * (t(a.vector.x.star) %*% solve(t(x) %*% x) %*% a.vector.x.star))
stderr.mean.ci

# comparing to lm and predit
tempx = x[,2]
lm1 = lm(y ~ tempx)
summary(lm1)
predict(lm1, newdata=list(tempx=3), se.fit=TRUE)
fit = predict(lm1, newdata=list(tempx=3), se.fit=TRUE)$fit


# finding R^2, SSE and SSTotal
Syy = t(y - mean(y)) %*% (y - mean(y)); Syy
sse
R2 = 1 - sse/Syy; 
summary(lm1)
R2 # yay they match

# Prediction intervals
stderr.PI = sqrt(s^2 * (1 + t(a.vector.x.star) %*% solve(t(x) %*% x) %*% a.vector.x.star))
crit = qt(0.025, df=n-2) * -1; crit
lower = fit - crit*stderr.PI; lower
upper = fit + crit*stderr.PI; upper
stderr.PI
#check: 
predict(lm1, newdata=list(tempx=3), se.fit=TRUE, interval="pred")

