# EXAMPLE
getwd()
setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/StatisticsIntroUsingR_Crawley")
reg.data <- read.csv("data/tannin.csv")
attach(reg.data)
reg.data
plot(tannin, growth, pch=19, col="cyan")
lm(growth~tannin)
abline(lm(growth~tannin, col="dodgerblue"))

fitted <- predict(lm(growth~tannin))
fitted
lines(c(0,0), c(12, fitted[1]))
for(i in 1:length(fitted))
  lines(c(tannin[i], tannin[i]), c(growth[i], fitted[i]), col="red")



# (1)

# Finding values from model (the lm thingy) by hand: 


# Looping through values to extract best estimate of b
b <- seq(-1.43, -1, 0.002)
sse <- numeric(length(b)) # sum of squared residuals
for(i in 1: length(b)){
  a <- mean(growth) - b[i]*mean(tannin)  
  residual <- growth - a - b[i]*tannin
  sse[i] <- sum(residual^2)
}
plot(b, sse, type="l", ylim=c(19, 24)) 
      arrows(-1.216, 20.07225, -1.216, 19, col="red")
      abline(h=20.07225, col="green", lty=2)
      lines(b, sse)
b[which(sse==min(sse))]

# Doing calculation of b by hand

# b = SSXY/SSX
# a = meany - b*meanx
# SSR = SSXY^2/SSX
# r = SSXY/sqrt(SSX*SSY)

n = length(tannin)
sumXY = sum(tannin*growth)
sumXsumY = sum(tannin)*sum(growth)
sumX2_individ = sum(tannin^2)
sumX2_all = (sum(tannin))^2
SSXY = (sumXY - sumXsumY/n)
SSX = (sumX2_individ - sumX2_all/n)
b = SSXY/SSX
a = mean(growth) - b*mean(tannin)

# ANOVA to find standard errors of slope and intercept
# SSY = SSE + SSR
# SSR = b*SSXY
SSY = (sum(growth^2) - (sum(growth))^2/n)
SSR = b*SSXY # explained variation
SSE = SSY - SSR # variation that is unexplained by the LSRL
# df of SSY = n-1 = 8
# df of SSE = n-2 = 7
# df of SSR = 1 
varY = SSY/8
varE = SSE/7
varR = SSR/1
# F ratios - is varR significantly greater than varE? (is there much more explained variance than unexplained variance?)
f.ratio <- varR/varE
f.ratio
1-pf(f.ratio, df1=1, df2=7) # yes

# Standard error of slope (b)
# SEb = sqrt(varE/SSX), where varE = SSE/(n-2)
# NOTE: when variance of x values (SSX) increases, the data is more like a line + there is more clarity, so error in slope is reduced
SEb = sqrt(varE/SSX)
SEb

# Standard error of intercept (a)
# SEa = sqrt(varE * sum(x^2)/ (n*SSX))
# NOTE: uncertainty in intercept increases with sum of squared xs because the farther the mean of xs get from the intercept, the more uncertainty there is in the intercept. 
SEa = sqrt(varE * sum(tannin^2)/(n*SSX))
SEa

# Standard error of predicted y value (y-hat)
# SEpred = sqrt(varE * (1/n + (x - mean(x))^2/SSX))
SEpred = sqrt(varE * (1/n + sum((tannin - mean(tannin))^2)/SSX))
SEpred





# Finding the values for model (the lm thing) the easy way: 
model <- lm(growth~tannin)
model
summary(model)
summary.aov(model) # just the ANOVA table...



# (2)

# Measuring degree of fit
# r squared = SSR/SSY
r = sqrt(SSR/SSY)
r
cor(tannin, growth)


 

# (3)

# Model Checking: constancy of variance and normal errors
par(mfrow=c(1,1))
plot(model) # residualsvs fitted, normal QQ, scale-location, residuals vs leverage
# scale location graph: sqrt of standardized residuals vs fitted values. Bad if is triangle shape and if scatter increases as fits increase
# standardized residuals vs leverage: measures infuential points that most affect parameter estimates
influence.measures(model)




# (4)

# Transformations
par(mfrow=c(1,1))
decay <- read.csv("data/decay.csv")
attach(decay)
decay
model <- lm(amount~time)
plot(time, amount, pch=19, col="dodgerblue")
abline(model, lwd=2, col="red")

summary = summary(model)
summary
plot(time, summary$residuals, pch=19, col="green")

# Linearize the equation y = ae^(-bx) to transform the data to estimate parameters of the linear model
# y-hat = log(y) = log(a) - bx
plot(time, log(amount), pch=19, col="dodgerblue")
log.model <- lm(log(amount)~time)
abline(log.model, col="red")
# problem: non constant variance
log.summary <- summary(log.model)
log.summary
a.exp.model = log.summary$coefficients[1] # y intercept
b.exp.model = log.summary$coefficients[2] # slope
SEa.exp.model <- log.summary$coefficients[1,][2]
SEa.exp.model
upper <- a.exp.model + SEa.exp.model
lower <- a.exp.model - SEa.exp.model
exp(upper)
exp(lower)
exp(a.exp.model)
plot(log.model)

# Draw exponent curve with data
par(mfrow=c(1,1))
plot(time, amount, pch=19, col="dodgerblue")
x.exp <- seq(0, 30, by=0.25)
y.exp <- exp(a.exp.model)*exp(b.exp.model*x.exp)
lines(x.exp, y.exp, col="red", lwd=2)



# Polynomial Regression
par(mfrow=c(2,2))
curve(-(x^2)/10 + 2*x + 4, xlim=c(0,10), col="red", ylab="y")
curve(-x^2/5 + 2*x + 4, xlim=c(0,10), col="red", ylab="y")
curve(3*x^2/10 - 4*x + 12, xlim=c(0, 10), col="red", ylab="y")
curve(x^2/10 + x/2 + 4, xlim=c(0, 10), col="red", ylab="y")

linear.model <- lm(amount ~ time)
quad.model <- lm(amount ~ time + I(time^2)) # I means "as is" and in the model formula, ^ means order of interaction terms to be fitted
quad.summary <- summary(quad.model)
c = quad.summary$coefficients[1]
b = quad.summary$coefficients[2]
a = quad.summary$coefficients[3]

# Draw quadratic curve with data
par(mfrow=c(1,1))
plot(time, amount, pch=19, col="dodgerblue")
x.quad <- seq(0, 30, by=0.25)
y.quad <- a*x.quad^2 + b*x.quad + c
lines(x.quad, y.quad, col="red", lwd=2)

# To compare goodness of fits between the linear and quadratic models, use AIC or anova
AIC(linear.model, quad.model) # the lower, the better fit
anova(linear.model, quad.model)
detach(decay)



# Non Linear Regression: means relationship cannot be linearized
deer <- read.csv("data/jaws.csv")
attach(deer)
deer

# FOrmula for data: y = a - be^(-cx)
par(mfrow=c(1,1))
plot(age, bone, pch=20, col="plum")
# technical detail for R function to work: must estimate values of a and b first
# when x = 0, y = a - b. When x = oo, y = a
# when x = 0, y ~ 10. When x = oo, y ~ 120 ==> 10 = 120-b ==> b = 110
# use coordinates (5, 40) to find c = 0.064
list(a=120, b=110, c=0.064)
nonlinear.model <- nls(bone ~ a - b*exp(-c*age), 
                       start=list(a=120, b=110, c=0.064))
nonlinear.model
summary(nonlinear.model)
# conclusion: model is not good enough since a and b need to differ by more than 2 standard errors to be significantly different (why need to be significantly different, and which standard error - the SEa or SEb?)


# Second model: y = a(1-e^(-cx))
nonlinear.model2 <- nls(bone ~ a*(1-exp(-c*age)), start=list(a=120, c=0.064))
nonlinear.model2
nonlinear.summary <- summary(nonlinear.model2)
a = nonlinear.summary$coefficients[1]
c = nonlinear.summary$coefficients[2]
anova(nonlinear.model, nonlinear.model2) # why does conclusion say model 2 is better since p=value is large... ?
### NOT WORKING!!!
### av <- seq(0, 50, by=0.011)
### bv <- predict(nonlinear.model2, list=(age=av)) # generate predicted bone lengths and use list to assign av to age
### lines(av, bv, col="blue")
x.nonlin <- av
y.nonlin <- a*(1-exp(-c*x.nonlin))
lines(x.nonlin, y.nonlin, col="blue", lwd=2)

nonlinear.summary

# How to find R squared:
# 1) first find SSY with a null model, estimating only the intercept
# 2) residual standard error = sqrt(SSE/(n-2))
# 3) find (SSY-SSE)/SST
null.model <- lm(bone ~ 1)
summary.aov(null.model)
# sum of squares SSY = 59008
SSY <- 59008 # 1
SSE <- 13.1^2*52 # 2
Rsquared <- 100*(SSY-SSE)/SSY # or over SST because SST = SSY
Rsquared

detach(deer)




# Generalized Additive Models: usedful when y and x are nonlinear but we have no model for them
library(mgcv)
library(nlme)
setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/StatisticsIntroUsingR_Crawley")
hump <- read.csv("data/hump.csv")
attach(hump)
hump

# Fit the generalized additive model as a smothed function of x (s(x))
model <- gam(y ~ s(x))
model

plot(model, col="blue")
points(x,   y - mean(y),  pch=20, col="red")

summary(model) #significant slope of s(x)
mean(y)

summary(lm(y ~ x)) # non significant slope of x

detach(hump)


# Influence
x <- c(2,3,3,3,4)
y <- c(2,3,2,1,2)
windows(7,7)
par(mfrow=c(1,1))
plot(x, y, xlim=c(0, 8), ylim=c(0, 8)) # no relation

# add an outlier at (7,6)
x <- c(x, 7)
y <- c(y, 6)
plot(x, y, xlim=c(0, 8), ylim=c(0, 8)) # no relation
abline(lm(y ~ x), col="blue", lwd=3)

# Measure of leverage: hi = 1/n + (x - x-bar)^2/(sum(x - x-bar)^2), denominator = SSX
# a point is highly influential if hi > 2p/n, where p = number of parameters in the model
hi <- 1/length(x) + (x - mean(x))^2/(sum((x - mean(x))^2))
hi
hi.limit = 2*1/length(x) # is this correct??

model <- lm(y ~ x)
influence.measures(model)
