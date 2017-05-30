library(ISwR)
attach(thuesen)

# SCATTER PLOT OF DATA AND SLOPE TEST
lm.velo = lm(short.velocity~blood.glucose) # y ~ x
plot(blood.glucose, short.velocity); abline(lm.velo)
summary(lm.velo)

# RESIDUALS AND FITS
fitted(lm.velo) # the results of going through LSRL
resid(lm.velo)

# short.velocity has 1 missing observation, so plot only 
# the observations of fits that exist in short.velocity
plot(blood.glucose[!is.na(short.velocity)], fitted(lm.velo))
lines(blood.glucose[!is.na(short.velocity)], fitted(lm.velo)) # same as abline(lm.velo)

# OR use:
cc <- complete.cases(thuesen) 
#attach(thuesen[cc,])
lm.velo = lm(short.velocity~blood.glucose) # y ~ x
plot(blood.glucose, short.velocity); abline(lm.velo)

# OR use: 
#attach(thuesen)
#options(na.action=na.exclude)
lm.velo <- lm(short.velocity~blood.glucose, na.action=na.exclude)#or as arg here
fitted(lm.velo)
lines(blood.glucose[short.velocity], fitted(lm.velo))

# regression line, observations, and residual segments
plot(blood.glucose, short.velocity)
segments(blood.glucose, fitted(lm.velo), 
         blood.glucose, short.velocity)
lines(blood.glucose, fitted(lm.velo))

# residual plot: both work
plot(fitted(lm.velo), resid(lm.velo))
plot(blood.glucose, resid(lm.velo))

# check for normality of residuals
qqnorm(resid(lm.velo))
hist(resid(lm.velo))



# PREDICTION AND CONFIDENCE BANDS
" * confidence bands (narrow): uncertainty about the line itself; 
narrow if there are many observations
  * prediction bands (wide): uncertainty about future observations. 
Limits approach the true line +/- 2 standard deviations away. Assume
normally distributed errors with constant variance"

predict(lm.velo) # same as fitted(lm.velo)
predict(lm.velo, interval="confidence") #or interval="prediction
predict(lm.velo, interval="prediction")

# plotting the above: 
pred.frame <- data.frame(blood.glucose=4:20)
pp <- predict(lm.velo, interval="prediction", newdata=pred.frame)
pc <- predict(lm.velo, interval="confidence", newdata=pred.frame)
# ylim to ensure enough room for prediction limits
plot(blood.glucose, short.velocity, 
     ylim=range(short.velocity, pp, na.rm=TRUE)) 
pred.gluc <- pred.frame$blood.glucose
matlines(pred.gluc, pc, lty=c(1,2,2), col="blue")
matlines(pred.gluc, pp, lty=c(1,3,3), col="red")


# Pearson's Correlation R
# Use na.rm=TRUE for mean,var, sd and similar one-vector functions
cor(blood.glucose, short.velocity, use="complete.obs")
# all possible correlations from a data frame
cor(thuesen, use="complete.obs")

# is the correlation significantly different from 0?
cor.test(blood.glucose, short.velocity) # Pearson's R
cor.test(blood.glucose, short.velocity, method="spearman") # Spearman's p (is the correlation of the ranks)
cor.test(blood.glucose, short.velocity, method="kendall") # Kendall's Tau 
  # counts the number of concordant and discordant pairs 





# ****************** EXERCISES *********************

#1
attach(rmr)
plot(body.weight, metabolic.rate) #x,y
lm.weight <- lm(metabolic.rate~body.weight) #y~x
summary(lm.weight)
#finding fitted value at 70
811.2267 + 7.0595*(70) # or
predict(lm.weight, newdata=data.frame(body.weight=70))
#confidence interval
cor.test(body.weight, metabolic.rate) #or 
7.0595 + c(-1,1)*qt(.975,42)*0.9776 # or
confint(lm.weight, level=0.95)

# 2
attach(juul)
lm.igf1 <- lm(sqrt(igf1)~age, data=juul, subset=age>25)
summary(lm.igf1)

# 3
attach(malaria)
lm.ab <- lm(log(ab)~age)
plot(log(ab)~age, data=malaria)

# 4
rho <- 0.90; n <- 100
X <- rnorm(100)
Y <- rnorm(100, mean=rho*X, sd=sqrt(1-rho^2))
plot(X, Y)
cor.test(X, Y)
cor.test(X, Y, method="spearman")
cor.test(X, Y, method="kendall")
