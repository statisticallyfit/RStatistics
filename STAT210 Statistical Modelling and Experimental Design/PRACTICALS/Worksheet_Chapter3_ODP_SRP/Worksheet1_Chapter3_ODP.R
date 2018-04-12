setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PRACTICALS/Worksheet_Chapter3_ODP_SRP/")

options(digits=10, show.signif.stars = FALSE)


dat1 <- read.table("worksheet1_data.txt", header=TRUE)
head(dat1)
attach(dat1)

xy.lm <- lm(ODP ~ SRP, data=dat1)

# Plot data and fitted line
plot(ODP ~ SRP, data=dat1, ylab="oxygen demand percent", xlab="solids reduction percent")
abline(xy.lm)
# Question 1: association seems strong, positive linear relation between x and y. 



# Step 2 - summary
summary(xy.lm)
anova(xy.lm)
# CI for regression coefs
betaCI(xy.lm)

# Question 2: line of fit: y-hat = 3.8296 + 0.9036x
# Question 3: t-test is significant and F-test is also , so yes SRP seems a good
# predictor of ODP
# Question 4: cor coef is
cor(ODP, SRP) # strong, positive linear relation
# note: positive since slope of B1 > 0
# Question 5: R^2: 
cor(ODP, SRP)^2
# About 91.3% of the sample variation in ODP is explained by the least squares line
# with SRP as predictor. 
# Question 6: signma squared estimate is s^2
3.23^2 # residual standard error on 31 degrees freedom
# also from sse/n-2 calculation
anv = anova(xy.lm)
anv$`Sum Sq`[2] / (nrow(dat1) - 2)

# Question 7 - interpretation of B1 confint
betaCI(xy.lm)
# We are 95% confident that for every 1-% increase in SRP, the average ODP increases
# by between 0.801 % and 1.01%. 

# INTERPRET SLOPE
# For a 1% increase in SRP, ODP will increase by 0.9%. 



# Step 3 - check model assumptions
par(mfrow=c(1,2))
plot(xy.lm, which=1:2, add.smooth=F)

library(ggplot2)
library(ggfortify)
autoplot(xy.lm)

# shaprio wilk normal residuals?
shapiro.test(xy.lm$residuals) # no evidence to reject null of normality. 

# Question 8 - assumptions of linear model: 
# 1. expectation of residuals E(ei | xi) = 0
# 2. variance of residuals is same for each xi: Var(ei | xi) = sigma^2 homoskedascity
# 3. residuals are normal with above mean and variance. 
# 4. errors associated with any two observations (yi, yj) are independent. So error
# from one y has no effect on error associated with other y-value (observed). 

# Question 9 - residuals appear normal (plot 2) and appear to have no pattern when
# compared to fitted y values (plot 1) so variance seems the same for all errors. 
# Observations 3 and 11 and 23 are potential outliers since they are
# outside the 2 standard dev. limit. 

# Question 10 - p-value = 0.10, not sig. at alpha=0.05 so no evidence to reject 
# the null hypothesis of normality

# Question 11 - reliability of SLR model based on CI and PI



# Step 4: Predicting
pred.df <- data.frame(SRP=c(10,20,30,40,50,60,70,80))
CI <- data.frame(predict(xy.lm, interval="confidence", newdata=pred.df))
PI <- data.frame(predict(xy.lm, interval="predict", newdata=pred.df))
cbind(pred.df$SRP, CI, PI)

# Plotting confidence bands
par(mfrow=c(1,1))
plot(pred.df$SRP, CI$fit, type="b", pch=16, xlab="SRP", ylab="mean ODP", 
     main="Scatterplot of ODP ~ SRP, with predicted values and 95% 
     confidence and prediction abnds")
points(dat1$ODP, dat1$SRP)

legend(10, 50, lty=c(1, 2, 3), legend=c("Line of best fit", 
                                        "95% Confidence Bands",
                                        "95% Prediction Bands"))
legend(10, 42, pch=c(1,16), legend=c("observed values", "predicted values"))

# lwr CI
lines(pred.df$SRP, CI[,2], lty=2) # lty = linetype, lty = 2 is dashed
# upr CI
lines(pred.df$SRP, CI[,3], lty=2)
# lwr PI
lines(pred.df$SRP, PI[,2], lty=3) # pred intervals always wider than CIs
# upr PI
lines(pred.df$SRP, PI[,3], lty=3)
