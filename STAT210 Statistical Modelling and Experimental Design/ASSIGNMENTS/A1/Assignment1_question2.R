setwd('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A1/')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')

options(digits = 3, show.signif.stars = FALSE)

insectData <- read.table("insect.txt", header=TRUE)
head(insectData)


# part a) and b) 
attach(insectData)
par(mfrow=c(1,1))
plot(Count ~ Ispray)

# Interpretation: 
# For insect sprays A, B, and F, the insect count seems to be higher (not as effective)
# compared to sprays C, D, and E, which have lower median values. 

# The median values in insect count for sprays A, B, and F are 14, 16.5, and 15 
# insects, respectively. 
median(insectData$Count[which(insectData$Ispray == "A")])
median(insectData$Count[which(insectData$Ispray == "B")])
median(insectData$Count[which(insectData$Ispray == "F")])
#The medians for sprays C, D, and E are 1.5, 5, and 3 insects,
# respectively. 
median(insectData$Count[which(insectData$Ispray == "C")])
median(insectData$Count[which(insectData$Ispray == "D")])
median(insectData$Count[which(insectData$Ispray == "E")])
# However sprays C, and D seem to have one outlier each - there is one
# instance per spray when the insect count was much higher than all the rest. 
# The potential outlier insect count for spray C is 7, since that is the maximum
# value, and the value is 12 insects for spray D. 
max(insectData$Count[which(insectData$Ispray == "C")])
max(insectData$Count[which(insectData$Ispray == "D")])



# part c) 1) 
# (i) 
insect.lm <- lm(Count ~ Ispray, data=insectData)
# The summary table says all mean differences in insecticide sprays are 
# significant, except for the mean difference between spray A and spray F, and
# also the mean difference between spray A and spray B. The A-F difference 
# p-value = 0.18 > alpha=0.05
# so we cannot reject the null hypothesis and thus must say there is not 
# enough evidence to suggest a significant mean difference between spray A and F. 
# Similarly, the p=value for mean difference of spray A and B is 0.60
# so this difference is not significant. 

##### However, the mean spray A is significant since the intercept has p-value
# of 2*10^-16 = 0 so there is a very low random chance that such an effect
# in insect count from spray A could happen. This means spray A is a significant
# influencer of insect count. 

# Overall, the model seems statistically useful for predicting insect Count
# since the global F-test yields p-value < 2e-16 < alpha=0.05 so we can reject
# the null hypothesis that all population mean differences insect counts from
# particular sprays are zero and conclude that
# at least one population mean difference is not zero. 
summary(insect.lm)


# (ii) Model assumptions
par(mfrow=c(1,2))
plot(insect.lm, which=1:2)

# Assumptions are NOT satisfied. 

# 1. Constant Variance of Errors:
# Residuals vs. fitted plot: as fitted values increase, the residuals just
# explode in a fan-shape. Residuals do not stay centered around the y-hat = 0 line.
# This is a sign of non-constant variance in errors
# for the x-values. This means the error variance is different for all the predictors
# and all their respective values. 

# 2. Normality of Errors: 
# Normal QQ-plot: the middle area of the standardized residuals is closely
# hugging the straight normal line but the two top and bottom ends are leaping
# off the normal line, which suggests that a large part of the sample variation
# in insect count is not fit by the model. This is verified by the R^2 adjusted = 0.704
# which shows only 70% of variation in count is explained by the linear model. This
# leaves some leftover pattern in the residuals, shown by the non-normal qq plot. 
summary(insect.lm)$adj.r.squared

# Also observations 8, 69 and 70 seem to be outliers since they are beyond
# the -2, 2 range in the normal QQ plot, where 95% of the data should be in a 
# normal distribution. 

shapiro.test(insect.lm$residuals)
# This test says the p-value = 0.02 < alpha=0.05 so there is enough evidence
# to reject the null hypothesis that residuals are normal. We conclude residuals
# are not normal. 

### 3. Independence of errors:  we cannot check this graphically, but we can
# ask ourselves if the experimental design promoted independent observations
# of insect count values. We might assume that observed insect counts per sprays
# are independent for each spray, since each spray acts separately on a group of insects.



# part c) 2) 
# (i)
insect.sqrt.lm <- lm(sqrt(Count) ~ Ispray, data=insectData)
summary(insect.sqrt.lm)


# (ii) diagnostics
par(mfrow=c(1,2))
plot(insect.sqrt.lm, which=1:2)

# 1. Constant Error Variance
# The residuals vs. fitted plot now seems to have much more constant variance for
# each predictor and its values since as fitted values increase, residuals
# seem to stay relatively centered around the y-hat = 0 line. 

# 2. Normality of errors: 
# The normal qq-plot shows the standardized residuals are very close to the 
# normal line, so residuals look normal. 

# But observations 26, 27, and 39 seem to be outliers since they are beyond
# the -2, 2 range in the normal QQ plot, where 95% of the data should be in a 
# normal distribution. 

shapiro.test(insect.sqrt.lm$residuals)
# The Shapiro-Wilk Test yields a p-value = 0.70 > alpha=0.05 so there is no 
# evidence to reject the null hypothesis that population errors are normal. 
# The residuals do not deviate significantly from normality. 

### 3. Independence of errors:  we cannot check this graphically, but we can
# ask ourselves if the experimental design promoted independent observations
# of insect count values. We might assume that observed insect counts per sprays
# are independent for each spray, since each spray acts separately on a group of insects.




# part iii)
# For model 2, the normal qq plot shows that the residuals do not deviate
# significantly from normality, and the residuals/fitted plot shows
# that residual standard error is more constant for the x-values. That means the sqrt
# model is a better fit since more of the sample variation in insect count
# has been included in the model, leaving less pattern leftover for residuals. 
# Indeed, the adjusted R squared is now 75.5 %, slightly higher than for model 1. 
# The shapiro wilk test also shows residuals aren't normal for the first model, 
# while they are normal for the second square root model. 
summary(insect.lm)$adj.r.squared
summary(insect.sqrt.lm)$adj.r.squared




# part (iv)
summary(insect.sqrt.lm)
summary(insect.lm)
# The summary table for the square-root model 
# says the global F-test yields a significant p-value = 2e-16 < 0.05
# so the model is statistically useful for predicting mean counts. 
# All mean count differences among the insecticides are significant (since their p-values
# are less than 0.05)
# except for the mean difference between spray A and B, and between spray A and F, 
# just as in the first model. 
# However the square root model is more statistically useful since the F-statistic is higher
# (it is 44.8 for sqrt-model and 34.7 for model 1)
summary(insect.sqrt.lm)$fstatistic
summary(insect.lm)$fstatistic



# part (v)

# The estimated mean of the square root of the count of insects for insecticide C
# is equal to approximately 1.24 (units are in square root of insects)
pred <- predict(insect.sqrt.lm, newdata = data.frame(Ispray="C"),
        interval="confidence", level=0.95, type="response")
pred #
sqrtInsectFit <- pred[,1]; sqrtInsectFit

# part (vi)
# The estimated mean insect count when using insecticide C is the square of the
# previous answer - it is about 1.55 mean insects leftover after using spray C. 
sqrtInsectFit^2
