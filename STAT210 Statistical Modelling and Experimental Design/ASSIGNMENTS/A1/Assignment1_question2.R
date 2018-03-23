setwd('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A1/')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')

options(digits = 5, show.signif.stars = FALSE)

insectData <- read.table("insect.txt", header=TRUE)
head(insectData)


# part a) and b) 
attach(insectData)

par(mfrow=c(1,1))
plot(Count ~ Ispray, main="Count of Surviving Insects After Each Insecticide")

# Outliers of insects after using sprays C, D 
max(insectData$Count[which(insectData$Ispray == "C")])
max(insectData$Count[which(insectData$Ispray == "D")])



# part c) 1) 
# (i) 
insect.lm <- lm(Count ~ Ispray, data=insectData)
summary(insect.lm)


# (ii) Model assumptions
par(mfrow=c(1,2))
plot(insect.lm, which=1:2)


# Testing normality of errors with Shapiro wilk test - p-value is low
# must reject H0 of normality of residuals. 
shapiro.test(insect.lm$residuals)


# part c) 2) Fitting the Square Root model
# (i)
# if we remove the intercept (add -1 to ispray) then we get no intercept
# and all coefficients correspond to mean insect sqrt count for A,B,C,D,E,F.
insect.sqrt.lm <- lm(sqrt(Count) ~ Ispray, data=insectData)
summary(insect.sqrt.lm)


# (ii) diagnostics
par(mfrow=c(1,2))
plot(insect.sqrt.lm, which=1:2)
# Testing residuals normality - better now - we can say residuals are not
# deviating from normality. 
shapiro.test(insect.sqrt.lm$residuals)


# part (iv)
summary(insect.sqrt.lm)
anova(insect.sqrt.lm)



# part (v)
# The estimated mean of the square root of the count of insects for insecticide C
# is equal to approximately 1.24 (units are in square root of insects)
pred <- predict(insect.sqrt.lm, newdata = data.frame(Ispray="C"),
        interval="confidence", level=0.95, type="response")
pred #
sqrtInsectFit <- pred[,1]; sqrtInsectFit

# part (vi) 
sqrtInsectFit^2

detach(insectData)
