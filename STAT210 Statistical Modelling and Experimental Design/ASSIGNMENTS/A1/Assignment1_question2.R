setwd('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A1/')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')

options(digits = 3, show.signif.stars = FALSE)

insectData <- read.table("insect.txt", header=TRUE)
head(insectData)


# part a) and b) 
attach(insectData)

par(mfrow=c(1,1))
plot(Count ~ Ispray, main="Count of Surviving Insects After Each Insecticide")

# Interpretation: median values and outlier values. 
median(insectData$Count[which(insectData$Ispray == "A")])
median(insectData$Count[which(insectData$Ispray == "B")])
median(insectData$Count[which(insectData$Ispray == "F")])

median(insectData$Count[which(insectData$Ispray == "C")])
median(insectData$Count[which(insectData$Ispray == "D")])
median(insectData$Count[which(insectData$Ispray == "E")])

max(insectData$Count[which(insectData$Ispray == "C")])
max(insectData$Count[which(insectData$Ispray == "D")])



# part c) 1) 
# (i) 
insect.lm <- lm(Count ~ Ispray, data=insectData)
summary(insect.lm)


# (ii) Model assumptions
par(mfrow=c(2,2))
plot(insect.lm, which=1:4)


# Testing normality of errors with Shapiro wilk test - p-value is low
# must reject H0 of normality of residuals. 
shapiro.test(insect.lm$residuals)


# part c) 2) Fitting the Square Root model
# (i)
insect.sqrt.lm <- lm(sqrt(Count) ~ Ispray, data=insectData)
summary(insect.sqrt.lm)


# (ii) diagnostics
par(mfrow=c(2,2))
plot(insect.sqrt.lm, which=1:4)
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