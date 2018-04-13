setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

library(ggplot2)
library(car)

load("data/Exercises and Examples/COFFEE2.Rdata")
is.factor(COFFEE2$X)
COFFEE2$X <- as.factor(COFFEE2$X)

# note: X = adverteizemen
coffeeDemand.lm <- lm(DEMAND ~ PRICE + X, data=COFFEE2)

summary(coffeeDemand.lm) # both terms significant, high R^2

# but residuals plot shows curvature term is missing. 
residualFittedPlot(coffeeDemand.lm)

# partial plotting
partialPlot(coffeeDemand.lm, variableName = "PRICE")

# comparing curvature data with straight line: Plotting residuals vs.
# model with advertising removed, so just against price. Indicates a curvature
# term in price is useful to add in.
# Gives better idea of functional form of demand-price WITHOUT Advertising effect.
crPlot(coffeeDemand.lm, variable="PRICE")
crPlots(coffeeDemand.lm, terms = ~. -X)



# Transformed model with 1 / PRICE
coffeeDemandInversePrice.lm <- lm(DEMAND ~ I(1/PRICE) + X, data=COFFEE2)
summary(coffeeDemandInversePrice.lm)
anova(coffeeDemandInversePrice.lm) # all significant, and Residual standard err
# is also reduced. 


# no indicating of curvature term missing, resids centered around zero. 
# constant variance
residualFittedPlot(coffeeDemandInversePrice.lm)

partialPlot(coffeeDemandInversePrice.lm, variableName = "I(1/PRICE)")

crPlot(coffeeDemandInversePrice.lm, variable="I(1/PRICE)")




# SHapiro test limitations
# * useful for only small samples
# low power: probability of detecting a non-normal error distribution 
# when it ixists is low. 
shapiro.test(coffeeDemandInversePrice.lm$residuals)
