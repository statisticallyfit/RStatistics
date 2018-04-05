setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/HAWAII.Rdata")
library(ggplot2)
library(ggfortify)
options(digits=10)

hawaii.lm <- lm(LEASEFEE ~ SIZE + I(SIZE^2), data=HAWAII)
summary(hawaii.lm)

residualFittedPlot(hawaii.lm)

normalQQPlot(hawaii.lm) # basically normal with middle deviations
shapiro.test(hawaii.lm$residuals) # fail to reject null
# but normality tests have low power, says the book
# power = P(Reject null | null is false)

residualPlot(hawaii.lm, variableName = "SIZE")
partialPlot(hawaii.lm, variableName = "SIZE")

# test for heteroskedasticity
f <- as.formula("LEASEFEE ~ SIZE + I(SIZE^2)")
HomoskedasticityRegressionTest(theFormula=formula(hawaii.lm), data=HAWAII,
                               xName = "SIZE", xSplit=12)
# there is evdidence of non-constant variance. 