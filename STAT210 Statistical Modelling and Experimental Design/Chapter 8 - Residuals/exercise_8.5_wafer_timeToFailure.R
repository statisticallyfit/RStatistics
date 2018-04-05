setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/WAFER.Rdata")
library(ggplot2)
library(ggfortify)
options(digits=10)

# first model

waferFailure.lm <- lm(FAILTIME ~ TEMP, data=WAFER)
summary(waferFailure.lm) # signifcant F but residuals show pattern. 

# first plotting data
B0 <- waferFailure.lm$coefficients[[1]]
B1 <- waferFailure.lm$coefficients[[2]]
ggplot(WAFER, aes(x=TEMP,y=FAILTIME)) + 
      geom_point(shape=19,size=3,color="dodgerblue")+
      geom_abline(intercept=B0, slope=B1, color="red", size=1)

residualPlot(waferFailure.lm)
partialPlot(waferFailure.lm, variableName = "TEMP") # suggests e^-x or 1/x relationship


# new model - exponential negative
wafer.expo.lm <- lm(FAILTIME ~ TEMP + I(exp(-TEMP)), data=WAFER)
partialPlot(wafer.expo.lm, variableName = "TEMP")
autoplot(wafer.expo.lm, which=1)
# not that good partial plot (not that straight line) so poisson not good fit
# for the model, then?

# new model - quadratic
wafer.quad.lm <- lm(FAILTIME ~ TEMP + I(TEMP^2), data=WAFER)
partialPlot(wafer.quad.lm, variableName = "TEMP") # better than wafer.expo!
autoplot(water.quad.lm, which=1) 

# new model - 1/temp
wafer.inverse.lm <- lm(FAILTIME ~ TEMP + I(1/TEMP), data=WAFER)
partialPlot(wafer.inverse.lm, variableName = "TEMP") # better than wafer.expo!
autoplot(wafer.inverse.lm, which=1) 
