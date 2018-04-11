setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 6, 7 - Variable Screening, Transformations/lecturedata/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')
source("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R")


library(ggplot2)
library(GGally)
options(digits=10, show.signif.stars = FALSE)



mileageData <- read.table("mileage.txt", header=TRUE)

# pairs plot
pairsQuantPlot(mileageData, 2:4)

# INTERPRET: multicollinearity present since r = 0.803 between weight and disp
# what extra benefit are we getting from including disp if we already have
# the weight predictor? and vice versa. 


# Models
mileWeight.lm <- lm(Mileage ~ Weight, data=mileageData)
summary(mileWeight.lm)
betaCI(mileWeight.lm) # weight is significant but not far from zero!

mileDisp.lm <- lm(Mileage ~ Disp, data=mileageData)
summary(mileDisp.lm)
betaCI(mileDisp.lm)

mileWeightDisp.lm <- lm(Mileage ~ Weight + Disp, data=mileageData)
summary(mileWeightDisp.lm)
betaCI(mileWeightDisp.lm) # but shee, once we include weight, disp is not significant
anova(mileWeightDisp.lm) # last row - model with disp not significant (given
# that weight has already been fitted)



mileDispWeight.lm <- lm(Mileage ~ Disp + Weight, data=mileageData)
summary(mileDispWeight.lm) # disp not significant with weight in the model
# so we see that weight explains most variation in Y, while disp less. 
betaCI(mileDispWeight.lm) # but see, once we include weight, disp is not significant
anova(mileDispWeight.lm) # last row - TRICKY - once fit disp, weight model is
# still significant (since we test param of weight = 0, and weight IS SIGNIF.)


# Looking at SSEs (in anovas of each model)
SSE(mileWeight.lm)
SSE(mileDisp.lm)
SSE(mileWeightDisp.lm)
SSE(mileDispWeight.lm)
# ... we see that disp model has higher SSE (higher unexplained variability)
# than the sse = 380 of the weight model. The combined model has slightly
# lower SSE than weight model. Disp is not contributing much extra information
# about Mileage so SSE is not much lower and Disp is not significant. 





# Stepwise regression
formL <- formula(~ 1)
formU <- formula(~ Weight + Disp)
start.model <- lm(Mileage ~ 1, data=mileageData)

step.forward.model <- step(start.model, direction="forward",
                           scope=list(lower=formL, upper=formU))
# So the final model is Mileage ~ Weight

betaCI(step.forward.model)
# INTERPRET: intercept is significantly different from 0 and so are the weights
# and are negative (means as weights increase, mileage decrases)