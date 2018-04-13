setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A2/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')

library(ggplot2)
options(digits=10, show.signif.stars = F)


data("ToothGrowth")
ToothGrowth$dose <- factor(ToothGrowth$dose)
toothData.1 <- ToothGrowth
# releveling data to dose = 1 so that desired comparisons can be made. 
toothData.1$dose <- relevel(ToothGrowth$dose, ref="1")


# part b) plotting len vs. dose with supp levels as tracing factor. 
par(mfrow=c(1,1)) 
with(ToothGrowth, interaction.plot(x.factor=dose, trace.factor = supp, response=len))



# part c) stepwise regression
formLower <- formula(~ 1) # the minimal model
formUpper <- formula( ~ dose * supp, data=toothData.1) # the maximal model

start.model <- lm(len ~ 1, data=toothData.1)

step.forward.model <- step(start.model, direction = "forward",
                           scope=list(lower=formLower, upper=formUpper))
# note: appropriate model is the interaction model since it lowers AIC. 

anova(step.forward.model) # given that we fitted the dose, and supp, the interaction
# term model with dose and supp has significant F-value (nested F-tests)
# If the interaction term is significant, then we don't need to look at significance
# of the main effects terms, since the model is hierarchical. 


# part d)
summary(step.forward.model)
# the main effects are significant, but the B4 = mean length difference between 
# VC and OJ from dose 0.5 to dose 1 is not significant. 
# Also significant is term B5, which is the mean length difference between 
# VC and OJ from dose 2 to dose 1. 



# part e)

# method 1 to check
df <- data.frame(dose="2", supp="OJ")
predict(step.forward.model, newdata=df, interval="confidence", type="response")

# method 2: (not using predict). 
# mean length for OJ, and dose=2 is 26.06 (bottom left corner)
with(toothData.1, tapply(len, list(dose, supp), mean))
