setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A2/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')

library(ggplot2)
options(digits=10, show.signif.stars = F)


data("ToothGrowth")
ToothGrowth$dose <- factor(ToothGrowth$dose)
attach(ToothGrowth)


# part b)
interaction.plot(x.factor=dose, trace.factor = supp, response=len)



# part c)
formLower <- formula(~ 1)
formUpper <- formula( ~ dose * supp, data=ToothGrowth)

start.model <- lm(len ~ 1, data=ToothGrowth)

step.forward.model <- step(start.model, direction = "forward",
                           scope=list(lower=formLower, upper=formUpper))
# note: appropriate model is the interaction model since it lowers AIC. 


# part d)
summary(step.forward.model)
# see that main effects are sig, but slope difference between dose 1 and vc
# is not sig. 

anova(step.forward.model) # given that we fitted the dose, and supp, the interaction
# term model with dose and supp has significant F-value (nested F-tests)
# If the interaction term is sig, then we don't need to look at significance
# of the main effects terms, since the model is hierarchical. 



# predicting mean length with dose = 2, supp= OJ
betas <- summary(step.forward.model)$coef[,1]
betas
# dose=2, supp = OJ === mean = B0 + B2
betas[1] + betas[3]




# part e)
pred <- predict(insect.sqrt.lm, newdata = data.frame(Ispray="C"),
                interval="confidence", level=0.95, type="response")

predict(step.forward.model, newdata=data.frame(dose="2", supp="OJ"), type="response")


detach(ToothGrowth)