setwd("/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/learnstatistics/[ChristopherBilder] Analysis Of Categorical Data With R")

library(car)
library(lmtest)


placekick <- read.csv("data/Placekick.csv")
head(placekick)
tail(placekick)

change.dist.model <- glm(good ~ change + distance, family=binomial(link="logit"), data=placekick)
change.dist.model
dist.model <- glm(good ~ distance, family=binomial(link="logit"), data=placekick)
dist.model

ResidualDeviance(change.dist.model)
NullDeviance(change.dist.model)
ResidualDeviance(dist.model)
NullDeviance(dist.model)


# Likelihood Ratio Test
Anova(change.dist.model) # method 1 (test="LR" default)
Anova(dist.model)
lrtest(dist.model, change.dist.model) # method 2
anova(dist.model, change.dist.model) # method 3
anova(change.dist.model)
anova(dist.model)
result <- LikelihoodRatioModelTest(dist.model, change.dist.model) # method 4 for anova()
result 


# Wald Tests
Anova(change.dist.model, test="Wald") # method 1
# method 2
Z0 <- change.dist.model$coefficients[2] / sqrt(vcov(change.dist.model)[2,2]); Z0
pvalue <- 2*(1 - pnorm(abs(Z0))); pvalue



# Test of Ho: logit(pi) = beta_0 vs. Ha: logit(pi) = beta_0 + beta_1*change
nullMod <- glm(good ~ 1, family=binomial(link=logit), data=placekick)
# NOTE: the null dev = resid dev because the intercept model and built model are the same
nullMod
altMod <- glm(good ~ change, family=binomial(link=logit), data=placekick)
# NOTE: the resid dev is close to null dev because the intercept model and built 
# model are very similar (intercept only has good ~ 1 but build model is good ~ change)
altMod
# shorter way
BinaryLogisticRegression(xNames=c("change"), yName="good", data=placekick)
BinaryLogisticRegression(NULL, "good", placekick)
# model fit
anova(nullMod, altMod, test="Chisq") # likratio test statistic = 24.277
anova(nullMod) # comparing null against null
anova(altMod) # comparing null against alternative

# building the statistic likratio 
pi.hat.Ho <- nullMod$fitted.values # equal to mean(y) = observed prop of successes
pi.hat.Ha <- altMod$fitted.values
y <- placekick$good
likRatio <- -2 * sum(y * log(pi.hat.Ho/pi.hat.Ha) + (1-y)*log((1-pi.hat.Ho)/(1-pi.hat.Ha)))
likRatio
pvalue <- 1 - pchisq(likRatio, df = nullMod$df.residual - altMod$df.residual); pvalue
# shorter way
LikelihoodRatioModelTest(nullMod, altMod)


# Fit saturated model
rowNums <- 1:nrow(placekick)
# the "-1" in the formula makes it estimate without intercept term
saturatedModel <- glm(good ~ factor(rowNums) - 1, family=binomial(link=logit), data=placekick)
summary(saturatedModel)
names(saturatedModel)
saturatedModel$converged
# resid dev is small because = G2 (sat) - G2 (built) and built is the saturated one
# null dev is big because = G2 (sat) - G2 (null model) and there is a big difference
ResidualDeviance(saturatedModel)
NullDeviance(saturatedModel)

anova(saturatedModel)
anova(nullMod)



### Hosmer-Lemeshow (my test, not from book)
HosmerLemeshowTest(saturatedModel)
HosmerLemeshowTest(dist.model)
HosmerLemeshowTest(change.dist.model)
HosmerLemeshowTest(nullMod)
HosmerLemeshowTest(altMod)

