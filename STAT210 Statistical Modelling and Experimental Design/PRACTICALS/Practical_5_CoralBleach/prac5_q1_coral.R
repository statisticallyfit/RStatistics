setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PRACTICALS/Practical_5_CoralBleach/")

library(ggplot2)
options(digits=10, show.signif.stars = FALSE)

coralData <- read.table("coral.txt", header=TRUE)
coralData



# Plot the data
ggplot(data=coralData, aes(x=temperature, y=bleach)) + 
      geom_point(size=3, shape=19) + 
      xlab("Water Temperature") + ylab("Probability of Bleaching Event")


# make the GLM
coral.glm <- glm(bleach ~ temperature, data=coralData, family=binomial)

# Is model a good fit? 
anova(coral.glm, test="Chisq")
# from anova: 
# 1) deviance = 8.92, df = 1, p=0.0028 so highly different from the null model
# so we can reject H0: all slopes are zero. The temperature is useful predictor
# of coral bleaching probability. 
DevianceTest(coral.glm) # all slopes diff from zero since p = 0.00028

####### RESID DEVIANCE IS THE MODEL FIT MEASURE
# 2) residual deviance = 18.8, df=18, p = 0.40 > 0.05 so model is good fit 
# since that means we have support for assumption that model is good fit
# for the coral bleaching data. Resid dev is small. 
ResidualDevianceTest(coral.glm) # overall good model fit since p = 0.40


# regression coefs
summary(coral.glm)
# INTERPRET: summary: water temperature has a significant impact
# on the probability of a bleaching event occurring (p = 0.026)

betaCI(coral.glm) # the log odds ratios (also called log odds)
exp(betaCI(coral.glm)) # the odds ratios (also called odds)


# predict probability of coral bleaching when temperature = 30 C
predict(coral.glm, newdata=data.frame(temperature=30), type="response")
# verify result: 
# linear predictor (nu) = B0 + b1*temperature 
cof <- coef(coral.glm)
nu <- function(value) cof[[1]] + cof[[2]]*value
p = exp(nu(30)) / (1 + exp(nu(30))) ; p



# INTERPRET EVERYTHING: (how water temp relates to whether coral bleaching
# event will occur)
exp(cof) # this is the odds of a bleaching event
exp(betaCI(coral.glm))

exp(cof)-1
# INTERPRET: a 1-degree increase in water temperature increases the odds
# of a coral bleaching event by 169%. 
exp(betaCI(coral.glm)) -1
# INTERPRET: we can be 95% confidence that there will be between a 33% and 720%
# increase in the odds of a bleaching event with a 1 degree increase in temp.


# PLOTTING conf ints
plotConfidenceBands.glm(coral.glm)
