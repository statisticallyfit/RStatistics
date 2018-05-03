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
anova(coral.glm, test="Chisq")
DevianceTest(coral.glm) # all slopes diff from zero since p = 0.00028
ResidualDevianceTest(coral.glm) # overall good model fit since p = 0.40

# regression coefs
summary(coral.glm)
betaCI(coral.glm) # the log odds ratios
exp(betaCI(coral.glm)) # the odds ratios


# predict probability of coral bleaching when temperature = 30 C
predict(coral.glm, newdata=data.frame(temperature=30), type="response")


# PLOTTING conf ints
plotConfidenceBands.glm(coral.glm)
