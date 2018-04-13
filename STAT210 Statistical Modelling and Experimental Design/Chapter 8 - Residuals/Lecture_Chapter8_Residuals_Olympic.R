setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')
source("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R")


options(digits=10, show.signif.stars = FALSE)

load("data/Exercises and Examples/OLYMPIC.Rdata")


# Scatter
# there is saturation in CHOLES as fat is high
ggplot(data=OLYMPIC, aes(x=FAT, y=CHOLES)) + geom_point(shape=19, size=3)


# first model
olympic1.lm <- lm(CHOLES ~ FAT, data=OLYMPIC)
summary(olympic1.lm)
anova(olympic1.lm)

autoplot(olympic1.lm, which=1:2, size=3, color="dodgerblue")
# curvature in residuals




# Quadratic model
olympic2.lm <- lm(CHOLES ~ FAT + I(FAT^2), data=OLYMPIC)
anova(olympic2.lm)
residualFittedPlot(olympic2.lm)
# INTERPRET: non-constant variance since at bottom very low. More points
# in positive range than negative range, since not quite centered at 0. 
# check whether lowest one is outlier