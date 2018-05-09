setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load('data/Exercises and Examples/GPA3.Rdata')
options(digits=10, show.signif.stars = FALSE)
library(car)
library(ggplot2)


# testing normality overall # fail reject null of normality. 
shapiro.test(GPA3$GPA)
# showing normality for each group
ggplot(data=GPA3, aes(x=GPA, colour=CLASS)) + 
      geom_line(stat="density", size=2)


# testing equal variances
with(GPA3, leveneTest(GPA, group=CLASS)) # fail reject null of equal variances
with(GPA3, bartlett.test(GPA, g=CLASS))


