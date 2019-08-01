setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggplot2)
options(show.signif.stars = FALSE)


# part a) --------------------------------------------------------------------------------

# Binomial formulation: create the data so that it can be used to fit the binomial glm. 
miceData.binom <- data.frame(Strain=c(rep("X",4), rep("Y",4)), 
                             Gender=rep(c(rep("male",2), rep("female",2)),2), 
                             Exposure= rep(c("exposed", "control"), 4), 
                             TumourCount = c(12, 74, 12, 84, 14, 80, 14, 79),
                             NoTumourCount = c(4,5,2,3,4,10,1,3))

miceData.binom$Total <- miceData.binom$TumourCount + miceData.binom$NoTumourCount
miceData.binom

#aphidData.binomial
#aphidData.poisson
miceData.pois <- data.frame(Strain=c(rep("X",4*2), rep("Y",4*2)), 
           Gender=rep(c(rep("male",2*2), rep("female",2*2)),2), 
           Exposure= rep(c(rep("exposed",2), rep("control",2)), 4), 
           Tumour=rep(c("yes", "no"), 8), 
           Count = c(12, 4, 74, 5, 12, 2,84,3,14,4,80,10,14,1,79,3))
aphidData.poisson
aphidData.binomial      
