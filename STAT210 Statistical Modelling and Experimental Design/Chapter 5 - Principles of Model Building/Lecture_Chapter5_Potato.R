setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 5 - Principles of Model Building/lecturedata/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')

library(ggplot2)

potatoData <- read.table("potatoes.txt", header=TRUE)
head(potatoData)


# Two-way interaction plots
par(mfrow=c(1,3))
with(potatoData, interaction.plot(x.factor = OXYGEN, trace.factor = BAC,
                                  response=ROT))
with(potatoData, interaction.plot(x.factor = BAC, trace.factor = TEMP,
                                  response=ROT))
with(potatoData, interaction.plot(x.factor = OXYGEN, trace.factor = TEMP,
                                  response=ROT))

# IF Three-way term isn't sifnificant then we can go back and chew on
# the two-way model but if it is significant, then we can't worry about
# significance of the two-way terms due to the averaging over different
# levels issue. 

#install.packages("dae")
library(dae)

interaction.ABC.plot(response=ROT, x.factor = TEMP, groups.factor = BAC,
                     trace.factor = OXYGEN, data=potatoData)
