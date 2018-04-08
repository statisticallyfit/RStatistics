setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 5 - Principles of Model Building/lecturedata/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')

library(ggplot2)

potatoData <- read.table("potatoes.txt", header=TRUE)


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

# INTERPRET: as temperature increases, rotting increases generally for all
# levels of bacteria and across all levels of oxygen. More or less the same. 
# But the red lines and green lines aren't exactly parallel over oxygen levels.
# This means for different levels of oxygen, the rotting increases differently
# as temp increases for bacteria levels 1 and 2. (red,green)
# Is this significant enough to appear in the test?


rot.lm <- lm(ROT ~ OXYGEN * BAC * TEMP, data=potatoData)
summary(rot.lm)
anova(rot.lm)
# INTERPRET: only BAC:TEMP interaction is significant. Can get rid of
# the 3-way term, also OXY:BAC, and OXY:TEMP. The above ones aren't significant
# given that the previous hierarchical model has been fitted. 
# Can aalso remove Oxygen since not significant, and is also not in the BAC:TEMP
# interaction. 


rot2.lm <- lm(ROT ~ OXYGEN + BAC*TEMP, data=potatoData)
summary(rot2.lm)
anova(rot2.lm)
# In this anova model, we do have to look at OXYGEN p-value (not sig)
# since it is not included as an interaction term, just as single main effect. 
# So now we can delete it. 




# MEANS ------------------------------------------------------------------
with(potatoData, tapply(ROT, INDEX = list(TEMP, BAC), mean))
# TEMP levels - as rows, BAC levels = as cols. 
with(potatoData, interaction.plot(x.factor = BAC, trace.factor = TEMP,
                                  response=ROT))
# INTERPRET: the increase in rotting with increase in bacteria
# is greater for TEMP 2 than for TEMP 1. 


# MEANS OXYGEN -----------------------------------------------------------
with(potatoData, tapply(ROT, OXYGEN, mean)) # oxygen not sig, strictly speaking, 
# so we don't really need to know the means. 

