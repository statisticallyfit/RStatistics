setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PRACTICALS/Practical_6_HerbicideAnova")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ADVANCED_PLOTTING.R')

options(show.signif.stars = FALSE)
library(effects)



# Enter the data (location = block, herbicide = treatment)
cottonData <- data.frame(Location=c(rep("1",3), rep("2",3), rep("3",3), rep("4",3)),
                         Herbicide=rep(c("H1","H2","H3"), 4),
                         Yield=c(12.7,15.2,12.3,13,16.2,9.4,15.6,13.7,9.1,7.1,7.8,4.7),
                         stringsAsFactors = TRUE)
cottonData$Herbicide <- relevel(cottonData$Herbicide, ref="H1")



# part 1) a) plot the means for the randomized design
cotton.crd.lm <- lm(Yield ~ Herbicide, data=cottonData)
CRD.eff <- allEffects(cotton.crd.lm)

plot(CRD.eff)
effectPlot(cotton.crd.lm)
# INTERPRET: substantial overlap in 95% CI's shows that there seems to be no 
# significant difference in mean yield between the three herbicides. 

anova(cotton.crd.lm) # P-value is not significant. H0: mu_low = mu_med = mu_high
#
cotton.null.lm <- lm(Yield ~ 1, data=cottonData)
NestedFTest(cotton.null.lm, cotton.crd.lm)



# note: the means are calculated
CRD.eff$Herbicide$fit
with(cottonData, tapply(Yield, INDEX=list(Herbicide), mean))


# Both factors model (block) -----------------------------------------------------
cotton.rcb.lm <- lm(Yield ~ Location + Herbicide, data=cottonData)
RCB.eff <- allEffects(cotton.rcb.lm)

RCB.eff
with(cottonData, tapply(Yield, INDEX=list(Location), mean))
with(cottonData, tapply(Yield, INDEX=list(Location), mean))

plot(RCB.eff)

effectPlot(cotton.crd.lm) # compare with this one. (same on the right)
effectPlot(cotton.rcb.lm)
# INTERPRET: significant different in mean yield across locations 1 and 4? and
# between levels of herbicide (high and between other 2)

anova(cotton.rcb.lm) # now there is significant difference in mean yield
# across the herbicide levels, once variability in location is accounted for. 
summary(cotton.rcb.lm)
# INTERPRET: mean yield for crops with Herbicide=high is significantly lower
# then the mean yield for crops with Herbicide=low.  (B = -3.225)



# Plotting the actuall data for each location
#plot(Yield ~ Herbicide, type="n", data=cottonData)
#colour <- c("black", "red", "green", "blue")
##plot(cottonData$Herbicide, cottonData$Yield)
#for(i in 1:4){
#      points(Yield[Location == i] ~ Herbicide[Location == i], cex=1.5, col=colour[i],
#             data=cottonData)
#}
#legend(2.5, 15, cex=1.5, c("Location 1", "Location 2", "Location 3", "Location 4",
#                           pch=1:4, col=colour))



# Set Herbicide = between as base level
cottonData$Herbicide <- relevel(cottonData$Herbicide, ref="H2")
cotton.rcb2.lm <- lm(Yield ~ Location + Herbicide, data=cottonData)
summary(cotton.rcb2.lm)
# No significant diff in mean yield between H_low and H_between, but there is a signif
# diff in mean yield between H_high and H_between (B = -4.35), with H_high's yield
# being significantly lower. 
