setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 12 - ANOVA for Experimental Design")

library(effects)
library(dae)
options(show.signif.stars = FALSE)

mangoldData <- read.table("mangold.txt", header=TRUE)
mangoldData

dungName <- function(elem){ if(elem == 1) "D1" else if(elem == 2) "D2"}
blockName <- function(elem){
      if(elem == 1)"B1"
      else if(elem == 2) "B2"
      else if(elem == 3) "B3"
      else if(elem == 4) "B4"
}
ammoniaName <- function(elem){ if(elem == 1) "A1" else "A2"}
saltName <- function(elem) {if(elem == 1) "S1" else "S2"}

mangoldData$block <- factor(sapply(mangoldData$block, blockName))
mangoldData$dung <- factor(sapply(mangoldData$dung, dungName))
mangoldData$nh4 <- factor(sapply(mangoldData$nh4, ammoniaName))
mangoldData$salt <- factor(sapply(mangoldData$salt, saltName))
mangoldData



# Starting analysis
# global model = all possible interactions
mangold.lm <- lm(yield ~ block + nh4*salt*dung, data=mangoldData)
eff.mangold <- allEffects(mangold.lm)
plot(eff.mangold) # for separate levels of dung

# for averaging over levels of dung
attach(mangoldData)
par(mfrow=c(1,1))

interaction.plot(x.factor=salt, trace.factor=nh4, response=yield) # seems interaction
interaction.plot(x.factor=salt, trace.factor = dung, response=yield) # no interaction
# really between dung and salt
interaction.plot(x.factor=nh4, trace.factor = dung, response=yield) # slight

# THREE WAY PLOTS (need just one ,which ever is easiest)

# NULL HYPOTHESIS: 
# H0: the two way interaction between nh4 and salt is the same for different dung levels.
# fail to reject
# OR: 
# H0: the two way interaction between salt*dung is the same for nh4 levels
# OR
# H0: the two-way interactino between nh4*dung is same for different salt levels. 
interaction.ABC.plot(response=yield, x.factor=nh4, trace.factor = dung, groups.factor = salt, 
                     data=mangoldData)
# for different levels of dung, the salt*nh4 lines look the same so no three-way
# interaction

interaction.ABC.plot(response=yield, x.factor = nh4, trace.factor=salt, groups.factor=dung,
                     data=mangoldData)
# parallel lines for dung when S2 level, from A1 to A2, so no interaction at the S2
# level between dung and nh4, but potential interaction between nh4 and dung
# when salt = S1 because of non-parallel lines. 

interaction.ABC.plot(response=yield, x.factor = salt, trace.factor=nh4, groups.factor=dung,
                     data=mangoldData)
# parallel lines for A2 between dung*salt, but interaction for A1 between salt*dung
# so lines are NOT the same across nh4 levels, for salt*dung so some interaction.

interaction.ABC.plot(response=yield, x.factor = salt, trace.factor=dung, groups.factor=nh4,
                     data=mangoldData)
# non-parallel lines across nh4 levels between salt and dung, and parallel nearly
# at the A2 level

interaction.ABC.plot(response=yield, x.factor = dung, trace.factor=salt, groups.factor=nh4,
                     data=mangoldData)
interaction.ABC.plot(response=yield, x.factor = dung, trace.factor=nh4, groups.factor=salt,
                     data=mangoldData)



# INTERPRETING THE MODEL
anova(mangold.lm)
# INTERPRET: 
# 3-way not significant even though slight variation between lines from D1 to D2 
# between salt * ammonia

summary(mangold.lm)
# INTERPRET: 



# new model
mangold2.lm <- lm(yield ~ block + (nh4 + salt + dung)^2, data=mangoldData)
anova(mangold2.lm)
# nh4:salt = the way yield response to ammonia with addition of salt is not the
# same for different levels of salt
# nh4:dung = the way yield responds to addition of dung is the same regardless of whether
# you have nh4 present or absent. 
# salt:dung = the way yield responds to addition of dung is the same regardless 
# of salt levels. 


# drop 2 nonsignif ones
mangold3.lm <- lm(yield ~ block + nh4 + salt + dung + nh4:salt, data=mangoldData)
# if dung main effect were NTO significant then you could remove it, but here it is
# significant so keep it
anova(mangold3.lm)

# INTERPRET: the increase in mean yield from nh4 level 1 to 2 depends on
# different levels of salt (is different for salt 2 and 1). 
# OR: increase in mean yield with the addition of nh4 is not the same for
# absense or presence of salt. 
interaction.plot(response=yield, x.factor = nh4, trace.factor = salt)



#
# Gives main effect for dung (signif) = when dung is present, mean yield is significantly
# great er than when dung is not present. 
# Doesn't give main effect means for salt and nh4 since main effects aren't signif. 
eff.mangold3 <- allEffects(mangold3.lm)
eff.mangold3
# Significantly higher mean yield in presence of salt and nh4 (levels 2,2) mean=27.61
# then in the absense of salt, nh4 (levels 1,1), mean = 17.75



par(mfrow=c(2,2))
plot(mangold3.lm, which=c(1,2,3,5), cook.levels = c(0.2, 0.5, 1))
# cooks are not even appearing on plot so points are not influential. 
# NOTE: cooks levels here are not the same as the cooks distance (F distribution)