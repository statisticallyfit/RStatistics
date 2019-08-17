setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


# model fitting
library(nlme)
library(lme4)
# afex # helper functions

# plotting
library(ggfortify)
library(lattice)
library(dotwhisker) #coefficient plots

# data manipulation
library(broom)
library(broom.mixed)
library(tidyr)

options(show.signif.stars = FALSE)


### SOURCE for this lab: ------------------------------------------------------------
# https://bbolker.github.io/morelia_2018/notes/mixedlab.html
# -----------------------------------------------------------------------------------

## TODO: another lab (chickweights): 
# https://terpconnect.umd.edu/~egurarie/teaching/Biol709/Topic3/Lecture16_MixedEffectsModels.html

# subject = individual bird
# roostsitu = roost site factor variable with levels: tree, nest-box, inside or other
# mnth = monh with levels Nov or Jan
# stmass = mass of bird
load("data/starling.RData")
starlingData <- dataf

# Plot the data
ggplot(data=starlingData, aes(x=mnth, y=stmass)) + geom_point() + 
      geom_line(aes(group=subject)) + # connect subjects with a line
      facet_grid( . ~ roostsitu)  # 1 row of panels by roost levels

# Or can do all on same plot with differen colors  
ggplot(dataf,aes(mnth,stmass,colour=roostsitu))+
      geom_point()+
      geom_line(aes(group=subject))

# Meaning of plot: each line to dot is an observation ( a bird): 
s1 = subset(starlingData, roostsitu == "tree")
s2 = subset(s1, mnth == "Jan")

# Overkill for this data set, but sometimes it can be useful to put every individual in its own facet:
  ## reorder individuals by magnitude of difference
tempData <- transform(dataf,subject=reorder(subject,stmass,FUN=diff))
ggplot(tempData,aes(mnth,stmass,colour=roostsitu,group=subject))+
      geom_point()+geom_line()+facet_wrap(~subject)

##### INTERPRET: 

# -- It’s pretty obvious that the starting (November) mass varies among roost 
# situations (tree/nest-box/etc.), and that mass increases from November to 
# January, but we might like to know if the slopes differ among situations. 
# That means our fixed effects would be ~roostsitu*mnth, with our attention
# focused on the roostsitu:mnth (interaction) term. 

# --  For random effects, we can allow both intercept (obviously) and slope (maybe) 
# to vary among individuals, via (1+mnth|subject) or equivalent … in this case, 
# because measurements are only taken in two months, we can also write the random
# term as (1|subject/mnth).


# Fit the model
starling.lme <- lmer(stmass ~ mnth * roostsitu + (1|subject), data=starlingData)
summary(starling.lme)
# Can see that stddev of intercept among subjects (between subject variation) (0.587)
# is much less than within (group?) variation: residual error (4.165)


### Diagnostic plots: 

ggplot(augment(starling.lme),
       aes(sample=.resid/sd(.resid)))+  ## scale to variance=1
      stat_qq(aes(group=1,colour=roostsitu))+
      geom_abline(intercept=0,slope=1)

# Boxplot of residuals subdivided by roostsitu (grouping variable is on the 
# left side of the formula)
aa <- augment(starling.lme)
ggplot(aa,aes(roostsitu,.resid))+
      geom_boxplot()+coord_flip()

# Plot random effects to look for ouliers: 
tt <- tidy(starling.lme,effects="ran_vals")
ggplot(tt,aes(level,estimate))+
      geom_pointrange(aes(ymin=estimate-1.96*std.error,
                          ymax=estimate+1.96*std.error))+
      coord_flip()

# Influence measures: 
# plot(influence(starling.lme,group="subject"))


# Coefficient plot: for inference of the model results
dwplot(starling.lme)+geom_vline(xintercept=0,lty=2)


ggplot(starlingData,aes(x=roostsitu,y=stmass))+geom_boxplot()+
      geom_dotplot(binaxis="y",stackdir="center",fill="red",alpha=0.5,
                   binwidth=0.5)

# Predictions and plotting
starlingData$pred <- predict(starling.lme,re.form=NA)  ## population level

starlingData$pred1 <- predict(starling.lme) ## individual level

g0 <- ggplot(starlingData,aes(mnth,stmass))+
      geom_point()+
      geom_line(aes(group=subject))+
      facet_grid(.~roostsitu)

g0 +   geom_line(colour="gray",aes(y=pred1,group=subject)) +
      geom_line(colour="red",aes(y=pred,group=subject))

# INTERPRET: There is so much shrinkage (the among-individual variance is 
# very small) that we can barely see the individual-level predictions (gray lines) 
# behind the population-level predictions (red lines).

# Confident intervals for predictions
ggplot(starlingData,aes(mnth,pred1))+
      geom_line(aes(group=subject,x=as.numeric(mnth)),colour="gray")+
      facet_wrap(~roostsitu,scale="free_y",nrow=1)+
      geom_line(aes(y=pred,x=as.numeric(mnth)),colour="red")
