setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
#detach(package:lme4)
#detach(package:nlme)
library(nlme)
library(lme4)
library(lattice)
options(show.signif.stars = FALSE)


### SOURCE for this lab: ------------------------------------------------------------
# https://bbolker.github.io/morelia_2018/notes/mixedlab.html
# -----------------------------------------------------------------------------------

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