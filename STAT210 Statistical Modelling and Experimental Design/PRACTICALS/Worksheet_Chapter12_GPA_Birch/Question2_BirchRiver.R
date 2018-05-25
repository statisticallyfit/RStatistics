setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PRACTICALS/Worksheet_Chapter12_GPA_Birch")

options(show.signif.stars = FALSE)

birchData <- read.table("birch.txt", header=TRUE)


with(birchData, 
     interaction.plot(response=ATP, x.factor=Species, trace.factor=Treat))

# the 2x2 factorial design
birch.lm <- lm(ATP ~ Species * Treat, data=birchData)

with(birchData, tapply(ATP, INDEX=Species, mean)) # if we interpret the species
# main effect when INTERACTION term is significant, we would be averaging
# over the other Treatment predictor so we would get just the Species avg (for Euro)
# of 0.746 which is between the lines for Euro. 
with(birchData, tapply(ATP, INDEX=Treat, mean))


anova(birch.lm)
# There is a significant interaction between tree species and treatment. 
# So the difference in mean ATP for the two tree levels (River and Euro) depends 
# upon the 2 levels of treatment (control and flood). 

# INTERACTION IS SIGNIFICANT SO WE WANT 2-way table
with(birchData, tapply(ATP, INDEX=list(Treat, Species), mean))
# -- estimated difference in means between euro and river birch when we don't
# flood them is 1.78 - 1.2 = 0.58 (the smaller slope or distances to x-axis
# between euroa nd river for the control dotted line)
# -- estimated difference in means between euro and river birch we when DO FLOOD
# is 1.0975 (the larger diff to x-axis between E and R for the FLood line)


summary(birch.lm)
# No need to interpret main effects since then we would be averaging over the
# other predictor. 
# And,
# B3 = (mu_Riv/Flood - mu_Eur/Flood) - (mu_Riv/Control - mu_Eur/Control) 
# -- The difference in mean ATP between River and Euro is significantly greater
# when flooded than when controlled. 

