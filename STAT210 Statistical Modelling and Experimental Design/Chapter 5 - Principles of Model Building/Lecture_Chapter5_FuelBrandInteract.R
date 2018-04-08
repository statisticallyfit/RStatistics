setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 5 - Principles of Model Building/lecturedata/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')

library(ggplot2)

dieselData <- read.table("DIESEL.txt", header=TRUE)
head(dieselData)

# Need signif.tests to see if interactions are significant, since
# the dramatic slope may be from scale differences. 

with(dieselData, interaction.plot(x.factor = FUEL, trace.factor = BRAND,
                                  response=PERFORM))

# INTERPRET: The way performance increases for both brands as it moves from F1 to F2
# is the same for both brands. (not seemingly significant relation)

# INTERPRET: we get a similar increase in performance as we go from F1 to F2
# for both brands, but performance decreases from F2 to F3 for B1 while it
# increases from F2 to F3 for B2. (brand 1 performs very poorly under F3
# but F3 yields the best performance for brand 2)

with(dieselData, interaction.plot(x.factor = BRAND, trace.factor = FUEL,
                                  response=PERFORM))

# INTERPRET: we get a decrease in performance as we go from B1 to B2 
# for both F1 and F2, but we get an increase in performance as we go from
# B1 to B2 for F3 use. 





# INTERACTION MODEL
perform.lm <- lm(PERFORM ~ FUEL*BRAND, data=dieselData)
summary(perform.lm)
anova(perform.lm)

# Need to keep main effects if INTERACTION is significant (hierarchical)

# Misleading to interpret main effects if interaction is significant because
# each of the main effects is averaged over the levels of the other one. 
# (if we look at fuel, we average over levels of brand, so we ignore
# particular info from brand levels! bad)



# GETTING THE ACTUAL MEANS (not from coefs table)
attach(dieselData)
tapply(PERFORM, list(FUEL, BRAND), mean) # yuo can see they match up
# to the diagram: (this is just like plugging in with formulas, but faster)

with(dieselData, interaction.plot(FUEL, BRAND, PERFORM))

tapply(PERFORM, list(FUEL, BRAND), sd) 


detach(dieselData)
