setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A4")

options(digits=10, show.signif.stars = FALSE)
library(dae)
library(effects)


# Preparing the data. 
oxygenData <- read.table("o2.txt", header=TRUE)

# naming the temperature levels
numToTempLevel <- function(num){
      if(num == 1) return("T1")
      else if(num == 2) return("T2")
      else if(num == 3) return("T3")
}
# naming the species leves to C1, C2, C3 for crab types
numToSpeciesLevel <- function(num){
      if(num == 1) return("C1")
      else if(num == 2) return("C2")
      else if(num == 3) return("C3")
}

oxygenData$temperature <- sapply(oxygenData$temperature, numToTempLevel)
oxygenData$species <- sapply(oxygenData$species, numToSpeciesLevel)

# making the temperature a factor. 
oxygenData$temperature <- factor(oxygenData$temperature)
# making species a factor.
oxygenData$species <- factor(oxygenData$species)
# gender is already a factor
is.factor(oxygenData$gender)


# part b) ------------------------------------------------------------------------

# exploring the two-way interactions
attach(oxygenData)

# species x gender
interaction.plot(x.factor=species, trace.factor=gender, response=resprate)
# temperature x gender
interaction.plot(x.factor=temperature, trace.factor = gender, response=resprate)
# species x temperature
interaction.plot(x.factor=species, trace.factor=temperature,response=resprate)

# exploring the 3-way interaction: gender x species x temperature
interaction.ABC.plot(response=resprate, x.factor = species, trace.factor=gender,
                     groups.factor = temperature, data=oxygenData)

detach(oxygenData)



# part c) ------------------------------------------------------------------------

# global model
global.lm <- lm(resprate ~ gender * species * temperature, data=oxygenData)
anova(global.lm)
summary(global.lm)

# fit model without three-way interaction term since it was not significant. 
nothreeway.lm <- lm(resprate ~ gender + species + temperature + gender:species + 
                          gender:temperature + species:temperature, data=oxygenData)
anova(nothreeway.lm)
summary(nothreeway.lm)

# fit final model without 3-way term and without gender x temperature. 
final.lm <- lm(resprate ~ gender + species + temperature + gender:species + 
                     species:temperature, data=oxygenData)
anova(final.lm)
summary(final.lm)


# part d) ------------------------------------------------------------------------

eff.crab <- allEffects(final.lm)
plot(eff.crab)
print(eff.crab)
