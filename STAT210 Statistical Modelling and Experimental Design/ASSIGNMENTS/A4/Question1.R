setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A4")

options(digits=10, show.signif.stars = FALSE)
library(dae)


# Preparing the data. 
oxygenData <- read.table("o2.txt", header=TRUE)

# naming the temperature levels
numToTempLevel <- function(num){
      if(num == 1) return("low")
      else if(num == 2) return("medium")
      else return("high")
}
oxygenData$temperature <- sapply(oxygenData$temperature, numToTempLevel)

# making the species a factor (so its levels are not numerical)
oxygenData$species <- factor(oxygenData$species)
# making the temperature a factor. 
oxygenData$temperature <- factor(oxygenData$temperature)
# gender is already a factor
is.factor(oxygenData$gender)

# making "low" as base temperature
levels(oxygenData$temperature)
oxygenData$temperature <- relevel(oxygenData$temperature, ref="low")
levels(oxygenData$temperature)


# part a) ------------------------------------------------------------------------


# part b) ------------------------------------------------------------------------
interaction.ABC.plot(response=resprate, trace.factor=gender,
                     x.factor=temperature, groups.factor=species, data=oxygenData)
# part c) ------------------------------------------------------------------------

# part d) ------------------------------------------------------------------------