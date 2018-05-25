setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 12 - ANOVA for Experimental Design")


library(effects)
options(show.signif.stars = FALSE)

tulipData <- read.table("tulips.txt", header=TRUE)
tulipData$BED <- factor(tulipData$BED)
tulipData$SHADE <- factor(tulipData$SHADE)
tulipData$WATER <- factor(tulipData$WATER)

with(tulipData, interaction.plot(x.factor=WATER, trace.factor=SHADE, response=BLOOMS))

tulip.lm <- lm(BLOOMS ~ BED + WATER*SHADE, data=tulipData)

# plotting effects
eff.tulip <- allEffects(tulip.lm)
plot(eff.tulip)

eff.tulip

with(tulipData, tapply(BLOOMS, INDEX=list(WATER, SHADE), mean)) # same interaction
# means as the calculated eff.tulip ones. 

# INTERPRET: 
# -- bed is blocking, nuisance factor. So there is an overlap in confints, not
# clear signif diffs. 
# -- WATER*SHADE: Water = 3, greater response under shade 1 than under shade 2, 3


anova(tulip.lm)
# * BED is signif so it was  good to include it in design (there is variability
# among the bed blocks)
# -- WATER*SHADE term: there is an interaction. Diff in mean number of tulips
# for any 2 levels of WATER depends on the levels of shade, or vice versa. 