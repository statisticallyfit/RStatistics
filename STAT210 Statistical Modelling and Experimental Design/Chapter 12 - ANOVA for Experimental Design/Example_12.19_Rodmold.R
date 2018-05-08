setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/RODMOLD.Rdata")
options(digits=10, show.signif.stars = FALSE)

RODMOLD$Batch <- factor(RODMOLD$Batch)


rodmold.lm <- lm(ExtRate ~ Pressure + Temperature + Pressure*Temperature + 
                       Batch, data=RODMOLD)
summary(rodmold.lm) # overall F test is significant F = 83.22, so there is enough
# evidence to indicate differences among block means or treatment means or both. 

anova(rodmold.lm)
# Pressure, Temp, Press*TEMP stats are all significant so these all contribute
# info toward predicting Y 
# --- Pressure: different mean Ys among the two pressure levels 
# --- Temp: different mean Ys among the two temp levels. 
# --- Pressure*Temp: difference in mean Ys for the two pressure levels depends
# upon the temperature levels. 

# BUT: the block means (Batch) are not significant. So the insufficient evidence
# to say a difference in the mean extrusio of the plastic from batch to 
# batch exists. (cannot say). Blocking has not increased amount of
# information in experiment. 