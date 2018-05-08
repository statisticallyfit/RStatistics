setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/WATERVAPOR.Rdata")
options(digits=10, show.signif.stars = FALSE)
library(ggplot2)


#is.factor(WATERVAPOR$TEMP)
#WATERVAPOR$TEMP <- factor(WATERVAPOR$TEMP)
#WATERVAPOR$MOLE <- factor(WATERVAPOR$MOLE)
#WATERVAPOR <- removeWhitespace(WATERVAPOR, colsToFix = c("TEMP", "MOLE"))

oxydiff.lm <- lm(OXYDIFF ~ MOLE*TEMP, data=WATERVAPOR)
summary(oxydiff.lm)
anova(oxydiff.lm)

with(WATERVAPOR, 
     interaction.plot(x.factor = TEMP, trace.factor = MOLE, response=OXYDIFF))
# no interaction

ggplot(data=WATERVAPOR, aes(x=TEMP, y=OXYDIFF)) + geom_point(shape=19, size=3)
ggplot(data=WATERVAPOR, aes(x=MOLE, y=OXYDIFF)) + geom_point(shape=19, size=3)



predict(oxydiff.lm, newdata=data.frame(TEMP=1300, MOLE=0.017), interval="conf")
predict(oxydiff.lm, newdata=data.frame(TEMP=1300, MOLE=0.017), interval="pred")
