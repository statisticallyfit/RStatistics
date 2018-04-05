setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/SOCWORK.Rdata")
library(ggplot2)
library(ggfortify)
options(digits=5)



# model 2 quadratic
social.quad.lm <- lm(SALARY ~ EXP + I(EXP^2), data=SOCWORK)
residualFittedPlot(social.quad.lm)

#sample1 <- SOCWORK[SOCWORK$EXP < 20, ]
#sample2 <- SOCWORK[SOCWORK$EXP >= 20, ]
#nrow(sample2)
#mod1 <- lm(SALARY ~ EXP + I(EXP^2), data=sample1)
#mod2 <- lm(SALARY ~ EXP + I(EXP^2), data=sample2)

f <- as.formula("SALARY ~ EXP + I(EXP^2)")
HomoskedasticityRegressionTest(theFormula = f, data=SOCWORK, xName="EXP",xSplit=19)



