setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/GASTURBINE.Rdata")

turbine.reg.lm <- lm(HEATRATE ~ RPM + INLETTEMP + EXHTEMP + CPRATIO + AIRFLOW, 
                     data = GASTURBINE)
summary(turbine.reg.lm)

turbine.anova <- anova(turbine.reg.lm); turbine.anova
turbine.anova$Df
turbine.anova$`Sum Sq`
turbine.anova$`Mean Sq`
turbine.anova$`F value`
turbine.anova$`Pr(>F)`

turbine.anova

# These are all the same SST
sst <- sum(turbine.anova$`Sum Sq`); sst
SST(turbine.reg.lm)
SSyy(turbine.reg.lm)

# These are all the same SSEs
sse <- SSE(turbine.reg.lm); sse
turbine.anova$`Sum Sq`[6]

# standard error of regression
s <- standardErrorOfRegression(turbine.reg.lm); s
sqrt(sse / 61)
summ <- summary(turbine.reg.lm); summ$sigma

meanCI(turbine.reg.lm, x.values=c(1,1,1,1,1))
predictCI(turbine.reg.lm, x.values=c(1,1,1,1,1))
