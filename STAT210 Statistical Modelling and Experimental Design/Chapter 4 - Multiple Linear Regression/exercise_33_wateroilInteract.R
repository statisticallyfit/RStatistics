setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R', echo=FALSE)
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/INTERPRET.R', echo=FALSE)

load("data/Exercises and Examples/WATEROIL.Rdata")

wateroil.interact.lm <- lm(VOLTAGE ~ VOLUME + SALINITY + SURFAC + 
                                 VOLUME*SALINITY + VOLUME*SURFAC, data=WATEROIL)
summary(wateroil.interact.lm)
