setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
#library(nlme)
#library(lme4)
#detach(package:lme4)
library(lattice)
library(geepack)
library(ggcorrplot) # for ggcorrplot
library(forecast) # for ggAcf

options(show.signif.stars = FALSE)