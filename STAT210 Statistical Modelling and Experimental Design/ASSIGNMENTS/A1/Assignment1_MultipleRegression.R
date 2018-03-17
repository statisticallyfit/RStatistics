setwd('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A1/')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')
#source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')
#source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/INTERPRET.R')

options(digits = 3, show.signif.stars = FALSE)

hollyData <- read.table("Hollywood.txt", header=TRUE)
head(hollyData)


# part a) plotting with pairs()
pairs(hollyData, lower.panel = panel.smooth, upper.panel = panel.cor)

# Summary: 
# 1. There is a strong positive linear correlation of 0.92 between Receipts
# and Production

ggpairs(hollyData, upper=list(continuous="cor", params=c(size=10)), 
        lower=list(continuous="smooth", params=c(colour="blue")))
