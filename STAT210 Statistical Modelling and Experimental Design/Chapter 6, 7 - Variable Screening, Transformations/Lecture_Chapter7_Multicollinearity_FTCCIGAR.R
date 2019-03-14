setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 6, 7 - Variable Screening, Transformations/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R")


options(digits=10, show.signif.stars = FALSE)


cigarData <- read.table("lecturedata/FTCIGAR.txt", header=TRUE)


pairsQuantPlot(cigarData, 1:4)
# Strong correlations between nicotine and tar (r = 0.97) and between
# the Co and Nicotine (r = 0.926) and between Tar and Co (r = 0.957)
# Probably don't need tar AND nicotine, and if so, then which, and also
# the research question is asking whether we need Weight to add 
# extra information to explain the response CO? 

# in R=functions - correlation matrix. 
# But here, p-values are above the diagonal and correlations are below
# the diagonal. So p-value of cor(CO, Weight) is 0.0198
cor.prob(cigarData)



# Model
cigar.lm <- lm(CO ~ TAR + NICOTINE + WEIGHT, data=cigarData)
summary(cigar.lm)
# the t-tests are checking: once we've fitted the other predictors, do
# we or not need the other predictor? 

# SOME NONSIG t-test: Given Tar and Weight and Nicotine, need to include Tar but not
# the other two: Nicotine and Weight must be dropped. 

# OVERALL good F-test while t-tests are not significant. 

# OPPOSITE SIGNS: all the correlations are positive but here the estimated coeffs
# opposite signs. Not definitive sign of multicollinearity because it 
# depends on how all they act together, but it seems suspicious. 

# so there seems to be multicollinearity. 


# FINDING VIFS
tarX.lm <- lm(TAR ~ NICOTINE + WEIGHT, data=cigarData)
R2.tar <- summary(tarX.lm)$r.squared
VIF.tar <- 1 / (1 - R2.tar)
VIF.tar
# RULE if VIF.i > 10 then there is a problem. 

# CALCULATING ALL VIFS
library(car)
vif(cigar.lm) # tar and nicotine have high VIFs, but Weight not. 

# Which one to choose? Once tar is fitted, we don't need nicotine 
# (from summary table)


summary(cigarData[1:3])

