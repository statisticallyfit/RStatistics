setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A2/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')


library(MASS)
options(digits=10, show.signif.stars = F)




# part a)
insectData <- read.table("insect.txt", header=TRUE)
# choosing only the rows without the Count = 0
insectData.NoZero <- insectData[insectData$Count != 0, ]



# part b) 
insect.sqrt.lm <- lm(sqrt(Count) ~ Ispray, data=insectData.NoZero)
boxcox(Count ~ Ispray, , data=insectData.NoZero, lambda=seq(from=0, to=1, by=0.01))


# part c)
# the squared root trans is apprpriate because it is inside the 95% CI