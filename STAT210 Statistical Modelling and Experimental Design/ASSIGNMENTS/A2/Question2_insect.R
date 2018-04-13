setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A2/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')


library(MASS)
options(digits=10, show.signif.stars = F)




# part a)
insectData <- read.table("insect.txt", header=TRUE)
# choosing only the rows without the Count = 0
insectData.NoZero <- insectData[insectData$Count != 0, ]



# part b) 
insect.lm <- lm(Count ~ Ispray, data=insectData.NoZero)
library(MASS)
par(mfrow=c(1,1))
boxcox(Count ~ Ispray, data=insectData.NoZero, lambda=seq(from=0, to=1, by=0.01))

library(lindia)
g <- gg_boxcox(insect.lm, scale.factor=1)


cookGraph <- function(fit){
  data <- fit$model 
  data$Obs <- 1:nrow(data)
  cs <- cooks.distance(fit)
  
  ggplot(data, aes(x=Obs, y=cs)) + 
    geom_point() + geom_linerange(ymin=0, ymax=cs) + 
    scale_x_continuous("Observation Number") +
    scale_y_continuous("Cook's distance") +
    ggtitle("Cook's Distance")
}

