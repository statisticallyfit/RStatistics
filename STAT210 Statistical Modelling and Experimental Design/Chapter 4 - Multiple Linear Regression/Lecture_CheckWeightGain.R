setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 4 - Multiple Linear Regression")

chickData <- read.table("chickWeightGain.txt", header=TRUE)
head(chickData)

library(ggplot2)

ggplot(chickData, aes(x=diet, y=wtgain, group = diet, color=diet)) + 
      geom_boxplot()

chick.lm <- lm(wtgain ~ diet, data=chickData)
summary(chick.lm)
