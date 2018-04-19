setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 9 - Logistic Regression/")

library(ggplot2)


# source: 
# https://www.fromthebottomoftheheap.net/2017/05/01/glm-prediction-intervals-i/

leafData <- read.table("darlington.txt", header=TRUE)
leafData <- setNames(leafData, nm=c("LeafHeight", "Visited"))
leafData <- transform(leafData, Visited=as.logical(Visited))
is.factor(leafData$Visited)
head(leafData)

