setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/")
options(show.signif.stars = FALSE)

library(ggplot2)

bikeData <- read.csv("bike.csv", header=TRUE)


# part a) histogram of casual users (number of casual users per day, the response variable)

# note: breaks = 15 implies 15+1 = 16 classes
numClasses = 16
# formula for calculating binwidth is: (max - min) / number of classes = class width
# Calculating: 
theBinWidth = round(diff(range(bikeData$Casual)) / numClasses); theBinWidth

ggplot(data=bikeData, aes(x=Casual)) + 
      geom_histogram(color="white", fill="dodgerblue",binwidth=theBinWidth)

# Or can do it with regular R
hist(bikeData$Casual, breaks=15)

# Distribution is highly right-skewed with long right tail (skewness would be > 0)
# This means over the 200 days, there are more likely to be few casual bike users
# than many casual bike users. This is shown by the fact that most of the distribution
# is gathered on the left, where the response has low value, and there are few
# high values of casual users, shown by the long right tail. 

# part b) --------------------------------------------------

pairs(bikeData[,1:8]) # include all predictors but the last, since Casual = response

# There seems to be no correlation between variables since either the plots contain
# a cloud of dots, with no linear pattern, or they are vertical series or horizontal
# series of dots, again indicating no linear correlation. 


# part c) --------------------------------------------------
