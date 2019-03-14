setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/")
library(ISLR)


collegeData <- read.csv("data/College.csv")
head(collegeData)

# a) 
fix(collegeData)
rownames(collegeData) <- collegeData[,1]
collegeData <- collegeData[, -1]
head(collegeData)


# c) 
summary(collegeData)

pairs(collegeData[,1:10])
# my function
pairsQuantPlot(collegeData, 1:3)
pairs(collegeData[,1:3])

# Produce boxplots of outstate versus private
plot(collegeData$Outstate, collegeData$Private)

ggplot(collegeData, aes(x = Private, y = Outstate)) + geom_boxplot() + 
      labs(y="Outstate university tuition", x="Private university in US")

# create qualitative variable Elite by binning Top10perc. Going to divide universities
# into two groups based on whether or not proportion of students coming from the 
# top 10$ of their high school classes exceeds 50%
Elite <- rep("No", nrow(collegeData))
Elite[collegeData$Top10perc > 50] = "Yes" # subs Yes at those indices
Elite <- as.factor(Elite)
eliteCollegeData <- data.frame(collegeData, Elite)
head(eliteCollegeData)

summary(eliteCollegeData)
# there are 78 elite universities. 

# boxplots of outstate tuition versus elite
ggplot(eliteCollegeData, aes(x = Elite, y=Outstate)) + geom_boxplot()
# (cost is higher in outstate unis for Elite unis)

# make histograms with differing number of bins for some quantitative variables
par(mfrow=c(2,2))
hist(collegeData$Top25perc, col=2, xlab="Top25perc", ylab="Count")
hist(collegeData$PhD, col=3, xlab="PhD", ylab="Count")
hist(collegeData$Grad.Rate, col=4, xlab="Grad Rate", ylab="Count")
hist(collegeData$Expend, col=5, xlab="% Expend", ylab="Count")
