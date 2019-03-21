setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A1/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')
library(ISLR)
library(MASS)
library(car)
library(caret)
library(ggplot2)

options(show.signif.stars = FALSE)


# GOAL: determine if the three job classifications appeal to different personality types

# JOB = 1 (customer service), job = 2 (mechanics), job = 3 (dispatchers)
# outdoor = measure of interest in outdoor activity
# social = sociability
# convservative = level of conservativism
jobData <- read.table("classif.txt", header=TRUE)
jobData$JOB <- factor(jobData$JOB)


# part a) exploratory plots -------------------------------------------------

cor(jobData[, c(1,2,3)])
# there does not appear to be any major pairwise correlation between the predictors. 

# Pairs plot to show correlations
pairs(jobData[,c(1,2,3)])
# As in the correlation matrix, there is no major correlation among the three predictors
# since the scatter is clouded and never strictly increasing or decreasing.



# boxplot: job vs outdoor
ggplot(jobData, aes(x=JOB, y=OUTDOOR, color=JOB)) + geom_boxplot(size=1)
# For  all jobs, there is similar iQR (variation) for outdoor scores, but outdoor
# scores are generally higher for job 2 than jobs 1 and 3. 

# boxplot: job vs social
ggplot(jobData, aes(x=JOB, y=SOCIAL, color=JOB)) + geom_boxplot(size=1)
# For all jobs, there is similar IQR in social scores but social scores are generally 
# higher for jobs 1 and 2, and lowest for job 3. 

# boxplot: job vs conservative
ggplot(jobData, aes(x=JOB, y=CONSERVATIVE, color=JOB)) + geom_boxplot(size=1)
# For job 3, conservative score is the highest, with similar score for jobs 1 and 2
# but in general the conservative score for job 2 has lower variation and higher median
# than for job 1. 




# scatterplots
ggplot(jobData, aes(x=OUTDOOR, y=SOCIAL, color=JOB)) + geom_point(size=2.5)
ggplot(jobData, aes(x=OUTDOOR, y=CONSERVATIVE, color=JOB)) + geom_point(size=2.5)
ggplot(jobData, aes(x=CONSERVATIVE, y=SOCIAL, color=JOB)) + geom_point(size=2.5)
# no clear separations between jobs for the different predictors

 

# part b) ------------------------------------------------------
# logistic - used only often for 2 classes not three, but can be used fo rmulticlass
# lda is often used for multiclass when linear decision boundary
# qda = can also be used for when variances are different between classes
# knn = most flexible method, for highly nonlinear boundaries
var(jobData$OUTDOOR)
var(jobData$SOCIAL)
var(jobData$CONSERVATIVE)


df <- data.frame(jobD)



# par c) ----------------------------------------------------
library(caTools)
set.seed(123, kind="Mersenne-Twister", normal.kind="Inversion")
sample = sample.split(jobData, SplitRatio=0.6)
jobTrainData <- subset(jobData, sample==TRUE)
jobTestData <- subset(jobData, sample==FALSE)


# part d) -) ----------------------------------------------------

# lda model ----

# qda model ---
# knn model ---
