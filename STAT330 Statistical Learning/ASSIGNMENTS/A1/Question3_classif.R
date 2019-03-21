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


# TODO: check normality of each class? df <- data.frame(jobD)



# par c) ----------------------------------------------------
library(caTools)
set.seed(123, kind="Mersenne-Twister", normal.kind="Inversion")
sample = sample.split(jobData, SplitRatio=0.6)
jobTrainData <- subset(jobData, sample==TRUE)
jobTestData <- subset(jobData, sample==FALSE)


# part d) and e) ----------------------------------------------------

############### LDA MODEL
job.lda <- lda(JOB ~ OUTDOOR + CONSERVATIVE + SOCIAL, data=jobTrainData)
job.lda

pred.train.lda <- predict(job.lda)
pred.test.lda <- predict(job.lda, jobTestData)

# confusion matrices for train and test data
cm.train.lda <- table(PredictedJob=pred.train.lda$class, TrueJob=jobTrainData$JOB)
cm.test.lda <- table(PredictedJob=pred.test.lda$class, TrueJob=jobTestData$JOB)
# marginalTable(lda.tbl)

# confusion matrices for train and test data
confusionMatrix(data=pred.train.lda$class, reference=jobTrainData$JOB)
confusionMatrix(data=pred.test.lda$class, reference=jobTestData$JOB)

# Accuracy = percentage of correct predictions is 76.28%
accuracy.train.lda <- mean(pred.train.lda$class == jobTrainData$JOB); accuracy.train.lda
accuracy.test.lda <- mean(pred.test.lda$class == jobTestData$JOB); accuracy.test.lda

train.error.lda <-1 - accuracy.train.lda
test.error.lda <-1 - accuracy.test.lda

############### QDA MODEL
job.qda <- qda(JOB ~ OUTDOOR + CONSERVATIVE + SOCIAL, data=jobTrainData)
job.qda

pred.train.qda <- predict(job.qda)
pred.test.qda <- predict(job.qda, jobTestData)

# confusion matrices for train and test data
cm.train.qda <- table(PredictedJob=pred.train.qda$class, TrueJob=jobTrainData$JOB)
cm.test.qda <- table(PredictedJob=pred.test.qda$class, TrueJob=jobTestData$JOB)

confusionMatrix(data=pred.train.qda$class, reference=jobTrainData$JOB)
confusionMatrix(data=pred.test.qda$class, reference=jobTestData$JOB)

# Accuracy = percentage of correct predictions is 76.28%
accuracy.train.qda <- mean(pred.train.qda$class == jobTrainData$JOB); accuracy.train.qda
accuracy.test.qda <- mean(pred.test.lda$class == jobTestData$JOB); accuracy.test.qda
train.error.qda <-1 - accuracy.train.qda
test.error.qda <-1 - accuracy.test.qda

############### KNN MODEL 
library(class)
jobTrain.X <- jobTrainData[,c(1,2,3)]
jobTest.X <- jobTestData[, c(1,2,3)]
jobTrain.Y <- jobTrainData[,4]
jobTest.Y <- jobTestData[,4]

knnLoop <- function(){
      set.seed(1)
      knn.accs = c()
      knn.errs = c()
      
      for(i in 1:20){
            job.knn <- knn(train=jobTrain.X, test=jobTest.X, cl=jobTrain.Y, k=i)
            
            # confusion matrices for train and test data
            cm.test.knn <- table(PredictedJob=job.knn, TrueJob=jobTest.Y)
            cat(paste("\nConfusion matrix for K = " , i, sep=""))
            cat("\n")
            print(cm.test.knn)
            cat("\n") # space line
            # TOOD: cm.train.knn <- table(PredictedJob=job.knn, TrueJob=jobTrain.Y)
            
            # Accuracy = percentage of correct predictions
            #accuracy.train.qda <- mean(pred.train.qda$class == jobTrainData$JOB); accuracy.train.qda
            accuracy.test.knn <- mean(job.knn == jobTestData$JOB); accuracy.test.knn
            knn.accs <- c(knn.accs, accuracy.test.knn)
            cat(paste("Accuracy = ", accuracy.test.knn, "\n"))
            
            # ERror rates = percentage of incorrect predictions
            test.error.knn <- 1 - accuracy.test.knn
            knn.errs <- c(knn.errs, test.error.knn)
            cat(paste("Test Error = ", test.error.knn, "\n"))
      }
      
      knn.results <- data.frame(Index = 1:20, Accuracy=knn.accs, TestErrors=knn.errs)
      
      return(invisible(knn.results))
}

results = knnLoop()
results

k = which.min(results$TestErrors); k
which.max(results$Accuracy)

accuracy.test.knn14 = results$Accuracy[k]
test.error.knn14 = results$TestErrors[k]


# The knn model with K = 14 has the lowest test error rate, and therefore the highest
# accuracy rate


# part f) which model has lowest test error rate

overallSummaryDf <- data.frame(AccuracyTrain=c(accuracy.train.lda, accuracy.train.qda, NA),
                               AccuracyTest =c(accuracy.test.lda, accuracy.test.qda, accuracy.test.knn14),
                               ErrorTrain=c(train.error.lda, train.error.qda, NA),
                               ErrorTest=c(test.error.lda, test.error.qda, test.error.knn14))
rownames(overallSummaryDf) <- c("LDA", "QDA", "KNN_14")
overallSummaryDf

# All the models have the same test error rate, and LDA has minimal train error rate