setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A1/")
#source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
#source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
#source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')
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

# Pairs plot to show correlations
pairs(jobData[,c(1,2,3)])
# As in the correlation matrix, there is no major correlation among the three predictors
# since the scatter is clouded and never strictly increasing or decreasing.

cor(jobData[, c(1,2,3)])
# there does not appear to be any major pairwise correlation between the predictors. 

# boxplot: job vs outdoor
ggplot(jobData, aes(x=JOB, y=OUTDOOR, color=JOB)) + geom_boxplot(size=1) + 
      ggtitle("Outdoor Score Across Job Levels")
# For  all jobs, there is similar iQR (variation) for outdoor scores, but outdoor
# scores are generally higher for job 2 than jobs 1 and 3. 

# boxplot: job vs social
ggplot(jobData, aes(x=JOB, y=SOCIAL, color=JOB)) + geom_boxplot(size=1) + 
      ggtitle("Social Score Across Job Levels")
# For all jobs, there is similar IQR in social scores but social scores are generally 
# higher for jobs 1 and 2, and lowest for job 3. 

# boxplot: job vs conservative
ggplot(jobData, aes(x=JOB, y=CONSERVATIVE, color=JOB)) + geom_boxplot(size=1) + 
      ggtitle("Conservative Score Across Job Levels")
# For job 3, conservative score is the highest, with similar score for jobs 1 and 2
# but in general the conservative score for job 2 has lower variation and higher median
# than for job 1. 

# scatterplots
#ggplot(jobData, aes(x=OUTDOOR, y=SOCIAL, color=JOB)) + geom_point(size=2.5)
#ggplot(jobData, aes(x=OUTDOOR, y=CONSERVATIVE, color=JOB)) + geom_point(size=2.5)
#ggplot(jobData, aes(x=CONSERVATIVE, y=SOCIAL, color=JOB)) + geom_point(size=2.5)
# no clear separations between jobs for the different predictors
# so logistic is good to use
 

# part b) ------------------------------------------------------
# logistic - used only often for 2 classes not three, but can be used fo rmulticlass
# lda is often used for multiclass when we have  a linear decision boundary
# qda = can also be used for when variances are different between classes
# knn = most flexible method, for highly nonlinear boundaries

# Checking lda and qda assumptions: are predictors within each class normally distributed?
df.job1 = subset(jobData, JOB == "1")
df.job2 = subset(jobData, JOB == "2")
df.job3 = subset(jobData, JOB == "3")

shapiro.test(df.job1$OUTDOOR) # no reason to reject normality
shapiro.test(df.job2$OUTDOOR) # no reason to reject normality
shapiro.test(df.job3$OUTDOOR) # marginally non-normal for outdoor, job=3

shapiro.test(df.job1$SOCIAL)  # no reason to reject normality
shapiro.test(df.job2$SOCIAL) # no reason to reject normality
shapiro.test(df.job3$SOCIAL)# no reason to reject normality

shapiro.test(df.job1$CONSERVATIVE)  # no reason to reject normality
shapiro.test(df.job2$CONSERVATIVE)  # no reason to reject normality
shapiro.test(df.job3$CONSERVATIVE) # no reason to reject normality


# Checking lda and quda assumptions: if variances are unequal per class, then QDA is best.
sapply(list(OUT1=df.job1$OUTDOOR, OUT2=df.job2$OUTDOOR, OUT3=df.job3$OUTDOOR,
            SOC1=df.job1$SOCIAL, SOC2=df.job2$SOCIAL, SOC3=df.job3$SOCIAL,
            CNV1=df.job1$CONSERVATIVE, CNV2=df.job2$CONSERVATIVE, CNV3=df.job3$CONSERVATIVE), var)
# variances are mostly the same among classes and predictors, so LDA seems fine
# except for job 1, conservative predictor has much lower variance than outdoor, job 1. 


# part c) ----------------------------------------------------
library(caTools)


set.seed(123, kind="Mersenne-Twister", normal.kind="Inversion")

sample = sample.split(jobData, SplitRatio=0.6)
jobTrainData <- subset(jobData, sample==TRUE)
jobTestData <- subset(jobData, sample==FALSE)
jobTrain.X <- jobTrainData[,c(1,2,3)]
jobTest.X <- jobTestData[, c(1,2,3)]
jobTrain.Y <- jobTrainData[,4]
jobTest.Y <- jobTestData[,4]

# part d) and e) ----------------------------------------------------

############### LDA MODEL
job.lda <- lda(JOB ~ OUTDOOR + CONSERVATIVE + SOCIAL, data=jobTrainData)
job.lda

pred.train.lda <- predict(job.lda)
pred.test.lda <- predict(job.lda, jobTestData)

# confusion matrices for train and test data
cm.train.lda <- table(PredictedJob=pred.train.lda$class, TrueJob=jobTrain.Y)
cm.train.lda
cm.test.lda <- table(PredictedJob=pred.test.lda$class, TrueJob=jobTest.Y)
cm.test.lda

# confusion matrices for train and test data
#confusionMatrix(data=pred.train.lda$class, reference=jobTrainData$JOB)
#confusionMatrix(data=pred.test.lda$class, reference=jobTestData$JOB)

# Accuracy = percentage of correct predictions 
train.acc.lda <- mean(pred.train.lda$class == jobTrain.Y); train.acc.lda # 0.7482993
test.acc.lda <- mean(pred.test.lda$class == jobTest.Y); test.acc.lda # 0.76288
train.error.lda <-1 - train.acc.lda
test.error.lda <-1 - test.acc.lda

############### QDA MODEL
job.qda <- qda(JOB ~ OUTDOOR + CONSERVATIVE + SOCIAL, data=jobTrainData)
job.qda

pred.train.qda <- predict(job.qda)
pred.test.qda <- predict(job.qda, jobTestData)

# confusion matrices for train and test data
cm.train.qda <- table(PredictedJob=pred.train.qda$class, TrueJob=jobTrain.Y)
cm.train.qda
cm.test.qda <- table(PredictedJob=pred.test.qda$class, TrueJob=jobTest.Y)
cm.test.qda

#confusionMatrix(data=pred.train.qda$class, reference=jobTrainData$JOB)
#confusionMatrix(data=pred.test.qda$class, reference=jobTestData$JOB)

# Accuracy = percentage of correct predictions
train.acc.qda <- mean(pred.train.qda$class == jobTrain.Y); train.acc.qda # 0.7415
test.acc.qda <- mean(pred.test.qda$class == jobTest.Y); test.acc.qda # 0.7319588
train.error.qda <- 1 - train.acc.qda
test.error.qda <- 1 - test.acc.qda

############### KNN MODEL 
library(class)
set.seed(1)

knnLoop <- function(n){
      nulls = rep(NA, n)
      
      rates.df <- data.frame(Index=1:n, TrainAccuracyRate=nulls, TestAccuracyRate=nulls,
                            TrainErrorRate=nulls, TestErrorRate=nulls)
      model.list <- vector("list", length=n)
      matrix.list <- vector("list", length=n)
      
      for(i in 1:n){
            job.train.knn <- knn(train=jobTrain.X, test=jobTrain.X, cl=jobTrain.Y, k=i)
            job.test.knn <- knn(train=jobTrain.X, test=jobTest.X, cl=jobTrain.Y, k=i)
            
            # confusion matrices for train and test data
            cm.train.knn <- table(PredictedJob=job.train.knn, TrueJob=jobTrain.Y)
            cm.test.knn <- table(PredictedJob=job.test.knn, TrueJob=jobTest.Y)
            
            model.list[[i]]$TrainModel <- job.train.knn
            model.list[[i]]$TestModel <- job.test.knn
            matrix.list[[i]]$TrainConfusionMatrix <- cm.train.knn
            matrix.list[[i]]$TestConfusionMatrix <- cm.test.knn
            
            # Print out ################################################
            #cat(paste("\nTrain Confusion matrix for K = \n" , i, sep=""))
            #print(cm.train.knn)
            #cat(paste("\nTest Confusion matrix for K = \n" , i, sep=""))
            #print(cm.test.knn)
            #cat("\n---------------") # space line
            ##############################################################
            
            # Accuracy = percentage of correct predictions
            rates.df$TrainAccuracyRate[i] <- mean(job.train.knn == jobTrain.Y)
            rates.df$TestAccuracyRate[i] <- mean(job.test.knn == jobTest.Y)
      }
      
      # Error rates = percentage of incorrect predictions
      rates.df$TrainErrorRate <- 1 - rates.df$TrainAccuracyRate
      rates.df$TestErrorRate <- 1 - rates.df$TestAccuracyRate
      
      return(list(KNNModels=model.list, KNNConfusionMatrices=matrix.list,
                  KNNPerformance=rates.df))
}

results = knnLoop(20)
results

# The K = 16 knn model has lowest test error rate. 
k = which.min(results$KNNPerformance$TestErrorRate); k
which.max(results$KNNPerformance$TestAccuracyRate)


# confusion matrix for the kth model (best)
cm.train.knn.best <- results$KNNConfusionMatrices[[k]]$TrainConfusionMatrix
cm.train.knn.best
cm.test.knn.best <- results$KNNConfusionMatrices[[k]]$TestConfusionMatrix
cm.test.knn.best


# part f) which model has lowest test error rate
train.acc.knn.best <- results$KNNPerformance$TrainAccuracyRate[k]
test.acc.knn.best <- results$KNNPerformance$TestAccuracyRate[k]
train.error.knn.best <- results$KNNPerformance$TrainErrorRate[k]
test.error.knn.best <- results$KNNPerformance$TestErrorRate[k]

overallSummaryDf <- data.frame(TrainAccuracyRate=c(train.acc.lda, train.acc.qda, train.acc.knn.best),
                               TestAccuracyRate =c(test.acc.lda, test.acc.qda, test.acc.knn.best),
                               TrainErrorRate=c(train.error.lda, train.error.qda, train.error.knn.best),
                               TestErrorRate=c(test.error.lda, test.error.qda, test.error.knn.best))
rownames(overallSummaryDf) <- c("LDA", "QDA", "KNN_best")
overallSummaryDf

# The KNN with K = 16 has lowest test error rate. 