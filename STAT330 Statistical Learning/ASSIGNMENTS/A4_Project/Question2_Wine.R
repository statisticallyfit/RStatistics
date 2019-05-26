setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A4_Project")

library(ggplot2)
#library(corrplot)
library(ggcorrplot)
library(plyr)
library(dplyr)
library(purrr)
library(gridExtra)
library(class)          # KNN
library(randomForest)   # random forest

# part a) exploratory plots

wineRaw <- read.csv("winequality-red.csv", sep=";", header=TRUE)
head(wineRaw)

any(is.na(wineData))
p <- ncol(wineData) - 1; p # number of predictors
N <- nrow(wineData); N


# Create the classes: 
# low = quality = 3 to 5 inclusive, high = qualit is 6 and above
wineData <- wineRaw
wineData$quality <- as.factor(ifelse(wineRaw$quality <= 5, "low", "high"))

# Observe how groups are split, how many in which group
table(wineData$quality)
# high = 855, low = 744 observations so generally well balanced. 

# Plot the data to see if classes are well-separated
mu <- ddply(wineData, "quality", summarise, 
            fixed.acidity.mu=mean(fixed.acidity),
            volatile.acidity.mu = mean(volatile.acidity),
            citric.acid.mu = mean(citric.acid), 
            residual.sugar.mu = mean(residual.sugar),
            chlorides.mu = mean(chlorides),
            free.sulfur.dioxide.mu = mean(free.sulfur.dioxide),
            total.sulfur.dioxide.mu = mean(total.sulfur.dioxide),
            density.mu = mean(density),
            pH.mu = mean(pH),
            sulphates.mu = mean(sulphates),
            alcohol.mu = mean(alcohol))
head(mu)


p.fixed <- ggplot(data=wineData, aes(x=fixed.acidity, fill=quality)) + 
      geom_density(alpha=0.3, size=1) +
      geom_vline(data=mu, aes(xintercept=fixed.acidity.mu, color=quality), 
                 size=1, linetype="dashed") + 
      ggtitle("Quality of Fixed Acidity")

p.vol <- ggplot(data=wineData, aes(x=volatile.acidity, fill=quality)) + 
      geom_density(alpha=0.3, size=1) +
      geom_vline(data=mu, aes(xintercept=volatile.acidity.mu, color=quality), 
                 size=1, linetype="dashed") + 
      ggtitle("Quality of Volatile Acidity")

p.citric <- ggplot(data=wineData, aes(x=citric.acid, fill=quality)) + 
      geom_density(alpha=0.3, size=1) +
      geom_vline(data=mu, aes(xintercept=citric.acid.mu, color=quality), 
                 size=1, linetype="dashed") + 
      ggtitle("Quality of Citric Acid")

p.sugar <- ggplot(data=wineData, aes(x=residual.sugar, fill=quality)) + 
      geom_density(alpha=0.3, size=1) +
      geom_vline(data=mu, aes(xintercept=residual.sugar.mu, color=quality), 
                 size=1, linetype="dashed") + 
      ggtitle("Quality of Residual Sugar")

# ---
p.chlo <- ggplot(data=wineData, aes(x=chlorides, fill=quality)) + 
      geom_density(alpha=0.3, size=1) +
      geom_vline(data=mu, aes(xintercept=chlorides.mu, color=quality), 
                 size=1, linetype="dashed") + 
      ggtitle("Quality of Chlorides")

p.freesulf <- ggplot(data=wineData, aes(x=free.sulfur.dioxide, fill=quality)) + 
      geom_density(alpha=0.3, size=1) +
      geom_vline(data=mu, aes(xintercept=free.sulfur.dioxide.mu, color=quality), 
                 size=1, linetype="dashed") + 
      ggtitle("Quality of Free Sulfur Dioxides")

p.totsulf <- ggplot(data=wineData, aes(x=total.sulfur.dioxide, fill=quality)) + 
      geom_density(alpha=0.3, size=1) +
      geom_vline(data=mu, aes(xintercept=total.sulfur.dioxide.mu, color=quality), 
                 size=1, linetype="dashed") + 
      ggtitle("Quality of Total Sulfur Dioxide")

p.density <- ggplot(data=wineData, aes(x=density, fill=quality)) + 
      geom_density(alpha=0.3, size=1) +
      geom_vline(data=mu, aes(xintercept=density.mu, color=quality), 
                 size=1, linetype="dashed") + 
      ggtitle("Quality of Wine Density")

# ---
p.pH <- ggplot(data=wineData, aes(x=pH, fill=quality)) + 
      geom_density(alpha=0.3, size=1) +
      geom_vline(data=mu, aes(xintercept=pH.mu, color=quality), 
                 size=1, linetype="dashed") + 
      ggtitle("Quality of PH")

p.sulph <- ggplot(data=wineData, aes(x=sulphates, fill=quality)) + 
      geom_density(alpha=0.3, size=1) +
      geom_vline(data=mu, aes(xintercept=sulphates.mu, color=quality), 
                 size=1, linetype="dashed") + 
      ggtitle("Quality of Sulphates")

p.alc <- ggplot(data=wineData, aes(x=alcohol, fill=quality)) + 
      geom_density(alpha=0.3, size=1) +
      geom_vline(data=mu, aes(xintercept=alcohol.mu, color=quality), 
                 size=1, linetype="dashed") + 
      ggtitle("Quality of Alcohol")

# Fixed acidity, volatile, citric acid, residual sugar
grid.arrange(p.fixed, p.vol, p.citric, p.sugar)
grid.arrange(p.chlo, p.freesulf, p.totsulf, p.density)
grid.arrange(p.pH, p.sulph, p.alc)
#OR in larger groups
#grid.arrange(p.fixed, p.vol, p.citric, p.sugar,  p.chlo, p.freesulf)
#grid.arrange(p.totsulf, p.density, p.pH, p.sulph, p.alc)

# Generally, classes are not well separated according to each variable. The means are 
# similar and the density by quality is not well separated for quality = low versus high. 


# NOnlinear decision boundary?
# Scatterplots: to see what kind of decision boundary there is
ggplot(data=wineData, aes(x=fixed.acidity, y=free.sulfur.dioxide, colour=quality)) + 
      geom_point(size=1)
# generally no clear separation, so use KNN for most flexible model fit


# Correlation matrix - need to do PCA (dimension reduction) before analysis?
cor(wineData[,-12])
ggcorrplot(cor(wineData[,-12]), lab=TRUE, ggtheme=theme_gray, type="lower")
# no very few highly correlated variables, so no PCA


# part b) fitting the models

# First prepare the test / training data
set.seed(124)

iTrain <- sample(x=1:N, size=(2/3)*N) # training indices
wineTrain <- wineData[iTrain, ]
wineTest <- wineData[-iTrain, ]

X.train <- as.matrix(wineTrain[, -12]) # all predictors from train
X.test <- as.matrix(wineTest[, -12]) # all predictors from test
Y.train <- as.matrix(wineTrain$quality) # 
Y.test <- as.matrix(wineTest$quality)

X.train2 <- wineTrain[,-12]
X.test2 <- wineTest[,-12]
Y.train2 <- as.factor(wineTrain$quality)
Y.test2 <- as.factor(wineTest$quality)


# MODEL 1: KNN ============================================================================
# data are not well separated, which is when logistic/knn regression performs best. 
# SVM only does well when classes have strong separation. 
# Logistic regr. is not good when boundary is nonlinear

set.seed(8934)

knnAnalysis <- function(xtrain, xtest, ytrain, ytest, numIter = 20 ){
      #n <- nrow(ytrain) + nrow(ytest) # number of observations
      nulls = rep(NA, numIter)
      
      rates.df <- data.frame(Index=1:numIter, TrainAccuracyRate=nulls, TestAccuracyRate=nulls,
                             TrainErrorRate=nulls, TestErrorRate=nulls)
      knnModels <- vector("list", length=numIter)
      confMats <- vector("list", length=numIter)
      
      for(i in 1:numIter){
            trainModel.knn <- knn(train=xtrain, test=xtrain, cl=ytrain, k=i)
            model.knn <- knn(train=xtrain, test=xtest, cl=ytrain, k=i)
            
            # confusion matrices for train and test data
            cm.train.knn <- table(Pred=trainModel.knn, Truth=ytrain)
            cm.test.knn <- table(Pred=model.knn, Truth=ytest)
            
            knnModels[[i]]$TrainModel <- trainModel.knn
            knnModels[[i]]$TestModel <- model.knn
            confMats[[i]]$TrainConfusionMatrix <- cm.train.knn
            confMats[[i]]$TestConfusionMatrix <- cm.test.knn
            
            # Accuracy = percentage of correct predictions
            rates.df$TrainAccuracyRate[i] <- mean(trainModel.knn == ytrain)
            rates.df$TestAccuracyRate[i] <- mean(model.knn == ytest)
      }
      
      # Error rates = percentage of incorrect predictions
      rates.df$TrainErrorRate <- 1 - rates.df$TrainAccuracyRate
      rates.df$TestErrorRate <- 1 - rates.df$TestAccuracyRate
      
      return(list(KNNModels=knnModels, KNNConfusionMatrices=confMats,
                  KNNPerformance=rates.df))
}

results = knnAnalysis(xtrain=X.train, xtest=X.test, ytrain=Y.train, ytest=Y.test)
results

# The K = 1 knn model has lowest test error rate. 
k = which.min(results$KNNPerformance$TestErrorRate); k
which.max(results$KNNPerformance$TestAccuracyRate)


# confusion matrix for the kth model (best)
cm.train.knn.best <- results$KNNConfusionMatrices[[k]]$TrainConfusionMatrix
cm.train.knn.best
cm.test.knn.best <- results$KNNConfusionMatrices[[k]]$TestConfusionMatrix
cm.test.knn.best


# hich model has lowest test error rate
train.acc.knn.best <- results$KNNPerformance$TrainAccuracyRate[k]; train.acc.knn.best
#  1
test.acc.knn.best <- results$KNNPerformance$TestAccuracyRate[k]; test.acc.knn.best
#  0.69606
train.error.knn.best <- results$KNNPerformance$TrainErrorRate[k]; train.error.knn.best
# 0
test.error.knn.best <- results$KNNPerformance$TestErrorRate[k]; test.error.knn.best
# 0.30394



# MODEL 2: RANDOM FOREST ====================================================================
set.seed(458)

hyperGrid <- expand.grid(
      Mtry       = 1:p,
      NodeSize   = seq(3, 9, by = 2)
)

wineRandForests <- vector("list", length=nrow(hyperGrid))

for(i in 1:nrow(hyperGrid)) {
      model <- randomForest(X.train2, y=Y.train2, xtest=X.test2, ytest=Y.test2,
                            ntree           = 500,
                            oob.error = TRUE,
                            mtry            = hyperGrid$Mtry[i],
                            nodesize        = hyperGrid$NodeSize[i])
      model$test$err.rate <- data.frame(model$test$err.rate)
      wineRandForests[[i]]  <- model 
      'wineRandForests[[i]]  <- randomForest(
            formula         = quality ~ ., 
            data            = wineTrain, 
            ntree           = 500,
            mtry            = hyperGrid$Mtry[i],
            nodesize        = hyperGrid$NodeSize[i],
            sampsize        = hyperGrid$SampleFrac[i] # fraction of observations to sample
            
      )'
      'wineRandForests[[i]]  <- ranger(
            formula         = quality ~ ., 
            data            = wineTrain, 
            num.trees           = 500,
            mtry            = hyperGrid$Mtry[i],
            min.node.size        = hyperGrid$NodeSize[i],
            sample.fraction    = hyperGrid$SampleFrac[i], # fraction of observations to sample
            seed=123
      )'
}


'
# function to get optimal number of trees by using OOB error
getBestNumTrees.rf <- function(fit.rf) {
      # get index of minimum test error (this is the number of trees)
      ntree    <- which.min(fit.rf$err.rate[,"OOB"])
}
# function to get minimum error
getMinOOBError.rf <- function(fit.rf) {
      minOOB    <- min(fit.rf$err.rate[,"OOB"])
}'



getOob <- function(fit.rng) { fit.rng$prediction.error }
getTestErr <- function(fit.rng) {mean(fit.rng$predictions != Y.test[,]) }

tableWine = hyperGrid %>%
      mutate(
            oobErr = purrr::map_dbl(wineRandForests, getOob),
            testErr = purrr::map_dbl(wineRandForests, getTestErr))


# arrange sorts the resulting matrix from above according to minimum error
tableWine.ord <- tableWine %>% arrange(testErr) 
# Describe
tableWine.ord

#Get the id of the best model
BM <- which.min(tableWine$testErr); BM
# 6

# Get the optimal tuning parameters
bestNumTrees.forest <- tableBikeForest$numTrees[BM]; bestNumTrees.forest
# 275
bestTestError.forest <- tableBikeForest$error[BM]; bestTestError.forest
# 129695.2
bestMtry.forest <- tableBikeForest$mtry[BM]; bestMtry.forest
# 6
# random forest with m = 6 is the best model

# Fit the optimal model on the whole data
set.seed(1)
bike.optimal.forest <- randomForest(Casual ~., data=data, mtry=bestMtry.forest,
                                    ntree=bestNumTrees.forest, importance=TRUE)

#Describe
bike.optimal.forest # has training error performance there


# Plot the test errors for the  best random forest model
df <- data.frame(TestMSE = wineRandForests[[BM]]$test$mse, NumTrees=1:500)

ggplot(data=df,aes(x=NumTrees)) + 
      geom_line(aes(y=TestMSE), colour="deeppink", size=1)  + 
      geom_point(x=bestNumTrees.forest, y=bestTestError.forest, color="black", size=4)+
      geom_vline(aes(xintercept = bestNumTrees.forest), color="black", linetype="dashed",size=1) +
      ggtitle("Test Errors for Random Forest Cross-Validation Model")
