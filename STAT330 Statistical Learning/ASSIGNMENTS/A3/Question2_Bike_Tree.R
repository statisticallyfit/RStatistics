setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A3")

#library(tree)

library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging
#library(ggplot2)
library(gridExtra)
library(randomForest)


# Read in the data
data <- read.csv("bike.csv", header=TRUE)
data <- data[, -1] # removing the Date column so that we don't get more than 32
# factor levels. 
p <- ncol(data) - 1; # p is the number of predictors

# Make the categorical variables be factors
data$Weekend <- as.factor(data$Weekend)
data$Weather <- as.factor(data$Weather)
data$Year <- as.factor(data$Year)


# First split the data in train /test set
set.seed(1)
N <- nrow(data)
iTrain <- sample(1:N, size=0.6 * N) # ratio of 0.6 for train data, and 0.4 for test data
bikeTrain <- data[iTrain, ]
bikeTest <- data[-iTrain, ]

X.train <- bikeTrain[,-8]
Y.train <- bikeTrain$Casual
Y.test <- bikeTest$Casual
X.test <- bikeTest[,-8]




# part a) fit regression tree, response = casual ---------------------------------------------
set.seed(1)

# Use cross-validation to get best number of leaves
      # Cost complexity pruning is used to select a sequence of trees for consideration. 
      # cv.tree() reports number of leaves of each tree considered (leaves = size) and also the
      # corresponding error rate and the value of the cost-complexity parameter used (k = alpha)

hyperGrid <- expand.grid(
      minsplit = seq(5, 20, 1),
      maxdepth = seq(8, 15, 1)
)

bikeRegrTrees <- list()
for (i in 1:nrow(hyperGrid)) {
      
      # train a model and store in the list
      bikeRegrTrees[[i]] <- rpart(
            formula = Casual ~ .,
            data    = bikeTrain,
            method  = "anova",
            # get minsplit, maxdepth values at row i
            control = list(minsplit = hyperGrid$minsplit[i], 
                           maxdepth = hyperGrid$maxdepth[i])
      )
}

# Tuning parameter = alpha: controls tradeoff between a subtree's fit 
# vs. its complexity. 
# function to get optimal cp (alpha) of a given tree
getBestAlpha <- function(tree.fit) {
      # get index of minimum test error
      min    <- which.min(tree.fit$cptable[, "xerror"])
      # get the ALPHA corresponding to model with min test error. 
      cp <- tree.fit$cptable[min, "CP"] 
}

# function to get minimum error of a given tree
getMinError <- function(tree.fit) {
      # get the index  of minimum test error
      min    <- which.min(tree.fit$cptable[, "xerror"])
      # get the actual minimum test error
      xerror <- tree.fit$cptable[min, "xerror"] 
}


tableBikeRegr <- hyperGrid %>%
      # mutate adds these vector columns (list of alphas and errors) 
      # to the hyper grid
      mutate(
            # map_dbl(x, f) applies function f elementwise to vector x
            # and returns a vector
            alpha    = purrr::map_dbl(bikeRegrTrees, getBestAlpha),
            error = purrr::map_dbl(bikeRegrTrees, getMinError)
      ) %>%
      # arrange sorts the resulting matrix from above according 
      # to minimum error
      arrange(error) 


### Fit the optimal model
# Take the first value of each relevant column since 'arrange' above
# sorted the values by minimum test error

# Best cost complexity value (the alpha associated with min test error)
bestAlpha <- tableBikeRegr$alpha[1]
# Minimum test error
bestTestError.rtree <- tableBikeRegr$error[1]
# Number of tree splits associated with min test error
bestMinSplit <- tableBikeRegr$minsplit[1]
# Maximum depth associated with min test error
bestMaxDepth <- tableBikeRegr$maxdepth[1]


bike.optimal.rtree <- rpart(
      formula = Casual ~ .,
      data    = bikeTrain,
      method  = "anova",
      control = list(minsplit = bestMinSplit, 
                     maxdepth = bestMaxDepth, 
                     cp = bestAlpha)
)

### Plot the tree
rpart.plot(bike.optimal.rtree)


# Test Set performance of Regression Tree
pred <- predict(bike.optimal.rtree, newdata=bikeTest)
testMSE.rtree <- mean((pred - Y.test)^2); testMSE.rtree
# [1] 161270.2




# Plotting the error rate as function of both size (num leaves) 
# and cp (alpha)
df <- data.frame(NumLeaves=bike.optimal.rtree$cptable[,"nsplit"] + 1, 
                 Alpha=bike.optimal.rtree$cptable[,"CP"], 
                 TestErrors=bike.optimal.rtree$cptable[,"xerror"])

p1 <- ggplot(data=df, aes(x=NumLeaves, y=TestErrors)) + 
      geom_line(size=2, color='dodgerblue')  + geom_point(size=3)
p2 <- ggplot(data=df, aes(x=Alpha, y=TestErrors)) + 
      geom_line(size=2, color='magenta')   + geom_point(size=3)
grid.arrange(p1, p2)

# Can see that lowest test error rate is at 7 leaves




# part b) fit three models: bagging, boosting, random forest ---------------------------------


### Part (i) Selecting Tuning Parameters

# BAGGING: bagging is special case of random forest with m = p
set.seed(1)
bike.bag.cv <- randomForest(X.train, y=Y.train, xtest=X.test, ytest=Y.test, mtry=p,ntree=500,importance=TRUE)
set.seed(1)
bike.bag <- randomForest(Casual ~ ., data=bikeTrain, mtry=p, importance=TRUE, ntree=500)
bike.bag
bike.bag.cv
# mtry = p means all p=7 predictors should be considered for each split of the tree (means
# that bagging should be done)

# Test Evaluation: this is the test error corresponding to the largest tree
pred.bag <- predict(bike.bag, newdata=bikeTest)
testMSE.bag <- mean((pred.bag - Y.test)^2); testMSE.bag
# 128201.3


# Tuning parameter = number of trees
bestNumTrees <- which.min(bike.bag.cv$test$mse); bestNumTrees
# 8
min(bike.bag.cv$test$mse) # lowest test MSE, corresponding to bestNumTrees above

# Plot the test set errors versus the number of trees for p = 7
df <- data.frame(NumTrees=1:500, TestError=bike.bag.cv$test$mse)

ggplot(data=df, aes(x=NumTrees, y = TestError)) + 
      geom_line(size=1) + 
      geom_vline(xintercept=bestNumTrees, color="red", linetype="dashed", size=2) +
      xlab("Number of Trees") + ylab("Test MSE")
# Lowest test MSE corresponds to num trees = 8 




## RANDOM FOREST ---
# Same method as bagging except mtry is lower : m = sqrt(p), or m = p/3 by default

# Method: create range of m-values to fit the model over, using a fixed ntree=500

set.seed(123)

# Creating mtry values
mtryValues <- 1:p

bikeRandForests <- list()
for(i in mtryValues){
      bikeRandForests[[i]] <- randomForest(X.train, y=Y.train, xtest=X.test, 
                              ytest=Y.test, ntree=500, mtry= i, importance=TRUE)
}

# function to get optimal number of trees given a random forest model
# that was fit using the Xtrain, ytrain method (yielding cv errors inside the object)
getBestNumTrees <- function(fit.rf) {
      # get index of minimum test error (this is the number of trees)
      ntree    <- which.min(fit.rf$test$mse)
}


# function to get minimum error
getMinError <- function(fit.rf) {
      # get the index  of minimum test error
      minMSE    <- min(fit.rf$test$mse)
}

mtryValues <- data.frame(mtryValues)
tableBikeForest <- mtryValues %>%
      # mutate adds these vector columns (list of alphas and errors) to the hyper grid
      mutate(
            numTrees    = purrr::map_dbl(bikeRandForests, getBestNumTrees),
            error = purrr::map_dbl(bikeRandForests, getMinError)
      ) %>%
      # arrange sorts the resulting matrix from above according to minimum error
      arrange(error) 

# Fit the optimal model
bestNumTrees <- tableBikeForest$numTrees[1]
bestTestError.forest <- tableBikeForest$error[1]
# random forest usually uses m = p/3 = 7/3 rounds to 2, so consider the
# mtry = 2 model to be the random forest model. 
# here, best mtry is at m = 7, the bagging model. 
# Hence, bagging model does better here than random forest. 
bestMtry <- tableBikeForest$mtry[1]

bike.optimal.forest <- randomForest(Casual ~., data=bikeTrain, mtry=bestMtry,
                                    ntree=bestNumTrees, importance=TRUE)

# Test Set performance of Random Forest
# Getting test error for the model fit with optimal number of trees and mtry
pred <- predict(bike.optimal.forest, newdata=bikeTest)
testMSE.forest <- mean((pred - Y.test)^2); testMSE.forest
# 126646.1


### Plot the test set errors versus the number of trees for various values of p 

# First create the data frame of test errors for various values of P
#df <- data.frame(NumTrees=1:500)
#for(p in mtryValues[,1]){
#      name <- paste("P_", p, sep="")
#      df[,name] <- bikeRandForests[[p]]$test$mse
#}
## Now create the plots for various p
#g <- ggplot(data=df, aes(x=NumTrees))
#for(p in mtryValues[,1]){
#      print(p)
#      g <- g + geom_line(aes(y=df[,p]), size=1)
#}
#g <- xlab("Number of Trees") + ylab("Test MSE")



### BOOSTING------------------

library(gbm)

set.seed(103)

hyperGrid <- expand.grid(
      shrinkage = 10^ seq(-2, -0.2, by = 0.1), #c(.01, .1, .3),
      interaction.depth = c(1, 3, 5)
)


bikeBoosts <- list()
for(i in 1:nrow(hyperGrid)) {
      
      # train model
      bike.boost.cv <- gbm(
            formula = Casual ~ .,
            distribution = "gaussian",
            data = bikeTrain,
            n.trees = 5000,
            interaction.depth = hyperGrid$interaction.depth[i],
            shrinkage = hyperGrid$shrinkage[i],
            cv.folds=10,
            n.cores = NULL, # will use all cores by default
            verbose = FALSE
      )
      bikeBoosts[[i]] <- bike.boost.cv
}

getBestNumTrees.boost <- function(fit.boost) {
      # get index of minimum test error (this is the number of trees)
      ntree    <- which.min(fit.boost$cv.error)
}


# function to get minimum error
getMinError.boost <- function(fit.boost) {
      # get the index  of minimum test error
      err    <- min(fit.boost$cv.error)
}

#getMinTrainError.boost <- function(fit.boost) {
#      err <- min(fit.boost$train.error)
#}

getInteractionDepth.boost <- function(fit.boost){
      d <- fit.boost$interaction.depth
}

getShrinkage.boost <- function(fit.boost){
      lambda <- fit.boost$shrinkage
}

tableBikeBoosts <- hyperGrid %>% 
      mutate(
            interactionDepth = purrr::map_dbl(bikeBoosts, getInteractionDepth.boost),
            shrinkage = purrr::map_dbl(bikeBoosts, getShrinkage.boost),
            bestNumTrees    = purrr::map_dbl(bikeBoosts, getBestNumTrees.boost),
            #minTrainError = purrr::map_dbl(bikeBoosts, getMinTrainError.boost),
            minTestError = purrr::map_dbl(bikeBoosts, getMinError.boost)
      ) %>%
      dplyr::arrange(minTestError)


bestDepth <- tableBikeBoosts$interactionDepth[1]
bestShrinkage <- tableBikeBoosts$shrinkage[1]
bestNumTrees <- tableBikeBoosts$bestNumTrees[1]
bestTestError.boost <- tableBikeBoosts$minTrainError[1]

## Fit the optimal model
bike.optimal.boost <- gbm(formula = Casual ~ ., 
                          distribution = "gaussian", 
                          data = bikeTrain, 
                          n.trees = bestNumTrees, 
                          interaction.depth = bestDepth, 
                          shrinkage = bestShrinkage, 
                          verbose=FALSE)

# Test set performance of the boosting model
pred <- predict(bike.optimal.boost, bikeTest)
testMSE.boost <- mean((pred - Y.test)^2)
#trainError.boost <- tableBikeBoosts$minTestError[1]
# $$$$$$$$$$$$$$$$$$$
#lambdas = 10^ seq(-2, -0.2, by = 0.1) # range of lambdas to use
#L = length(lambdas)

"trainErrors = rep(NA, L)
testErrors = rep(NA, L)

for (i in 1:L) {
      bike.train.boost <- gbm(Casual ~ ., data=bikeTrain, distribution='gaussian, 
                              n.trees=1000,
                              shrinkage = lambdas[i])
      # Calculate training errors
      pred1 = predict(bike.train.boost, bikeTrain, n.trees = 1000)
      trainErrors[i] = mean((pred1 - Y.train)^2)
      # Calculate testing errors
      pred2 = predict(bike.train.boost, bikeTest, n.trees = 1000)
      testErrors[i] = mean((pred2 - Y.test)^2)
}

df <- data.frame(Lambdas=lambdas, TrainErrors=trainErrors, TestErrors=testErrors)

# Get the min train error and its index
iMinTrain <- which.min(trainErrors); iMinTrain
trainMSE.boost.min <- min(trainErrors); trainMSE.boost.min
iMinTest <- which.min(testErrors); iMinTest
testMSE.boost.min <- min(testErrors); testMSE.boost.min
# Get the lambdas of minimum errors
trainLambda <- lambdas[iMinTrain]; trainLambda
testLambda <- lambdas[iMinTest]; testLambda 

ggplot(data=df, aes(x=Lambdas)) + 
      geom_line(aes(y=TrainErrors), size=1, colour='red) + 
      geom_point(x=trainLambda, y=trainMSE.boost.min, color='red, size=3) +
      
      geom_line(aes(y=TestErrors), size=1, colour='blue) + 
      geom_point(x=testLambda, y=testMSE.boost.min, color='blue, size=3) +
      
      xlab('Shrinkage Parameter (Lambda)) + ylab('Errors (MSE)) #+ 
      #scale_fill_discrete(name = 'Error Type, labels = c('train error, 'test error))

"










### Part (ii) compare test MSEs of each model
testMSE.rtree
testMSE.bag
testMSE.forest
testMSE.boost.min

# The minimum test mse is from the boosting model with a lambda of 1e-10 ~ 0 (almost zero)
testLambda




### Part (iii) Select one model and comment on which variables are most important

# Fit the boost model corresponding to best lambda

# Fitting on the whole data set since we already have the train and test error
# for a training model with thi slambda

bike.boost <- gbm(Casual ~ ., data=data, distribution="gaussian", n.trees=1000,
                  shrinkage = testLambda)

# Importance plot 1: weekend is the most important variable
summary(bike.boost)
#     var                  rel.inf
# Weekend         Weekend 33.084503
# Temperature Temperature 25.607292
# Humidity       Humidity 13.095108
# Windspeed     Windspeed 10.270730
# Season           Season  9.197879
# Year               Year  6.652674
# Weather         Weather  2.091814



# Importance plot 2: 

# Partial dependence plots: marginal effect that the selected variables have on the response
# after integrating out the other variables. 

# number of casual observers is higher on weekends (0) than (1) Monday/Friday
plot(bike.boost, i="Weekend") 
# number of casual observers increases when temperature increases
plot(bike.boost, i = "Temperature") 