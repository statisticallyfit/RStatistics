setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A3")

library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ggplot2)
library(gridExtra)
library(randomForest) # bagging, and random forest
library(gbm)          # boosting
library(vip)          # variable importance plot


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


# part a) fit regression tree, response = casual =============================================
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
getBestAlpha.rtree <- function(tree.fit) {
      # get index of minimum test error
      min    <- which.min(tree.fit$cptable[, "xerror"])
      # get the ALPHA corresponding to model with min test error. 
      cp <- tree.fit$cptable[min, "CP"] 
}

# function to get minimum error of a given tree
getMinError.rtree <- function(tree.fit) {
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
            alpha    = purrr::map_dbl(bikeRegrTrees, getBestAlpha.rtree),
            error = purrr::map_dbl(bikeRegrTrees, getMinError.rtree)
      ) 
tableBikeRegr.ord <- tableBikeRegr %>%
      # arrange sorts the resulting matrix from above according 
      # to minimum error
      arrange(error) 
# Describe
head(tableBikeRegr)

# Get the id of the best model
BM <- which.min(tableBikeRegr$error); BM 
# 5
# Get the optimal tuning parameters: 
# Best cost complexity value (the alpha associated with min test error)
bestAlpha.rtree <- tableBikeRegr$alpha[BM]; bestAlpha.rtree 
# 0.01
# Minimum test error
bestTestError.rtree <- tableBikeRegr$error[BM]; bestTestError.rtree
# 0.3786509
# Number of tree splits associated with min test error
bestMinSplit.rtree <- tableBikeRegr$minsplit[BM]; bestMinSplit.rtree
# 9
# Maximum depth associated with min test error
bestMaxDepth.rtree <- tableBikeRegr$maxdepth[BM]; bestMaxDepth.rtree
# 8

# Fit the optimal model (fitting on the entire data set since now we have the optimal
# tuning parameters)
set.seed(1)
bike.optimal.rtree <- rpart(
      formula = Casual ~ .,
      data    = data,
      method  = "anova",
      control = list(minsplit = bestMinSplit.rtree, 
                     maxdepth = bestMaxDepth.rtree, 
                     cp = bestAlpha.rtree)
)

### Plot the tree
rpart.plot(bike.optimal.rtree, tweak=1.3)

# Plotting the cross-validated error rates (using the training data) 
# as function of both size (num leaves) and cp (alpha)
df <- data.frame(NumLeaves=bike.optimal.rtree$cptable[,"nsplit"] + 1, 
                 Alpha=bike.optimal.rtree$cptable[,"CP"], 
                 CVErrors=bike.optimal.rtree$cptable[,"xerror"])

p1 <- ggplot(data=df, aes(x=NumLeaves, y=CVErrors)) + 
      geom_line(size=1, color='dodgerblue')  + geom_point(size=4, alpha=0.4) +
      ggtitle("Cross-Validated MSEs from Training Data for Regression Tree")
p2 <- ggplot(data=df, aes(x=Alpha, y=CVErrors)) + 
      geom_line(size=1, color='magenta')   + geom_point(size=4, alpha=0.4) +
      ggtitle("Cross-Validated MSEs from Training Data for Regression Tree")
grid.arrange(p1, p2)


# part b) fit three models: bagging, boosting, random forest =============================

### Part (i) Selecting Tuning Parameters

# BAGGING:  ------------------------------------------------------------------------------
# bagging is special case of random forest with m = p
set.seed(111)
bike.bag.cv <- randomForest(X.train, y=Y.train, xtest=X.test, ytest=Y.test,
                            mtry=p,ntree=500,importance=TRUE)

# mtry = p means all p=7 predictors should be considered for each split of the tree (means
# that bagging should be done)

# Tuning parameter = number of trees
bestNumTrees.bag <- which.min(bike.bag.cv$test$mse); bestNumTrees.bag
# 11
bestTestError.bag <- min(bike.bag.cv$test$mse) # lowest test MSE, corresponding to bestNumTrees above
# 125907.6

# Plot the test set errors  for p = 7 (bagged model)
df <- data.frame(TestMSE = bike.bag.cv$test$mse, NumTrees=1:500)

ggplot(data=df,aes(x=NumTrees)) + geom_line(aes(y=TestMSE), colour="deeppink",size=1)  + 
      geom_vline(aes(xintercept = bestNumTrees.bag), 
                 color="black", linetype="dashed",size=1) +
      geom_point(x=bestNumTrees.bag, y=bestTestError.bag, colour="black", size=4, alpha=0.3) +
      ggtitle("Test Errors for Best CV Bagged Model")

# Lowest test MSE corresponds to num trees = 11 , as pink dot shows


# Fit the optimal model
set.seed(1)
bike.optimal.bag <- randomForest(Casual ~., data=data, ntree=bestNumTrees.bag, mtry=p,
                                 importance=TRUE)
# Describe the model 
bike.optimal.bag 


## RANDOM FOREST -----------------------------------------------------------------------
# Same method as bagging except mtry is lower : m = sqrt(p), or m = p/3 by default
# Method: create range of m-values to fit the model over, using a fixed ntree=500
set.seed(458)

# Creating mtry values
mtryValues <- 1:p

bikeRandForests <- list()
for(i in mtryValues){
      bikeRandForests[[i]] <- randomForest(X.train, y=Y.train, xtest=X.test, 
                              ytest=Y.test, ntree=500, mtry= i, importance=TRUE)
}

# function to get optimal number of trees given a random forest model
# that was fit using the Xtrain, ytrain method (yielding cv errors inside the object)
getBestNumTrees.rf <- function(fit.rf) {
      # get index of minimum test error (this is the number of trees)
      ntree    <- which.min(fit.rf$test$mse)
}
# function to get minimum error
getMinError.rf <- function(fit.rf) {
      # get the index  of minimum test error
      minMSE    <- min(fit.rf$test$mse)
}

mtryValues <- data.frame(mtryValues)
tableBikeForest <- mtryValues %>%
      # mutate adds these vector columns (list of alphas and errors) to the hyper grid
      mutate(
            numTrees    = purrr::map_dbl(bikeRandForests, getBestNumTrees.rf),
            error = purrr::map_dbl(bikeRandForests, getMinError.rf)
      )
# arrange sorts the resulting matrix from above according to minimum error
tableBikeForest.ord <- tableBikeForest %>% arrange(error) 
# Describe
tableBikeForest.ord

#Get the id of the best model
BM <- which.min(tableBikeForest$error); BM
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
df <- data.frame(TestMSE = bikeRandForests[[BM]]$test$mse, NumTrees=1:500)

ggplot(data=df,aes(x=NumTrees)) + 
      geom_line(aes(y=TestMSE), colour="deeppink", size=1)  + 
      geom_point(x=bestNumTrees.forest, y=bestTestError.forest, color="black", size=4)+
      geom_vline(aes(xintercept = bestNumTrees.forest), color="black", linetype="dashed",size=1) +
      ggtitle("Test Errors for Random Forest Cross-Validation Model")


### BOOSTING ----------------------------------------------------------------------------

set.seed(103)
hyperGrid <- expand.grid(
      shrinkage = 10^ seq(-2, -0.2, by = 0.1), #c(.01, .1, .3),
      interaction.depth = c(1, 3, 5)
)
getBestNumTrees.boost <- function(fit.boost) {
      # get index of minimum test error (this is the number of trees)
      ntree    <- which.min(fit.boost$cv.error)
}
# function to get minimum error
getMinError.boost <- function(fit.boost) {
      # get the index  of minimum test error
      err    <- min(fit.boost$cv.error)
}

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

# Get all the information in a tale
tableBikeBoosts <- hyperGrid %>% 
      mutate(
            bestNumTrees    = purrr::map_dbl(bikeBoosts, getBestNumTrees.boost),
            minTestError = purrr::map_dbl(bikeBoosts, getMinError.boost)
      )
# Order the table by minimum test error
tableBikeBoosts.ord <- tableBikeBoosts  %>% dplyr::arrange(minTestError)
# Describe the table
head(tableBikeBoosts.ord)

# Find the model with minimum test error and plot its training and test errors vs lambda
BM <- which.min(tableBikeBoosts$minTestError) # best model index
# 21
# get the best tuning parameters
bestDepth.boost <- tableBikeBoosts$interaction.depth[BM]; bestDepth.boost
# 3
bestShrinkage.boost <- tableBikeBoosts$shrinkage[BM]; bestShrinkage.boost
# 0.01258925
bestNumTrees.boost <- tableBikeBoosts$bestNumTrees[BM]; bestNumTrees.boost
# 917
bestTestError.boost <- tableBikeBoosts$minTestError[BM]; bestTestError.boost
# 132941.9

## Fit the optimal model
set.seed(3)
bike.optimal.boost <- gbm(formula = Casual ~ ., 
                          distribution = "gaussian", 
                          data = bikeTrain, 
                          n.trees = bestNumTrees.boost, 
                          interaction.depth = bestDepth.boost, 
                          shrinkage = bestShrinkage.boost, 
                          verbose=FALSE)

# Plot the train versus test error of the best model
df <- data.frame(TestMSE = bikeBoosts[[BM]]$cv.error,
                 TrainMSE = bikeBoosts[[BM]]$train.error, NumTrees=1:5000)

ggplot(data=df,aes(x=NumTrees, color=c("TestMSE", "TrainMSE"))) + 
      geom_line(aes(y=TestMSE, colour="TestMSE"), size=1)  + 
      geom_line(aes(y=TrainMSE, colour="TrainMSE"), size=1) +
      geom_vline(aes(xintercept = bestNumTrees.boost), color="black", linetype="dashed",size=1) +
      
      scale_colour_discrete(name="Error Type", breaks=c("TestMSE", "TrainMSE"), 
                            labels=c("Test error", "Train error")) + 
      xlab("Number of trees") + ylab("Error (MSE)") + 
      ggtitle("Train versus Test Errors for Best Boost Cross-Validation Model")

### Part (ii) compare test MSEs of each model ------------------------------------------

# Test Set performance of Regression Tree
pred <- predict(bike.optimal.rtree, newdata=bikeTest)
testMSE.rtree <- mean((pred - Y.test)^2); testMSE.rtree
# [1] 49148.42

# # Test Evaluation of Bag model
pred <- predict(bike.optimal.bag, newdata=bikeTest)
testMSE.bag <- mean((pred - Y.test)^2); testMSE.bag
# 22665.17

# Test Set performance of Random Forest
pred <- predict(bike.optimal.forest, newdata=bikeTest)
testMSE.forest <- mean((pred - Y.test)^2); testMSE.forest
# 18317.14

# Test set performance of the boosting model
pred <- predict(bike.optimal.boost, bikeTest, n.trees=bestNumTrees.boost)
testMSE.boost <- mean((pred - Y.test)^2); testMSE.boost
# 130221.6

testMSE.rtree
# 49148.42
testMSE.bag
# 22665.17
testMSE.forest
# 18317.14
testMSE.boost
# 130221.6

### Part (iii) ==========================================================================
# Select one model and comment on which variables are most important

# Fitted the boost model corresponding to best lambda on entire data set
bike.optimal.boost

# Importance plot 1: weekend is the most important variable
summary(bike.optimal.boost)
# var   rel.inf
# Weekend         Weekend 33.905143
# Temperature Temperature 17.774427
# Season           Season 14.664052
# Humidity       Humidity 14.589034
# Windspeed     Windspeed  7.424385
# Year               Year  6.771183
# Weather         Weather  4.871775
vip(bike.optimal.boost, color="dodgerblue", fill="blue", size=18, alpha=0.5)


# Importance plot 2: 
# Partial dependence plots: marginal effect that the selected variables have on 
# the response after integrating out the other variables. 

# number of casual observers is higher on weekends (0) than (1) Monday/Friday
plot(bike.optimal.boost, i="Weekend") 
# number of casual observers increases when temperature increases
plot(bike.optimal.boost, i = "Temperature") 
