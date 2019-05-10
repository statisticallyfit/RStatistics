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

bikeModels <- list()
for (i in 1:nrow(hyperGrid)) {
      
      # get minsplit, maxdepth values at row i
      minsplit <- hyperGrid$minsplit[i]
      maxdepth <- hyperGrid$maxdepth[i]
      
      # train a model and store in the list
      bikeModels[[i]] <- rpart(
            formula = Casual ~ .,
            data    = bikeTrain,
            method  = "anova",
            control = list(minsplit = minsplit, maxdepth = maxdepth)
      )
}


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


tableBike <- hyperGrid %>%
      # mutate adds these vector columns (list of alphas and errors) 
      # to the hyper grid
      mutate(
            # map_dbl(x, f) applies function f elementwise to vector x
            # and returns a vector
            alpha    = purrr::map_dbl(bikeModels, getBestAlpha),
            error = purrr::map_dbl(bikeModels, getMinError)
      ) %>%
      # arrange sorts the resulting matrix from above according 
      # to minimum error
      arrange(error) 


### Fit the optimal model
# Take the first value of each relevant column since 'arrange' above
# sorted the values by minimum test error

# Best cost complexity value (the alpha associated with min test error)
bestAlpha <- tableBike$alpha[1]
# Minimum test error
bestTestError <- tableBike$error[1]
# Number of tree splits associated with min test error
bestMinSplit <- tableBike$minsplit[1]
# Maximum depth associated with min test error
bestMaxDepth <- tableBike$maxdepth[1]


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
testMSE.rtree <- mean((pred - bikeTest$Casual)^2); testMSE.rtree
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
# Tuning parameter = alpha: controls tradeoff between a subtree's fit 
# vs. its complexity. 



# part b) fit three models: bagging, boosting, random forest ---------------------------------


### Part (i) Selecting Tuning Parameters

# BAGGING: bagging is special case of random forest with m = p
set.seed(1)
bike.train.bag.E <- randomForest(X.train, y=Y.train, xtest=X.test, ytest=Y.test, mtry=p,ntree=500,importance=TRUE)
set.seed(1)
bike.train.bag <- randomForest(Casual ~ ., data=bikeTrain, mtry=p, importance=TRUE)
bike.train.bag
bike.train.bag.E
# mtry = p means all p=7 predictors should be considered for each split of the tree (means
# that bagging should be done)

# Test Errors: bagging (another way to obtain it)
pred.bag <- predict(bike.train.bag, newdata=bikeTest)
testMSE.bag <- mean((pred.bag - Y.test)^2); testMSE.bag
# 128201.3


# Tuning parameter = number of trees
bestNumTrees <- which.min(bike.train.bag.E$test$mse); bestNumTrees
min(bike.train.bag.E$test$mse) # lowest test MSE, corresponding to bestNumTrees above

# Plot the test set errors versus the number of trees for p = 7
df <- data.frame(NumTrees=1:500, TestError=bike.train.bag.E$test$mse)

ggplot(data=df, aes(x=NumTrees, y = TestError)) + geom_line(size=1) +
      xlab("Number of Trees") + ylab("Test MSE")
# Lowest test MSE corresponds to num trees = 8 




## RANDOM FOREST ---
# Same method as bagging except mtry is lower : m = sqrt(p), or m = p/3 by default

set.seed(1)
# the mtry = p/3 random forest model
bike.train.forest.p3 <- randomForest(X.train, y=Y.train, xtest=X.test, ytest=Y.test,ntree=500, mtry=p/3, importance=TRUE)
# mtry = sqrt(p) random forest model
bike.train.forest.sqrtP <- randomForest(X.train, y=Y.train, xtest=X.test, ytest=Y.test,ntree=500, mtry=round(sqrt(p)), importance=TRUE)
# mtry = p / 2 random forest model
bike.train.forest.p2 <- randomForest(X.train, y=Y.train, xtest=X.test, ytest=Y.test,ntree=500, mtry=p/2, importance=TRUE)
# mtry = p random forest mdoel (same as bagging case)
bike.train.forest.p <- randomForest(X.train, y=Y.train, xtest=X.test, ytest=Y.test,ntree=500, mtry=p, importance=TRUE)

set.seed(1)
# fitting this model with mtry = p / 3 to do predictions
bike.train.forest <- randomForest(Casual ~ ., data=bikeTrain, ntree=500, mtry=p/3, importance=TRUE)
bike.train.forest # algo used p = 2 (num variables tried at each split)

bike.train.forest.p
bike.train.forest.p2
bike.train.forest.p3
bike.train.forest.sqrtP


# Test Errors: random forest (another way to obtain it)
# Getting test error for the model fit with 500 trees
pred.forest <- predict(bike.train.forest, newdata=bikeTest)
testMSE.forest <- mean((pred.forest - Y.test)^2); testMSE.forest
# 182607.2167


# Tuning parameter = number of trees. Finding best number of trees: for which test error is a minimum. 
iP3 = which.min(bike.train.forest.p3$test$mse) ; iP3 
# 324
iP2 = which.min(bike.train.forest.p2$test$mse) ; iP2 
# 5
iP = which.min(bike.train.forest.p$test$mse) ; iP
# 150
iSqrtP <- which.min(bike.train.forest.sqrtP$test$mse) ; iSqrtP
# 107

# Lowest test MSE for the models with various mtry tuning parameters
minP3 <- min(bike.train.forest.p3$test$mse); minP3 
# 179725.972
minP2  <- min(bike.train.forest.p2$test$mse); minP2
# 128608.5718
minP <- min(bike.train.forest.p$test$mse); minP
# 131210.9002
minSqrtP <- min(bike.train.forest.sqrtP$test$mse); minSqrtP
# 152466.7358

# Plot the test set errors versus the number of trees for various values of p 
df <- data.frame(NumTrees=1:500, 
                 P = bike.train.forest.p$test$mse, 
                 P2 = bike.train.forest.p2$test$mse, 
                 P3 = bike.train.forest.p3$test$mse,
                 PSQ = bike.train.forest.sqrtP$test$mse)

ggplot(data=df, aes(x=NumTrees)) + 
      geom_line(aes(y=P), size=1, colour="black") + 
      geom_point(x=iP, y=minP, color="black", size=3) +

      geom_line(aes(y=P2), size=1, colour="dodgerblue") + 
      geom_point(x=iP2, y=minP2, color="dodgerblue", size=3) +
      
      geom_line(aes(y=P3), size=1, colour="purple") + 
      geom_point(x=iP3, y=minP3, color="purple", size=3) +
      
      geom_line(aes(y=PSQ), size=1, colour="magenta") + 
      geom_point(x=iSqrtP, y=minSqrtP, color="magenta", size=3) +
      
      xlab("Number of Trees") + ylab("Test MSE") + 
      scale_fill_discrete(name = "Model Type (mtry)", labels = c("m = p", "m = p/2", "m = p/3", "m = sqrt(p)"))



### BOOSTING

library(gbm)

set.seed(103)


lambdas = 10^ seq(-10, -0.2, by = 0.1) # range of lambdas to use
L = length(lambdas)
trainErrors = rep(NA, L)
testErrors = rep(NA, L)

for (i in 1:L) {
      bike.train.boost <- gbm(Casual ~ ., data=bikeTrain, distribution="gaussian", n.trees=1000,
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
      geom_line(aes(y=TrainErrors), size=1, colour="red") + 
      geom_point(x=trainLambda, y=trainMSE.boost.min, color="red", size=3) +
      
      geom_line(aes(y=TestErrors), size=1, colour="blue") + 
      geom_point(x=testLambda, y=testMSE.boost.min, color="blue", size=3) +
      
      xlab("Shrinkage Parameter (Lambda)") + ylab("Errors (MSE)") #+ 
      #scale_fill_discrete(name = "Error Type", labels = c("train error", "test error"))




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