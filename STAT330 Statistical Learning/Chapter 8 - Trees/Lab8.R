setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 8 - Trees")

library(tree)
# install.packages("tree") # by R command line
library(ISLR)
library(MASS)
library(ggplot2)
library(gridExtra)
library(randomForest)

data("Carseats")
head(Carseats)


# GOAL: using classification trees to predict continuous variable Sales
High = ifelse(Carseats$Sales <= 8, "No", "Yes")

carData <- data.frame(Carseats, High)
head(carData)

car.tree <- tree(High ~ . - Sales, data=carData)
car.tree
summary(car.tree)
# lists the variables used as internal nodes, and lists number of leaves (27) (terminal ndoes)
# and training error rate = 0.09 (9 percen)

# Deviance = -2 sum(over m) ( sum(over k) n_mk * log(p_mk))
# where n_mk = number of observations in the mth leaf that belongs to kth class. 
# Small deviance means the tree provides a good fit to the training data. 

# Residual mean deviance = deviance / (n - |T_0|), where T0 = the fit tree and |T0| = 27
# or num leaves fo the fit tree. 
# n - |T0| = 400 - 27 = 373

# Show the tree structure and text to show node labels
plot(car.tree)
text(car.tree, pretty=0)
levels(carData$ShelveLoc)
# [1] "Bad"    "Good"   "Medium"

# From the graph, ShelveLoc is most important predictor since it is the first branch. 
# On the left, put Bad and Medium, on the right is the Good locations. 

car.tree #prints output corresponding to each branch of the tree

# Example: Price < 92.5 is the split criterion in that second branch, and also number of 
# observations in that branch, deviance, and overall prediction (Yes, no) and fraction (proportions)
# of observations that take Yes / No. 
# Asterisks = branches that lead to leaves (terminal nodes)



# Need to get TEST error not just training error: so split the data
set.seed(2)
N <- nrow(carData)
trainIndices <- sample(1:N, size=N/2)
carTest <- carData[-trainIndices,]
carTrain <- carData[trainIndices, ]
Y.test <- carTest$High

car.train.tree <- tree(High ~ . - Sales, data=carTrain)
#plot(car.train.tree)
#text(car.train.tree, pretty=0) # pretty option explains variable names (which branch takes
# which level for a categorical variable)

# Predictions
pred.tree <- predict(car.train.tree, carTest, type="class") # class converts probs to labels
pred.tree
table(PredictionHigh=pred.tree, TestHigh=Y.test)

#library(caret)
#confusionMatrix(data=pred.tree, reference=Y.test, positive="Yes")
accuracy <- mean(pred.tree == Y.test); accuracy
testError <- mean(pred.tree != Y.test); testError



# Try: does pruning improve results? 
# Use cross-validation to find the optimal level of tree complexity. 

# Cost complexity pruning is used to select a sequence of trees for consideration. 
# cv.tree() reports number of leaves of each tree considered (leaves = size) and also the
# corresponding error rate and the value of the cost-complexity parameter used (k = alpha)
# Use argument FUN = prune.misclass to indicate we want the classification error rate
# to guide the cross-validation and pruning process, rather than the default (deviance)

set.seed(3)
car.tree.cv <- cv.tree(car.train.tree, FUN=prune.misclass)
str(car.tree.cv)
# NOTE: despite the name 'dev' is actually the cross-validation error rate E = 1 - max(prob)
#, and not the deviance. 

car.tree.cv
car.tree.cv$size[which.min(car.tree.cv$dev)] # so the tree with 9 leaves got lowest 
# classification error rate, with 50 cross validation errors. 
car.tree.cv$k[which.min(car.tree.cv$dev)] # tuning parameter ALPHA with min dev is 1.75
# Closer to zero means the subtree is closer to the largest tree. ALpha -> oo means cost
# is minimized. 

# Plotting the error rate as function of both size (num leaves) and k (alpha)
par(mfrow=c(1,2))
plot(car.tree.cv$size, car.tree.cv$dev, type="b")
plot(car.tree.cv$k, car.tree.cv$dev, type="b")

# Or use ggplot

df <- data.frame(NumLeaves=car.tree.cv$size, Alpha=car.tree.cv$k, ErrorRates=car.tree.cv$dev)
p1 <- ggplot(data=df, aes(x=NumLeaves, y=ErrorRates)) + geom_line(size=2, color="dodgerblue")  + geom_point(size=3)
p2 <- ggplot(data=df, aes(x=Alpha, y=ErrorRates)) + geom_line(size=2, color="magenta")   + geom_point(size=3)
grid.arrange(p1, p2)

# Now apply the prune.misclass() to prune the tree to get the nine-node tree
bestLeafTree <- 9
car.prune <- prune.misclass(car.train.tree, best=bestLeafTree)
car.prune
par(mfrow=c(1,1))
plot(car.prune); text(car.prune, pretty=0)

# Evaluate how the pruned tree does on the data set
pred.tree.pruned <- predict(car.prune, carTest, type="class")
table(PredsPruned=pred.tree.pruned, Test=Y.test)
testError.pruned <- mean(pred.tree.pruned != Y.test)
testError.pruned
testError

# so the pruned tree did just a bit better (0.23 vs 0.285)
# So pruning process made (1) highe rprediction accuracy and (2) interpretable tree.

# Using another tree yields lower pred accuracy
car.prune2 <- prune.misclass(car.train.tree, best=15)
pred <- predict(car.prune2, carTest, type="class")
table(pred, Y.test)
mean(pred == Y.test) # accuracy of the 15-node tree. 
mean(pred != Y.test) # test accuracy is 0.26, higher than 0.23 for 9 -node tree



### FITTING REGRESSION TREES -----------------------------------------------------
data("Boston")

set.seed(1)
N <- nrow(Boston)
trainIndices <- sample(1:N, size=N/2)
bostonTrain <- Boston[trainIndices, ]
bostonTest <- Boston[-trainIndices, ]
Y.test <- bostonTest$medv

boston.train.tree <- tree(medv ~ ., bostonTrain)
summary(boston.train.tree)
# Got tree with 8 leaves
# Deviance = sum of squared errors (RSS) for the tree

plot(boston.train.tree)
text(boston.train.tree, pretty=0)

# lstat = percentage of people with lower socioeconomic status
# Tree predicts higher median housing prices (medv, leaves) for lower lstat (socioecon status)
boston.train.tree


# Prune the tree to see if performance is better
boston.train.cv <- cv.tree(boston.train.tree)
boston.train.cv$size[which.min(boston.train.cv$dev)]
# the 8-leaf tree has minimum CV (MSE) error

# Plotting the error rate as function of both size (num leaves) and k (alpha)
df <- data.frame(NumLeaves=boston.train.cv$size, Alpha=boston.train.cv$k, 
                 ErrorRates=boston.train.cv$dev)
p1 <- ggplot(data=df, aes(x=NumLeaves, y=ErrorRates)) + geom_line(size=2, color="dodgerblue")  + geom_point(size=3)
p2 <- ggplot(data=df, aes(x=Alpha, y=ErrorRates)) + geom_line(size=2, color="magenta")   + geom_point(size=3)
grid.arrange(p1, p2)

# Prune the tree using the best number of leaves
boston.prune <- prune.tree(boston.train.tree, best=8)

# The upruned predictions
pred <- predict(boston.train.tree, bostonTest)
mean((pred - Y.test)^2) # test MSE

# the pruned predictions
pred.prune <- predict(boston.prune, bostonTest)
testMSE.boston.prune <- mean((pred.prune - Y.test)^2) # test MSe for pruned tree. 
testMSE.boston.prune 
# 25.05

# sqrt root is 5.005 so the model leads to test predictions that are within $5005 of the true
# median hone vaule for the suburb. 


## BAGGING / RANDOM FOREST -------------------------------------------------------------------
# bagging is special case of random forest with m = p 

# BAGGING: 
set.seed(1)
boston.bag <- randomForest(medv ~ ., data=bostonTrain, mtry=13, importance=TRUE)
boston.bag
# mtry = 13 means all 13 predictors should be considered for each split of the tree (means
# that bagging should be done)

# Get test set error
pred.bag <- predict(boston.bag, newdata=bostonTest)
testMSE.boston.bag <- mean((pred.bag - Y.test)^2); testMSE.boston.bag
# 13.5
# this is half the test MSE obtained by optimally pruning a single tree
testMSE.boston.prune

# Plotting
plot(pred.bag, Y.test)
abline(0,1)
# OR
df <- data.frame(YTest=Y.test, PredBag=pred.bag)
ggplot(data=df, aes(x=PredBag, y=YTest)) + geom_point(shape=19) + geom_abline(intercept=0, slope=1)


# Changing number of trees grown
boston.bag.ntree <- randomForest(medv ~ ., data=bostonTrain, mtry=13, ntree=25)
pred.bag <- predict(boston.bag.ntree, newdata=bostonTest)
testMSE.boston.bag.ntree <- mean((pred.bag - Y.test)^2); testMSE.boston.bag.ntree
# 13.948 # is higher



## RANDOM FOREST
# Same method as bagging except mtry is lower : m = sqrt(p), or m = p/3 by default

set.seed(1)
boston.rf <- randomForest(medv ~ ., data=bostonTrain, mtry=6, importance=TRUE)
boston.rf

pred.rf <- predict(boston.rf, newdata=bostonTest)
testMSE.boston.rf <- mean((pred.rf - Y.test)^2); testMSE.boston.rf
# 11.66454

# compare
testMSE.boston.rf
testMSE.boston.bag
testMSE.boston.prune
# so random forest gives lowest testMSE here


### Check importance of each variable (mean decrease of MSE due to splits, for a given predictor
# averaging over all B bootstrap-sampled trees)
importance(boston.rf)
importance(boston.bag)
# importance(boston.prune) # not applicable just for 1 tree, needs many trees

# Meaning: 
### first col: based on mean decrease of accuracy in predictions (increase in MSE) on the 
# out of bag samples when a given variable is excluded from the model. So this measures
# how much the MSE increased when that particular variable wasn't included. So if the 
# % increase MSE is large, then we need it in the model. 
### second col: measures total decrease in node impurity (increase in node purity) that
# results from splits over a variable, averaged over all trees. 
varImpPlot(boston.rf)

# lstat and house size (rm) are the most important

# For regression trees, node impurity = measured by training RSS
# For classification trees, node impurity = measured by deviance. 


### BOOSTING -------------------------------------------------------------------------------
library(gbm)

# regression = need distribution="gaussian", 
# classification = need distribution = "bernoulli"

set.seed(1)
boston.boost <- gbm(medv ~ ., data=bostonTrain, distribution = "gaussian", 
                    n.trees=5000, interaction.depth = 4) # maxdepth = 4


############
# Brenda Vo's example using cv.folds
bvo.boost.boston=gbm(medv~.,data=bostonTrain,distribution="gaussian",n.trees=5000, 
                     interaction.depth=3, shrinkage=0.1, cv.folds = 5, verbose = F )


names(bvo.boost.boston)
names(boston.boost)

# get number of trees B for which test error is minimum
gbm.perf(bvo.boost.boston, method="cv")
#  504
# or
iMinTE <- which.min(bvo.boost.boston$cv.error); iMinTE
# 504
iMinRE <- which.min(bvo.boost.boston$train.error); iMinRE

# Cv boost: Test vs Train error
df <- data.frame(TestMSE = bvo.boost.boston$cv.error, TrainMSE = bvo.boost.boston$train.error,
                 numtrees=1:5000)

ggplot(data=df,aes(x=numtrees, color=c("TestMSE", "TrainMSE"))) + 
      geom_line(aes(y=TestMSE, colour="TestMSE"), size=1)  + 
      geom_line(aes(y=TrainMSE, colour="TrainMSE"), size=1) +
      geom_vline(aes(xintercept = iMinTE), color="black", linetype="dashed",size=1) +
      
      scale_colour_discrete(name="Error Type", breaks=c("TestMSE", "TrainMSE"), 
                            labels=c("Test error", "Train error")) + 
      xlab("Number of trees") + ylab("Error (MSE)")

# --- now use the hyper grid approach:

# 1) keep ntree fixed = 5000
# 2) do hypergrid of depth / shrinkage values
# (3) find optimal num trees (for a model record is min test error and corresp. numtrees)
# 4) return: use SMALLEST of all min test errors to return corresp. num trees.
#############

boston.boost
summary(boston.boost)
'var    rel.inf
lstat     lstat 37.0661275
rm           rm 25.3533123
dis         dis 11.7903016
crim       crim  8.0388750
black     black  4.2531659
nox         nox  3.5058570
age         age  3.4868724
ptratio ptratio  2.2500385
indus     indus  1.7725070
tax         tax  1.1836592
chas       chas  0.7441319
rad         rad  0.4274311
zn           zn  0.1277206'

# relative influence says lstat and rm are by far the most important variables
str(boston.boost)
class(boston.boost$trees)
#boston.boost$trees[[1]]


# Partial dependence plots: marginal effect that the selected variables have on the response
# after integrating out the other variables. 
par(mfrow=c(1,2))
plot(boston.boost, i="rm")
plot(boston.boost, i = "lstat")


# Get test MSE: use boosted model to predict medv on test set and get test MSE
pred.boost <- predict(boston.boost, newdata=bostonTest, n.trees=5000)
testMSE.boston.boost <- mean((pred.boost - Y.test)^2); testMSE.boston.boost
# 10.81479
# even less than the others


# Do it all again except with another shrinkage parameter value: default is lambda = 0.001
boston.boost <- gbm(medv ~ ., data=bostonTrain, distribution="gaussian", n.trees=5000,
                    interaction.depth = 4, shrinkage = 0.2, verbose=FALSE)
pred.boost <- predict(boston.boost, newdata=bostonTest, n.trees=5000)
testMSE.boston.boost <- mean((pred.boost - Y.test)^2); testMSE.boston.boost
# 11.51 , slightly higher test MSE