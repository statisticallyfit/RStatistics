setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 8 - Trees")

library(tree)
library(ISLR)
library(MASS)
#library(ggplot2)
library(gridExtra)
library(randomForest)

data("Carseats")

set.seed(1)

N <- nrow(Carseats)
trainIndices <- sample(1:N, size=N/2)
carTrain <- Carseats[trainIndices, ]
carTest <- Carseats[-trainIndices, ]
Y.test <- carTest$Sales


# part b) regression tree 

car.train.tree <- tree(Sales ~ ., data=carTrain)
summary(car.train.tree)

plot(car.train.tree)
text(car.train.tree, pretty=0)

# Test MSE
pred.tree <- predict(car.train.tree, newdata=carTest)
testMSE.tree <- mean((pred.tree - Y.test)^2)
testMSE.tree
# 4.148897

# part c) ------------------------
# Cross-validation to find optimal level of tree complexity, pruning

# 1) cross-validate
car.tree.cv <- cv.tree(car.train.tree)

iMinMSE <- which.min(car.tree.cv$dev)
minMSE <- min(car.tree.cv$dev); minMSE
car.tree.cv$dev[iMinMSE]

car.tree.cv$size[iMinMSE]
# the 10-leaf tree has minimum CV (MSE) error
car.tree.cv$k[iMinMSE]
# alpha = 25.785 for the tree with min MSE


# Plotting the error rate as function of both size (num leaves) and k (alpha)
library(ggplot2)
df <- data.frame(NumLeaves=car.tree.cv$size, Alpha=car.tree.cv$k, 
                 ErrorRates=car.tree.cv$dev)
p1 <- ggplot(data=df, aes(x=NumLeaves, y=ErrorRates)) + geom_line(size=2, color="dodgerblue")  + geom_point(size=3) + 
      geom_point(x=iMinMSE, y=minMSE, colour="green", size=5)
p3 <- ggplot(data=df, aes(x=Alpha, y=ErrorRates)) + geom_line(size=2, color="magenta")   + geom_point(size=3)
grid.arrange(p1, p3)


# Prune the tree using the best number of leaves (best is 10 but chegg uses 8)
car.prune <- prune.tree(car.train.tree, best=8)
plot(car.prune)
text(car.prune, pretty=0)

# The pruned predictions
pred.prune <- predict(car.prune, carTest)
testMSE.prune <- mean((pred.prune - Y.test)^2) # test MSE
testMSE.prune
# 5.0908




# part d) bagging, test MSE importance...

car.bag <- randomForest(Sales ~ ., data=carTrain, mtry=10, ntree=500, importance=TRUE)
pred.bag <- predict(car.bag, carTest)
testMSE.bag <- mean((pred.bag - carTest$Sales)^2)
testMSE.bag
# 2.604

importance(car.bag)
# most significant variable is price, shelvloc, age



# part e) random forest
car.rf <- randomForest(Sales ~., data=carTrain, mtry=3, ntree=500, importance=TRUE)
pred.rf <- predict(car.rf, newdata=carTest)
testMSE.rf <- mean((pred.rf - carTest$Sales)^2)
testMSE.rf
# 3.28

importance(car.rf)
# price, shelvloc