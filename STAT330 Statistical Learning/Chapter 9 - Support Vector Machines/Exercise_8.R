
library(ISLR)
data(OJ)

library(e1071)


# part a) train and test set

N <- nrow(OJ)

set.seed(1)
iTrain <- sample(1:N, size=800) # sample from entire indices set, but use only 800 for train
ojTrain <- OJ[iTrain, ]
ojTest <- OJ[-iTrain, ]



# part b) support vector classifier

oj.linear.svm <- svm(Purchase ~ ., data=ojTrain, kernel="linear", cost=0.01)
summary(oj.linear.svm)
# best cost = 0.01, gamma = 0.055
# 432 support vectors out of 800 points (wide margins, so low variance so restrictive model)
# 215 support vectors are in class CH, and 217 are in class MM


# part c) train and test error rates

# TRAIN ERRORS
pred.train <- predict(oj.linear.svm, ojTrain)
# confusion matrix
table(Pred.Train=pred.train, Truth.train = ojTrain$Purchase)
trainErr.linear <- mean(pred.train != ojTrain$Purchase); trainErr.linear
# 16.625 train error rate


# TEST ERRORS
pred.test <- predict(oj.linear.svm, ojTest)
# confusion matrix
table(Pred.test=pred.test, Truth.test = ojTest$Purchase)
testErr.linear <- mean(pred.test != ojTest$Purchase); testErr.linear
# 18.1 test error rate




# part d) tune to select optimal cost, ranges 0.01, to 10
set.seed(1554)
oj.tune.linear <- tune(svm, Purchase ~ ., data=ojTrain, kernel="linear", 
                       ranges=list(cost=10^seq(-2,1,by=0.25)))
summary(oj.tune.linear)

oj.tune.linear$best.performance # lowest train error
# 0.1625
oj.tune.linear$best.parameters
# best cost = 0.1



# part e) train error and test errors for best cost
# fit the optimal model
svm.linear <- svm(Purchase ~., kernel="linear", data=ojTrain, 
                  cost=oj.tune.linear$best.parameters$cost)

# TRAIN ERROR
pred <- predict(svm.linear, ojTrain)
table(Pred=pred, Truth=ojTrain$Purchase) # confusion matrix
trainErr.cv.linear <- mean(pred != ojTrain$Purchase); trainErr.cv.linear
# 0.1587

# TEST ERROR
pred <- predict(svm.linear, ojTest)
table(Pred=pred, Truth=ojTest$Purchase) # confusion matrix
testErr.cv.linear <- mean(pred != ojTest$Purchase); testErr.cv.linear
# 0.188



# part f) now same as above but for RADIAL KERNEL
set.seed(1)
oj.tune.radial <- tune(svm, Purchase ~ ., data=ojTrain, kernel="radial", 
                       ranges=list(cost=c(0.01, 0.1, 1,5,10),gamma=c(0.01, 0.1, 1,5))) #use default gamma????
summary(oj.tune.radial)

oj.tune.radial$best.performance # lowest train error
# 0.1637
oj.tune.radial$best.parameters
# best cost = 1, gamma = 0.01

# TRAIN ERROR
pred <- predict(oj.tune.radial$best.model, ojTrain)
table(Pred=pred, Truth=ojTrain$Purchase) # confusion matrix
trainErr.cv.radial <- mean(pred != ojTrain$Purchase); trainErr.cv.radial
# 0.1575

# TEST ERROR
pred <- predict(oj.tune.radial$best.model, ojTest)
table(Pred=pred, Truth=ojTest$Purchase) # confusion matrix
testErr.cv.radial <- mean(pred != ojTest$Purchase); testErr.cv.radial
# 0.174




# part g) POLYNOMIAL KERNEL
set.seed(1)
oj.tune.poly <- tune(svm, Purchase ~ ., data=ojTrain, kernel="polynomial", 
                       ranges=list(cost=c(0.01, 0.1, 1,5,10),degree=2)) 
summary(oj.tune.poly)

oj.tune.poly$best.performance # lowest train error
# 0.1725
oj.tune.poly$best.parameters
# best cost = 5, degree=2

# TRAIN ERROR
pred <- predict(oj.tune.poly$best.model, ojTrain)
table(Pred=pred, Truth=ojTrain$Purchase) # confusion matrix
trainErr.cv.poly <- mean(pred != ojTrain$Purchase); trainErr.cv.poly
# 0.14875

# TEST ERROR
pred <- predict(oj.tune.poly$best.model, ojTest)
table(Pred=pred, Truth=ojTest$Purchase) # confusion matrix
testErr.cv.poly <- mean(pred != ojTest$Purchase); testErr.cv.poly
# 0.18148

trainErr.cv.linear
trainErr.cv.radial
trainErr.cv.poly
# poly does best on training set

testErr.cv.linear
testErr.cv.radial
testErr.cv.poly
# radial does best on training set