library(e1071)

# * two classes
# n = 100
# p = 2
# nonlinear separation

# SHOW: 
# (1) an SVM with polynomial kernel (d > 1) OR radial kernel outperforms the Support Vector
# Classifier (the always linear one) , on the training data
# (2) which is best for test data?

set.seed(1)
x = rnorm(100) # first variable
y = 4 * x^2 + 1 + rnorm(100) # second variable
z <- rep(-1, 100) # class, to predict
iClass <- sample(100, size=50)
y[iClass] <- y[iClass] + 3
y[- iClass] <- y[- iClass] - 3 # the obs not in iClass indices get subtracted by 3
z[iClass] = 1 # so now there are two classes -1, and 1

# plotting the two predictors against each other.
plot(x[iClass], y[iClass], col="red", xlab="X", ylab="Y", ylim=c(-6, 30))
points(x[-iClass], y[-iClass], col="blue")

# Creating test and train data
data <- data.frame(x = x, y = y, z = as.factor(z))
iTrain <- sample(100, size=50)
trainData <- data[iTrain, ]
testData <- data[-iTrain, ]


# MODEL 1: linear (SVC) classifier
svm.classifier.linear <- svm(z ~ ., data=trainData, kernel="linear", cost=10)

# MODEL 1: Evaluation
plot(svm.classifier.linear, trainData) 
# INTERPRETATION: 6 missclassifications for training data
# Missclassifiations are:
# (one red cross on blue side, a support vector from red side on the wrong side of the margin,
#, and 5 other blue crosses on the red side, which are support vectors on the wrong side
# of the margin)
# 
# Red crosses on red, and blue crosses on blue sides are fine - they are support vectors
# on the right side of the margin
#
# Blue circles on blue side and red circles on red side are correct classifications, and
# are not support vectors (not within margins). 

# plot(svm.classifier.linear, testData) 

# confusion matrix: TRAIN
pred.train <- predict(svm.classifier.linear, trainData) # train predictions
table(Pred = pred.train, Truth = trainData$z)
# 6 misslciassifications for training data
trainErr.linear <- mean(pred.train != trainData$z); trainErr.linear

# confusion matrix: TEST
pred.test <- predict(svm.classifier.linear, testData) # test predictions
table(Pred = pred.test, Truth = testData$z)
# 6 misslciassifications for training data, also seen on plot, 6 opposite colors
testErr.linear <- mean(pred.test != testData$z); testErr.linear

# same train and testing errors = 0.12

## -------------------------

# MODEL 2: Polynomial (SVM)
svm.poly <- svm(z ~ ., data=trainData, kernel="polynomial", cost=10)

# MODEL 1: Evaluation
plot(svm.poly, trainData) 

# confusion matrix: TRAIN
pred.train <- predict(svm.poly, trainData) # train predictions
table(Pred = pred.train, Truth = trainData$z)
# 9 misslciassifications for training data
trainErr.poly <- mean(pred.train != trainData$z); trainErr.poly

# confusion matrix: TEST
pred.test <- predict(svm.poly, testData) # test predictions
table(Pred = pred.test, Truth = testData$z)
# 9 misslciassifications for training data, also seen on plot, 6 opposite colors
testErr.poly <- mean(pred.test != testData$z); testErr.poly

# same train and test errors = 0.18


## -------------------------

# MODEL 3: Radial (SVM)
svm.radial <- svm(z ~ ., data=trainData, kernel="radial", cost=10)

# MODEL 1: Evaluation
plot(svm.radial, trainData)  # this is what train errors look like
plot(svm.radial, testData) # this is what test errors look like

# confusion matrix: TRAIN
pred.train <- predict(svm.radial, trainData) # train predictions
table(Pred = pred.train, Truth = trainData$z)
# no misslciassifications for training data
trainErr.radial <- mean(pred.train != trainData$z); trainErr.radial

# confusion matrix: TEST
pred.test <- predict(svm.radial, testData) # test predictions
table(Pred = pred.test, Truth = testData$z)
# no misslciassifications for training data, also seen on plot, 6 opposite colors
testErr.radial <- mean(pred.test != testData$z); testErr.radial

# same train and test errors = no error!




trainErr.linear
trainErr.poly
trainErr.radial # radial best, poly worst
testErr.linear
testError.poly
testErr.radial
