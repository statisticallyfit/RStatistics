setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A3")

library(e1071)


# income:  0 = "<= 50 K group",    1 = "> 50 K group"
load("datacens.Rdata")

# first check if any NA observations
any(is.na(datacens$train))
any(is.na(datacens$val))

# Then take random sample of original data set, from both train and test sets. 
Nt <- nrow(datacens$train)
Nv <- nrow(datacens$val)
n <- 500 # use a smaller sample
set.seed(111)
ts <- sample(1:Nt, size=n)
vs <- sample(1:Nv, size=n)
censusTrain <- datacens$train[ts, ]
censusTest <- datacens$val[vs, ]




# part b) radial kernel svm

# SVM USING CROSS-VALIDATION to select best COST AND GAMMA:  --------------
set.seed(1)
tune.radial <- tune(svm, income ~., data=censusTrain, kernel="radial", 
                 ranges=list(cost=c(0.01, 0.1, 1, 10, 20),gamma=c(0.5, 5, 10)))
tune.radial
summary(tune.radial)

tune.radial$best.performance # best train error
# 0.184
tune.radial$best.parameters # best cost and gamma
# cost = 1, gamma = 0.5
tune.radial$best.model
# num support vectors = 377, so has wide margins


### Fitting the models using the best parameters
bestCost <- 1
bestGamma <- 0.5
census.svm.radial <- svm(income ~ ., data=censusTrain, kernel="radial", cost=bestCost, gamma=bestGamma)


# Plot the resulting SVC

# first make all variables numerical
censusTrain.num <- censusTrain
censusTrain.num$income <- as.numeric(censusTrain$income)
censusTrain.num$type_employer <- as.numeric(censusTrain$type_employer)
censusTrain.num$education <- as.numeric(censusTrain$education)
censusTrain.num$marital <- as.numeric(censusTrain$marital)
censusTrain.num$occupation <- as.numeric(censusTrain$occupation)
censusTrain.num$relationship <- as.numeric(censusTrain$relationship)
censusTrain.num$race <- as.numeric(censusTrain$race)
censusTrain.num$sex <- as.numeric(censusTrain$sex)
censusTrain.num$capital_gain <- as.numeric(censusTrain$capital_gain)
censusTrain.num$capital_loss <- as.numeric(censusTrain$capital_loss)
censusTrain.num$country <- as.numeric(censusTrain$country)


fp = "/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A3/"
useNames <- names(censusTrain.num)[!names(censusTrain.num) %in% c("income")]

# Plots using the training data
pdf(file=file.path(paste(fp, "Train_Radial.pdf", sep="")))
for (name in useNames) {
      plot(census.svm.radial, censusTrain.num, as.formula(paste("income~", name, sep = "")))
}
dev.off()

# Plots using the test data
pdf(file=file.path(paste(fp, "Testn_Radial.pdf", sep="")))
for (name in useNames) {
      plot(census.svm.radial, censusTest, as.formula(paste("income~", name, sep = "")))
}
dev.off()

#plot(census.svm.radial, censusTrain)
#plot(census.svm.radial, censusTest)
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



# Training set Evaluation
# confusion matrix: TRAIN
pred <- predict(census.svm.radial, newx=censusTrain) # train predictions
table(Pred = pred, Truth = censusTrain$income)
# 2 + 18 = 20 misslciassifications for training data
trainError.radial <- mean(pred != censusTrain$income); trainError.radial
# 0.04 = 4% missclassification on training data


# Test set Evaluation
pred <- predict(tune.radial$best.model, newx=censusTest)
# confusion matrix: TEST
table(Pred=pred, Truth = censusTest$income) # there are 71+100=171 missclassifications
# test error
testError.radial <- mean(pred != censusTest$income); testError.radial
# so 34.2% of test observations are missclassified. 