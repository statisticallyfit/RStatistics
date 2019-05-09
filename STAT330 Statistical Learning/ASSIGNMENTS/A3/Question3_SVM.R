setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A3")

library(e1071)


# income:  0 = "<= 50 K group",    1 = "> 50 K group"
load("datacens.Rdata")

# first check if any NA observations
any(is.na(datacens$train))
any(is.na(datacens$val))

# Income can stay numeric, but make a factor column of income also
datacens$train$income <- as.numeric(datacens$train$income)
datacens$val$income <- as.numeric(datacens$val$income)

datacens$train$incomeBinary <- as.factor(datacens$train$income)
datacens$val$incomeBinary <- as.factor(datacens$val$income)

# Then take random sample of original data set, from both train and test sets. 
Nt <- nrow(datacens$train)
Nv <- nrow(datacens$val)
n <- 500 # use a smaller sample
set.seed(111)
ts <- sample(1:Nt, size=n)
vs <- sample(1:Nv, size=n)
censusTrain <- datacens$train[ts, ]
censusTest <- datacens$val[vs, ]




# part b) radial kernel svm --------------------------------------------------------------------

# SVM USING CROSS-VALIDATION to select best COST AND GAMMA-
set.seed(1)
tune.radial <- tune(svm, incomeBinary ~., data=censusTrain, kernel="radial", 
                 ranges=list(cost=c(0.01, 0.1, 1, 10, 20),gamma=c(0.5, 5, 10)))
tune.radial
summary(tune.radial)

tune.radial$best.performance # best train error
# 0.03
tune.radial$best.parameters # best cost and gamma
# cost = 10, gamma = 0.5
tune.radial$best.model
# num support vectors = 325, so has wide margins




# part c) fit the best model -----------------------------------------------------------------

### Fitting the models using the best parameters
bestCost <- tune.radial$best.parameters[[1]]; bestCost
bestGamma <- tune.radial$best.parameters[[2]]; bestGamma
census.svm.radial <- svm(incomeBinary ~ ., data=censusTrain, kernel="radial", cost=bestCost, gamma=bestGamma)


# Training set Evaluation
# confusion matrix: TRAIN
pred <- predict(census.svm.radial, newx=censusTrain) # train predictions
table(Pred = pred, Truth = censusTrain$incomeBinary)
# 0+0 = 0 misslciassifications for training data
trainError.radial <- mean(pred != censusTrain$incomeBinary); trainError.radial
# 0% missclassification on training data



# part d)  -------------------------------------------------------------------------------------

# Test set Evaluation

pred <- predict(tune.radial$best.model, newx=censusTest)
# confusion matrix: TEST
table(Pred=pred, Truth = censusTest$incomeBinary) # there are 85 + 98 = 183 missclassifications
# test error
testError.radial <- mean(pred != censusTest$incomeBinary); testError.radial
# so 36.6% of test observations are missclassified. 


# Plot the resulting SVC

# first make all variables numerical
# censusTrain <- censusTrain.old #
#censusTrain$type_employer <- as.numeric(censusTrain.old$type_employer)
#censusTrain$education <- as.numeric(censusTrain.old$education)
#censusTrain$marital <- as.numeric(censusTrain.old$marital)
#censusTrain$occupation <- as.numeric(censusTrain.old$occupation)
#censusTrain$relationship <- as.numeric(censusTrain.old$relationship)
#censusTrain$race <- as.numeric(censusTrain.old$race)
#censusTrain$sex <- as.numeric(censusTrain.old$sex)
#censusTrain$capital_gain <- as.numeric(censusTrain.old$capital_gain)
#censusTrain$capital_loss <- as.numeric(censusTrain.old$capital_loss)
#censusTrain$country <- as.numeric(censusTrain.old$country)


fp = "/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A3/"
useNames <- names(censusTrain)[!names(censusTrain) %in% c("income", "incomeBinary")]

# Plots using the training data

pdf(file=file.path(paste(fp, "graphw.pdf")))
for (name in useNames) {
      plot(census.svm.radial, censusTrain, as.formula(paste("income ~", name, sep = "")))
}
dev.off()


# Plots using the test data

pdf(file=file.path(paste(fp, "Test_Radial.pdf", sep="")))
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


