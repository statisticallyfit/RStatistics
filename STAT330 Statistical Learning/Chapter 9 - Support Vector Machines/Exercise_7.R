
library(ISLR)
data("Auto")
library(e1071)

# a) create binary variable that takes 1 for cars with gas mileage above median, and 0 for
# cars with gas mileage below median
N <- nrow(Auto)
var <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
Auto$mpgBinary <- as.factor(var)
head(Auto)


# b) SVC with various cost values to predict if car gets high or low gas mileage
# report cross-validation errors associated with different values of cost. 

# Creating train / test sets 
set.seed(1)
iTrain <- sample(1:N, size=N*3/4)
autoTrain <- Auto[iTrain, ]
autoTest <- Auto[-iTrain, ]


## MODEL 1: SVC classifier (linear)====================================================
set.seed(1)
# Cross-validation, various cost values
tune.linear <- tune(svm, mpgBinary ~ ., data=autoTrain, kernel="linear", ranges=list(cost=c(0.01, 0.1, 1,5,10,100,1000)))
summary(tune.linear)
# best cost = 1

# Evaluate: 
pred.linear <- predict(tune.linear$best.model, autoTest)
# confusion matrix
table(Pred = pred.linear, Truth = autoTest$mpgBinary)
testErr.linear <- mean(pred.linear != autoTest$mpgBinary); testErr.linear

# 0 test error rate

## Model 2: SVM (polynomial) ====================================================

set.seed(2)
# Cross-validation, various cost values
tune.poly <- tune(svm, mpgBinary ~ ., data=autoTrain, kernel="polynomial", 
                  ranges=list(cost=c(0.01, 0.1, 1,5,10,100,1000), degree=c(2,3,4)))
summary(tune.poly)
tune.poly$best.parameters
tune.poly$best.model
# best cost = 1000, degree = 2
# best.performance = gives lowest training error
tune.poly$best.performance
# 0.248

# Evaluate: 
pred.poly <- predict(tune.poly$best.model, autoTest)
# confusion matrix
table(Pred = pred.poly, Truth = autoTest$mpgBinary)
testErr.poly <- mean(pred.poly != autoTest$mpgBinary); testErr.poly
# 0.204 test error rate, higher than linear


## Model 3: SVM (radial) ====================================================

set.seed(3)
# Cross-validation, various cost values
tune.radial <- tune(svm, mpgBinary ~ ., data=autoTrain, kernel="radial", 
                    ranges=list(cost=c(0.01, 0.1, 1,5,10,100,1000), gamma=c(0.01, 0.1, 1,5,10,100)))
summary(tune.radial)
tune.radial$best.performance
# bestperformance = best train error = 0.013
tune.radial$best.parameters # 100, 0.01
tune.radial$best.model
# best parameters = cost = 100, gamma = 0.01 

# Evaluate: 
pred.radial <- predict(tune.radial$best.model, autoTest)
# confusion matrix
table(Pred = pred.radial, Truth = autoTest$mpgBinary)
testErr.radial <- mean(pred.radial != autoTest$mpgBinary); testErr.radial
# 0.0 test error rate, like linear , so poly here is the worst. 


# compare train error rates
tune.linear$best.performance
tune.poly$best.performance
tune.radial$best.performance

tune.poly$best.parameters
tune.linear$best.parameters
tune.radial$best.parameters

# Best training performance was for linear, with radial near 0 also

testErr.linear
testErr.poly
testErr.radial
# Best test error is for linear and radial



### Fitting the models using the best parameters
svm.final.linear <- svm(mpgBinary ~ ., data=Auto, kernel="linear", cost=1)
tune.poly$best.parameters
svm.final.poly <- svm(mpgBinary ~ ., data=Auto, kernel="polynomial", cost=1000, degree=2)
tune.radial$best.parameters # which gamma???
svm.final.radial <- svm(mpgBinary ~ ., data=Auto, kernel="radial", cost=100, gamma=0.01)



### Plotting
fp = "/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 9 - Support Vector Machines/"

pdf(file=file.path(paste(fp, "Ex7_Linear.pdf", sep="")))
for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
      plot(svm.final.linear, Auto, as.formula(paste("mpg~", name, sep = "")))
}
dev.off()
# --------------------------
pdf(file=file.path(paste(fp, "Ex78_Poly", ".pdf", sep="")))
for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
      plot(svm.final.poly, Auto, as.formula(paste("mpg~", name, sep = "")))
}
dev.off()
# --------------------------
pdf(file=file.path(paste(fp, "Ex7_Radial", ".pdf", sep="")))
for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
      plot(svm.final.radial, Auto, as.formula(paste("mpg~", name, sep = "")))
}
dev.off()
