setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A2/")
library(ggplot2)
library(gridExtra)
library(MASS)

library(leaps)
library(glmnet)
library(pls)


sydneyData <- read.table("SydneyCrime.txt", header=TRUE)
head(sydneyData)

### Data Preparation ------------------------------------------------------------------------

set.seed(1)
N <- nrow(sydneyData) # number of observations
numVars <- ncol(sydneyData) - 1 # number of (p) variables/ predictors

trainIndices <- sample(1:N, size=N/2)
testIndices <- -trainIndices

X <- model.matrix(crim ~ . -1, data=sydneyData)
Y <- sydneyData$crim
X.train = model.matrix(crim ~. -1, data=sydneyData[trainIndices, ]) 
Y.train = sydneyData$crim[trainIndices]
X.test <- model.matrix(crim ~ . -1, data=sydneyData[testIndices, ])
Y.test <- sydneyData$crim[testIndices]




# SUBSET SELECTION ---------------------------------------------------------------------------


# Part 1) using full data set to choose the best subset using the CP, BIC, and Adj R^2
sydney.subset <- regsubsets(crim ~ . , data=sydneyData, nvmax=numVars, method="forward")
summarySubset <- summary(sydney.subset)

iMinCp.sub <- which.min(summarySubset$cp); iMinCp.sub # 8 predictors chosen by Min CP
iMinBIC.sub <- which.min(summarySubset$bic); iMinBIC.sub # 3 predictors
iMaxAdj.sub <- which.max(summarySubset$adjr2); iMaxAdj.sub # 9 predictors


# Plotting Model selection Statistics Cp, BIC, ...
df.sub <- data.frame(NumVars=1:numVars, AdjR=summarySubset$adjr2, 
                     Cp=summarySubset$cp, BIC=summarySubset$bic)


p.sub.adj <- ggplot(df.sub, aes(x=NumVars, y=AdjR)) + geom_line() + 
      geom_point(aes(x=iMaxAdj.sub, y=df.sub$AdjR[iMaxAdj.sub]), size=3, colour="purple") + 
      ggtitle("Adjusted R^2 vs Number of Predictors for Full-Data Subset Model")

p.sub.cp <- ggplot(df.sub, aes(x=NumVars, y=Cp)) + geom_line() + 
      geom_point(aes(x=iMinCp.sub, y=df.sub$Cp[iMinCp.sub]), size=3, colour="purple") + 
      ggtitle("Cp vs Number of Predictors for Full-Data Subset Model")

p.sub.bic <- ggplot(df.sub, aes(x=NumVars, y=BIC)) + geom_line() + 
      geom_point(aes(x=iMinBIC.sub, y=df.sub$BIC[iMinBIC.sub]), size=3, colour="purple") + 
      ggtitle("BIC vs Number of Predictors for Full-Data Subset Model")

grid.arrange(p.sub.adj, p.sub.cp, p.sub.bic)



# Part 2) splitting train / test data to do cross validation to pick the best subset

predict.regsubsets <- function(object, testData, id, ...) {
      #print(cat("GO HERE"))
      form = as.formula(object$call[[2]])
      X = model.matrix(form, testData)
      # For a particular model size (id = numvars), extract the coefficients from the
      # regsubsets object for the best model of that size (id)
      coef_i = coef(object, id =id) # id = the model id
      #print(cat("id = ", id, sep=""))
      
      # get the predictor names for this model
      xVars = names(coef_i)
      # Multiply those coefficients into the appropriate columns of the test matrix
      X[, xVars] %*% coef_i 
}


# Carry out a for loop to do cross validation. In the jth fold, the elements of folds that
# equal j are in the test set and the remainder are in the training set. Make predictions
# for each model size, compute test errors on the appropriate subset, and store them
# in appropriate slot in the matrix cv.errors

numFolds = 10
numVars = ncol(sydneyData) - 1
folds = sample(1:numFolds, size=nrow(sydneyData), replace=TRUE)
cv.errors = matrix(NA, nrow=numFolds, ncol=numVars, dimnames=list(Folds=NULL, Vars=paste(1:numVars)))

for(j in 1:numFolds){
      # fit data on training set (observations not in the kth fold)
      #theFormula <- as.formula(paste(yname, "~ ."))
      sydney.train.subset <- regsubsets(crim ~ ., data=sydneyData[folds != j, ], nvmax = numVars)
      
      for(i in 1:numVars){ # for each model size ...
            # ... make a prediction based on the test set (data that IS in this current fold)
            pred = predict.regsubsets(sydney.train.subset, sydneyData[folds == j, ], id=i)
            # compute test error using observations from test set and the predictions we made
            cv.errors[j, i] <- mean( (sydneyData$crim[folds == j] - pred)^2 ) 
      }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

# Plotting the MSEs
df.subset <- data.frame(NumVars = 1:numVars, MeanCVErrors=mean.cv.errors)
iMinCV.sub = which.min(mean.cv.errors); iMinCV.sub # 9 predictor model
minCVTestError.subset <- min(mean.cv.errors); minCVTestError.subset
# 45.98215


# Using a pink dot to denote the (9th predictor) model with minimum estimated test MSE
ggplot(df.subset, aes(x=NumVars, y=MeanCVErrors)) + geom_line() + geom_point() + 
      ggtitle("Cross Validation MSEs for Best Subset Selection") +
      geom_point(x=iMinCV.sub, y=df.subset$MeanCVErrors[iMinCV.sub], color="magenta", size=3)


# Final step: do best subset selection on full data set to get the final coefficients
sydney.finalbest.subset = regsubsets(crim ~ ., data=sydneyData, nvmax=numVars)
coefs.final.subset <- coef(sydney.finalbest.subset, iMinCV.sub)
coefs.final.subset


### FORWARD SELECTION -----------------------------------------------------------------------

set.seed(1)

# Part 1) using full data set and graphics to do exploratory analysis 
sydney.fwd <- regsubsets(crim ~ . , data=sydneyData, nvmax=numVars, method="forward")
summaryFwd <- summary(sydney.fwd)

iMinCp.fwd <- which.min(summaryFwd$cp); iMinCp.fwd # 8 predictors chosen by Min CP
iMinBIC.fwd <- which.min(summaryFwd$bic); iMinBIC.fwd # 3 predictors
iMaxAdj.fwd <- which.max(summaryFwd$adjr2); iMaxAdj.fwd # 9 predictors


# Plotting Model selection Statistics Cp, BIC, ...
df.fwd <- data.frame(NumVars=1:numVars, AdjR=summaryFwd$adjr2, 
                     Cp=summaryFwd$cp, BIC=summaryFwd$bic)


p.fwd.adj <- ggplot(df.fwd, aes(x=NumVars, y=AdjR)) + geom_line() + 
      geom_point(aes(x=iMaxAdj.fwd, y=df.fwd$AdjR[iMaxAdj.fwd]), size=3, colour="green") + 
      ggtitle("Adjusted R^2 vs Number of Predictors for Full-Data Forward Stepwise Model")

p.fwd.cp <- ggplot(df.fwd, aes(x=NumVars, y=Cp)) + geom_line() + 
      geom_point(aes(x=iMinCp.fwd, y=df.fwd$Cp[iMinCp.fwd]), size=3, colour="green") + 
      ggtitle("Cp vs Number of Predictors for Full-Data Forward Stepwise Model")

p.fwd.bic <- ggplot(df.fwd, aes(x=NumVars, y=BIC)) + geom_line() + 
      geom_point(aes(x=iMinBIC.fwd, y=df.fwd$BIC[iMinBIC.fwd]), size=3, colour="green") + 
      ggtitle("BIC vs Number of Predictors for Full-Data Forward Stepwise Model")

grid.arrange(p.fwd.adj, p.fwd.cp, p.fwd.bic)



# Part 2) using cross validation (splitting data into train / test sets)

# fit the training model
sydney.train.fwd <- regsubsets(crim ~ . , data=sydneyData[trainIndices, ], nvmax=numVars, 
                               method="forward")

# Doing cross validation to see which forward subset is the best
cv.errors.fwd = rep(NA, numVars)

# Getting test errors for each model size
for(i in 1:numVars){ # for each model size ...
      pred = predict.regsubsets(sydney.train.fwd, testData=sydneyData[testIndices, ], id=i)
      # compute test error using observations from test set and the predictions we made
      cv.errors.fwd[i] <- mean( (Y.test - pred)^2 ) 
}

# Plotting the MSEs
df.cv.fwd <- data.frame(NumVars = 1:numVars, MeanCVErrors=cv.errors.fwd)
iMinCV.fwd = which.min(cv.errors.fwd); iMinCV.fwd
# The model with minimum estimated test error is the 12th model (12 predictors)
# It has min test error of: 
minCVTestError.fwd <- min(cv.errors.fwd); minCVTestError.fwd
# 39.27592

# Using a pink dot to denote the model with minimum estimated test MSE
ggplot(df.cv.fwd, aes(x=NumVars, y=MeanCVErrors)) + geom_line() + geom_point() + 
      ggtitle("Cross Validation MSEs for Forward Selection") +
      geom_point(x=iMinCV.fwd, y=df.cv.fwd$MeanCVErrors[iMinCV.fwd], color="magenta", size=3)


# Final step: do forward regression on full data set to get the final coefficients
# sydney.finalbest.fwd = regsubsets(crim ~ ., data=sydneyData, nvmax=numVars, method="forward")
# already fit the model above
coefs.final.fwd <- coef(sydney.fwd, iMinCV.fwd)
coefs.final.fwd


### BACKWARD SELECTION -----------------------------------------------------------------------


set.seed(1)

# Part 1) using full data set and graphics to do exploratory analysis 
sydney.bwd <- regsubsets(crim ~ . , data=sydneyData, nvmax=numVars, method="backward")
summaryBwd <- summary(sydney.bwd)

iMinCp.bwd <- which.min(summaryBwd$cp); iMinCp.bwd # 8 predictors chosen by Min CP
iMinBIC.bwd <- which.min(summaryBwd$bic); iMinBIC.bwd # 4 predictors chosen by min BIC
iMaxAdj.bwd <- which.max(summaryBwd$adjr2); iMaxAdj.bwd # 9 predictors chosen by max Adj R^2


# Plotting Model selection Statistics Cp, BIC, ...
df.bwd <- data.frame(NumVars=1:numVars, AdjR=summaryBwd$adjr2, 
                     Cp=summaryBwd$cp, BIC=summaryBwd$bic)


p.bwd.adj <- ggplot(df.bwd, aes(x=NumVars, y=AdjR)) + geom_line() + 
      geom_point(aes(x=iMaxAdj.bwd, y=df.bwd$AdjR[iMaxAdj.bwd]), size=3, colour="blue") + 
      ggtitle("Adjusted R^2 vs Number of Predictors for Full-Data Backward Stepwise Model")

p.bwd.cp <- ggplot(df.bwd, aes(x=NumVars, y=Cp)) + geom_line() + 
      geom_point(aes(x=iMinCp.bwd, y=df.bwd$Cp[iMinCp.bwd]), size=3, colour="blue") + 
      ggtitle("Cp vs Number of Predictors for Full-Data Backward Stepwise Model")

p.bwd.bic <- ggplot(df.bwd, aes(x=NumVars, y=BIC)) + geom_line() + 
      geom_point(aes(x=iMinBIC.bwd, y=df.bwd$BIC[iMinBIC.bwd]), size=3, colour="blue") + 
      ggtitle("BIC vs Number of Predictors for Full-Data Backward Stepwise Model")

grid.arrange(p.bwd.adj, p.bwd.cp, p.bwd.bic)



# Part 2) using cross validation (splitting data into train / test sets)

# fit the training model
sydney.train.bwd <- regsubsets(crim ~ . , data=sydneyData[trainIndices, ], nvmax=numVars, method="backward")

# Doing cross validation to see which forward subset is the best
cv.errors.bwd = rep(NA, numVars)

# Getting test errors for each model size
for(i in 1:numVars){ # for each model size ...
      pred = predict.regsubsets(sydney.train.bwd, testData=sydneyData[testIndices, ], id=i)
      # compute test error using observations from test set and the predictions we made
      cv.errors.bwd[i] <- mean( (Y.test - pred)^2 ) 
}

# Plotting the MSEs
df.cv.bwd <- data.frame(NumVars = 1:numVars, MeanCVErrors=cv.errors.bwd)
iMinCV.bwd = which.min(cv.errors.bwd); iMinCV.bwd
# 4 - predictor model has minimum CV test error
minCVTestError.bwd <- min(cv.errors.bwd); minCVTestError.bwd
# 38.96427

# Using a pink dot to denote the model with minimum estimated test MSE
ggplot(df.cv.bwd, aes(x=NumVars, y=MeanCVErrors)) + geom_line() + geom_point() + 
      ggtitle("Cross Validation MSEs for Backward Selection") +
      geom_point(x=iMinCV.bwd, y=df.cv.bwd$MeanCVErrors[iMinCV.bwd], color="magenta", size=3)


# Final step: do forward regression on full data set to get the final coefficients
# sydney.finalbest.fwd = regsubsets(crim ~ ., data=sydneyData, nvmax=numVars, method="forward")
# already fit the model above
coefs.final.bwd <- coef(sydney.bwd, iMinCV.bwd)
coefs.final.bwd


### RIDGE REGRESSION --------------------------------------------------------------------------

set.seed(1)

sydney.train.ridge <- cv.glmnet(x=X.train, y=Y.train, alpha=0)
lambda.ridge <- sydney.train.ridge$lambda.min; lambda.ridge
# 0.7908625 is minimum lambda that yields the lowest cross-validated MSE

min(sydney.train.ridge$cvm) #this lowest CV training MSE corresponding to min lambda 
# 48.6947 

plot(sydney.train.ridge) # plots log lambda against MSE

# coefficients of the model with minimum lambda
coef(sydney.train.ridge)

# get test mse associated with this value of lambda
pred.ridge = predict(sydney.train.ridge, s=lambda.ridge, newx = X.test)
cvTestErrorMinLambda.ridge <- mean( (pred.ridge - Y.test)^2 )
cvTestErrorMinLambda.ridge
# 38.36719

# Fit the FINAL model using this lambda (final model is defined as using the FULL data set)
sydney.final.ridge <- glmnet(X, Y, alpha=0)
# These are the coefficients of the final model using this value of lambda
coefs.final.ridge <- predict(sydney.final.ridge, type="coefficients", s=lambda.ridge)[1:numVars + 1, ]
coefs.final.ridge
# note: these coeffs are of course not necessarily the same as from the training model 
# since the data sets (train and full data) are different. 


## LASSO ---------------------------------------------------------------------------------

set.seed(2)

# ... using the same X and Y matrices as for ridge regression .....

sydney.train.lasso <- cv.glmnet(x=X.train, y=Y.train, alpha=1)
lambda.lasso <- sydney.train.lasso$lambda.min; lambda.lasso
# 0.1202 is minimum lambda

plot(sydney.train.lasso) # plots log lambda against MSE

# Coeffs of training model with best lambda?
coef(sydney.train.lasso)

min(sydney.train.lasso$cvm) #this lowest CV training MSE corresponding to min lambda 
# 49.47474 

# Test mse associated with this value of lambda
pred.lasso = predict(sydney.train.lasso, s=lambda.lasso, newx = X.test)
cvTestErrorMinLambda.lasso = mean( (pred.lasso - Y.test)^2 )
cvTestErrorMinLambda.lasso
# 38.26532

# Fit the FINAL model using this lambda (final model is defined as using the FULL data set)
sydney.final.lasso <- glmnet(X, Y, alpha=1)
# These are the coefficients of the final model using this value of lambda
coefs.final.lasso <- predict(sydney.final.lasso, type="coefficients", s=lambda.lasso)[1:numVars+1, ]
coefs.final.lasso <- coefs.final.lasso[coefs.final.lasso != 0]
coefs.final.lasso


# PCR -------------------------------------------------------------------------------------

set.seed(4)

sydney.train.pcr <- pcr(crim ~., data=sydneyData[trainIndices, ], scale=TRUE, validation="CV")
summaryPCRTrain <- summary(sydney.train.pcr)

# EVALUATING: training MSE (test MSE is preferred)

# Maximum R2 seems to be at about M = 13 principal components
validationplot(sydney.train.pcr, val.type="R2")
# Minimum training MSE seems to be at about M = 13 principal components, but no major difference
# between the training MSE at 3 components
validationplot(sydney.train.pcr, val.type="MSEP") # to plot the MSE's

# Checking the number of components using MSEP() from the Mass library
MSEP.obj <- MSEP(sydney.train.pcr)
# Getting the cross validation errors from the 2 by 1 by 14 dimensional array 'val'
dim(MSEP.obj$val)
str(MSEP.obj)

cv.errors.pcr <- MSEP.obj$val[1,1,] 
# Verified: 13 components are required to get minimum cv.error
names(which.min(cv.errors.pcr))
minCVTrainError.pcr <- min(cv.errors.pcr); minCVTrainError.pcr # the minimum training MSE
# 49.90654

# EVALUATING: test MSE (this is the measure to use, never training MSE)
# Computing test error on the training model for M = 13
M = 13
predPCR <- predict(sydney.train.pcr, X.test, ncomp = M) # predicting training with M = 173
minCVTestError.pcr <- mean( (predPCR - Y.test)^2 ); minCVTestError.pcr
# 39.27592

# Fitting the final model with the M chosen by cross-validation
sydney.final.pcr <- pcr(crim ~ ., data=sydneyData, scale=TRUE, ncomp=M)
coefs.final.pcr <- coef(sydney.final.pcr, ncomp=M)[1:numVars, ,]
coefs.final.pcr

# PLS -------------------------------------------------------------------------------------

set.seed(5)

sydney.train.pls <- plsr(crim ~., data=sydneyData[trainIndices, ], scale=TRUE, validation="CV")
summaryPLSTrain <- summary(sydney.train.pls)

# plotting the training MSE's
validationplot(sydney.train.pls, val.type="MSEP")


# EVALUATING: test MSE (this is the measure to use, never training MSE)

# First get the recommended number of components, by training MSE
MSEP.obj <- MSEP(sydney.train.pls)
cv.errors.pls <- MSEP.obj$val[1,1,] 
names(which.min(cv.errors.pls))
minCVTrainError.pls <- min(cv.errors.pls); minCVTrainError.pls # the minimum training MSE
# 48.71021

# Computing test error on the training model for M = 11
M = 11
predPLS <- predict(sydney.train.pls, X.test, ncomp = M) # predicting training with M = 10
minCVTestError.pls <- mean( (predPLS - Y.test)^2 ); minCVTestError.pls
# 39.27655

# Fitting the final model with the M chosen by cross-validation
sydney.final.pls <- plsr(crim ~ ., data=sydneyData, scale=TRUE, ncomp=M)
coefs.final.pls <- coef(sydney.final.pls, ncomp=M)[1:numVars, ,]
coefs.final.pls



# OVERALL SUMMARY

# Comparing models based on accuracy and model interpretability: 

# Accuracy 
testErrorSummary = data.frame(TestErrors=c(minCVTestError.subset, minCVTestError.fwd, 
                                           minCVTestError.bwd, cvTestErrorMinLambda.ridge, 
                                           cvTestErrorMinLambda.lasso,minCVTestError.pcr, 
                                           minCVTestError.pls))
rownames(testErrorSummary) <- c("Subset", "Forward", "Backward", "Ridge", "Lasso", "PCR", "PLS")
testErrorSummary
# Accuracy: The Lasso has lowest test MSE = 38.26, and Ridge regression is second best with 
# test mse = 38.36. Backward is next lowest with 38.96. 
# Lasso has lowest estimated test MSE. 




# Interpretability: backward selection model has fewest coefficients so it is the simplest
# or most interpretable. Lasso and subset model have similar number of coefficients, so 
# because lasso has lower test MSE than the subset model, pick lasso over the subset model.

coefs.final.subset
length(coefs.final.subset) - 1 # 9 

coefs.final.fwd
length(coefs.final.fwd) - 1 # 13

coefs.final.bwd
length(coefs.final.bwd) - 1 # 4

coefs.final.ridge # 13
length(coefs.final.ridge) # 13

coefs.final.lasso # 10
length(coefs.final.lasso) # 10

coefs.final.pcr
length(coefs.final.pcr) # 13 

coefs.final.pls
length(coefs.final.pls) # 13



# CONCLUSION: choose the lasso model since even though it has 9  coefficients compared to the
# backward model with 5 coefficients, their test MSEs are not that different. 