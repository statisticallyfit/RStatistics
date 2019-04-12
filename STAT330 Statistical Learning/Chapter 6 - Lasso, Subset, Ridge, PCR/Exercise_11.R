setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 6 - Lasso, Subset, Ridge, PCR/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')
library(ISLR)
library(ggplot2)
library(gridExtra)
library(MASS)

library(leaps)
library(glmnet)
library(pls)

data("Boston")
head(Boston)

set.seed(1)

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


# 2) do for loop to do cross validation. In the jth fold, the elements of folds that
# equal j are in the test set and the remainder are in the training set. Make predictions
# for each model size, compute test errors on the appropriate subset, and store them
# in appropriate slot in the matrix cv.errors

numFolds = 10
numVars = ncol(Boston) - 1
folds = sample(1:numFolds, size=nrow(Boston), replace=TRUE)
cv.errors = matrix(NA, nrow=numFolds, ncol=numVars, dimnames=list(Folds=NULL, Vars=paste(1:numVars)))

for(j in 1:numFolds){
      # fit data on training set (observations not in the kth fold)
      #theFormula <- as.formula(paste(yname, "~ ."))
      boston.subset <- regsubsets(crim ~ ., data=Boston[folds != j, ], nvmax = numVars)
      
      for(i in 1:numVars){ # for each model size ...
            # ... make a prediction based on the test set (data that IS in this current fold)
            pred = predict.regsubsets(boston.subset, Boston[folds == j, ], id=i)
            # compute test error using observations from test set and the predictions we made
            cv.errors[j, i] <- mean( (Boston$crim[folds == j] - pred)^2 ) 
      }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

# Plotting the MSEs
df.cv <- data.frame(NumVars = 1:numVars, MeanCVErrors=mean.cv.errors)
iMinCV = which.min(mean.cv.errors)
ggplot(df.cv, aes(x=NumVars, y=MeanCVErrors)) + geom_line() + geom_point() + 
      geom_point(x=iMinCV, y=mean.cv.errors[iMinCV], color="magenta", size=3)


### RIDGE ------------------------------------------------------------------------------------

set.seed(1)

X = model.matrix(crim ~. -1, data=Boston) ; X # get rid of intercept (?) why?
Y = Boston$crim 

boston.ridge.cv <- cv.glmnet(x=X, y=Y, alpha=0)
lambda.ridge <- boston.ridge.cv$lambda.min; lambda.ridge
# 0.5899047 is minimum lambda

plot(boston.ridge.cv, col="blue") # plots log lambda against MSE

# coefficients of the model with minimum lambda
coef(boston.ridge.cv)

## LASSO ---------------------------------------------------------------------------------

set.seed(1)

X = model.matrix(crim ~ . -1, data=Boston) ; X # get rid of intercept (?) why?
Y = Boston$crim 

boston.lambda.cv <- cv.glmnet(x=X, y=Y, alpha=1)
lambda.lasso <- boston.lambda.cv$lambda.min; lambda.lasso
# 0.0202365 is minimum lambda

plot(boston.lambda.cv, col="blue") # plots log lambda against MSE

# Coeffs of model with best lambda?
coef(boston.lambda.cv)

# PCR -------------------------------------------------------------------------------------

set.seed(1)
boston.pcr <- pcr(crim ~., data=Boston, scale=TRUE, validation="CV")
summary(boston.pcr)

validationplot(boston.pcr, val.type="R2")
validationplot(boston.pcr) # RMSE
validationplot(boston.pcr, val.type="MSEP") # to plot the MSE's

# Smallest mse at M = 13 components
# note: can only get an estimate of the test error for M = 13 when we have actually
# fit the training model and can use the test set to get the test error. 