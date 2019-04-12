setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 6 - Lasso, Subset, Ridge, PCR/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')
library(ISLR)
library(ggplot2)
library(gridExtra)
library(leaps)
#install.packages("leaps")

data("Hitters")
head(Hitters)

sum(is.na(Hitters$Salary)) # missing salary for 59 players
hitData <- na.omit(Hitters)

dim(hitData)

#hitters.subsets.lm <- regsubsets(Salary ~ ., data=hitData)
#s1 = summary(hitters.subsets.lm)

# An asterisk indicates that a given variable is included in the corresponding model. 
# see pdf for how to read:
# http://www2.hawaii.edu/~taylor/z632/Rbestsubsets.pdf


# max vars = 19
hitters.subsets.lm <- regsubsets(Salary ~., data=hitData, nvmax=19)
s <- summary(hitters.subsets.lm)
names(s)
s$rsq
s

# Can examine the r-squares and Cp and BICs for all models to determine which subset model
# is the best (this is the final selection step)
s$adjr2
which.min(s$bic)

# Example: see that r^2 increases from 32% when only one variable is in the model, to 
# almost 55% when all the variables are included (as expected, R^2 increases monotonically
# as more vars are included)
s$rsq


# Plotting RSS, adj R^2, Cp, and BIC for all the models at once

numVars = dim(s$outmat)[1]
iMinRss = which.min(s$rss); iMinRss
iMaxAdj = which.max(s$adjr2); iMaxAdj
iMinCp = which.min(s$cp); iMinCp
iMinBIC = which.min(s$bic); iMinBIC

df <- data.frame(NumVars=1:numVars, RSS=s$rss, AdjR=s$adjr2, Cp=s$cp, BIC=s$bic)

p.rss <- ggplot(df, aes(x=NumVars, y=RSS)) + geom_line() + 
      geom_point(aes(x=iMinRss, y=s$rss[iMinRss]), colour="blue")

p.adj <- ggplot(df, aes(x=NumVars, y=AdjR)) + geom_line() + 
      geom_point(aes(x=iMaxAdj, y=s$adjr2[iMaxAdj]), colour="blue")

p.cp <- ggplot(df, aes(x=NumVars, y=Cp)) + geom_line() + 
      geom_point(aes(x=iMinCp, y=s$cp[iMinCp]), colour="blue")

p.bic <- ggplot(df, aes(x=NumVars, y=BIC)) + geom_line() + 
      geom_point(aes(x=iMinBIC, y=s$bic[iMinBIC]), colour="blue")

grid.arrange(p.rss, p.adj, p.cp, p.bic)

# ----------------

par(mfrow=c(2,2))
plot(s$rss, xlab="Number of Variables", ylab="RSS", type="l")

plot(s$adjr2, xlab="Number of Variables", ylab="RSS", type="l")
# Identify which is the max is the maximum
which.max(s$adjr2) # 11
points(11, s$adjr2[11], col="red", cex=2, pch=20)

plot(s$cp, xlab="Number of Variables", ylab="Cp", type='l')
# Get the model number with minimum cp
which.min(s$cp)
points(10, s$cp[10], col="red", cex=2, pch=20)

plot(s$bic, xlab="Number of Variables", ylab="BIC", type="l")
which.min(s$bic) # which model has lowest BIC
points(6, s$bic[6], col="red", cex=2, pch=20)


# Plot the selected variables for the best model, ranked according to a given statistic
par(mfrow=c(2,2))
par(mfrow=c(1,1))
plot(hitters.subsets.lm, scale="r2")
plot(hitters.subsets.lm, scale="adjr2")
plot(hitters.subsets.lm, scale="Cp")
plot(hitters.subsets.lm, scale="bic")

# See coefficients associated with the model of least BIC
coef(hitters.subsets.lm, iMinBIC)
summary(hitters.subsets.lm, matrix.logical = TRUE)


# FORWARD + BACWARD STEPWISE -----------------------------------------------------------

hitters.fwd <- regsubsets(Salary ~ . , data=hitData, nvmax=19, method="forward")
hitters.bwd <- regsubsets(Salary ~ . , data=hitData, nvmax=19, method="backward")
summary(hitters.fwd)
# The best one-variable model contains only CRBI
# the best two-variable model contains Hits and CRBI

summary(hitters.bwd)
# same result as above - the best one and two variable models are the same for backward
# and forward selection

# but the best seven variable models are different
coef(hitters.fwd, 7)
coef(hitters.bwd, 7)
coef(hitters.subsets.lm, 7)


# CHOSSING AMONG MODELS USING CROSS_VALIDATION --------------------------------------------

# 1) use only training data to fit the model
# (if we use the entire data set to do best subset selection, validation set errors
# will not be a good measure of the test error)
numVars = 19

set.seed(1)
trainIndices = sample(c(TRUE, FALSE), nrow(hitData), rep=TRUE)
testIndices = !trainIndices

###### SUBSET

# 1) Cross Validation on full data

subset.best = regsubsets(Salary ~ ., data=hitData[trainIndices, ], nvmax=numVars)
# compute validation set error for best model of each size
testMatrix = model.matrix(Salary ~ ., data=hitData[testIndices, ]) # an X matrix ???

# Extracting the coefficients from subset.best for the best model of that size
# and multiply them into the appropriate columns of the test model matrix to form the
# predictions then compute test MSE
validationErrors = rep(NA, numVars)
#prediction <- NULL 
for(i in 1:numVars){
      coef_i = coef(subset.best, id=i)
      predictions = testMatrix[, names(coef_i)] %*% coef_i 
      validationErrors[i] = mean( (hitData$Salary[testIndices] - predictions)^2 )
}
validationErrors
bestTrainingModel = which.min(validationErrors); bestTrainingModel
# the best model is the one with 10 variables

# object = the regsubsets object
# id = the model id
# testData = the original data subset by test Indices
predict.regsubsets <- function(object, testData, id, ...) {
      form = as.formula(object$call[[2]])
      testMatrix = model.matrix(form, testData)
      # For a particular model size (id = numvars), extract the coefficients from the
      # regsubsets object for the best model of that size (id)
      coef_i = coef(object, id =id) # id = the model id
      # get the predictor names for this model
      xVars = names(coef_i)
      # Multiply those coefficients into the appropriate columns of the test matrix
      testMatrix[, xVars] %*% coef_i 
}


# Final step: do best subset selection on full data set to get more accurate coefficient
# estimates then select that best ten-variable model, rather than use variables
# from the training set. 
subset.fulldata.best = regsubsets(Salary ~., data=hitData, nvmax=numVars)
# get the coefs of the model that was the best training model, but obtained using 
# full data set. 
coef(subset.fulldata.best, bestTrainingModel)

# Note: the best ten-var model on full data set has a different set of vars than the
# best ten-var model on training set



# 2) Cross Validation on Train / Test data

# Now choose among the models of different sizes using cross-validation. (Do best subset
# selection within each of the k training sets)

# 1) create vector to allocate each observation to one of k = 10 folds. Create matrix to 
# store results
numFolds = 10 # they name it k
set.seed(1)
folds = sample(1:numFolds, size=nrow(hitData), replace=TRUE)
cv.errors = matrix(NA, nrow=numFolds, ncol=numVars, dimnames=list(Folds=NULL, Vars=paste(1:numVars)))

# 2) do for loop to do cross validation. In the jth fold, the elements of folds that
# equal j are in the test set and the remainder are in the training set. Make predictions
# for each model size, compute test errors on the appropriate subset, and store them
# in appropriate slot in the matrix cv.errors
for(j in 1:numFolds){
      # fit data on training set (observations not in the kth fold)
      best.fit <- regsubsets(Salary ~ ., data=hitData[folds != j, ], nvmax = numVars)
      
      for(i in 1:numVars){ # for each model size ...
            # ... make a prediction based on the test set (data that IS in this current fold)
            pred = predict(best.fit, hitData[folds == j, ], id=i)
            # compute test error using observations from test set and the predictions we made
            cv.errors[j, i] <- mean( (hitData$Salary[folds == j] - pred)^2 )
            
            # note: ncol = numVars, nrow = num folds
            # (i, j)th elem = test MSE for ith cross validation fold for the best j-variable
            # model. 
      }
}
cv.errors
mean.cv.errors = apply(cv.errors, 2, mean) # apply mean function over COLS = numVars
mean.cv.errors
# mean(cv.errors[,1]) # 160093.5 = mean of first column

# Plotting the cv.errors by num variables
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')
# Comment: cross validation selects the 11-variable model
which.min(mean.cv.errors)

# or with ggplot (and plotting the minimum model)
df.cv <- data.frame(NumVars = 1:numVars, MeanCVErrors=mean.cv.errors)
iMinCV = which.min(mean.cv.errors)
ggplot(df.cv, aes(x=NumVars, y=MeanCVErrors)) + geom_line() + geom_point() + 
      geom_point(x=iMinCV, y=mean.cv.errors[iMinCV], color="magenta", size=3)


# Now do best subset selection on full data set to get the 11 variable model that we
# saw was the best
reg.best <- regsubsets(Salary ~., data=hitData, nvmax = numVars)
coef(reg.best, iMinCV)
# (Intercept)        AtBat         Hits        Walks       CAtBat        CRuns         CRBI 
# 135.7512195   -2.1277482    6.9236994    5.6202755   -0.1389914    1.4553310    0.7852528 
# CWalks      LeagueN    DivisionW      PutOuts      Assists 
# -0.8228559   43.1116152 -111.1460252    0.2894087    0.2688277 