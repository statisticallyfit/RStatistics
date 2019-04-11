setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 6 - Lasso, Subset, Ridge, PCR/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')
library(ISLR)
library(ggplot2)
library(gridExtra)

library(leaps)
library(glmnet)
library(pls)

# a) 
set.seed(1)
x <- rnorm(n=100)
epsilon <- rnorm(n=100)

# b) response
b0 <- 3; b1 <- 2; b2 <- -3; b3 <- 0.3
y <- b0 + b1*x + b2*x^2 + b3*x^3 + epsilon


# c) best subset
numVars <- 10
data <- data.frame(x=x, y=y)
# Finding the
norm.subsets <- regsubsets(y ~ poly(x, numVars, raw=TRUE), data=data, nvmax=numVars)
summarySubsets <- summary(norm.subsets)
summarySubsets

iMinRss = which.min(summarySubsets$rss); iMinRss
iMinCp = which.min(summarySubsets$cp); iMinCp
iMinBIC = which.min(summarySubsets$bic); iMinBIC
iMaxAdj <- which.max(summarySubsets$adjr2); iMaxAdj

# Model 3 is best from all measures
# Getting coeffs of the best model
coef(norm.subsets, iMinBIC)

# INTERPRETATION FO COEFFICIENTS: 
# The topmost picture row has black squares only at intercept, poly1, poly2, and poly7,
# corresponding to the existing coefficients of the third model (best model)
plot(norm.subsets, scale="bic")

summarySubsets$bic[iMinBIC] # the min BIC is the topmost one on the plot above


# PLOT FOR BEST MODEL SUMMARY: 
df <- data.frame(NumVars=1:numVars, RSS=summarySubsets$rss, AdjR=summarySubsets$adjr2, 
                 Cp=summarySubsets$cp, BIC=summarySubsets$bic)

p.rss <- ggplot(df, aes(x=NumVars, y=RSS)) + geom_line() + 
      geom_point(aes(x=iMinRss, y=summarySubsets$rss[iMinRss]), size=3, colour="magenta")

p.adj <- ggplot(df, aes(x=NumVars, y=AdjR)) + geom_line() + 
      geom_point(aes(x=iMaxAdj, y=summarySubsets$adjr2[iMaxAdj]), size=3, colour="magenta")

p.cp <- ggplot(df, aes(x=NumVars, y=Cp)) + geom_line() + 
      geom_point(aes(x=iMinCp, y=summarySubsets$cp[iMinCp]), size=3, colour="magenta")

p.bic <- ggplot(df, aes(x=NumVars, y=BIC)) + geom_line() + 
      geom_point(aes(x=iMinBIC, y=summarySubsets$bic[iMinBIC]), size=3, colour="magenta")

grid.arrange(p.rss, p.adj, p.cp, p.bic)




# d)  Repeat c) using forward stepwise selection and backwards stepwise. Comparison?
norm.forward <- regsubsets(y ~ poly(x, numVars, raw=TRUE), data=data, nvmax=numVars, method="forward")
norm.backward <- regsubsets(y ~ poly(x, numVars, raw=TRUE), data=data, nvmax=numVars, method="backward")
s.fwd <- summary(norm.forward); s.fwd
s.bwd <- summary(norm.backward); s.bwd

iMinCp.fwd <- which.min(s.fwd$cp); iMinCp.fwd
iMinBIC.fwd <- which.min(s.fwd$bic); iMinBIC.fwd
iMaxAdj.fwd <- which.max(s.fwd$adjr2); iMaxAdj.fwd

iMinCp.bwd <- which.min(s.bwd$cp); iMinCp.bwd
iMinBIC.bwd <- which.min(s.bwd$bic); iMinBIC.bwd
iMaxAdj.bwd <- which.max(s.bwd$adjr2); iMaxAdj.bwd

# Plotting Model selection Statistics Cp, BIC, ...
df.fwd <- data.frame(NumVars=1:numVars, AdjR=s.fwd$adjr2, 
                 Cp=s.fwd$cp, BIC=s.fwd$bic)


p.fwd.adj <- ggplot(df.fwd, aes(x=NumVars, y=AdjR)) + geom_line() + 
      geom_point(aes(x=iMaxAdj.fwd, y=s.fwd$adjr2[iMaxAdj.fwd]), size=3, colour="green")

p.fwd.cp <- ggplot(df.fwd, aes(x=NumVars, y=Cp)) + geom_line() + 
      geom_point(aes(x=iMinCp.fwd, y=s.fwd$cp[iMinCp.fwd]), size=3, colour="green")

p.fwd.bic <- ggplot(df.fwd, aes(x=NumVars, y=BIC)) + geom_line() + 
      geom_point(aes(x=iMinBIC.fwd, y=s.fwd$bic[iMinBIC.fwd]), size=3, colour="green")

grid.arrange(p.fwd.adj, p.fwd.cp, p.fwd.bic)


# -- backward
df.bwd <- data.frame(NumVars=1:numVars, AdjR=s.bwd$adjr2, 
                     Cp=s.bwd$cp, BIC=s.bwd$bic)


p.bwd.adj <- ggplot(df.bwd, aes(x=NumVars, y=AdjR)) + geom_line() + 
      geom_point(aes(x=iMaxAdj.bwd, y=s.bwd$adjr2[iMaxAdj.bwd]), size=3, colour="orange")

p.bwd.cp <- ggplot(df.bwd, aes(x=NumVars, y=Cp)) + geom_line() + 
      geom_point(aes(x=iMinCp.bwd, y=s.bwd$cp[iMinCp.bwd]), size=3, colour="orange")

p.bwd.bic <- ggplot(df.bwd, aes(x=NumVars, y=BIC)) + geom_line() + 
      geom_point(aes(x=iMinBIC.bwd, y=s.bwd$bic[iMinBIC.bwd]), size=3, colour="orange")

grid.arrange(p.bwd.adj, p.bwd.cp, p.bwd.bic)

# Most of the measures pick the 3-variable models for forward
coef(norm.forward, id=iMinBIC.fwd)
# 3-variable backward model
coef(norm.backward, id=iMinBIC.bwd)



# e) lasso , use cross-validation to select optimal lambda
X = model.matrix(y ~ poly(x, numVars, raw=TRUE))[,-1] # get rid of intercept (?) why?
X
Y = y 
norm.lasso <- cv.glmnet(X, Y, alpha=1)
bestLambda.lasso <- norm.lasso$lambda.min; bestLambda.lasso
# 0.0399
plot(norm.lasso) # plots log lambda against MSE estimated

# Fit the best-lambda model
bestModel <- glmnet(X, Y, alpha=1) # why is this different than norm.lasso model?
# coefs of the model with minimum lambda
bestLambda.coefs <- predict(bestModel, s = bestLambda.lasso, type="coefficients")
bestLambda.coefs[1:11,]


# part f) generate response vec and do best subset and lasso
b7 = 7 
y = b0 + b7*x^7 + epsilon

# SUBSET ----------------------------
data <- data.frame(x=x, y=y)
norm.full.subset <- regsubsets(y ~ poly(x, numVars, raw=TRUE), data=data, nvmax = numVars)
normSummary <- summary(norm.full.subset)
normSummary

iMinCp <- which.min(normSummary$cp); iMinCp
iMinBIC <- which.min(normSummary$bic); iMinBIC
iMaxAdj <- which.max(normSummary$adjr2); iMaxAdj

# Get coefficients of best models
coef(norm.full.subset, id=iMinBIC) # most accurate to real model
coef(norm.full.subset, id=iMinCp)
coef(norm.full.subset, id=iMaxAdj)

# *can do the minimum ggplots too

# LASSO ----------------------------

# BIC picks the most accurate model since it has only the X^7 term
X = model.matrix(y ~ poly(x, numVars, raw=TRUE))[,-1] # get rid of intercept (?) why?
X
Y = y 
norm.lasso <- cv.glmnet(X, Y, alpha=1)
bestLambda.lasso <- norm.lasso$lambda.min; bestLambda.lasso
# 13.574 # blog gets 12.37 ???
plot(norm.lasso) # plots log lambda against MSE estimated

# Fit the best-lambda model
bestModel <- glmnet(X, Y, alpha=1) # why is this different than norm.lasso model?
# coefs of the model with minimum lambda
bestLambda.coefs <- predict(bestModel, s = bestLambda.lasso, type="coefficients")
bestLambda.coefs

# intercept is a bit off = is 3.8 not 3
# Best subset min BIC model has most accurate coefficients