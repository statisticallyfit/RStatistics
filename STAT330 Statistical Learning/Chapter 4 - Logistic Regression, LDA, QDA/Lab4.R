setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 4 - Logistic Regression, LDA, QDA/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')
library(ISLR)
library(MASS)
library(car)
library(caret)

options(show.signif.stars = FALSE)


data("Smarket")
head(Smarket)
# Volume = number of shares (billions) traded on previous day
# Lag1-5 => percentage returns for each of the five previous trading days. 
# Today = percentage return on the date in question
# Direction = whether market was up or down on a particular date

# Pairwise correlations among predictors
cor(Smarket[,-9])
sum(cor(Smarket[,-9]) > 0.5)

# note: a matrix indices are questioned by unpacking elements snakewise (going down the rows
# first vertically, for each column):
m = matrix(c(1,2,3,4,11,12,13,14),nrow=2,byrow=TRUE)
m
# vector form
c(m) # how which() checks indices
which(m > 5)

# Now check which correlations are large
which(abs(cor(Smarket[,-9])) > 0.5)

# Lag vs Today returns =low correlations (low cor between today and previous day returns)
# Year vs Volume = higher correlation
plot(Smarket$Volume)
# Similar in ggplot:
Index = seq_along(Smarket$Volume)
ggplot(data=Smarket, aes(x=Index, y=Volume)) + geom_point(shape=19)
# Can see volume is increasing over TIME (index?): the number of shares traded daily
# (volume) increased from 2001 to 2005




# LOGISTIC REGRESSION -----------------------------------------------------------------
# Goal: Y = Direction, predictors = Lag1-5, Volume
market.glm <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
                  data=Smarket, family=binomial)
summary(market.glm)
# Lag1 negative coeff: for one unit increase in return yesterday, the market direction
# is less likely to go up today (or the odds of market going up today decreases by 
100*(exp(-0.073)-1)
# by 7.04%, holding other predictors constant. 

# Large p-values: no clear association between Lags and Direction

# Predicting response
pred.probabilities = predict(market.glm, type="response") # outputs probabilities P(Y = 1 | X), using the
# training data X that was used to fit the model. 
# These are probabilities of market going up because contrasts is 1 for Up

contrasts(Smarket$Direction)

# Classifying up or down based on probabilities
n = nrow(Smarket)
pred.label = rep("Down", n)
pred.label[pred.probabilities > 0.5] = "Up" # plugging in Yes wheneer prob > 0.5
pred.label


# table() to make confusion matrix
market.tbl <- table(Prediction=pred.label, Truth=Smarket$Direction)
totals.tbl = marginalTable(market.tbl); totals.tbl

# These commands for confusionmatrix are equivalent - table way and array way
conf = confusionMatrix(data=pred.label, reference=Smarket$Direction, positive="Up"); conf
confusionMatrix(mirror(market.tbl), positive="Up")

# Accuracy = fraction of values for which prediction is correct
# Logistic model correctly predicted movement of market for 52.2% of the time
accuracy = mean(pred.label == Smarket$Direction); accuracy
# OR
sum(diag(market.tbl)) # total number of correct predictions
# calc accuracy
diag(totals.tbl) 
(145 + 507) / 1250

# Training error rate = 1 - accuracy
1 - accuracy # trainerror rate tends to underestimate the test error rate


# New model: train data = 2001 to 2004 and Test data = 2005
trainIndices = Smarket$Year < 2005
trainData = Smarket[trainIndices, ]
testData = Smarket[!trainIndices, ]

market.train.glm <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
                        data=Smarket, family=binomial, subset=trainIndices)
market.train.glm
# Same as using training data directly
# glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
#     data=trainData, family=binomial)

pred.probabilities.test <- predict(market.train.glm, testData, type="response")
pred.label.test <- rep("Down", nrow(testData))
pred.label.test[pred.probabilities.test > 0.5] = "Up"

# confusion matrix
table(pred.label.test, testData$Direction)
# confusion matrix
confusionMatrix(data=pred.label.test, reference=testData$Direction, positive="Up")

mean("Up"== testData$Direction) # random chance percent "up"
mean(pred.label.test == testData$Direction) # accuracy, worse than chance
1 - 0.4802 # test error rate
# Test set error rate is 52%, which is worse than random guessing

# Causes of bad model: large p-valus for predictors
# ** NOTE: using predictors that have no relation to response causes deterioration
# in test error rate because the rise in predictors raise variance without lowring bias.


### New model
market.again.glm <- glm(Direction ~ Lag1 + Lag2, data=Smarket, family=binomial, subset=trainIndices)
pred.probs.test = predict(market.again.glm, testData, type="response")
pred.label.test = rep("Down", nrow(testData))
pred.label.test[pred.probs.test > 0.5] = "Up"
pred.label.test <- factor(pred.label.test)

# conf matrix 1
t = table(PredictionTest=pred.label.test, TruthTest=testData$Direction); t
c = confusionMatrix(data=pred.label.test, reference=testData$Direction, positive="Up"); c

accuracy = mean(pred.label.test == testData$Direction); accuracy
mean("Up" == testData$Direction) # random chance
1 - accuracy # test error rate

# So 56% of daily movements have been correctly predicted
# A much simpler strategy of predicting that market will increase every day will
# also be correct 56% of the time
# So logistic is no better than null model

# PosPredValue = 
# But confsion matrix shows that on days when logistic predicts increase in the market, 
# it has 58% accuracy rate (pospredvalue = TP / P*)
# POS PRED VALUE: "When model predicts "pos", it has a PPV % accuracy rate"
posPredValue(data=pred.label.test, reference=testData$Direction, positive="Up")
# SUggests a trading strategy of buying on days when model predicts increasing market
# and avoiding trades when model predicts decrease.



# Predict certain values: on a day 1 when lag1=1.2 and Lag2 = 1.1, and on a day2
# when lag1=1.5 and lag2=-0.8
predict(market.again.glm, newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)), type="response")




# LINEAR DISCRIMINANT ANALYSIS (LDA) ------------------------------------------------
library(dplyr)


market.lda = lda(Direction ~ Lag1 + Lag2, data=Smarket, subset=trainIndices)
market.lda
### Priors:
# prior_down = 0.49 => 49.2% of train observations correspond to days when market went down
# prior_up = 0.508 => 50.8% of train obs correspodn to days when market went up. 
### Group means:
# average of each predictor (Lag1, lag2) per class
mean(filter(trainData, Direction=="Down")$Lag1)
mean(filter(trainData, Direction=="Down")$Lag2)
mean(filter(trainData, Direction=="Up")$Lag1)
mean(filter(trainData, Direction=="Up")$Lag2)
### Coefficients of linear discriminants: the linear combination of Lag1 and Lag2
# used to form the LDA decision rule (discriminant function)
# discr(X) = -0.642 * Lag1 - 0.514 * Lag2
# If this is large, it will predict a market increase, and if small, lda classifier
# will predict market decline
# Plot of the linear descriminant, obtained by computing discr(X) for each of the
# training observations
plot(market.lda)

# What is plotted: (but for each separate class=Down or Up)
p = predict(market.lda)
# PREDICT: 
# $class = containsLDA's predictions about market movements (class labels)
# $posterior = matrix whose kth column contains the posterior probability that the
# corresponding observation belongs to the kth class (Up or Down)
# $x = contains the linear discriminant values:
df <- data.frame(p); head(df)
head(p$x, 10)
head(-0.642019 * df$Lag1 - 0.5135293 * df$Lag2, 10)
#downDF = data.frame(DownLD1=filter(df, class == "Down")$LD1) #, DownPosterior=df$posterior.Down)
#upDF = data.frame(UpLD1=filter(df, class == "Up")$LD1) #, UpPosterior=df$posterior.Up)
#ggplot(data=downDF, aes(x=DownLD1)) + geom_histogram()


# RULE: if there are k levels (classes) in response Y, there are (k-1) discriminant funcs
# so that there are (k-1) decision boundaries, meaning observations get separated
# into k regions. 
### HELP: why are one less LDi's than number of classes? Shouldn't they be ==?

# Scatterplot ggplot of discriminants
lda.df <- data.frame(Direction = trainData$Direction, p$x)
head(lda.df)
ggplot(data=lda.df, aes(LD1)) + geom_point(colour=lda.df$Direction,size=2.5)

# HELP not working - how to plot one single LD1?

# NOTE: LDA and logistic predictions are almost identical: 
pmarket = predict(market.lda, testData)
head(pmarket)
table(Prediction=pmarket$class, TrueDirection=testData$Direction)
# accuracy - similar to logistic regression
mean(pmarket$class == testData$Direction)

# OR
confusionMatrix(data=pmarket$class, reference=testData$Direction, positive="Up")



# Apply a 50% threshold to posterior probs to allow us to recreate predictions
# contained in pmarket$class
head(pmarket$posterior) # posterior for Downa nd Up
sum(pmarket$posterior[,1] >= 0.5) # down posteriors
sum(pmarket$posterior[,1] < 0.5) #  #up posteriors, method 1
sum(pmarket$posterior[,2] >= 0.5) # up posteriors, method 2

pmarket$posterior[1:20, 1] # rows 1-20 of down posteriors: probabilities that predict
# market will decrease
pmarket$class[1:20] #all of the classes mixed up

# Now applying a threshold other than 50% tomake classifications
# GOAL: predict a market decrease only if posterior probability is at least 90%
sum(pmarket$posterior[,1] > 0.9)
# No days meet that threshold
max(pmarket$posterior[,1]) # max prob decrease in 2005 was 52.0235%




# QDA analysis --------------------------------------------------------------------
market.qda <- qda(Direction ~ Lag1 + Lag2, data=Smarket, subset=trainIndices)
market.qda
# does not contain coefs of linear discriminants since QDA has a quadratic discrim func.

p2market <- predict(market.qda, testData)
# confmatrix 1
c1 = table(Prediction=p2market$class, TrueDirection=testData$Direction)
c1
c2 = confusionMatrix(data=p2market$class, reference=testData$Direction, positive="Up")
c2
# accuracy
mean(p2market$class == testData$Direction)
# QDA predictions are accurate almost 60% of the time even though the 2005 data was not
# used to fit the model, so more accurate than LDA and logistic
# Suguests QDA captures true relation more accurately than linear forms of LDA and logistic



## KNN Nearest Neighbors -------------------------------------------------------------
library(class)


# Arguments: 
# 1) train.X = matrix containing predictors associated with training data
# 2) test.X = matrix containing predictors of test data
# 3) train.Direction = vector containing class labels for training observations
# 4) K = number of nearest neighbors
train.X <- cbind(trainData$Lag1, trainData$Lag2)
test.X  <- cbind(testData$Lag1, testData$Lag2)
train.TrueDirection <- trainData$Direction

# NOTE: set seed because if several observations are tied as nearest neighbors,
# then R randomly breaks the tie. 
set.seed(1)
market.knn <- knn(train = train.X, test = test.X, cl = train.TrueDirection, k = 1)
class(market.knn)

# confusion matrix 1
c1 = table(Prediction=market.knn, TrueDirection=testData$Direction); c1
marginalTable(c1)
# accuracy
mean(market.knn == testData$Direction)
(43 + 83) / 252 # no better than chance! (k = 1 may be too flexible for the data)

# confusion matrix 2
c2 = confusionMatrix(data=market.knn, reference=testData$Direction, positive="Up"); c2

# -------- k = 3
market.knn3 = knn(train=train.X, test=test.X, cl=train.TrueDirection, k = 3)
#c1 = table(Prediction=market.knn3, TrueDirection=testData$Direction): c1
# what is wrong??
confusionMatrix(data=market.knn3, reference=testData$Direction, positive="Up")
mean(market.knn3 == testData$Direction)
# accuracy = 53.57% a bit better than chance
# But increasing K further has no further improvements, so QDA is best so far



# KNN to Caravan Insurance -----------------------------------------------------------

data("Caravan")
head(Caravan)

# 85 predictors of demographic traits for 5822 people
# Response = Purchase = whether or not person buys a caravan insurance policy
summary(Caravan$Purchase)
348/(348 + 5474) # only 6% of people in this data set bought the policy


# NOTE: scale and units of predictors affects knn prediction
# Approach: standardize xs so all xs has mean 0 and stdev = 1. Then all comparable
standardized.X = scale(Caravan[,-86])
var(standardized.X[,1])
var(Caravan[,1])
mean(standardized.X[,1])
mean(Caravan[,1])

testIndices = 1:1000
train.X = standardized.X[-testIndices,]
test.X = standardized.X[testIndices, ]

train.Y = Caravan$Purchase[-testIndices]
test.Y = Caravan$Purchase[testIndices]

set.seed(1)

# Making the knn model predictions
caravan.knn = knn(train=train.X, test=test.X, cl=train.Y, k = 1)

# how to know which is the positive class?
confusionMatrix(data=caravan.knn, reference=test.Y, positive="Yes") 
# test accuracy
mean(test.Y == caravan.knn) # model's total accuracy rate of purchase and non purchase
# test error rate: just under 12% error rate (wrong predictions 12% of the time)
mean(test.Y != caravan.knn)
1 - mean(test.Y == caravan.knn)
# Seems to be ok but look at: only 6% of customers actually purchased insurance
# RANDOM CHANCE rate of purchase:
mean(test.Y != "No")
mean(test.Y == "Yes") # random chance rate of purchase
# models' accuracy rate of purchase
posPredValue(data=caravan.knn, reference=test.Y, positive="Yes")

# ... so  we could get the error rate down to 6% by always predicting No regardless
# of the values of the predictors! ??? (meaning?)

# INTERPRETATION of Pos pred value: Among 77 customers  predicted to buy insurance, 
# there are 9 of them (or 11.7%)  actually do buy insurance. 

# INTERPRETATION OF random chance value: 6$ of real life people actually buy insurance.

# Model's pos pred value is better (double) the 6% success rate obtained
# from random guessing (null model)


# NOTE: so confusionmatrix with positive = "Yes" is the correct one, since
# the pospred values match. 


# K = 3 ----------------------------------------
caravan.knn3 = knn(train.X, test.X, cl=train.Y, k=3)
table(PredictedPurchase=caravan.knn3, TruePurchase=test.Y)
confusionMatrix(data=caravan.knn3, reference=test.Y, positive="Yes") 
# test accuracy 
acc = mean(caravan.knn3 == test.Y); acc
# test error rate
1 - acc

# Success rate = positive pred value = 0.192
5/(21 + 5)
posPredValue(data=caravan.knn3, reference=test.Y, positive="Yes")

# So from k = 1 to k = 3, the success rate increases from 11.6% to 19.2%
# With k = 5, the pos pred value is 26.7%. This is 4 = 26/6 times the rate from
# random guessing (since 6% is in the data already)


# K = 3 ----------------------------------------
caravan.knn5 = knn(train.X, test.X, cl=train.Y, k=5)
table(PredictedPurchase=caravan.knn5, TruePurchase=test.Y)
confusionMatrix(data=caravan.knn5, reference=test.Y, positive="Yes") 
# test accuracy 
acc = mean(caravan.knn5 == test.Y); acc
# test error rate
1 - acc
mean(caravan.knn5 != test.Y) # percentage of wrong test classifications

# Success rate = positive pred value = 26.7
posPredValue(data=caravan.knn5, reference=test.Y, positive="Yes")


# LOGISITIC MODEL WITH CARAVAN DATA ---------------------------------------------
caravan.glm <- glm(Purchase ~ ., data=Caravan, family=binomial, subset = -testIndices)
pred.probs.test = predict(caravan.glm, Caravan[testIndices, ], type="response")
pred.label.test = rep("No", 1000)
pred.label.test[pred.probs.test > 0.5] = "Yes"
pred.label.test <- factor(pred.label.test)

# accuracy
mean(pred.label.test == test.Y)
# test error rate = 1 - acc

table(PredictedPurchase=pred.label.test, TruePurchase=test.Y)
posPredValue(data=pred.label.test, reference=test.Y, positive="Yes")

# INTERPRETATION of Pos pred value: Among 7 customers (number of customers 
# predicted to buy insurance), 0 of them actually do buy insurance. 
# ISSUE!

# Use cutoff 0.25: predict a purchase any time predicted probability exceeds 0.25
pred.label.test = rep("No", 1000)
pred.label.test[pred.probs.test > 0.25] = "Yes"
pred.label.test <- factor(pred.label.test)

table(PredictedPurchase=pred.label.test, TruePurchase=test.Y)
posPredValue(data=pred.label.test, reference=test.Y, positive="Yes")

# INTERPRETATION pos pred value: Among the 33 customers predicted to buy insurance,
# the model predicts (is correct for) about 11 of them (33%) buy insurance. 

# This is 5 time sbetter than random guessing:
mean(test.Y == "Yes") # random guessing (of the pos pred value)
0.33333 / 0.059 # five times better
