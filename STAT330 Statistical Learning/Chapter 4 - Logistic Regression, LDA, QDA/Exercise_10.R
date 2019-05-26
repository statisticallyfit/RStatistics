setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 4 - Logistic Regression, LDA, QDA/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')
library(ISLR)
library(MASS)
library(car)
library(caret)

options(show.signif.stars = FALSE)


data("Weekly")
head(Weekly)

# GOAL: predict if for a given week the shares go up or down (Y = Direction)

# part a) Make some exploratory numerical and graphical summaries
summary(Weekly)
pairs(Weekly) # no seeming correlation line, just random cloud scatter, between any two pairs
cor(Weekly[-9])

# highest correlation is 0.84 between Volume and Year. As either variable increases,
# the other variable increases also. 
cor(Weekly$Year, Weekly$Volume)
cor(Weekly$Volume, Weekly$Year)

# NOT NEEDED: Plotting volume
#Index = seq_along(Weekly$Volume)
#ggplot(data=Weekly, aes(x=Index, y=Volume)) + geom_point(shape=19)
# shows increasing trend in data set: number of shares traded daily (volume) increased
# over time. 


# part b) logistic regr
week.glm <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Weekly,
                family=binomial)
summary(week.glm)
# Only lag2 is significant, given other predictors have been fitted


# part c) confusion matrix and overall fraction of correct predictions (NOT pos pred value, 
# is accuracy)
# METHOD 1
pred.probs = predict(week.glm, type="response") # predicted probabilities on the trainig
# data that was used to fit the model (if no test data is given)
pred.label = rep("Down", nrow(Weekly))
pred.label[pred.probs > 0.5] = "Up"
pred.label <- factor(pred.label)

week.tbl = table(PredictedDirection=pred.label, TrueDirection=Weekly$Direction); week.tbl
marginalTable(week.tbl)

# METHOD 2
confusionMatrix(data=pred.label, reference=Weekly$Direction, positive="Up")
# accuracy = 56.11%, just bit better than random chance
mean(pred.label == Weekly$Direction) # accuracy = overall fraction of correct predictions
# Accuracy: Logistic model is accurate 56.11% of the time
# Looking just at accuracy, model seems to predict a bit better than random chance
# BUT ... look at pos pred value too!


# INTERPRETATION POS PRED VALUE: Among the weeks that market is prediced to go up, the 
# model is correct only 56.43% of the time. 
posPredValue(data=pred.label, reference=Weekly$Direction, positive="Up") # model of pos pred value
# RANDOM Guessing of pos pred value
mean(pred.label == "Up") # random chance (of the pos pred value)

# So this model could be a lot better. It is not better than chance!


# Sensitivity: TP/P = when the market goes up, the model is right 92.07% of the time
sensitivity(mirror(week.tbl), positive="Up")
# Specificity: TN / N = when market goes down, model is right 11.16% of the time
specificity(mirror(week.tbl), positive="Up")
# PosPredValue: TP / P* = when market is predicted to go up, model is correct 56.43% of time
posPredValue(mirror(week.tbl), positive="Up")
# NegPredValue: TN / N* = when market is predicted to go down, model is correct
# only  52.9% of the time
negPredValue(mirror(week.tbl), positive="Up")

# PRedominance: UP: model predicts well in the up direction. 


# part d) Logistic - one predictor Lag 2-------------
trainIndices <- Weekly$Year < 2009 #which(Weekly$Year < 2009)
testData <- Weekly[!trainIndices, ]
trainData <- Weekly[trainIndices,]

week.train.glm <- glm(Direction ~ Lag2, data=trainData, family=binomial)
summary(week.train.glm)

pred.probs.test <- predict(week.train.glm, testData, type="response")
pred.label.test <- rep("Down", nrow(testData))
pred.label.test[pred.probs.test > 0.5] <- "Up"
pred.label.test <- factor(pred.label.test)

# confusion matrix
glm.tbl <- table(PredDirection=pred.label.test, TrueDirection=testData$Direction)
marginalTable(glm.tbl)

confusionMatrix(data=pred.label.test, reference=testData$Direction, positive="Up")

# Accuracy = percentage of correct predictions is 62.5 (both pos and down correct)
acc.glm <- mean(pred.label.test == testData$Direction); acc.glm 
# Pos pred value
posPredValue(mirror(glm.tbl), positive="Up") # so model does better than chance at
# predicting up. 
# random chance pos pred value
mean(testData$Direction == "Up")


# Sensitivity: TP/P = when the market goes up, the model says up 91.8% of the time
sensitivity(mirror(glm.tbl), positive="Up")
# Specificity: TN / N = when market goes down, model says down 20.9% of the time
specificity(mirror(glm.tbl), positive="Up")
# PosPredValue: TP / P* = when market is predicted to go up, model says up 62.2% of time
posPredValue(mirror(glm.tbl), positive="Up")
# NegPredValue: TN / N* = when market predicted down, model says down 64.28% of the time
negPredValue(mirror(glm.tbl), positive="Up")


# part e) same but for LDA ------------------------------
week.train.lda <- lda(Direction ~ Lag2, data=trainData)
week.train.lda

pred.lda <- predict(week.train.lda, testData)
pred.lda$class

# confusion matrix
lda.tbl <- table(PredDirection=pred.lda$class, TrueDirection=testData$Direction)
marginalTable(lda.tbl)

confusionMatrix(data=pred.lda$class, reference=testData$Direction, positive="Up")

# Accuracy = percentage of correct predictions is 62.5 (both pos and down correct)
acc.lda <- mean(pred.lda$class == testData$Direction); acc.lda

# Sensitivity: TP/P = when the market goes up, the lda says up 91.8% of the time
sensitivity(mirror(lda.tbl), positive="Up")
# Specificity: TN / N = when market goes down, lda says down 20.9% of the time
specificity(mirror(lda.tbl), positive="Up")
# PosPredValue: TP / P* = when market is predicted to go up, lda says up 62.2% of time
posPredValue(mirror(lda.tbl), positive="Up")
# NegPredValue: TN / N* = when market predicted down, lda says down 64.28% of the time
negPredValue(mirror(lda.tbl), positive="Up")


# part f) same but for QDA ------------------------------
week.train.qda <- qda(Direction ~ Lag2, data=trainData)
week.train.qda

pred.qda <- predict(week.train.qda, testData)
pred.qda$class

# confusion matrix
qda.tbl <- table(PredDirection=pred.qda$class, TrueDirection=testData$Direction)
marginalTable(qda.tbl)

confusionMatrix(data=pred.qda$class, reference=testData$Direction, positive="Up")

# Accuracy = percentage of correct predictions is 58.653% (both pos and down correct)
# lower than other methods
# Its accuracy is 58.6 even though it picked "Up" the whole time.
acc.qda  = mean(pred.qda$class == testData$Direction); acc.qda

# Sensitivity: TP/P = when the market goes up, the qda says up 100% of the time
sensitivity(mirror(qda.tbl), positive="Up")
# Specificity: TN / N = when market goes down, qda says down NEVER
specificity(mirror(qda.tbl), positive="Up")
# PosPredValue: TP / P* = when market is predicted to go up, qda says up 58.6% of time
posPredValue(mirror(qda.tbl), positive="Up")
# NegPredValue: TN / N* = when market predicted down, qda says down NAN of the time
# (since both sums are 0)
negPredValue(mirror(qda.tbl), positive="Up")


# part g) same but for KNN ------------------------------
library(class)

train.X <- as.matrix(trainData$Lag2)
test.X <- as.matrix(testData$Lag2)
train.Y <- as.matrix(trainData$Direction)
test.Y <- as.matrix(testData$Direction)

set.seed(1)
week.train.knn1 <- knn(train=train.X, test=test.X, cl=train.Y, k=1)
week.train.knn1


# confusion matrix
knn.tbl <- table(PredDirection=week.train.knn1, TrueDirection=testData$Direction)
marginalTable(knn.tbl)

confusionMatrix(data=week.train.knn1, reference=testData$Direction, positive="Up")

# Accuracy = percentage of correct predictions is 50% (both pos and down correct)
# lower than other methods
acc.knn = mean(week.train.knn1 == testData$Direction); acc.knn
# this is test error rate

# this is train error rate -- -CORRECTION - nope need to see Assg 1 Q3 classif for how
# to get the test and train knn errors. 
# mean(week.train.knn1 == trainData$Direction)

# Sensitivity: TP/P = when the market goes up, the knn says up 50.8% of the time
sensitivity(mirror(knn.tbl), positive="Up")
# Specificity: TN / N = when market goes down, knn says down 48.8% of the time
specificity(mirror(knn.tbl), positive="Up")
# PosPredValue: TP / P* = when market is predicted to go up, knn says up 58.49% of time
posPredValue(mirror(knn.tbl), positive="Up")
# NegPredValue: TN / N* = when market predicted down, knn says down 41.17% of the time.
negPredValue(mirror(knn.tbl), positive="Up")




# part h) ------ which model is better


t = cbind(rbind(acc.glm, acc.lda, acc.qda, acc.knn), 
          rbind(1 - acc.glm, 1 - acc.lda,1 - acc.qda,1 - acc.knn))
colnames(t) = c("Accuracy rates", "Test error rates")
t


# LDA and Logistic seem to have minimal error rates overall. 