setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 4 - Logistic Regression, LDA, QDA/")
library(ISLR)
library(MASS)
library(car)

options(show.signif.stars = FALSE)

data(Default)
head(Default)

#relevel to base level as "Yes" because before it was "No" (same result)
levels(Default$default)
#Default$default <- relevel(Default$default, ref="Yes")
#levels(Default$default)

default.lda <- lda(default ~ student + balance, data=Default)
summary(default.lda)

default.lda

default.lda$prior
default.lda$means
default.lda$N
nrow(Default) # equals sample size, N

# Coefficients of the linear descriminant:
# -0.249 * studentYes + 0.002 * balance
coef(default.lda)

# Predictions
pred.list <- predict(default.lda)
# easier to see in data frame
head(ldaToDataFrame(default.lda))

# The overall LDA error Rate 
# (estimate of Bayes error rate, which can never be computed
# in real life because we don't have population values)
#LDAErrorRate <- 1 - mean(pred.list$class == Default$default)
#LDAErrorRate
ldaErrorRate(default.lda, observedY=Default$default)

# My two-way Confusion matrix: Counting predicted classes and Default classes
conf.mat = twoWayConfusionMatrix(default.lda, Default$default);conf.mat





# -----------------------------------------
library(caret) #library(hmeasure)

pred = predict(default.lda)$class
truth = Default$default
default.tbl = table(pred, truth)
default.tbl

# using the DATA.R file
mirror(default.tbl)

conf.mat # print confusion matrix  (my two way matrix)
confusionMatrix(data= pred, reference=truth, positive="Yes") # the caret library function

# sensitivity = TP / P = 81 / 333 = 0.243
sensitivity(pred, truth, positive="Yes")
sensitivity(mirror(default.tbl), positive="Yes")

# specificity = TN / N = 9644 / 9667 = 0.9976208
specificity(pred, truth, positive="Yes") # see - is 0.243, wrong if we don't relevel
specificity(mirror(default.tbl), positive="Yes") # but using table + my mirror takes care of issue

# negpredvalue = TN / N* = 9644 / 9896 = 0.9745352
negPredValue(pred, truth, positive="Yes") # should relevel Default$default, not correct
negPredValue(mirror(default.tbl), positive="Yes") # but using mirror takes care of issue

# posPredValue = TP / P* = 81 / 104 = 0.7788462
posPredValue(pred, truth, positive="Yes")
posPredValue(mirror(default.tbl), positive="Yes")


# THESE ARE NOT CORRECT WITHOUT USING MIRROR: there must be a bug in the functions
# for not recognizing col/row names!!!!
# sensitivity(default.tbl, positive="Yes")
# specificity(default.tbl, positive="Yes")
# negPredValue(default.tbl, positive="Yes")
# posPredValue(default.tbl, positive="Yes")

