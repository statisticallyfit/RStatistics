setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 5 - Cross-Validation, Bootstrap/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')
library(ISLR)


# LOOCV test error rate study: via for loop and via cv.glm()
data("Weekly")

# a) 
week.glm <- glm(Direction ~ Lag1 + Lag2, data=Weekly, family=binomial)

# b) fit model using all but the first observation
week.missingObs1.glm <- glm(Direction ~ Lag1 + Lag2, family=binomial, data=Weekly,
                            subset=2:nrow(Weekly))

# c) use the missing-one model to predict first observation

# test data is the missing observation row
prob1 = predict(week.missingObs1.glm, Weekly[1,], type="response"); prob1
# 0.5713923
label1 = if(prob1 > 0.5) "Up" else "Down"
label1
# "Up"

# true direction: was "Down"
Weekly$Direction[1]


# d) for loop

n <- nrow(Weekly)
binaryClassifications <- rep(0, n)

set.seed(1)

for(i in 1:n){
      # fit model using all but the ith observation
      missingObsi.glm <- glm(Direction ~ Lag1 + Lag2, family=binomial, data=Weekly[-i,])
      # compute posterior probability of market moving up
      # test data is that ith observation
      prob.i <- predict(missingObsi.glm, Weekly[i, ], type="response")
      # compute classification
      label.i <- if(prob.i > 0.5) "Up" else "Down"
      # find if a missclassification occurred: vector of 1's (correct classification)
      # and 0's (missclassification)
      binaryClassifications[i] <- as.numeric(Weekly$Direction[i] != label.i)
}
LOOCV.testErrorEstimate <- mean(binaryClassifications); LOOCV.testErrorEstimate
# high test error rate: classified wrongly 0.4499541 * 100 % of the time. 