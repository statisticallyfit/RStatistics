setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')

library(ggplot2)
library(ggfortify)
options(show.signif.stars = FALSE)


# temp = temperature of the shuttle launcher thing
# failure: 0 = no failure, 1 = one or more failures
shuttleData <- read.table("data/shuttle.txt", header=TRUE)
head(shuttleData)
shuttleData$failure <- factor(ifelse(shuttleData$failure == 0, "No", "Yes"))

# part a) b) -----------------------------------------------------------------------------

shuttle.glm <- glm(failure ~ temp, data=shuttleData, family=binomial)

anova(shuttle.glm, test='Chisq')

# INTERPRET: 
# --> Deviance = 5.944, p-value = 0.0147 ===> means the temperature is an important
# predictor of shuttle failure. Or that the temp model is significantly better predictor
# of shuttle failure than the null model
DevianceTest(shuttle.glm)

# ---> ResidualDeviance = 23.0304, p-value = 0.400037 ===> so the model is good fit, since
# the expected and observed (s/fs) are not too different. 
1 - pchisq(23.03, df=22)
ResidualDevianceTest(shuttle.glm)

summary(shuttle.glm)


# part c) ------------------------------------------------------------------------------

# Predict probability of failure for each of the observed temperatures: 

# Way 1: (in Y units, has been transformed already: exp(n)/(1+exp(n))) 
shuttle.glm$fitted.values

# Way 2: (in Y units, has been transformed already: exp(n)/(1+exp(n)))
pred <- predict(shuttle.glm, type="response") 
pred

# part d) ------------------------------------------------------------------------------

# Plotting predictions with observed data: 
# plotConfidenceBands.glm(shuttle.glm) # doesn't work since different Y-axis for the
# data (yes/no) and the fitted values (probabilities, whcih are between 0, 1)
ggplot(data=shuttleData, aes(x=temp, y=pred)) + geom_point(size=3) + 
      geom_line(color="red") + ggtitle("Predicted Probability of O-ring failure vs Temperature")

 #INTERPRET: the high probability of failure for low temperatures and low probability of failure
# for higher temperatures. 

# part e,f) ------------------------------------------------------------------------------

# Predict prob failure for temperatures of 31, 50, 75 degrees F
newXs <- data.frame(temp=c(31, 50, 75)) # note: must have same title here as predictor name
predict(shuttle.glm, new=newXs, type="response")
# Or
n = predict(shuttle.glm, new=newXs)
exp(n)/(1+exp(n))

# INTERPRET: P(failure | temp = 31 degrees F) = 0.99618
# So there is very high probability of failure for this temperature. 
# Unacceptably high risk of failure (even though we are extrapolating beyond the
# range of x-values)

# My Recommendation: on that day of the launch, do not launch! high probability of
# crashing at 31 degrees. 

# part f) ------------------------------------------------------------------------------

# Find and interpret the odds ratio for temperaure and its CI
cof <- betaCI(shuttle.glm)[,c(1,3,4)]
cof.list <- list(LogOdds=cof, OddsRatio=exp(cof), OddsRatioPercentChange=100*(exp(cof)-1))

cof.list
#### Odds ratio:
# ---> temp: the odds ratio for temperature is 0.84255. This means: for a 1 degree increase
# in temperature, the odds of shuttle failure over odds of no failure is 0.8425. 
#### Percent change: 
# ---> temp: for a one degree increase in temperature, the odds of a shuttle failure
# versus the odds of no shuttle failure decreases by 15.74%. 