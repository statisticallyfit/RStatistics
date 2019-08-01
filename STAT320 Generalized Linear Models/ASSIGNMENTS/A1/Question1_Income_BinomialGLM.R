setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggplot2)
options(show.signif.stars = FALSE)

# Data = family income of twenty houses, and info about home ownership. 
# status = 1 (yes for home ownership), 0 (no home ownership)
# income = income of the house in $10,000
homeData <- read.table("data/home.txt", header=TRUE)
homeDataNoFactor <- read.table("data/home.txt", header=TRUE)
head(homeData)

#Making the status a factor
homeData$status <- factor(ifelse(homeData$status == 1, "Yes", "No"))
levels(homeData$status) # since No is first in this list, it is the base level. 

# part a) --------------------------------------------------------------------------

# Fit logistic model with simple linear predictor where status = Y, income = X
income.glm <- glm(status ~ income, family=binomial, data=homeData)

# interpret coefs (later). Here is the summary table for presentation in part (a)
summary(income.glm)

# part b) --------------------------------------------------------------------------

anova(income.glm, test='Chisq')

# INTERPRET: 

# Deviance = 5.016, with df = 1, so p-value = 0.025
# ===> means that the income predictor is statistically significant. There is a nonzero
# relationship between status and income. 

# Residual deviance = 22.509, df = 18, p-value = 0.210168
# ==> since p-value is not < 0.05, this means the expected and observed values are
# similar ===> so model is a good fit for the data. 
1 - pchisq(22.509, df=18)

#ResidualDevianceTest(income.glm)
#DevianceTest(income.glm)


# part c,d) --------------------------------------------------------------------------

cof <- summary(income.glm)$coef[,1]
cofChart <- cbind(LogOdds=cof, OddsRatio=exp(cof), PercentChangeInOddsRatio=100*(exp(cof)-1))

cofChart

# INTERPRET COEFFICIENT: 

# ---> Income:

### log odds: for a one unit ($10,000) increase in income, the logodds of status increases by 0.99

### odds ratio: for a one unit ($10,000) increase in income, the odds of owning a home over odds of
# not owning a home increases by a factor of 2.7055

### percent change: for a one unit ($10,000) increase in income, the odds of owning a home over odds
# of not owning a home increases by 170.557 %. 



# INTERPRET CONFIDENCE INTERVAL: 
ci <- confint(income.glm)
ci

ciChart <- list(LogOdds.CI=ci, OddsRatio.CI=exp(ci),
                 PercentChangeOddsRatio.CI=100*(exp(ci) - 1))
ciChart

### log odds: we are 95% confident that for a $10,000 increase in household income, 
# the log odds of owning a home versus log odds of not owning a home increases
# between 0.11519 and 2.17.

### odds ratio: we are 95% confident that for a $10,000 increase in household income,
# the odds of owning a home over the odds of not owning a home increases by
# a factor of between 1.12209 and 8.758. 
# NOTE: an odds ratio of 1 would indicate that the odds of home ownereship and non-home
# ownership are the same. So, since this interval does not contain
# the odds ratio = 1, we know the income predictor is significant. 

# The 95% CI for the odds ratio for income is: (1.122, 8.758)

### percent change: we are 95% confident that for a $10,000 increase in household income,
# the odds of owning a home over odds of not owning a home increases between
# 12.209 % and 775.826 %. 


# part e) --------------------------------------------------------------------------

# Predict probability of home ownership (status = 1) for a house with income = $75,000
# and one with income = $110,000

# Since units of income is $10,000, need to divide these values to this amount:
# 75,000 / 10,000 ==> 7.5
# 110,000 / 10,000 ==> 11

newX <- data.frame(income=c(75000, 110000)/10000)
pred <- predict(income.glm, new=newX, type="response")
pred

# verify: 
linearPredictor <- function(incomeValue){ return(cof[1] + cof[2]*incomeValue) }

exp(linearPredictor(newX))/(1 + exp(linearPredictor(newX)))


# This matches the predicted probabilities. 
# Probability of home ownership is higher for a the given higher-income house
# than for the given lower-income house. 

# Plotting
ggplot(data=homeDataNoFactor, aes(x=income, y=status)) + geom_point(size=3) + 
      geom_line( aes(y=income.glm$fitted.values), color="dodgerblue", size=1) + 
      ggtitle("Probability of Ownership vs. Income") + 
      ylab("Predicted and Observed probabilities")

# Higher probability of home ownership when income is higher. 
