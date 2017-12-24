# Source: www.ats.ucla.edu/stat/r/dae/logit.htm

library(aod)
#install.packages("aod")
library(ggplot2)

# response: admit (0/1, admit or don't admit)
schoolData <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
head(schoolData)
summary(schoolData)
sapply(schoolData, sd)

# two-way contingency table of categorical outcome and
# predictors, make sure there are no 0 cells
xtabs(~admit + rank, data=schoolData)

schoolData$rank <- factor(schoolData$rank)
logit <- glm(admit ~ gre + gpa + rank, data=schoolData, 
             family="binomial")
logit
summary(logit) # rank2,3,4 are compared to rank1

# Confints using profiled log-likelihood function
confint(logit)
# Confints using standard errors
confint.default(logit)

# Test overall effect of [rank]
wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 4:6)
# overall rank effect is significant

# Test that difference of rank2 and rank3 coeffs is 0
l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(logit), Sigma = vcov(logit), L = l)
# thus the rank2 and rank3 coeffs are different

# odds-ratios
exp(coef(logit))

# odds ratios and confints
exp(cbind(OR = coef(logit), confint(logit)))
