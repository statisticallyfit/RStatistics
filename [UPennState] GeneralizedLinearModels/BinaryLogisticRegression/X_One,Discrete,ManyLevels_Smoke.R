source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R')

# install.packages("ResourceSelection")



# Penn state stat 504 example: 
# https://onlinecourses.science.psu.edu/stat504/node/225



### FITTING LOGISTIC REGRESSION FOR 2X2 TABLE 


#                  |  student smokes  | student does not smoke
# -------------------------------------------------------------
# parents smoke    |    816           |      3203
# no parents smoke |    188           |      1168



### FITTING LOGISTIC REGRESSION FOR 2X2 TABLE

# predictor has two levels: X = 1 (parents smoke), X = 0 (no parents smoke)
# because it is arranged as (1,0) the baseline (0) is that neither parent is smoking (X = 0)
parentSmoke <- as.factor(c(1,0)); parentSmoke
# response vector with counts for both success and failure (students smoke)
response <- cbind(yes = c(816, 188), no = c(3203, 1168)); response 


# Logistic model (saturated)
BinaryLogisticRegression_T(response)
smoke.logistic <- glm(response ~ parentSmoke, family = binomial(link = "logit"))
smoke.logistic

LikelihoodRatioTableTest(response)

# Intercept-only model
reduced.logistic <- glm(response ~ 1, family = binomial(link = "logit"))
reduced.logistic

# fisher scoring is variant of newton-raphson but they
# are equivalent in logistic regression. 
summary(smoke.logistic) 
summary(reduced.logistic)$coef

# residual deviance = G2 (saturated) - G2 (built model) = 0 here because this is saturated.
ResidualDeviance(smoke.logistic)
ResidualDeviance(reduced.logistic)
# null deviance = G2 (saturated) - G2 (intercept-only model)
NullDeviance(smoke.logistic)
ResidualDeviance(reduced.logistic)

##### PARAMETER INTERPRETATION
# Estimated $B&B(B0 =$B!](B1.827 with standard error 0.078 is significant and it says 
# that log-odds of a child smoking versus not smoking if neither 
# parents is smoking (the baseline level) is -1.827 (statistically significant).

# Estimated $B&B(B1 = 0.459 with standard error 0.088 is significant and it says 
# that log-odds-ratio of a child smoking versus not smoking if at least one 
# parent is smoking versus neither parents is smoking (the baseline level) 
# is 0.459 (statistically significant). 
# exp(0.459)=1.58 are the estimated odds-ratios




### FITTING LOGISTIC REGRESSION FOR 2X3 TABLE 
parentSmoke <- as.factor(c(2,1,0))
response <- cbind(c(400,416,188), c(1380,1823,1168)); response 

## Saturated model
# short way
result <- BinaryLogisticRegression_T(response) 
result$PredictLogisticScale
result$PredictLinearScale

# long way
smoke.logistic <- glm(response ~ parentSmoke, family=binomial(link="logit"))
summary(smoke.logistic)$coef
reduced.logistic <- glm(response ~ 1, family=binomial(link = "logit"))
summary(reduced.logistic)$coef

# deviance tests
ResidualDeviance(smoke.logistic)
ResidualDeviance(reduced.logistic)
NullDeviance(smoke.logistic)
NullDeviance(reduced.logistic)

# Likelihood ratio test + Chisquare
reduced.logistic$deviance - smoke.logistic$deviance
LikelihoodRatioTableTest(response)
LikelihoodRatioModelTest(reduced.logistic, smoke.logistic)
ChiSquareIndependence(response)

# anova
anova(reduced.logistic, smoke.logistic)
anova(reduced.logistic)
anova(smoke.logistic)




# NOTICE THAT G2 from null model (intercept only) is the same as G2 from 2-way table
smoke <- matrix(c(816,3203,188,1168), byrow=TRUE,nrow=2,
                dimnames=list(ParentSmoke=c("At least one","Neither"),
                              StudentSmoke=c("Yes","No"))); smoke

# null model test
nullModel <- glm(smoke ~ 1, binomial(link = "logit"))
NullDeviance(nullModel)
ResidualDeviance(nullModel)

# See nullModel has same G2 (in both null and residual deviance) as G2 from LikRatioTest
LikelihoodRatioTableTest(smoke)
OddsRatioCI(smoke)







##### HOSMER-LEMESHOW test for overall goodness-of-fit of logistic model. 
# method 1 
HosmerLemeshowTest(smoke.logistic) 
# method 2 
HosmerLemeshowTest_Calc(smoke.logistic, g=3)
# method 3
y.hat <- rowSums(response) * result$PredictLogisticScale; y.hat
y <- response[,1]; y 
HosmerLemeshowTest_T(y, y.hat, g=3)



