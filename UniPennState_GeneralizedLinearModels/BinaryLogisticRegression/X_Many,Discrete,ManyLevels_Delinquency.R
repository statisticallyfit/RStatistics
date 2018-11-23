source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R')
setwd("/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/learnstatistics/[PennState] GeneralizedLinearModels/BinaryLogisticRegression")


### Input the table
delinquent <- c("no","yes")
scout <- c("no", "yes")
SES <- c("low","med","high")
#table <- expand.grid(delinquent=c("yes","no"), scout=c("yes","no"),
table <- expand.grid(delinquent=delinquent, scout=scout, SES=SES)
#c(11,42,43,169, 14,20,104,132, 8,2,196,59)
table <- cbind(table, count=c(169,42,43,11,132,20,104,14,59,2,196,8))
table
temp.by.SES <- xtabs(count ~ delinquent + scout + SES, table); temp.by.SES
temp.by.scout <- xtabs(count ~ delinquent + SES + scout, table); temp.by.scout
temp.by.delinquent <- xtabs(count ~ SES + scout + delinquent, table); temp.by.delinquent

table <- ftable(temp.by.delinquent)




### LOGISTIC REGRESSION for marginal table (scout x delinquent, margin=SES)
scout_delinquent <- margin.table(temp.by.SES, c(2,1)); scout_delinquent
log(oddsRatio(scout_delinquent))
OddsRatioCI(scout_delinquent)

# Marginal Table 1 = scout x delinquent, margin = SES
# BoyScout = nonscout (base), base is chosen alphabetically by R.
scout_delinquent
B <- factor(c("scout", "non-scout"))
B.scout <- B == "scout"; B.scout
B.nonscout <- B == "non-scout"; B.nonscout
yes <- c(33,64)
n <- c(376,424)
count_1 <- cbind(yes, n-yes); count_1
# Saturated model
B.model <- glm(count_1 ~ B.scout + B.nonscout, family=binomial("logit"))
coef(B.model)
# same way of stating the above
B.model <- glm(count_1 ~ B.scout, family=binomial("logit"))
coef(B.model) # baseline is nonscout (intercept)
# NOTE: -0.614 is identical to log-odds ratio of scout_delinquent
log(oddsRatio(scout_delinquent)) # logodds for scouts = yes is equal to coef of B1
log(64/360)                      # logodds for scouts = no is equal to coef of B0

ResidualDeviance(B.model)
NullDeviance(B.model)
# null deviance = G2 from testing idnependence in 2x2 table - check if for n x n too. 
LikelihoodRatioTableTest(count_1, printNice = FALSE)$LikRatio # same as null deviance
anova(B.model)




# Marginal Model 2 = SES x delinquent, margin=scout
# SES = low (base)
SES_delinquent <- margin.table(temp.by.SES, c(1,3)); SES_delinquent
S <- factor(c("low","medium", "high"))
yes <- c(53, 34, 10)
n <- c(265,270,265)
count_2 <- cbind(yes, n-yes); count_2
S.med <- S == "medium"; S.med
S.low <- S == "low"; S.low
S.high <- S == "high"; S.high
# Saturated model ######################### TODO why saturated? Shouldn't it have all combos?
S.model <- glm(count_2 ~ S.med + S.high, family=binomial(logit))
coef(S.model) # baseline (intercept) is S.low coef = -1.386
log(oddsRatio(SES_delinquent)) # same coeffs as logodds ratio
### NOTE: base is S=low 
# log odds of low/high = -1.852 is the same as S.highTRUE coef (with respect to S = low base)
# log odds of low/med = -0.5512 is same as S.medTrue with respect to S = low base
# log(53/212) of all delinquent on SES = low = -1.38629 = coef of intercept S = low base

ResidualDeviance(S.model) # almost zero because model is saturated
NullDeviance(S.model) # big because model far from null intercept model
# conclude that SES is related to delinquency, because we reject Ho of independence.
LikelihoodRatioTableTest(count_2,p=F)$LikRatio
ChiSquareIndependence(count_2,p=F)$Chi
anova(S.model)




# Conditional Model 3 = SES x scout | delinquent  
table
S
S <- factor(rep(c("low","medium","high"),c(2,2,2))); S # rep each level twice, keep together
S.low <- S == "low"; S.low
S.med  <- S == "medium"; S.med
S.high <- S == "high"; S.high
B <- factor(rep(c("scout", "nonscout"), 3)); B # rep each 3x but not together
B.scout <- B == "scout"; B.scout
B.nonscout <- B == "nonscout"; B.nonscout
yes <- c(11,42,14,20,8,2)
n <- c(54,211,118,152,204,61)
count_3 <- cbind(yes, n-yes); count_3
# the below two mdoels are the same whether we include B-non or S.high, it doesn't matter
SB.model <- glm(count_3 ~ B.scout + B.nonscout + S.low + S.med + S.high, family=binomial(logit))
coef(SB.model)
# NOTE: S=high (base), B=nonscout (base)
SB.model <- glm(count_3 ~ B.scout + S.low + S.med, family=binomial(logit))
coef(SB.model)
# mixing model ############################## IS this the saturated model? 
saturated.model <- glm(count_3 ~ B.scout + S.med + S.high + B.scout:S.med + B.scout:S.high,
              family=binomial(logit))
# See that log-odds ratio when S=low equals B1 coef = 0.0289
coef(saturated.model)

# S = low (base), B = nonscout (base)
# note - doesn't matter if we include the interaction base or not (B=nonscout, S=low)
coef(glm(count_3 ~ B.scout + S.med + S.high + B.scout:S.med + B.scout:S.high + 
               B.nonscout:S.low, 
         family=binomial(logit)))
# S = low (base), B = nonscout (base)
# note - doesn't matter if we include the base or not (B=nonscout)
coef(glm(count_3 ~ B.scout + B.nonscout + S.med + S.high + 
               B.scout:S.med + B.scout:S.high, family=binomial(logit)))
# S = high (base), B = nonscout (base)
# note - matters if alphabetically-changing variable is placed (S.low) so base is S.high
# and B = nonscout (variable B = scout doesn't rank higher in alphabet so it is not
# the base, which means B.nonscout is the base)
coef(glm(count_3 ~ B.scout + B.nonscout + S.low + S.med + S.high + 
               B.scout:S.med + B.scout:S.high, family=binomial(logit)))
# S = high (base), B = scout (base)
coef(glm(count_3 ~ B.nonscout + S.low + S.med + S.high + 
               B.scout:S.med + B.scout:S.high, family=binomial(logit)))

### NOTE: these two are the same because the variables noted in second saturated model
# are hidden in intercept in first saturated model. 
coef(saturated.model)
coef(glm(count_3 ~ B.scout + S.med + S.high + B.scout:S.med + B.scout:S.high + 
               # these variables below are part of the intercept in saturated.model model (same)
               B.nonscout + B.scout:S.low +  B.nonscout:S.med + B.nonscout:S.high + 
               B.nonscout:S.low, 
         family=binomial(logit)))

ResidualDeviance(saturated.model)
NullDeviance(saturated.model)
anova(saturated.model)

### To get the scaled covariance matrix
covMatrix <- summary(saturated.model)$cov.scaled
covMatrix
# get stdandard error for beta_1 + beta_4 estimate
saturated.model$call
covMatrix[2,2]
covMatrix[5,5]
covMatrix[2,5]
# estimated standard error for B1 and B4
stderr.hat.b1.b4 <- sqrt(covMatrix[2,2] + covMatrix[5,5] + 2*covMatrix[2,5]) 
stderr.hat.b1.b4
# odds ratio for scout x delinquent for level S = medium = 0.8885 
# and log(0.8885) = -0.11827
# And B1 + B4 = -0.11827
coef(saturated.model)[2]
coef(saturated.model)[5]
coef(saturated.model)[2] + coef(saturated.model)[5] # B1 + B4 = -0.11827
############################ TODO make oddsratio function work with ftable
table
# get estimate of beta_1 + beta_4
coef(saturated.model)[2]
coef(saturated.model)[5] 




##### NOTE: this and the below are the same even though data is differently prepared
## for S.model, S = was low.med.high.
## for result3.3, S = low low, med med, high high
S.model_unfit <- glm(count_3 ~ S.med + S.high, family=binomial(logit))
coef(S.model_unfit)
coef(S.model)

ChiSquareIndependence(count_3,p=F)$Chi
LikelihoodRatioTableTest(count_3,p=F)$Lik

#ConditionalIndependence(temp.by.delinquent) #why not showing statistic 0.1602??

# Current model did not change but when we changed ordering of S, we changed current model
# so the residual deviance changes in comparison with S.model
ResidualDeviance(S.model_unfit) 
ResidualDeviance(S.model)
NullDeviance(S.model_unfit) # same as likelihood ratio for 3x2 table (tempbydelinq[,,1])
NullDeviance(S.model)

anova(S.model_unfit)








#  Conditional Model 4 = SES x scout | delinquent
# The same saturated model as above except expressed differently so that coefficients
# are easier to interpret. 
S <- factor(rep(c("low","medium","high"),c(2,2,2))); S # rep each level twice, keep together
B <- factor(rep(c("scout", "nonscout"), 3)); B # rep each 3x but not together
yes <- c(11,42,14,20,8,2)
n <- c(54,211,118,152,204,61)

x1 <- S == "low"; x1
x2 <- (S == "low")*(B == "scout"); x2
x3 <- S == "medium"
x4 <- (S == "medium") * (B == "scout"); x4
x5 <- S == "high"; x5
x6 <- (S == "high") * (B == "scout"); x5

count <- cbind(yes, n-yes); count

# the -1 means no intercept (because if there were, we would have COLLINEARITY)
# because there is no more room for any meaningful coefficient as intercept.
# Because there's no intercept, B1 is the intercept in first row, not B0. 
saturated.model.neat <- glm(count ~ cbind(x1, x2, x3, x4, x5, x6) - 1, 
                            family=binomial(logit))
# Coefs are log-odds of delinquency for each SxB group
cs <- coef(saturated.model.neat); cs

# Verifying that logodds of S = Low equals B2 (effect of scouting when S=low)
table
log(169*11/(42*43))
# Verifying that logodds of S = med equals B4 (effect of scouting when S=med)
log(132*14/(20*104))
# Verifying that logodds of S = high equals B6 (effect of scouting when S=high)
log(59*8/(2*196))
# B1 = (S=low, B=nonscout)
cs[1]
# B1 + B2 = (S=low, B=scout)
cs[1] + cs[2]
# B3 = (S=med, B=nonscout)
cs[3]
# B3 + B4 = (S=med, B=scout)
cs[3] + cs[4]
# B5 = (S=high, B=nonscout)
cs[5]
# B5 + B6 (S=high, B=scout)
cs[5] + cs[6]

ResidualDeviance(saturated.model.neat) # close to zero because this is the saturated model
NullDeviance(saturated.model.neat)
anova(saturated.model.neat)
anova(glm(count ~ 1, family=binomial(logit)), saturated.model.neat)

