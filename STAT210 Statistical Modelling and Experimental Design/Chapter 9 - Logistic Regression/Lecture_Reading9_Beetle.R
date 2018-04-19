setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 9 - Logistic Regression/")


beetleData <- read.table("beetle.txt", header=TRUE)
beetleData <- setNames(beetleData, nm=c("Log10Dose", "Sample", "NumDead"))
beetleData # num dead is as response to exposure of various dosages of carbon disulphide. 

# Plot of proportion killed against logdose
# the logistic curve. 
ggplot(beetleData, aes(x=Log10Dose, y=NumDead/Sample)) + 
      geom_point(shape=19, size=3) 


# Model fitting

# NOTE: MUST use weights=sample since we need to account for differing
# sample sizes by giving more weight to larger samples. 
attach(beetleData)
prop <- NumDead/Sample
beetle.glm <- glm(prop ~ Log10Dose, family="binomial", weights=Sample,
                  data=beetleData)
summary(beetle.glm)
anova(beetle.glm, test="Chisq")


# But if you have data stored as cols of successes and failures rather than 
# sample size and successes, then the weights argument is NOT required. 
count <- cbind(NumDead, Sample - NumDead)
count
beetle2.glm <- glm(count ~ Log10Dose, family=binomial, data=beetleData)
beetle2.glm
beetle.glm # they are the same. 

beetle.null.glm <- glm(count ~ 1, family=binomial,data=beetleData)

# Here we test ldose model against the null model and get residual deviance
# of 272.97, which has chi-square dist with 1 df (tests B1 = 0)
# INTERPRET: the 272.9 (deviance) tests the hypothesis that B1 = 0. T
# The p-value is in the column
# and it equals < 2.22e-16 = 0 so the ldose term is very significantly different from 0.
# But the p-value for 11.23 (residual deviance, or model fit) is not shown

# NULL model means: count ~ 1
# ldose df = 1, and deviance is 272, p-value is testing H0: B_ldose = 0
anova(beetle2.glm, test="Chisq")
ResidualDevianceTest(beetle2.glm) # the global fit for the model
# So fitting the single predictor ldose accounts for the majority of 
# sample variation in proportion of dead insects. 


# model goodness of fit, once we fit with ldose - NO PVALUE IN ANOVA (res dev = 11.23)
anova(beetle.null.glm, beetle2.glm, test="Chisq")
DevianceTest(beetle2.glm) # the nested lik-ratio test with null model. 
# so ldose is a useful predictor




## Getting back-transformed fitted vals in original scale
# (say type = response)
# if you want it in log-odds form say type="link"

#dev.resids <- summary(beetle2.glm)$deviance.resid # all the deviance residuals
newd <- data.frame(Log10Dose=seq(min(Log10Dose), max(Log10Dose), by = 0.001))
preds <- predict.glm(beetle2.glm, newdata=newd, se.fit=T, type="response") #scale proportions

pred.df <- data.frame(fit.back=preds$fit, 
                      lwr.back= preds$fit - 1.96*preds$se.fit,
                      upr.back = preds$fit + 1.96 * preds$se.fit,
                      Log10Dose=newd)
head(pred.df); nrow(pred.df); nrow(beetleData)

ggplot(data=beetleData, aes(x=Log10Dose, y=NumDead/Sample)) + geom_point(shape=19, size=3) + 
      geom_line(data=pred.df, aes(y=fit.back), col="blue", size=1) + 
      geom_line(data=pred.df, aes(y=lwr.back), col="red", size=1, linetype="dashed") + 
      geom_line(data=pred.df, aes(y=upr.back), col="red", size=1, linetype="dashed") 



# predicting with ldose = 1.69 - this is the proportion of insects that will
# die with dose 1.69
p = predict(beetle2.glm, type="response", newdata=data.frame(Log10Dose=1.69))
# sameple = 59 when dose = 1.6907
expected = p * 59; expected # eexpected number of insects about 4 insects. 


#count.fit <- preds$fit * beetleData$Sample # multiply each probability by its sample size

#df <- data.frame(n=beetleData$Sample, NumDead=beetleData$NumDead, ExpectedFit=count.fit,
#           DevianceResiduals=dev.resids)
#df



# Plotting the normal qq plot of deviance residuals
plot(beetle2.glm, which=2) # skewed
shapiro.test(dev.resids) # not significant due to small sample size


