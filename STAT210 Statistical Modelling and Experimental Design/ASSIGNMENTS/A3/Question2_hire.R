setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A3")

library(ggplot2)
options(digits=10, show.signif.stars = FALSE)


# part a)

#load the data
discrData <- read.table("discrim.txt", header=TRUE)

# stepwise regression
formLower <- formula(~1) # the minimal model
formUpper <- formula(~ EDUC + EXP + GENDER, data=discrData)
start.model <- glm(HIRE ~ 1, data=discrData, family=binomial)
full.model <- glm(HIRE ~ EDUC + EXP + GENDER, data=discrData, family=binomial)

step.forward.model <- step(start.model, direction="forward",
                           scope=list(lower=formLower, upper=formUpper))
step.backward.model <- step(full.model, direction="backward",
                            scope=list(lower=formLower, upper=formUpper))

# We choose the 3-predictor model since it is the result of both methods. 
# The 3-predictor model has lowest AIC. 

hire.glm <- step.forward.model

# part b) checking residual deviance
anova(hire.glm, test="Chisq")
# residual deviance is 14.735, df = 24, with p-value = 
1 - pchisq(14.735, df=24)
# which is greater than 0.05 so the model is a good fit. 

############## ADD about DEVIANCE here############################
ResidualDevianceTest(hire.glm)
DevianceTest(hire.glm)
####################################################################################


# part c) equation for final model
coef(hire.glm)



# part d) probability of being hired

# probability of being hired for males (GENDER=1), EDUC=6, EXP=8
predict(hire.glm, newdata=data.frame(GENDER=1, EDUC=6,EXP=8), type="response")
# is 0.996 (very likely)

# probability  of being hired for females
predict(hire.glm, newdata=data.frame(GENDER=0, EDUC=6,EXP=8), type="response")
# is 0.4899, so much lower than for males with this much education and experience.


