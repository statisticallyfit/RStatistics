setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A3")

options(digits=10, show.signif.stars = FALSE)

# part a)

# make the data
heartData <- data.frame(CK=c(20,60,100,140,180,220,260, 300,340,380,420,460,500),
                        attack=c(2,13,30,30,21,19,18,13,19,15,7,8,35),
                        total=c(90,39,38,35,21,20,19,14,19,15,7,8,35))
heartData
count <- cbind(attack=heartData$attack, none=heartData$total - heartData$attack)

heart.glm <- glm(count ~ CK, data=heartData, family=binomial)

# part a.i) deviance = 283.15, with
# p-value 2.22e-16 which means the CK slope is different from zero. Here we
# are testing the nested likelihood ratio test of the null model and the CK
# single predictor model. A lot of variation is explained in heart attacks
# by including the CK predictor in the model.
# Primarily, model is NOT good fit since residual deviance = 28.14, df = 11, with
# p-value = 0.003 so that means the residual deviance is large. Large differences
# between observed and expected successes and observed/expected failures.
anova(heart.glm, test="Chisq") 
1 - pchisq(28.140225, df=11)

# part a.ii) summary table
summary(heart.glm)
# all coefficients are significant
cof <- summary(heart.glm)$coef[,1:2]
exp(cof)-1
# for a 1 unit increase in CK, the odds of heart attack increases by 3.57%



# part b)  median dose
intercept = cof[[1]]
slope = cof[[2]]
medianCK = -intercept/slope; medianCK
# So 50% of patients suffer heart attack with CK level = 86.267 units. 