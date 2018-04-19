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
prop <- NumDead/Sample
beetle.glm <- glm(prop ~ Log10Dose, family="binomial", weights=Sample,
                  data=beetleData)
summary(beetle.glm)
anova(beetle.glm, test="Chisq")


# But if you have data stored as cols of successes and failures rather than 
# sample size and successes, then the weights argument is NOT required. 
attach(beetleData)
count <- cbind(NumDead, Sample - NumDead)
count
beetle2.glm <- glm(count ~ Log10Dose, family=binomial, data=beetleData)
beetle2.glm
beetle.glm # they are the same. 


# NOTE: if we use the beetle.glm we get error because of non integer numbers. 
result <- LikelihoodRatioGLMTest(beetle2.glm)


# Null hypothesis is that expected mu and observed ys are the same
# If low p-value then they are not. 
ResidualDevianceTest(beetle2.glm)
NullDevianceTest(beetle2.glm)

# Here we test ldose model against the null model and get residual deviance
# of 272.97, which has chi-square dist with 1 df (tests B1 = 0)
anova(beetle2.glm, test="Chisq")
# significant, means coeff of ldose is different from zero. 


# Same as likelihood nested
anova(beetle2.glm, test="Chisq")

beetle.null.glm <- glm(count ~ 1, family=binomial,data=beetleData)
LikelihoodRatioNestedGLMTest(beetle.null.glm, beetle2.glm)
