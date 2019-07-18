setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')

library(ggplot2)
library(ggfortify)
options(show.signif.stars = FALSE)


## Example 6.1 - Faults Data, model with Poisson glm --------------------------------------

fabricData <- read.table("data/fault.txt", header=TRUE)

# Response: number of faults, which occur at random at a fixed rate per meter
# (use poisson count distribution)

# Plotting the length of a roll vs number of faults
ggplot(data=fabricData, aes(x=faults)) + 
     geom_density(size=1, alpha=0.3, fill="pink")

# Scatterp;ot of faults vs length of roll
ggplot(data=fabricData, aes(x=length, y=faults)) + geom_point(colour="dodgerblue")



## Fitting the model
fault.glm <- glm(faults ~ length, family=poisson, data=fabricData)
null.glm <- glm(faults ~ 1, family=poisson, data=fabricData)


#### DEVIANCE CALCULATION  ###############################################

# Same things: nested likelihood ratio / anova / deviance tests are the same:
anova(fault.glm, test="Chisq")
NestedLikelihoodRatioTest(null.glm, fault.glm)
DevianceTest(fault.glm) # same as the anova of fault.glm, above

# is same as ...
null.glm$deviance - fault.glm$deviance
##########################################################################


#### RESIDUAL DEVIANCE CALCULATION #######################################
ResidualDevianceTest(fault.glm)
anova(null.glm, fault.glm, test="Chisq")
# is same as ...
fault.glm$deviance
##########################################################################

#### DF CALCULATION ######################################################
n <- nrow(fabricData); n
k <- length(fault.glm$coefficients) - 1; k 

df.null <- n - 1; df.null
df.residual <- n - k - 1; df.residual 
##########################################################################


# Summary of the model
summary(fault.glm)

# Sign of overdispersion: the residual deviance 61.75 is much larger than
# residual df, 30. 

# OVERDISPERSION: inflated residual deviance (when residual deviance G >> residual df)
# If the Poisson model fits the data reasonably, we would expect the
# residual deviance to be roughly equal to the residual degrees of freedom.
# That the residual deviance is so large suggests that the conditional variance 
# of the expected number of faults exceeds the variance  of a 
# Poisson-distributed variable, for which the variance equals the mean. 
# This common occurrence in the analysis of count data is termed overdispersion.

# ISSUES: if you let overdispersion occur, your coefficient estimates will be ok
# but the inference (CI's, p-values) will not be reliably. 

# REASONS: 
# 1. research did not include all the necessary predictors
# 2. the response Y may not be poisson, where E(Y) = Var(Y)
# 3. the observations are not independent. 



# # --------------------------------------------------------------------------------
# METHOD 1 to deal with overdispersion: Negative binomial model
# If Y ~ NegativeBinomially distributed, then Var(Y) = phi * E(Y), where
# phi is a constant, called "dispersion" parameter, and phi > 1 (can be estimated from data)
 #When phi > 1, then conditional variation of Y, var(Y_i|n_i), increases more
# rapdily than its mean, mu_i. 




# ----------------------------------------------------------------------------------
# METHOD 2 to deal with overdispersion: quasi-likelihood model 
# BENEFIT: if overdispersion occurred from wrongly assuming Y is poisson, then
# using a quasi-poisson can deal with overdispersion in the best way. 

# Quasipoisson alows greater variability in the data than would be expected
# from a simple poisson. Allows clustering around some Y values. 

# CHANGES TO THE QUASI MODEL: let phi-bar be fit from the data. Then the standard errors
# of the coefficients in the quasi-poisson model are sqrt(phi-bar) times those for 
# the original poisson model. Thus the quasi poisson stderrors are larger => F-tests
# for the terms in the model give smaller F-statistics => larger p-values. 

# FORMULA FOR DISPERSION PARAMETER, estimated from data: 
# phi-bar ~ residualdeviance / (n - k - 1)
res = ResidualDevianceTest(fault.glm)

dispersionEstimate <- res$LikRatio / res$df # (approximate)
dispersionEstimate


# Fitting the quasi poisson model: GLM with quasi poisson distribution
fault.quasi.glm <- glm(faults ~ length, family=quasipoisson, data=fabricData)
anova(fault.quasi.glm, test="Chisq")
# Deviance (difference in residual deviances) = 41.95
# Residual deviance of this model = 61.758
anova(fault.glm, test="Chisq")
# same as the old model

# Only the standard errors are different, coeffs are the same: 
summary(fault.quasi.glm)

stdErrs.df <- data.frame(cbind(OriginalStdErr=summary(fault.glm)$coef[,2], 
      QuasiStdErr=summary(fault.quasi.glm)$coef[,2]))
stdErrs.df

# Relation between old and new standard errors: 
stdErrs.df$OriginalStdErr[1] * sqrt(dispersionEstimate)
stdErrs.df$OriginalStdErr[2] * sqrt(dispersionEstimate)




### DEVIANCE RESIDUALS 
DevianceResiduals.Poisson(fault.glm)
# is same as
residuals(fault.glm)
fault.glm$residuals # pearson residuals, not same as deviance residuals

# Plotting the deviance residuals: the discrepancy between observed and predicted
# values. They are approximately normal for a well-fitting modl. 
residualFitPlot(fault.glm)

# All the diagnostic plots:
autoplot(fault.glm, which=c(1,2,3,4))
# Deviance residuals: are approximately normally distributed, as they should be
# for a wel--fitting model. 
# No pattern in residuals vs fits plot, so no heteroskedasticity
# Thus: di ~ N(0, sigma^2) is satisfied. 

# Influence measures
cooks.df <- influence.cooksDistances(fault.glm)
cooks.df
cooks.df[13,]
cooks.distance(fault.glm)






# SECTION 6.3: Contingency Table (2x2) -------------------------------------------

