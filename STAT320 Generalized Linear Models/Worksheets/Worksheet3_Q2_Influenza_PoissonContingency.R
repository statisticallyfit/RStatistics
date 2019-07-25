setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')

library(ggplot2)
library(ggfortify)
options(show.signif.stars = FALSE)

count <- c(25, 8, 5, 6, 18, 11)
group <- c(rep("P", 3), rep("V", 3))
response <- c(rep(c("S", "M", "L"), 2))

# create a data frame
virusData <- cbind.data.frame(count, group, response)
virusData


## RESEARCH QUESTION: does the response level of an antibody in the blood show
# association / dependence with the type of vaccine? (placebo/actual)


# part a) -----------------------------------------------------------------------------

# MODEL OF INDEPENDENCE: Fit model without interaction term
virus.indep.glm <- glm(count ~ group + response, data=virusData, family=poisson)
# SATURATED MODEL: Fit all the interaction terms
virus.saturated.glm <- glm(count ~ group * response, data=virusData, family=poisson)

anova(virus.indep.glm, virus.saturated.glm, test="Chisq")
NestedLikelihoodRatioTest(virus.indep.glm, virus.saturated.glm, printNice=F)
# CONCLUSION: 
# deviance between saturated and independence model is 18.64 with p-value=0.000089
# so reject H0: independence model is true. Conclude there is significant 
# interaction effect ==> response and group are associated. 

# INADEQUACIES OF INDEPENDENCE MODEL:  
# (1) The model is inadquate because residual deviance >> residual df so there
# is sign of overdispersion. Must use quasipoisson. 
# (2) the model is also inadquate because the residual deviance is significant: 
# this means the model is not a good fit. 
ResidualDevianceTest(virus.indep.glm)


#### NOTE: **** page 463 John Fox: deviance statistic gained from 
#### nestedLikTes(indepModel, saturatedModel) is same testing 
#### residualdeviance(indepModel)


# For these two reasons above, the model is thus not a good fit. 
# Poissible reason - need an interaction term. 


# part b) -------------------------------------------------------------------------
# Analyze the saturated model

anova(virus.saturated.glm, test="Chisq")
# CONCLUSION: the LAST line is the nested test: 
NestedLikelihoodRatioTest(virus.indep.glm, virus.saturated.glm, printNice=F)
# .. because the second-last line in the anova command represents the main effects model
# and the last line represents the saturated model. So the last line's deviance
# is testing if interactions are significant. 
# Deviance p-value is 0.0000895 ==> reject H0 of independenc emodel (main effects
# model) ===> there is association between response and group. 


# part c) ---------------------------------------------------------------------------

#Expected counts under null hypothesis: H0: independence for each cell in the 
# two-way table. 
# Expected: 
pred.indep <- predict(virus.indep.glm, type="response")
expVsObs = cbind(Expected=pred.indep, Observed=virusData$count, virusData[,2:3])

# INTERPRET: Response=small was observed more times for the observations than 
# Expected. Response=small for vaccine group was observed more times for observations
# than for Expected group. 
# Interaction term is significant: conclude there is significant difference in 
# RESPONSE RATES between the vaccine and placebo groups. 

# part d) ---------------------------------------------------------------------------

# Compute relevant proportions: 
virusData


virusMatrix = matrix(c(25,8,5,6,18,11),nrow=2,ncol=3, 
           dimnames=list(Group=c("Placebo", "Vaccine"), Response=c("Small", "Moderate", "Large")))
#virusTbl <- marginalTable(table(virusMatrix)); virusTbl

virusMatrix
Group <- c(rep("P", 25+5+18), rep("V", 8+6+11))
Response <- c(rep("S", 25), rep("M", 5), rep("L", 18), rep("S",8),rep("M",6),rep("L",11))
virusTable <- table(Group,Response)
virusTable

rowProbabilityHat(virusTable)
colProbabilityHat(virusTable)

marginalTable(virusTable)

# P(Response = small | group = placebo) =
prob.Small.given.Placebo <- 25 /48; prob.Small.given.Placebo
# P(Response = small | group = vaccine) = 
prob.Small.given.Vaccine <- 8/25; prob.Small.given.Vaccine
# INTERPRET: So a much greater proportion of placebo patients than vaccine patients 
# recorded a small response. 
# This means there are fewer incidents of diseases fater the vaccine. 
# So vaccine is effective. 



prob.Large.given.Placebo <- 18/48; prob.Large.given.Placebo
prob.Large.given.Vaccine <- 11/25; prob.Large.given.Vaccine
# INTERPRET: a greater proportion of vaccine patients than placebo patients
# recorded large levls of the antibody HIA. 

# So again, by both sets of proportions, the vaccine was more effective in 
# producing the antibody. 
rowProbabilityHat(virusTable)

# Comparing: 
expVsObs
# here are more SMALL responses given placebo patients, and more LARGE responses
# of antibody given vaccine patients, exactly what the proportions say. 


# PROCEDURE CONCLUSION: 
#### (1) use the anova to determine if there is a significant interaction effect. 
#### (2) use exp/obs table OR proportions to infer the direction of this interaction.