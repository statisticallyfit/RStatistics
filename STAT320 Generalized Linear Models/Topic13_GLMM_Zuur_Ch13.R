setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
#library(nlme)
library(lme4) # for glmer() for fitting GLMM model
#detach(package:nlme)
library(lattice)
library(MASS) # for glmmPQL
library(glmmML) # for glmmML

options(show.signif.stars = FALSE)


### DEER DATA --------------------------------------------------------------------------

# Data: 
# Response = presence absense of parasite in deer
# Y_it = 1 if parasite is found in animal (j) at farm(i) at time (t)
# Y_it = 0 if parasite is NOT found in animal (j) at farm (i) at time (t)
# Y_it ` Binomial(1, p_it)  (bernoulli)
# --> p_ijt = probability that deer (j) on farm (i) has parasite at time (t)
# --> gender_ijt = gender of deer (j) on farm (i) at time (t)
# --> length_ijt = length of deer (j) on farm (i) at time (t)

# Predictors: 
# length of the host
# gender of the host. 


data("DeerEcervi")
deerData <- DeerEcervi

head(deerData)
deerData$EcerviPresence <- deerData$Ecervi
deerData$EcerviPresence[deerData$Ecervi > 0] <- 1
deerData$Gender <- factor(deerData$Sex)
deerData$Farm <- factor(deerData$Farm)
# note: centering length. Otherwise, intercept represents probability that a deer of 
# length 0 has the parasite ==> nonsense since no deer has length 0. 
# Centering: gives intercept the interpretation of probability that an animal of average
# length has the parasite. 
deerData$Length.centred <- deerData$Length - mean(deerData$Length)


# GLM for deer data: --------------------------------------------------------------------

deer.glm <- glm(EcerviPresence ~ Length.centred * Gender + Farm, data=deerData, family=binomial)

anova(deer.glm, test="Chisq")

# Perform single-term deletions and apply likelihood ratio test.
# drop1 compares deviance of the specified model with those of nested models. 
drop1(deer.glm, test="Chi") # interaction is significant so keeping it. 
# INTERPRET: 
### first line: shows deviance for the model with no term dropped. AIC is 799, smallest. 
### second line: shows deviance for model with Farm term dropped, highest AIC and deviance
# Change in deviance is LRT = 258.22, and is chi-square distributed with 23 df and
# has p-value < 0.001 so the Farm term is highly significant. 
### third line: model with Len * Gender dropped. 
# change in deviance from dropping interaction term is LRT = 9.98 with p-value = 0.00157
# so also significant. 

summary(deer.glm)
# PROBLEM with glm: independences: the data were obtained from 24 farms, so we are sampling
# deer that may have been in contact, so presence/absence of parasites on deer from
# the same farm cannot be assumed independent .



# Visualizing predicted values: for females only and for all farms, and for a range of 
# length values. 
plot(deerData$Length.centred, deerData$EcerviPresence, xlab="Length Centered",
     ylab="Probability of Presence of E. cervi L1", main = "Female data")
# order() function gives the indices which you should use to sort the given vector
# in increasing order. 
iLen <- order(deerData$Length.centred)
allFarms <- unique(deerData$Farm)

for (j in 1:length(allFarms)){
      df <- data.frame(Length.centred = deerData$Length.centred, Gender = "1", Farm=allFarms[j])
      n <- dim(df)[1]
      
      if(n > 10){
            pred <- predict(deer.glm, df, type="response")
            lines(df$Length.centred[iLen], pred[iLen])
      }
}

# GLMM for Deer data: -------------------------------------------------------------------

# IDEA OF GLMM: the glm version of a mixed model. 
# Can use a random intercept variable when that variable isa  random sample from a populatio
# and there is lots of within-subject variation within that variation (like variation within
# a block variation). 

# HERE: block variable is farm. Lots of indiivdual deer variation within a particular
# sampled farm. 
# If variance component of farm is large, then each farm will have different intercepts. 
# If not, each farm's intercepts are close to each other, so not much need for
# random intercepts model. 

# Size of variance of the random effect suggests whether or not it is needed. 
# Using Random effect instead of fixed effect reduces cost of estimating parameters. 


## IDea here: grouped data. Each  plant (random effect) consists of a group of 
# repeated measures. 

#There are two levels of variability:
# (1) measurement error on each individual within a group. (random intercepts / slopes) 
# (2) variability amongst groups. (fixed effect interactions)


# Fitting the GLMM
# Fixed effects: length, gender
# Random intercept: Farm
# Error dist: binomial
deer.intercepts.glmer <- glmer(EcerviPresence ~ Length.centred * Gender + (1|Farm), 
                   family=binomial, data=deerData)
summary(deer.intercepts.glmer)
anova(deer.intercepts.glmer)

# Gives probability of presence of parasite as function of interaction of len*Gender
deer.intercepts.glmmPQL <- glmmPQL(EcerviPresence ~ Length.centred * Gender, random = ~1|Farm, 
                   family=binomial, data=deerData)
# anova(deer.intercepts.glmmPQL) # NA for pql fits
summary(deer.intercepts.glmmPQL)
# INTERPRET: 
# sigma.intercept = 1.462
# sigma.residual = 0.962
## ==> intercept variation is larger than residual variation so suggests the random
# intercept was needed. 

# Female line: logit(p_ij) = 0.888 + 0.037* len.c + farm_i,   farm_i ~ N(0, 1.462^2)
# Male line: logit(p_ij) = 1.498 + 0.072 * len.c + farm_i,    farm_i ~ N(0, 1.462^2)

# Checking significance of variance components
intervals(deer.intercepts.glmmPQL) # from nlme

# INTERPRET: sigma.intercept is significantly higher than residual variation. 


# Plotting: female random intercepts model line with 95% confidence bands, # determined
# by the random intercept. 
# The middle line is the estimated probabilities for range of length values for 
# females, for a typical farm (farm_i = 0)
# meaning: go to typical farm and sample a deer of average length = 0. Then it has
# probability 0.7 of having the parasite. 
# But there is considerable between-farm variation since thsi depends on which 
# particular farm we visit. 
femaleLine.fixed <- 0.888 + 0.037* deerData$Length.centred
p.averageFarm <- exp(femaleLine.fixed) / (1 + exp(femaleLine.fixed))
iLen <- order(deerData$Length.centred) # to avoid spaghetti plot

#plot(deerData$Length.centred, deerData$EcerviPresence, ylab="Probability of presence of E.cervi",xlab="Length")
#lines(deerData$Length.centred[iLen], p.averageFarm[iLen], lwd=3)
sigmas.ci <- data.frame(lower=c(ints$reStruct$Farm$lower, ints$sigma[1]), 
                        est = c(ints$reStruct$Farm$est., ints$sigma[2]), 
                        upper = c(ints$reStruct$Farm$upper, ints$sigma[3]))
rownames(sigmas.ci) <- c("sigma.intercept", "sigma.residual")
sigmas.ci
p.upper <- exp(femaleLine.fixed + sigmas.ci$upper[1]) / (1 + exp(femaleLine.fixed + sigmas.ci$upper[1]))
#p.lower <- exp(femaleLine.fixed - 1.96*1.462) / (1 + exp(femaleLine.fixed - 1.96*1.462))
p.lower <- exp(femaleLine.fixed - sigmas.ci$lower[1]) / (1 + exp(femaleLine.fixed - sigmas.ci$lower[1]))
#lines(deerData$Length.centred[iLen], p.lower[iLen])
#lines(deerData$Length.centred[iLen], p.upper[iLen])

df <- NULL
df <- data.frame(Data=deerData$EcerviPresence,
                  LenCOrd = deerData$Length.centred[iLen], 
                 ProbLowerOrd = p.lower[iLen], 
                 ProbUpperOrd = p.upper[iLen], 
                 ProbPredOrd = p.averageFarm[iLen],
                 LenC = deerData$Length.centred,
                 ProbLower = p.lower, 
                 ProbUpper = p.upper, 
                 ProbPred = p.averageFarm) # 

ggplot(df, aes(x=LenC, y=Data)) +  geom_point(size=1, shape=19) + 
      geom_line(data=df, aes(y=ProbPred, colour="red"),size=1) +
      geom_line(data=df, aes(y=ProbLower),colour="dodgerblue", linetype="dashed",size=1) + 
      geom_line(data=df, aes(y=ProbUpper), colour="dodgerblue", linetype="dashed",size=1) 






#### OWL DATA ------------------------------------------------------------------------

# Data: 
### Response = sibling negotiation (number of calls made by all offspring in absense of
# parents counted during 30-second time intervals before arrival of parent
# divided by number of siblings)
# So response: 
# Y_it = NCalls_it at time (t) in nest (i)
# NCalls_it ~ Pois(mu_it)
### Predictors: 
# Gender of parent = male or female
# Food treatment = deprived or satiated
# Arrival time of parent to nest: 21 to 30 (9:00 pm to 6:00 am)
# offset: number of nestlings

data("Owls")
head(Owls)
owlData <- Owls
cs = colnames(owlData)
cs[5] <- "NCalls"
colnames(owlData) <- cs
owlData$logBroodSize <- log(owlData$BroodSize)
owlData$Nest <- factor(owlData$Nest)

# GOAL OF STUDY: number of calls of nestlings affected by satiation, arrival time?



# GLM for owl data: --------------------------------------------------------------------

with(owlData, interaction.plot(x.factor=FoodTreatment, trace.factor=SexParent, 
                               response=NCalls))
interactionPlot(x.factor="FoodTreatment", trace.factor="SexParent", 
                response="NCalls", data=owlData)
#interactionPlot(x.factor="ArrivalTime", trace.factor="SexParent", 
#                 response="NCalls", data=owlData)

# Fitting the glm owl model: 
formOwl <- formula(NCalls ~ offset(logBroodSize) + SexParent*FoodTreatment + 
                         SexParent*ArrivalTime)

owl.glm <- glm(formOwl, family=poisson, data=owlData)

anova(owl.glm, test="Chisq") # nonsignificant interaction
summary(owl.glm) # there is overdispersion since residual deviance 3641 >> residual df = 593


owl.quasi.glm <- glm(formOwl, family=quasipoisson, data=owlData)
summary(owl.quasi.glm)

# Checking interactions: the two-way interactions are not significant: 
drop1(owl.quasi.glm, test="F")

# Fitting the non-interaction model: 
owl.quasi.nointeract.glm <- glm(NCalls ~ offset(logBroodSize) + FoodTreatment + 
                                      ArrivalTime, family=quasipoisson, data=owlData)

# Comparing interaction effects: not significant. 
anova(owl.quasi.nointeract.glm, owl.quasi.glm, test="Chisq")

anova(owl.quasi.nointeract.glm, test="Chisq") # significant main effects
summary(owl.quasi.nointeract.glm)



### GLMM for Owl Data ------------------------------------------------------------------

# Random intercepts: Nest
# Fixed effects: offset + Gender*treat
owl.glmmPQL <- glmmPQL(NCalls ~ offset(logBroodSize) + SexParent*FoodTreatment + 
                             SexParent*ArrivalTime , random=~1|Nest,
                       family=poisson, data=owlData)
#anova(owl.glmmPQL)
owl.glmmML <- glmmML(NCalls ~ offset(logBroodSize) + SexParent*FoodTreatment + 
                           SexParent*ArrivalTime ,
                     cluster = Nest, family=poisson, data=owlData)
summary(owl.glmmML) # nonsignificant gender * arrivaltime
# anova(owl.glmmML) # NA

# Dropping the interaction term: gender * arrival


# Need to rescale variables so this converges
#owlData.scaled <- owlData
#owlData.scaled$NCalls <- scale(owlData$NCalls)
#owlData.scaled$logBroodSize <- scale(owlData$logBroodSize)
#owlData.scaled$ArrivalTime <- scale(owlData$ArrivalTime)

#owl.glmer <- glmer(NCalls ~ offset(logBroodSize) + SexParent*FoodTreatment + 
#                         SexParent*ArrivalTime + (1|Nest),
#                   family=poisson, data=owlData.scaled)
#                   control=glmerControl(optimizer="bobyqa",check.conv.grad=.makeCC("warning",1e-3)))

