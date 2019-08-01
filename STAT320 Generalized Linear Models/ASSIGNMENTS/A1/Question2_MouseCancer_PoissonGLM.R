setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggplot2)
options(show.signif.stars = FALSE)


# part a) --------------------------------------------------------------------------------

# Binomial formulation: create the data so that it can be used to fit the binomial glm. 
miceData.binom <- data.frame(Strain=c(rep("X",4), rep("Y",4)), 
                             Gender=rep(c(rep("male",2), rep("female",2)),2), 
                             Exposure= rep(c("exposed", "control"), 4), 
                             TumourCount = c(12, 74, 12, 84, 14, 80, 14, 79),
                             NoTumourCount = c(4,5,2,3,4,10,1,3))

miceData.binom$Total <- miceData.binom$TumourCount + miceData.binom$NoTumourCount
miceData.binom

#aphidData.binomial
#aphidData.poisson
miceData.pois <- data.frame(Strain=c(rep("X",4*2), rep("Y",4*2)), 
           Gender=rep(c(rep("male",2*2), rep("female",2*2)),2), 
           Exposure= rep(c(rep("exposed",2), rep("control",2)), 4), 
           Tumour=rep(c("yes", "no"), 8), 
           Count = c(12, 4, 74, 5, 12, 2,84,3,14,4,80,10,14,1,79,3))
#aphidData.poisson
#aphidData.binomial      

miceData.pois
miceData.binom

tumourCount <- cbind(TumourCount=miceData.binom$TumourCount, NoTumourCount=miceData.binom$NoTumourCount)



# STEP 1: Fit the saturated model
mice.saturated.binom.glm <- glm(tumourCount ~ Strain * Gender * Exposure, 
                                data=miceData.binom, family=binomial)

sat.sge = mice.saturated.binom.glm                     ###
sat.seg = glm(tumourCount ~ Strain * Exposure * Gender, 
              data=miceData.binom, family=binomial)
sat.ges = glm(tumourCount ~  Gender * Exposure * Strain, 
              data=miceData.binom, family=binomial)
sat.gse = glm(tumourCount ~ Gender * Strain * Exposure,  ###
              data=miceData.binom, family=binomial)
sat.egs = glm(tumourCount ~ Exposure * Gender * Strain, 
              data=miceData.binom, family=binomial)
sat.esg = glm(tumourCount ~ Exposure * Strain * Gender, 
    data=miceData.binom, family=binomial)

anova(sat.sge, test="Chisq")
anova(sat.seg, test="Chisq")
anova(sat.ges, test="Chisq") # possible two-term indep model: G + E
anova(sat.gse, test="Chisq")
anova(sat.egs, test="Chisq") # possible two-term indep model: E + G
anova(sat.esg, test="Chisq")




# --------------
anova(mice.saturated.binom.glm, test="Chisq")

# INTERPRET: 

# DEVIANCE: 
# Strain:Gender:Exposure term: the deviance = 0.0058, df=1, p-value = 0.939 ==> 
# ==> means this 3-way interaction term is not a significant predictor of cancer in mice. 
# Must remove it. 
# Other terms: other interaction terms have non-signfiicant p-values for the deviance
# values, so must remove them. They are not significant predictors. 

# RESIDUAL DEVIANCE: 
# Residual deviance for the saturated model = is approximately 0 (from
# in the last line) with df = 0, and p-value ~ 1. So the model is a near perfect
# fit to the data. 
# Residual deviance for independence model = 1.472, p-value = 0.83159 ===> 
# ===> model is a good fit for the data. 



# STEP 2: remove the 3-way interaction term
mice.alltwoway.binom.glm <- glm(tumourCount ~ (Strain + Gender + Exposure)^2, 
                                data=miceData.binom, family=binomial)

anova(mice.alltwoway.binom.glm, test="Chisq")
# INTERPRET: 
# all the two way terms have p-values > 0.05 for their deviance statistics, so they 
# are not significant predictors of number of tumor cases. 


# STEP 3: remove all 2-way interaction terms and the 3-way term: this is the independence
# model. 
mice.indep.binom.glm <- glm(tumourCount ~ Strain + Gender + Exposure , 
                            data=miceData.binom, family=binomial)
anova(mice.indep.binom.glm, test='Chisq')

# The independence model fits best (also stated above, from saturated model anova table)

# Another way: nested likelihood test between indep vs saturated models:
anova(mice.indep.binom.glm, mice.saturated.binom.glm, test="Chisq")
# INTERPRET: deviance = 1.47 with p-value = 0.83 so fail to reject H0 that independence
# model is true ==> idnependence model is more useful than saturated model. 
#NestedLikelihoodRatioTest(mice.indep.binom.glm, mice.saturated.binom.glm)



# part b) -----------------------------------------------------------------------------

# Fit the poisson model to verify the form of binomial
miceData.pois

mice.saturated.pois.glm <- glm(Count ~ Strain * Gender * Exposure * Tumour, 
                               data=miceData.pois, family=poisson)

anova(mice.saturated.pois.glm, test="Chisq") # gender:tumor

anova(mice.saturated.binom.glm, test="Chisq") # gender

# Strain:Tumour (pois) == Strain (binom)
# Gender:Tumour (pois) == Gender (binom)
# Exposure:Tumour (pois) == Exposure (binom)
# Strain:Gender:Tumor (pois) == Strain:Gender (binom)
# Strain:Exposure:Tumor (pois) == Strain.Exposure (binom)
# Gender:Exposure:Tumor (pois) == Gender:Exposure (binom)
# Strain:Gender:Exposure:Tumor (pois) == Strain:Gender:Exposure (binom)

# The poisson model also corresponds to binomial and verifies it sincethe  p-values, 
# deviance, and residual deviances of  the above corresponding terms match.