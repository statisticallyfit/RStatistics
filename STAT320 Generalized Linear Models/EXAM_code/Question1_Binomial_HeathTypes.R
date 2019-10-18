setwd("C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/stat320_rcode")
source('C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/FORMULAS.R')
source('C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/PLOTTING.R')
source('C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/Rfunctions.R')


library(ggfortify)
# library(dae)

options(show.signif.stars = FALSE)


heathData <- read.table("data/Heath.txt", header=TRUE)
head(heathData)


banksiaData <- heathData[,c(1,2,3,8,9)]
head(banksiaData)

banksiaData$Heath # 3 types of heath
banksiaData$Seed # 3 types of seed treatments (dry, wet, or none)
banksiaData$Enclosure # 3 types of enclosures to keep out animals: none, open, closed
banksiaData$Banksia2R # number of seeds survived 
banksiaData$Banksia2N # total seeds planted

nrow(banksiaData)
# 18 observations

# NOTE: heath NESTED in seed NESTED in Enclosure

# part a) -----------------------------------------------------------------------------


# Exploratory plots (random effects) ---------------------------------------------------

# Heath WITHIN seed
ggplot(banksiaData, aes(x=Heath, y=Banksia2R/Banksia2N, color=Heath)) + 
  geom_boxplot(size=1) + facet_grid(. ~ Seed)
# INTERPRET: within the (wet) Seed level, there is noticeable variation in Heath level
# and similar variability in dry/none seed levels since in the wet seed level, the
# heath boxplots are well separated.

## RESIDUAL VARIANCE: (variation among replicates): since the boxplots for each Heath 
# within Seed are narrow for Seed (dry,none), this means there is little variation in 
# banksia survivor rate among replicates.  Means the replicate measures for each Heath
# are similar for seed (dry, none). 
# But boxplots are wider for Seed = wet, so there is higher variation in banksia survivor
# rate among replicates for the Seed = wet level (high within-heath variability for seed= wet)




# Seed within Enclosure
ggplot(banksiaData, aes(x=Seed, y=Banksia2R/Banksia2N, color=Seed)) + 
  geom_boxplot(size=1) +   facet_grid(. ~ Enclosure)

# INTERPRET: 
# (1) For all Enclosure levels, there is high variability in the Seed = wet level for 
# all enclosure levels compared to see d= dry/wet since the rates for seed = wet are
# higher for all enclosures than are the rates for see d= none/dry

# (within replicates for Seed)
# Boxplots are wider for Seed = wet for all enclosure levels so higher within-seed
# variation for Seed = wet than for the seed = dry,none.


# Heath within Enclosure
ggplot(banksiaData, aes(x=Heath, y=Banksia2R/Banksia2N, color=Heath)) + 
  geom_boxplot(size=1) + facet_grid(. ~ Enclosure)

# INTERPRET: 
# (1) Not too much variability among heath levels since boxplots are not well-separated. 
# (2) Higher within-heath variation for all enclosure levels since boxplots are
# wide. 


 #Exploratory plots (Fixed effects) ---------------------------------------------------

# Interaction between Seed and Heath : interaction between Heath and Seed may no tbe the
# same for all levels of Enclosure (middle plot differs from the rest) but weakly. 
interaction.ABC.plot(response=Banksia2R, x.factor=Heath, 
                     trace.factor=Enclosure, groups.factor=Seed, data=banksiaData)

# All two-way interactions

# p = 0.089
interactionPlot(x.factor="Heath", trace.factor = "Seed", response="Banksia2R", data=banksiaData)
# Steeper slope when seed is wet than for Seed = Dry and None, going from Heath dry to wet
# Not clear if this is enough to say there is a significant interaction. 

interactionPlot(x.factor="Heath", trace.factor = "Enclosure", response="Banksia2R", data=banksiaData)
# Similar slopes of heath for all Enclosure levels

# p = 0.067
interactionPlot(x.factor="Seed", trace.factor = "Enclosure", response="Banksia2R", data=banksiaData)
# INTERPRET: similar slopes for all enclosure levels across seed levels



# Testing crossed interaction model (factorial)

#banksia.randSaturated.glmer <- glmer(banksiaCount ~ 1 + + (1|Enclosure/Seed) + (1|Enclosure/Heath) + (1|Seed/Heath), 
#                                  family=binomial, weights=Banksia2N, data=banksiaData, 
#                                  control=glmerControl(optimizer="bobyqa",check.conv.grad=.makeCC("warning",1e-3) ) )


#temp = glmer(Banksia2R ~ (Enclosure + Seed + Heath)^2 + (1|Heath), 
#      family=poisson, data=banksiaData, 
#      control=glmerControl(optimizer="bobyqa",check.conv.grad=.makeCC("warning",1e-3) ) )




# part b) ----------------------------------------------------------------------------


banksiaCount <- cbind(Banksia2Count=banksiaData$Banksia2R, 
                      NoBanksia2Count=banksiaData$Banksia2N - banksiaData$Banksia2R)

# STEP 1: Fit the saturated model

# P-values test the deviance, (predictor significant) so the third-way interaction not necessary at all! p-value ~ 1. 
banksia.saturated.glm <- glm(banksiaCount ~ Enclosure * Seed * Heath, 
                                data=banksiaData, family=binomial)
anova(banksia.saturated.glm, test='Chisq') # three way term not needed


# STEP 2: fit all two way model

banksia.alltwoway.glm <- glm(banksiaCount ~ (Enclosure + Seed + Heath)^2,
                             data=banksiaData, family=binomial)
anova(banksia.alltwoway.glm, banksia.saturated.glm, test='Chisq')

anova(banksia.alltwoway.glm, test='Chisq')
summary(banksia.alltwoway.glm)

# INTERPRET: 
# * Seed: Heath not significant: p-value = 0.0885
# * Enclosure: Heath not significant: p-value = 0.4205

# No twoway interactions significant except the particular level
# of EnclosureOpen:SeedNone with p-value = 0.027

#banksia.alltwoway.rates.glm <- glm(Banksia2R/Banksia2N ~ (Enclosure + Seed + Heath)^2, 
#                                   data=banksiaData, weight=Banksia2N, family=binomial)


# STEP 3: fit model without seed:heath
banksia.noSeedHeath.glm <- glm(banksiaCount ~ Enclosure*Seed + Enclosure*Heath,
                             data=banksiaData, family=binomial)
anova(banksia.noSeedHeath.glm, test='Chisq')
# remove the enclosure: heath term

# STEP 4: remove enclosure:heath
banksia.onlyEnclosureSeed.glm <- glm(banksiaCount ~ Heath + Enclosure*Seed,
                                     data=banksiaData, family=binomial)
anova(banksia.onlyEnclosureSeed.glm, test='Chisq')


# Step 5: remove Enclosure: seed
banksia.maineffects.glm <- glm(banksiaCount ~ Heath + Seed + Enclosure, 
                               data=banksiaData, family=binomial)
#anova(banksia.maineffects.glm, test="Chisq")
anova(banksia.maineffects.glm, banksia.onlyEnclosureSeed.glm, test="Chisq")

# Test all together
anova(banksia.maineffects.glm, 
      banksia.onlyEnclosureSeed.glm, 
      banksia.noSeedHeath.glm, 
      banksia.alltwoway.glm, 
      banksia.saturated.glm,
      test="Chisq")

# p = 0.888 = tests (alltwo vs noseedheath)
# p = 0.42 = tests (noseedheath vs onlyEncSeed)
# p = 0.06657 = tests (onlyEncSeed vs maineffects)

anova(banksia.alltwoway.glm, banksia.saturated.glm, test="Chisq")
anova(banksia.noSeedHeath.glm, banksia.alltwoway.glm, test="Chisq")
anova(banksia.onlyEnclosureSeed.glm, banksia.noSeedHeath.glm, test="Chisq")
anova(banksia.maineffects.glm, banksia.onlyEnclosureSeed.glm, test="Chisq")




# Testing residual deviance of final model
ResidualDevianceTest(banksia.maineffects.glm)
# INTERPRET: p-value = 0.2133, G = 15.53, df = 12, so not enough envidence to conclude
# the residual deviance is not 0 so it is small so expected successes / fails are similar
# So model is a good fit for the data. 




# Residuals plots

autoplot(banksia.maineffects.glm, which=c(1,2,3,4)) # these are the deviance residuals
# Outliers: observations 9, 17, 4, way beyond the -2,2 boundary
# Residuals : no pattern so homoeskedasticity ov error variance. 


# Testing normality of deviance residuals
shapiro.test(residuals(banksia.maineffects.glm)) # some non-normality of error

banksia.cook <- influence.cooksDistances(banksia.maineffects.glm)
banksia.cook
# no influential measures

# part d) ------------------------------------------------------------------------------

# Binomial independence model
summary(banksia.maineffects.glm)

cofs <- summary(banksia.maineffects.glm)$coef[,1]
cofs
cbind(LogOdds = cofs, OddsRatio=exp(cofs), PercentChange=100*(exp(cofs)-1))


 #INTERPRET coefficients: 

#### Heath : base = DRY
#### Seed : base = DRY
### Encosure: base = CLOSED


# ---> B0 = -5.016. 
# Is log odds of survival for seed with CLOSED enclosure, DRY seed, and DRY heath. 
# Odds ratio: is exp(-5.016): odds of this seed surviving over odds not surviving

# ---> HeathWet: p-value = 0, coef = 1.55 > 0 
# Log odds: difference in log odds of Survival between heath wet and dry seeds 
# for all levels of Seed and Enclosure. 
# Odds ratio: odds ratio of survival between Heath = wet and dry seed observation for
# all levels of Seed and Enclosure is exp(1.55)
# Percent change: 100*(exp(1.55) - 1) = 3.74 % so the odds of survival for Heath = wet
# seed vs for heath = dry seed is 3.7 % higher. 

# ---> SeedNone: p-value= 1 (not significant)
# Percent change: odds of survival for Seed = None is ~ 0% higher than for Seed = DRY
# NO noticeable difference. 

# ---> SeedWet: p-value = 0
# Percent change: odds  of survival for Seed = wet is 1059.0218 % higher than for
# Seed = dry. 

# --> EnclosureNone: p-value, percent change = 122.8% higher of survival for Enclosure None 
# than for Enclosure closed. 
# p-value = 2.39-08 (significant)

# ---> EnclosureOpen: 23% higher chance of survival for open than closed enclosure
# p-value = 0.17 (not significant)_


