setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/Worksheets")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')


options(show.signif.stars = FALSE)
library(ggplot2)
library(MASS)
library(lattice)

data(cats)

# Heart and body weights of adult male and female cats
head(cats)
colnames(cats) <- c("Gender", "BodyWeight", "HeartWeight")

# Setting up the xyplot
#trellis.device()
xyp <- xyplot(HeartWeight ~ BodyWeight | Gender, data=cats , 
              panel=function(x, y){
                    panel.lmline(x, y); 
                    panel.xyplot(x, y)
                    })
print(xyp)

# part d) ---------------
# INTERPRET: 
### slopes: As body weight increases so does heart weight for both males and females.
# heart weight seems to increase more rapidly for males as body weight grows. This
# suggestss and interaction model. Need to determine via the interaction model
# if this observed difference in slopes is statistically significant. 

### intercepts: heart weights are similar for males and females for low levels of body weight
# but the heart weight are lower for females than males when body weight is highest. 



# part e) --------------------------------------------------------------------------------
# FIT AN INTERACTION MODEL WITH DESIGN MATRIX

options(digits=3, show.signif.stars = F)

cats.interact.lm <- lm(HeartWeight ~ Gender * BodyWeight, data=cats, x=TRUE)

anova(cats.interact.lm)
# INTERPRET: 
# Gender:BodyWeight: F = 4.01, p-value = 0.047 so interaction term is significant. Therefore,
# the rate of change in heart weight (response) as body weight increases is NOT the same
# for male and female cats.

summary(cats.interact.lm)$coef # treatment effects
# INTERPRET: 
### BodyWeight: for a unit increase in body weight, the heart weight of female cats
# increases by 2.64 units, and this is significantly higher than 0. 
### GenderM:BodyWeight: for a unit increase in body weight, the heart weight of male
# cats increases by 1.68 units higher than for females (1.68 + 2.64 = 4.32). 
# This DIFFERENCE is significant since p-value = 0.04


# Confidence intervals
betaCI(cats.interact.lm)
### BodyWeight: we are 95% confident that body weight is between 1.1 and 4.17 for female cats
### GenderM:BodyWeight: the difference in heart weight is between 0.02, 3.33 units higher
# for males than for females, for a unit increase in body weight. This is significantly
# higher than 0 (significant because the CI is above 0)



# Diagnostic plots to check model assumptions
autoplot(cats.interact.lm)
### qqplot : straight line, so normality satisfied. 
# NOTE: cat #144 has a much higher heart weight  than expected (because it is above
# 2 standard deviations of the heart weights of other cats in the normal qq plot)
# while cat # 140 has much lower heart weight than expected (because it is below
# 2 std devs of heart weights of other cats)

### residuals vs. fitted: no fanning nor curvature so constant variance assumption
# seems satisfied. 


# check for influential points using leverage
lev <- influence.leverageValues(cats.interact.lm)
sum(lev$IsInfluential) # there are 17 influential points
which(lev$IsInfluential) # these observations are influenial

# check for influential points using cooks distance
cooks <- influence.cooksDistances(cats.interact.lm)
names(cooks)
sum(cooks$IsInfluential) # there are no influential points using cooks distance
which.max(cooks$CooksPoints) # observation 144 has cooks distance of 0.186
max(cooks$CooksPoints)


# plot of cooks distance
autoplot(cats.interact.lm, which=c(5,6))

library(lindia)
gg_cooksd(cats.interact.lm)

# INTERPRET: Cooks: observation 144 has cooks distance of 0.186 ~ 0.2 so is not influential
# since it corresponds to only 6% percentile on the F(4, 140) distribution. 
# It is not above the 50% critical value. 



# part f) fit a contrast -------------------------------------------------------------

catsData.contr <- cats
contrasts(catsData.contr$Gender) <- c(0.5, -0.5)

cats.interact.contrast.lm <- lm(HeartWeight ~ Gender*BodyWeight,
                                data=catsData.contr, x = TRUE)

# Comparing treatment coeffs
summary(cats.interact.contrast.lm)$coef # coefs are treatment effects
summary(cats.interact.lm)$coef

# COMPARING: 
### BodyWeight: for females, for a one unit increase in body weight, the heart weight
# increases by 3.475 units in the contrasts model, and by 2.64 in the non-contrasts model

### Gender1:BodyWeight: the females have a higher increase in heart weight than males now, 
# for a one unit increase in body weight (because coeff is  negative)


# part g) equations of lines ------------------------------------------------------

### NON-CONTRASTS MODEL: ----
contrasts(cats$Gender)

# E(Y) = 2.98 - 4.17 * GenderM + 2.64 * BodyWeight + 1.68 * GenderM*BodyWeight

# Females line (gender = 0): 
# E(Y) = 2.98 + 2.64 * BodyWeight

# males line (gender = 1): 
# E(Y) = 2.98 - 4.17 + 2.64*BodyWeight + 1.68*BodyWeight
#      = -.19 + 4.32 * BodyWeight


### CONTRASTS MODEL: ----
contrasts(catsData.contr$Gender)

# E(Y) = 0.899 + 4.165 * Gender1 + 3.475 * BodyWeight - 1.676 * Gender1 * BodyWeight

# females line (gender = 0.5):
# E(Y) = 0.899 + 4.165 * (0.5) + 3.475 * BodyWeight - 1.676 * (0.5) * BodyWeight
#      = 2.98 + 2.64 * BodyWeight

# males line (gender = 0.5):
# E(Y) = 0.899 + 4.165 * (-0.5) + 3.475 * BodyWeight - 1.676 * (-0.5) * BodyWeight
#      = -1.18 + 4.31 * BodyWeight

# The equations of the lines are the same for both contrasts and non-contrasts models