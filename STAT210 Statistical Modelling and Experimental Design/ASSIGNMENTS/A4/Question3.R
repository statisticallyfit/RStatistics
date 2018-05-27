setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A4")

library(effects)
options(digits=10, show.signif.stars = FALSE)

# Preparing the data
weightData <- data.frame(Supplement=c(rep("1", 5), rep("2", 5), rep("3", 5)), 
                         Litter=rep(1:5, 3), 
                         WeightGain=c(28.7, 30.6, 27.2, 28.6, 31.9,
                                      30.7, 34.5, 32.6, 34.4, 30.7, 
                                      25.4, 26.3, 27.5, 24.3, 25.8))
# Declaring the variables as factors. 
weightData$Litter <- factor(weightData$Litter)
weightData$Supplement <- factor(weightData$Supplement)

# part b) -------------------------------------------------------------------------- 
# Do the nested F test using the rediuced model which contains just blocks, 
# and the complete model
complete.weight.lm <- lm(WeightGain ~ Litter + Supplement, data=weightData)

# Do the data indicate difference across the dietary supplements? 
eff.weight <- allEffects(complete.weight.lm)
plot(eff.weight)
print(eff.weight)

# anova table
anova(complete.weight.lm)



# part c) -------------------------------------------------------------------------
# partitioning the diet SS using contrasts

# i) test if sup1 and supp2 are more effective over supp3: Hi: (mu1+mu2)/2 = mu3
# Ha: (mu1 + mu2)/2 > mu3
supp12v3 = C(weightData$Supplement, contr=c(1,1,-2), 1)

# ii) test if supp1 and supp 2 differ
# Hii: mu1 = mu2
# Ha: mu1 != mu2
supp1v2 = C(weightData$Supplement, contr=c(1,-1,0),  1)


# fit the model to test the contrasts
supp.testDiffs.lm <- lm(WeightGain ~ supp12v3 + supp1v2, data=weightData)
anova(supp.testDiffs.lm) # yess, all significant. 
summary(supp.testDiffs.lm) #pvalues are the same for both tables since
# the contrasts are orthogonal

# part d) -------------------------------------------------------------------------
# testing the contrasts are orthogonal

# get the contrasts
getContrastMatrix <- function(fit){
      
      xNameOrContrasts <- names(fit$contrasts)
      info <- data.frame(fit$contrasts)[, xNameOrContrasts]
      
      if(is.data.frame(info)){ # then the model was fit with user defined contrasts
            return(as.matrix(info))
      } else { # else can be "contr.treatment" ... etc
            contrasts(fit$model[[xNameOrContrasts]])     
      }
}

suppContrasts <- getContrastMatrix(supp.testDiffs.lm); suppContrasts

# test they are orthogonal
t(suppContrasts) %*% suppContrasts

# CONCLUDE: the off diagonal elements are all zero so YES the 
# contrasts are orthogonal


# part e) -------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(supp.testDiffs.lm, which=c(1,2))
plot(supp.testDiffs.lm, which=c(3,5), cook.levels = c(0.2, 0.5, 1))

# checking normality
shapiro.test(supp.testDiffs.lm$residuals)

# Checking influential points

# This is a function to calculate leverage values of all observations
# and compare them to the mean. If any are greater than the h.mean
# then they are influential. 
influence.leverageValues <- function(fit){
      hs <- hatvalues(fit)
      k <- length(fit$model) - 1
      n <- nrow(fit$model)
      h.mean <- 2*(k+1)/n 
      isInfluential <- hs > h.mean 
      return(data.frame(InfluentialPoints=hs, CutOffInflMean=h.mean, 
                        IsInfluential=isInfluential))
}
# this is a function to compare the cooks distances with the critical value
# at the cutoff point: if any cooks value is greater than the cooks critical 
# value at the 50th percentile on the F(k+1, n-k-1) distribution, then 
# that observation is influential. 
influence.cooksDistances <- function(fit) {
      cks <- cooks.distance(fit)
      k <- length(fit$model) - 1
      n <- nrow(fit$model)
      Fcrit <- qf(0.5, df1=k+1, df2=n-k-1)
      isInfluential <- cks > Fcrit 
      return(data.frame(CooksPoints=cks, CutOffFcrit=Fcrit,
                        IsInfluential=isInfluential))
}

leverageInfo <- influence.leverageValues(supp.testDiffs.lm)
which(leverageInfo$IsInfluential) # So no observations are influential!

cookInfo <- influence.cooksDistances(supp.testDiffs.lm)
which(cookInfo$IsInfluential) # integer(0) array so none are past the
# cutoff cooks value. 


