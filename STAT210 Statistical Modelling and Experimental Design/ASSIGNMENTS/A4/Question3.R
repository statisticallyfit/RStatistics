setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A4")

options(digits=10, show.signif.stars = FALSE)

# Preparing the data
weightData <- data.frame(Supplement=c(rep("1", 5), rep("2", 5), rep("3", 5)), 
                         DietaryLitter=rep(1:5, 3), 
                         WeightGain=c(28.7, 30.6, 27.2, 28.6, 31.9,
                                      30.7, 34.5, 32.6, 34.4, 30.7, 
                                      25.4, 26.3, 27.5, 24.3, 25.8))
weightData

mean(subset(weightData, Supplement=="1")$WeightGain)
mean(subset(weightData, Supplement=="2")$WeightGain)
mean(subset(weightData, Supplement=="3")$WeightGain)



# part b) -------------------------------------------------------------------------- 
# Do the data indicate difference across the dietary supplements? 
# Do the nested F test using the rediuced model which contains just blocks, 
# and the complete model
complete.weight.lm <- lm(WeightGain ~ DietaryLitter + Supplement, data=weightData)
summary(complete.weight.lm)

# from anova, the nested F-test between block model and complete model gives
# F = 18.86, p-value = 0.00027 so there is a difference in mean weight gain across 
# the supplements (1,2,3)
anova(complete.weight.lm)



# part c) -------------------------------------------------------------------------
# partitioning the diet SS using contrasts

# i) test if sup1 and supp2 are more effective over supp3: Hi: (mu1+mu2)/2 = mu3
# Ha: (mu1 + mu2)/2 > mu3
supp12v3 = C(weightData$Supplement, contr=c(1,1,-2), 1)

# ii) test iff supp1 and supp 2 differ
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
par(mfrow=c(2,2))
plot(complete.weight.lm, which=1:4, add.smooth = FALSE)
par(mfrow=c(1,2))
plot(complete.weight.lm, which=5:6, cook.levels = c(0.1, 0.2, 0.5), add.smooth = FALSE)
