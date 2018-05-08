setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/TINLEAD.Rdata")
options(digits=10, show.signif.stars = FALSE)

is.factor(TINLEAD$ANTIMONY)
TINLEAD$ANTIMONY <- factor(TINLEAD$ANTIMONY)
is.factor(TINLEAD$METHOD)


tin.lm <- lm(STRENGTH ~ ANTIMONY * METHOD, data=TINLEAD)
summary(tin.lm)
anova(tin.lm)

# interesting! so the order of fit doesn't seem to matter ..
# since corresponding F-values are the same. 
tin.lm2 <- lm(STRENGTH ~ METHOD * ANTIMONY, data=TINLEAD)
anova(tin.lm2)

cof2 <- coef(summary(tin.lm2))[,1]
cof1 <- coef(summary(tin.lm))[,1]
cbind(cof1,cof2) # coefficients for different orders of fit are NOT the same. 




# testing interaction: F = 1.6, p = 0.152, not significant, so test MAINS
# main (METHOD) p = 0.003, signif
# main (ANTIMONY) p = 0.00000016 so signif
anova(tin.lm)



# refitting quant model
tinQuantData <- TINLEAD
tinQuantData$ANTIMONY <- as.numeric(tinQuantData$ANTIMONY)

tin.quant.lm <- lm(STRENGTH ~ ANTIMONY + METHOD + ANTIMONY:METHOD, data=tinQuantData)
summary(tin.quant.lm)
anova(tin.quant.lm)

# testing that second and third order terms can be dropped
tin.complete.lm <- lm(STRENGTH ~ ANTIMONY + I(ANTIMONY^2) + METHOD + 
                            ANTIMONY:METHOD + I(ANTIMONY^2):METHOD, 
                      data=tinQuantData)
summary(tin.complete.lm)

# enough evidence to reject H0 and say that the higher order params are not zero
# so they cannot be dropped from the model
df = NestedFTest(tin.quant.lm, tin.complete.lm)
df$RecommendedName
# OR do this
anova(tin.quant.lm, tin.complete.lm)
