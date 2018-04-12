setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A2/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')

library(ggplot2)
library(ggfortify)
options(digits=10, show.signif.stars = F)


# part a)
bflowData <- read.table("bloodflow.txt", header=TRUE)

# Scatterplot suggests there are 3 peaks and troughs
ggplot(bflowData, aes(x=AOT, y=BF)) + 
      geom_point(shape=19, size=3) +
      ggtitle("Arterial Oxygen Tension vs. Bloodflow")

# Polynomial order = peaks/troughs - 1. Here is looks like peaks/troughs = 3
# so at most 3+1= 4th order could be fitted, perhaps. 
# There is definite curvature so at least a quadratic model could be fitted.


# part b)
# see: linear is definitely not appropriate: anova term for AOT
# means model with AOT is not significant. That is the p-value of the
# global F-test. 
bflow.1.lm <- lm(BF ~ AOT, data=bflowData)
anova(bflow.1.lm)
# diagnostics: there is curvature in residuals, which suggests the
# quadratic term is missing. 
par(mfrow=c(1,2))
plot(bflow.1.lm, which=c(1,2), add.smooth = FALSE)


bflow.2.lm <- lm(BF ~ AOT + I(AOT^2), data=bflowData)
anova(bflow.2.lm) # quadratic model is significant, given linear
# model has been fitted, so continue


bflow.3.lm <- update(bflow.2.lm, .~. + I(AOT^3), data=bflowData)
anova(bflow.3.lm) # cubic isn't significant so just use quadratic. 



# part c)
from = min(bflowData$AOT)
to = max(bflowData$AOT)
xs <- data.frame(AOT=seq(from=from,to=to, len=nrow(bflowData)))

# the levels are by default 95% for the CIs
CI <- data.frame(predict(bflow.2.lm, interval="confidence", newdata=xs))
# placing also the AOT generated values here for plotting purposes. 
pred.df <- data.frame(AOT=xs$AOT, fit=CI$fit, lwr=CI$lwr, upr=CI$upr)

# Plotting confidence bands 
par(mfrow=c(1,1))
plot(fit ~ AOT,  xlab="Arterial Oxygen Tension", ylab="Bloodflow", 
     data=pred.df, pch=20,
     main="Predicted and Observed Values of BF vs AOT and 95% Confidence Bands", 
     ylim = c(min(pred.df$lwr), max(pred.df$upr)))

points(y=bflowData$BF, x=bflowData$AOT )

lines(pred.df$AOT, pred.df$fit, lty=1)
lines(pred.df$AOT, pred.df$lwr, lty=2) # lower CI (2.5%)
lines(pred.df$AOT, pred.df$upr, lty=2) # upper CI (97.5%)

legend(360,84, lty=c(1, 2, 3), legend=c("Line of best fit", 
                                        "95% Confidence Bands"))
legend(400, 82, pch=c(1,16), legend=c("observed values", "predicted values"))



# part d)

par(mfrow=c(2,2))
plot(bflow.2.lm, add.smooth=FALSE)
par(mfrow=c(1,2))
plot(bflow.2.lm, which=c(4,6), add.smooth = FALSE)

cooksDistance(bflow.2.lm)
influentialPoints(bflow.2.lm)

shapiro.test(bflow.2.lm$residuals) # no deviation from normality. 




# part e)
summary(bflow.2.lm)
