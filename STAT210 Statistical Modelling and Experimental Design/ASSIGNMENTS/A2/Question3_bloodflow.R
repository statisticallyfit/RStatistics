setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A2/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')


library(ggplot2)
options(digits=10, show.signif.stars = F)




# part a)
bflowData <- read.table("bloodflow.txt", header=TRUE)

# The scatterplot
ggplot(bflowData, aes(x=AOT, y=BF)) + 
      geom_point(shape=19, size=3) +
      ggtitle("Scatterplot of Arterial Oxygen Tension (AOT) against Bloodflow (BF)")


# at least a cubic or quadratic polynomial is required, by the number of peaks
# Polynomial order = peaks/troughs - 1. Here is looks like peaks/troughs = 3
# so at most 4th order could be fitted, perhaps. 


# part b)
bflow.1.lm <- lm(BF ~ AOT, data=bflowData)
anova(bflow.1.lm)

bflow.2.lm <- lm(BF ~ AOT + I(AOT^2), data=bflowData)
anova(bflow.2.lm) # quadratic model is significant, so continue

bflow.3.lm <- update(bflow.2.lm, .~. + I(AOT^3), data=bflowData)
anova(bflow.3.lm) # cubic isn't significant so just use quadratic. 


# METHOD - stepwise
df <- data.frame(BF=bflowData$BF, AOT=bflowData$AOT, AOT2=bflowData$AOT^2,
                 AOT3=bflowData$AOT^3,AOT4=bflowData$AOT^4,AOT5=bflowData$AOT^5)

formL = formula(~1)
formU = formula(~AOT + AOT2 + AOT3 + AOT4 + AOT5, data=df)
min.model <- lm(BF ~ 1, data=df)
max.model <- lm(BF ~ AOT + AOT2 + AOT3 + AOT4 + AOT5, data=df)
step.foward <- step(start.model, direction = "forward", 
                    scope=list(lower=formL, upper=formU))

step.backward <- step(max.model, direction = "backward", 
     scope=list(lower=formL, upper=formU))

anova(step.backward)



# part c)
from = min(bflowData$AOT)
to = max(bflowData$AOT)
n <- nrow(bflowData)
aot = seq(from=from,to=to, len=n)
xs <- data.frame(AOT=aot, AOT2=aot^2, AOT3=aot^3, AOT4=aot^4)

CI <- data.frame(predict(step.backward, interval="confidence", newdata=preds))
PI <- data.frame(predict(step.backward, interval="predict", newdata=preds))
pred.df <- data.frame(AOT=preds$AOT, fit=CI$fit, CI.lower=CI$lwr, CI.upper=CI$upr,
                      PI.lower=PI$lwr, PI.upper=PI$upr)


# Plotting confidence bands 
# TODO update the legend positions


par(mfrow=c(1,1))
plot(pred.df$AOT, pred.df$fit, type="b", pch=16, xlab="AOT", ylab="mean Bloodflow", 
     main="Scatterplot of Bflow ~ AOT, with predicted values and 95% 
     confidence and prediction bands", ylim = c(min(allLower), max(allUpper)))
allLower = c(pred.df$CI.lower, pred.df$PI.lower)
allUpper = c(pred.df$CI.upper, pred.df$PI.upper)
points(bflowData$AOT, bflowData$BF)

legend(360,84, lty=c(1, 2, 3), legend=c("Line of best fit", 
                                        "95% Confidence Bands",
                                        "95% Prediction Bands"))
legend(400, 82, pch=c(1,16), legend=c("observed values", "predicted values"))

# lwr CI
lines(pred.df$AOT, pred.df$CI.lower, lty=2) # lty = linetype, lty = 2 is dashed
# upr CI
lines(pred.df$AOT, pred.df$CI.upper, lty=2)
# lwr PI
lines(pred.df$AOT, pred.df$PI.lower, lty=3) # pred intervals always wider than CIs
# upr PI
lines(pred.df$AOT, pred.df$PI.upper, lty=3)





# part d)
library(ggfortify)
autoplot(bflow.2.lm, which=1:2)
# seems random scatter, normal line

shapiro.test(bflow.2.lm$residuals)

