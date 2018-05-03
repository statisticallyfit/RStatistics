setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A2/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')

library(ggplot2)
library(car)
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
par(mfrow=c(1,1))
crPlot(bflow.1.lm, variable="AOT", main="Partial Residuals for AOT in Linear Model")

summary(bflow.1.lm)

# ---------------------------------------------------------------------
bflow.2.lm <- lm(BF ~ AOT + I(AOT^2), data=bflowData)
anova(bflow.2.lm) # quadratic model is significant, given linear
# model has been fitted, so continue

# Partial plot suggests curvature was removed. 
par(mfrow=c(1,1))
crPlot(bflow.2.lm, variable="AOT", 
       main="Partial Residuals for AOT in Quadratic Model")

summary(bflow.2.lm)
betaCI(bflow.2.lm)

# ---------------------------------------------------------------------
bflow.3.lm <- lm(BF ~ AOT + I(AOT^2) + I(AOT^3), data=bflowData)
anova(bflow.3.lm) # cubic model isn't significant so just use quadratic. 



# part c)
from = min(bflowData$AOT)
to = max(bflowData$AOT)
xs <- data.frame(AOT=seq(from=from,to=to, len=nrow(bflowData)))

# the levels are by default 95% for the CIs
CI <- data.frame(predict(bflow.2.lm, interval="confidence", newdata=xs))
# placing also the AOT generated values here for plotting purposes. 
pred.df <- data.frame(AOT=xs$AOT, fit=CI$fit, lwr=CI$lwr, upr=CI$upr)

# Plotting confidence bands 
p.data = ggplot(bflowData, aes(x=AOT, y=BF)) + 
      geom_point(shape=19, size=3) 

p.fits = p.data + 
      geom_line(data=pred.df, aes(y=fit, colour="a", linetype="a"),size=1) +
      geom_line(data=pred.df, aes(y=lwr, colour="b", linetype="b"),size=1) + 
      geom_line(data=pred.df, aes(y=upr, colour="b", linetype="b"),size=1) 

p.plot <- p.fits + 
      ggtitle("Predicted and Observed Values of BF vs AOT 
              and 95% Confidence Bands") +
      scale_colour_manual(name="Legend", values=c("a"="red", "b"="dodgerblue"),
                          labels=c("Fitted Line", "95%\nConfidence\nBands")) +
      scale_linetype_manual(name="Legend", values=c("a"="solid", "b"="dashed"),
                            labels=c("Fitted Line", "95%\nConfidence\nBands"))

p.plot 

# NOTE: my addition later - this works!!!! ------------------------------------
plotConfidenceBands.lm(bflow.3.lm)
plotConfPredBands.lm(bflow.2.lm)
plotConfPredBands.lm(bflow.3.lm)
# -----------------------------------------------------------------------------




# part d)

# Check residuals and normality 
par(mfrow=c(2,2))
plot(bflow.2.lm, add.smooth=FALSE, which=c(1,2,3,5), cook.levels=c(0.2,0.5,1.0))

# Checking normality assumption: formal test. 
shapiro.test(bflow.2.lm$residuals) # no deviation from normality. 

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

leverageInfo <- influence.leverageValues(bflow.2.lm)
obs14 <- which(leverageInfo$IsInfluential) # So observation 14 is influential
# The leverage of the 14th observation is about 0.4508 > h.mean = 0.4
leverageInfo[obs14, ]

cookInfo <- influence.cooksDistances(bflow.2.lm)
which(cookInfo$IsInfluential) # integer(0) array so none are past the
# cutoff cooks value. 
cookInfo$CutOffFcrit[1]
cookInfo[obs14,] # its value is close to the cutoff, but not past it. 
cookInfo[2, ] # observation 2 is not close to the cooks cutoff. 
cookInfo[4, ] # observation 4 is not close to the cooks cutoff. 
