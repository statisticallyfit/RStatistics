setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 6, 7 - Variable Screening, Transformations/lecturedata/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')
source("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R")

#install.packages("matrixStats")
library(matrixStats)
options(digits=10, show.signif.stars = FALSE)


poppyData <- read.table("poppy.txt", header=TRUE)
poppyData$treat <- factor(poppyData$treat)
poppyData$block <- factor(poppyData$block)


# under treatment A, we have higher count than treat A and higher variability. 
# SD for A is higher than all other treatments from A -F (drop in means and sds)
# So variance may not be constant among the six groups - problem. 

# This is what we do now - dealing with count data unconstant variability. 
# Most of the time for count data, variance is not constant between groups. 




poppy1.lm <- lm(count ~ treat + block - 1, data=poppyData)
summary(poppy1.lm)
head(betaCI(poppy1.lm))

# INTERPRET: standard errors are same for each one - this is based on 
# asssumption of ANOVA, same pooled SDSs but that is not valid for this data. 
# PROBLEM: negative lower limits for E and F (count cannot be negative)



# DIAGNOSTICS
autoplot(poppy1.lm, which=1:3)
shapiro.test(poppy1.lm$residuals) 
# residuals - have increase in variance (cone shape) (VARIANCE HOMO VIOLATED)
# normality - good, but two potential outliers
# scale location - take absolute values of residuals and square roots them. 
# --- useful because: can detect a trend when you have little data, for picking
# up on nonconst variance. An increasing or decreasing trend suggests
# a nonconst variance.




# COMPARING TRANFORMATIONS
As <- poppyData[poppyData$treat=="A",]
Bs <- poppyData[poppyData$treat=="B",]
Cs <- poppyData[poppyData$treat=="C",]
Ds <- poppyData[poppyData$treat=="D",]
Es <- poppyData[poppyData$treat=="E",]
Fs <- poppyData[poppyData$treat=="F",]

groupCounts <- with(poppyData, tapply(count, list(treat, block), mean))
groupCounts

library(matrixStats)
transCompare = data.frame(meansY=rowMeans(groupCounts), varsY=rowVars(groupCounts), 
           meansSqrtY=rowMeans(sqrt(groupCounts)), 
           varsSqrtY=rowVars(sqrt(groupCounts)),
           meansLnY=rowMeans(log(groupCounts)), 
           varsLnY=rowVars(log(groupCounts)))
transCompare

# Now making the Fmax for each column = largest group var / smallest group var
attach(transCompare)
Fmaxes <- c(max(varsY) / min(varsY), max(varsSqrtY)/min(varsSqrtY),
            max(varsLnY)/min(varsLnY))
Fmaxes
# Square root is doing best in association between means and vars, and its
# Fmax is smaller. Ratio must be < 4 (rule of thumb)
detach(transCompare)



# new model
poppy.sqrt.lm <- lm(sqrt(count) ~ block + treat, data=poppyData)
betaCI(poppy.sqrt.lm) # no more negatives




# Predictions
pred.df <- data.frame(block=1, treat=LETTERS[1:6])
pred.df$block <- factor(pred.df$block)
pred.df
preds <- predict(poppy.sqrt.lm, newdata=pred.df, interval="confidence")
pred.df <- cbind(pred.df, preds)

# Square results (backgtransform of sqrt)
pred.df$backT_fit <- pred.df$fit^2
pred.df$backT_lwr <- pred.df$lwr^2
pred.df$backT_upr <- pred.df$upr^2
pred.df

# PROBLEM
# more resources: 
# https://stackoverflow.com/questions/14069629/plotting-confidence-intervals

#ggplot(data=poppyData, aes(x=count, y=treat, colour=block)) + geom_point() +
#      geom_errorbar(aes(ymin=min(pred.df$backT_lwr), ymax=max(pred.df$backT_upr)))





#d=data.frame(drink=c("coffee","tea","water"), mean=c(3,6,2), 
#             lower=c(2.6,5.6,1.8), upper=c(3.5,6.3,2.8))
#ggplot() + 
#      geom_errorbar(data=d, mapping=aes(x=drink, ymin=upper, ymax=lower), #
#                    width=0.1, size=1, color="purple") + 
#      geom_point(data=d, mapping=aes(x=drink, y=mean), size=5, shape=19, fill="white")

#ggplot() + 
#      geom_errorbar(data=pred.df, mapping=aes(x=count, ymin=backT_lwr,
#                                              ymax=backT_upr),
#                    with=0.1, size=1, color="purple") + 
#      geom_point(data=pred.df, mapping=aes(x=count, y = backT_fit),
#                 size=5, shape=19, fill="black")



# To find the power transformation
library(MASS)
# running these commands when using docker R: 
#setwd("~/projects/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 6, 7 - Variable Screening, Transformations/lecturedata/")
#poppyData <- read.table("poppy.txt", header=TRUE)
#poppyData$treat <- factor(poppyData$treat)
#poppyData$block <- factor(poppyData$block)
# note: lambda seq from to sets the x-axis range. 
boxcox(count ~ block + treat, data=poppyData, lambda=seq(from=0,to=1,by=0.01))
# the likely values of lambda are within the 95% confidence interval.

# Choose one of the transformation powers that are within this
# confidence interval - so choose the square root transformation now. 