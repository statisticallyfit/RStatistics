setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PRACTICALS/Practical 1 - Intro/")

source('/datascience/projects/statisticallyfit/github/R/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')


library(ggplot2)
# 
# setting digits options:
options(digits=3, show.signif.stars = FALSE)

## Getting data
acidData <- read.table("acid.txt", header=TRUE)
acidData

## Listing names of variables
m = print(names(acidData)); m 
names(acidData)


## Listing first 3 lines
head(acidData, 3)

## Fitting Linear Model with Y = fungus, X = acid
acid.lm <- lm(fungus ~ acid, data=acidData)
summary(acid.lm)

## Anova table
anova(acid.lm)


## Plotting
# same thing (plotting fungus ~ acid means plotting fungus as function of acid = plot(y ~ x))
## Adding the least squares line to the plot
plot(acidData$acid, acidData$fungus)
plot(fungus ~ acid, data=acidData)
abline(acid.lm)
legend(13, 30, legend="fungus = 31.7 - 0.75 acid  ") # coeffs of reg.line


# Plot with ggplot
g <- ggplot(acidData, aes(x = acid, y = fungus))
g + geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="Acid", y="Fungus") +
      stat_smooth(method="lm", col="red", lwd=1)
      #geom_smooth(method="lm", lwd=1, alpha=0.1, fill="red")

# Easier way, all set out
ggplotRegression(acid.lm, x.value=15)


## Prediction intervals
meanCI(acid.lm, x.value=15, level=0.95)
predict(acid.lm, new=data.frame(acid=15), interval="confidence", level=0.95)
interpret.MeanCI(acid.lm, x.value=15, x.unit="mug/mL",y.unit = "mm")

predictCI(acid.lm, x.value=15, level=0.95)
predict(acid.lm, new=data.frame(acid=15), interval="prediction", level=0.95)
interpret.MeanCI(acid.lm, x.value=15, x.unit="mug/mL",y.unit = "mm")
interpret.PredictCI(acid.lm, x.value=15, x.unit="mug/mL",y.unit = "mm")

# Confidence intervals for regression parameters
confint(acid.lm, level=0.95)



## ------- Checking Assumptions ---------------


### 1) NORMALITY 

# Diagnostic plots to check model assumptions
par(mar=c(1,1,1,1)) # to avoid the figure margin error
par(mfrow=c(2,2))
plot(acid.lm, which=1:4)

# Diagnostics with ggplot
library(ggfortify)
autoplot(acid.lm) # don't even need par(mfrow) setting

# Shapiro wilk tests normality of residuals
# H0: residuals are normally distributed
shapiro.test(acid.lm$residuals)



### 2) EQUAL VARIANCE
s <- standardErrorOfRegression(acid.lm); s

# can get this value with anova table too:
anv <- anova(acid.lm) # (under MeanSq Column and Resids Row - that is s^2)
anv
anv$`Mean Sq`[2]^(1/2)



## Checking correlation
cor(acidData$fungus, acidData$acid)
# (strong negative linear relation between fungus and acid)