setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

## LECTURE PART ---------------------------------------------------------------
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')


# Plotting
aswellsData <- read.table("Chapter 4 - Multiple Linear Regression/aswells_short.txt", header=TRUE)
# the red line is just a smoother
# interpret: Arsenic-latitude - small decrease, corr = -0.183
# interpret 2: the predictor correlations between them are larger than
# predictor corrs with arsenic, the response.
# interpret 3: lots of scatter (variability) in the data - no strong relation
# seems to be present between predictor/predictors and predictors/arsenic. 
pairs(aswellsData[, 1:4], lower.panel=panel.smooth, upper.panel = panel.cor)

# NOTE: if scatter is quadratic, corr is not useful since it only picks up
# on LINEAR association between two quantitative variables. 
# 
ggpairs(aswellsData, columns=1:4, upper=list(continuous="cor", params=c(size=10)), 
        lower=list(continuous="smooth", params=c(color="blue")))

# testing cor of quadratic = 0!
as <- -5:5
bs <- as^2
cor(as, bs)


arsenic.lm <- lm(ARSENIC ~ LATITUDE + LONGITUDE + DEPTHFT, data=aswellsData)
# all slope parameters are significant 
# means predictors contribute information to the y = arsenic level. 
summary(arsenic.lm)

# R^2 = only 12% variability in arsenic is covered by predictors - so predictors
# aren't that useful - need to go back to field to pick up new predictors. 

# 95% CI's for parameters
betaCI(arsenic.lm)
slopeCI(arsenic.lm)
confint(arsenic.lm)


# Residual std error: 103 and squared = 10670
#MSE  = sigma squared = 10670 = SSE / (n-k-1) = 3446366 / 323
anova(arsenic.lm)


# Testing model assumptions
par(mfrow=c(2,2))
plot(arsenic.lm)

library(ggfortify)
autoplot(arsenic.lm)
# Assumptions not satisfied: 
# 1. in residuals vs fitted plot, variance explodes is not constant
# for increasing values of y. 
# 3. for the normal QQ plot, the standardized residuals leap off the
# straight y = x line for higher theoretical quantile values. 


# Continue on to build conf ints as if assumptions were satisfied
LONGITUDE <- 90.67; LATITUDE <- 23.74; DEPTHFT <- 210
newX <- data.frame(LONGITUDE,LATITUDE,DEPTHFT )
predict(arsenic.lm, newdata=newX, interval="confidence")
predict(arsenic.lm, newdata=newX, interval="prediction")


meanCI(arsenic.lm, x.values=c(LATITUDE, LONGITUDE, DEPTHFT))
interpret.MeanCI(arsenic.lm, x.values=c(LATITUDE, LONGITUDE, DEPTHFT),
                 x.units=c("degrees", "degrees", "feet"), y.unit = "litres")
# prediction is so wide it is becoming meaningfless, also wide from
# model's unexplained variability. 
predictCI(arsenic.lm, x.values=c(LATITUDE, LONGITUDE, DEPTHFT))
interpret.PredictCI(arsenic.lm, x.values=c(LATITUDE, LONGITUDE, DEPTHFT),
                 x.units=c("degrees", "degrees", "feet"), y.unit = "litres")




## LECTURE PART ---------------------------------------------------------------
load("data/Exercises and Examples/ASWELLS.Rdata")

arsenic.lm <- lm(ARSENIC ~ LATITUDE + LONGITUDE + DEPTHFT, data=ASWELLS)
summary(arsenic.lm)

interpret.SlopeCoeffs(arsenic.lm)
interpret.SlopeCI(arsenic.lm, x.units = c("degrees", "degrees", "feet"))


# 
lowestLatitude <- range(ASWELLS$LATITUDE)[1]; lowestLatitude
highestLongitude <- range(ASWELLS$LONGITUDE)[2]; highestLongitude
lowestDepth <- range(ASWELLS$DEPTHFT, na.rm=TRUE)[1]; lowestDepth
x.values <- c(lowestLatitude, highestLongitude, lowestDepth)

meanCI(arsenic.lm, x.values=x.values)
interpret.MeanCI(arsenic.lm, x.values=x.values, x.units=c("degrees", "degrees", "feet"))

predictCI(arsenic.lm, x.values=x.values)
interpret.PredictCI(arsenic.lm, x.values = x.values, x.units = c("degrees","degrees","feet"))

