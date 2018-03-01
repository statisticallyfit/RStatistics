setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/OJUICE.Rdata")


reg.line <- lm(SWEET ~ PECTIN, data=OJUICE)
summary(reg.line)
anova(reg.line)

# Setting data values 
y <- OJUICE$SWEET
yhat <- reg.line$fitted.values
x <- OJUICE$PECTIN

n <- length(y)

# Calculating SSE and the numbers that build it up. 
SSyy(y)
SSxx(x)
SSxy(x, y)

## Getting the SSE
SSE(y, yhat)
# another way 
slope <- reg.line$coefficients[[2]]
sse <- SSyy(y) - slope * SSxy(x,y); sse 

## Getting std.error of regression
s <- standardErrorOfRegression(y, yhat); s 
# directly (another way)
sqrt(SSE(y,yhat) / (n - 2))


## Getting the standard error of slope
standardErrorOfSlope(x, y, yhat)
summary(reg.line)

## Getting conf.int
slopeCI(x, y, level=0.90)
