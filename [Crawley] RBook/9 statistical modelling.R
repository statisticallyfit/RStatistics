library(MASS)

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learnstatistics/crawleyRBook")

timberData <- read.delim("data/timber.txt")
names(timberData)
attach(timberData)

# ===============================================================
# BOX-COX TRANSFORMATION, MAXIMUM LIKELIHOOD 

boxcox(volume ~ log(girth) + log(height), 
       lambda = seq(-0.5, 0.5, 0.01))
# ==> likelihood is maxed at lambda~ -0.08 = close to zero with log

boxcox(volume ~ girth + height)
boxcox(volume ~ girth + height, 
       lambda = seq(0.1, 0.6, 0.01))
# ==> likelihood is maxed at lambda ~ 1/3

# ===============================================================
# NORMAL ERRORS

modelCheck <- function (obj, ...) 
{
      resids <- obj$resid
      fits <- obj$fitted
      par(mfrow = c(1, 2))
      plot(fits, resids, xlab="Fitted", ylab="Residuals", 
           pch=16, col="red")
      abline(h=0, lty=2)
      qqnorm(resids, xlab="Normal scores", ylab="Ordered Residuals", 
             main="", pch=16)
      qqline(resids, lty=2, col="green")
      par(mfrow = c(1,1))
      invisible(NULL)
}

x <- 0:30
# errors are taken from different distributions for each (xi)
e <- rnorm(31, mean=0, sd=5) 
y <- 10 + x + e
model <- lm(y ~ x)

modelCheck(model)


# UNIFORM ERRORS
eu <- 20 * (runif(31) - 0.5)
yu <- 10 + x + eu
unifModel <- lm(yu ~ x)
modelCheck(unifModel)


# NEGATIVE BINOMIAL ERRORS
enb <- rnbinom(31, 2, 0.3)
ynb <- 10 + x + enb
mnb <- lm(ynb ~ x)
modelCheck(mnb)
