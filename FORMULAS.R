
# use DescTools
# Kurt
# Skew
#skewness <- function(x) {
#      m3 <- mean((x - mean(x))^3)
#      return(m3/(sd(x))^3)
#}
#kurtosis <- function(x) {
#      m4 <- mean((x - mean(x))^4)
#      return(m4/(sd(x))^4 - 3)
#}



# same SSE as for ANOVA as one from SUMMARY
SSE <- function(fit) {
      y <- fit$model[[1]]
      yhat <- fit$fitted
      
      return(sum( (y - yhat)^2 ))
}
# can get this value with anova table too:
# (under MeanSq Column and Resids Row - that is s^2)
standardErrorOfRegression <- function(fit) {
      sse.value <- invisible(SSE(fit))
      n <- length(fit$fitted)
      kplusOne <- ncol(fit$model)
      
      return (sqrt((sse.value) / (n - kplusOne)))
}

standardErrorOfSlope <- function(fit) {
      x <- fit$model[[2]]
      
      s <- standardErrorOfRegression(fit)
      return (s / sqrt(SSxx(x)))
}

SSyy <- function(y) {
      y.mean <- mean(y)
      return ( sum( (y - y.mean)^2 ) )
}

SSxx <- function(x) {
      x.mean <- mean(x)
      return ( sum( (x - x.mean)^2 ) )
}

SSxy <- function(x, y) {
      x.mean <- mean(x)
      y.mean <- mean(y)
      return ( sum( (x - x.mean) * (y - y.mean) ) )
}
#standardErrorOfRegression <- function(sse.value, n) {
#      return(sqrt(sse.value / (n - 2)))
#}
# OR
#a <- anova(lm)
#ss <- a$`Sum Sq`
#return(ss[length(ss)])
# OR
#s <- summary(r.lm)
#(1-s$r.squared)*SST(var$datamat$dc)

SST <- function(y) {
      y.mean <- mean(y)
      return ( sum( (y - y.mean)^2 ) )
}

## Confidence interval for the Slope in Regression Model (simple)
slopeCI <- function(fit, level = 0.95) {
      slope <- fit$coefficients[[2]]
      sB1 <- standardErrorOfSlope(fit)
      
      # Calculate alpha for the t-statistic, and df
      alpha <- 1 - level
      n <- length(y)
      df <- n - 2 
      t.alpha2 <- abs(qt(alpha/2, df))
      
      # calculate conf.int 
      leftCI <- slope - t.alpha2 * sB1
      rightCI <- slope + t.alpha2 * sB1
      
      return( c(leftCI, rightCI) )
}

# Confidence interval for the Mean when x = xp 
# E(y) = yhat(xp) +- t(alpha/2,df) * s * sqrt(1/n + (xp - xmean)^2/SSxx)
## NOTE: can be found using R with this: 
# predict(model, new = data.frame(x.name=x.value), interval="confidence", level=0.95)
meanCI <- function(fit, x.values, level=0.95){
      predictorNames <- names(fit$model)[-1]
      df <- data.frame(rbind(x.values))
      rownames(df) <- ""
      colnames(df) <- predictorNames
      
      return(predict(fit, new = df, interval="confidence", level=level))
}

predictCI <- function(fit, x.values, level=0.95){
      predictorNames <- names(fit$model)[-1]
      df <- data.frame(rbind(x.values))
      rownames(df) <- ""
      colnames(df) <- predictorNames
      
      return(predict(fit, new = df, interval="prediction", level=level))
}
#meanCI <- function(fit, x.value, level=0.95) {
#      # finding the yhat value at a particular x-.value
#      yhat.at.xp <- fit$coeff[[2]] * x.value + fit$coeff[[1]]
#      yhat <- fit$fitted
#      x <- fit$model[[2]]
#      y <- fit$model[[1]]
#      
#      # Finding the std error of regression, s 
#      s <- standardErrorOfRegression(fit)
#      
#      # Finding the t, alpha
#      alpha <- 1 - level 
#      n <- length(y)
#      df <- n - 2
#      t.alpha2 <- abs(qt(alpha/2, df))
#      
#      # Calculating the confidence interval. 
#      leftCI <- yhat.at.xp - t.alpha2 * s * sqrt(1/n + (x.value - mean(x))^2 / SSxx(x))
#      rightCI <- yhat.at.xp + t.alpha2 * s * sqrt(1/n + (x.value - mean(x))^2 / SSxx(x))
#      
#      return(c(leftCI, rightCI))
#}




# Confidence interval for the Predictor when x = xp
# yi = yhat(xp) +- t(alpha/2,df) * s * sqrt(1 + 1/n + (xp - xmean)^2/SSxx)
## NOTE: can be found using R with this: 
# predict(model, new = data.frame(x.name=x.value), interval="prediction", level=0.95)
#predictCI <- function(fit, x.value, level=0.95) {
#      # finding the yhat value at a particular x-.value
#      yhat.at.xp <- fit$coeff[[2]] * x.value + fit$coeff[[1]]
#      yhat <- fit$fitted
#      x <- fit$model[[2]]
#      y <- fit$model[[1]]
#      
#      # Finding the std error of regression, s 
#      s <- standardErrorOfRegression(fit)
#      
#      # Finding the t, alpha
#      alpha <- 1 - level 
#      n <- length(y)
#      df <- n - 2
#      t.alpha2 <- abs(qt(alpha/2, df))
#      
#      # Calculating the confidence interval. 
#      leftCI <- yhat.at.xp - t.alpha2 * s * sqrt(1 + 1/n + (x.value - mean(x))^2 / SSxx(x))
#      rightCI <- yhat.at.xp + t.alpha2 * s * sqrt(1 + 1/n + (x.value - mean(x))^2 / SSxx(x))
#      
#      return(c(leftCI, rightCI))
#}