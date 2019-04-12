setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A2/")
library(ISLR)
library(ggplot2)

options(show.signif.stars = F)

stockData <- read.table("Stockchanges.txt", header=TRUE)
head(stockData)

# part a) Make some exploratory numerical and graphical summaries
pairs(stockData) # no seeming correlation line, just random cloud scatter, between most  pairs

# Overall correlation matrix
cor(stockData[-9]) # excluding the Direction categorical variable

# highest correlation is 0.84 between Volume and Year. As either variable increases,
# the other variable increases also. 
cor(stockData$Year, stockData$Volume)


# Plotting volume
#index = seq_along(stockData$Volume)
ggplot(data=stockData, aes(x=Year, y=Volume)) + geom_point(shape=19) + 
      ggtitle("Year vs Volume")
# shows increasing trend in data set: number of shares traded daily (volume) increased
# over time. 



# part b) logistic regression - full model

stock.full.glm <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
                 data=stockData, family=binomial)

summary(stock.full.glm)
# Only lag2 is significant, given other predictors have been fitted



# Can test overall model fit (statistical utility) using the deviance test: 

DevianceTest(stock.full.glm)

# Conclusion: insufficient evidence to say the model isn't statistically useful. 

# nested likelihood test between null model and fit model
# NOTE: use the count = cbind(success, total) y-value when fitting the fit model
# so that here tempData is made of two cols. Do not let y be a proportion
# otherwise we get weird results. 
DevianceTest <- function(fit){
      family <- fit$family
      tempData <- data.frame(fit$model[1])
      theFormula <- as.formula(paste(colnames(tempData), " ~ 1", sep=""))
      nullModel <- glm(theFormula, family=family, data=tempData)
      
      result <- NestedLikelihoodRatioTest(nullModel, fit, printNice = TRUE)
      
      return(invisible(result))
}

# NOTE: null model deviance is bigger (assuming) so the statistic
# will be negative if not. 
# Equivalent to nested F-test between two models here, like anova(null, alt)
NestedLikelihoodRatioTest <- function(reducedModel, fullModel, printNice=TRUE) {
      y <- reducedModel$model[1]
      pi.hat.Ho <- reducedModel$fitted.values
      pi.hat.Ha <- fullModel$fitted.values
      # don't know why this doesn't work when comparing null Mod with alternative
      #likRatioStat <- -2 * sum(y * log(pi.hat.Ho/pi.hat.Ha) + (1-y)*log((1-pi.hat.Ho)/(1-pi.hat.Ha)))
      likRatioStat <- reducedModel$deviance - fullModel$deviance; likRatioStat
      
      # note: df.null = n - 1, df.resid = n - k - 1, k = num params (not incl. B0)
      df <- reducedModel$df.residual - fullModel$df.residual
      pValue <- 1 - pchisq(likRatioStat, df)
      chi.crit <- qchisq(0.05, df=df, lower.tail=F)
      
      result <- data.frame(Deviance.Ho=reducedModel$deviance, Deviance.Ha=fullModel$dev,
                           df.Ho=reducedModel$df.residual, df.Ha=fullModel$df.residual,
                           df=df, LikRatio=likRatioStat, ChiCrit=chi.crit, PValue=pValue)
      
      statement <- ""
      if(pValue < 0.05){
            statement <- paste("Reject H0. Conclude at least one of the extra β ",
                               "coefficients \n",
                               "in the complete model is nonzero, so that the complete model is\n",
                               "statistically useful for predicting ", names(reducedModel$model)[1], " (y).", sep="")
      } else{
            statement <- 
                  paste("Insufficient evidence to reject Ho, that is, to conclude that",
                        "\nthe extra terms in the complete model are statistically useful",
                        "\nuseful predictors in the regression model.", sep="")
      }
      
      if(printNice){
            #likRatioPrintNice(result, reducedModel, fullModel, statement)
            nf = formula(reducedModel)
            na = formula(fullModel)
            cat("\n")
            cat("#####################################################################\n")
            cat("#######                 Likelihood-Ratio Test                 #######\n")
            cat("#####################################################################\n")
            cat("\tH0: reduced model is true: "); cat(paste(nf[[2]], nf[[1]], nf[3]))
            cat("\n")
            cat("\tHA: complete model is true: "); cat(paste(na[[2]], na[[1]], na[3]))
            cat("\n\n")
            cat("  ΔG:\t\t                                ", result$LikRatio, "\n")
            #cat("  Critical Chi-square (α = 0.05):               ", ChiCrit, "\n")
            cat("  df:\t\t                                ", result$df, "\n")
            cat("  p-value:\t\t                        ", result$PValue, "\n\n")
            cat(statement)
            
            return(invisible(result))
      }else{
            return(result)                 
      }
}



# c) fit logistic with Lag1 and Lag2
stock.glm <- glm(Direction ~ Lag1 + Lag2, family=binomial, data=stockData)
summary(stock.glm)

# d) fit model using all but the first observation
stock.missingObs1.glm <- glm(Direction ~ Lag1 + Lag2, family=binomial, data=stockData,
                            subset=2:nrow(stockData))
summary(stock.missingObs1.glm)

# e) use the missing-one model to predict first observation

# test data is the missing observation row
prob1 = predict(stock.missingObs1.glm, stockData[1,], type="response"); prob1
# 0.5713923
label1 = if(prob1 > 0.5) "Up" else "Down"
label1
# "Up"

# true direction: was "Down" so the first observation was incorrectly classified
stockData$Direction[1]


# f) for loop

n <- nrow(stockData)
binaryClassifications <- rep(0, n)

set.seed(1)

for(i in 1:n){
      # fit model using all but the ith observation
      missingObsi.glm <- glm(Direction ~ Lag1 + Lag2, family=binomial, data=stockData[-i,])
      # compute posterior probability of market moving up (test data is that ith observation)
      prob.i <- predict(missingObsi.glm, stockData[i, ], type="response")
      # compute classification
      label.i <- if(prob.i > 0.5) "Up" else "Down"
      # find if a missclassification occurred: vector of 1's (correct classification)
      # and 0's (missclassification)
      binaryClassifications[i] <- as.numeric(stockData$Direction[i] != label.i)
}

# g) determine whether or not an error was made in predicting direction for an ith observation. 
# if an error was made, indicate this as a 1, else as a 0. (this is in the binaryClassifs) vector
head(binaryClassifications, 200)


# h) take the average to get the LOOCV estimate for the test error
LOOCV.testErrorEstimate <- mean(binaryClassifications); LOOCV.testErrorEstimate
# high test error rate: classified wrongly 44.99541 % of the time. 