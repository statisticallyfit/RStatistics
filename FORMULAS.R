source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')
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

MSE <- function(fit) {
      s <- standardErrorOfRegression(fit)
      return(s^2)
}

# Validation - training MSE
# Given a model fit on the training data, use the full Data set to calculate test info
# to calculate the test MSE
MSE <- function(train.fit, fullData){
      
      trainIndices <- as.numeric(rownames(train.fit$model))
      yName = colnames(train.fit$model)[1]
      y <- fullData[,yName]
      yHat = predict(train.fit, fullData)
      return(mean( (y - yHat)[-trainIndices]^2 ))
}


# take only test index rows and the predictors used in the model
#testData <- fullData[-trainIndices, colnames(train.fit$model)]
#testY <- testData[, 1]
#preds = predict(train.fit, testData)
#overallMSE <- mean( (testY - preds)^2 )

standardErrorOfSlope <- function(fit) {
      x <- fit$model[[2]]
      
      s <- standardErrorOfRegression(fit)
      return (s / sqrt(SSxx(x)))
}

SSyy <- function(fit) {
      print("Warning: assuming quantitative response data")
      
      y <- fit$model[[1]]
      y.mean <- mean(y)
      return ( sum( (y - y.mean)^2 ) )
}

SSxx <- function(x) {
      #x <- fit$model[[xName]]
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

SST <- function(fit) {
      return(SSyy(fit) - SSE(fit))
}

# TODO
# between group variance (equivalent to SST)
# input: list of xs and ys, where xs are the factor names and y = response
betweenGroupVariance <- function(xs, ys){
      groups <- unique(xs)
      
      grandMean <- mean(ys)
      
}
# within group variance (same as SSE)
# from page 636 in Allan Bluman

# TODO: BlockDesignTest (anova nested) that contains tests for both
# the treatments and blocks as in the notes. 
# TODO RandomizedDesignTest (anova global F)
# TODO: TwoFactorialDesignTest (anova)


## Confidence interval for the Slope in Regression Model (multiple)
slopeCI <- function(fit, level=0.95) {
      mat <- confint(fit, level=level)
      df <- data.frame(as.numeric(mat[,1]), as.numeric(mat[,2]))
      colnames(df) <- colnames(mat)
      return(df)
}
#slopeCI <- function(fit, level = 0.95) {
#      slope <- fit$coefficients[[2]]
#      sB1 <- standardErrorOfSlope(fit)
#      
#      # Calculate alpha for the t-statistic, and df
#      alpha <- 1 - level
#      n <- length(y)
#      df <- n - 2 
#      t.alpha2 <- abs(qt(alpha/2, df))
#      
#      # calculate conf.int 
#      leftCI <- slope - t.alpha2 * sB1
#      rightCI <- slope + t.alpha2 * sB1
#      
#      return( c(leftCI, rightCI) )
#}

# Confidence interval for the Mean when x = xp 
# E(y) = yhat(xp) +- t(alpha/2,df) * s * sqrt(1/n + (xp - xmean)^2/SSxx)
## NOTE: can be found using R with this: 
# predict(model, new = data.frame(x.name=x.value), interval="confidence", level=0.95)

# TODODO use Rfunctions file betaCI to get many decimal places in output
meanCI <- function(fit, x.values=c(), level=0.95){
      predictorNames <- names(fit$model)[-1]
      df <- data.frame(x.values)
      #rownames(df) <- rep("", length(predictorNames))
      colnames(df) <- predictorNames
      
      return(predict(fit, newdata = df, interval="confidence", level=level))
}

predictCI <- function(fit, x.values, level=0.95){
      predictorNames <- names(fit$model)[-1]
      df <- data.frame(cbind(x.values))
      #rownames(df) <- ""
      colnames(df) <- predictorNames
      
      return(predict(fit, newdata = df, interval="prediction", level=level))
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




# Given restricted and unrestricted models, 
# do joint F-test but give Chi square instead of F
# k = number of restrictions = df1
# n - (k+1) = df2, n = sample size. 
##### TODO: write the hypothesis tested: figure out the B# = B# = 0 etc. 
# from the coefs that are missing in the reduced model. 
####################################### TODO: check out EtaSq from DescTools
######################################## Use Anova() from car package.

#TODO: if the predictor is a factor THEN make the null hypothesis be stated
# as H0: mu_level1 = mu_level2 = ....
NestedFTest <- function(r.lm, u.lm, printNice=TRUE){
      # g = number of parameter difference
      g = length(u.lm$coef) - length(r.lm$coef)
      # k = num parameters in the unrestricted/complete model
      k = length(u.lm$coef) - 1
      # n - k - 1
      df2 = nrow(u.lm$model) - k - 1
      df1 = g #
      sse.r <- SSE(r.lm)
      sse.c <- SSE(u.lm)
      
      F.nested <- ((sse.r - sse.c) / df1) / ((sse.c / df2)); F.nested
      chi.stat <- F.nested * df1
      chi.p.value <- round(1 - pchisq(chi.stat, df=df1), 5)
      f.p.value <- round(1 - pf(F.nested, df1=df1, df2=df2), 5)
      
      result = data.frame(ChiSquare=chi.stat, ChiSqPValue=chi.p.value,
                          df.chi = df1,
                          FStatistic=F.nested, FPvalue=f.p.value, 
                          df1=df1, df2=df2)
      if(f.p.value < 0.05){
            recommend = u.lm
            recName = "complete model"
      } else{
            recommend = r.lm
            recName = "reduced model"
      }
      result = list(RecommendedModel=recommend, RecommendedName=recName,Test=result)
      
      if(printNice){
            nu = formula(u.lm)
            nr = formula(r.lm)
            cat("\n")
            cat("#############################################################\n")
            cat("####                 Analysis Of Variance                 ###\n")
            cat("####    (F Test and Chi-Square Test for Nested Models)    ###\n")
            cat("#############################################################\n")
            cat("\n")
            cat("H0: reduced model is true: \n"); cat(paste(nr[[2]], nr[[1]], nr[3]))
            cat("\n\nHA: complete model is true: \n"); cat(paste(nu[[2]], nu[[1]], nu[3]))
            cat("\n\n")
            cat("F statistic:                         ", F.nested, "\n")
            cat("p-value:                             ", f.p.value, "\n")
            cat("numerator df:                        ", df1, "\n")
            cat("denominator df:                      ", df2, "\n\n")
            
            return(invisible(result))
      }else{
            return(result)                 
      }
}

# The Global F-test for regression model fit. 
FTest <- function(fit) {
      sst = SST(fit)
      sse = SSE(fit)
      k = length(fit$coefficients) - 1
      n = nrow(fit$model)
      Fcrit <- ((sst - sse)/k) / (sse / (n-k-1))
      p.value <- 1 - pf(Fcrit, df1=k, df2= n-k-1)
      
      cat("\n")
      cat("#############################################################\n")
      cat("####                 Analysis Of Variance                 ###\n")
      cat("####             (F Test The Regression Model)            ###\n")
      cat("#############################################################\n")
      cat("\n")
      cat("F statistic:                         ", Fcrit, "\n")
      cat("p-value:                             ", p.value, "\n\n")
      df <- data.frame(FStatistic=Fcrit, PValue=p.value, 
                       DFNumerator=k, DFDenominator=n-k-1)
      rownames(df) <- ""
      return(invisible(df))
      
}


# REASONOING: method shown in section 8.4 Sinicich. 
# We are splitting data determined by xSplit (by value of xs) and then
# doing F-variance test on regression stderroreg of both models (MSE's)

# xSplit = the x value such that x <= xSplit is sample 1 and x > xSplit is sample2
# variableName = the name of the variable that we want to split on. 
# fit = the lm object
# alpha = sig level
# alternative = two-sided or one-sided (cif one sided then calcs which side)

# PREREQ: only for non-multiple regression (simple models)
HomoskedasticityRegressionTest.Split <- function(theFormula, data, xName, xSplit,
                                                 alternative="two-sided", 
                                                 alpha=0.05){
      
      sample1 <- data[data[[xName]] <= xSplit, ]
      sample2 <- data[data[[xName]] > xSplit, ]
      
      fit1 <- lm(theFormula, data=sample1)
      fit2 <- lm(theFormula, data=sample2)
      
      n1 <- nrow(sample1)
      n2 <- nrow(sample2)
      
      Fstat <- 0
      k <- ncol(fit1$model) -1# should be equal to that of mod2, is num independent vars
      # including transformations. 
      numerator.df <- 0
      denominator.df <- 0
      mse1 <- MSE(fit1)
      mse2 <- MSE(fit2)
      if(mse1 > mse2) { 
            Fstat <- mse1 / mse2
            numerator.df <- n1 - (k+1)
            denominator.df <- n2 - (k+1)
      } else {
            Fstat  <- mse2 / mse1
            numerator.df <- n2 - (k+1)
            denominator.df <- n1 - (k+1)
      }
      
      
      
      # calculating F-crit
      Fcrit <- 0
      p.value <- 0
      alternativeStr <- 0 
      if(alternative == "two-sided"){
            alternativeStr <- "Ha: MSE.1 != MSE.2"
            Fcrit <- qf(alpha/2, df1=numerator.df, df2=denominator.df, lower.tail=F)
            p.value <- 2*pf(Fstat, df1=numerator.df, df2=denominator.df, lower.tail=F)
      } else { # else it's one-sided
            if(mse1 < mse2) alternativeStr <- "Ha: MSE.1 < MSE.2"
            else alternativeStr <- "Ha: MSE.1 > MSE.2"
            Fcrit <- qf(alpha, df1=numerator.df, df2=denominator.df, lower.tail=F)
            p.value <- pf(Fstat, df1=numerator.df, df2=denominator.df, lower.tail=F)
      }
      
      cat("\n")      
      cat("#############################################################\n")
      cat("########     Homoskedasticity of Regression Test     ########\n")
      cat("#############################################################\n")
      cat("                     H0: MSE.1 == MSE.2                      \n")
      cat("                     ",alternativeStr,"                       \n",sep="")
      #cat("")
      cat("F-statistic =                           ", Fstat, sep="", "\n")
      cat("Numerator DF =                          ", numerator.df, sep="", "\n")
      cat("Denominator DF =                        ", denominator.df, sep="", "\n")
      cat("Critical F-value =                      ", Fcrit, sep="", "\n")
      cat("P-value =                               ", p.value,sep="","\n")
      
      df <- data.frame(Fstat=Fstat, FCritical=Fcrit, PValue=p.value)
      return(invisible(df))
}









# Making the prediction equation, returning it as a function
# fit = the lm object
#predictionEquation <- function(fit) {
#      intercept = fit$coef[[1]]
#      slopes = as.numeric(fit$coef[-1])
#      
#      slopesSign = c() #if(slopes < 0) " - " else " + "
#      newSlopes = c()
#      for (i in 1:length(slopes)) {
#            if(slopes[i] < 0){
#                  slopesSign = c(slopesSign, " - ")
#                  newSlopes = c(newSlopes, abs(slopes[i]))
#            }
#            else{
#                  slopesSign = c(slopesSign, " + ")
#                  newSlopes = c(newSlopes, slopes[i])
#            }
#            
#      }
#      # Making the x-names x1 x2 x3 ... etc
#      xStrings <- names(fit$coefficients)[-1]
#      #numXs = length(fit$coefficients) - 1
#      #xStrings = c()
#      #for(i in 1:numXs){
#      #      xStrings = c(xStrings, paste("x", i, sep=""))
#      #}
#      numCoefs <- length(fit$model)
#      multiplySigns <- replicate(numCoefs, "*")
#      allCoefsWithVars <- paste(intercept, 
#                                paste(c(rbind(slopesSign, newSlopes, 
#                                              multiplySigns, xStrings)), 
#                                      collapse = ''), sep="")
#      
#      # args = find independent names first with 'stringr' package
#      tup <- separateArgs(fit)
#      args <- tup[[1]]
#      extraArgs <- tup[[2]]
#      ## TODO: body = detect types of patterns in the formula so I(RPM^2)
#      # means square of the arg1=RPM, and RPM:CPRATIO is RPM*CPRATIO
#      # Then put those variables in the formula. 
#      # PROCSS: 
#      # 1. get independent args
#      # 2. for each independent arg, check for I-terms and get the matching
#      # i-term( thing inside the Iterm must match the independent arg. Get
#      # the poly-power (hint - split at ^))
#      # 3. for each idnependent arg, if term is not I-term, then must be
#      # interaction term so split at ";" and create variable x1 * x2 to put 
#      # in formula. 
#      patterns <- c("\\^", "\\:")
#      bodyArgs <- c()
#      for (p in patterns){
#            testPattern <- str_detect(extraArgs, p)
#            # for this pattern, get the simple arg with this pattern
#            str_detect(args, p)
#            ## TODO FINSIH
#      }
#      
#      eval(parse(text = paste("yHat <- function(", args, ") { return(", 
#                 body, ") }", sep="")))
#      
#      return(eval(theFormula))
#}

# Finds the independent, untransformed arguments of the lm object
# So if we have I(RPM^2), it returns RPM
separateArgs <- function(fit){
      allNames <- names(fit$coefficients)[-1]
      patterns <- c("\\^", "\\:")
      extraArgs <- c()
      
      for (p in patterns) {
            # If no detecting of a pattern, then is INDEP ARG
            testExtra <- str_detect(allNames, p)
            extraArgs <- c(extraArgs, allNames[testExtra])
      }
      
      indicesNotExtraArgs <- which(!allNames %in% extraArgs)
      args <- allNames[indicesNotExtraArgs]
      return(list(args, extraArgs))
}





# OUTLIER POINTS = are those beyond (-2,2) of std residuals
outlier.outlierValues <- function(fit){
      library(car)
      srs = rstandard(fit)
      isOutlier <- srs < -2 | srs > 2
      print(outlierTest(fit)) # from car
      
      df <- data.frame(Fitted=fit$fitted.values, StandardizedResiduals=srs,
                       IsOutlier=isOutlier)
      
      return(invisible(df))
}

# INFLUENTIAL POINTS
# fit = the lm object
influence.leverageValues <- function(fit){
      hs <- hatvalues(fit)
      k <- length(fit$model) - 1
      n <- nrow(fit$model)
      h.mean <- (k+1)/n 
      h.cutoff <- 2*(k+1)/n 
      isInfluential <- hs > h.cutoff 
      return(data.frame(InfluentialPoints=hs, CutOff=h.cutoff, AvgLev=h.mean,
                        IsInfluential=isInfluential))
}

influence.cooksDistances <- function(fit) {
      cks <- cooks.distance(fit)
      k <- length(fit$model) - 1
      n <- nrow(fit$model)
      Fcrit <- qf(0.5, df1=k+1, df2=n-k-1)
      cks.fvalues <- pf(cks, df1=k+1, df2=n-k-1)
      
      # TODO: check if my method here cks > Fcrit is correct: 
      # or do we use cks.fvalues > Fcrit???
      isInfluential <- cks > Fcrit | cks >= 1
      # https://newonlinecourses.science.psu.edu/stat501/node/340/ 
      # An alternative method for interpreting Cook's distance that is sometimes 
      # used is to relate the measure to the F(p, n–p) distribution and to find the 
      # corresponding percentile value. If this percentile is less than about
      # 10 or 20 percent, then the case has little apparent influence on the
      # fitted values. On the other hand, if it is near 50 percent or even higher, 
      # then the case has a major influence. (Anything "in between" is more ambiguous.)

      return(data.frame(CooksPoints=cks, CooksFValues=cks.fvalues, CutOffFcrit=Fcrit,
                        IsInfluential=isInfluential))
}













# NOTE
# pi.hat.Ho <- nullMod$fitted.values
#pi.hat.Ha <- altMod$fitted.values
#y <- placekick$good
# ******
# likRatio <- -2 * sum(y * log(pi.hat.Ho/pi.hat.Ha) + (1-y)*log((1-pi.hat.Ho)/(1-pi.hat.Ha)))


# NOTE: null model deviance is bigger (assuming) so the statistic
# will be negative if not. 
# Equivalent to nested F-test between two models here, like anova(null, alt)
# Tests missing parameter significance. 
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


# Likelihood Ratio Test for the GLM Model

# Null hypothesis is that expected mu and observed ys are the same
# If low p-value then they are not.
# TESTS: the difference between expected successes with observed ones, and the
# expected failures with observed ones. (global F-test)
# Equivalent to global F-test, tests overall model fit. 
ResidualDevianceTest <- function(fit, printNice=TRUE) { 
      # residualdeviance has chi-square distribution on n - k degrees freedom. 
      df <- fit$df.residual # always n - k - 1, where k+1 = num params/coefs
      dev <- fit$deviance
      result <- data.frame(LikRatio=dev, df=df,
                           PValue= 1 - pchisq(dev, df=df))
      row.names(result) <- ""
      
      
      
      # Ho: deviance = 0 p = big fail reject H0 = good model fit = deviance small
      statement <- ""
      if(result$PValue < 0.05){
            # testing if G = 0
            # ni <- gen$total; yi <- gen$male; mui <- ni *ratio.glm$fitted.values
            # G <- 2*sum( yi*log(yi/mui) + (ni-yi)*log((ni-yi)/(ni-mui)) )
            statement <- 
                  paste("Reject H0. Conclude the residual deviance is large and\n",
                        "different from zero. So expected successes are not equal to\n",
                        "observed successes and expected failures are not equal to \n",
                        "observed failures. The model is not a good fit for the data.", sep="")
      } else{
            statement <- 
                  paste("Fail to reject H0. Not enough evidence to conclude that the\n",
                        "residual deviance is not 0, so we say the residual deviance is small.\n",
                        "So expected and observed successes/failures are similar. \n",
                        "Thus the model is a good fit for the data.", sep="")
      }
      
      # TODO: update null hypothesis, is not the same of the nested test H0. 
      if(printNice){
            yName <- names(fit$model)[1]
            form = as.formula(paste(yName, " ~ 1", sep=""))
            nullModel <- glm(form, data=fit$model, family=fit$family)
            #likRatioPrintNice(result, nullModel, fit, statement)
            
            nf = formula(nullModel)
            na = formula(fit)
            cat("\n")
            cat("#####################################################################\n")
            cat("#######        Likelihood-Ratio Residual Deviance Test        #######\n")
            cat("#####################################################################\n")
            cat("\tH0: residual deviance ΔG = 0 \n") #; cat(paste(nf[[2]], nf[[1]], nf[3]))
            #cat("\n")
            cat("\tHA: residual deviance ΔG != 0\n\n") #; cat(paste(na[[2]], na[[1]], na[3]))
            cat("  ΔG:\t\t                                ", result$LikRatio, "\n")
            cat("  df:\t\t                                ", result$df, "\n")
            cat("  p-value:\t\t                        ", result$PValue, "\n\n")
            cat(statement)
            
            return(invisible(result))
      } else{
            return(result)                 
      }
}





#ResidualDevianceTest <- function(model){
#      # residualdeviance has chi-square distribution on n - k degrees freedom. 
#      df <- model$df.residual
#      dev <- deviance(model)
#      result <- data.frame(ResidualDeviance=dev, df=df,
#                       PValue= 1 - pchisq(dev, df=df))
#      row.names(result) <- ""
#      return(result)
#}

NullDevianceTest <- function(model){
      # null deviance has chi-square distribution on n - 1 degrees freedom. 
      df <- model$df.null
      dev <- model$null.deviance
      result <- data.frame(ResidualDeviance=dev, df=df,
                           PValue= 1 - pchisq(dev, df=df))
      row.names(result) <- ""
      return(result)
}


DevianceResiduals <- function(obsData, expData=NULL) {
      # doing chi-test just to get expected values. 
      if(!is.null(expData)) chi.test <- chisq.test(obsData, p=expData/sum(expData))
      else chi.test <- chisq.test(obsData)
      os <- obsData 
      es <- chi.test$exp 
      return(sign(os - es) * sqrt(abs(2 * os * log(os/es))))
}






# ---------------------- Row/Col Probability Estimates -----------------------------------------
rowProbabilityHat <- function(tbl){prop.table(tbl, margin=1)}
colProbabilityHat <- function(tbl){prop.table(tbl, margin=2)}

# ---------------------- Returns table with marginal row/col sums ------------------------------
################ TODO make for k-way tables too, not just two-way
marginalTable <- function(tbl) {
      t = tbl 
      dimnames(t) <- NULL 
      t <- cbind(t, margin.table(t, margin=1))
      t <- rbind(t, margin.table(t, margin=2))
      
      dms <- dimnames(tbl)
      dms[[1]] <- c(dms[[1]], "") # c(dms[[1]], "ColTotals")
      dms[[2]] <- c(dms[[2]], "")
      dimnames(t) <- dms 
      
      return(t)
}


# ----------------------------------------------------------------------------------------------
#################################################################################
########            Measures of Association for 1,2-way Tables           ########
#################################################################################

# odds ratio, relative risk, diff of proportions test independence of two variables
# in the two-way table. 



# ----------------------          Odds Ratio          ----------------------------

#################################### TODO make it work with ftable
#################################### problem because no names in ftable

#  odds ratio for n x m table (two-way) is the same anyway you place the coeffs
# Returns odds ratio for all combinations of pairings between variable levels. 
# CAN TEST WITH THIS DATA: 
#eyeTable <- as.table(matrix(c(20,30,10,15,10,85,25,15,12,20,10,82,45,45,22,35,20,167),byrow = TRUE, nrow=3))
#rownames(eyeTable) <- c("Female", "Male", "Total")
#colnames(eyeTable) <- c("Black", "Brown", "Blue", "Green", "Gray", "Total")
#oddsRatio(eyeTable)

colOdds <- function(tbl) {
      
      if(length(dim(tbl)) > 2){
            cat("Error: Needs to be 2-dimensional table")
            return(invisible(NULL))
      }
      
      ### make names combos
      combosColnames <- combn(colnames(tbl), m=2)
      combosColnames.forward <- paste(combosColnames[1,], "/", combosColnames[2,], sep="")
      combosColnames.backward <- paste(combosColnames[2,], "/", combosColnames[1,], sep="")
      
      ### make odds for each col, holding rows constant
      combosCol <- lapply(1:nrow(tbl), function(i) combn(tbl[i,], m=2))
      cOdds.forward <- lapply(1:nrow(tbl), function(i){combosCol[[i]][1, ] / combosCol[[i]][2, ]})
      cOdds.backward <- lapply(1:nrow(tbl), function(i){combosCol[[i]][2, ] / combosCol[[i]][1, ]})
      # adjusting
      library(plyr)
      cOdds.forward <- ldply(cOdds.forward)
      cOdds.backward <- ldply(cOdds.backward)
      #naming 
      colnames(cOdds.forward) <- combosColnames.forward
      rownames(cOdds.forward) <- rownames(tbl)
      colnames(cOdds.backward) <- combosColnames.backward
      rownames(cOdds.backward) <- rownames(tbl)
      
      return(cbind(cOdds.forward, cOdds.backward))
}

rowOdds <- function(tbl){
      
      if(length(dim(tbl)) > 2){
            cat("Error: Needs to be 2-dimensional table")
            return(invisible(NULL))
      }
      
      ### make names combos
      combosRownames <- combn(rownames(tbl), m=2)
      combosRownames.forward <- paste(combosRownames[1, ], "/", combosRownames[2,], sep="")
      combosRownames.backward <- paste(combosRownames[2, ], "/", combosRownames[1,], sep="")
      
      ### make odds for each col, holding rows constant
      combosRow <- lapply(1:ncol(tbl), function(i) combn(tbl[,i], m=2))
      rOdds.forward <- lapply(1:ncol(tbl), function(i){combosRow[[i]][1, ] / combosRow[[i]][2, ]})
      rOdds.backward <- lapply(1:ncol(tbl), function(i){combosRow[[i]][2, ] / combosRow[[i]][1, ]})
      # adjusting a bit
      library(plyr)
      rOdds.forward <- t(ldply(rOdds.forward))
      rOdds.backward <- t(ldply(rOdds.backward))
      
      #naming 
      colnames(rOdds.forward) <- colnames(tbl)
      rownames(rOdds.forward) <- combosRownames.forward
      colnames(rOdds.backward) <- colnames(tbl)
      rownames(rOdds.backward) <- combosRownames.backward
      
      return(rbind(rOdds.forward, rOdds.backward))
}

oddsRatio <- function(tbl) {
      if(length(dim(tbl)) > 2){
            cat("Error: Needs to be 2-dimensional table")
            return(invisible(NULL))
      }
      
      ### make names combos
      combosColnames <- combn(colnames(tbl), m=2)
      combosColnames <- paste(combosColnames[1,], "/", combosColnames[2,], sep="")
      combosRownames <- combn(rownames(tbl), m=2)
      combosRownames <- paste(combosRownames[1, ], "/", combosRownames[2,], sep="")
      ### make odds for each col, holding rows constant
      combosCol <- lapply(1:nrow(tbl), function(i) combn(tbl[i,], m=2))
      cOdds <- lapply(1:nrow(tbl), function(i){combosCol[[i]][1, ] / combosCol[[i]][2, ]})
      
      library(plyr)
      cOdds <- ldply(cOdds)
      colnames(cOdds) <- combosColnames
      rownames(cOdds) <- rownames(tbl)
      
      oddsPairs <- lapply(1:ncol(cOdds), function(i) combn(cOdds[,i], m=2))
      
      oddsRatios <- lapply(1:ncol(cOdds), function(i) {oddsPairs[[i]][1,] / oddsPairs[[i]][2,]})
      oddsRatios <- t(ldply(oddsRatios))
      colnames(oddsRatios) <- combosColnames
      rownames(oddsRatios) <- combosRownames
      
      return(oddsRatios)
}






# TODO: normality subset tests for the completely randomized design # and rest
# factorials etc... within each population, do shapiro wilk normality test
# and report p-values in a table. Also graph densities like in the
# Leaf data set (logistic)

# TODO: equal variance subset tests for all the randomized/block/factorial
# designs (levens and bartlet) and report p-vals in table. 



# -------------------------------------------------------------------------------
bootstrapMean <- function(data, R, level=0.95){
      # Get n samples from the known population distribution. (this is the data given)
      n = length(data) # observed data, original data
      
      # Sample from F-hat (instead of rexp) to sample with replacement from F-hat
      H.star = rep(NA, R)
      for(i in 1:R){
            # resample from original data with replacement. 
            dataStar = sample(data, size=n, replace=TRUE) #this is the bootstrap sample
            H.star[i] = mean(dataStar)
      }
      
      # MEAN(H*) should be close to MU_XBAR should be close to MU
      # SD(H*) should be close to SD_XBAR should be close to  S/SQRT(N)
      cat("boot.mu_xbar (hstar) = ", mean(H.star), "\n", sep="")
      cat("xbar (mean of data) = ", mean(data), "\n\n", sep="")
      
      B.hat = mean(H.star) - mean(data) # estimated bias
      cat("B.hat = mean(hstar) - mean(data) = ", B.hat, "\n", sep="")
      mu.hat.B = mean(data) - B.hat  # bias-corrected version of mu
      cat("theta.hat.B = mean(data) - bias = ", mu.hat.B, "\n\n",sep="")
      
      se.mean = sd(H.star) # standard error of the mean
      cat("boot.se.mean (hstar) = ", se.mean, "\n", sep="") # call this se.mean
      #print(sqrt(var(H.star))) # same as sd(H.star)
      cat("se.mean = sigma/sqrtn(n) = ", theta/sqrt(n), "\n", sep="")
      cat("se.mean = s / sqrt(n) = ", sd(data)/sqrt(n), "\n\n", sep="")
      
      # 95 % confidence interval methods. 
      # calculate critical value first
      lower = abs( (1-level)/2)
      upper = lower + level 
      z.crit = abs(qnorm(lower))
      
      
      cat("CI by normal approximation: ")
      cat("(",mu.hat.B - z.crit*se.mean,", ",mu.hat.B + z.crit*se.mean,")","\n",sep="")
      
      cat("CI by percentile method: ")
      qs = quantile(H.star, c(lower, upper))
      cat("(", qs[1],", ", qs[2],")", "\n", sep="") 
      
      cat("CI by basic method:", "")
      cat("(", 2*xbar - qs[2],", ", 2*xbar - qs[1],")", "\n", sep="")
      
      return(invisible(H.star))
}



# STAT330 Learning -----------------------------------------------------------------------

# GOAL: fit lda model but attach also the observed data in a separate column. 
# ldaFit <- function(formula())
# TODO: how to get formula in arguments
#ldaFit <- function(response, predictors, data){
#      library(MASS)
#      formulaToFit <- reformulate(response=response, termlables=predictors)
#      lda.fit <- lda(formulaToFit, data=data)
#      return (lda.fit)
#}

# lda.fit = the model fit by lda (MASS package)
# GOAL: converts output of lda.fit (list) to data frame to look nicer
ldaToDataFrame <- function(lda.fit){
      pred.list <- predict(lda.fit)
      # note: posterior in pred.list may be multidim array? depending on num classes
      # so that changes num cols of this df ??
      ldaValues <- pred.list$x
      colnames(ldaValues) <- "LDAClassifier"
      pred.df <- data.frame(PredictedClass = pred.list$class, ldaValues,
                            Posterior=pred.list$posterior)
      # note: LDA classifier == linear discriminant fits
      return(pred.df)
}


# observedY = original observed y-values (categorical)
# lda.fit = the model fit by lda (MASS package)
# GOAL: calculate lda error rate, which is estimation of Bayes error rate. 
ldaErrorRate <- function(lda.fit, observedY) {
      return(1 - mean(predict(lda.fit)$class == observedY))
}


# GOAL: calculate confusion matrix of the lda fit
# observedY = vector of observed y values (categorical)
# lda.fit = model fit by lda
# link to another type of confusion matrix: 
# https://maths-people.anu.edu.au/~johnm/courses/mathdm/2008/pdf/r-exercisesVI.pdf
twoWayConfusionMatrix <- function(lda.fit, observedY){
      df = data.frame(PredictedResponse=predict(lda.fit)$class, 
                       ObservedResponse=observedY)
      
      # note: returning transpose so positions of sensitivity and specificity 
      # match my notes
      tbl = marginalTable(table(df))
      
      # rename "ColTotals" to total population counts per class
      rownames(tbl)[3] <- "TotalPopulationCounts"
      colnames(tbl)[3] <- "TotalPredictedCounts"
      
      # calculate sensitivity (true positive) = lower right corner rate: TP / P
      # TODO: apply to multivariate case
      # calculate specificity (true negative) = upper left corner rate: TN / N
      
      return(tbl)
}
