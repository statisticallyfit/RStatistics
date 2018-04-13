source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')
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

SST <- function(fit) {
      y <- fit$model[[1]]
      y.mean <- mean(y)
      return ( sum( (y - y.mean)^2 ) )
}

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
      df <- data.frame(t(x.values))
      rownames(df) <- ""
      colnames(df) <- predictorNames
      
      return(predict(fit, newdata = df, interval="confidence", level=level))
}

predictCI <- function(fit, x.values, level=0.95){
      predictorNames <- names(fit$model)[-1]
      df <- data.frame(rbind(x.values))
      rownames(df) <- ""
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
NestedFTest <- function(r.lm, u.lm){
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
      
      cat("\n")
      cat("#############################################################\n")
      cat("####                 Analysis Of Variance                 ###\n")
      cat("####    (F Test and Chi-Square Test for Nested Models)    ###\n")
      cat("#############################################################\n")
      cat("\n")
      #cat("χ2 statistic:                        ", chi.stat, "\n")
      #cat("p-value:                             ", chi.p.value, "\n")
      cat("F statistic =                         ", F.nested, "\n")
      cat("p-value =                             ", f.p.value, "\n")
      cat("numerator df =                        ", df1, "\n")
      cat("denominator df =                      ", df2, "\n\n")
      return(invisible(data.frame(ChiSquare=chi.stat, ChiSqPValue=chi.p.value,
                                  df.chi = df1,
                                  FStatistic=F.nested, FPvalue=f.p.value, 
                                  df1=df1, df2=df2)))
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







# INFLUENTIAL POINTS
# fit = the lm object
influence.leverageValues <- function(fit){
      hs <- hatvalues(fit)
      k <- length(fit$model) - 1
      n <- nrow(fit$model)
      h.mean <- 2*(k+1)/n 
      isInfluential <- hs > h.mean 
      return(data.frame(InfluentialPoints=hs, CutOffInflMean=h.mean, 
                        IsInfluential=isInfluential))
}

influence.cooksDistances <- function(fit) {
      cks <- cooks.distance(fit)
      k <- length(fit$model) - 1
      n <- nrow(fit$model)
      Fcrit <- qf(0.5, df1=k+1, df2=n-k-1)
      isInfluential <- cks > Fcrit 
      return(data.frame(CooksPoints=cks, CutOffFcrit=Fcrit,
                        IsInfluential=isInfluential))
}