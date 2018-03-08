


interpret.SlopeCoeffs <- function(fit, level=0.95, x.units=c(), y.unit="unit") {
      
      # Making units and including the y.unit -------------------------
      if(length(x.units) == 0){
            x.units <- list()
            x.units <- replicate(length(fit$coeff)-1, "unit")
      }
      all.units <- c(y.unit, x.units)
      
      # Making the data.frame of information
      df <- data.frame(cbind(names(fit$model), 
                             round(fit$coeff,4), 
                             all.units), stringsAsFactors = FALSE)
      colnames(df) <- c("Names", "Coefficients", "Units")
      df$Coefficients <- as.numeric(df$Coefficients)
      
      # Interpretations --------------------------------------------------
      # Making interpretation for intercept 
      intrp.B0 <- paste("We are ", level*100, "% confident that when all predictors are ", 
                        "set to 0, the mean value of ",
                        df$Names[1], " is ", df$Coef[1], " ", df$Units[1],".", sep="") 
      # Now making interpretations for all other predictors
      allSlopeInterps <- c(intrp.B0)
      
      for(i in 2:nrow(df)){
            change <- if(df$Coefficients[i] < 0) "decreases" else "increases"
            newCoef <- abs(df$Coefficients[i])
            
            newInterp <- paste("We are ", level*100, "% confident that when all other ",
                               "predictors are held constant,",
                               " for every 1 ", df$Units[i], " increase in ",
                               df$Names[i], " (x", i,"), mean ", 
                               df$Names[1], " ", change, " by ", newCoef, 
                               " ", df$Units[1], ".", sep=""
            )
            # Accumulate the interpretation
            allSlopeInterps <- c(allSlopeInterps, newInterp)
      }
      df$Interpretations <- allSlopeInterps
      
      simpleDf <- data.frame(PredictorNames=names(fit$coef), 
                             SlopeCoefficients=as.numeric(fit$coef),
                             INTERPRETATIONS=df$Interpretations)
      
      return(simpleDf)
}


interpret.SlopeCI <- function(fit, level =0.95,
                              x.units=c(), y.unit="unit")  {
      
      # Making units and including the y.unit -------------------------
      if(length(x.units) == 0){
            x.units <- list()
            x.units <- replicate(length(fit$coeff)-1, "unit")
      }
      all.units <- c(y.unit, x.units)
      
      
      slope.cis <- round(slopeCI(fit, level=level), 4)
      
      # Making the data.frame of information
      df <- data.frame(cbind(names(fit$coef), 
                             names(fit$model), 
                             all.units), stringsAsFactors = FALSE)
      colnames(df) <- c("PredictorNames", "AllNames", "Units")
      df <- cbind(df, slope.cis)
      
      # Interpretations --------------------------------------------------
      interp.B0 <- paste("With ",level*100,"% confidence, when all predictors are zero, ",
                          "the mean ", df$AllNames[1], " is between ", df$`2.5 %`[1], " and ",
                         df$`97.5 %`[1], " ", df$Units[1], ".", sep="")
      allInterps <- c(interp.B0)
      
      for(i in 2:nrow(df)){
            
            change <- if(df$`2.5 %`[i] < 0 && df$`97.5 %`[i] < 0) "decreases" else "increases"
            newLower <- if(df$`2.5 %`[i] < 0 && df$`97.5 %`[i] < 0) 
                  abs(df$`2.5 %`[i])  else df$`2.5 %`[i]            
            newUpper <- if(df$`2.5 %`[i] < 0 && df$`97.5 %`[i] < 0) 
                  abs(df$`97.5 %`[i]) else df$`97.5 %`[i]
            
            newInterp <- paste("With ",level*100,"% confidence, when all other predictors ",
                               "are held fixed, for every 1 ", df$Units[i], " increase in (x",
                               i, ") ", df$AllNames[i], ", the mean response (y) ", 
                               df$AllNames[1], " ", change, " between ", newLower, " and ",
                               newUpper, " ", df$Units[1], ".", sep="")
            
            allInterps <- c(allInterps, newInterp)
      }
      df$INTERPRETATION <- allInterps
      
      simpleDf <- data.frame(cbind(df$PredictorNames, df$`2.5 %`, df$`97.5 %`,
                                   df$INTERPRETATION))
      colnames(simpleDf) <- c("PredictorNames", colnames(df)[4],colnames(df)[5], 
                              "INTERPRETATION")
      
      return(simpleDf)
}


# TODO do rest for multiple regression
interpret.MeanCI <- function(fit, x.values=c(), x.units=c(), y.unit="unit", level=0.95){
      
      # Making units and including the y.unit -------------------------
      if(length(x.units) == 0){
            x.units <- list()
            x.units <- replicate(length(fit$coeff)-1, "unit")
      }
      # Making the mean CI's
      mat <- round(meanCI(fit, x.values=x.values, level=level), 4)
      mean.ci <- data.frame(mat)
      yName <- names(fit$model)[1]
      yNameValue <- paste(yName, "Value", sep="")
      lowerName <- paste((1-level)/2 * 100, "%")
      upperName <- paste(((1-level)/2 + level)*100, "%")
      colnames(mean.ci) <- c(yNameValue, lowerName, upperName)
      
      
      # Building the values in the df. 
      xdf <- data.frame(rbind(x.values))
      colnames(xdf) <- names(fit$coefficients)[-1]
      df <- cbind(xdf, mean.ci)
      rownames(df) <- ""
      
      # Building the x-value sentence to explain y(xp)
      xValSentence <- ""
      for(i in 1:ncol(xdf)){
            xValSentence <- paste(xValSentence, ", ", names(xdf)[i], "=", xdf[[i]], 
                                  " ", x.units[i], sep="")
      }
      xValSentence <- substr(xValSentence, 3, nchar(xValSentence))

      interp <- paste("We are ",level*100, "% confident that the mean ",yName," (E(y)) ",
          "for all xs at the values: ", xValSentence, ", is between ",
          df$`2.5 %`, " and ", df$`97.5 %`, " ", y.unit, ".", sep="")
            
      simpleDf <- cbind(df, INTERPRETATION=interp)
      rownames(simpleDf) <- ""
      
      return(simpleDf)
}

interpret.PredictCI <- function(fit, x.values=c(), x.units=c(), y.unit="unit", level=0.95){
      
      # Making units and including the y.unit -------------------------
      if(length(x.units) == 0){
            x.units <- list()
            x.units <- replicate(length(fit$coeff)-1, "unit")
      }
      # Making the mean CI's
      mat <- round(predictCI(fit, x.values=x.values, level=level), 4)
      predict.ci <- data.frame(mat)
      yName <- names(fit$model)[1]
      yNameValue <- paste(yName, "Value", sep="")
      lowerName <- paste((1-level)/2 * 100, "%")
      upperName <- paste(((1-level)/2 + level)*100, "%")
      colnames(predict.ci) <- c(yNameValue, lowerName, upperName)
      
      
      # Building the values in the df. 
      xdf <- data.frame(rbind(x.values))
      colnames(xdf) <- names(fit$coefficients)[-1]
      df <- cbind(xdf, predict.ci)
      rownames(df) <- ""
      
      # Building the x-value sentence to explain y(xp)
      xValSentence <- ""
      for(i in 1:ncol(xdf)){
            xValSentence <- paste(xValSentence, ", ", names(xdf)[i], "=", xdf[[i]], 
                                  " ", x.units[i], sep="")
      }
      xValSentence <- substr(xValSentence, 3, nchar(xValSentence))
      
      interp <- paste("We are ",level*100, "% confident that the single future predicted ",
                      yName," for all xs at the values: ", xValSentence,", will be between ", 
                      df$`2.5 %`, " and ", df$`97.5 %`, " ", y.unit, ".", sep="")
      
      simpleDf <- cbind(df, INTERPRETATION=interp)
      rownames(simpleDf) <- ""
      
      return(simpleDf)
}