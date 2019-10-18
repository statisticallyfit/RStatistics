# Creating evenly spaced ggplot colors from the hcl color wheel: 
# g = number of groups or colors you need
ggplotColors <- function(g){
      d <- 360/g
      h <- cumsum(c(15, rep(d,g - 1)))
      hcl(h = h, c = 100, l = 65)
}
# SOURCE: https://data.library.virginia.edu/setting-up-color-palettes-in-r/

# -----------------------------------------------------------------------------------------

# Interaction plot:
interactionPlot <- function(x.factor, trace.factor, response, data){
      #detach(package:plyr)
      suppressMessages(require(dplyr))
      #summ <- ddply(drinkData, .(weeks, treat), summarise, wt=mean(wt))
      #cs = colnames(summ)
      #cs[3] = response # placing string value here
      #colnames(summ) = cs
      
      meanFunction = paste0('mean(', response, ')'); 
      meanName = response # paste0('mean_', response)
      
      mysumm = suppressWarnings(data %>% 
                                      group_by_(.dots = list(x.factor, trace.factor)) %>% 
                                      summarise_(.dots = setNames(meanFunction, meanName)))
      
      mysumm <- data.frame(mysumm)
      
      ggplot(data, aes_string(x=x.factor, y=response, color=trace.factor)) + 
            geom_point(data=mysumm, aes_string(group=trace.factor, color=trace.factor), 
                       size=2) + 
            geom_line(data=mysumm, aes_string(group=trace.factor), size=1)
}


# -----------------------------------------------------------------------------------------

ResidualDevianceTest <- function(fit, printNice=TRUE) { 
   # residualdeviance has chi-square distribution on n - k - 1 degrees freedom. 
   df <- fit$df.residual # always n - k - 1, where k+1 = num params/coefs
   res.dev <- fit$deviance
   result <- data.frame(LikRatio=res.dev, df=df,
                        PValue= 1 - pchisq(res.dev, df=df))
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
      cat("\tH0: residual deviance G = 0 \n") #; cat(paste(nf[[2]], nf[[1]], nf[3]))
      #cat("\n")
      cat("\tHA: residual deviance G != 0\n\n") #; cat(paste(na[[2]], na[[1]], na[3]))
      cat("  G:\t\t                                ", result$LikRatio, "\n")
      cat("  df:\t\t                                ", result$df, "\n")
      cat("  p-value:\t\t                        ", result$PValue, "\n\n")
      cat(statement)
      
      return(invisible(result))
   } else{
      return(result)                 
   }
}



# -----------------------------------------------------------------------------------------

influence.cooksDistances <- function(fit) {
   cks <- cooks.distance(fit)
   p <- length(fit$coef) # p = number of parameters
   n <- nrow(fit$model)
   # Cooks distance Di follows an F-distribution: Di ~ F(df1=p, df2=n-p)
   Fcrit <- qf(0.5, df1=p, df2=n-p)
   cks.fprob <- pf(cks, df1=p, df2=n-p)
   
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
   
   return(data.frame(CooksPoints=cks, FcritValue=Fcrit,
                     CooksFProb=cks.fprob, FcritProb=rep(0.5, n),
                     IsInfluential=isInfluential))
}
