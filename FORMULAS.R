source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/Util.R')
       
library(DescTools)


## testing tbl
tbl <- matrix(c(12,34,58,44,10,11,31,88,89,17,15,10,43,55,50,13, 70, 23, 30, 12), 
              ncol=4, byrow=TRUE, dimnames=list(
                    Health=c("Unsatisfactory","Medium", "Good", 
                             "Very good", "Excellent"),
                    Treatment=c("Placebo", "VitaminB12", "Manganese", "VitaminC")))

# for testing concordant pairs
job <- matrix(c(1,3,10,6, 2,3,10,7, 1,6,14,12, 0,1,9,11),
              ncol=4, byrow = TRUE, dimnames=list(
                    Income=c("<15,000", "15,000-25,000", "25,000-40,000", ">40,000"),
                    JobSatisfaction=c("VeryDis", "LittleDis","ModeratelySat", "VerySat")
              ))

# agresti page 80 pdf
religion <- matrix(c(178,138,108, 570,648,442, 138,252,252), ncol=3, byrow = TRUE,
                   dimnames=list(
                         HighestDegree=c("< highschool", "highschool/juniorcollege", 
                                         "bachelor/graduate"),
                         ReligiousBeliefs=c("Fundamentalist","Moderate","Liberal")
                   ))



# Get the coefficients of a model
coef <- function(model){
      return(summary(model)$coef)
}



# Given restricted and unrestricted models, 
# do joint F-test but give Chi square instead of F
# J = number of restrictions
####################################### TODO: check out EtaSq from DescTools
######################################## Use Anova() from car package.
ANOVA <- function(r.lm, u.lm, J){
      F.stat.list <- anova(r.lm, u.lm)$F
      F.stat <- F.stat.list[length(F.stat.list)]
      chi.stat <- F.stat * J
      p.value <- 1 - pchisq(chi.stat, df=J) 
      cat("\n")
      cat("##################################################\n")
      cat("######        Analysis Of Variance         #######\n")
      cat("##################################################\n")
      cat("\n")
      cat("χ2 statistic:                        ", chi.stat, "\n")
      cat("F statistic:                          ", F.stat, "\n")
      cat("p-value:                              ", p.value, "\n\n")
      return(invisible(data.frame(ChiSquare=chi.stat, FStatistic=F.stat, PValue=p.value)))
}

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
SSE <- function(y, yhat) {
      return(sum( (y - yhat)^2 ))
}
# OR
#a <- anova(lm)
#ss <- a$`Sum Sq`
#return(ss[length(ss)])
# OR
#s <- summary(r.lm)
#(1-s$r.squared)*SST(var$datamat$dc)

SST <- function(y) {
      return(sum( (y - mean(y))^2 ))
}

# -----------------------------------------------------------------------------------------------
##################################################################################
########            χ2, G2 (goodness of fit) for 1,2-way Tables           ########
##################################################################################



################################## TODO: for one-way and three-way tables
LikelihoodRatioTableTest <- function(obsData, expData=NULL, printNice=TRUE) {
      # if one-way table ..
      if(is.vector(obsData)) {
            if(is.null(expData)) expData <- chisq.test(obsData)$expected 
            df <- length(obsData) - 1
            tableType <- " (one-way table) "
      } else if (length(dim(obsData)) == 2){ # if two-way table ... (### distinguish for when 3-way table)
            if(is.null(expData)) expData <- chisq.test(obsData)$expected 
            numRows <- dim(obsData)[1]
            numCols <- dim(obsData)[2]
            df <- (numRows - 1) * (numCols - 1)
            tableType <- " (two-way table) "
      } else if(length(dim(obsData)) == 3) { # else it is k-way table
            A <- margin.table(obsData, 1)
            B <- margin.table(obsData, 2)
            C <- margin.table(obsData, 3)
            
            expData <- (A %o% B %o% C) / sum(obsData)^2
            df <- (length(obsData)-1) - (sum(dim(obsData))-3)
            
            ##################### HELP TODO is this the right G2 for k-way tables?    
            #expData <- margin.table(obsData, 1) #outer(A, B)
            #for(i in 2:length(dim(obsData))){
            #      B <- margin.table(obsData, i)
            #      expData <- outer(expData, B)
            #}
            #df <- (length(obsData)-1) - (sum(dim(obsData))-3)
            tableType <- "(three-way table)"
      }
      
      G2 <- 2 * sum(obsData * log(obsData/expData))
      p.value <- 1 - pchisq(G2, df)
      
      ###################### TODO add resids, and stdres
      result <- list(LikRatio=G2, DF=df, PValue=p.value, Expected=expData)
      
      if(printNice){
            cat("\n")
            cat("######################################################################\n")
            cat("##########              Likelihood-Ratio Test              ###########\n")
            cat("##########               ", tableType, "                 ###########\n", sep="")
            cat("######################################################################\n")
            cat("\tH0: independence model fits the given data\n")
            ################# TODO clarify HA
            cat("\tHA: saturated model (observed data) fits the given data\n\n")
            cat("G^2 statistic:\t\t                             ", G2, "\n")
            cat("df:\t\t                                     ", df, "\n")
            cat("p-value:\t\t                             ", p.value, "\n\n")
            return(invisible(result))
      }
      return(result)
}




# NOTE
# pi.hat.Ho <- nullMod$fitted.values
#pi.hat.Ha <- altMod$fitted.values
#y <- placekick$good
# ******
# likRatio <- -2 * sum(y * log(pi.hat.Ho/pi.hat.Ha) + (1-y)*log((1-pi.hat.Ho)/(1-pi.hat.Ha)))

LikelihoodRatioModelTest <- function(nullModel, altModel, printNice=TRUE) {
      df <- nullModel$df.residual - altModel$df.residual; df 
      likRatioStat <- nullModel$deviance - altModel$deviance; likRatioStat
      pValue <- 1 - pchisq(likRatioStat, df); pValue
      result <- round(matrix(c(nullModel$dev, altModel$dev, nullModel$df.resid, 
                               altModel$df.resid, df, likRatioStat, pValue), 
                             ncol=1, byrow = TRUE), 5)
      colnames(result) <- ""
      rownames(result) <- c("Deviance (Ho)", "Deviance (Ha)", "DF (Ho)", "Df (Ha)", "Df",
                             "LikRatio", "PValue")
      
      if(printNice){
            cat("\n")
            cat("#####################################################################\n")
            cat("#######                 Likelihood-Ratio Test                 #######\n")
            cat("#####################################################################\n")
            cat("\tH0: reduced model is true = "); print(nullModel$formula) #; cat("\n")
            cat("\tHA: current model is true = "); print(altModel$formula); cat("\n")
            cat("ΔG^2:\t\t                                     ", likRatioStat, "\n")
            cat("df:\t\t                                     ", df, "\n")
            cat("p-value:\t\t                             ", pValue, "\n\n")
            return(invisible(result))
      }
      return(result)
}


# deviance --> in stats package, returns residual deviance of fitted model object. 
ResidualDeviance <- function(model){
      df <- data.frame(ResidualDeviance=deviance(model), Df=model$df.resid)
      row.names(df) <- ""
      return(df)
}

NullDeviance <- function(model){
      df <- data.frame(NullDeviance=model$null.deviance, Df=model$df.null)
      row.names(df) <- ""
      return(df)
}


devianceResiduals <- function(obsData, expData=NULL) {
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
      extraRow <- cbind(tbl, RowTotals=margin.table(tbl, margin=1))
      extraCol <- rbind(extraRow, ColTotals=margin.table(extraRow, margin=2))
      
      dms <- dimnames(tbl)
      dms[[1]] <- c(dms[[1]], "ColTotals")
      dms[[2]] <- c(dms[[2]], "RowTotals")
      dimnames(extraCol) <- dms 
      
      return(extraCol) 
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
oddsRatio <- function(tbl) {
      
      ### make names combos
      combosColnames <- combn(colnames(tbl), m=2)
      combosColnames <- paste(combosColnames[1,], "/", combosColnames[2,], sep="")
      combosRownames <- combn(rownames(tbl), m=2)
      combosRownames <- paste(combosRownames[1, ], "/", combosRownames[2,], sep="")
      ### make odds for each col, holding rows constant
      combosCol <- lapply(1:nrow(tbl), function(i) combn(tbl[i,], m=2))
      oddsCol <- lapply(1:nrow(tbl), function(i){combosCol[[i]][1, ] / combosCol[[i]][2, ]})
      library(plyr)
      oddsCol <- ldply(oddsCol)
      colnames(oddsCol) <- combosColnames
      rownames(oddsCol) <- rownames(tbl)
      
      oddsPairs <- lapply(1:ncol(oddsCol), function(i) combn(oddsCol[,i], m=2))
      
      oddsRatios <- lapply(1:ncol(oddsCol), function(i) {oddsPairs[[i]][1,] / oddsPairs[[i]][2,]})
      oddsRatios <- t(ldply(oddsRatios))
      colnames(oddsRatios) <- combosColnames
      rownames(oddsRatios) <- combosRownames
      
      return(oddsRatios)
}


# hypothesis test for the odds ratio = 1 or not
############################################ TODO do calcs self to get actual OR estimate.
############################################ Weird error for big /small cell counts (workspace)
FisherExactTest <- function(tbl, alternative="two.sided", printNice=TRUE){
      fisher <- fisher.test(tbl, alternative = alternative)
      OR <- fisher$estimate # not there if not 2 x 2
      p.value <- fisher$p.value
      CI <- fisher$conf.int # not there if not 2x2
      
      ############################## HELP todo why in ?fisher.test does it say that
      ############################ the alternative  = "two etc" is only used in 2x2 case?
      sign <- ""
      if(alternative == "two.sided") sign <- "≠"
      else if(alternative == "less") sign <- "<"
      else sign <- ">"
      
      result <- data.frame()
      if(!is.null(OR) || !is.null(CI)){
            result <- data.frame(OddsRatioEstimate=OR, PValue=p.value)
      } 
      
      if(printNice){
            cat("\n")
            cat("##########################################################\n")
            cat("#####       Fisher's Exact Test for Count Data       #####\n")
            cat("##########################################################\n")
            cat("\t\tH0: ρ = 1 (true odds ratio = 1)\n")
            cat("\t\tHA: ρ ", sign, " 1", sep="")
            if(!is.null(OR)) cat("\nOddsRatio-hat:                       ", OR, sep="")
            cat("\np-value:                             ", p.value, sep="")
            if(!is.null(OR)) cat("\n95% confidence interval:             (",CI[1],", ",CI[2],")", 
                                 sep="")
            cat("")
            return(invisible(result))
      }
      return(result)
}

################################# TODO update to take all combos of odds ratios from
################################# my custom oddsRatio function
OddsRatioCI <- function(tbl, printNice=TRUE){
      library(vcd)
      or <- oddsratio(tbl, log=FALSE)
      lor <- oddsratio(tbl)
      or.df <- data.frame(or)
      lor.df <- data.frame(lor)
      
      verdict <- function(i){
            # H0's ρ = 1 is not in confidence interval - reject H0.
            # The two variables are dependent
            if (CI[i,1] > 1 || CI[i,2] < 1) { "Reject H0" }
            # H0's ρ = 1 is inside confidence interval - do not reject H0
            # The two variables are independent
            else { "Do not reject H0" }
      }
      
      CI <- exp(confint(lor)) # in order to use the lognormal (less skewed 
      # compared to normal because ratio is elimmed with log rule: mult => sum)
      CI <- as.data.frame(CI)
      CI$HypTestResult <- unlist(lapply(1:nrow(CI), verdict))
      
      if(printNice){
            cat("\n")
            cat("#########################################################################\n")
            cat("########            Confidence Interval for Odds Ratio           ########\n")
            cat("#########################################################################\n")
            cat("\t\tH0: ρ = 1  (each odds ratio is 1)\n")
            cat("\t\tHA: ρ ≠ 1  (not all odds ratios are 1)\n\n")
            print(or)
            cat("\nConfidence Interval(s):\n")
            print(CI)
            cat("\n")
            
            return(invisible(CI))
      }
      return(CI)
}

# ----------------------------------- Relative Risk -------------------------------------------

# for two-way table (n x m)  (because we use two cols in data.frame
relativeRisk <- function(tbl, printNice=TRUE) {
      rowRisks <- prop.table(tbl, margin=1)
      colRisks <- prop.table(tbl, margin=2)
      
      # make row rel risks by dividing across all rows per column
      # row rel risks belong along a col. 
      rowRelRisks <- c()
      rownamePairs <- combn(rownames(tbl), m=2)
      rownameCombos <- paste(rownamePairs[1,] , "/" , rownamePairs[2,],sep="")
      for(i in 1:ncol(tbl)){
            # taken m = 2 at a time. 
            alongColPairs <- combn(rowRisks[,i], m=2) 
            rowRelRisks_col_i <- alongColPairs[1,] / alongColPairs[2, ]
            rowRelRisks <- c(rowRelRisks, rowRelRisks_col_i)
      }
      rowRelRisks <- matrix(rowRelRisks, nrow=length(rownameCombos))
      rownames(rowRelRisks) <- rownameCombos
      colnames(rowRelRisks) <- colnames(tbl)
      
      
      # make col rel risks by dividing across all cols per (along) row
      # col rel risks belong along a row. 
      # Ex: For row 1, 0-199 / 200-219, and 0-199/220-259 ... 200-219/220-259 ... etc 
      colRelRisks <- c()
      colnamePairs <- combn(colnames(tbl), m=2)
      colnameCombos <- paste(colnamePairs[1,], "/", colnamePairs[2,], sep="")
      for(i in 1:nrow(tbl)){
            alongRowPairs <- combn(colRisks[i,], m=2)
            colRelRisks_row_i <- alongRowPairs[1,] / alongRowPairs[2,]
            colRelRisks <- c(colRelRisks, colRelRisks_row_i)
      }
      colRelRisks <- matrix(colRelRisks, ncol=length(colnameCombos))
      rownames(colRelRisks) <- rownames(tbl)
      colnames(colRelRisks) <- colnameCombos
      
      
      result  <- list(ColumnRelRisks=colRelRisks, RowRelRisks=rowRelRisks)
      if(printNice){
            cat("\n")
            cat("----------------------------------------------------------------\n")
            cat("|                   Column Relative Risks                      |\n")
            cat("----------------------------------------------------------------\n")
            print(colRelRisks)
            cat("\n")
            cat("----------------------------------------------------------------\n")
            cat("|                    Row Relative Risks                        |\n")
            cat("----------------------------------------------------------------\n")
            print(rowRelRisks)
            
            return(invisible(result))
      }
      return(result)
}



# get proportions constants for row rel risk or diff props.
getRowConstCombos <- function(tbl){
      numerColCombos <- lapply(1:ncol(tbl), function(i) combn(tbl[,i], m=2))
      denomColCombos <- combn(rowSums(tbl), m=2)
      propList <- lapply(1:ncol(tbl), function(i) 
            cbind(t(numerColCombos[[i]]), t(denomColCombos)))
      
      totalDf <- data.frame()
      for(i in 1:ncol(tbl)){
            df <- data.frame(propList[[i]])
            colnames(df) <- c("n1", "n2", "n1.marg", "n2.marg")
            totalDf <- rbind(totalDf, df)
      }
      return(totalDf)
}
# get proportion constants for col rel risk or diff props. 
getColConstCombos <- function(tbl){
      numerRowCombos <- lapply(1:nrow(tbl), function(i) combn(tbl[i,], m=2))
      denomRowCombos <- combn(colSums(tbl), m=2)
      propList <- lapply(1:nrow(tbl), function(i) 
            cbind(t(numerRowCombos[[i]]), t(denomRowCombos)))
      
      totalDf <- data.frame()
      for(i in 1:nrow(tbl)){
            df <- data.frame(propList[[i]])
            colnames(df) <- c("n1", "n2", "n1.marg", "n2.marg")
            totalDf <- rbind(totalDf, df)
      }
      return(totalDf)
}



RelativeRiskCI <- function(tbl, printNice=TRUE){
      
      # if row = true then we passed in rowProps, false if colProps
      # argument is result of getRowConstCombos .. etc functions
      makeCI <- function(constData, row=TRUE){
            n1 <- constData$n1
            n1.marg <- constData$n1.marg
            n2 <- constData$n2
            n2.marg <- constData$n2.marg 
            
            ρ.hat <- (n1/n1.marg)/(n2/n2.marg)
            var.log.hat <- 1/n1 - 1/n1.marg + 1/n2 - 1/n2.marg 
            var.hat <- exp(var.log.hat)
            lower <- exp(log(ρ.hat) - 1.96*sqrt(var.log.hat))
            upper <- exp(log(ρ.hat) + 1.96*sqrt(var.log.hat))
            
            result <- data.frame()
            if(row){
                  result <- data.frame(RowRelRisk=round(ρ.hat,5), lower2.5=round(lower,5),
                                       upper97.5=round(upper,5))
            }
            else{
                  result <- data.frame(ColRelRisk=round(ρ.hat,5), lower2.5=round(lower,5), 
                                       upper97.5=round(upper,5))
            }
            result 
      }
      
      colConsts <- getColConstCombos(tbl)
      rowConsts <- getRowConstCombos(tbl)
      result.row <- makeCI(rowConsts)
      result.col <- makeCI(colConsts, row=FALSE)
      result <- cbindPad(result.row, result.col)
      
      if(printNice){
            cat("\n")
            cat("#################################################################\n")
            cat("#######       Confidence Interval for Relative Risks      #######\n")
            cat("#################################################################\n")
            cat("\t\t     H0: ρ = 1\n")
            cat("\t\t     HA: ρ ≠ 1\n")
            cat("Confidence intervals:\n")
            print(result)
            cat("\n")
            return(invisible(result))
      }
      return(result)
}


# ----------------------        Difference of Proportions        ----------------------------
# for two-way table
# difference of proportions (between any two rows)
propDifferences <- function(tbl) {
      rowConsts <- getRowConstCombos(tbl)
      colConsts <- getColConstCombos(tbl)
      rd <- rowConsts$n1/rowConsts$n1.marg - rowConsts$n2/rowConsts$n2.marg
      cd <- colConsts$n1/colConsts$n1.marg - colConsts$n2/colConsts$n2.marg
      
      df <- cbindPad(data.frame(rd), data.frame(cd))
      diffProps <- data.frame(RowPropDiffs=df$rd, ColPropDiffs=df$cd)
      
      return(diffProps)
}

# two-way and (2x2) table | n1, n2 are cell counts, n1marg, n2marg = marginal sums 
# if colMarg = TRUE, props will be calculated column wise. If false, then row-wise.
# pr,pc,qr,qc = propOne row and col indices, and propTow row and col indices. 
TwoPropPooledTestTable <- function(tbl, printNice=TRUE) {
      
      # pooled proportion estimate
      r <- getRowConstCombos(tbl)
      c <- getColConstCombos(tbl)
      pp.col.hat <- (c$n1 + c$n2) / (c$n1.marg + c$n2.marg)
      pp.row.hat <- (r$n1 + r$n2) / (r$n1.marg + r$n2.marg)
      
      # var estimate
      var.row.hat <- pp.row.hat*(1-pp.row.hat)/r$n1.marg + pp.row.hat*(1-pp.row.hat)/r$n2.marg 
      var.col.hat <- pp.col.hat*(1-pp.col.hat)/c$n1.marg + pp.col.hat*(1-pp.col.hat)/c$n2.marg
      
      # critical stats, significance
      p <- propDifferences(tbl)
      rps <- p$RowPropDiffs; rps <- rps[!is.na(rps)]
      cps <- p$ColPropDiffs; cps <- cps[!is.na(cps)]
      z.row.stat <- rps/sqrt(var.row.hat)
      z.col.stat <- cps/sqrt(var.col.hat)
      z.crit <- qnorm(p=0.975) # std normal dist  
      p.value.row <- 1 - pnorm(z.row.stat)
      p.value.col <- 1 - pnorm(z.col.stat)
      
      # fancy presentation
      r1s <- paste(r$n1, r$n1.marg, sep="/")
      r2s <- paste(r$n2, r$n2.marg, sep="/")
      rs <- paste(r1s, r2s, sep=" - ")
      c1s <- paste(c$n1, c$n1.marg, sep="/")
      c2s <- paste(c$n2, c$n2.marg, sep="/")
      cs <- paste(c1s, c2s, sep=" - ")
      
      result <- cbindPad(data.frame(RowPropDiffs=rs, Result=round(rps,5), 
                                 PValue=round(p.value.row,3)), 
                         data.frame(ColPropDiffs=cs, Result=round(cps,5), 
                                 PValue=round(p.value.col,3)))
      
      if(printNice){
            cat("\n")
            cat("#########################################################################\n")
            cat("#####         Hypothesis Test for Difference of Proportions        ######\n")
            cat("#########################################################################\n")
            cat("\t\t\t     H0: δ = 0\n")
            cat("\t\t\t     HA: δ ≠ 0\n\n")
            print(result)
            cat("\n")
            return(invisible(result))
      }
      return(result)
}

 
TwoPropCITable <- function(tbl, printNice=TRUE){
      
      # row and col const combos
      r <- getRowConstCombos(tbl)
      c <- getColConstCombos(tbl)
      p <- propDifferences(tbl)
      rps <- p$RowPropDiffs; rps <- rps[!is.na(rps)]
      cps <- p$ColPropDiffs; cps <- cps[!is.na(cps)]
      
      # n1/n1.marg*(1-n1/n1.marg)/n1.marg + n2/n2.marg*(1-n2/n2.marg)/n2.marg
      var.row.hat <- r$n1/r$n1.marg * (1 - r$n1/r$n1.marg)/r$n1.marg + 
            r$n2/r$n2.marg * (1 - r$n2/r$n2.marg)/r$n2.marg
      var.col.hat <- c$n1/c$n1.marg * (1 - c$n1/c$n1.marg)/c$n1.marg + 
            c$n2/c$n2.marg * (1 - c$n2/c$n2.marg)/c$n2.marg
      z.crit <- qnorm(0.975)
      
      lower.row <- rps - z.crit*sqrt(var.row.hat) 
      upper.row <- rps + z.crit*sqrt(var.row.hat)
      lower.col <- cps - z.crit*sqrt(var.col.hat) 
      upper.col <- cps + z.crit*sqrt(var.col.hat)
      
      # fancy presentation
      r1s <- paste(r$n1, r$n1.marg, sep="/")
      r2s <- paste(r$n2, r$n2.marg, sep="/")
      rs <- paste(r1s, r2s, sep=" - ")
      c1s <- paste(c$n1, c$n1.marg, sep="/")
      c2s <- paste(c$n2, c$n2.marg, sep="/")
      cs <- paste(c1s, c2s, sep=" - ")
      
      result.row <- data.frame(RowPropDiff=rs, Result=round(rps,5), 
                                 lower2.5=round(lower.row, 5), upper97.5=round(upper.row,5))
      result.col <- data.frame(ColPropDiff=cs, Result=round(cps,5), 
                                 lower2.5=round(lower.col,5), upper97.5=round(upper.col,5))
      result <- cbindPad(result.row, result.col)
      
      if(printNice){
            cat("\n")
            cat("#############################################################\n")
            cat("###   Confidence Interval for Difference of Proportions   ###\n")
            cat("#############################################################\n")
            cat("\t\t      H0: δ = 0\n")
            cat("\t\t      HA: δ ≠ 0\n")
            cat("Confidence intervals:\n")
            print(result.row)
            cat("\n")
            print(result.col)
            cat("\n")
            return(invisible(result))
      }
      return(result)
}



# ------------------------------------   Correlations   -----------------------------------


# Sources for formulas on calculation: 
# https://onlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson03/pears.cor_.R
# https://onlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson03/spear.cor_.R

### MAke CODE formulas for these cases of data: 
# https://www.andrews.edu/~calkins/math/edrm611/edrm13.htm

# both X and Y are quantitative
PearsonCor <- function(x, y){
      return(cor(x, y))
}
# X or Y or both are ordinal ...
PearsonCorCategorical <- function(tbl){
      rscore <- 1:nrow(tbl)  #?????????? ############################ TODO correct?
      cscore <- 1:ncol(tbl)  #??????????
      rbar <- sum(margin.table(tbl,1)*rscore)/sum(tbl) 
      rdif <- rscore - rbar 
      cbar <- sum(margin.table(tbl,2)*cscore)/sum(tbl) 
      cdif <- cscore-cbar 
      ssr <- sum(margin.table(tbl,1)*(rdif^2)) 
      ssc <- sum(margin.table(tbl,2)*(cdif^2)) 
      ssrc <- sum(t(tbl * rdif) * cdif) 
      pcor <- ssrc/(sqrt(ssr*ssc)) 
      return(pcor)
}

########################
# psych package has - tetrachoric, polyserial, polychoric, biserial, 
########################


# X and Y are binary (2x2 table)
# Phi() from DescTools

# quantitative X and ordinal Y
# biserialCorrelation() ################################# TODO

# quant X and nominal Y OR nominal X and quant Y
# pointBiserialCor() ################################# TODO

# ordinal X and nominal Y OR nominal X and ordinal Y
# rankBiserialCor() ################################# TODO

# ordinal X and Y
# tetrachoricCor() ################################# TODO
#tetrachoricCor <- function(tbl){
#      library(psych)
#      tetrachoric(tbl) ############# TODO: present tau, but what is the first one?
#}

# X and Y or either is ordinal
CorrelationTwoWay <- function(tbl, printNice=TRUE) { 
      # only one of X or Y must be ordinal ???? ################################# TODO
      pcor <- PearsonCorCategorical(tbl)
      # ordinal X and Y
      scor <- SpearmanRho(tbl)
      # Kendal correlation (tauA is standard, but tauB = tauA when there are no ties.
      # tauB handles ties better than tauA. )
      # ordinal X and Y
      tauB <- KendallTauB(tbl)
      kw <- KendallW(tbl)
      # ordinal X and Y
      gma <- GoodmanKruskalGamma(tbl)$Gamma
      # Mantel-haenszel
      mantel <- MantelHaenszelTwoWay(tbl, printNice=FALSE)
      
      result <- data.frame(Pearson=pcor, Spearman=scor, KendallTauB=tauB, KendallW=kw,
                           GoodmanKruskalGamma=gma, MantelHaenszel=mantel)
      
      if(printNice){
            cat("\n")
            cat("#################################################\n")
            cat("####       Correlation (two-way table)       ####\n")
            cat("#################################################\n")
            cat("\nPearson                            = ", pcor, sep="")
            cat("\nSpearman                           = ", scor, sep="")
            cat("\nKendall (tau-b)                    = ", tauB, sep="")
            cat("\nKendall (tau-c) ????")
            cat("\nKendall W (coef. of concordance)   = ", kw, sep="") 
            cat("\nGoodman-Kruskal's gamma            = ", gma, sep="")
            cat("\nMantel-Haenszel (pearson)          = ", mantel$UsingPearson, sep="")
            cat("\nMantel-Haenszel (spearman)         = ", mantel$UsingSpearman, sep="")
            cat("\nCramer's V                         = ")
            cat("\nPoint Biserial , rank biserial, biserial, phi  = ")
            cat("\n")
            return(invisible(result))
      }
      return(result)
} 

# for two-way: type of correlation for ordinal data based on pearson and spearman.
# for three-way: another conditional test (if odds ratios of all conditional two-way
# tables are all 1 or not)
# NOTE: computes only one of the 3 versions: only the general association version of the
# CMH statistic which treats both variables as nominal.
CochranMantelHaenszelTest <- function(tbl, correct=FALSE, alternative="two.sided",
                                      printNice=TRUE){
      
      # Two-way table
      if(length(dim(tbl)) == 2){
            pcor <- PearsonCorCategorical(tbl)
            scor <- SpearmanRho(tbl)
            
            M2.p <- (sum(tbl)-1)*pcor^2
            M2.s <- (sum(tbl)-1)*scor^2
            
            result <- data.frame(UsingPearson=M2.p, UsingSpearman=M2.s)
            
            if(printNice){ 
                  cat("#######################################################\n")
                  cat("#####             Mantel-Haenszel Test            #####\n")
                  cat("#####                (two-way table)              #####\n")
                  cat("#######################################################\n")
                  cat("\nM2 (from pearson):                  ", M2.p, sep="")
                  cat("\np-value:                            ", 1-pchisq(M2.p, df=1), sep="")
                  cat("\nM2 (from spearman):                 ", M2.s, sep="")
                  cat("\np-value:                            ", 1-pchisq(M2.s, df=1), sep="")
                  return(invisible(result))
            }
            return(result)
      
      ### Three-way table      
      } else if(length(dim(tbl)) == 3){
            test <- mantelhaen.test(tbl, correct = correct, alternative = alternative)
            
            if(printNice){
                  l <- test$conf.int[1]
                  u <- test$conf.int[2]
                  sign <- ""
                  if(alternative == "two.sided") sign <- "≠"
                  else if(alternative == "greater") sign <- ">"
                  else sign <- "<"
                  cat("###########################################################################\n")
                  cat("##########             Cochran-Mantel-Haenszel Test            ############\n")
                  cat("##########                  (three-way table)                  ############\n")
                  cat("###########################################################################\n")
                  cat("\t\tH0: conditional odds ratios are = 1\n")
                  cat("\t\tH1: at least one conditional odds ratio ",sign," 1", sep="")
                  cat("\n\nM2:\t\t                                   ", test$statistic)
                  cat("\ncommon odds-ratio:\t\t                   ", test$estimate)
                  cat("\ndf:\t\t                                   ", test$parameter)
                  cat("\np-value:\t\t                           ", test$p.value)
                  cat("\nConfidence interval:\t\t                    (",l,", ",u,")\n\n",sep="")
                  return(invisible(test))
            }
            return(test)
      }
}


#### Goodman and Kruskal gamma - tbl is a (nxm) matrix of counts.
GoodmanKruskalGamma <- function(tbl){
      n <- nrow(tbl)
      m <- ncol(tbl)
      # make list of zeroes (n-1) * (m-1) long
      pairTotals <- numeric((n-1)*(m-1))
      
      # calculate number of concordant pairs for each cell count
      for(i in 1:(n-1)){
            for(j in 1:(m-1)){
                  pairTotals[j + (m-1)*(i-1)] <- tbl[i,j] * sum(tbl[(i+1):n, (j+1):m])
            }
      }
      # number of concordant pairs
      C <- sum(pairTotals)
      
      # calculate number of discordant pairs for each cell count
      iter <- 0
      pairTotals <- numeric((n-1)*(m-1))
      for(i in 1:(n-1)){
            for(j in 2:m){
                  iter <- iter + 1
                  pairTotals[iter] <- tbl[i,j] * sum(tbl[(i+1):n, 1:(j-1)])
            }
      }
      # number of discordant pairs
      D <- sum(pairTotals)
      
      # statistic
      gamma <- (C - D)/(C + D)
      return(data.frame(Gamma=gamma, ConcordantPairs=C, DiscordantPairs=D))
}




##### McNemar test for marginal homogeneity (are marginal proportions the same?)
# IF marginal props are same then they have not changed from one sample to the other.
## Meant for matched-pairs data not independent sample, where X and Y are independent
# ASSUMPTIONS: only need to correct when sample size is small
McNemarTest <- function(tbl, correct=FALSE, printNice=TRUE){
      test <- mcnemar.test(tbl, correct = correct)
      result <- data.frame(ChiStatistic=test$stat, DegFree=test$par, PValue=test$p.value)
      if(printNice){
            cat("#################################################################\n")
            cat("#######      McNemar's Test for Marginal Homogeneity      #######\n")
            cat("#######                (n x n tables only)                #######\n")
            cat("#################################################################\n")
            cat("\t\t   H0: marginal prop. diff. = 0\n")
            cat("\t\t   HA: marginal prop. diff. ≠ 0\n")
            cat("χ2: \t\t\t\t\t            ", test$stat, "\n", sep="")
            cat("df: \t\t\t\t\t            ", test$parameter, "\n", sep="")
            cat("p-value: \t\t\t\t\t    ", test$p.value, "\n",sep="")
            cat("\n")
            return(invisible(result))
      }
      return(result)
}



## Chi-square tests
ChiSquareGoodnessOfFit <- function(vector, expVector=NULL, correct=FALSE, printNice=TRUE){
      if(!is.vector(vector)){
            return("ERROR: data was not vector (n x 1)")
      }
      if(!is.null(expVector)) {
            test <- chisq.test(vector,  correct = correct, p=expVector/sum(expVector))
      }
      else test <- chisq.test(vector, correct = correct)
      
      result <- data.frame(ChiStatistic=test$stat, DegFree=test$par, PValue=test$p.value)
      
      if(printNice){
            cat("#################################################\n")
            cat("###    Chi-Square Test for Goodness Of Fit    ###\n", sep="")
            cat("#################################################\n")
            cat("\t    H0: expected = observed\n")
            cat("\t    HA: expected ≠ observed\n")
            cat("χ2:                                   ", test$stat, "\n", sep="")
            cat("df:                                   ", test$par, "\n", sep="")
            cat("p-value:                              ", test$p.value, "\n\n", sep="")
            return(invisible(result))
      }
      return(result)
}

ChiSquareIndependence <- function(obsData, expData=NULL, correct=FALSE, printNice=TRUE){
      # test is a little illogical here, but just want to not accept things that
      # should be tested by GOF function. Can accept three-way and higher tables. 
      if(is.vector(obsData)){
            ################## TODO check if possibel for three-way table or k-way...??
            return("ERROR: data was not matrix (either 2-way or 3-way)")
      }
      
      if(length(dim(obsData)) == 2){
            if(!is.null(expData)) {
                  test <- chisq.test(obsData, correct=correct, p=expData/sum(expData))
            }
            else test <- chisq.test(obsData, correct=correct)
            tableType <- " (two-way table) "
            result <- list(ChiStatistic=unname(test$stat), DegFree=unname(test$par), 
                           PValue=unname(test$p.value), Expected=test$exp, 
                           Residuals=test$resid, StdResiduals=test$stdres)
            
      } else if (length(dim(obsData)) == 3) { # assume dim == 3
            A <- margin.table(obsData, 1)
            B <- margin.table(obsData, 2)
            C <- margin.table(obsData, 3)
            
            # expected values under mutual independence model. 
            # outer product of arrays
            expData <- (A %o% B %o% C) / sum(obsData)^2
            
            chi.stat <- sum(((expData - obsData)^2) / expData)
            i <- dim(obsData)[1]
            j <- dim(obsData)[2]
            k <- dim(obsData)[3]
            # num free pars in saturated model minus num free pars in independence model.
            df <- i*j*k -1 - ((i-1) + (j-1) + (k-1))
            p.value <- 1 - pchisq(chi.stat, df=df)
            ################################ TODO find stdres formula. 
            result <- list(ChiStatistic=chi.stat, DegFree=df, PValue=p.value, 
                           Expected=expData, Residuals=(obsData-expData)/sqrt(expData))
                           #StdResiduals=#)
            tableType <- "(three-way table)"
      }
      
      if(printNice){
            cat("####################################################\n")
            cat("####      Chi-Square Test for Independence      ####\n", sep="")
            cat("####              ", tableType, "             ####\n", sep="")
            cat("####################################################\n")
            cat("\t    H0: expected = observed\n")
            cat("\t    HA: expected ≠ observed\n")
            cat("χ2:                                   ", result$Chi, "\n", sep="")
            cat("df:                                   ", result$Deg, "\n", sep="")
            cat("p-value:                              ", result$PValue, "\n\n", sep="")
            return(invisible(result))
      }
      return(result)
}


############################################# TODO update for three-way like above
ChiSquareHomogeneity <- function(obsData, expData=NULL, correct=FALSE, printNice=TRUE){
      if(!is.matrix(obsData)){
            ################## TODO check if possibel for three-way table or k-way...??
            return("ERROR: data was not matrix (n x m)")
      }
      if(!is.null(expData)) {
            test <- chisq.test(obsData, correct=correct, p=expData/sum(expData))
      }
      else test <- chisq.test(obsData, correct=correct)
      
      result <- data.frame(ChiStatistic=test$stat, DegFree=test$par, PValue=test$p.value)
      
      if(printNice){
            cat("############################################################\n")
            cat("###    Chi-Square Test for Homogeneity Of Proportions    ###\n", sep="")
            cat("############################################################\n")
            cat("\t H0: same proportions over propulations\n")
            cat("\t HA: different proportions over propulations\n\n")
            cat("χ2:                                           ", test$stat, "\n", sep="")
            cat("df:                                           ", test$par, "\n", sep="")
            cat("p-value:                                      ", test$p.value, "\n\n", sep="")
            return(invisible(result))
      }
      return(result)
}





##### library vcd 
############################## TODO how to get p-values for hyptest?
CohenKappaTest <- function(tbl, printNice=TRUE){
      library(vcd)
      k <- Kappa(tbl)
      
      if(printNice){
            cat("#############################################################\n")
            cat("######      Cohen's Kappa of Inter-Rater Agreement      #####\n")
            cat("#############################################################\n")
            cat("\t H0: πii = (πi+)(π+i)    {independent ratings}\n")
            cat("\t HA: πii ≠ (πi+)(π+i)    {dependent ratings}\n\n")
            cat("Kappa = ", k$Unweighted[1], "\n", sep="")
            cat("Confidence Intervals:")
            print(confint(k))
            cat("\n")
            return(invisible(k))
      }
      return(k)
}



###########################################################################################
##################          Tests for 3-way tables          ###############################
###########################################################################################


MutualIndependence <- function(tbl, correct=FALSE, printNice=TRUE){
      test <- ChiSquareIndependence(tbl, correct=correct,printNice = FALSE)
      if(printNice){
            cat("###############################################################\n")
            cat("######             Mutual Independence Test              ######\n")
            cat("######         (Chi-Square Test for 3-way table)         ######\n")
            cat("###############################################################\n")
            cat("\t   H0: mutually independent variables\n")
            cat("\t   HA: non-mutually independent variables\n")
            cat("χ2:\t\t                                    ", test$Chi)
            cat("\ndf:\t\t                                    ", test$DegFree)
            cat("\np-value:\t\t                            ", test$PValue,"\n\n")
            return(invisible(test))
      }
      return(test)
}



################################# TODO: RELATIONS
################################# from marg tests sum to total mutual chistat test. 
MarginalIndependence <- function(tbl, correct=FALSE, printNice=TRUE){
      numVars <- length(dim(tbl))
      names <- names(dimnames(tbl))
      letters <- LETTERS[1:numVars]
      
      testCombos <- list()
      rowNames <- c()
      summaryTable <- data.frame(Model="", Margin="",ChiSquare=0, Xdf=0,XPvalue=0,
                                 LikRatio=0, Gdf=0, GPvalue=0)
      
      # It is 2:(n-1) because we cannot have (A, BCD) (one-way table) 
      # nor (ABCD, _) (no marginal)
      for(i in 2:(numVars - 1)){
            marginVars <- combn(1:numVars, m=i)
            testNames <- combn(names, m=i)
            letterNames <- combn(letters, m=i)
            
            for(c in 1:ncol(marginVars)){
                  # make the name pairs
                  pairOne <- testNames[,c]
                  pairOneL <- letterNames[,c]
                  pairTwo <- names[! names %in% pairOne]
                  pairTwoL <- letters[! letters %in% pairOneL] # ∩ 
                  modelName <- paste(paste(pairOne, collapse=","),"  margin=",
                                     paste(pairTwo, collapse=","), sep="")
                  modelLetterName <- paste("(",paste(pairOneL,collapse=""), ")",sep="")
                  margOver <- paste(pairTwoL,collapse="")
                  modelLetterNameX <- paste(paste(pairOneL,collapse=""),"m",
                                            paste(pairTwoL,collapse=""),sep="")
                  # keeping log to put as row names so we don't see numbers down the side.
                  rowNames <- c(rowNames, modelName)
                  
                  # do the test
                  temp <- margin.table(tbl, marginVars[,c])
                  G <- LikelihoodRatioTableTest(temp, printNice = FALSE)
                  X <- suppressWarnings(ChiSquareIndependence(temp, correct=correct,
                                                                 printNice = FALSE))
                  X$Name <- modelName
                  X$LetterName <- modelLetterName
                  test <- list(Name=modelName, LetterName=modelLetterName,
                                     ChiSquare=X$Chi, Xdf=X$D, XPvalue=X$P,
                                     Expected=X$Exp, Residuals=X$Res, StdResiduals=X$Std,
                                     LikRatio=G$Lik, Gdf=G$D, GPvalue=G$P)
                  
                  # add along a row to the matrix
                  summaryTable <- rbind(summaryTable, 
                                        data.frame(Model=modelLetterName,
                                                   Margin=margOver, 
                                                   ChiSquare=X$Chi,Xdf=X$D,XPvalue=X$PValue,
                                                   LikRatio=G$Lik,Gdf=G$D,GPvalue=G$PValue))
                  # keep log of all tests
                  testCombos[[modelLetterNameX]] <- test 
            }
      }
      # delete first zero row
      summaryTable <- summaryTable[-1,]
      row.names(summaryTable) <- rowNames 
      
      
      if(printNice){
            cat("#######################################################################################\n")
            cat("###############               Marginal Independence Tests               ###############\n")
            cat("###############       (Goodness-fit Tests for k-way table margins)      ###############\n")
            cat("#######################################################################################\n\n")
            cat("\t\tH0: variables in each marginal model are independent\n")
            cat("\t\tHA: variables in each marginal model are NOT independent\n\n")
            print(summaryTable)
            cat("\n")
            return(invisible(testCombos))
      }
      return(testCombos)
}


# for k-way table
# warning - there better not be as many variable as to run out of letters names. 
JointIndependence <- function(tbl, correct=FALSE, printNice=TRUE){
      # EXAMPLE: if dim = 4, then we generate combos up to m = 2
      # EXAMPLE: if dim = 5, then we generate combos up to m = 2 (because (MN, IJK) is same
      # as (IJK, MN) so we need just to develop until m=2, see that m=2 is reflected by MN)
      numVars <- length(dim(tbl))
      limitGroups <- floor(numVars / 2)
      names <- names(dimnames(tbl))
      letters <- LETTERS[1:numVars]
      
      testCombos <- list()
      rowNames <- c()
      summaryTable <- data.frame(Model="", ChiSquare=0, DF=0, PValue=0)
      
      for(i in 1:limitGroups){
            rowVars <- combn(1:numVars, m=i)
            testNames <- combn(names, m=i)
            letterNames <- combn(letters, m=i)
            
            for(c in 1:ncol(rowVars)){
                  # make the name pairs
                  pairOne <- testNames[,c]
                  pairOneL <- letterNames[,c]
                  pairTwo <- names[! names %in% pairOne]
                  pairTwoL <- letters[! letters %in% pairOneL]
                  modelName <- paste(paste(pairOne, collapse=",")," ∩ ",
                                     paste(pairTwo, collapse=","), sep="")
                  modelLetterName <- paste("(",paste(pairOneL,collapse=""), ", ",
                                           paste(pairTwoL,collapse=""), ")", sep="")
                  modelLetterNameX <- paste(paste(pairOneL,collapse=""),"x",
                                            paste(pairTwoL,collapse=""),sep="")
                  # keeping log to put as row names so we don't see numbers down the side.
                  rowNames <- c(rowNames, modelName)
                  
                  # do the test
                  temp <- ftable(tbl, row.vars = rowVars[,c])
                  test <- suppressWarnings(ChiSquareIndependence(temp, correct=correct,
                                                                 printNice = FALSE))
                  test$Name <- modelName
                  test$LetterName <- modelLetterName
                  # add along a row to the matrix
                  summaryTable <- rbind(summaryTable, 
                                        data.frame(Model=modelLetterName,
                                                   ChiSquare=test$Chi, DF=test$D, 
                                                   PValue=test$PValue))
                  # keep log of all tests
                  testCombos[[modelLetterNameX]] <- test 
            }
      }
      # delete first zero row
      summaryTable <- summaryTable[-1,]
      row.names(summaryTable) <- rowNames 
      
      
      if(printNice){
            cat("#############################################################################\n")
            cat("############              Joint Independence Tests               ############\n")
            cat("############      (Chi-Square Tests for k-way table subsets)     ############\n")
            cat("#############################################################################\n\n")
            cat("\t    H0: variables in each joint model are independent\n")
            cat("\t    HA: variables in each joint model are NOT independent\n\n")
            print(summaryTable)
            cat("\n")
            return(invisible(testCombos))
      }
      return(testCombos)
}

# PURPOSE: extract a conditional table from multi-dim array (dynamically, on the fly)
# from stack overflow
extract <- function(tbl, .dim, .value) {
      ids <- lapply(dim(tbl), seq_len)
      for(i in 1:length(.dim)){
            ids[[.dim[i]]] <- .value[i]
      }
      do.call(`[`, c(list(tbl), ids))
}

convertDFRowToVec <- function(df, row){
      vec <- c() 
      for(col in 1:ncol(df))  vec <- c(vec, as.vector(df[row,][[col]]))
      vec 
}

# input k-way table and do tests conditional for each level. 
ConditionalIndependence <- function(tbl, correct=FALSE, printNice=TRUE){
      numVars <- length(dim(tbl))
      names <- names(dimnames(tbl))
      letters <- LETTERS[1:numVars]
      
      testCombos <- list()
      rowNames <- c()
      summaryTable <- data.frame(Model="", Cond="",ChiSquare=0, DF=0, PValue=0)
      
      # It is 2:(n-1) because we cannot have (A, BCD) (one-way table) 
      # nor (ABCD, _) (no marginal)
      for(i in 2:(numVars - 1)){
            colVars <- combn(1:numVars, m=i)
            testNames <- combn(names, m=i)
            letterNames <- combn(letters, m=i)
            #colVars <- combn(1:numVars, m=3)
            #testNames <- combn(names, m=3)
            #letterNames <- combn(letters, m=3)
            
            for(c in 1:ncol(colVars)){
                  # Make the name pairs
                  pairOne <- testNames[,c]
                  pairOneL <- letterNames[,c]
                  #pairOne <- testNames[,3]
                  #pairOneL <- letterNames[,3]
                  pairTwo <- names[! names %in% pairOne]
                  pairTwoL <- letters[! letters %in% pairOneL]
                  modelLetterName <- paste("(",paste(pairOneL,collapse=""), ")",sep="")
                  totalName <- paste(paste(pairOne, collapse=","), " | ",
                                     paste(pairTwo, collapse=","), sep="")
                  modelLetterNameX <- paste(paste(pairOneL,collapse=""),"c",
                                            paste(pairTwoL,collapse=""),sep="")
                  cond <- paste(pairTwoL,collapse="")
                  
                  # Extract the table
                  .values <- list()
                  # for certain c = (1,2,5) (dim), get corresponding levels.
                  #.dims <- seq_len(numVars)[! seq_len(numVars) %in% colVars[,3]]
                  .dims <- seq_len(numVars)[! seq_len(numVars) %in% colVars[,c]] 
                  for(var in .dims){
                        .values <- append(.values, list(dimnames(tbl)[[var]]))
                  }
                  .values <- expand.grid(.values) # get all combinations of groups
                  colnames(.values) <- pairTwo
                  
                  # Do the test for each level of the (c) variables chosen. 
                  totalChi <- 0
                  totalDF <- 0 
                  for(r in 1:nrow(.values)){
                        # convert data.frame row to usable vector
                        #.vals <- convertDFRowToVec(df=.values, row=4)
                        .vals <- convertDFRowToVec(df=.values, row=r)
                        # get table + flatten it.
                        temp <- ftable(extract(tbl, .dims, .vals))
                        
                        # keeping log to put as row names so we don't see numbers down the side.
                        condNames <- data.frame(pairTwo, .vals)
                        condNames <- paste(
                              lapply(1:nrow(condNames), function(i) 
                                    paste(convertDFRowToVec(condNames, i), collapse="=")),
                              collapse=",")
                        modelName <- paste(paste(pairOne, collapse=","),
                                           " | ", condNames, sep="")
                        rowNames <- c(rowNames, modelName)
                        
                        # Do the test  for this particular conditional LEVEL. 
                        test <- suppressWarnings(ChiSquareIndependence(temp, 
                                                                       correct=correct,
                                                                       printNice = FALSE))
                        test$Name <- modelName
                        test$LetterName <- modelLetterName
                        # add along a row to the matrix
                        summaryTable <- rbind(summaryTable, 
                                              data.frame(Model=modelLetterName,
                                                         Cond=cond, 
                                                         ChiSquare=round(test$Chi,4), 
                                                         DF=test$D, 
                                                         PValue=round(test$PValue,4)))
                        # keep log of all tests
                        #testCombos[[modelLetterNameX]] <- test 
                        totalChi <- totalChi + test$Chi 
                        totalDF <- totalDF + test$D
                  }
                  testCombos[[modelLetterNameX]] <- data.frame(ChiSquare=totalChi,
                                                               DF=totalDF, 
                                                               PValue=1-pchisq(totalChi, totalDF))
                  row.names(testCombos[[modelLetterNameX]]) <- totalName
            }
      }
      # delete first zero row
      summaryTable <- summaryTable[-1,]
      row.names(summaryTable) <- rowNames 
      
      
      if(printNice){
            cat("#############################################################################\n")
            cat("############            Conditional Independence Tests           ############\n")
            cat("############      (Chi-Square Tests for k-way table subsets)     ############\n")
            cat("#############################################################################\n\n")
            cat("\t    H0: model variables are conditionally independent\n")
            cat("\t    HA: model variables are NOT conditionally independent\n\n")
            print(summaryTable)
            cat("\n")
            return(invisible(testCombos))
      }
      return(testCombos)
}


# assumptions: breslow-day takes only 3-way table and only 2x2xJ table, at that, never IJK. 
HomogenousAssociation <- function(tbl, printNice=TRUE){
      test <- BreslowDayTest(tbl)
      result <- data.frame(ChiSquare=test$stat, DF=test$par, PValue=test$p.value)
      row.names(result) <- ""
      commonOddsRatio <- CochranMantelHaenszelTest(tbl, p=FALSE)$estimate
      
      if(printNice){
            cat("###############################################################\n")
            cat("####      Breslow-Day Test for Homogenous Odds Ratios      ####\n")
            cat("####             (3-way and (2x2xK) table only)            ####\n")
            cat("###############################################################\n")
            cat("\t   H0: equal odds-ratios across strata\n")
            cat("\t   HA: at least one different odds-ratio\n")
            cat("χ2:\t                                           ",result$Chi)
            cat("\ndf:\t                                           ", result$DF)
            cat("\np-value:\t                                   ", result$PValue)
            cat("\ncommon odds-ratio for strata:                      ", commonOddsRatio,"\n\n")
            return(invisible(result))
      }
      return(result)
}










###########################################################################################
################              Generalized Linear Model Tests              #################
###########################################################################################


# PRECONDITION: responseTbl is 2-way table that can be (n x 2)
# TYPE: Y = binary, X = one discrete predictor with J levels 
BinaryLogisticRegression_T <- function(responseTbl, printNice=TRUE){
      # the dim that is not 2 is the length of the predictor
      d <- dim(responseTbl)
      # find how many levels in predictor X
      limit <- if(d[1] == 2) d[2] else d[1]
      pred <- as.factor(rev(0 : (limit - 1)))
      
      fit <- glm(responseTbl ~ pred, family = binomial(link = "logit"))
      s <- summary(fit)
      result <- list(Coefficients=s$coef, NullDeviance=s$null.deviance, 
                     ResidualDeviance=s$deviance,  NullDf=s$df.null, ResidDf=s$df.residual,
                     PredictLogisticScale=predict(fit, type="response"),
                     PredictLinearScale=data.frame(predict(fit, type="terms")),
                     DevianceResiduals=residuals(fit), 
                     PearsonResiduals=residuals(fit, type="pearson"))

      if(printNice){
            cat("#####################################################################\n")
            cat("########             Binary Logistic Regression              ########\n")
            cat("########     (2-way table; predictor levels: unlimited)      ########\n")
            cat("#####################################################################\n")
            print(s$coefficients)
            cat("\nNull deviance:     ", s$null.deviance, "\n")
            cat("DF null:           ", s$df.null, "\n")
            cat("Residual deviance: ", s$deviance, "\n")
            cat("DF residual:       ", s$df.residual, "\n")
            return(invisible(result))
      }
      return(result)
}


################################# TODO figure out how to change to take saturated model
# saturatedModel <- glm(good ~ factor(rowNums) - 1, family=binomial(link=logit), data=placekick)
# PRECONDITION: names are strings
# TYPE: Y = binary, X = many Xs, each cont OR discrete (k = 2 or as many levels as needed)
BinaryLogisticRegression <- function(xNames=NULL, yName, data){
      if(is.null(xNames)){
            formula <- as.formula(paste(yName, " ~ ", 1))    
      } else {
            formula <- as.formula(paste(yName, " ~ ", paste(xNames, collapse=" + ")))
      }
      fit <- glm(formula, family=binomial(link=logit), data=data)
      s <- summary(fit)
      cat("###############################################################\n")
      cat("########           Binary Logistic Regression          ########\n")
      cat("###############################################################\n")
      print(s$coefficients)
      cat("\nNull deviance:     ", s$null.deviance, "\n")
      cat("DF null:           ", s$df.null, "\n")
      cat("Residual deviance: ", s$deviance, "\n")
      cat("DF residual:       ", s$df.residual, "\n")
      return(invisible(fit))
}


##########################################################################################
# Goodness of fit test for GLMs
# input = model object
################################################### TODO combine hosmerlem, HosmerLemeshow
######################### and the code in Predictor_Discrete_One file. 
HosmerLemeshowTest <- function(model, printNice=TRUE){
      library(ResourceSelection)
      test <- hoslem.test(model$y, model$fitted)
      result <- list(HLStatistic=test$stat, Df=test$par, PValue=test$p.value,
                     Expected=test$expected)
      
      if(printNice){
            cat("################################################################\n")
            cat("##########            Hosmer-Lemeshow Test            ##########\n")
            cat("################################################################\n")
            cat("\tHo: current model fits well: "); print(model$formula); 
            cat("\tHa: current model does NOT fit well\n\n")
            cat("HL statistic:                                         ", test$stat, "\n")
            cat("Df:                                                   ", test$par, "\n")
            cat("P-value:                                              ", test$p.value, "\n\n")
            return(invisible(result))
      }
      return(result)
}

###################################### TODO FIX
# g = num groups
HosmerLemeshowTest_Calc <- function(model, g=10, printNice=TRUE){
      
      pi.hat <- model$fitted.values 
      # categorize observations into deciles of the predicted probabilities
      #pi.hat.grouped <- cut(pi.hat, 
      #                      breaks=c(0,quantile(pi.hat, probs=seq(0.1, 0.9,0.1)), 1), 
      #                      labels=FALSE)
      pi.hat.grouped <- cut(pi.hat, breaks = quantile(pi.hat, 
                                                    probs = seq(0, 1, 1/g)), 
                            include.lowest = T)
      
      # cycle through 1:10 groups, counting the observed
      # number of 0s and 1s, and the expected number of 0s and 1s. 
      # To find expected 0s and 1s, find mean of predicted
      # probabilities in each group and multiply by group size (10)
      meanProbs <- array(0, dim=c(10,2))
      exp.0.1 <- array(0, dim=c(10,2))
      obs.0.1 <- array(0, dim=c(10,2))
      
      for(i in 1:10){
            # for 1's
            meanProbs[i,1] <- mean(pi.hat[pi.hat.grouped == i]) # purpose for exp.0.1 only
            exp.0.1[i,1] <- sum(pi.hat.grouped == i) * meanProbs[i,1]
            obs.0.1[i,1] <- sum(y[pi.hat.grouped == i])
            
            # for 0's
            meanProbs[i,2] <- mean(1 - pi.hat[pi.hat.grouped == i]) # purpose for exp.0.1 only
            exp.0.1[i,2] <- sum(pi.hat.grouped == i) * meanProbs[i,2]
            obs.0.1[i,2] <- sum(1 - y[pi.hat.grouped == i])
      }
      
      # now statistic is found by sum of observed-expected over 10x2 cells of the table. 
      library(ResourceSelection)
      stat <- sum((obs.0.1 - exp.0.1)^2 / exp.0.1)
      result <- data.frame(HLStat=stat, Df=g-2, PValue=1-pchisq(stat, g-2),
                           Expected=hoslem.test(model$y, model$fitted.values, g=g)$exp)
      
      if(printNice){
            cat("################################################################\n")
            cat("##########            Hosmer-Lemeshow Test            ##########\n")
            cat("################################################################\n")
            cat("\tHo: current model fits well: "); print(model$formula); 
            cat("\tHa: current model does NOT fit well\n\n")
            cat("HL statistic:                                         ", result$HL, "\n")
            cat("Df:                                                   ", result$Df, "\n")
            cat("P-value:                                              ", result$P, "\n\n")
            return(invisible(result))
      }
      return(result)
}


# y = values along each X level for Y = 1 (successes) (e.g. for smoke, it is response[,1])
# yhat = rowsums(response) * predicted 
HosmerLemeshowTest_T <- function (y, y.hat=n*prob, g = 10) {
      cut_y.hat <- cut(y.hat, 
                       breaks = quantile(y.hat, probs = seq(0, 1, 1/g)), 
                       include.lowest = T)
      obs <- xtabs(cbind(1 - y, y) ~ cut_y.hat)
      expect <- xtabs(cbind(1 - y.hat, y.hat) ~ cut_y.hat)
      chisq <- sum((obs - expect)^2/expect)
      result <- data.frame(X2 = chisq, Df = g - 2, PValue = 1-pchisq(chisq, g-2))
      row.names(result) <- ""
      return(result)
}





## TODO LIST. 
# (1) k-way dev resids

# (2) likelihood ratio test function for k-way tables, where arg = k = num
# of random variables on input. 

# (3) do chi-square GOF tests for multinomial, poisson, and binomial situations
# for one-way tables (see GOF_Mult_Pois_Soccer since we had to subtract -1
# from first df to get good df) (provide MLE estimates that change the df)
# Also in this test, compute the df cutoff sqrt( (k-1)/k ) for pearson residuals.

# (4) do chi-square INDEPENDENCE tests for mult, hyp, prod-mult, poisson for
# two-way tables. (use χ2)

# (5) all AP stat hyptests: z-test for mean, t-test for mean, all props ...

# (6) make function that identifies when simpson's paradox occurs (when (for which combo)
# is the marginal and conditional association different?)

# (7) add G2 statistics tests to the Independence Tests Cond, marg, Mutual...

# (8) make ChiSquareTest for Models (not just tables) - look at Hosmer for formula, 
# also img formula is in Formulas Statistics folder in anaschool statistics folder. 

# (9) meld hosmer lemeshow tests (make one for table and another for model arg)