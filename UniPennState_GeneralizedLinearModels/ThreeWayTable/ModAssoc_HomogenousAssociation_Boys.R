setwd('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/UniPennState_GeneralizedLinearModels/ThreeWayTable/')
#source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/learnstatistics/[PennState] GeneralizedLinearModels/ThreeWayTable/ModAssoc_MarginalIndependence_Boys.R')
#source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/learnstatistics/[PennState] GeneralizedLinearModels/ThreeWayTable/ModAssoc_JointIndependence_Boys.R')
#source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/learnstatistics/[PennState] GeneralizedLinearModels/ThreeWayTable/ModAssoc_ConditionalIndependence_Boys.R')


### NOTE: breslow-day and therefor homogenous tests have not been generalized to IJK tables
### It must be a 2 x 2 x J table
### PLUS, it must be a three-way table, never k-way or lower. 

# shorter way
HomogenousAssociation(temp.by.SES) # only this works because it is 2x2x3 

# H0: odds ratios for scout x delinquent tables are equal across levels of SES (low,med,high)
BreslowDayTest(temp.by.SES)
# assumptions that odds are comparable in size are verified
oddsRatio(temp.by.SES[,,1])
oddsRatio(temp.by.SES[,,2])
oddsRatio(temp.by.SES[,,3])


# H0: odds ratios for SES x delinquent tables are equal across levels of scout (no,yes)
BreslowDayTest(temp.by.scout)
# assumptions that odds are comparable in size are verified
oddsRatio(temp.by.scout[,,1])
oddsRatio(temp.by.scout[,,2])



# H0: odds ratios for scout x delinquent tables are equal across levels of SES
BreslowDayTest(temp.by.delinq)
# assumptions that odds are comparable in size are verified
oddsRatio(temp.by.SES[,,1])
oddsRatio(temp.by.SES[,,2])




###########################################################################################
# with tarone corrections ...

breslowday.test <- function(x) {
      #Find the common OR based on Mantel-Haenszel
      or.hat.mh <- mantelhaen.test(x)$estimate
      #Number of strata
      K <- dim(x)[3]
      #Value of the Statistic
      X2.HBD <- 0
      #Value of aj, tildeaj and Var.aj
      a <- tildea <- Var.a <- numeric(K)
      
      for (j in 1:K) {
            #Find marginals of table j
            mj <- apply(x[,,j], MARGIN=1, sum)
            nj <- apply(x[,,j], MARGIN=2, sum)
            
            #Solve for tilde(a)_j
            coef <- c(-mj[1]*nj[1] * or.hat.mh, nj[2]-mj[1]+or.hat.mh*(nj[1]+mj[1]),
                      1-or.hat.mh)
            sols <- Re(polyroot(coef))
            #Take the root, which fulfills 0 < tilde(a)_j <= min(n1_j, m1_j)
            tildeaj <- sols[(0 < sols) &  (sols <= min(nj[1],mj[1]))]
            #Observed value
            aj <- x[1,1,j]
            
            #Determine other expected cell entries
            tildebj <- mj[1] - tildeaj
            tildecj <- nj[1] - tildeaj
            tildedj <- mj[2] - tildecj
            
            #Compute \hat{\Var}(a_j | \widehat{\OR}_MH)
            Var.aj <- (1/tildeaj + 1/tildebj + 1/tildecj + 1/tildedj)^(-1)
            
            #Compute contribution
            X2.HBD <- X2.HBD + as.numeric((aj - tildeaj)^2 / Var.aj)
            
            #Assign found value for later computations
            a[j] <- aj ;  tildea[j] <- tildeaj ; Var.a[j] <- Var.aj
      }
      
      #Compute Tarone corrected test
      X2.HBDT <-as.numeric( X2.HBD -  (sum(a) - sum(tildea))^2/sum(Var.aj) )
      
      #Compute p-value based on the Tarone corrected test
      p <- 1-pchisq(X2.HBDT, df=K-1)
      
      res <- list(X2.HBD=X2.HBD,X2.HBDT=X2.HBDT,p=p)
      class(res) <- "bdtest"
      return(res)
}

print.bdtest <- function(x) {
      cat("Breslow and Day test (with Tarone correction):\n")
      cat("Breslow-Day X-squared         =",x$X2.HBD,"\n")
      cat("Breslow-Day-Tarone X-squared  =",x$X2.HBDT,"\n\n")
      cat("Test for test of a common OR: p-value = ",x$p,"\n\n")
}