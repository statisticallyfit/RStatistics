# data cleaning recipes

# cols = the column names (strings) that must be turned to chars
# and have their whitespaces removed, else we get all cols as char!
removeWhitespace <- function(data, colsToFix){
      n <- ncol(data) # for rearranging later
      
      colsToAdd <- names(data)[(names(data) != colsToFix)]
      
      # actually changing the column whitespace
      newCol.df <- data.frame(unlist(lapply(data[,colsToFix], trimws)), 
                    stringsAsFactors = TRUE)
      
      # making new data frame
      df <- data.frame(cbind(newCol.df, data[,colsToAdd]))
      # naming in current order
      df <- setNames(df, nm=c(colsToFix, colsToAdd))
      # reorder
      df <- df[, names(data)]
      
      return(df)
}

# precondition: simple linear regression
# TODO: make for multiple regression
getContrastMatrix <- function(fit){
      
      xNameOrContrasts <- names(fit$contrasts)
      info <- data.frame(fit$contrasts)[, xNameOrContrasts]
      
      if(is.data.frame(info)){ # then the model was fit with user def contrasts
            return(as.matrix(info))
      } else { # else can be "contr.treatment" ... etc
            contrasts(fit$model[[xNameOrContrasts]])     
      }
}

testContrastsOrthogonal <- function(fit){
      cMat = getContrastMatrix(fit)
      test <- t(cMat) %*% cMat
      
      for(i in 1:nrow(test)){
            for(j in 1:ncol(test)){
                  if(i != j && test[i,j] != 0){
                        return(FALSE)
                  }
            }
      }
      return(TRUE)
}

# just in data.frame form, not ready for lm model fitting
makeOrthogonalContrasts.df <- function(factorNames, contrastNames, mirror=FALSE, mirrorH=FALSE, mirrorV=FALSE){
      numTreatments = length(factorNames)
      numContrasts = numTreatments - 1
      
      df = data.frame(contr.helmert(n=numTreatments))
      if(mirror) df <- mirror(df)
      else if(mirroH) df <- mirrorHorizontal(df)
      else if(mirrorV) df <- mirrorVertical(df)
      
      # now naming
      rownames(df) <- factorNames
      colnames(df) <- contrastNames
      
      return(df)
}

# ready for lm-use
# note: data has y-var as first col in data and xvar as second col
makeOrthogonalContrasts.lm <- function(data, contrastNames, 
                                       mirror=F, mirrorH=F, mirrorV=F){
      xName = names(data)[[2]]
      yName = names(data)[[1]]
      xVar = data[[xName]]
      yVar = data[[yName]]
      
      numTreatments = length(unique(xVar))
      df = data.frame(contr.helmert(n=numTreatments))
      if(mirror) df <- as.data.frame(mirror(df))
      else if(mirroH) df <- as.data.frame(mirrorHorizontal(df))
      else if(mirrorV) df <- as.data.frame(mirrorVertical(df))
      
      #numRows = length(C(xVar, contr=c(df[,1]), how.many=1)) #testing
      #df.lm <- zeros(m=ncol(df), n=numRows)
      contrastHolder <- list()
      
      for(i in 1:ncol(df)){
            contrastHolder <- list(contrastHolder, C(xVar, contr=c(df[,i]), how.many=1))
      }
      
      contrastHolder <- flatten.list(contrastHolder)
      names(contrastHolder) <- contrastNames
      
      df <- cbind(yVar, data.frame(contrastHolder))
      df <- setNames(df, nm=c(yName, names(df)[-1]))
      
      return(df)
}

# uses the data's names to make a formula we can plug right into the lm
makeFormulaFromData <- function(data){
      ns = names(data)
      return(as.formula(paste(ns[1], " ~ ", paste(ns[-1], sep=" + ", collapse=" + "))))
}


# flatten a list into a 1-level list
flatten.list <- function(x) {
      len <- sum(rapply(x, function(x) 1L))
      y <- vector('list', len)
      i <- 0L
      rapply(x, function(x) { i <<- i+1L; y[[i]] <<- x })
      y
}


# flips columns horizontally (like mirror)
mirrorHorizontal <- function(tbl){
      as.table(t(apply(tbl, 1, rev)))
}

mirrorVertical <- function(tbl){
      as.table(apply(tbl, 2, rev))
}

mirror <- function(tbl){
      as.table(mirrorVertical(mirrorHorizontal(tbl)))
}