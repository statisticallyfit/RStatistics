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