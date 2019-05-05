library(ggplot2)


# svmPlot code from: 
# https://stackoverflow.com/questions/35823402/r-how-to-plot-the-hyperplane-and-margins-of-an-svm-in-ggplot2


plotPairs <- function(svm.fit) {
      iNames <- !(names(Auto) %in% c("mpg", "mpgBinary","mpglevel", "name"))
      n <- sum(iNames)
      plotsList <- vector("list", length = n)
      i = 0
      yName <- "mpg"
      
      for (name in names(Auto)[iNames]) {
            #df <- data.frame(Auto$mpg, Auto[,name])
            #colnames(df) <- c("mpg", name)
            #print(head(df))
            
            print(paste("current name: " , name)) # -----------------------------------
            
            plotsList[i] <- svmPlot(svm.fit, Auto, yName=yName, xName = name)
            i = i + 1
            
            print(paste("i = ", i)) # -----------------------------------
      }
      return(plotsList)
}

# Assume: 
# x = ncol = 2 (means two predictors only, so the hyperplane is a line)
svmPlot <- function(svm.fit, data, yName, xName, allXNames) {
      N <- nrow(data) # num observations
      N.half1 <- round(N / 2)
      N.half2 <- N - N.half1
      
      grid <- expand.grid(seq(min(data[, yName]), max(data[, yName]),length.out=N.half1), #100), 
                          seq(min(data[, xName]), max(data[, xName]),length.out=N.half2)) #100)) 
      
      print("created grid") # -----------------------------------
      
      names(grid) <- c(yName, xName) # names(data)[1:2]
      print("grid names: ")
      print(names(grid))
      # -----------------------------------
      
      preds <- predict(svm.fit, grid)
      print("predicted svm using grid") # -----------------------------------
      
      df <- data.frame(grid, preds)
      
      cols <- c('1' = 'red', '-1' = 'black')
      tiles <- c('1' = 'magenta', '-1' = 'cyan')
      shapes <- c('support' = 4, 'notsupport' = 1)
      data$support <- 'notsupport'
      data[svm.fit$index, 'support'] <- 'support'
      
      print("starting to graph ...") # -----------------------------------
      
      p <- ggplot(df, aes(x = x.2, y = x.1)) + geom_tile(aes(fill = preds)) + 
            scale_fill_manual(values = tiles) +
            geom_point(data = data, aes(color = y, shape = support), size = 2) +
            scale_color_manual(values = cols) +
            scale_shape_manual(values = shapes) +
            ggtitle('SVM classification plot')
      
      return(p)
}

plotPairs(svm.final.radial)
