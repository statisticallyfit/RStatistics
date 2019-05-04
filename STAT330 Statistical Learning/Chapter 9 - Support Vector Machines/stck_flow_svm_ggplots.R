library(ggplot2)

plotPairs <- function(svm.fit) {
      n <- sum(!(names(Auto) %in% c("mpg", "mpglevel", "name")))
      plotsList <- vector("list", length = n)
      i = 0
      
      for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
            df <- data.frame(Auto$mpg, Auto[,name])
            colnames(df) <- c("mpg", name)
            print(head(df))
            plotsList[i] <- svmPlot(svm.fit, df)
      }
      return(plotsList)
}

# Assume: 
# x = ncol = 2 (means two predictors only, so the hyperplane is a line)
svmPlot <- function(svm.fit, data) {
      N <- nrow(data) # num observations
      N.half1 <- round(N / 2)
      N.half2 <- N - N.half1
      
      grid <- expand.grid(seq(min(data[, 1]), max(data[, 1]),length.out=N.half1), #100), 
                          seq(min(data[, 2]), max(data[, 2]),length.out=N.half2)) #100)) 
      names(grid) <- names(data)[1:2]
      preds <- predict(svm.fit, grid)
      df <- data.frame(grid, preds)
      
      cols <- c('1' = 'red', '-1' = 'black')
      tiles <- c('1' = 'magenta', '-1' = 'cyan')
      shapes <- c('support' = 4, 'notsupport' = 1)
      data$support <- 'notsupport'
      data[svm.fit$index, 'support'] <- 'support'
      
      p <- ggplot(df, aes(x = x.2, y = x.1)) + geom_tile(aes(fill = preds)) + 
            scale_fill_manual(values = tiles) +
            geom_point(data = data, aes(color = y, shape = support), size = 2) +
            scale_color_manual(values = cols) +
            scale_shape_manual(values = shapes) +
            ggtitle('SVM classification plot')
      
      return(p)
}