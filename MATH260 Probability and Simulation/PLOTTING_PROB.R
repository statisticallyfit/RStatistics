library(ggplot2)


plotDiscreteDist <- function(xs, ys, colour="black", size.dot=7, size.line=2){
      df <- data.frame(xs=xs, ys=ys)
      ggplot(df, aes(x=xs, y=ys)) + 
            geom_point(size=size.dot, colour=colour) + 
            geom_linerange(ymin=0,ymax=df$ys, size=size.line, colour=colour) + 
            xlab("X") + ylab("P(X = x)")
}

plotContinuousDist <- function(xs, ys){
      df <- data.frame(xs=xs, ys=ys)
      ggplot() + 
            geom_line(data=df, aes(x=xs, y=ys), size=1, colour="blue") +
            #geom_vline(xintercept=mean(df$ys), 
            #           colour="black", linetype="dashed", size=1) + 
            xlab("X") + ylab("P(X = x)")
}

plotMultipleContinuousDist <- function(xs, ysList){
      df <- data.frame(xs=xs)
      df <- cbind(df, data.frame(sapply(ysList,c)))
      
      library(reshape2)
      df.melt <- melt(df, id="xs")
      
      ggplot(data=df.melt, aes(x=xs, y=value, colour=variable)) + geom_line(size=1)
}
