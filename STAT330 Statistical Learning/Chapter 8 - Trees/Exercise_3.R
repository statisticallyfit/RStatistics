setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 8 - Trees")
library(ggplot2)

# Have K = 2 classes so consider how the sums are written ...
p <- seq(0, 1, 0.01)

giniIndex <- p * (1 - p) + p * (1 - p)
classifErrorRate <- 1 - pmax(p, 1 - p) # meaning of pmax?
crossEntropy <- - (p*log(p) + p*log(p))

df <- data.frame(G = giniIndex, E = classifErrorRate, D = crossEntropy, p = p)
#ggplot(df[2:nrow(df), ], aes(x=p)) + geom_point(size=3, aes(y=E, colour="red"))  +
#      geom_point(size=3, aes(y=G, colour="green")) + 
#      geom_point(size=3, aes(y=C, colour="magenta")) 

library(reshape2)
df.melt <- melt(df, id="p")
ggplot(data=df.melt, aes(x=p, y=value, colour=variable)) + geom_point(size=3)