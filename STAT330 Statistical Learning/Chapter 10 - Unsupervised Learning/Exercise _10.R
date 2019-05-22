library(ggplot2)

# part a) generate data

set.seed(2)

# n = number of observations
x <- matrix(rnorm(n=20*3*50, mean = 0, sd=0.001), ncol=50)

x[1:20, 2] = 1 # obs 1 to 20, col 2 take label 1
x[21:40, 1] = 2 # obs 21 to 40, col 1 take label 2
x[21:40, 2] = 2
x[41:60, 1] = 1

trueLabels <- c(rep(1,20), rep(2, 20), rep(3, 20))
trueLabels




# part b) pca of the 60 observations and plot the first two PC score vectors
rand.pca <- prcomp(x)
summary(rand.pca)

# score vectors are in $x
# loadings are in $rotation
n <- nrow(rand.pca$x[,1:2]) # 60 observations
n

# note: have 3 colors only for 60 observations?? why
# answer: the observations are so strongly clustered (in 3 groups) that we use
# only 3 colors
plot(rand.pca$x[,1:2], col=1:3, pch=19, xlab="Z1", ylab="Z2")
# same as: plot(rand.pca$x[,1], rand.pca$x[,2])

## Success: ggplot
assignColor <- function(vec) {
      theColors <- rainbow(length(unique(vec)))
      return(theColors[as.numeric(as.factor(vec))])
}
df <- data.frame(PC1 = rand.pca$x[,1], PC2=rand.pca$x[,2], labels=assignColor(1:n))
ggplot(data=df, aes(x=PC1, y=PC2)) + geom_point(size=3, colour=df$labels)



# part c) do k-means clustering of observations with K = 3
# how well do the clusters here compare to true class labels (use table)

# do kmeans with k = 3
rand.km <- kmeans(x, 3, nstart=20)
table(True=trueLabels, Cluster=rand.km$cluster)

# perfect separation: observations are perfectly clustered. 





# part d) do kmeans with k = 2
rand.km2 <- kmeans(x, 2, nstart=20)
table(True=trueLabels, Cluster=rand.km2$cluster)

# not as well clustered. All the observations of one cluster is found in another cluster.
rand.km2$cluster
rand.km$cluster
rand.km$cluster - rand.km2$cluster # (number 3 move to 1)



# part e) kmeans k = 4
rand.km4 <- kmeans(x, 4, nstart=20)
table(True=trueLabels, Cluster=rand.km4$cluster)
# true cluster 1 is split in two: between clusters 1 and 4 in the fake clustering

rand.km$cluster - rand.km4$cluster # split in two







# part f) kmeans k = 3 on first two principal component score vectors, rather than on 
# the raw data
rand.km.pc1pc2 <- kmeans(rand.pca$x[,1:2], 3, nstart=20)
table(True=trueLabels, Cluster=rand.km.pc1pc2$cluster)
# true cluster 1 is labeled as cluster 3 and true cluster 3 is labeled as 1 but
# cluster 2 is labeled correctly for all observations that are cluster 2

# why solution says: perfectly clustered?




# part g) scale use k = 3
rand.km.scale <- kmeans(scale(x), 3, nstart=20)
table(True=trueLabels, Pred=rand.km.scale$cluster)

# here is worse than for unscaled data. 