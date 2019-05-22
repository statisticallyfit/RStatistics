set.seed(1)
x = cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 1, 2, 0))
x
plot(x[,1], x[,2])


# b) assign cluster label
labels = sample(2, nrow(x), replace=T)
labels

plot(x[,1], x[,2], col=(labels+1), pch=20, cex=2)


# c) centroid for each cluster => is vector of means for each cluster
centroid1 = c(mean(x[labels==1, 1]), mean(x[labels==1, 2]))
centroid2 = c(mean(x[labels==2, 1]), mean(x[labels==2, 2]))
centroid1
centroid2

# Plotting the centroids on the label plot
plot(x[,1], x[,2], col=(labels+1), pch=20, cex=2)
points(centroid1[1], centroid1[2], col=2, pch=4)
points(centroid2[1], centroid2[2], col=3, pch=4)



# d) assign each obs to centroid to which it is closest by euclidDistean distance
euclidDist = function(a, b) {
      return(sqrt((a[1] - b[1])^2 + (a[2]-b[2])^2))
}

assignObsToClosestCentroid = function(x, centroid1, centroid2) {
      labels = rep(NA, nrow(x))
      for (i in 1:nrow(x)) {
            # if euclid dist between observation i and centroid 1 is less than that
            # for obs i and centroid 2, then match obs i to centroid 1 else centroid 2
            if (euclidDist(x[i,], centroid1) < euclidDist(x[i,], centroid2)) {
                  labels[i] = 1
            } else {
                  labels[i] = 2
            }
      }
      return(labels)
}
labels = assignObsToClosestCentroid(x, centroid1, centroid2)
labels



# e) repeat computing centroid for each cluster and assigning obs to nearest cluster
# until the labels stop changing
lastLabels = rep(-1, 6)
while (!all(lastLabels == labels)) {
      lastLabels = labels
      centroid1 = c(mean(x[labels==1, 1]), mean(x[labels==1, 2]))
      centroid2 = c(mean(x[labels==2, 1]), mean(x[labels==2, 2]))
      print(centroid1)
      print(centroid2)
      labels = assignObsToClosestCentroid(x, centroid1, centroid2)
}

labels


# Plotting the centroids on the label plot (with final labels answer)
plot(x[,1], x[,2], col=(labels+1), pch=20, cex=2)
points(centroid1[1], centroid1[2], col=2, pch=4)
points(centroid2[1], centroid2[2], col=3, pch=4)