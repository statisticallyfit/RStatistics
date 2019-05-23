setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A4_Project")
library("factoextra")
library("cluster")
library("NbClust")
library("ggplot2")
library("dendextend")
library("corrplot")

## KMEANS CLUSTERING ------------------------------------------------------------------

# clustering factor points
#ggplot(data=pacificData.orig, aes(x=Inshore_fishing_area, y=Coral_reefs, color=factor(kmeans(pacificData, 3, nstart=20)$cluster))) + geom_point(size=3)

# FINCh data
finchData <- read.csv("Finches.csv")
finchData <- na.omit(finchData)
group.gender <- finchData[,2]
finchData <- finchData[,-2] # removing the categorical gender variable


# Scaling data
apply(finchData, 2, mean)
apply(finchData, 2, sd)
# We know the X variables have very different variances , for instance Band has
# variance 1453 while others near 1, so we must standardize 
finchData.scaled <- scale(finchData)

head(finchData)


# Exploratory analysis

# Visualizing relative relationships between observations - creates a heatmap of the 
# relative similarity of each of  observations.
# Values in orange have the highest correlation whereas values in aqua have the lowest 
# correlation.
finch.dist <- get_dist(finchData.scaled, stand = TRUE, method = "pearson")
fviz_dist(finch.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


# Find optimal K (number of kmeans groups to use)
# This method actually uses 30 different indices and then reports
# the frequency of best choice for each.  

# We get best clustering scheme from the different results obtained by varying all 
# combinations of number of clusters, distance measures, and clustering methods.
set.seed(125)
# use min number of clusters = 2, and max = 10
finch.nbclust <- NbClust(finchData.scaled, distance = "euclidean", 
                         min.nc = 2, max.nc = 10, method = "kmeans", index ="all") 
fviz_nbclust(finch.nbclust) + theme_gray()
# by majority rule, choose k = 2 

# Visualize a K-means cluster using K = 2
finch.km <- kmeans(finchData.scaled, 2, nstart = 25)
fviz_cluster(finch.km, data = finchData.scaled, ellipse.type = "convex")+theme_gray()

# See which observations are in which cluster
finch.km$cluster

# tot.withinss = 
# This is the total within-cluster sum of squares, seek to minimize (equation 10.11)
finch.km$tot.withinss

# Conclusion: optimal number of clusters = 2





## HIERARCHICAL CLUSTERING ------------------------------------------------------------------



## HIERARCHICAL CLUSTERING ----------------------------------------------------------------
