library(ISLR)
library(ggfortify)



# PRINCIPAL COMPONENTS --------------------------------------------------------------------


data("USArrests")

head(USArrests)

# Notice: the variables have very different means
# col = 2
apply(USArrests, 2, mean)

# vastly different variances: UrbanPop measures percentage of population in each state living
# in an urban area, which is not ocmparable to number of rpes in each state per 100,000 people
# If no scale, then most of the principla components would be driven by the Assault variable
# since it has largest mean and variance. 
apply(USArrests, 2, sd)

# Need to standardize before PCA
arrest.pca <- prcomp(USArrests, scale=TRUE)
arrest.pca
summary(arrest.pca) # the first principal component PC1 explains 62% of variation in data.

# center and scale parts of the object correspond to the means and stdevs of the
# variables used for scaling prior to doing PCA
arrest.pca$center
arrest.pca$sdev

# rotation matrix gives the principal component loadings. Each col contains the corresponding
# principal component loading vector
arrest.pca$rotation

# There are 4 principal components = min(n - 1, p)
# p = 4, n = 50
nrow(USArrests)

# These are the principal component score VECTORS, Z1, Z2, ...
dim(arrest.pca$x)
arrest.pca$x



# can plot the first two principal components
# scale = 0 ensures arrows are scaled to represent the loadings
biplot(arrest.pca, scale=0)

# Or with ggplot
# scale is automatic.
# Here, setting scale = 0, makes tthe loadings unscaled. 
autoplot(arrest.pca, label = TRUE, loadings.label = TRUE)



# Another way to produce (principal components are unique up to a sign change):
arrest.pca2 <- arrest.pca
arrest.pca2$rotation <- -arrest.pca$rotation # sign flip the loadings
arrest.pca2$x <- -arrest.pca$x # sign flip the score vectors
autoplot(arrest.pca2, label=TRUE, loadings.label=TRUE)


# Standard deviation of each principal component: 
arrest.pca$sdev


# Calculate the PVE (variance explained by each principal component)
v <- arrest.pca$sdev^2 # this is the variance explained by each principal component
v
pve <- v / sum(v); pve # proportion of variance explained by each PC

summary(arrest.pca)


# Plotting the scree plot
numPCs <- dim(arrest.pca$x)[2]
df <- data.frame(PrincipalComponent=1:numPCs, PVE=pve, CPVE = cumsum(pve))

ggplot(data=df, aes(x=PrincipalComponent, y=PVE)) + geom_line(size=1, colour="dodgerblue") +
      geom_point(size=3) + ggtitle("Scree Plot: Proportion of Variance explained")

ggplot(data=df, aes(x=PrincipalComponent, y=CPVE)) + 
      geom_line(size=1, colour="dodgerblue") + geom_point(size=3) + 
      ggtitle("Scree Plot: Cumulative Proportion of Variance explained")




# CLUSTERING ------------------------------------------------------------------------------

# making two clusters in the data (with only 2 predictors)
set.seed(2)
x = matrix(rnorm(50*2), ncol=2)
x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 2] = x[1:25, 2] - 4
x <- data.frame(x)

# 2 = means identify 2 clusters
data.km <- kmeans(x, 2, nstart=20)
data.km
names(data.km)
# the cluster assignments out of the 50 observations: 
data.km$cluster

# The algorithm perfectly separated the observations into 2 clusters though we 
# did not give group info: 
x$cluster <- factor(data.km$cluster)
head(x)
ggplot(data=x, aes(x=X1, y=X2, color=cluster)) + geom_point(size=3)



# In real life we don't know how many clusters there are
set.seed(4)
data.km2 <- kmeans(x[,1:2], 3, nstart=20)
data.km2$cluster
data.km2

# Plot
x$cluster2 <- factor(data.km2$cluster)
head(x)
ggplot(data=x, aes(x=X1, y=X2, color=cluster2)) + geom_point(size=3)

### NSTART = number of initial cluster assignments. If nstart > 1, kmeans algo will
# be performed using multiple random assignments in Step 1, and kmeans() reports only
# the best results. 

# Comparing nstart=1 and nstart=20
set.seed(3)
data.km_nstart1 <- kmeans(x[,1:2], 3, nstart=1)
data.km_nstart1$tot.withinss
data.km_nstart20 <- kmeans(x[,1:2], 3, nstart=20)
data.km_nstart20$tot.withinss # smaller for larger nstart

# tot.withinss = 
# This is the total within-cluster sum of squares, seek to minimize (equation 10.11)

# withinss = individual within cluster variation, W(Ck)

# MORAL: need to always run kmeans using large nstart (20 or 50) else undesirable
# local optimum is obtained. 



# HIERARCHICAL CLUSTERING ----------------------------------------------------------------

# use dist() to compute 50x50 inter-observation Euclidean distance matrix
# linkage = complete
hc.complete <- hclust(dist(x[,1:2]), method="complete")
hc.complete
names(hc.complete)

# linkage = average
hc.average <- hclust(dist(x[,1:2]), method="average")
# linkage = single
hc.single <- hclust(dist(x[,1:2]), method="single")

# Plot dendrograms. Numbers at bottom identify each observation
par(mfrow=c(1,3))
plot(hc.complete, main="Complete Linkage", xlab="", sub="", cex=0.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=0.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=0.9)

library(ggdendro)
ggdendrogram(hc.complete, rotate = FALSE, size = 2, theme_dendro = F)


# Find the cluster labels for each observation associated with a given CUT: 
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

# single linkage identifies one point as belonging to its own cluster
cutree(hc.single, 4) # two singletons


# Scaling before doing clustering: 
x.scaled <- scale(x[,1:2])
head(x.scaled)

ggdendrogram(hclust(dist(x.scaled), method="complete"), rotate=FALSE, theme=F, size=2)


# Correlation-based distance can be computed using as.dist() which converts a square
# symmetric matrix into a form hclust() recognizes as distance matrix. Only makes
# sense for data with >= 3 features since absolute correlation between any two obs
# with measurements on two features is always 1. 
x = matrix(rnorm(30*3), ncol=3)
dd <- as.dist(1 - cor(t(x)))
ggdendrogram(hclust(dd, method="complete"), rotate=FALSE, theme=F, size=3) +  
             labs(title="Complete Linkage with Correlation-Based Distance")






# NC160 DATA EXAMPLE --------------------------------------------------------------------

data("NCI60")
names(NCI60)

nci.labs <- NCI60$labs
nci.data <- NCI60$data

dim(nci.data)

# Examine the cancer types for the cell lines
nci.labs[1:4]
nci.labs

table(nci.labs)

#### PCA ON NC160 --------------------------------------------------------------------
nci.pca <- prcomp(nci.data, scale=TRUE)
names(nci.pca)

# There are 64 Principal components, min of n-1 and p

# loading vector
head(nci.pca$rotation)
# score vectors
head(nci.pca$x)

# Plot the first few principal component score vectors 
# Observations (cell lines) corresponding to a given cancer type will be plotted in the
# same color to see to what extent the observations within a cancer type are similar
# to each other. 

# Assigns a distinct color to each element of a numeric vector. 
# Will be used to assign a color to each of the 64 cell lines, based on cancer type
# to which it corresponds
assignColor <- function(vec) {
      theColors <- rainbow(length(unique(vec)))
      return(theColors[as.numeric(as.factor(vec))])
}

# Plotting the principal component score vectors
par(mfrow=c(1,2))

length(nci.labs)
nrow(nci.pca$x)

plot(nci.pca$x[,1:2], col=assignColor(nci.labs), pch=19, xlab="Z1", ylab="Z2")
plot(nci.pca$x[,c(1,3)], col=assignColor(nci.labs), pch=19, xlab="Z1", ylab="Z3")


# Or ggplot 
# Got help on how to get different color per point from this source: 
# https://github.com/slowkow/ggrepel/issues/82
df <- data.frame(Z1=nci.pca$x[,1], Z2=nci.pca$x[,2], 
                 Z3=nci.pca$x[,3], labels=assignColor(nci.labs))

ggplot(data=df, aes(x=Z1, y=Z2)) + geom_point(size=3, colour=df$labels, fill=df$labels) + 
      ggtitle("PC1 and PC2")
ggplot(data=df, aes(x=Z1, y=Z3)) + geom_point(size=3, colour=df$labels, fill=df$labels) + 
      ggtitle("PC1 and PC3")

# Cell lines corresponding to a single cancer type tend to have similar values on the first
# few princiapl component score vectors. Shows that cell lines from the same canceer
# type tend to have similar gene expression levels. 

summary(nci.pca)

#plot(nci.pca) # plotting the variance explained (first row) by the first few components.
# squaring values rom pca$sdev





# Better to plot PVE and cumulative PVE
pve = 100*nci.pca$sdev^2 / sum(nci.pca$sdev^2)
head(pve)

summary(nci.pca)$importance
# Another way: pve
summary(nci.pca)$importance[2,] #pve
summary(nci.pca)$importance[3,] #cpve


numPCs <- dim(nci.pca$x)[2]; numPCs
df <- data.frame(PC=1:numPCs, PVE=pve, CPVE = cumsum(pve))

ggplot(data=df, aes(x=PC, y=PVE)) + geom_line(size=1, color="dodgerblue") +
      geom_point(size=3, color="navyblue", alpha=0.5) + ggtitle("Scree Plot: Proportion of Variance explained")

ggplot(data=df, aes(x=PC, y=CPVE)) + 
      geom_line(size=1, color="dodgerblue") + geom_point(size=3, color="navyblue", alpha=0.5) + 
      ggtitle("Scree Plot: Cumulative Proportion of Variance explained")

# See from CPVE: the first seven principal components explain around 40% of the variance
# in the data. Not huge amount. 
# From PVE: the seventh principal component explains around 3 % of variance in the data
# Elbow poitn of diminishing returns: after the seventh principal component. 


#### Hierarchical Clustering ON NC160 -----------------------------------------------------

# Goal: see if the observations cluster into distinct types of cancer. 
# Standardize variables to mean zero and sd = 1

nciScaledData <- scale(nci.data)
mean(nciScaledData)
sd(nciScaledData)

# Do hierarchical clustering with complete, single, average linkage
# euclidean distance as dissimilarity measure. 
dd <- dist(nciScaledData)

par(mfrow=c(1,1))
plot(hclust(dd, "complete"), labels=nci.labs, xlab="", sub="", ylab="")
plot(hclust(dd, "average"), labels=nci.labs, xlab="", sub="", ylab="")
plot(hclust(dd, "single"), labels=nci.labs, xlab="", sub="", ylab="")

ggdendrogram(hclust(dd, method="complete"), rotate=FALSE, theme=F, size=2) + 
      labs(title="Complete Linkage with Euclidean Distance")
ggdendrogram(hclust(dd, method="average"), rotate=FALSE, theme=F, size=2) + 
      labs(title="Complete Average with Euclidean Distance")
ggdendrogram(hclust(dd, method="single"), rotate=FALSE, theme=F, size=2) + 
      labs(title="Complete Single with Euclidean Distance")

# Single linkage = gives trailing clusters: large clusters onto which individual 
# obs attach one by one
# Complete and average yield balanced clusters. 

# Answer to question: yes cell lines within single cancer type do cluster together
# breast together, melaoma together, ovarian together, leukemia together. 


# Can cut the dendrogram at the height to yield a particular number of clusters
hc.cancer <- hclust(dd) #using complete linkage
names(hc.cancer)
hc.cutree <- cutree(hc.cancer, 4) # cut tree to yield 4 clusters
hc.cutree

table(hc.cutree, nci.labs)
# Leukemia cell lines: all fall in cluster 3
# Breast cell lines: spread out over three different clusters

# Plotting the cut
par(mfrow=c(1,1))
plot(hc.cancer, labels=nci.labs)
abline(h=139, col="red") # height 139 produces 4 cluster groups (guess it by eye)



### KMEANS CLUSTERING ------------------------------------------------------------

set.seed(2)
# trying to identify 4 clusters in the data
nci.kmeans <- kmeans(nciScaledData, 4, nstart=20)
names(nci.kmeans)

nci.kmeans$cluster
table(KMClusters=nci.kmeans$cluster, HCClusters=hc.cutree) # comparing hierarch cluster with kmeans cluster

# See: the four clusters obtained from hierarchical and kmeans are different: 
# Cluster 2 in kmeans (vertical) is identical to cluster 3 in hierarchical clustering (???)
# But other clusters differ: 
# --> cluster 4 in kmeans contains a portion of observations assigned to cluster 1 by 
#     hierarchical clustering, and all observations assigned to cluster 2 by hierarch.



# Doing hierarchical clustering on just the first few princiapl component score vectors
# instead of entire data matrix:
nci.hc.smaller <- hclust(dist(nci.pca$x[, 1:5]))
nci.hc.smaller

plot(nci.hc.smaller, labels=nci.labs, main="Hier. Clust. on First Five Score Vectors")
# or ggplot
nci.hc.smaller$labels <- nci.labs
ggdendrogram(nci.hc.smaller, theme_dendro = F, rotate=F)


table(HCFirstFive = cutree(nci.hc.smaller, 4), Labels=nci.labs)
# cutree on smaller cluster to yield 4 clusters



# Results are different: from before, using full data
table(HCAllData=hc.cutree, Labels=nci.labs)
