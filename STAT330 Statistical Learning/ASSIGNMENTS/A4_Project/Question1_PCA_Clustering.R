setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A4_Project")

library(ggfortify) # for ggplot version of biplot (plotting principal component score vectors)


# part b) PCA on pacific pacifics dataset

pacificData.orig <- read.csv("Pacific Islands.csv")
# removing the categorical variables
pacificData <- pacificData.orig[,2:13]
head(pacificData)

p <- ncol(pacificData) # number of features
p
n <- nrow(pacificData) # number of observations
n


# Finding a low-dimensional representation using PCA
pacific.pca <- prcomp(pacificData, scale=TRUE)
# Have min(n-1, p) = 12 principal components
pacific.pca

# Describe in table
summary(pacific.pca)

# x = holds the score vectors
head(pacific.pca$x); nrow(pacific.pca$x) # is 22 by 12 (n x p)

# rotation = holds the loading vectors. Each col contains the corresponding
# principal component loading vector
head(pacific.pca$rotation); nrow(pacific.pca$rotation) # is 12 by 12 (p x p)



# Calculate the PVE (variance explained by each principal component)
v <- pacific.pca$sdev^2 # this is the variance explained by each principal component
v
pve <- v / sum(v); pve # proportion of variance explained by each PC

# Scree Plots: PVE and CPVE
numPCs <- dim(pacific.pca$x)[2]
df <- data.frame(PC=1:numPCs, PVE=pve, CPVE = cumsum(pve))

ggplot(data=df, aes(x=PC, y=PVE)) + geom_line(size=1, colour="dodgerblue") +
      geom_point(size=3) + ggtitle("Scree Plot: Proportion of Variance explained")

ggplot(data=df, aes(x=PC, y=CPVE)) + 
      geom_line(size=1, colour="dodgerblue") + geom_point(size=3) + 
      ggtitle("Scree Plot: Cumulative Proportion of Variance explained")
# elbow = around 4 PCs since 90% of variation in the observations is explained using
# 4 PC's. The fourth PC adds 6.68% more explanation:
pve[4]



# Plot the first two principal components
# scale = 0 ensures arrows are scaled to represent the loading vectors
#biplot(pacific.pca, scale=0)
autoplot(pacific.pca, label = TRUE, loadings.label = TRUE, scale=0)


# Or can choose which ones to plot

# Assigns a distinct color to each element of a numeric vector. 
# Will be used to assign a color to each of the 64 cell lines, based on cancer type
# to which it corresponds
assignColor <- function(vec) {
      theColors <- rainbow(length(unique(vec)))
      return(theColors[as.numeric(as.factor(vec))])
}


df <- data.frame(Z1=pacific.pca$x[,1], 
                 Z2=pacific.pca$x[,2], 
                 Z3=pacific.pca$x[,3], 
                 Z4=pacific.pca$x[,4], labels = pacificData.orig$Region)
                 #labels=assignColor(1:n))

# TODO: interpretations: 
# https://stats.stackexchange.com/questions/2038/interpretation-of-biplots-in-principal-components-analysis

# TODO more interp
# melanoma word graph: 
# https://rstudio-pubs-static.s3.amazonaws.com/93706_e3f683a8d77244a5b993b20ad6278f4b.html
ggplot(data=df, aes(x=Z1, y=Z2, colour = labels)) + geom_point(size=3) + ggtitle("PC1 and PC2")
# old multicolor plot
#ggplot(data=df, aes(x=Z1, y=Z3)) + geom_point(size=3, colour=df$labels, fill=df$labels) + 
#      ggtitle("PC1 and PC3")
ggplot(data=df, aes(x=Z1, y=Z4)) + geom_point(size=3, colour=df$labels, fill=df$labels) + 
      ggtitle("PC1 and PC4")





# part c) -------------------------------------------------------------------------------

## KMEANS CLUSTERING ------------------------------------------------------------------

# clustering factor points
#ggplot(data=pacificData.orig, aes(x=Inshore_fishing_area, y=Coral_reefs, color=factor(kmeans(pacificData, 3, nstart=20)$cluster))) + geom_point(size=3)

# FINCh data
finchData.orig <- read.csv("Finches.csv")
finchData <- finchData.orig[,-2]
head(finchData)


kmeansModels <- list()
bestModel <- NULL
for(k in 1:10){
      modelK <- kmeans(finchData, k, nstart=20)
      kmeansModels[[k]] = modelK
      
}


# Best model is for with tot within ss is smallest
bestK <- which.min(kmeansModels)

      #------
pacific.km <- kmeans(pacificData, 3, nstart=20)
pacific.km$cluster

# tot.withinss = 
# This is the total within-cluster sum of squares, seek to minimize (equation 10.11)
pacific.km$tot.withinss
# withinss = individual within cluster variation, W(Ck)
pacific.km$withinss

## HIERARCHICAL CLUSTERING ----------------------------------------------------------------
