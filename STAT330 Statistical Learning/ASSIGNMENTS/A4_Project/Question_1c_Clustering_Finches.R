setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A4_Project")




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
