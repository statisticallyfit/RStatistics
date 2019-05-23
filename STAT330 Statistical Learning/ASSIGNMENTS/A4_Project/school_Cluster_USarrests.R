# This script runs a cluster analysis on the USArrests dataset

# These packages need to be installed to run this analysis
#install.packages("factoextra")
#install.packages("cluster")
#install.packages("NbClust")
#install.packages("ggplot2")
#install.packages("dendextend")
#install.packages("corrplot")

# Above packages then need to be loaded into RStudio
library("factoextra")
library("cluster")
library("NbClust")
library("ggplot2")
library("dendextend")
library("corrplot")

# Data entry and preparation.  Note I've renamed the dataset my_data
data("USArrests")
my_data <- USArrests

# Remove any missing value (i.e, NA values for not available).  The USArrests data does not have any missing values 
# and so this step is not needed.  However, if you were working on any other data set that did this is a necessary
# step as you will have trouble running your analysis if you don't omit missing data.
my_data <- na.omit(my_data)

# Scale variables.  We know the X variables have very different variances and so this step is needed in order to 
# standardize the variables
my_data <- scale(my_data)

# View the firt 3 rows of the dataset
head(my_data, n = 3)

# Visualizing relative relationships between observations - creates a heatmap of the relative similarity of each of 
# the 50 US states.  Values in orange have the highest correlation whereas values in aqua have the lowest correlation.
res.dist <- get_dist(USArrests, stand = TRUE, method = "pearson")
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# K-means clustering

# Provides you with a calculated guess on the best number of clusters to use.  Optimal K is based partly on the Gap
# statistic (y-axis) as well as other properties.  This graphic suggests that you use K = 3 however both K = 4 and 
# K = 5 look somewhat favourable as well.
fviz_nbclust(USArrests, kmeans, method = "gap_stat")

# This is another way of finding a best guess for K.  This method actually uses 30 different indices and then reports
# the frequency of best choice for each.  This process suggests K = 2 and then K = 4. 
set.seed(123)
res.nbclust <- NbClust(USArrests, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all") 
factoextra::fviz_nbclust(res.nbclust) + theme_minimal()

# Visualize a K-means cluster using K = 3.  You can come back to this line and re-run the analysis (and visualization)
# using other values of K
km.res <- kmeans(my_data, 3, nstart = 25)
fviz_cluster(km.res, data = my_data, ellipse.type = "convex")+theme_minimal()

# Compute PAM (Partitioning Around Medoids (robust alternative to k-means clustering, less sensitve to outliers).
# K = 3 has been selected again.  You'll note that in this case the results are identical.
pam.res <- pam(my_data, 3)
fviz_cluster(pam.res)

# Hierarchical clustering

# Compute dissimilarity matrix
d <- dist(my_data, method = "euclidean")

# Hierarchical clustering using single linkage
res.hcS <- hclust(d, method = "single" )
S <- plot(res.hcS, cex = 0.6)

# From looking at your dendrograms you might then choose one and decide to assign groups based
# on a logical looking number of clusters.  So, from your dendrogram choose a value for K and 
# then visualize it as follows.

# Choose K = 4 in the dendrogram with single linkage
grp <- cutree(res.hcS, k = 4)
# Delineate groups using a rectangle for each cluster
rect.hclust(res.hc, k = 4, border = 2:5)
# An alternative way to distinguish groups
res <- hcut(USArrests, k = 4, stand = TRUE)
fviz_dend(res, rect = TRUE, cex = 0.5, k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))

# You can then look at other linkages and repeat the process of distinguishing groups.

# Hierarchical clustering using complete linkage
res.hcC <- hclust(d, method = "complete" )
C <- plot(res.hcC, cex = 0.6)

# Hierarchical clustering using average linkage
res.hcA <- hclust(d, method = "average" )
A <- plot(res.hcA, cex = 0.6)


# Hierarchical clustering using centroid linkage
res.hcCEN <- hclust(d, method = "centroid" )
CEN <- plot(res.hcCEN, cex = 0.6)

# Also, note that the different dendrograms are presented on different scales (i.e., the y-axis "Height" 
# is not consistent).  If you want to compare these 4 output on the same plot you should 
# change this.


# To compare dendrograms you could use the following
dend1 <- as.dendrogram (res.hcS)
dend2 <- as.dendrogram (res.hcC)
dend3 <- as.dendrogram (res.hcA)
dend4 <- as.dendrogram (res.hcCEN)

# Create a list of dendrograms
# Create a list of dendrograms
dend_list1 <- dendlist(dend1, dend2, dend3, dend4)

# To compare two at a time you could use the following - it connects the labels by lines
# yet it's rather messy due to the high number of observations.  The "Unique" nodes (i.e., 
# what occurs in one of the dendrograms but not the other) are shown with dashed lines.
tanglegram(dend1, dend2)

# The quality of alignment of the two trees can be measured by a metric known as entanglement.  This is a bit 
# more helpful and you can see some of the adjustments that have been made ot the output also.
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list1), 2))
)

# Compute the relative correlation between different linkage methods
dend_list2 <- dendlist("Single" = dend1, "Complete" = dend2, "Average" = dend3, "Centroid" = dend4)
cors <- cor.dendlist(dend_list2)
# View correlation matrix
round(cors, 2)
# Visualize the correlation matrix using corrplot package
corrplot(cors, "pie", "lower")



