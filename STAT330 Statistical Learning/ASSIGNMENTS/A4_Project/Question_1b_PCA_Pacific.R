setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A4_Project")

library(ggfortify) # for ggplot version of biplot (plotting principal component score vectors)
library("ggplot2")
library("factoextra")
library(FactoMineR)
library(ggcorrplot)
library(corrplot)

# part b) PCA on pacific pacifics dataset


pacificData <- read.csv("Pacific Islands.csv")

# We predict the supplementary categorical variables at the end using PCA.
pacific.supp <- pacificData[,14]

head(pacificData)
p <- ncol(pacificData) # number of features
p
n <- nrow(pacificData) # number of observations
n

# Finding a low-dimensional representation using PCA
pacificData <- pacificData[,-1]
pacific.pca <- PCA(pacificData, quali.sup=13, scale.unit=TRUE, graph=F)
# Have min(n-1, p) = 12 principal components
pacific.pca


# Scree plot (PVE / eigenvalues) -----------------------


# Eigenvalues: amount of variance retained by each principle component is called
# its eigenvalue. 
# Eigenvalues are large for the first PCs and small
# for the subsequent PCs. That is, the first PCs corresponds to the directions with the
# maximum amount of variation in the data set
pacific.pca$eig

# --> sum of all eigen values result in a total variance of 1226, explained by all PC's
sum(pacific.pca$eig)
# --> percent/prop of variance explained by each eigenvalue
pacific.pca$eig[,2] # PVE
# --> cumulative PVE is 94% at 4th principal component
pacific.pca$eig[,3]

# Eigenvalues can be used to determine the number of principal components to retain after
# PCA (Kaiser, 1961):
#     ---> An eigenvalue > 1 indicates that PCs account for more variance than accounted
# by one of the original variables in standardized data. This is commonly used
# as a cutoff point for which PCs are retained. This holds true only when the
# data are standardized.
#     ---> You can also limit the number of component to that number that accounts
# for a certain fraction of the total variance. For example, if you are satisfied
# with 70% of the total variance explained then use the number of components
# to achieve that.

# elbow = around 4 PCs since 94% of variation in the observations is explained using
# 4 PC's. The fourth PC adds 6.68% more explanation:
pacific.pca$eig[,2]

fviz_screeplot(pacific.pca, addlabels=TRUE, barfill="powderblue", barcolor="deepskyblue", 
               ggtheme=theme_gray(), main="Percent Variation Explained")



# Graphs of individuals ---------------------------------------------
# COS2 - quality of representation for individuals
## # red are individual obs which are most strongly represented by the PC1 and PC2
fviz_pca_ind(pacific.pca, 
             col.ind="cos2", 
             geom = "point",
             pointsize="cos2") +
      scale_color_gradient2(low="white", mid="blue",
                            high="red") + theme_gray()

# CONTRIB
# Color individuals by contribution to PC

# Contribution is always between 0 and 1. 
# For a given component, the sum of the contributions of all observations = 1. 
# The larger the value of the contribution, the more the observation  contributes to the 
# principal component.
# Basing the interpretation of a component on  the observations whose contribution 
# is larger than the average contribution.
fviz_pca_ind(pacific.pca, 
             col.ind="contrib", 
             pointsize="contrib", repel=TRUE) +
      scale_color_gradient2(low="white", mid="blue",
                            high="red", 
                            midpoint=mean(us.ind$contrib)) + theme_gray()



# Graphs of variables ---------------------------------------------

# Or called variable correlation plots
# Shows relation between all variables

# INTERPRETATION of graphs of variables: 
# --> positive correlateled variables are grouped together
# --> negatively correlated variables are positioned on opposite sides of the plot
#     origin (opposed quadrants)
# --> distance between variables and the origin measures the quality of the variables
#     on this graph. Variables away from the origin are important. 


## COS 2 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Cos 2 = quality of representation of variables on a factor map
corrplot(pacific.pca$var$cos2, is.cor=F) # best

fviz_pca_var(pacific.pca, 
             col.var="cos2", 
             gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"), #c("white", "blue", "red"),
             repel=TRUE, ggtheme=theme_gray())

# INTERPRET COS2
# --> high cos2 = good representation of a variable (predictor) on the principal component
#     means the PC represents it well (goal). Then, we see the variable is positioned
#     close to the circumference of the correlation circle. 
# --> low cos2 of a variable for a PC is not perfectly represented by the PC. Then
#     the variable is close to center of circle, away from borders. 

# For a given variable, the sum of the cos2 on all the principal components is equal to one.
# If a variable is perfectly represented by only two principal components (Dim.1 & Dim.2),
# the sum of the cos2 on these two PCs is equal to one. In this case the variables will be
# positioned on the circle of correlations.
# For some of the variables, more than 2 components might be required to perfectly
# represent the data. In this case the variables are positioned inside the circle of 
# correlations


## CONTRIB +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Color by contributions to the PC
# --> variables correlated with first few pc's (PC1, PC2) are the most important
# to explaining the variability in data (highest contrib)
# --> variables not correlated with any PC of with last dimensions are variables with 
# low contribution (contrib) and are not important to data

# INTERPRET: 
# large contrib value for a component => the more the variable contributes to that component.
pacific.pca$var$contrib # Murder contributes highly to PC1 and PC4, Urban mostly for PC2

# highlight the most contributing variables to each PC (dimension)
corrplot(pacific.pca$var$contrib,  is.corr=F)

# Most contributing variables can be highlighted on the correlation plot
fviz_pca_var(pacific.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) +     # Avoid text overlapping) 
      theme_gray()



### Final biplot -----------------------------------------------

# Biplot of individuals and variables

# note: the coordinates of individuals vs variables are NOT constructed on the same space
# so we should only focus on the direction of variables (arrows) 
# but not on their absolute positions  on the plot. 

# INTERPRETATION BIPLOT: 
# --> individual obs that is on the same side of a given variable (arrow) has a high value
#     for this variable
# --> individual obs that is on the opposite side of a given variable (arrow) has low value
#     for this variable

fviz_pca_biplot(pacific.pca, repel = TRUE,
                #col.var = "#2E9FDF", # Variables colour
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                col.var="contrib", #color variables by their contribution
                col.ind = "#696969"  # Individuals colour
) + theme_gray()


### Interpreting supplementary variables -----------------------

# Predicted results (coordinates, correlation, and cos2) for the supplementaru 
# categorical variable Region
pacific.pca$quali.sup # predicted results for supplementary variables
# (in this case we have no supplementary individs)

group.region <- as.factor(pacific.supp)
fviz_pca_ind(pacific.pca,
             #habillage=group.region, 
             pointsize=3, 
             col.ind = group.region, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
) + theme_gray()

