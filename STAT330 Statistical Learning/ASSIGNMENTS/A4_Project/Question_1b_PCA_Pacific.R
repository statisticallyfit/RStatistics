setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A4_Project")
library(ggfortify) # for ggplot version of biplot (plotting principal component score vectors)
library("ggplot2")
library("factoextra")
library(FactoMineR)
library(ggcorrplot)
library(corrplot)

pacificData <- read.csv("Pacific Islands.csv")

# We predict the supplementary categorical variables at the end using PCA.
pacific.supp <- pacificData[,14]

# Finding a low-dimensional representation using PCA
pacificData <- pacificData[,-1]
pacific.pca <- PCA(pacificData, quali.sup=13, scale.unit=TRUE, graph=F)
# Have min(n-1, p) = 12 principal components
pacific.pca

p <- ncol(pacificData) # number of features
n <- nrow(pacificData) # number of observations

# Need to standardize because each variable has different means and variances and units
# are different so PCA will be affected.
apply(pacificData[,1:(p-1)], 2, mean)
apply(pacificData[,1:(p-1)], 2, sd)

# Scree plot (PVE / eigenvalues) -----------------------

# Eigenvalues: amount of variance retained by each principle component is called
# its eigenvalue. 
pacific.pca$eig

# --> sum of all eigen values result in a total variance of 1226, explained by all PC's
sum(pacific.pca$eig)
# --> percent/prop of variance explained by each eigenvalue
pacific.pca$eig[,2] # PVE
# --> cumulative PVE is 94% at 4th principal component
pacific.pca$eig[,3]

# elbow = around 4 PCs since 94% of variation in the observations is explained using
# 4 PC's. The fourth PC adds 6.68% more explanation:
pacific.pca$eig[,2]

fviz_screeplot(pacific.pca, addlabels=TRUE, barfill="powderblue", barcolor="deepskyblue", 
               ggtheme=theme_gray(), main="Percent Variation Explained")


# Graphs of individuals ---------------------------------------------

# COS2   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# quality of representation for individuals 
## # red are individual obs which are most strongly represented by the PC1 and PC2
pacific.pca$ind$cos2

fviz_pca_ind(pacific.pca, 
             col.ind="cos2", 
             geom = "point",
             pointsize="cos2") +
      scale_color_gradient2(low="white", mid="blue",
                            high="red") + theme_gray()

# CONTRIB  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Color individuals by contribution to PC

pacific.pca$ind$contrib

fviz_pca_ind(pacific.pca, 
             col.ind="contrib", 
             pointsize="contrib", repel=TRUE) +
      scale_color_gradient2(low="white", mid="blue",
                            high="red", 
                            midpoint=mean(pacific.pca$ind$contrib)) + theme_gray()


# Graphs of variables ---------------------------------------------

# Or called variable correlation plots # Shows relation between all variables

## COS 2 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Cos 2 = quality of representation of variables on a factor map
corrplot(pacific.pca$var$cos2, is.cor=F) 

pacific.pca$var.cos2 

fviz_pca_var(pacific.pca, 
             col.var="cos2", 
             gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"), #c("white", "blue", "red"),
             repel=TRUE, ggtheme=theme_gray())

## CONTRIB +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Color by contributions to the PC

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

# Predicted results (coordinates, correlation, and cos2) for the supplementary variables
# categorical variable Region
pacific.pca$quali.sup # predicted results for supplementary variables
# (in this case we have no supplementary individuals)

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

