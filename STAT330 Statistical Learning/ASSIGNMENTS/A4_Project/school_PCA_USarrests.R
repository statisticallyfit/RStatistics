# PCA on USArrests data - alternative script for Lab 10.2

# Install packages
#install.packages("ggplot2")
#install.packages("mvabund")
#install.packages("factoextra")#
#install.packages("GGally")
#install.packages("corrplot")
#install.packages("reshape2")

# Libraries
library("ggplot2")
library("mvabund")
library("factoextra")
library(FactoMineR)
library(GGally)
library(corrplot)
library(reshape2)

# Link names of observations to output
states = row.names(USArrests)
states

# View means and variances of qualitative variables
# NOTE: when ther eis high correlation in the data, there is high redundancy between
# variables so we can use PCA to find lower-dimensional representation. 
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
apply(USArrests, 2, sd)

# Explore variables pattern
plot(USArrests)

# Use pairs plot to look at relationships between variables
pairs(USArrests, upper.panel = NULL, pch = 16, cex = 1.75)

# Look at relative correlations
ggcorr(USArrests, palette = "RdBu", label = TRUE)

# Compute PCA
us.pca <- prcomp(USArrests, scale = TRUE)

# Eigenvalues: amount of variance retained by each principle component is called
# its eigenvalue. 
# Eigenvalues are large for the first PCs and small
# for the subsequent PCs. That is, the first PCs corresponds to the directions with the
# maximum amount of variation in the data set
us.eig <- get_eigenvalue(us.pca); us.eig

# --> sum of all eigen values given total variance of 4 retained by all PC's
sum(us.eig$eigenvalue)
# --> percent/prop of variance explained by each eigenvalue
us.eig$variance.percent # PVE

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

# Scree plot (PVE / eigenvalues)
fviz_eig(us.pca, addlabels=TRUE, barfill="lightpink", barcolor="dodgerblue", 
         ggtheme=theme_gray())

# OR
numPCs <- nrow(us.eig)
df <- data.frame(PC=1:numPCs, PVE=us.eig$variance.percent, 
                 CPVE = us.eig$cumulative.variance.percent)

ggplot(data=df, aes(x=PC, y=PVE)) + geom_line(size=1, colour="dodgerblue") +
      geom_point(size=5, alpha=0.5, color="purple") + 
      ggtitle("Scree Plot: Proportion of Variance explained")

ggplot(data=df, aes(x=PC, y=CPVE)) + 
      geom_line(size=1, colour="dodgerblue") +
      geom_point(size=5, alpha=0.5, color="purple") + 
      ggtitle("Scree Plot: Cumulative Proportion of Variance explained")




# Results for Variables
us.var <- get_pca_var(us.pca)
us.var$coord          # Coordinates of variables (??)

# cos2 = represents the quality of representation for variables on the factor map.
#us.var.cos2 = us.var.coord * us.var.coord
us.var$cos2 
# var$contrib: contains the contributions (in percentage) of the variables to the 
# principal components. (var.cos2 * 100) / (total cos2 of the component).
us.var$contrib        # Contributions to the PCs


# Results for individuals
us.ind <- get_pca_ind(us.pca)
us.ind$coord          # Coordinates same as us.pca$x (principal comp score vectors)
us.ind$cos2
us.ind$contrib        # Contributions to the PCs


# Can get these results automatically using factomineR
us2.pca <- PCA(USArrests, graph=F, scale.unit=TRUE)
print(us2.pca)
us2.pca$var$cos2



# Graph of individuals -----------------------------------------------------------

# COS2 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Color by quality of representation
# cos2 = the quality of the individuals on the factor map
# orange are individual obs which are most strongly represented by the PC1 and PC2
fviz_pca_ind(us.pca,
             col.ind = "cos2", # cos2 = individual quality
             pointsize="cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
) + theme_gray()

# Color individuals by quality of representation (?)
fviz_pca_ind(us.pca, col.ind="cos2", geom = "point",pointsize="cos2") +
      scale_color_gradient2(low="white", mid="blue",
                            high="red") + theme_gray()

# CONTRIB ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Color individuals by contribution to PC
# Color by the contributions

# From paper: 
# The value of a contribution is between 0 and 1 and,
# for a given component, the sum of the contributions
# of all observations is equal to 1. The larger the
# value of the contribution, the more the observation
# contributes to the component. A useful heuristic
# is to base the interpretation of a component on
# the observations whose contribution is larger than
# the average contribution
fviz_pca_ind(us.pca, col.ind="contrib", geom="point", pointsize=4) +
      scale_color_gradient2(low="white", mid="blue",
                            high="red", midpoint=mean(us.ind$contrib)) + theme_gray()




# Graphs of variables ----------------------------------------------------------
# Or called variable correlation plots
# Shows relation between all variables
# INTERPRETATION: 
# --> positive correlateled variables are grouped together
# --> negatively correlated variables are positioned on opposite sides of the plot
#     origin (opposed quadrants)
# --> distance between variables and the origin measures the quality of the variables
#     on this graph. Variables away from the origin are important. 


## COS 2 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Cos 2 = quality of representation of variables on a factor map
corrplot(us.var$cos2, is.corr=F)
ggcorr(us.var$cos2, palette = "RdBu", label = TRUE)
ggcorrplot(us.var$cos2, lab=T, ggtheme=theme_gray()) # best
#Also barplot of cos2
fviz_cos2(us.pca, choice="var", axes=1:2) #f or PC's 1, 2 only

fviz_pca_var(us.pca, 
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
us.var$contrib # Murder contributes highly to PC1 and PC4, Urban mostly for PC2

# highlight the most contributing variables to each PC (dimension)
ggcorrplot(us.var$contrib, lab=T, ggtheme=theme_gray()) # best

# contributions of variables to PC1
fviz_contrib(us.pca, choice="var", axes=1, top=10) # top 10 variables
# red line = expected average contribution if variables were uniform. 

# total contribution of variables to BOTH Pc1 and PC2
fviz_contrib(us.pca, choice="var", axes=1:2, top=10)

# Most contributing variables can be highlighted on the correlation plot
fviz_pca_var(us.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) +     # Avoid text overlapping) 
      theme_gray()
#mine
fviz_pca_var(us.pca, 
             col.var="contrib", 
             gradient.cols=c("white", "blue", "red"),
             repel=TRUE, ggtheme=theme_gray())

# ----------------------------------------------------------
# Biplot of individuals and variables

# note: the coordinates of individuals vs variables are NOT constructed on the same space
# so we should only focus on the direction of variables (arrows) 
# but not on their absolute positions  on the plot. 

# INTERPRETATION BIPLOT: 
# --> individual obs that is on the same side of a given variable (arrow) has a high value
#     for this variable
# --> individual obs that is on the opposite side of a given variable (arrow) has low value
#     for this variable
fviz_pca_biplot(us.pca, repel = TRUE,
                #col.var = "#2E9FDF", # Variables colour
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                col.var="contrib", #color variables by their contribution
                col.ind = "#696969"  # Individuals colour
) + theme_gray()
# Variables on dimensions 2 and 3
fviz_pca_biplot(us.pca, repel=TRUE, axes = c(2, 3))
# Individuals on dimensions 2 and 3
fviz_pca_ind(us.pca, axes = c(2, 3))

# Color individuals by groups
# The variable Species (index = 5) is removed
# before the PCA analysis
iris.pca <- prcomp(iris[, -5],  scale = TRUE)
fviz_pca_ind(iris.pca, label="none", habillage=iris$Species) + theme_gray()