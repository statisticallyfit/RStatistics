setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A4_Project")

# Libraries
library(factoextra)
library(ggplot2)


# Load & attach the dataset
finchData <- read.csv("Finches.csv")
attach(finchData)

# Add a new variable, age
finchData$age <- finchData$Last_yr - finchData$First_adult_yr

# Change Last year to a categorical variable for Survival
finchData$Surv <- ifelse(Last_yr == 1977, 0, 1)

# View updated dataset
#View(finchData)

# Select QUANTITATIVE variables from the dataset
F.quant <- finchData[,5:10]

#########################
###  Textbook version ###
#########################

finch.pca =prcomp (F.quant, scale =TRUE)
names(finch.pca)

# Means of the variables prior to implementing the PCA
finch.pca$center

# SD of the variables prior to implementing the PCA
finch.pca$scale

# PC loadings
finch.pca$rotation

# Biplot
biplot (finch.pca , scale =0)

# Variance explained by each PC
pr.var <- finch.pca$sdev ^2; pr.var

# PVE
pve <- pr.var/sum(pr.var); pve

# Plot PVE
plot(pve , xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", ylim=c(0,1) , type='b')

# Plot cumulative PVE
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", ylim=c(0,1), type='b')

##########################
### Alternative script ###
##########################


# View means and variances of qualitative variables
apply(F.quant, 2, mean)
apply(F.quant, 2, var)
apply(F.quant, 2, sd)

# Compute PCA
finch.alt.pca <- prcomp(F.quant, scale = TRUE)

# View eigen values
fviz_eig(finch.alt.pca)

# Graph of individuals
fviz_pca_ind(finch.alt.pca,
             pointsize="cos2",
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Graph of variables
fviz_pca_var(finch.alt.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Biplot of individuals and variables
fviz_pca_biplot(finch.alt.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables colour
                col.ind = "#696969"  # Individuals colour
)

# Eigenvalues
eig.val <- get_eigenvalue(finch.alt.pca); eig.val

# Results for Variables
res.var <- get_pca_var(finch.alt.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

# Results for individuals
res.ind <- get_pca_ind(finch.alt.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation

# Add qualitative variables
# Sex
group.gender <- as.factor(finchData$Sex[])
fviz_pca_ind(finch.alt.pca,
             pointsize="cos2",
             col.ind = group.gender, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)

# Survived
group.survived <- as.factor(finchData$Surv[])
fviz_pca_ind(finch.alt.pca,
             col.ind = group.survived, # color by groups
             palette = c("#00AFBB", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)
