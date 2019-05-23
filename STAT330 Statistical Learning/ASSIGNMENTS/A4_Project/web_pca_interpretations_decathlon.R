setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A4_Project")

library(FactoMineR)
library(FactoInvestigate)


data(decathlon)
decat.pca = PCA(decathlon, quanti.sup = 11:12, quali.sup=13, graph=FALSE)

fviz_pca_var(decat.pca)

# Customizing -----

# Change color of variables
fviz_pca_var(decat.pca,
             col.var = "black",     # Active variables
             col.quanti.sup = "red" # Suppl. quantitative variables
)
# Hide active variables on the plot, 
# show only supplementary variables
fviz_pca_var(decat.pca, invisible = "var")
# Hide supplementary variables
fviz_pca_var(decat.pca, invisible = "quanti.sup")
#-----------------------

Investigate(decat.pca)
