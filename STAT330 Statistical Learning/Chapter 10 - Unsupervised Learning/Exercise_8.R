
data("USArrests")

# part a) compute PVE using the sdev way

arrest.pca <- prcomp(USArrests, scale=TRUE)


# Calculate the PVE (variance explained by each principal component)
v <- arrest.pca$sdev^2 # this is the variance explained by each principal component
v
pve <- v / sum(v); pve # proportion of variance explained by each PC




# part b) using formula 10.8 directly

# rotation matrix gives the principal component loadings. 
# Each col contains the corresponding principal component loading vector
loadings <- arrest.pca$rotation
USArrests.scaled <- scale(USArrests)
# apply sum on the column of usarrests.scaled squared matrix
sumVar <- sum(apply( (USArrests.scaled)^2, 2, sum))

# apply sum on the colum of the matrix resulting from multiplying scaled obs with loadings
apply( (USArrests.scaled %*%  loadings) ^ 2, 2, sum) / sumVar 
