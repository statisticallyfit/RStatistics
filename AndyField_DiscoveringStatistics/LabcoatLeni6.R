
# LABCOAT LENI
personalityData = read.delim("data/Chamorro-Premuzic.dat", header=TRUE)
head(personalityData)

personalityMatrix = as.matrix(personalityData[, c(3:dim(personalityData)[2])])
rc <- rcorr(personalityMatrix) 
# which correlations are significant?
rc$r[which(rc$P < 0.05)]

cor(personalityData$studentN, personalityData$lectureN, use="pairwise.complete.obs")
