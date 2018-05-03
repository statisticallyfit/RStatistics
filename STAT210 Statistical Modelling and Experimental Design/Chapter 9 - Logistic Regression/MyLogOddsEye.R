
eyeTable <- as.table(matrix(c(20,30,10,15,10,25,15,12,20,10),byrow = TRUE, nrow=2))
rownames(eyeTable) <- c("Female", "Male")
colnames(eyeTable) <- c("Black", "Brown", "Blue", "Green", "Gray")

eyeTable

oddsRatio(eyeTable)

rowOdds(eyeTable)
colOdds(eyeTable)
