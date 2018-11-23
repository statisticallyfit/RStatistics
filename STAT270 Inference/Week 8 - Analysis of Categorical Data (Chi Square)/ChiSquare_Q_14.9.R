############
# From Q14.19 in WMS, from Brush et al 1985
# Var 1 (column): Life-threatening condition (Y/N)
# Var 2 (row): ECG (pos/neg)
Brush1985 = matrix( c(166, 260,
                      1, 42), ncol=2)
colnames(Brush1985) = c("No", "Yes")
rownames(Brush1985) = c("Negative", "Positive")
Brush1985

# Chi Square function
chisq.test(Brush1985, correct=F)

# or, by definitions (many ways to code this)
n = sum(Brush1985)
p.1 = sum(Brush1985[,1])/n
p.2 = sum(Brush1985[,2])/n
p1. = sum(Brush1985[1,])/n
p2. = sum(Brush1985[2,])/n
E11 = n*p1.*p.1
E12 = n*p1.*p.2
E21 = n*p2.*p.1
E22 = n*p2.*p.2
E = matrix(c(E11, E21, E12, E22), ncol=2)
X2 = sum((Brush1985 - E)^2/E)
X2

1 - pchisq(X2, (nrow(Brush1985) - 1)*(ncol(Brush1985) - 1) )



###################
# another example, where columns need to be collapsed to get expected counts > 5
# Q1 (column): If I don't buy my own copy, I just do without:
# Q2 (row): Has lack of access to textbooks ever had a negative effect on you?

data = matrix( c(7,4,9,9,2, 
                 13,15,23,24,7,
                 21,14,14,19,6,
                 7,3,3,5,2,
                 6,4,5,4,0), ncol=5 )
colnames(data) = c("Never", "Rarely", "Sometimes", "Most of the time", "Always")
rownames(data) = c("Def Yes", "Prob Yes", "Prob Not", "Def Not", "Do not know")
data

data2 = matrix(NA, nrow=4, ncol=4)
for (i in 1:4) {
      data2[i,] = c(data[i,1:3], data[i,4] + data[i,5])
}
colnames(data2) = c("Never", "Rarely", "Sometimes", "Most or Always")
rownames(data2) = c("Def Yes", "Prob Yes", "Prob Not", "Def Not")
data2

chisq.test(data2)
