# Exercises: Allan Bluman Elementary Statistics 8th

# Doctor
dbinom(x=3, size=10, prob=1/5)
dpois(x=3, lambda=10*1/5)

# Employment
1-pbinom(q=2, size=5, prob=0.3)
1-ppois(q=2, lambda=5*0.3)
x1 <- -2:7
plot(x1, dbinom(x1, size=5, prob=0.3), pch=19, col="blueviolet")
x2 <- -3:7
points(x2, dpois(x2, lambda=5*0.3), pch=19, col="turquoise2")
x3 <- -3:7
points(x3, dt(x3, df=3), pch=19, col="goldenrod1")
points(x3, df(x4, df1=2, df2=4), pch=19, col="indianred1")
points(x3, dgeom(x5, 0.9), pch=19, col="yellowgreen")
points(x3, dhyper(x=x3, m=10, n=20, k=15), pch=19, col="thistle1")

# Alone at Night
dbinom(x=5, size=20, prob=0.05)
dpois(x=5, lambda=20*0.05)
pbinom(q=3, size=20, prob=0.05)
1-pbinom(q=2, size=20, prob=0.05)

# Coin distribution
heads <- 0:4
prob.heads <- dbinom(x=heads, size=4, prob=1/2)
exp <- sum(heads*prob.heads)
s <- sqrt(sum(heads^2*prob.heads)-exp^2)


# ************ EXERCISES ************

# 4
dbinom(x=2, size=5, prob=0.05)
1-pbinom(2, 5, 0.05)
dbinom(5,5,0.05)

# 5
1-pbinom(14, 20, 1/2)
# 6
1-pbinom(14, 20, 1/5)
# 7
dbinom(8, 8, 0.77)
1-pbinom(4, 8, 0.77)
dbinom(3, 8, 0.77)
# 8
pbinom(2, 10, 0.103)
1-pbinom(5, 10, (1-0.103)) 
dbinom(0, 10, 0.103)
# 9
pbinom(3, 7, 0.75)
# 10
dbinom(0, 5, 0.521)
1-pbinom(3, 5, 0.521)
pbinom(1, 5, 0.521)
# 11
dbinom(2, 5, 0.4)
pbinom(3, 5, 0.4)
1-pbinom(1, 5, 0.4)
pbinom(2, 5, 0.4)
# 12
dbinom(6, 12, 0.26)
1-pbinom(5, 12, 0.26)
pbinom(4, 12, 0.26)
# 13
dbinom(5, 10, 0.53)
1-pbinom(4, 10, 0.47)
pbinom(4, 10, 0.53)
# 21
dbinom(5, 18, 0.25)
# 22
dbinom(9, 14, 0.63)
# 23
pbinom(3, 10, 1/3)
# 25
dbinom(12, 20, 0.58)
# 26
1-pbinom(2, 5, 0.13)
# 27
dbinom(2, 7, 0.14) + dbinom(3, 7, 0.14)
# 30
x <- -5:5
plot(x, dbinom(x, 4, 0.3), pch=19, col="orchid1")
