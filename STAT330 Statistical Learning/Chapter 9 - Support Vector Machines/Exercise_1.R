
# sketching hyperplane
#library(ggplot2)

# part (a)
# Hyperplane.1 = 1 + 3 * X1 - X2 = 0
x1 = -10:10
x2 = 1 + 3*x1


#df <- data.frame(x1=x1, x2=x2)
#ggplot(data=df, aes(x=x1, y=x2)) + geom_line() + geom_text()

plot(x1, x2, type="l", col="red")
text(c(0), c(-20), "greater than 0", col='red')
text(c(0), c(20), "less than 0", col="red")

# part (b)
# hyperplane - 2 + X1 + 2X2 = 0
lines(x1, 1 - x1/2)
text(c(0), c(-15), "less than 0")
text(c(0), c(15), "greater than 0")

# opposite locatiosn of less and greater text because this time we flipped the x1 and x2 (?)