
# Question 1 - plotting the joint pdf using R. 
N <- 128
x1 <- x2 <- seq(0, 1, length=N)
const <- 6/5

var.grid <- expand.grid("x1" =x1, "x2"=x2)# create grid of bivarate rvs
X1 <- var.grid$x1
X2 <- var.grid$x2

# Joint pdf formula f(x,y) = c(x + y^2) for 0 <= X <= 1, 0 <= Y <= 1
jointPDF <- const * (X1 + X2^2)
jointPDF <- matrix(jointPDF, ncol=N, nrow=N)
jointPDF # typecast as matrix. 

x11()
persp(z=jointPDF, y=x1, x=x2, col="lightblue") # perspective plot

x11()
image(jointPDF, xlab="x1", ylab="x2", col=heat.colors(256))
# image view
axis(side=1, at = seq(0,1,by=0.2), labels=seq(0,1,length=6))
axis(side=2, at = seq(0,1,by=0.2), labels=seq(0,1,length=6))
contour(jointPDF, nlevels=10, add=TRUE)


# Question 2 - find P(0 <+ X1 <= 0.5, 0 <= Y <= 0.5) using a Riemann summ. 

indices <- which(X1 <= 0.5 & X2 <= 0.5)
head(indices, 20)
regionOfInterest <- jointPDF[indices] # extract the joint pdf values where X1 <= 0.5
# and where X2 <= 0.5
regionOfInterest
prob <- sum(regionOfInterest * (1/N^2)) # calculate probability using a riemann sum. 
prob
