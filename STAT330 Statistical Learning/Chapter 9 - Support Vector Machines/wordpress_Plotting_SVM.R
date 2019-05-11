# ATTEMPT 3 - best plotting but don't know how to shade the regions: 
# SOURCE: 
# https://eight2late.wordpress.com/2018/06/06/an-intuitive-introduction-to-support-vector-machines-using-r-part-1/

# Setting variables for this script
svm.fit = census.svm.radial
trainData = censusTrain 
y = censusTrain$hr_per_week # censusTrain$income 
ybinary = censusTrain$incomeBinary 
x = censusTrain$education # censusTrain$age
xName = "education"
yName = "hr_per_week"
######################################################


# adding margins and hyperplane line
# weight vector
w <- t(svm.fit$coefs) %*% svm.fit$SV
# calculate slope
slope_1 <- -w[1]/w[2]
intercept_1 <- svm.fit$rho/w[2]

# mpg, displacement, linear model

# support vectors
#df_sv <- trainData[svm.fit$index, ]
svs <- svm.fit$index 

p1 <- ggplot(data=trainData, aes(x = x, y = y, colour=ybinary)) + 
      geom_point(shape=19) + #scale_colour_manual(values=c("blue", "red")) + 
      expand_limits(x=c(0,20), y=c(0, 250)) ; p1

p2 <- p1 + geom_point(data=trainData[svs,],aes(x=x[svs],y=y[svs]),colour='purple',size = 4,alpha=0.5); p2

#p2 <- p1 + geom_point(data=trainData[svs,],aes(x=x[svs],y=y[svs]),shape=9, size=4); p2

# add layer marking out support vectors with purple plobs

p3 <- p2 + geom_abline(slope=slope_1, intercept=intercept_1); p3

# Claculate margins: their intercepts are offset by 1/w[2] units on either side of decision
# boundary
p4 <- p3 + geom_abline(slope=slope_1, intercept=intercept_1 - 1/w[2], linetype="dashed") + 
      geom_abline(slope=slope_1, intercept=intercept_1 + 1/w[2], linetype="dashed")
p4