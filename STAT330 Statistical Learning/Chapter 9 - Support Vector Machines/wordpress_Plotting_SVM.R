# ATTEMPT 3 - best plotting but don't know how to shade the regions: 
# SOURCE: 
# https://eight2late.wordpress.com/2018/06/06/an-intuitive-introduction-to-support-vector-machines-using-r-part-1/


# adding margins and hyperplane line
# weight vector
w <- t(svm.final.linear$coefs) %*% svm.final.linear$SV
# calculate slope
slope_1 <- -w[1]/w[2]
intercept_1 <- svm.final.linear$rho/w[2]

# mpg, displacement, linear model
p1 <- ggplot(data=autoTrain, aes(x = displacement, y = mpg, colour=mpgBinary)) + 
      geom_point(shape=19) + scale_colour_manual(values=c("blue", "red"))

# support vectors
df_sv <- autoTrain[svm.final.linear$index, ]

# add layer marking out support vectors with purple plobs
p2 <- p1 + geom_point(data = df_sv, aes(x=displacement, y=mpg), shape=9, size=4) + 
      scale_colour_manual(values=c("blue", "red"))
# , colour="purple", shape=9, size=4)

p3 <- p2 + geom_abline(slope=slope_1, intercept=intercept_1)

# Claculate margins: their intercepts are offset by 1/w[2] units on either side of decision
# boundary
p4 <- p3 + geom_abline(slope=slope_1, intercept=intercept_1 - 1/w[2], linetype="dashed") + 
      geom_abline(slope=slope_1, intercept=intercept_1 + 1/w[2], linetype="dashed")
