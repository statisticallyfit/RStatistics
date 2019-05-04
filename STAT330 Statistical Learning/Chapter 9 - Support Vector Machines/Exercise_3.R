
# Maximal Margin classifier on Toy Data Set: 

# a) 
x1 = c(3,2,4,1,2,4,4)
x2 = c(4,2,4,4,1,3,1)
y <- c("red", "red", "red", "red", "blue","blue","blue")

plot(x1, x2, col=y, xlim=c(0, 5), ylim=c(0, 5))


# (b) what is optimal separating hyperplane, provide equation

### plane must pass through (4, 3.5) and (2, 1.5) so calculate: 

# slope = m = (y2 - y1)/(x2 - x1) = (1.5 - 3.5) / (2 - 4) = -2 / -2 = 1

# y - y1 = m(x - x1)
# y - 3.5 = x - 4
# y = x - 0.5

# HYPERPLANE IS: X1 - X2 = 0.5

plot(x1, x2, col=y, xlim=c(0,5), ylim=c(0,5))
abline(a = -0.5, b = 1) # b = slope, a = intercept

# This line also maximizes the marginal space between the points where it separates. 


# c) 

# classify to RED if (-0.5 + X1 - X2 < 0)    # since y was flipped (eqiuv: X2 - X1 + 0.5 > 0)
# classify to BLUE if (-0.5 + X1 - X2 > 0)


# d) indicate the margin for the maximal margin hyperplane
plot(x1, x2, col=y, xlim=c(0,5), ylim=c(0,5))
# b = slope, a = intercept
abline(a = -0.5, b = 1) # the MMC boundary hyperplane
abline(a = 0, b = 1, lty=2) # upper margin
abline(a = -1, b = 1, lty=2) # lower margin


# e)  indicate the support vectors (points lying exactly on the margins or on the wrong side
# of the margin)

# In this case the support vectors are only lying on the margin (none on the wrong side of the margin)

# points: (2,1), (2,2), (4,3), (4,4)




# f) argue that a slight movement of the seventh observation would not affect the
# MMH (maximal margin hyperplane)

# seventh obs. = (4,1), which is not a support vector so it won't affect the MMH



# g) sketch a hyperplane that is not the optimal separating hyperplane and give equation. 

# any shift that is less 0.5 in magnitude up or down between (2,1), (2,2) and (4,3), (4,4)
# is not optimal 

# Equation: X1 - X2 - 0.2 = 0
plot(x1, x2, col=y, xlim=c(0,5), ylim=c(0,5))
# b = slope, a = intercept
abline(a = -0.2, b = 1) # the MMC boundary hyperplane
# no margins required?




# h) draw another obs to make the points not linearly separable. 
points(2, 2.1)