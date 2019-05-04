

# part a) b): 
# sketch the curve: (1 + X1)^2 + (2 - X2)^2 = 4


plot(NA, NA, type="n", xlim=c(-4, 2), ylim=c(-1, 5), asp=1, xlab="X1", ylab="X2")
symbols(c(-1), c(2), circles=c(2), add=TRUE, inches=FALSE)

text(c(-1), c(2), "< 4")
text(c(-4), c(2), "> 4")


# part c): 

# classifier assigns an observation to the Class = BLUE if > 4 and to Class = RED
# otherwise. 

# (0,0): (1 + 4 = 5 > 4) ==> BLUE
# (-1, 1): 0 + 1 = 1 < 4 ==> RED
# (2,2): 9 + 0 > 4 ===> BLUE
# (3, 8): 16 + 36 > 4 ==> BLUE
# point (0,0), (-1, 1), (2,2), (3,8)
plot(c(0, -1, 2, 3), c(0, 1, 2, 8), col=c("blue", "red", "blue", "blue"),
     type="p", asp=1, xlab="X1", ylab="X2")
symbols(c(-1), c(2), circles = c(2), add=TRUE, inches=FALSE)


# part d) argue that while boundary in (c) is not linear in X1 and X2, it is linear in
# X1, X1^2, X2, and X2^2

# simplify equation: X1^2 + X2^2 + 2X1 - 4X2 + 1 = 0 which is linear under the given variables.