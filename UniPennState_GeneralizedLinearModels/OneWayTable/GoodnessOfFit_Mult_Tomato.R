## (I) Goodness of fit tests line by line to show formulas
tomato.obs <- c(926, 288, 293, 104); tomato.obs
n <- sum(tomato.obs); n 
tomato.props <- c(9, 3, 3, 1) / 16
tomato.exp <- n * tomato.props; tomato.exp

df <- length(tomato.obs) - 1

X2 <- sum((tomato.obs - tomato.exp)^2 / tomato.exp); X2
1 - pchisq(X2, df)

G2 <- 2 * sum(tomato.obs * log(tomato.obs / tomato.exp)); G2
1 - pchisq(G2, df)


############################################################

## (II) Automatic goodness-of-fit tests

# Chi-square test of goodness of fit
chisq.test(x = tomato.obs, p = tomato.props)

# Likelihood Ratio Test
likelihoodRatioTest(tomato.obs, tomato.exp)

# deviance residuals
dev.resid <- devianceResiduals(tomato.obs, tomato.exp); dev.resid

# nice output
output <- round(cbind(1:4, tomato$obs, tomato$exp, tomato$residuals, 
                      dev.resid), 3)
output <- as.data.frame(output)
names(output) <- c("cell.j", "Oj", "Ej", "res.j", "dev.j")
output

# plotting expected and observed values
plot(c(1:4), obs, xlab="cell index", ylab="counts", xlim=c(0, 5))
points(exp, pch=3, col="red")
legend(3, 700, c("observed", "expected"), col=c(1, "red"), pch=c(1,3))
