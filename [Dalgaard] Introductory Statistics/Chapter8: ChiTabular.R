library(ISwR)
" Test that p-hat=39/215 have asthma but null hypothesis: p=0.15"

# Single Proportion Test
prop.test(39, 215, 0.15)

# Two Proportion Test
lewitt.machin.success <- c(9, 4)
lewitt.machin.total <- c(12,13)
prop.test(lewitt.machin.success, lewitt.machin.total)
prop.test(lewitt.machin.success, lewitt.machin.total, correct=FALSE)

# Binomial Test
binom.test(39, 215, 0.15)
1-pbinom(q=38, size=215, prob=0.15)

# Fisher test (for hypergeometric)
" Situation: 13 white and 12 black balls, taking 9 white and 4 black"
lewitt.machin <- matrix(c(9, 4, 3, 9), nrow=2)
fisher.test(lewitt.machin)

# Chisquared test
# 1
chisq.test(lewitt.machin)
# 2
caff.marital <- matrix(c(652,1537,598,242,36,46,38,21,
                         218, 327, 106, 67),
                       nrow=3,
                       byrow=TRUE)
colnames(caff.marital) <- c("0", "1-150", "151-300", ">300")
rownames(caff.marital) <- c("Married", "Prev.married", "Single")
caff.marital
chisq.test(caff.marital)
E <-chisq.test(caff.marital)$expected
O <- chisq.test(caff.marital)$observed
(O-E)^2/E

# Chi square test on non-tabulated data
attach(juul)
chisq.test(tanner, sex)


# k proportions, test for trend
"Situation: mother shoe size and if birth from caesarean section"
caesar.shoe
caesar.shoe.yes <- caesar.shoe["Yes",]
caesar.shoe.total <- margin.table(caesar.shoe, 2)
prop.test(caesar.shoe.yes, caesar.shoe.total)

# test for (linear assumed) trend
prop.trend.test(caesar.shoe.yes, caesar.shoe.total)





# *************************** EXERCISES *************************


# 1
n <- 10; p <- 0.2; mu <- n*p; sigma <- sqrt(n*p*(1-p))
range <- floor(mu+c(-1,1)*4*sigma)
lwr <- range[1]; upr <- range[2]
x <- lwr :upr
plot(x, dbinom(x, size=10, prob=0.20), pch=19, col="dodgerblue")

binom.test(0, 10, p=0.20, alt="less")
binom.test(0, 13, p=0.20, alt="less")
binom.test(0, 14, p=0.20, alt="less")
