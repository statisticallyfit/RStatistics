setwd('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A1/')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')


options(digits = 5, show.signif.stars = FALSE)

hollyData <- read.table("Hollywood.txt", header=TRUE)
head(hollyData)


# part a) plotting with pairs()
pairs(hollyData[, c(2,3,4,1)], lower.panel = panel.smooth, upper.panel = panel.cor)

# part b) 
receipts.lm <- lm(Receipts ~ Production + Promo + Books, data=hollyData)
print(summary(receipts.lm))
print(anova(receipts.lm))


# part f)
receipts2.lm <- lm(Receipts ~ Production + Promo, data=hollyData)
print(summary(receipts2.lm))
print(anova(receipts2.lm))


# part g)
betaCI(receipts2.lm)


# part h) 
# i) 95% CI when production=7, promo = 7
predict(receipts2.lm, newdata = data.frame(Production=7, Promo=7), 
        interval="confidence", level=0.95)

# ii) 95% CI when production=14, promo = 2
predict(receipts2.lm, newdata = data.frame(Production=14, Promo=2), 
        interval="confidence", level=0.95)


# Commenting on reliability of the intervals 
attach(hollyData)

par(mfrow=c(1,1))
plot(Production, Promo, main="Relationship Between Production and Promo")




# part i) testing assumptions using R diagnostics. 
par(mfrow=c(1,2))
# testing constant variance (plot 1) and normality of errors (plot 2)
plot(receipts2.lm, which=1:2)
# further test for normality of errors
shapiro.test(receipts2.lm$residuals)



detach(hollyData)
