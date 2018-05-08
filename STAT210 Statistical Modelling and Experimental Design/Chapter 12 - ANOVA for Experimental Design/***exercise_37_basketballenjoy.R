setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

options(digits=10, show.signif.stars = FALSE)

basketTable <- matrix(c(1.77, 2.73, 5.38, 4.34, 7.16, 7.52, 7.59, 4.92),
                         byrow = TRUE, nrow=4)
basketTable <- data.frame(basketTable)
colnames(basketTable) <- c("Male", "Female")
rownames(basketTable) <- c("Minimal", "Moderate", "Substantial", "Extreme")
basketTable

# actual regression data
basketData <- data.frame(GENDER=c(rep("Male", 4), rep("Female", 4)),
                         SUSPENSE=c(rep(c("Minimal", "Moderate", "Substantial", "Extreme"),2)),
                         RATING=c(1.77, 5.38, 7.16, 7.59, 2.73, 4.34, 7.52, 4.92),
                         stringsAsFactors = TRUE)
basketData

with(basketData, interaction.plot(x.factor=SUSPENSE, trace.factor = GENDER, response=RATING))



# anova f-test for interaction
basket.lm <- lm(RATING ~ GENDER*SUSPENSE, data=basketData)
summary(basket.lm)
is.factor(basketData$RATING)
EGG2
