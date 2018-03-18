setwd('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A1/')

smokeData <- read.table("birthsmokers.txt", header=TRUE)
head(smokeData)

smokeData2 <- smokeData
smokeData2$Smoke <- ifelse(smokeData$Smoke == 1, "yes", "no")

smokeData$Smoke <- as.factor(smokeData$Smoke)

model2 <- lm(Wgt ~ Gest + Smoke, data=smokeData2)
summary(model2)
model <- lm(Wgt ~ Gest + Smoke, data=smokeData)
summary(model)

confint(model)

predict(model2, interval="confidence",
        newdata=data.frame(Gest=c(38,38), Smoke=c("yes", "no")))

predict(model, interval="confidence",
        newdata=data.frame(Gest=c(38,38), Smoke=as.factor(c(1, 0))))
# separately
predict(model, interval="confidence",
        newdata=data.frame(Gest=38, Smoke="1"))
predict(model, interval="confidence",
        newdata=data.frame(Gest=38, Smoke="0"))
