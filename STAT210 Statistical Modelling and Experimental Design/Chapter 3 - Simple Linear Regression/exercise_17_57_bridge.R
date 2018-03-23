setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/FHWABRIDGE.Rdata")

reg.line <- lm(AREA ~ NUMBER, data = FHWABRIDGE)
reg.line
summary(reg.line)

plot(AREA ~ NUMBER, data=FHWABRIDGE)

ggplot(FHWABRIDGE, aes(x=NUMBER, y=AREA)) + geom_point(shape=19, size=3,color="dodgerblue")

# degrees freedom = n - 2 = 52 - 2 = 50 
n <- nrow(FHWABRIDGE); n


# from FORMULAS (source the file)
y <- FHWABRIDGE$AREA 
yhat <- reg.line$fitted.values
x <- FHWABRIDGE$NUMBER

SSE(y, yhat)
standardErrorOfRegression(y, yhat)


# Interpreting confint / predict intervals
meanCI(x, y, reg.line, x.value=350)
interpret.MeanCI(x, y, reg.line, x.value=350, x.unit="bridges", 
                 x.name="number of bridges", y.unit="thousands of sq. ft",y.name="area")

predictCI(x, y, reg.line, x.value=350)
interpret.PredictCI(x, y, reg.line, x.value=350, x.unit="bridges", 
                 x.name="number of bridges", y.unit="thousands of sq. ft",y.name="area")

