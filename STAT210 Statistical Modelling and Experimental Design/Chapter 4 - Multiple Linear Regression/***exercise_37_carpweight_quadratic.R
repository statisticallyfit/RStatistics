setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/FORMULAS.R', echo=FALSE)
source('/datascience/projects/statisticallyfit/github/R/RStatistics/INTERPRET.R', echo=FALSE)
source('/datascience/projects/statisticallyfit/github/R/RStatistics/PLOTTING.R', echo=FALSE)

load("data/Exercises and Examples/CARP.Rdata")

# regression
CARP$WEIGHT_2 <- CARP$WEIGHT^2
carp.quadratic.lm <- lm(ENE ~ WEIGHT + WEIGHT_2, data=CARP)
summary(carp.quadratic.lm)

# Scatterplot
g <- ggplot(CARP, aes(x = WEIGHT, y = ENE))
g + geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="Weight", y="Nitrogen Excretion (ENE)") +
      stat_smooth(method="lm", col="red", lwd=1)
