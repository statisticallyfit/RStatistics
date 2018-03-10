setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/FORMULAS.R', echo=FALSE)
source('/datascience/projects/statisticallyfit/github/R/RStatistics/INTERPRET.R', echo=FALSE)
source('/datascience/projects/statisticallyfit/github/R/RStatistics/PLOTTING.R', echo=FALSE)

load("data/Exercises and Examples/AEROBIC.Rdata")

# a) Scatterplot
# Plot with ggplot
g <- ggplot(AEROBIC, aes(x = MAXOXY, y = IGG))
g + geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="Oxygen", y="IGG") +
      stat_smooth(method="lm", col="red", lwd=1)
##geom_smooth(method="lm", lwd=1, alpha=0.1, fill="red")

# a) Scatterplot
#ggplotRegression(wateroil.interact.lm)


# b) least squares QUADRATIC
AEROBIC$MAXOXY_2 <- AEROBIC$MAXOXY^2
aeorobic.quadratic.lm <- lm(IGG ~ MAXOXY + MAXOXY_2, data=AEROBIC)
summary(aeorobic.quadratic.lm)

# c)
# how to do with ggplot? https://stackoverflow.com/questions/42764028/fitting-a-quadratic-curve-in-ggplot
plot(AEROBIC$MAXOXY, AEROBIC$IGG)
abline(aeorobic.quadratic.lm)

# HELP not finished
