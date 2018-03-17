source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')

library(GGally)
options(digits = 3, show.signif.stars = FALSE)

# read data
perchData <- read.table("Perch.txt", header=TRUE)
head(perchData)

# Pairs plot
# cols 3,4,2 = cols Length, Width, Weight
pairs(perchData[, c(3, 4, 2)], lower.panel = panel.smooth, lwd=3, col="blue", upper.panel = panel.cor)

# Ssee other options for continuous X, categorical Y etc data: ...
# https://cran.r-project.org/web/packages/GGally/GGally.pdf
ggpairs(data=perchData, columns=c(3,4,2), upper=list(continuous="cor"),
        diag=list(continuous="bar"))
ggpairs(data=perchData, columns=c(3,4,2), upper=list(continuous="cor"),
        diag=list(continuous="density", params=c(colour="magenta")))

ggpairs(data=perchData, columns=c(3,4,2), 
        lower=list(continuous="smooth", params=c(colour="blue")),
        diag=list(continuous="density", params=c(colour="magenta")),
        upper = list(continuous="cor", params=c(size=10)))

# need to upgrade R to get 1.01 version of GGally so we have wrap function so that
# we can plot the red reg.line on top!
#lowerFn <- function(data, mapping, method = "lm", ...) {
#      p <- ggplot(data = data, mapping = mapping) +
#            geom_point(colour = "blue") +
#            geom_smooth(method = method, color = "red", ...)
#      p
#}
#
#ggpairs(
#      iris[, 1:4], lower = list(continuous = wrap(lowerFn, method = "lm")),
#      diag = list(continuous = wrap("barDiag", colour = "blue")),
#      upper = list(continuous = wrap("cor", size = 10))
#)