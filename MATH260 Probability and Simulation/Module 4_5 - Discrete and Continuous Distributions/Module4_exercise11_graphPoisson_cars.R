setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/MATH260 Probability and Simulation/")
source("/datascience/projects/statisticallyfit/github/R/RStatistics/MATH260 Probability and Simulation/PLOTTING_PROB.R")

xs = 0:8
ys = dpois(x=xs, lambda=1/2)
plotDiscreteDist(xs, ys)
