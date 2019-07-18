setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')

library(ggplot2)
library(ggfortify)
options(show.signif.stars = FALSE)


# Question 1: Example 6.2 (in topic 6)

# OFFSET: 
# The data are counts and modelled with a Poisson distribution. 
# The number of person-years
# are not the same for all age groups and this has to be incorporated into the model.
# If we view the response as deaths/ person-years, then whilst the numerator has a
# random component, the denominator does not. We need to adjust the systematic part 
# to account for the denominator but not the random part.