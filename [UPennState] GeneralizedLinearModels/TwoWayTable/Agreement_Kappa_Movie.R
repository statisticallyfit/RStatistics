source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R')

critique <- matrix(c(24,8,10,8,13,9,13,11,64),nrow=3,
                   dimnames=list("Siskel"=c("con","mixed","pro"),
                                 "Ebert"=c("con","mixed","pro")))
critique 

LikelihoodRatioTest(critique)
ChiSquareIndependence(critique)
CohenKappaTest(critique)
