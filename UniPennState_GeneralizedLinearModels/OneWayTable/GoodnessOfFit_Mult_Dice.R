source('/datascience/projects/statisticallyfit/github/R/RStatistics/FORMULAS.R')


## run a goodness of fit test
rolls.obs <- c(3, 7, 5, 10, 2, 3)
n <- sum(rolls.obs)
rolls.prop <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
rolls.exp <- n * rolls.prop; rolls.exp 

dice.test <- chisq.test(x=rolls.obs, p=rolls.prop); dice.test

dice.test$observed
dice.test$expected
dice.test$residuals # pearson residuals

# manual residuals: 
os <- dice.test$observed; es <- dice.test$expected
rs <- (os - es) / sqrt(es); rs
df <- length(rolls.obs) - 1; df
cutoff <- sqrt( (df-1)/df ); cutoff # limit over which Pearson resids are too big


### Deviance and p-value
LikelihoodRatioTableTest(rolls.obs)

## Deviance residuals
dev.res.j <- devianceResiduals(os, es); dev.res.j


## make nice output table
output <- round(cbind(cell.j=1:6, O.j=os, E.j=es, res.j=rs, dev.res.j=dev.res.j), 3)
output <- as.data.frame(output); output 

# make a file
# path <- "/datascience/projects/statisticallyfit/github/learningstatistics/R/learnstatistics/[PennState] GeneralizedLinearModels/OneWayTable/"
# write.table(output, cat(path, "diceRollsResults"), row.names=F, col.names=T, sep="\t")

