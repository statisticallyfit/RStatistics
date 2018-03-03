source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/learnstatistics/[PennState] GeneralizedLinearModels/ThreeWayTable/ModAssoc_MutualIndependence_Boys.R')



##########################  TEST OF MARGINAL INDEPENDENCE  ##########################
############## (mutual independence for each of the two-way tables) #################
#####################################################################################

temp.by.SES <- xtabs(count ~ delinquent + scout + SES, table); temp.by.SES
temp.by.scout <- xtabs(count ~ SES + delinquent + scout, table)
temp.by.delinq <- xtabs(count ~ scout + SES + delinquent, table); temp.by.delinq


## Test of marginal independence for two-way table SES * scout
# the table is organized last by which variable is last
ftable(temp.by.SES)
temp.by.SES[1,,] + temp.by.SES[2,,]
SES_Scout <- margin.table(temp.by.SES, c(2,3)); SES_Scout
result <- ChiSquareIndependence(SES_Scout) # ses, scout  are marginally independent
result$Expected
SES_Scout <- list(Frequency=SES_Scout, RowPerc=prop.table(SES_Scout, 2)); SES_Scout


## Test of marginal independence for two-way table SES * delinquent
temp.by.delinq[1,,] + temp.by.delinq[2,,]
SES_delinquent <- margin.table(temp.by.delinq, c(2,3)); SES_delinquent
result <- ChiSquareIndependence(SES_delinquent) # ses, delinquent are marginally independent
result$Expected
SES_delinquent <- list(Frequency=SES_delinquent, RowPerc=prop.table(SES_delinquent, 1))
SES_delinquent


## Test of marginal independence for two-way table delinquent * scout
temp.by.delinq[,1,] + temp.by.delinq[,2,]
temp.scout.delinq <- xtabs(count ~ SES + scout + delinquent, table); temp.scout.delinq
temp.scout.delinq[1,,] + temp.scout.delinq[2,,] ################# TODO why not same as next?
delinquent_scout <- margin.table(temp.scout.delinq, c(3,2)); delinquent_scout
result <- ChiSquareIndependence(delinquent_scout) # delinq,scout are marginally independent
result$Expected
delinquent_scout <- list(Frequency=delinquent_scout, RowPerc=prop.table(delinquent_scout, 1))
delinquent_scout



# Compute odds ratio
# Mutual ind. implies marginal ind. so if at least one odds ratio in the marginal
# tables is rejected, then we can reject H0 or mutual ind. 
OddsRatioCI(SES_Scout$Frequency)
OddsRatioCI(SES_delinquent$Frequency)
OddsRatioCI(delinquent_scout$Frequency)


# easy way 
MarginalIndependence(temp.by.SES)
MarginalIndependence(temp.by.scout)
MarginalIndependence(temp.by.delinq)
