source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/learnstatistics/[PennState] GeneralizedLinearModels/ThreeWayTable/ModAssoc_MutualIndependence_Boys.R')
source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/learnstatistics/[PennState] GeneralizedLinearModels/ThreeWayTable/ModAssoc_MarginalIndependence_Boys.R')


############################ Test Joint Independence (D, BS) #############################
##########################################################################################

# (BS, D)
# creating 6 x 2 table BS x D (boy, SES x delinquent)
SESscout_delinquent <- ftable(temp.by.SES, row.vars=c(3,2))
SESscout_delinquent
ChiSquareIndependence(SESscout_delinquent)

# (DS, B)
temp.by.scout <- xtabs(count ~ SES + delinquent + scout, table); temp.by.scout
SESdelinquent_scout <- ftable(temp.by.scout, row.vars=c(1,2))
SESdelinquent_scout
ChiSquareIndependence(SESdelinquent_scout)

# (DB, S)
scoutdelinquent_SES <- ftable(temp.by.SES, row.vars=c(1,2))
scoutdelinquent_SES
ChiSquareIndependence(scoutdelinquent_SES)


JointIndependence(temp.by.SES)
JointIndependence(temp.by.scout)
JointIndependence(temp.by.delinq)
JointIndependence(temp.scout.delinq)
