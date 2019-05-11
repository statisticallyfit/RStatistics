setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A3")

library(e1071)


# income:  0 = "<= 50 K group",    1 = "> 50 K group"
load("datacens.Rdata")

# first check if any NA observations
any(is.na(datacens$train))
any(is.na(datacens$val))

# Income can stay numeric, but make a factor column of income also
datacens$train$income <- as.numeric(datacens$train$income)
datacens$val$income <- as.numeric(datacens$val$income)

datacens$train$type_employer <- as.numeric(datacens$train$type_employer)
datacens$train$education <- as.numeric(datacens$train$education)
datacens$train$marital <- as.numeric(datacens$train$marital)
datacens$train$occupation <- as.numeric(datacens$train$occupation)
datacens$train$relationship <- as.numeric(datacens$train$relationship)
datacens$train$race <- as.numeric(datacens$train$race)
datacens$train$sex <- as.numeric(datacens$train$sex)
datacens$train$capital_gain <- as.numeric(datacens$train$capital_gain)
datacens$train$capital_loss <- as.numeric(datacens$train$capital_loss)
datacens$train$country <- as.numeric(datacens$train$country)

datacens$val$type_employer <- as.numeric(datacens$val$type_employer)
datacens$val$education <- as.numeric(datacens$val$education)
datacens$val$marital <- as.numeric(datacens$val$marital)
datacens$val$occupation <- as.numeric(datacens$val$occupation)
datacens$val$relationship <- as.numeric(datacens$val$relationship)
datacens$val$race <- as.numeric(datacens$val$race)
datacens$val$sex <- as.numeric(datacens$val$sex)
datacens$val$capital_gain <- as.numeric(datacens$val$capital_gain)
datacens$val$capital_loss <- as.numeric(datacens$val$capital_loss)
datacens$val$country <- as.numeric(datacens$val$country)

# make < 50K be -1 and >= 50K be 1
datacens$train$incomeBinary <- as.factor(datacens$train$income)
datacens$val$incomeBinary <- as.factor(datacens$val$income)

# Then take random sample of original data set, from both train and test sets. 
Nt <- nrow(datacens$train)
Nv <- nrow(datacens$val)
n <- 500 # use a smaller sample
set.seed(111)
ts <- sample(1:Nt, size=n)
vs <- sample(1:Nv, size=n)
censusTrain <- datacens$train[ts, ]
censusTest <- datacens$val[vs, ]

# part b) radial kernel svm --------------------------------------------------------------------

# SVM USING CROSS-VALIDATION to select best COST AND GAMMA-
set.seed(1)
tune.radial <- tune(svm, incomeBinary ~., data=censusTrain, kernel="radial", 
                 ranges=list(cost=c(0.01, 0.1, 1, 10, 20),gamma=c(0.5, 5, 10)), scale=TRUE)
tune.radial
summary(tune.radial)

# cost = small = tuning param C = large (wide margins)
# cost = large => tuning param C = small (narrow margins)
tune.radial$best.performance # best train error
# 0.098
tune.radial$best.parameters # best cost and gamma
# cost = 10, gamma = 0.5
tune.radial$best.model
# num support vectors = 325, so has wide margins


# part c) fit the best model -----------------------------------------------------------------

### Fitting the models using the best parameters
bestCost <- tune.radial$best.parameters[[1]]; bestCost
bestGamma <- tune.radial$best.parameters[[2]]; bestGamma
census.svm.radial <- svm(incomeBinary ~ ., data=censusTrain, 
                         kernel="radial", cost=bestCost, gamma=bestGamma, scale=TRUE)

# Describe
summary(census.svm.radial)


# Training set Evaluation
# confusion matrix: TRAIN
pred <- predict(census.svm.radial, newx=censusTrain) # train predictions
table(Pred = pred, Truth = censusTrain$incomeBinary)
# 0+0 = 0 misslciassifications for training data
trainError.radial <- mean(pred != censusTrain$incomeBinary); trainError.radial
# 0% missclassification on training data



# part d)  -------------------------------------------------------------------------------------

# Test set Evaluation

pred <- predict(tune.radial$best.model, newx=censusTest)
# confusion matrix: TEST
table(Pred=pred, Truth = censusTest$incomeBinary) # there are 85 + 98 = 183 missclassifications
# test error
testError.radial <- mean(pred != censusTest$incomeBinary); testError.radial
# so 36.6% of test observations are missclassified. 


# Plot the resulting SVC

filePath2 = "/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A3/"
useNames <- names(censusTrain)[!names(censusTrain) %in% c("income", "incomeBinary")]

# Plots using the training data

# Plotting all variable predictors vs income
pdf(file=file.path(paste(filePath2, "Train_Radial", ".pdf", sep="")))
for (name in useNames) {
      plot(census.svm.radial, censusTrain, as.formula(paste("income ~", name, sep = "")))
}
dev.off()

# Plotting two numerical variables
plot(census.svm.radial, censusTrain, hr_per_week ~ age)


# Calculating the slope an dintercept of the hyperplane
w <- t(census.svm.radial$coefs) %*% census.svm.radial$SV
slope_1 <- -w[1]/w[2]; slope_1
#[1] 20.77031
intercept_1 <- census.svm.radial$rho/w[2]; intercept_1
#[1] -0.298573




#pdf(file=file.path(paste(filePath2, "mytest.pdf", sep="")))
#for(name in useNames){
#      for(name2 in useNames[!(useNames %in% name)]){
#            plot(census.svm.radial, censusTrain, as.formula(paste(name2,"~",name,sep="")))
#      }
#}
#dev.off()

