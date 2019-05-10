setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A3")


#http://uc-r.github.io/regression_trees#tune
library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging


# Set up the data


# Read in the data
data <- read.csv("bike.csv", header=TRUE)
data <- data[, -1] # removing the Date column so that we don't get more than 32
# factor levels. 
p <- ncol(data) - 1; # p is the number of predictors

# Make the categorical variables be factors
data$Weekend <- as.factor(data$Weekend)
data$Weather <- as.factor(data$Weather)
data$Year <- as.factor(data$Year)


# First split the data in train /test set
set.seed(1)
N <- nrow(data)
iTrain <- sample(1:N, size=0.6 * N) # ratio of 0.6 for train data, and 0.4 for test data
bikeTrain <- data[iTrain, ]
bikeTest <- data[-iTrain, ]

X.train <- bikeTrain[,-8]
Y.train <- bikeTrain$Casual
Y.test <- bikeTest$Casual
X.test <- bikeTest[,-8]



# fit model with RPART
set.seed(1)
bike.rpart <- rpart(formula=Casual ~., data=bikeTrain, method="anova")
bike.rpart
summary(bike.rpart)

rpart.plot(bike.rpart)

hyper_grid <- expand.grid(
      minsplit = seq(5, 20, 1),
      maxdepth = seq(8, 15, 1)
)


bikeModels <- list()

for (i in 1:nrow(hyper_grid)) {
      
      # get minsplit, maxdepth values at row i
      minsplit <- hyper_grid$minsplit[i]
      maxdepth <- hyper_grid$maxdepth[i]
      
      # train a model and store in the list
      bikeModels[[i]] <- rpart(
            formula = Casual ~ .,
            data    = bikeTrain,
            method  = "anova",
            control = list(minsplit = minsplit, maxdepth = maxdepth)
      )
}


# function to get optimal cp
get_cp <- function(x) {
      # get index of minimum test error
      min    <- which.min(x$cptable[, "xerror"])
      # get the ALPHA corresponding to model with min test error. 
      cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
      # get the index  of minimum test error
      min    <- which.min(x$cptable[, "xerror"])
      # get the actual minimum test error
      xerror <- x$cptable[min, "xerror"] 
}

tableBike <- hyper_grid %>%
      # mutate adds these vector columns (list of alphas and errors) to the hyper grid
      mutate(
            cp    = purrr::map_dbl(bikeModels, get_cp),
            error = purrr::map_dbl(bikeModels, get_min_error)
      ) %>%
      # arrange sorts the resulting matrix from above according to minimum error
      arrange(error) 