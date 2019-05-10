#http://uc-r.github.io/regression_trees#tune
library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging
library(AmesHousing)
library(vip) # variable importance from gbm


set.seed(123)
ames_split <- initial_split(AmesHousing::make_ames(), prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

# Specify we are doing 10-fold cross validation
ctrl <- trainControl(method="cv", number=10)

# CV bagged model
ames.bag.cv <- train(
      Sale_Price ~ ., 
      data=ames_train, 
      method="treebag",
      trControl=ctrl,
      importance=TRUE
)

ames.bag.cv 
## Bagged CART 
## 
## 2051 samples
##   80 predictor
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 1846, 1845, 1847, 1845, 1846, 1847, ... 
## Resampling results:
## 
##   RMSE      Rsquared   MAE     
##   36477.25  0.8001783  24059.85


# plot most important variables
plot(varImp(ames.bag.cv), 20)  







### BETTER WAY USING GBM -----------------------------------------------------
hyper_grid <- expand.grid(
      shrinkage = c(.01, .1, .3),
      interaction.depth = c(1, 3, 5)
)

# total number of combinations
nrow(hyper_grid)
## [1] 81


# reproducibility
set.seed(123)
# grid search 
for(i in 1:nrow(hyper_grid)) {
      
      
      # train model
      gbm.tune <- gbm(
            formula = Sale_Price ~ .,
            distribution = "gaussian",
            data = ames_train,
            n.trees = 5000,
            interaction.depth = hyper_grid$interaction.depth[i],
            shrinkage = hyper_grid$shrinkage[i],
            n.cores = NULL, # will use all cores by default
            verbose = FALSE
      )
      
      # add min training error and trees to grid
      hyper_grid$numOptimalTrees[i] <- which.min(gbm.tune$valid.error)
      hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
      dplyr::arrange(min_RMSE) %>%
      head(10)

##    n.trees shrinkage interaction.depth n.minobsinnode bag.fraction optimal_trees min_RMSE
## 1     6000      0.10                 5              5         0.65           483 20407.76
## 2     6000      0.01                 5              7         0.65          4999 20598.62
## 3     6000      0.01                 5              5         0.65          4644 20608.75
## 4     6000      0.05                 5              7         0.80          1420 20614.77
## 5     6000      0.01                 7              7         0.65          4977 20762.26
## 6     6000      0.10                 3             10         0.80          1076 20822.23
## 7     6000      0.01                 7             10         0.80          4995 20830.03
## 8     6000      0.01                 7              5         0.80          4636 20830.18
## 9     6000      0.10                 3              7         0.80           949 20839.92
## 10    6000      0.01                 5             10         0.65 


# Fit final model
# for reproducibility
set.seed(123)

# train GBM model
gbm.fit.final <- gbm(
      formula = Sale_Price ~ .,
      distribution = "gaussian",
      data = ames_train,
      n.trees = 483,
      interaction.depth = 5,
      shrinkage = 0.1,
      n.cores = NULL, # will use all cores by default
      verbose = FALSE
)  