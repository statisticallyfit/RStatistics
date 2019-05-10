#http://uc-r.github.io/regression_trees#tune
library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging
library(AmesHousing)


set.seed(123)
ames_split <- initial_split(AmesHousing::make_ames(), prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

m1 <- rpart(formula = Sale_Price ~ ., data = ames_train, method  = "anova")

# Tuning the parameters: alpha (cp), maxdepth, and minsplit
hyper_grid <- expand.grid(
      minsplit = seq(5, 20, 1),
      maxdepth = seq(8, 15, 1)
)

head(hyper_grid)


# Getting each model for each parameter combination

models <- list()

for (i in 1:nrow(hyper_grid)) {
      
      # get minsplit, maxdepth values at row i
      minsplit <- hyper_grid$minsplit[i]
      maxdepth <- hyper_grid$maxdepth[i]
      
      # train a model and store in the list
      models[[i]] <- rpart(
            formula = Sale_Price ~ .,
            data    = ames_train,
            method  = "anova",
            control = list(minsplit = minsplit, maxdepth = maxdepth)
      )
}

# We can now create a function to extract the minimum error 
# associated with the optimal cost complexity alpha value for each model. 
# After a little data wrangling to extract the optimal alpha value and its 
# respective error, adding it back to our grid, and filter for the top 5 
# minimal error values we see that the optimal model makes a slight 
# improvement over our earlier model (xerror of 0.242 versus 0.272).

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

result <- hyper_grid %>%
      # mutate adds these vector columns (list of alphas and errors) to the hyper grid
      mutate(
            cp    = purrr::map_dbl(models, get_cp),
            error = purrr::map_dbl(models, get_min_error)
      ) %>%
      # arrange sorts the resulting matrix from above according to minimum error
      arrange(error) 
#%>%      #top_n(-5, wt = error)


# Fit the final tree and predict on the test set
optimal_tree <- rpart(
      formula = Sale_Price ~ .,
      data    = ames_train,
      method  = "anova",
      control = list(minsplit = 11, maxdepth = 8, cp = 0.01)
)

pred <- predict(optimal_tree, newdata = ames_test)
#RMSE
RMSE(pred = pred, obs = ames_test$Sale_Price)
# MSE (sqrt of RMSE)
mean((pred - ames_test$Sale_Price)^2) # MSE
# The final RMSE is 39145.39 which suggests that, on average, our
# predicted sales prices are about $39,145 off from the actual sales price.