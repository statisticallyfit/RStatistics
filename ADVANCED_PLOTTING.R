# This was made by https://rdrr.io/github/mattmills49/modeler/src/R/partial_plot.R



#' Partial Regression Plots for visualizing relationship between variables
#' 
#' \code{partial_plot} accepts a fitted regression object and the name of the 
#' variable you wish to view the 
#' \href{https://en.wikipedia.org/wiki/Partial_regression_plot}{partial 
#' regression plot} of as a character string. It returns a \code{ggplot} object 
#' showing the independent variable values on the x-axis with the resulting 
#' predictions from the independent variable's values and coefficients on the 
#' y-axis. This shows the relationship that the model has estimated between the
#' independent variable the dependent variable. You can determine which
#' prediction level the plot is against using the \code{response} parameter
#' which is a logical value defaulted to \code{FALSE}.
#' 
#' @param fitted_model a complete regression model object
#' @param variable the name of the independent variable as a character string
#' @param response logical indicating if the plot should be on the linear 
#'   prediction scale or the response scale. Defaults to \code{FALSE}
#' @param rug logical indicating if a rug plot should be added on the 
#'   independent variable axis
#' @return a \code{ggplot2} object of the partial regression plot
#' @export
#' @importFrom magrittr %>%
#' @import ggplot2
#' @examples
#' library(mgcv)
#' car_gam <- gam(mpg ~ s(hp), data = mtcars)
#' partial_plot(car_gam, "hp")
#' 
#' # Response level changes look for non-gaussian families
#' am_gam <- gam(factor(am) ~ s(hp), data = mtcars, family = "binomial")
#' partial_plot(am_gam, "hp") # on the log odds scale
#' partial_plot(am_gam, "hp", response = T) # on the probability scale

library(magrittr)


partial_plot <- function(fitted_model, ...) UseMethod("partial_plot")

#' @describeIn partial_plot provides partial plots for fitted \code{gam} models
#' @export

partial_plot.gam <- function(fitted_model, variable, response = F, rug = F) {
      
      pred_matrix <- predict(fitted_model, type="lpmatrix")
      variable_values <- fitted_model$model[[variable]]
      model_coefs <- coef(fitted_model)
      
      coefficient_terms <- stringr::str_replace_all(stringr::str_extract_all(names(model_coefs), "\\([^)]*\\)"), c("\\(" = "", "\\)" = ""))
      variable_coefs <- model_coefs[coefficient_terms == variable]
      variable_linpred <- pred_matrix[, coefficient_terms == variable]
      
      smooth_predictions <- variable_linpred %*% variable_coefs %>% drop
      plot_values <- dplyr::data_frame(variable = variable_values, smooth_curve = smooth_predictions) %>% dplyr::distinct()
      if (response) {
            if (fitted_model$family$family == "gaussian") {
                  plot_values$predictions <- plot_values$smooth_curve + model_coefs["(Intercept)"]
            } else if (fitted_model$family$family == "binomial") {
                  plot_values$predictions <- 1 / (1 + exp(-(plot_values$smooth_curve + model_coefs["(Intercept)"])))
            }
      } else {
            plot_values$predictions <- plot_values$smooth_curve + model_coefs["(Intercept)"]
      }
      
      yname <- attr(attr(fitted_model$pterms, "dataClasses"), "names")
      ylabel <- stringr::str_c("Prediction for ", yname)
      if (response) {
            ylabel <- stringr::str_c(ylabel, "\n(Response Scale)")
      } else {
            ylabel <- stringr::str_c(ylabel, "\n(Linear Prediction Scale)")
      }
      
      partial_p <- ggplot() + 
            geom_line(aes(x = variable, y = predictions), data = plot_values, color = "blue", size = 2) + 
            ylab(ylabel) +
            xlab(variable) +
            ggtitle(stringr::str_c("Partial Regression Plot for ", variable))
      
      if (rug) {
            partial_p <- partial_p + geom_rug(aes(x = variable), data = plot_values, sides = "b")
      }
      
      return(partial_p)
}