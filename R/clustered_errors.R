
#' Runs a simple or multiple regression with clustered standard errors
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param response The dependent variable of the regression, as a string
#' @param predictors A vector of strings containing each independent variable used in the model
#' @param cluster_predictor A variable to cluster the standard errors by.
#' @export
clustered_errors <- function(df, response, predictors, cluster_predictor) {
  my_predictors <- paste(predictors, collapse = " + ")
  my_formula <- paste(response, "~", my_predictors)
  mod <- lm(my_formula, data=df)
  results <- lmtest::coeftest(mod, sandwich::vcovCL, cluster = df$cluster_predictor)
  return(broom::tidy(results))
}
