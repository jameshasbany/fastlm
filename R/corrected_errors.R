
#' Runs a simple or multiple regression, automatically correcting for possible heteroskedasticity in
#' the standard errors
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param response The dependent variable of the regression, as a string
#' @param predictors A vector of strings containing each independent variable used in the model
#' @param type An optional term for specifiying the specific estimator wanted, see vcovHC for options
#' @return Returns summarized output of a linear regression model with corrected standard errors
#' @export
corrected_errors <- function(df, response, predictors, type = "HC"){
  my_predictors <- paste(predictors, collapse = " + ")
  my_formula <- paste(response, "~", my_predictors)
  mod <- lm(my_formula, data=df)
  correct_vcov <- sandwich::vcovHC(mod, type)
  results <- lmtest::coeftest(mod, vcov = correct_vcov)
  return(broom::tidy(results))
}

