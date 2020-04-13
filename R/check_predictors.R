#' Starting with a base linear regression, checkvars takes turns adding a variables from a list of
#' variables to it one at a time, calculating the change in RSquared Adjusted for each variable addition to
#' to the base regression, and showing the user if the variable was good or bad for the RSquared Adjusted
#'
#' @param df A dataframe
#' @param response The dependent variable of the regressions, as a string
#' @param predictors A vector of strings containing the independent variables already in the model
#' @param predictors_to_check A vector of strings containing the independent variable to be checked
#' as possible additions to the model
#' @return A sentence in the form of a vector with each tested variable, whether it was good or bad for
#' Adjusted RSquared, and the exact change in Adjusted RSquared, rounded to 6 decimal places
#'
#' @examples
#'
#' response = c("mpg")
#' explan <- c("am", "hp")
#' to_try <- c("wt", "gear", "cyl")
#'
#' check_predictors(mtcars, response, explan, to_try)
#'
#' @export
check_predictors <- function(df, response, existing_predictors, predictors_to_check) {

  predictors <- paste(predictors, collapse = "+")
  my_formula <- paste(response, "~", predictors)

  mod <- lm(my_formula, data = df)

  crit_val <- summary(mod)$adj.r.squared

  purrr::map_chr(predictors_to_check, ~try_one_predictor(df, my_formula, .x, crit_val))

}


#' Helper function that tries adding one predictor to a model and checks Adjusted R Squared.
#'
#' @param df A dataframe
#' @param partial_formula String representing the formula of the original model.
#' @param new_predictor Name of new independent variable to check.
#' @param crit_val The Adjusted R-Squared value of the original model.
#'
#' @return A sentence stating whether the new variable was good or bad for the Adjusted RSquared,
#' and the exact change in Adjusted RSquared, rounded to 6 decimal places
try_one_predictor <- function(df, partial_formula, new_predictor, crit_val) {

  formula_up <- paste(my_formula, " + ", predictors_to_check[i])

  mod_alt <- lm(formula_up, data = df)

  adj_r <- summary(modalt)$adj.r.squared
  diff <- round((adj_r - critvalue), 6)

  good_bad <- case_when(
    diff > 0 ~ "Good",
    diff <= 0 ~ "Bad"
  )

  glue::glue("{new_predictor} : {good_bad}, {diff}.")

}

