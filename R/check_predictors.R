#' Starting with a base linear regression, check_predictors takes turns adding a variables from a list of
#' variables to it one at a time, calculating the change in RSquared Adjusted for each variable addition to
#' to the base regression, and showing the user if the variable was good or bad for the RSquared Adjusted
#'
#' @param df A dataframe
#' @param model A model object of class "lm" or a valid formula.
#' @param ... Any number of (unquoted) variables to be checked as possible additions to the model
#' @return A sentence in the form of a vector with each tested variable, whether it was good or bad for
#' Adjusted RSquared, and the exact change in Adjusted RSquared, rounded to 6 decimal places
#'
#' @examples
#'
#' check_predictors(mtcars, mpg ~ cyl, hp, am)
#'
#' @export
check_predictors <- function(df, model, ...) {

  if (!(class(model) %in% c("formula", "lm"))) {
    stop("Please supply either a formula or a model.")
  } else if (class(model) == "formula") {
    my_formula <- model
    model <- lm(my_formula, data = df)
  } else if (class(model) == "lm") {
    my_formula <- model_to_formula(model)
  }

  crit_val <- summary(model)$adj.r.squared

  purrr::map_chr(enquos(...), ~try_one_predictor(df, my_formula, !!(.x), crit_val))

}


#' Helper function that tries adding one predictor to a model and checks Adjusted R Squared.
#'
#' @param df A dataframe
#' @param model A model object of class "lm" or a valid formula.
#' @param new_predictor A variable to try in the model (unquoted)
#' @param crit_val The Adjusted R-Squared value of the original model.
#'
#' @return A sentence stating whether the new variable was good or bad for the Adjusted RSquared,
#' and the exact change in Adjusted RSquared, rounded to 6 decimal places
try_one_predictor <- function(df, model, new_predictor, crit_val) {

  if (!(class(model) %in% c("formula", "lm"))) {
    stop("Please supply either a formula or a model.")
  } else if (class(model) == "lm") {
    my_formula <- model_to_formula(model)
  }

  formula_up <- paste(my_formula, " + ", ensym(new_predictor))

  mod_alt <- lm(formula_up, data = df)

  adj_r <- summary(mod_alt)$adj.r.squared
  diff <- round((adj_r - crit_val), 6)

  good_bad <- ifelse(diff > 0, "Good", "Bad")

  glue::glue("{new_predictor}: {good_bad}, {diff}.")

}

