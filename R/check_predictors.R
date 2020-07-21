#' Starting with a base linear regression, check_predictors takes turns adding a variables from a list of
#' variables to it one at a time, calculating the change in RSquared Adjusted for each variable addition to
#' to the base regression, and showing the user if the variable was good or bad for the RSquared Adjusted
#'
#' @param df A dataframe
#' @param model A model object of class "lm" or a valid formula.
#' @param ... Any number of (unquoted) variables to be checked as possible additions to the model
#' @return A tibble with each tested variable, whether it was good or bad for
#' Adjusted RSquared, and the exact change in Adjusted RSquared.
#'
#' @examples
#'
#' check_predictors(mtcars, mpg ~ cyl, hp, am)
#'
#' @import dplyr
#' @importFrom stats lm
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

  to_try <- purrr::map_chr(ensyms(...), as_string)

  new_rsqs <- purrr::map_dbl(to_try, ~summary(add_one_predictor_strings(df, my_formula, .x))$adj.r.squared)

  results <- tibble::tibble(
    new_var = to_try,
    original_adj_R2 = rep(crit_val, length(to_try)),
    new_adj_R2 = new_rsqs,
    diff = new_adj_R2 - original_adj_R2,
    keep = diff > 0
  ) %>%
    purrr::map_dfc(unname) %>%
    arrange(desc(diff))

  return(results)

}


#' Helper function that adds one predictor to a model.
#'
#' @param df A dataframe
#' @param model A model object of class "lm" or a valid formula.
#' @param new_predictor An (unquoted) variable to try in the model
#'
#' @return A new model object
#'
#' @importFrom stats lm formula
#'
#' @export
add_one_predictor <- function(df, model, new_predictor) {

  if (!(class(model) %in% c("formula", "lm"))) {
    stop("Please supply either a formula or a model.")
  } else if (class(model) == "formula") {
    my_formula <- model
  } else if (class(model) == "lm") {
    my_formula <- model_to_formula(model)
  }

  new_predictor <- rlang::as_string(rlang::ensym(new_predictor))

  new_formula <- formula(paste(deparse(my_formula), " + ", new_predictor))

  new_model <- lm(new_formula, data = df)

  return(new_model)


}


#' Helper function that adds one predictor to a model sans quoting.
#'
#' @param df A dataframe
#' @param my_formula A formula or string.
#' @param new_predictor An (QUOTED) variable to try in the model.
#'
#' @importFrom stats lm formula
#'
#' @return A new model object
add_one_predictor_strings <- function(df, my_formula, new_predictor) {

  new_formula <- formula(paste(deparse(my_formula), " + ", new_predictor))

  new_model <- lm(new_formula, data = df)

  return(new_model)


}

