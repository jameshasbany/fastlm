
#' Checks omitted variable bias in a variable between an initial regression and a longer one
#'
#' @param df A dataframe R will pull the variables from to run the regressions
#' @param response The dependent variable of the regressions, as a string
#' @param predictor_to_check The independent variable to be checked for bias
#' @param other_predictors A vector of strings containing each independent variable used in the initial model
#' and the long model
#' @param omitted_variables A vector of strings containing each independent variable used in the long model only,
#' also known as the omitted variables
#' @return A sentence in the form of a vector with each omitted variable and the change in the estimate
#' of the biasvar in question
#' @export
check_bias <- function(df, response, predictor_to_check, other_predictors, omitted_variables) {
  secondary_predictors <- paste(other_predictors, collapse = " + ")
  all_predicators <- paste(predictor_to_check, " + ", secondary_predictors, collapse = " + ")
  my_formula <- paste(response, "~", all_predicators)
  short_mod <- lm(my_formula, data=df)
  estimate_short <- short_mod$coefficients[predictor_to_check]
  appendlist <- c()
  for (i in 1:length(omitted_variables)) {
    formulaup <- paste(my_formula, " + ", omitted_variables[i])
    long_mod <- lm(formulaup, data=df)
    estimlong <- long_mod$coefficients[predictor_to_check]
    estimate_diff <- round((estimate_short - estimlong), 6)
    appendlist <- paste0(appendlist, omitted_variables[i], ":", " OVB = ", estimate_diff, ". ")
  }
  return(stringr::str_trim(appendlist))
}
