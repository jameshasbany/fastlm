
#' Checks omitted variable bias in a variable between an initial regression and a longer one
#'
#' @param df A dataframe
#' @param model A model object of class "lm" or a valid formula.
#' @param target_predictor The (unquoted) independent variable to be checked for bias
#' @param ... Any number of (unquoted) variables to omitted for checking bias of
#' the target predictor
#'
#' @return A tibble with each omitted variable and the resulting change in the
#' estimate of the target predictor in question
#'
#' @examples
#'
#' check_bias(mtcars, mpg ~ cyl + hp, hp, am)
#'
#' @importFrom rlang as_string ensym ensyms enquos
#' @import dplyr
#' @importFrom stats lm
#'
#' @export
check_bias <- function(df, model, target_predictor, ...) {

  if (!(class(model) %in% c("formula", "lm"))) {
    stop("Please supply either a formula or a model.")
  } else if (class(model) == "formula") {
    my_formula <- model
    model <- lm(my_formula, data = df)
  } else if (class(model) == "lm") {
    my_formula <- model_to_formula(model)
  }

  target_predictor <- as_string(ensym(target_predictor))

  orig_est <- model$coefficients[[target_predictor]]

  to_try <- purrr::map_chr(ensyms(...), as_string)

  if (length(to_try) == 0) {

    to_try <- names(model$coefficients)[-1][-target_predictor]

  }


  new_ests <- purrr::map_dbl(enquos(...),
                             ~add_one_predictor(df, my_formula, !!.x)$coefficients[[target_predictor]])



  results <- tibble::tibble(
    ommitted_var = to_try,
    original_coef = rep(orig_est, length(to_try)),
    new_coef = new_ests,
    diff = original_coef - new_coef
  ) %>%
    purrr::map_dfc(unname) %>%
    arrange(desc(abs(diff)))

  return(results)
}



#' Checks omitted variable bias in a variable between an initial regression and a longer one
#'
#' @param df A dataframe
#' @param model A model object of class "lm" or a valid formula.
#' @param ... Any number of (unquoted) variables to omitted for checking bias of
#' the target predictor
#'
#' @return A tibble with each omitted variable and the resulting change in the
#' estimate of the target predictor in question
#'
#' @examples
#'
#' check_bias(mtcars, mpg ~ cyl + hp, hp, am)
#'
#' @importFrom rlang as_string ensym ensyms enquos
#' @import dplyr
#' @importFrom stats lm
#'
#' @export
check_bias_all <- function(df, model, ...) {

  if (!(class(model) %in% c("formula", "lm"))) {
    stop("Please supply either a formula or a model.")
  } else if (class(model) == "formula") {
    my_formula <- model
    model <- lm(my_formula, data = df)
  } else if (class(model) == "lm") {
    my_formula <- model_to_formula(model)
  }

  target_predictors <- names(model$coef)

  to_try <- purrr::map_chr(ensyms(...), as_string)

  if (length(to_try) == 0) {

    to_try <- names(model$coef)[-1]

  }

  new_mods <- purrr::map(to_try,
                             ~add_one_predictor_strings(df, my_formula, .x))


  new_coefs <- purrr::map_dfr(new_mods, ~as.list(.x$coef[-length(.x$coef)])) %>%
    mutate(
      omitted_var = to_try
    )

  diffs <- purrr::map_dfc(target_predictors, ~model$coefficients[[.x]] - new_coefs[[.x]]) %>%
    mutate(
      omitted_var = to_try
    )

  names(diffs) <- names(new_coefs)

  results <- new_coefs %>%
    full_join(diffs, by = "omitted_var", suffix = c("", "_change")) %>%
    select(omitted_var, everything()) %>%
    purrr::map_dfc(unname)


  return(results)
}



