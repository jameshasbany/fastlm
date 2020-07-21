
#' Creates a dataframe of bootstrapped standard errors given a regression, allowing the user to create
#' confidence intervals, calculate means, plot graphs, etc.
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param model A model object of class "lm" or a valid formula.
#' @param n Number of samples to take from the df and regressions to run
#'
#' @return A list of three data frames with bootstrapped estimates of coefficients,
#' standard errors, and summary statistics.
#'
#' @examples
#'
#' bootstrap_lm(mtcars, mpg ~ cyl + am + hp)
#'
#' @importFrom stats lm
#'
#' @export
bootstrap_lm <- function(df, model, n = 1000){

  if (!(class(model) %in% c("formula", "lm"))) {
    stop("Please supply either a formula or a model.")
  } else if (class(model) == "lm") {
    model <- model_to_formula(model)
  }

  boots <- modelr::bootstrap(df, n)

  bs_models <- purrr::map(boots$strap, ~lm(model, data.frame(.x)))

  res <- list(
    estimate = purrr::map_dfr(bs_models, ~.x$coefficients),
    std_error = purrr::map_dfr(bs_models, ~sqrt(diag(vcov(.x)))),
    statistics = dplyr::bind_cols(
      purrr::map_dfr(bs_models, ~summary(.x)[c("r.squared", "adj.r.squared")]),
      purrr::map_dfr(bs_models, ~summary(.x)["fstatistic"]$fstatistic)
    )
  )

  return(res)
}
