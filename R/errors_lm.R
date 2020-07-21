
#' Computes a linear regression with clustered standard errors.
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param model A model object of class "lm" or a valid formula.
#' @param cluster_predictor An (unquoted) variable to cluster the standard errors by.
#'
#' @return A data frame of model estimates.
#'
#' @examples
#'
#' clustered_errors_lm(mtcars, mpg ~ hp + wt, cyl)
#'
#' @importFrom stats lm
#'
#' @export
clustered_errors_lm <- function(df, model, cluster_predictor) {

  if (!(class(model) %in% c("formula", "lm"))) {
    stop("Please supply either a formula or a model.")
  } else if (class(model) == "formula") {
    model <- lm(model, df)
  }

  clust <- df %>% dplyr::pull({{cluster_predictor}})

  results <- lmtest::coeftest(model, sandwich::vcovCL, cluster = factor(clust))

  return(broom::tidy(results))
}



#' Computes a linear regression with standard errors corrected for
#' heteroskedasticity,  using the
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param model A model object of class "lm" or a valid formula.
#' @param cluster_predictor An (unquoted) variable to cluster the standard errors by.
#' @param type An optional term for specifiying the estimator method, see vcovHC for options
#'
#' @return A data frame of model estimates.
#'
#' @examples
#'
#' corrected_errors_lm(mtcars, mpg ~ hp + wt, cyl)
#'
#' @importFrom stats lm
#'
#' @export
corrected_errors_lm <- function(df, model, cluster_predictor, type = "HC") {

  if (!(class(model) %in% c("formula", "lm"))) {
    stop("Please supply either a formula or a model.")
  } else if (class(model) == "formula") {
    model <- lm(model, df)
  }

  correct_vcov <- sandwich::vcovHC(model, type)

  results <- lmtest::coeftest(model, vcov = correct_vcov)

  return(broom::tidy(results))
}

