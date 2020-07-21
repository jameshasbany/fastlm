
#' A simple wrapper on \code{lm()} that takes input in a pipe-friendly order
#' and gives output tidied by \code{broom::tidy()}
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param model A model object of class "lm" or a valid formula.
#' @param ... Further arguments to \code{lm()}
#'
#' @return A data frame of model information
#'
#' @examples
#'
#' tidy_lm(mtcars, mpg ~ cyl + am + hp, ...)
#'
#' @export
tidy_lm <- function(df, model, ...){

  if (!(class(model) %in% "formula")) {
    stop("Please supply a valid formula.")
  }

  return(broom::tidy(lm(model, df, ...)))
}
