#' Extracts formula call from an lm object
#'
#' @param model An object of class "lm"
#'
#' @return An object of class "formula"
#'
#' @export
model_to_formula <- function(model) {

  return(eval(model$call[[2]]))

}
