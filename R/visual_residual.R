
#' Visualizes the distribution of, simple or multiple, regression residuals, checking for normal distribution
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param response The dependent variable of the regression, as a string
#' @param predictors A vector of strings containing each independent variable used in the model
#' @return A density histogram of the residuals
#' @export
visual_residual <- function(df, response, predictors){
  my_predictors <- paste(predictors, collapse = " + ")
  my_formula <- paste(response, "~", my_predictors)
  mod <- lm(my_formula, data=df)
  df$residuals <- residuals(mod)
  plot <- ggplot2::ggplot(df, ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                   fill = "white",
                   color = "black") +
    ggplot2::geom_density(fill = "cornflowerblue", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = 0, color = "red", lwd = 1)
  return(plot)
}
