
#' Plots the relationship of an dependent variable in a simple regression to the residuals of that
#' regression
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param response The dependent variable of the regression, as a string
#' @param predictor The single dependent variable of the regression, as a string
#' @return A linear plot of the relationship between the dependent variable and the residuals
#' @export
simple_visual_residual <- function(df, response, predictor) {
  my_formula <- paste(response, "~", predictor)
  mod <- lm(my_formula, data=df)
  df$residuals <- residuals(mod)
  plot <- ggplot2::ggplot(df, ggplot2::aes_string(x = predictor, y = "residuals")) +
    ggplot2::geom_point() +
    ggplot2::stat_smooth(method = "lm", col = "red")
  return(plot)
}
