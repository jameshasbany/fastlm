

#' Visualizes the distribution of, simple or multiple, regression residuals, checking for normal distribution
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param output The dependent variable of the regression, as a string
#' @param startersvec A vector of strings containing each independent variable used in the model
#' @return A density histogram of the residuals
#' @export
visresid <- function(df, output, startersvec){
  my_starters <- paste(startersvec, collapse = " + ")
  my_formula <- paste(output, "~", my_starters)
  mod <- lm(my_formula, data=df)
  df$residuals <- residuals(mod)
  plot <- ggplot(df, aes(x = residuals)) +
    geom_histogram(aes(y = ..density..),
                   fill = "white",
                   color = "black") +
    geom_density(fill = "cornflowerblue", alpha = 0.5) +
    geom_vline(xintercept = 0, color = "red", lwd = 1)
  return(plot)
}

#' Plots the relationship of an dependent variable in a simple regression to the residuals of that
#' regression
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param output The dependent variable of the regression, as a string
#' @param xvar The single dependent variable of the regression, as a string
#' @return A linear plot of the relationship between the dependent variable and the residuals
#' @export
simplelmvis <- function(df, output, xvar) {
  my_formula <- paste(output, "~", xvar)
  mod <- lm(my_formula, data=df)
  df$residuals <- residuals(mod)
  plot <- ggplot(df, aes_string(x = xvar, y = "residuals")) +
    geom_point() +
    stat_smooth(method = "lm", col = "red")
  return(plot)
}

