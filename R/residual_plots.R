#' Plots the histogram of regression residuals to check for normality.
#' Includes results of Shapiro-Wilk test in visualization.
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param model A model object or valid formula.
#'
#' @return A density histogram of the residuals
#'
#' @examples
#'
#' mtcars %>%
#'     residual_histogram(hp ~ cyl)
#'
#' my_lm <- lm(hp ~ cyl, data = mtcars)
#'
#' mtcars %>%
#'     residual_histogram(my_lm)
#'
#' @import ggplot2
#'
#' @export
residual_histogram <- function(df, model){

  if (class(model) == "formula") {
    model <- lm(model, data = df)
  }

  df$residuals <- residuals(model)

  sw_pv <- round(shapiro.test(df$residuals)$p.value, 4)

  sig <- sd(df$residuals)
  n <- nrow(df)

  ggplot(df, aes(x = residuals)) +
    geom_histogram(aes(y = ..density..),
                   color = "black",
                   fill = "lightgrey",
                   bins = ceiling(sqrt(n)*2)) +
    geom_density(color = "cornflowerblue",
                 fill = "cornflowerblue",
                 alpha = 0.5) +
    geom_function(fun = ~dnorm(., 0, sig),
                  fill = "indianred",
                  color = "indianred",
                  alpha = 0.5,
                  lwd = 1,
                  lty = 2) +
    geom_vline(xintercept = median(df$residuals),
               color = "cornflowerblue",
               lwd = 1) +
    xlab(glue::glue("Residuals \n
                    Shapiro-Wilk p-value: {sw_pv}")) +
    theme_classic()

}



#' Creates scatter plot of residuals and fitted values
#' (and optionally also selected variables).
#'
#' @param df A data frame
#' @param model A model object or valid formula
#' @param ... Any number of (unquoted) variable names to plot the residuals
#' against. If none are supplied, only fitted values will be shown.
#'
#' @return A scatter plot of the relationship between the supplied variable(s) and the residuals
#'
#' @examples
#'
#' mtcars %>%
#'     residual_scatterplots(mpg ~ hp + cyl, hp, wt)
#'
#' @import ggplot2
#' @import rlang
#'
#' @export
residual_scatterplots <- function(df, model, ...) {

  if (!(class(model) %in% c("formula", "lm"))) {
    stop("Please supply either a formula or a model.")
  } else if (class(model) == "formula") {
    model <- lm(model, data = df)
  }

  df$residuals <- residuals(model)
  df$fitted.values <- model$fitted.values

  print(
  ggplot(df, aes(x = fitted.values, y = residuals)) +
      geom_point() +
      stat_smooth(col = "cornflowerblue") +
      geom_hline(yintercept = 0,
                 color = "indianred",
                 lwd = 1) +
      xlab("Fitted Values") +
      ylab("Residuals")
  )

  extra_vars <- enquos(...)

  if (length(extra_vars) > 0) {

     purrr::walk(extra_vars,
        ~ print(
          ggplot(df, aes(x = !!(.x), y = residuals)) +
          geom_point() +
          stat_smooth(col = "cornflowerblue") +
          geom_hline(yintercept = 0,
                     color = "indianred",
                     lwd = 1) +
          xlab(quo_text(.x)) +
          ylab("Residuals")
    ))
  }

}



#' Creates scatter plot of residuals and fitted values
#' (and optionally also selected variables).
#'
#' @param df A data frame
#' @param model A model object or valid formula
#' @param ... Any number of (unquoted) variable names to plot the residuals
#' against.
#'
#' @return A boxplot of the relationship between the supplied variable(s) and the residuals
#'
#' @examples
#'
#' mtcars %>%
#'     residual_boxplots(mpg ~ hp + cyl, am)
#'
#' @import ggplot2
#' @import rlang
#'
#' @export
residual_boxplots <- function(df, model, ...) {

  if (!(class(model) %in% c("formula", "lm"))) {
    stop("Please supply either a formula or a model.")
  } else if (class(model) == "formula") {
    model <- lm(model, data = df)
  }

  df$residuals <- residuals(model)


  extra_vars <- enquos(...)

  if (length(extra_vars) > 0) {

    df <- df %>%
      dplyr::mutate_at(vars(!!!extra_vars), factor)

    purrr::walk(extra_vars,
                ~ print(
                  ggplot(df, aes(x = !!(.x), y = residuals)) +
                    geom_boxplot(fill = "cornflowerblue",
                                 alpha = 0.5) +
                    geom_hline(yintercept = 0,
                               color = "indianred",
                               lwd = 1) +
                    xlab(quo_text(.x)) +
                    ylab("Residuals") +
                    theme_classic()
                ))
  }

}

