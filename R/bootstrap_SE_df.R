
#' Creates a dataframe of bootstrapped standard errors given a regression, allowing the user to create
#' confidence intervals, calculate means, plot graphs, etc.
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param response The dependent variable of the regression, as a string
#' @param predictors A vector of strings containing each independent variable used in the model
#' @param number_of_reps Number of samples to take from the df and regressions to run
#' @return A dataframe with a column for each variable with rows containing each sample regression's
#' standard errors
#' @export
bootstrap_SE_df <- function(df, response, predictors, number_of_reps){
  store_df = data.frame()
  my_predictors <- paste(predictors, collapse = " + ")
  my_formula <- paste(response, "~", my_predictors)
  for (i in 1:number_of_reps) {
    my_boots <- dplyr::sample_n(df, nrow(df), replace = TRUE)
    mod <- lm(my_formula, data=my_boots)
    my_se <- sqrt(diag(vcov(mod)))
    temp_df <- data.frame(my_se)
    df_transpose <- t(temp_df)
    store_df <- rbind(store_df, df_transpose)
    rownames(store_df) <- 1:nrow(store_df)
  }
  return(tibble::as_tibble(store_df))
}
