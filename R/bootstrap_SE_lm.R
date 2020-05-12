
#' Runs a linear regression using bootstrapped estimates to create the covarience matrix.
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param response The dependent variable of the regression, as a string
#' @param predictors A vector of strings containing each independent variable used in the model
#' @param number_of_reps Number of samples to take from the df and regressions to run
#' @return A linear regression result, similar to summary of a model
#' @export
bootstrap_SE_lm <- function(df, response, predictors, number_of_reps) {
  df_se <- bootstrap_EST_df(df, response, predictors, number_of_reps)
  m8trix = matrix(, nrow = nrow(df_se), ncol = ncol(df_se))
  m8trix_mean = matrix(, nrow = nrow(df_se), ncol = ncol(df_se))
  for (i in 1:ncol(df_se)){
    coeff_vec <- df_se[,i]
    m8trix[,i] <- coeff_vec
    m8trix_mean[,i] <- mean(coeff_vec)
  }
  m8trix_diff <- (m8trix - m8trix_mean)
  cov_matrix <- ((nrow(m8trix)-1)^-1 * t(m8trix_diff) %*% m8trix_diff)
  my_starters <- paste(predictors, collapse = " + ")
  my_formula <- paste(response, "~", my_starters)
  mod <- lm(my_formula, data=df)
  result <- lmtest::coeftest(mod, vcov = cov_matrix)
  return(broom::tidy(result))
}




#' Helper function that creates a dataframe of bootstrapped coefficient estimates given a regression, allowing
#' the user to create confidence intervals, calculate means, plot graphs, etc.
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param response The dependent variable of the regression, as a string
#' @param predictors A vector of strings containing each independent variable used in the model
#' @param number_of_reps Number of samples to take from the df and regressions to run
#' @return A dataframe with a column for each variable with rows containing each sample regression's
#' estimates
#' @export
bootstrap_EST_df <- function(df, response, predictors, number_of_reps){
  store_df = data.frame()
  my_predictors <- paste(predictors, collapse = " + ")
  my_formula <- paste(response, "~", my_predictors)
  for (i in 1:number_of_reps) {
    my_boots <- dplyr::sample_n(df, nrow(df), replace = TRUE)
    mod <- lm(my_formula, data=my_boots)
    coeff <- unlist(mod[1])
    coeff_vector <- as.vector(coeff)
    df2 <- data.frame(coeff_vector)
    df_transpose <- t(df2)
    store_df <- rbind(store_df, df_transpose)
    rownames(store_df) <- 1:nrow(store_df)
  }
  return(tibble::as_tibble(store_df))
}
