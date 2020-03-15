
#' Runs a simple or multiple regression, automatically correcting for possible heteroskedasticity in
#' the standard errors
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param output The dependent variable of the regression, as a string
#' @param startersvec A vector of strings containing each independent variable used in the model
#' @param type An optional term for specifiying the specific estimator wanted, see vcovHC for options
#' @return Returns summarized output of a linear regression model with corrected standard errors
#' @export
correctedSElm <- function(df, output, startersvec, type = "HC1"){
  my_starters <- paste(startersvec, collapse = " + ")
  my_formula <- paste(output, "~", my_starters)
  mod <- lm(my_formula, data=df)
  correctvcov <- vcovHC(mod, type)
  results <- coeftest(mod, vcov = correctvcov)
  return(results)
}

#' Runs a simple or multiple regression with clustered standard errors
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param output The dependent variable of the regression, as a string
#' @param startersvec A vector of strings containing each independent variable used in the model
#' @param clustervar A variable to cluster the standard errors by.
#' @export
clusterSElm <- function(df, output, startersvec, clustervar) {
  my_starters <- paste(startersvec, collapse = " + ")
  my_formula <- paste(output, "~", my_starters)
  mod <- lm(my_formula, data=df)
  results <- coeftest(mod, vcovCL, cluster = df$clustervar)
  return(results)
}

#' Creates a dataframe of bootstrapped standard errors given a regression, allowing the user to create
#' confidence intervals, calculate means, plot graphs, etc.
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param output The dependent variable of the regression, as a string
#' @param startersvec A vector of strings containing each independent variable used in the model
#' @param numreg Number of samples to take from the df and regressions to run
#' @return A dataframe with a column for each variable with rows containing each sample regression's
#' standard errors
#' @export
bootstrapSEdf <- function(df, output, startersvec, numreg){
  dfstore = data.frame()
  my_starters <- paste(startersvec, collapse = " + ")
  my_formula <- paste(output, "~", my_starters)
  for (i in 1:numreg) {
    my_boots <- sample_n(df, nrow(df), replace = TRUE)
    mod <- lm(my_formula, data=my_boots)
    SE <- sqrt(diag(vcov(mod)))
    df2 <- data.frame(SE)
    df_transpose <- t(df2)
    dfstore <- rbind(dfstore, df_transpose)
  }
  return(dfstore)
}

#' Creates a dataframe of bootstrapped coefficient estimates given a regression, allowing the user
#' to create confidence intervals, calculate means, plot graphs, etc.
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param output The dependent variable of the regression, as a string
#' @param startersvec A vector of strings containing each independent variable used in the model
#' @param numreg Number of samples to take from the df and regressions to run
#' @return A dataframe with a column for each variable with rows containing each sample regression's
#' estimates
#' @export
bootstrapESTdf <- function(df, output, startersvec, numreg){
  dfstore = data.frame()
  my_starters <- paste(startersvec, collapse = " + ")
  my_formula <- paste(output, "~", my_starters)
  for (i in 1:numreg) {
    my_boots <- sample_n(df, nrow(df), replace = TRUE)
    mod <- lm(my_formula, data=my_boots)
    coeff <- unlist(mod[1])
    coeffvec <- as.vector(coeff)
    df2 <- data.frame(coeffvec)
    df_transpose <- t(df2)
    dfstore <- rbind(dfstore, df_transpose)
  }
  return(dfstore)
}

#' Runs a linear regression using bootstrapped estimates to create the covarience matrix.
#'
#' @param df A dataframe R will pull the variables from to run the regression
#' @param output The dependent variable of the regression, as a string
#' @param startersvec A vector of strings containing each independent variable used in the model
#' @param numreg Number of samples to take from the df and regressions to run
#' @return A linear regression result, similar to summary of a model
#' @export
bootstrapSElm <- function(df, output, startersvec, numreg) {
  dfse <- bootstrapESTdf(df, output, startersvec, numreg)
  m8trix = matrix(, nrow = nrow(dfse), ncol = ncol(dfse))
  m8trix_mean = matrix(, nrow = nrow(dfse), ncol = ncol(dfse))
  for (i in 1:ncol(dfse)){
    vec <- dfse[,i]
    m8trix[,i] <- vec
    m8trix_mean[,i] <- mean(vec)
  }
  diff <- (m8trix - m8trix_mean)
  Cov <- ((nrow(m8trix)-1)^-1 * t(diff) %*% diff)
  my_starters <- paste(startersvec, collapse = " + ")
  my_formula <- paste(output, "~", my_starters)
  mod <- lm(my_formula, data=df)
  result <- coeftest(mod, vcov = Cov)
  return(result)
}

