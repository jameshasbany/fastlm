
library(roxygen2)

#' Starting with a base linear regression, checkvars takes turns adding a variables from a list of
#' variables to it one at a time, calculating the change in RSquared Adjusted for each variable addition to
#' to the base regression, and showing the user if the variable was good or bad for the RSquared Adjusted
#'
#' @param df A dataframe R will pull the variables from to run the regressions
#' @param output The dependent variable of the regressions, as a string
#' @param startersvec A vector of strings containing each independent variable used in the initial model
#' @param inputsvec A vector of strings containing each independent variable to be tested with the initial
#' model
#' @return A sentence in the form of a vector with each tested variable, whether it was good or bad for
#' Adjusted RSquared, and the exact change in Adjusted RSquared, rounded to 6 decimal places
#' @export
checkvars <- function(df, output, startersvec, inputsvec) {
  my_starters <- paste(startersvec, collapse = " + ")
  my_formula <- paste(output, "~", my_starters)
  mod <- lm(my_formula, data=df)
  critvalue <- summary(mod)$adj.r.squared
  appendlist <- c()
  for (i in 1:length(inputsvec)) {
    formulaup <- paste(my_formula, " + ", inputsvec[i])
    modalt <- lm(formulaup, data=df)
    adj_r <- summary(modalt)$adj.r.squared
    diff <- round((adj_r - critvalue), 6)
    if (critvalue < adj_r) {
      appendlist <- paste0(appendlist, inputsvec[i], ":", " Good, ", diff, ". ")
    } else {
      appendlist <- paste0(appendlist, inputsvec[i], ":", " Bad, ", diff, ". ")
    }
  }
  return(str_trim(appendlist))
}

#' Checks omitted variable bias in a variable between an initial regression and a longer one
#'
#' @param df A dataframe R will pull the variables from to run the regressions
#' @param output The dependent variable of the regressions, as a string
#' @param biasvar The independent variable to be checked for bias
#' @param omvars A vector of strings containing each independent variable used in the long model only,
#' also known as the omitted variables
#' @param startersvec A vector of strings containing each independent variable used in the initial model
#' and the long model
#' @return A sentence in the form of a vector with each omitted variable and the change in the estimate
#' of the biasvar in question
#' @export
OmVarBias <- function(df, output, biasvar, omvars, startersvec) {
  my_starters <- paste(startersvec, collapse = " + ")
  my_starters2 <- paste(biasvar, " + ", my_starters, collapse = " + ")
  my_formula <- paste(output, "~", my_starters2)
  smod <- lm(my_formula, data=df)
  summary(smod)
  estimshort <- smod$coefficients[biasvar]
  appendlist <- c()
  for (i in 1:length(omvars)) {
    formulaup <- paste(my_formula, " + ", omvars[i])
    lmod <- lm(formulaup, data=df)
    estimlong <- lmod$coefficients[biasvar]
    diff <- round((estimshort - estimlong), 6)
    appendlist <- paste0(appendlist, omvars[i], ":", " OVB = ", diff, ". ")
  }
  return(str_trim(appendlist))
}

