#' Checks omitted variable bias in a variable between an initial regression and a longer one
#'
#' @param df A dataframe
#' @param response The dependent variable of the regressions, as a string
#' @param biasvar The independent variable to be checked for bias
#' @param omvars A vector of strings containing each independent variable used in the long model only,
#' also known as the omitted variables
#' @param predictors A vector of strings containing each independent variable used in the initial model
#' and the long model
#' @return A sentence in the form of a vector with each omitted variable and the change in the estimate
#' of the biasvar in question
#' @export
OmVarBias <- function(df, response, biasvar, omvars, predictors) {
  predictors <- paste(predictors, collapse = " + ")
  predictors2 <- paste(biasvar, " + ", predictors, collapse = " + ")
  my_formula <- paste(response, "~", predictors2)
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

