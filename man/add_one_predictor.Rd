% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_predictors.R
\name{add_one_predictor}
\alias{add_one_predictor}
\title{Helper function that adds one predictor to a model.}
\usage{
add_one_predictor(df, model, new_predictor)
}
\arguments{
\item{df}{A dataframe}

\item{model}{A model object of class "lm" or a valid formula.}

\item{new_predictor}{An (unquoted) variable to try in the model}
}
\value{
A new model object
}
\description{
Helper function that adds one predictor to a model.
}
