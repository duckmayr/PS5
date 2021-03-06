% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmsle.R
\name{rmsle}
\alias{rmsle}
\title{Calculate Root Mean Squared Logarithmic Error (RMSLE)}
\usage{
rmsle(y, P, ...)
}
\arguments{
\item{y}{A numeric vector of length n (the number of observations) giving
the observed outcomes}

\item{P}{A numeric matrix of n nows and m (the number of forecasting models)
columns, where each entry i,j is the predicted value of observation i
from forecasting model j}

\item{...}{Other arguments that will not be used by this function}
}
\value{
A numeric vector of length n whose values are the RMSLE for each
  forecasting model used to construct P
}
\description{
\code{rmsle} calculates RMSLE for statistical models.
}
