#' Calculate Median Relative Absolute Error (MRAE)
#' 
#' \code{mrae} calculates MRAE for statistical models.
#' 
#' @param y A numeric vector of length n (the number of observations) giving
#'   the observed outcomes
#' @param P A numeric matrix of n nows and m (the number of forecasting models)
#'   columns, where each entry i,j is the predicted value of observation i
#'   from forecasting model j
#' @param r A numeric vector of naive forecasts
#' 
#' @return A numeric vector of length n whose values are the MRAE for each
#'   forecasting model used to construct P
mrae <- function(y, P, r){
  e <- abs(P - y)
  b <- abs(r - y)
  return(apply(e, 2, function(x) median(x / b)))
}
