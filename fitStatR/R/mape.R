#' Calculate Mean Absolute Percentage Error (MAPE)
#' 
#' \code{mape} calculates MAPE for statistical models.
#' 
#' @param y A numeric vector of length n (the number of observations) giving
#'   the observed outcomes
#' @param P A numeric matrix of n nows and m (the number of forecasting models)
#'   columns, where each entry i,j is the predicted value of observation i
#'   from forecasting model j
#' 
#' @return A numeric vector of length n whose values are the MAPE for each
#'   forecasting model used to construct P
#' @export
mape <- function(y, P, ...){
  return(apply(abs(P - y), 2, function(x) mean((x/y) * 100)))
}
