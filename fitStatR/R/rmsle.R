#' Calculate Root Mean Squared Logarithmic Error (RMSLE)
#' 
#' \code{rmsle} calculates RMSLE for statistical models.
#' 
#' @param y A numeric vector of length n (the number of observations) giving
#'   the observed outcomes
#' @param P A numeric matrix of n nows and m (the number of forecasting models)
#'   columns, where each entry i,j is the predicted value of observation i
#'   from forecasting model j
#' @param ... Other arguments that will not be used by this function
#' 
#' @return A numeric vector of length n whose values are the RMSLE for each
#'   forecasting model used to construct P
#' @export
rmsle <- function(y, P, ...){
  return(apply((log(P + 1) - log(y + 1))^2, 2, function(x) sqrt(mean(x))))
}
