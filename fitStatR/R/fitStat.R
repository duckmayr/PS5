#' Calculate Fit Statistics for Forecasting Models
#' 
#' \code{fitStat} calculates various fit statistics for statistical models.
#' 
#' @param y A numeric vector of length n (the number of observations) giving
#'   the observed outcomes
#' @param P A numeric matrix of n nows and m (the number of forecasting models)
#'   columns, where each entry i,j is the predicted value of observation i
#'   from forecasting model j
#' @param r An optional numeric vector of naive forecasts
#' @param stat A character vector whose elements are the names of the fit
#'   statistic functions to use; should be some combination of 'rmse', 'mad',
#'   'rmsle', 'mape', 'meape', and 'mrae'
#' 
#' @return A numeric matrix with m rows where each i,j entry is the relevant
#'   fit statistic for forecasting model i
fitStat <- function(y, P, r=NULL, stat=c('rmse','mad','rmsle','mape','meape')){
  if (!is.null(r) & !('mrae' %in% stat)) {
    stat <- c(stat, 'mrae')
  }
  return(sapply(stat, do.call, list(y, P, r)))
}
