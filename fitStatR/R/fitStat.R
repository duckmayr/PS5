#' Calculate Fit Statistics for Forecasting Models
#' 
#' \code{fitStat} calculates various fit statistics for statistical models.
#' 
#' @param y A numeric vector of length n (the number of observations) giving
#'   the observed outcomes
#' @param P A numeric matrix of n nows and m (the number of forecasting models)
#'   columns, where each entry i,j is the predicted value of observation i
#'   from forecasting model j
#' @param r An optional numeric vector of naive forecasts; \code{r} must be
#'   given if MRAE should be calculated
#' @param stat A character vector whose elements are the names of the fit
#'   statistic functions to use; should be some combination of 'rmse', 'mad',
#'   'rmsle', 'mape', 'meape', and 'mrae'. By default all applicable fit
#'   statistic functions are used.
#' 
#' @return A numeric matrix with m rows where each i,j entry is the relevant
#'   fit statistic for forecasting model i
#'
#' @examples
#' y <- 0:2
#' P <- matrix(c(1:3, 3:1, rep(2, 3)), nrow=3)
#' r <- rnorm(3)
#' fitStat(y, P) # warning about zero value(s) in y
#' fitStat(y, P, r) # warning about zero value(s) in y
#' y <- 1:3
#' fitStat(y, P)
#' fitStat(y, P, r)
#' fitStat(y, P, stat='rmse') # if you only want one of the fit statistics
#' fitStat(y, P, stat=c('rmse', 'rmsle')) # if you want some but not all
#' y <- c(NA, 2, 3)
#' fitStat(y, P) # warning about NA value(s) in y
#' fitStat(y, P, r) # warning about NA value(s) in y
#' y <- c(0, NA, 2)
#' fitStat(y, P) # warning about zero and NA value(s) in y
#' fitStat(y, P, r) # warning about zero and NA value(s) in y
#' 
#' @export
fitStat <- function(y, P, r=NULL, stat=c('rmse','mad','rmsle','mape','meape')){
  if (any(is.na(c(y, P, r)))) {
    indices <- which(sapply(c(y, P, r), is.na)) %% length(y)
    warning('Missing values were omitted.', call.=FALSE)
    return(fitStat(y[-indices], P[-indices, ], r[-indices], stat=stat))
  }
  if (any(na.omit(y) == 0) & ('mape' %in% stat | 'meape' %in% stat)) {
    out <- paste('One or more y values are equal to 0.', 
                 'Absolute percentage error (which is used in MAPE and MEAPE)',
                 'is undefined for such observations.')
    warning(out, call.=FALSE)
  }
  if (is.null(r)) {
    return(sapply(stat, do.call, list(y, P)))
  }
  return(sapply(unique(c(stat, 'mrae')), do.call, list(y, P, r)))
}
