#' Detrend (remove a trend) a density profile either using linear or
#' GAM regression
#'
#' This function will take a dpa object and remove the trend from the
#' measurement either by fitting a linear regression or by fitting a
#' GAM regression using REML. The trend is then subtracted from the
#' actual data and a detrended dpa object is returned. Be advised
#' detrending should be done on measurements without the starting or
#' ending point, e.g. they should be trimmed. GAM is more useful in
#' tree ring detection, while linear regression is more commonly used
#' for further analysis of the density data. GAM requires \code{mcgv}
#' package to run.
#'
#' @param dpa A dpa object, see load_dpa.
#' @param type Either "linear" for a fitting linear regression or
#'   "gam" for a GAM fit using REML.
#' @return A dpa object without the trend.
#' @seealso dtrim, dtriml
#' @export
#' @examples
#' ## load a single file
#' dpa <- load_dpa("data/test.dpa")
#' ## trim the measurement
#' dpa.trimmed <- dtrim(dpa)
#' ## detrend the measurement
#' dpa.detrended <- ddtrend(dpa, type = "linear")
#' ## detrend a list without displaying progress
#' dpa.list.detrended <- lapply(dpa.list, ddetrend, type = "linear")
#' ## detrend a list with displaying progress and run in parallel to
#' ## speed things up - requires pbapply library
#' dpa.list.detrended <- pblapply(dpa.list, ddetrend, type = "linear", cl = 7)
ddetrend <- function(dpa, type = ""){
  if (!inherits(dpa,"dpa")) {stop("not a dpa object")}
  df <-  dpa$data
  if (type == "linear") {
    trend <- lm(amplitude ~ position, data = df)
    fit <- predict(trend, newdata = df)
    new  <- df %>%  dplyr::mutate(amplitude = amplitude - fit + fit[1])
    dpa$data <- new
  } else if (type == "gam") {
    if (requireNamespace("mgcv", quietly = TRUE)) {
      m <- mgcv::gam(amplitude~s(position), data = df, method = "REML")
      new  <- df %>%
        dplyr::mutate(amplitude =  amplitude - m$fitted.values + m$fitted.values[1])
      dpa$data <- new
    } else {stop("Package \"mgcv\" needed for GAM detrending. Please install it.")}
  } else {stop("Please specify detrending function.")}
  return(dpa)
}
