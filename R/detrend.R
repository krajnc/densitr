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
#' dpa  <- load_dpa(system.file("extdata", "00010001.dpa", package = "densiter"))
#' ## load several dpa objects
#' dpa.list <- load_dpa(dpa.directory = system.file("extdata", package = "densiter"))
#' ## trim the measurement
#' dpa.trimmed <- dtrim(dpa)
#' ## detrend the measurement
#' dpa.detrended <- ddetrend(dpa, type = "linear")
#' ## detrend a list without displaying progress
#' dpa.list.detrended <- lapply(dpa.list, ddetrend, type = "linear")
#' ## detrend a list with displaying progress and run in parallel to
#' ## speed things up - requires pbapply library
#' \dontrun{
#' dpa.list.detrended <- pbapply::pblapply(dpa.list, ddetrend, type = "linear", cl = 7)
#' }
ddetrend <- function(dpa, type = ""){
  if (!inherits(dpa,"dpa")) {stop("not a dpa object")}
  if (type == "linear") {
    trend <- stats::lm(amplitude ~ position, data = dpa$data)
    fit <- stats::predict(trend, newdata = dpa$data)
    dpa$data$amplitude  <- dpa$data$amplitude - fit + fit[1]
  } else if (type == "gam") {
    if (requireNamespace("mgcv", quietly = TRUE)) {
      m <- mgcv::gam(amplitude~s(position), data = dpa$data, method = "REML")
      dpa$data$amplitude  <- dpa$data$amplitude - m$fitted.values + m$fitted.values[1]
    } else {stop("Package \"mgcv\" needed for GAM detrending. Please install it.")}
  } else {stop("Please specify detrending function.")}
  return(dpa)
}
