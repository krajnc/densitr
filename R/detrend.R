#' Detrend (remove a trend) a density profile either using linear or
#' GAM regression
#'
#' This function will take a dp object and remove the trend from the
#' measurement either by fitting a linear regression or by fitting a
#' GAM regression using REML. The trend is then subtracted from the
#' actual data and a detrended dp object is returned. Be advised
#' detrending should be done on measurements without the starting or
#' ending point, e.g. they should be trimmed. GAM is more useful in
#' tree ring detection, while linear regression is more commonly used
#' for further analysis of the density data. GAM requires \code{mcgv}
#' package to run.
#'
#' @param dp A dp object, see dpload.
#' @param type Either "linear" for a fitting linear regression or
#'   "gam" for a GAM fit using REML.
#' @return A dp object without the trend.
#' @seealso dptrim, dptriml, dptrim_s, dptriml_s
#' @export
#' @examples
#' ## load a single file
#' dp <- dpload(system.file("extdata", "00010001.dpa", package = "densitr"))
#' ## load several dp objects
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densitr"))
#' ## trim the measurement
#' dp.trimmed <- dptrim(dp)
#' ## detrend the measurement
#' dp.detrended <- dpdetrend(dp, type = "linear")
#' ## detrend a list without displaying progress
#' dp.list.detrended <- lapply(dp.list, dpdetrend, type = "linear")
#' ## detrend a list with displaying progress and run in parallel to
#' ## speed things up - requires pbapply library, adjust the cl argument to
#' ## desired number of cores
#' \donttest{
#' dp.list.detrended <- pbapply::pblapply(dp.list, dpdetrend, type = "linear", cl = 1)
#' }
dpdetrend <- function(dp, type = "") {
  if (!inherits(dp, "dp")) {
    stop("not a dp object")
  }
  if (type == "linear") {
    trend <- stats::lm(amplitude ~ position, data = dp$data)
    fit <- stats::predict(trend, newdata = dp$data)
    dp$data$amplitude <- dp$data$amplitude - fit + fit[1]
  } else if (type == "gam") {
    if (requireNamespace("mgcv", quietly = TRUE)) {
      m <- mgcv::gam(amplitude ~ s(position), data = dp$data, method = "REML")
      dp$data$amplitude <- dp$data$amplitude - m$fitted.values + m$fitted.values[1]
      rownames(dp$data) <- NULL
    } else {
      stop("Package \"mgcv\" needed for GAM detrending. Please install it.")
    }
  } else {
    stop("Please specify detrending function, either 'gam' or 'linear'.")
  }
  return(dp)
}
