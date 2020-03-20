dpa_detrend <- function(dpa, type = c("linear","gam")){
  if (type == "linear") {
    trend <- lm(amplitude ~ position, data = dpa)
    fit <- predict(trend, newdata = dpa)
    dpa <- dpa %>%
      dplyr::mutate(amplitude = amplitude - fit + fit[1])
  } else if (type == "gam") {
    if (requireNamespace("mgcv", quietly = TRUE)) {
      m <- mgcv::gam(amplitude~s(position), data = dpa, method = "REML")
      dpa <- dpa %>%
        dplyr::mutate(amplitude =  amplitude - m$fitted.values + m$fitted.values[1])
    } else {
      stop("Package \"mgcv\" needed for GAM detrending. Please install it.")
    }
  }
  return(dpa)
}
