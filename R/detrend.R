ddetrend <- function(dpa, type = ""){
  if (!inherits(dpa,"dpa"))  {
    stop("not a dpa object")
  }
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
    } else {
      stop("Package \"mgcv\" needed for GAM detrending. Please install it.")
    }
  } else {
    stop("Please specify detrending function.")
  }
  #names(dpa$data)  <- dpa$footer$ID
  return(dpa)
}
