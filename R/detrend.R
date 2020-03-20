dpa_detrend_linear <- function(dpa){
  detrend <- lm(amplitude ~ position, data = dpa)
  fit <- predict(detrend, newdata = dpa)
  ## detrended  <- dpa$amplitude - fit + fit[1]
  dpa <- dpa %>%
    mutate(amplitude = amplitude - fit + fit[1])

  return(dpa)
}
