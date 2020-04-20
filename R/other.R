## Found somewhere on StackExchange, posibly on https://
## stackoverflow.com/questions/16357962/r-split-numeric-vector-at-
## position.
splitAt <- function(x, pos) {
  pos <- c(1L, pos, length(x) + 1L)
  Map(function(x, i, j) x[i:j], list(x), utils::head(pos, -1L), utils::tail(pos, -1L) - 1L)
}

## a faster alternative to zoo::rollmeanr
## found on https://stackoverflow.com/questions/30090336/why-is-zoorollmean-slow-compared-to-a-simple-rcpp-implementation
baseR.rollmean <- function(dat, window) {
  n <- length(dat)
  y <- dat[window:n] - dat[c(1, 1:(n - window))]
  y[1] <- sum(dat[1:window])
  return(cumsum(y) / window)
}
