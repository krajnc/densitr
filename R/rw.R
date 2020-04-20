#' Get ring widths from identified tree rings
#'
#' Called on an object returned by \code{dprings}, it will return ring
#' widths for all detected rings. The units are determined by the
#' xUnit from the footer of density profile.
#'
#' @param rings A data frame with the identified rings, a result of
#'   the dprings() call on an individual profile
#' @return A vector of ring widths, which are peak-to-peak differences.
#' @seealso dprings
#' @export
#' @examples
#' ## load a single file
#' dp <- dpload(system.file("extdata", "00010001.dpa", package = "densitr"))
#' ## trim and detrend the measurement
#' dp.trimmed <- dptrim(dp)
#' dp.detrended <- dpdetrend(dp.trimmed, type = "gam")
#' ## identify rings
#' rings <- dprings(dp.detrended)
#' ## get tree ring widths:
#' get_RW(rings)
get_RW <- function(rings) {
  if (is.data.frame(rings) == FALSE |
        all(colnames(rings) == c("value", "type", "amplitude")) == FALSE) {
    stop("not a result of densitr::dprings()")
  }
  return(tryCatch(diff(rings[rings$type == "peak", ]$value,
                       error = function(e) NULL
                       )))
}

#' Automatically identify tree rings in a density profile
#'
#' Called on a density profile it will return tree rings, which were
#' automatically detected in the density profile. For best results,
#' run on a trimmed and detrended density profile (use GAM for best
#' results, see \code{dpdetrend}). The function will then search for
#' local peaks and valleys within the profile. Normally works well in
#' softwood species, where density increases in late wood and
#' decreases in nearly wood. It will return a data frame containing
#' peaks and valleys, along with their horizontal position. A
#' diagnostic plot will be returned instead when return.plot = TRUE.
#' Green points are valleys, blue points are peaks and red points were
#' automatically excluded. The algorithm will search for peaks and
#' valleys, after which it will automatically exclude all repeated
#' points. Each peak should be followed by a valley and vice versa,
#' when peak-peak situation is found, it will always take the higher
#' peak and the opposite in valleys (keeps the lowest values). Adjust
#' sensitivity by either adjusting \code{pps}, which dictates how many
#' points on each side of the identified peak are the minimum.
#' Essentially this dictates the minimum width of detected rings, try
#' adjusting it and display the plot. Minimum peak value can also be
#' adjusted with the parameter threshold, which dictates how many
#' stand deviations from the mean amplitude of the profile is the
#' lowest minimum peak value. Before ring detection the profile can
#' also be denoised by setting \code{smooth = TRUE}, which applies a
#' loess regression to smooth the data using the span parameter.
#'
#'
#' @param dp An dp object, see \code{dpload}
#' @param return.plot Return a plot instead of peak/valley data frame?
#'   If TRUE, returns a plot instead of a dp object.
#' @param pps Points per peak, the minimum width of a peak, half on
#'   each side. A local peak is identified when half of those points
#'   are lower on each side of the potential peak. The inverse is true
#'   in valleys.
#' @param threshold.sd Minimum peak value in standard deviations away
#'   from the overall mean of the signal. By default no peaks are
#'   allowed to be beneath the overall mean, can be adjusted to
#'   negative to lower the minimum peak allowed.
#' @param return.plot If TRUE, the function will return a diagnostic
#'   plot. Green points are valleys, blue points are peaks and red
#'   points were automatically excluded.
#' @param smooth Set to TRUE, the profile will be denoised using a
#'   LOESS regression.
#' @param span Span of the LOESS regression.
#' @return A data frame including the values and positions for all
#'   peaks and values. Usually piped into \code{get_RW} to get ring
#'   widths.
#' @seealso get_RW
#' @export
#' @examples
#' \donttest{
#' ## load a single file
#' dp <- dpload(system.file("extdata", "00010001.dpa", package = "densitr"))
#' ## trim and detrend the measurement
#' dp.trimmed <- dptrim(dp)
#' dp.detrended <- dpdetrend(dp.trimmed, type = "gam")
#' ## identify rings
#' rings <- dprings(dp.detrended)
#' ## plot a diagnostic
#' dprings(dp.detrended, return.plot = TRUE)
#' ## get tree ring widths:
#' get_RW(rings)
#' }
dprings <- function(dp, pps = 200, threshold.sd = 0,
                    return.plot = FALSE, smooth = FALSE, span = 0.01) {
  if (!inherits(dp, "dp")) {
    stop("not a dp object")
  }
  if (smooth == FALSE) {
    cutoff <- mean(dp$data$amplitude, na.rm = TRUE) +
      (threshold.sd * stats::sd(dp$data$amplitude, na.rm = TRUE))
    pk <- find_peaks(dp$data$amplitude, m = pps / 2)
    ## delete all peaks below threshold, valleys will be removed by remove_duplicates()
    pk <- pk[dp$data$amplitude[pk] > cutoff]
    val <- find_peaks(-dp$data$amplitude, m = pps / 2)
    values <- rbind(
      data.frame(value = pk, type = "peak"),
      data.frame(value = val, type = "valley")
    )
    values <- values[order(values$value), ]
    values$amplitude <- dp$data$amplitude[values$value]
  } else {
    y.smooth <- stats::loess(amplitude ~ position, data = dp$data, span = span)$fitted
    cutoff <- mean(y.smooth, na.rm = TRUE) +
      (threshold.sd * stats::sd(y.smooth, na.rm = TRUE))
    pk <- find_peaks(y.smooth, m = pps / 2)
    ## delete all peaks below threshold, valleys will be removed by remove_duplicates()
    pk <- pk[y.smooth[pk] > cutoff]
    val <- find_peaks(-y.smooth, m = pps / 2)
    values <- rbind(
      data.frame(value = pk, type = "peak"),
      data.frame(value = val, type = "valley")
    )
    values <- values[order(values$value), ]
    values$amplitude <- y.smooth[values$value]
  }
  values2 <- remove_duplicates(values)
  removed <- values[!(values$value %in% values2$value), ]
  if (return.plot == TRUE) {
    if (smooth == FALSE) {
      graphics::plot(
        x = dp$data$position, y = dp$data$amplitude, type = "l",
        xlab = paste0("Drilling depth [", dp$footer$xUnit[1], "]"),
        ylab = paste0("Resistograph density [", dp$footer$yUnit[1], "]"),
        main = paste0("Density profile ID: ", dp$footer$ID)
      )
      graphics::points(
        x = dp$data$position[pk],
        y = dp$data$amplitude[pk], col = "blue", pch = 16
      )
      graphics::points(
        x = dp$data$position[val],
        y = dp$data$amplitude[val], col = "green", pch = 16
      )
      graphics::points(
        x = dp$data$position[removed$value],
        y = removed$amplitude, col = "red", cex = 2, pch = 16
      )
      graphics::abline(h = cutoff, col = "black", lty = 2)
    } else {
      graphics::plot(
        x = dp$data$position, y = y.smooth, type = "l",
        xlab = paste0("Drilling depth [", dp$footer$xUnit[1], "]"),
        ylab = paste0("Resistograph density [", dp$footer$yUnit[1], "]"),
        main = paste0("Density profile ID: ", dp$footer$ID)
      )
      graphics::points(
        x = dp$data$position[pk],
        y = y.smooth[pk], col = "blue", pch = 16
      )
      graphics::points(
        x = dp$data$position[val],
        y = y.smooth[val], col = "green", pch = 16
      )
      graphics::points(
        x = dp$data$position[removed$value],
        y = removed$amplitude, col = "red", cex = 2, pch = 16
      )
      graphics::abline(h = cutoff, col = "black", lty = 2)
    }
    p <- grDevices::recordPlot()
    return(p)
  }
  return(values2)
}

## Supporting function for dprings
## Will take a df of rings and compare them one by one. Peak should
## always be followed by a valley, this function returns those
## duplicating values
get_duplicates <- function(values) {
  removals <- c()
  for (i in 1:(nrow(values) - 1)) {
    k <- i + 1
    if (values[i, ]$type == values[k, ]$type) {
      ## compare two valleys, keep the lowest
      if ((values[i, ]$type == "valley") && (values[k, ]$type == "valley")) {
        if (values[i, ]$amplitude <= values[k, ]$amplitude) {
          removals <- c(removals, k)
        } else {
          removals <- c(removals, i)
        }
      } else if ((values[i, ]$type == "peak") && (values[k, ]$type == "peak")) {
        ## compare two peaks, take the higher one
        if (values[i, ]$amplitude >= values[k, ]$amplitude) {
          removals <- c(removals, k)
        } else {
          removals <- c(removals, i)
        }
      }
    }
  }
  return(removals)
}

## Supporting function for dprings
## If there are peak-peak or valley-valley present, the higher peak
## and the lower valley should stay, the rest will be removed. This is
## run sequentially until there are always peak-valley combinations,
## e.g. will run get_duplicates to remove sequences until we are happy
remove_duplicates <- function(values) {
  success <- FALSE
  while (!success) {
    duplicates <- get_duplicates(values)
    if (length(duplicates) > 0) {
      values <- values[-duplicates, ]
    }
    success <- length(duplicates) == 0
  }
  return(values)
}

### Written by Stasia Grinberg, licensed under GPL-3
## A simple algorithm to find local maxima/minima in sequential data,
## used for peak detection in dprings.
## https://github.com/stas-g/findPeaks
## https://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data/164830#164830
find_peaks <- function(x, m = 3) {
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i) {
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if (all(x[c(z:i, (i + 2):w)] <= x[i + 1])) {
      return(i + 1)
    } else {
      return(numeric(0))
    }
  })
  pks <- unlist(pks)
  pks
}
