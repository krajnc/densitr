#' Detect measurement starting point automatically using changepoint
#' segmentation
#'
#' A typical resistance drilling measurement starts with an increase
#' in resistance values in between the measurement start and the
#' immersion of the needle in the wood. These values are not useful
#' when estimating density and should be removed before further
#' analysis. This function will detect the starting point
#' automatically using binary segmentation from the package
#' \code{changepoint}, which separates the measurement in segments
#' based on their mean and variance. Start is detected, when the
#' segment mean is outside of the cutoff limit, see \code{return.plot
#' = TRUE} to display the diagnostic plot. This function will only
#' check the mean values of the first four (4) segments and compare
#' them to the cutoff value. The function is called on a dp object
#' and returns either a row number of the starting point or a plot
#' displaying the segmentation and detection. The sensitivity can be
#' adjusted using the cutoff.sd parameter, which is an indicator on
#' how many standard deviations the segment mean value can be before
#' cutting it off. Will return a warning if start not detected.
#' @param dp A dp object, see dpload.
#' @param cutoff.sd How many standard deviations for the cutoff limit?
#' @param return.plot If true, will return a plot displaying segment
#'   detection for the current dp file.
#' @return Either a row number where the actual measurement starts or
#'   a plot, displaying changepoint segmentation and set limits.
#' @seealso dpdetect_e, dptrim, dptriml, dptrim_s, dptriml_s
#' @export
#' @examples
#' ## load a single file
#' dp <- dpload(system.file("extdata", "00010001.dpa", package = "densitr"))
#' ## get starting point
#' start <- dpdetect_s(dp)
#' ## plot the start detection
#' \donttest{
#' dpdetect_s(dp, return.plot = TRUE)
#' }
dpdetect_s <- function(dp, cutoff.sd = 1, return.plot = FALSE) {
  ## check if dp object
  if (!inherits(dp, "dp")) {
    stop("not a dp object")
  }
  ## get a rolling mean of diff lags
  fit <- stats::loess(dp$data$amplitude ~ dp$data$position, span = 0.1)
  fitted <- stats::predict(fit)
  data.in <- baseR.rollmean(diff(fitted), 100) # defined in others.R
  ## set limits and find segments
  limit <- abs(mean(data.in) + (cutoff.sd * stats::sd(data.in)))
  segments.points <- suppressWarnings(changepoint::cpt.meanvar(data.in,
                                                               method = "BinSeg", Q = 10,
                                                               minseglen = 250, class = FALSE
                                                               ))
  segments.list <- splitAt(data.in, segments.points)
  segments.list[length(segments.list)] <- NULL # remove the last item in a list
  segment.value <- function(number) {
    return(abs(mean(segments.list[[number]])))
  }
  ## check the first four segments, if they are outside of the set,
  ## limit and return the end positions of those segments
  if (segment.value(4) < limit) {
    if (segment.value(3) < limit) {
      if (segment.value(2) < limit) {
        if (segment.value(1) < limit) {
          ## no segments found outside the limit in the first 4
          ## segments
          warning(paste("start not detected in measurement ", dp$footer$ID[1], sep = ""))
          cutoff <- 1
        } else {
          cutoff <- segments.points[1]
        }
      } else {
        ## return the position of the second segment, deleting the
        ## first two
        cutoff <- segments.points[2]
      }
    } else {
      ## return the position of the third segment, deleting the first
      ## three
      cutoff <- segments.points[3]
    }
  } else {
    ## return the position of the fourth segment, deleting the first
    ## four segments
    cutoff <- segments.points[4]
  }
  if (return.plot == TRUE) {
    segments.points2 <- suppressWarnings(changepoint::cpt.meanvar(data.in,
                                                                  method = "BinSeg", Q = 10,
                                                                  minseglen = 250, class = TRUE
                                                                  ))
    graphics::plot.new()
    ## save and restore par setting
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    ## plot
    graphics::par(mfrow = c(2, 1))
    graphics::plot(dp$data$amplitude,
                   type = "l",
                   xlab = paste0("Drilling depth [", dp$footer$xUnit, "]"),
                   ylab = paste0("Resistograph density [", dp$footer$yUnit, "]"),
                   main = paste0("Density profile ID: ", dp$footer$ID)
                   )
    graphics::abline(v = cutoff, col = "red", lwd = 3, lty = 2)
    ## [1:length(dp$data$amplitude)/2]
    changepoint::plot(segments.points2,
                      xlab = paste0("Drilling depth [", dp$footer$xUnit, "]"),
                      ylab = paste0("Moving average of lagged differences"),
                      main = "Detected segments"
                      )
    graphics::abline(h = mean(data.in), col = "blue")
    graphics::abline(h = limit, col = "green")
    graphics::abline(v = cutoff, col = "red", lwd = 3, lty = 2)
    graphics::legend("topright",
                     legend = c("Segment mean", "Overall mean", "Cutoff limit"),
                     col = c("red", "blue", "green"), lty = 1, cex = 1
                     )
    p <- grDevices::recordPlot()
    return(p)
  } else {
    return(cutoff) # add 100 to account for rolling mean right centered
  }
}

#' Detect measurement ending point automatically using changepoint
#' segmentation
#'
#' The opposite of the dpdetect_s, it will check the mean values
#' of the last four segments and compare them to the cutoff limit.
#' Will give a warning if end not detected, which is expected on
#' measurements where the needle did not exit the tree on the opposite
#' side of the tree. See \code{return.plot = TRUE} to display the
#' actual process. The function is called on a dp object and returns
#' either a row number of the measurement ending or a plot displaying
#' the segmentation and detection. The sensitivity can be adjusted
#' using the cutoff.sd parameter, which is an indicator on how many
#' standard deviations the segment mean value can be before cutting it
#' off.
#' @param dp A dp object, see dpload.
#' @param cutoff.sd How many standard deviations for the cutoff limit?
#' @param return.plot If true, will return a plot displaying segment
#'   detection for the current dp file.
#' @return Either a row number where the actual measurement ends or
#'   a plot, displaying changepoint segmentation and set limits.
#' @seealso dpdetect_s, dptrim, dptriml, dptrim_s, dptriml_s
#' @export
#' @examples
#' ## load a single file
#' dp <- dpload(system.file("extdata", "00010001.dpa", package = "densitr"))
#' ## get ending point
#' start <- dpdetect_e(dp)
#' ## plot the end detection
#' \donttest{
#' dpdetect_e(dp, return.plot = TRUE)
#' }
dpdetect_e <- function(dp, cutoff.sd = 1, return.plot = FALSE) {
  ## check if dp object
  if (!inherits(dp, "dp")) {
    stop("not a dp object")
  }
  ## get a rolling mean of diff lags
  fit <- stats::loess(dp$data$amplitude ~ dp$data$position, span = 0.1)
  fitted <- stats::predict(fit)
  data.in <- baseR.rollmean(diff(fitted), 100) # defined in others.R
  ## get limits and get segments
  limit <- mean(data.in) - (cutoff.sd * stats::sd(data.in))
  segments.points <- suppressWarnings(changepoint::cpt.meanvar(data.in,
                                                               method = "BinSeg", Q = 10,
                                                               minseglen = 250, class = FALSE
                                                               ))
  segments.list <- splitAt(data.in, segments.points)
  segments.list[length(segments.list)] <- NULL # remove the last item in a list
  segment.value2 <- function(number) {
    return(mean(segments.list[[length(segments.list) - number]]))
  }
  if (segment.value2(3) < limit) {
    ## delete the last 4 segments
    cutoff <- segments.points[length(segments.points) - 4]
  } else {
    if (segment.value2(2) < limit) {
      ## delete the last 3 segments
      cutoff <- segments.points[length(segments.points) - 3]
    } else {
      if (segment.value2(1) < limit) {
        ## delete the last 2 segments
        cutoff <- segments.points[length(segments.points) - 2]
      } else {
        if (segment.value2(0) < limit) {
          ## delete the last segment
          cutoff <- segments.points[length(segments.points) - 1]
        } else {
          ## no segments deleted, no end detected
          warning(paste("end not detected in file ", dp$footer$ID[1], sep = ""))
          cutoff <- nrow(dp$data)
        }
      }
    }
  }
  if (return.plot == TRUE) {
    segments.points2 <- suppressWarnings(changepoint::cpt.meanvar(data.in,
                                                                  method = "BinSeg", Q = 10,
                                                                  minseglen = 250, class = TRUE
                                                                  ))
    graphics::plot.new()
    ## save and restore par setting
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    ## plot
    graphics::par(mfrow = c(2, 1))
    graphics::plot(dp$data$amplitude,
                   type = "l",
                   xlab = paste0("Drilling depth [", dp$footer$xUnit[1], "]"),
                   ylab = paste0("Resistograph density [", dp$footer$yUnit[1], "]"),
                   main = paste0("Density profile ID: ", dp$footer$ID)
                   )
    graphics::abline(v = cutoff + 100, col = "red", lwd = 3, lty = 2)
    changepoint::plot(segments.points2,
                      xlab = paste0("Drilling depth [", dp$footer$xUnit[1], "]"),
                      ylab = paste0("Moving average of lagged differences"),
                      main = "Detected segments"
                      )
    graphics::abline(h = mean(data.in), col = "blue")
    graphics::abline(h = limit, col = "green")
    graphics::abline(v = cutoff, col = "red", lwd = 3, lty = 2)
    graphics::legend("topright",
                     legend = c("Segment mean", "Overall mean", "Cutoff limit"),
                     col = c("red", "blue", "green"), lty = 1, cex = 1
                     )
    p <- grDevices::recordPlot()
    return(p)
  } else {
    ## if end detected, add 100 to account for moving averages
    if (cutoff == nrow(dp$data)) {
      return(cutoff)
    } else {
      return(cutoff + 100)
    }
  }
}
