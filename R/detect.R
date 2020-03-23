#' Detect measurement starting point automatically
#'
#' Loads either a single .dpa file or a list of .dpa files. If
#' dpa.file is specified, it will load a single file. If dpa.directory
#' is specified, it will search for all dpa files in that directory
#' (recursively in all subfolders, can be turned off) and return a
#' list of dpa files. It will use pbapply to display progress, if
#' loading a directory.
#'
#' @param dpa.file A path to a single file, including file name.
#' @param dpa.directory A directory with .dpa files.
#' @param recursive Also look for dpa files in subfolders?
#' @param name Either \code{c("file", "folder")}, used for naming of
#'   list items. If "file", only file name withouth the complete path
#'   will be used for naming ("00050060"). If "folder", the complete
#'   path along with file name will be used to name the dpa objects
#'   ("data/0005/00/00050060"). *.dpa ending is removed from the name
#'   in both cases.
#' @return A \code{dpa} object or a list of \code{dpa} objects.
#' @seealso read_dpa.
#' @export
#' @examples
#' ## load a single file
#' load_dpa("data/test.dpa")
#' dpa <- load_dpa("data/0005/00/00050060.dpa")
#' ## load all files in directory
#' dpa.list <- load_dpa(dpa.directory = "data")
dpa_detect_start <- function(dpa, cutoff.sd = 1, return.plot = FALSE){
  ## check if dpa object
  if (!inherits(dpa,"dpa")) {stop("not a dpa object")}
  ## get a rolling mean of diff lags
  data.in <-
    loess(dpa$data$amplitude ~ dpa$data$position, span=0.1) %>%
    predict(.) %>%
    diff(.) %>%
    zoo::rollmeanr(.,100)
  ## set limits and find segments
  limit <- abs(mean(data.in) + (cutoff.sd * sd(data.in)))
  segments.points <- suppressWarnings(changepoint::cpt.meanvar(data.in,
                                                               method="BinSeg", Q=10,
                                                               minseglen=250,class=FALSE))
  segments.list <- splitAt(data.in,segments.points)
  segments.list[length(segments.list)] <- NULL  #remove the last item in a list
  segment.value <- function(number){return(abs(mean(segments.list[[number]])))}
  ## check the first four segments, if they are outside of the set,
  ## limit and return the end positions of those segments
  if (segment.value(4) < limit) {
    if (segment.value(3) < limit) {
      if (segment.value(2) < limit) {
        if (segment.value(1) < limit) {
          ## no segments found outside the limit in the first 4
          ## segments
          warning(paste("start not detected in measurement ",dpa$footer$ID[1],sep="" ))
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
                                                                  method="BinSeg", Q=10,
                                                                  minseglen=250,class=TRUE))
    par(mfrow=c(2,1))
    plot(dpa$data$amplitude, type = "l",
         xlab = paste0("Drilling depth [", dpa$footer$xUnit, "]"),
         ylab= paste0("Resistograph density [", dpa$footer$yUnit, "]"),
         main = paste0("Resistograph data: file ",dpa$footer$ID))
    abline(v=cutoff, col="red",lwd=3, lty=2)
    ## [1:length(dpa$data$amplitude)/2]
    changepoint::plot(segments.points2,
                      xlab = paste0("Drilling depth [", dpa$footer$xUnit, "]"),
         ylab= paste0("Moving average of lagged differences"),
         main="Detected segments, overall mean and cutoff limits")
    abline(h=mean(data.in), col="blue")
    abline(h=limit, col="green")
    abline(h=-limit, col="green")
    abline(v=cutoff, col="red", lwd=3, lty=2)
    p <- recordPlot()
    return(p)
  } else {
    return(cutoff) # add 100 to account for rolling mean right centred
  }
}

dpa_detect_end <- function(dpa, cutoff.sd = 1, return.plot = FALSE){
  ## check if dpa object
  if (!inherits(dpa,"dpa"))  {stop("not a dpa object")}
  ## get a rolling mean of diff lags
  data.in <-
    loess(dpa$data$amplitude ~ dpa$data$position, span=0.1) %>%
    predict(.) %>%
    diff(.) %>%
    zoo::rollmeanr(.,100)
  ## get limits and get segments
  limit <- mean(data.in) - (cutoff.sd * sd(data.in))
  segments.points <- suppressWarnings(changepoint::cpt.meanvar(data.in,
                                                               method="BinSeg", Q=10,
                                                               minseglen=250,class=FALSE))
  segments.list <- splitAt(data.in,segments.points)
  segments.list[length(segments.list)] <- NULL  #remove the last item in a list

  segment.value2 <- function(number){return(mean(segments.list[[length(segments.list)-number]]))}

  if (segment.value2(3) < limit) {
    ## delete the last 4 segments
    cutoff <- segments.points[length(segments.points)-4]
  } else {
    if (segment.value2(2) < limit) {
      ## delete the last 3 segments
      cutoff <- segments.points[length(segments.points)-3]
    } else {
      if (segment.value2(1) < limit) {
        ## delete the last 2 segments
        cutoff <- segments.points[length(segments.points)-2]
      } else {
        if (segment.value2(0) < limit) {
          ## delete the last segment
          cutoff <- segments.points[length(segments.points)-1]
        } else {
          ## no segments deleted, no end detected
          warning(paste("end not detected in file ",dpa$footer$ID[1],sep="" ))
          cutoff <- nrow(dpa$data)
        }
      }
    }
  }

  if (return.plot == TRUE) {
    segments.points2 <- suppressWarnings(changepoint::cpt.meanvar(data.in,
                                                                  method="BinSeg", Q=10,
                                                                  minseglen=250,class=TRUE))
    par(mfrow=c(2,1))
    plot(dpa$data$amplitude, type = "l",
         xlab = paste0("Drilling depth [", dpa$footer$xUnit[1], "]"),
         ylab= paste0("Resistograph density [", dpa$footer$yUnit[1], "]"),
         main = paste0("Resistograph data: file ",dpa$footer$ID))
    abline(v=cutoff + 100, col="red", lwd=3, lty=2)
    changepoint::plot(segments.points2,
                      xlab = paste0("Drilling depth [", dpa$footer$xUnit[1], "]"),
         ylab= paste0("Moving average of lagged differences"),
         main="Detected segments, overall mean and cutoff limits")
    abline(h=mean(data.in), col="blue")
    abline(h=limit, col="green")
    abline(h=-limit, col="green")
    abline(v=cutoff, col="red", lwd=3, lty=2)
    p <- recordPlot()
    return(p)
  } else {
    ## if end detected, add 100 to account for moving averages
    if (cutoff == nrow(dpa$data)) {
      return(cutoff)
    } else {
      return(cutoff + 100)
    }
  }
}
