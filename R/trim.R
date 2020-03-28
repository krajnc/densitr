#' Automatically trim an individual density profile on both sides
#'
#' Calls dpa_detect_start and dpa_detect_end on a given dpa object and
#' returns a trimmed dpa object with the the row before the starting
#' point and after the ending removed. If \code{return.plot = TRUE},
#' it will return a plot displaying the dpa object with detected
#' starting and ending point. If called with the option
#' \code{return.fail = FALSE} and \code{return.plot = FALSE}, the
#' returned object will also include information on whether both
#' cutoff points were detected. If starting/ending point not detected,
#' dpa object is returned with no changes. When running on a list of
#' dpa objects, use \code{dtriml}.
#'
#' @param dpa An dpa object, see \code{load_dpa}
#' @param return.plot Return a plot instead of dpa object? If TRUE,
#'   returns a plot instead of a dpa object. When return.fail = TRUE,
#'   it returns a list of three: dpa object, start trimming success
#'   and end trimming success.
#' @param return.fail Should information on the success of trimming be
#'   included when returning a dpa object?
#' @param silent Mute detection warnings, used when calling on list. A
#'   list of trimmed dpa objects, a result of calling dtriml or
#'   dtriml_s on a dpa list with return.fail = FALSE.
#' @return A trimmed dpa object, with the beginning and ending
#'   removed, if they were detected. When return.plot = TRUE, it
#'   returns a plot displaying the process.
#' @seealso dtriml, dtrim_s, dtriml_s
#' @export
#' @examples
#' \dontrun{
#' ## load a single dpa file
#' dpa  <- load_dpa(system.file("extdata", "00010001.dpa", package = "densiter"))
#' ## trim the measurements
#' dpa.trimmed <- dtrim(dpa)
#' ## plot trimming
#' dtrim(dpa, return.plot = TRUE)
#' }
dtrim <- function(dpa, return.plot = FALSE, return.fail = FALSE, silent = FALSE){
  if (!inherits(dpa,"dpa"))  {stop("not a dpa object")}   # check if dpa object
  if (silent == TRUE) {
    start <- suppressWarnings(dpa_detect_start(dpa))
    end <-  suppressWarnings(dpa_detect_end(dpa))
  } else {
    start <- dpa_detect_start(dpa)
    end <-  dpa_detect_end(dpa)
  }
  if (return.plot == TRUE) {
    graphics::par(mfrow=c(1,1))
    graphics::plot(dpa$data$amplitude, type = "l",
                   xlab = paste0("Drilling depth [", dpa$footer$xUnit[1], "]"),
         ylab = paste0("Resistograph density [", dpa$footer$yUnit[1], "]"),
         main = paste0("Density profile ID: ",dpa$footer$ID))
    graphics::abline(v=start, col="red", lwd=3, lty=2)
    graphics::abline(v=end, col="red", lwd=3, lty=2)
    p <- grDevices::recordPlot()
    return(p)
  } else {
    if (return.fail == FALSE){
      dpa$data <- dpa$data[start:end, ]
      return(dpa)
    } else {
      if (start == 1){
        start.status  <- "failed"
      } else {
        start.status <- "succeeded"
      }
      if (end == nrow(dpa$data)){
        end.status <- "failed"
      } else {
        end.status <- "succeeded"
      }
      dpa$data <- dpa$data[start:end, ]
      return(list("dpa" = dpa,
                  "detection.start" = start.status,
                  "detection.end" = end.status))
    }
  }
}

#' Automatically trim a list of density profiles on both sides
#'
#' Calls \code{dtrim} on a list of dpa objects and return a list of
#' trimmed objects. If automatic detection fails, the dpa objects are
#' not trimmed. Can be run in parallel on multiple cores, this speeds
#' up the trimming process significantly.
#'
#' @param dpa.list A list of dpa objects, see \code{load_dpa}
#' @param rreport Return an embedded report on automatic trim success,
#'   mandatory when using \code{correct_failures} to manually pick
#'   starting/ending.
#' @param cl Number of cores to run the trimming in parallel, passed through to \code{pbapply}.
#'   starting/ending.
#' @return A list of trimmed dpa objects. When rreport = TRUE, it return a two-item list of (i) trimmed dpa objects and (ii) trimming report data frame.
#' @seealso dtrim, dtrim_s, dtriml_s,
#' @export
#' @examples
#'\dontrun{
#' ## load several dpa files
#' dpa.list <- load_dpa(dpa.directory = system.file("extdata", package = "densiter"))
#' ## trim the measurements
#' dpa.trimmed <- dtriml(dpa.list)
#' }
dtriml  <- function(dpa.list, rreport = FALSE, cl = 1) {
  message("started trimming ", length(dpa.list), " files")
  if (requireNamespace("pbapply", quietly = TRUE)) {
    dpa.trimmed  <- pbapply::pblapply(dpa.list,  dtrim, return.fail = T, silent = T, cl = cl)
  } else {
    dpa.trimmed  <- lapply(dpa.list,  dtrim, return.fail = T, silent = T)
  }
  data  <- unlist(lapply(dpa.trimmed, function(x) x[-(2:3)]),recursive=FALSE)
  names(data)  <- names(dpa.trimmed)
  ## there is a reason why people use dplyr bind_rows
  ## report1  <-  lapply(dpa.trimmed, function(x) x[-(1)]) %>%
  ##   dplyr::bind_rows(., .id="ID")
  a  <- lapply(dpa.trimmed, function(x) x[-(1)])
  report  <- do.call("rbind", lapply(a, as.data.frame))
  report$ID  <- rownames(report)
  message("########################################\ntrimming report: \nanalysed ",
          length(dpa.list),
          " file(s) \nstart detection failed in: ",
          sum(report[,"detection.start"] == "failed"),
          " file(s)\nend detection failed in: ",
          sum(report[,"detection.end"] == "failed"), " file(s).\n",
          "########################################\n")
  if (sum(report[,"detection.start"] == "failed") > 0){
    message("start fail(s):\n", paste(report[report$detection.start %in% "failed",]$ID, collapse = ", "), "\n")
  }
  if (sum(report[,"detection.end"] == "failed") > 0){
    message("end fail(s):\n", paste(report[report$detection.end %in% "failed",]$ID, collapse = ", "))
  }
  if (rreport == TRUE){
    return(list("dpa" = data, "report" = report))
  } else {
    return(data)
  }
}

#' Automatically trim an individual density profile on the starting side
#'
#' Calls dpa_detect_start on a given dpa object and returns a trimmed
#' dpa object with the the row before the starting point removed. If
#' return.plot = TRUE, it will return a plot displaying the dpa object
#' with detected starting point. If called with the option
#' return.fail = FALSE and return.plot = FALSE, the returned
#' object will also include information on whether starting cutoff
#' point was detected. If starting point not detected, dpa object is
#' returned with no changes. When running on a list of dpa objects,
#' use \code{dtriml_s}.
#'
#' @param dpa An dpa object, see \code{load_dpa}
#' @param return.plot Return a plot instead of dpa object? If TRUE,
#'   returns a plot instead of a dpa object. When return.fail = TRUE,
#'   it returns a list of three: dpa object, start trimming success
#'   and end trimming success.
#' @param return.fail Should information on the success of trimming be
#'   included when returning a dpa object?
#' @param silent Mute detection warnings, used when calling on list. A
#'   list of trimmed dpa objects, a result of calling dtriml or
#'   dtriml_s on a dpa list with return.fail = FALSE.
#' @return A trimmed dpa object, with the beginning and ending
#'   removed, if they were detected. When return.plot = TRUE, it
#'   returns a plot displaying the process.
#' @seealso dtrim, dtriml, dtrim_s
#' @export
#' @examples
#' \dontrun{
#' ## load a single file
#' dpa  <- load_dpa(system.file("extdata", "00010001.dpa", package = "densiter"))
#' ## trim the measurement at start
#' dpa.trimmed <- dtrim_s(dpa)
#' ## plot trimming
#' dtrim_s(dpa, return.plot = TRUE)
#' }
dtrim_s <- function(dpa, return.plot = FALSE, return.fail = FALSE, silent = FALSE){
  ## check if dpa object
  if (!inherits(dpa,"dpa")) {stop("not a dpa object")}

  if (silent == TRUE) {
    start <- suppressWarnings(dpa_detect_start(dpa))
  } else {
    start <- dpa_detect_start(dpa)
  }

  if (return.plot == TRUE) {
    graphics::par(mfrow=c(1,1))
    graphics::plot(dpa$data$amplitude, type = "l",
                   xlab = paste0("Drilling depth [", dpa$footer$xUnit[1], "]"),
         ylab = paste0("Resistograph density [", dpa$footer$yUnit[1], "]"),
         main = paste0("Density profile ID: ",dpa$footer$ID))
    graphics::abline(v=start, col="red", lwd=3, lty=2)
    p <- grDevices::recordPlot()
    return(p)
  } else {
    if (return.fail == FALSE){
      dpa$data <- dpa$data[start:nrow(dpa$data), ]
      return(dpa)
    } else {
      if (start == 1){
        start.status  <- "failed"
      } else {
        start.status <- "succeeded"
      }
      dpa$data <- dpa$data[start:nrow(dpa$data), ]
      return(list("dpa" = dpa,
                  "detection.start" = start.status))
    }
  }
}

#' Automatically trim a list of density profiles on the starting side
#'
#' Calls \code{dtrim} on a list of dpa objects and returns a list of
#' trimmed objects. If automatic detection fails, the dpa objects are
#' not trimmed. Can be run in parallel on multiple cores, this speeds
#' up the trimming process significantly. Only trims the starting
#' side, see \code{dtriml} for trimming both side simultaneously.
#'
#' @param dpa.list A list of dpa objects, see \code{load_dpa}
#' @param rreport Return an embedded report on automatic trim success,
#'   mandatory when using \code{correct_failures} to manually pick
#'   starting/ending.
#' @param cl Number of cores to run the trimming in parallel, passed
#'   through to \code{pbapply}. starting/ending.
#' @return A list of trimmed dpa objects. When rreport = TRUE, it
#'   return a two-item list of (i) trimmed dpa objects and (ii)
#'   trimming report data frame.
#' @seealso dtrim, dtrim_s, dtriml_s,
#' @export
#' @examples
#' \dontrun{
#' ## load several dpa files
#' dpa.list <- load_dpa(dpa.directory = system.file("extdata", package = "densiter"))
#' ## trim the measurements
#' dpa.trimmed <- dtrim_sl(dpa.list)
#' }
dtrim_sl  <- function(dpa.list, rreport = FALSE, cl = 1) {
  message("started start trimming ", length(dpa.list), " files")
  if (requireNamespace("pbapply", quietly = TRUE)) {
    dpa.trimmed  <- pbapply::pblapply(dpa.list,  dtrim_s, return.fail = T, silent = T, cl = cl)
  } else {
    dpa.trimmed  <- lapply(dpa.list,  dtrim_s, return.fail = T, silent = T)
  }
  data  <- unlist(lapply(dpa.trimmed, function(x) x[-2]),recursive=FALSE)
  names(data)  <- names(dpa.trimmed)
  ## there is a reason why people use dplyr bind_rows
  ## report1  <-  lapply(dpa.trimmed, function(x) x[-(1)]) %>%
  ##   dplyr::bind_rows(., .id="IDa  <- lapply(dpa.trimmed, function(x) x[-(1)])
  a  <- lapply(dpa.trimmed, function(x) x[-(1)])
  report  <- do.call("rbind", lapply(a, as.data.frame))
  report$ID  <- rownames(report)
  rownames(report) <- NULL
  message("########################################\ntrimming report: \nanalysed ",
          length(dpa.list),
          " files \nstart detection failed in: ",
          sum(report[,"detection.start"] == "failed"),
          " file(s).\n",
          "########################################\n")
  if (sum(report[,"detection.start"] == "failed") > 0){
    message("start fail(s):\n", paste(report[report$detection.start %in% "failed",]$ID, collapse = ", "), "\n")
  }
  if (rreport == TRUE){
    return(list("dpa" = data, "report" = report))
  } else {
    return(data)
  }
}
