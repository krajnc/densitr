#' Automatically trim an individual density profile on both sides
#'
#' Calls dpdetect_s and dpdetect_e on a given dp object and
#' returns a trimmed dp object with the the row before the starting
#' point and after the ending removed. If \code{return.plot = TRUE},
#' it will return a plot displaying the dp object with detected
#' starting and ending point. If called with the option
#' \code{return.fail = FALSE} and \code{return.plot = FALSE}, the
#' returned object will also include information on whether both
#' cutoff points were detected. If starting/ending point not detected,
#' dp object is returned with no changes. When running on a list of
#' dp objects, use \code{dptriml}.
#'
#' @param dp An dp object, see \code{dpload}
#' @param return.plot Return a plot instead of dp object? If TRUE,
#'   returns a plot instead of a dp object. When return.fail = TRUE,
#'   it returns a list of three: dp object, start trimming success
#'   and end trimming success.
#' @param return.fail Should information on the success of trimming be
#'   included when returning a dp object?
#' @param silent Mute detection warnings, used when calling on list. A
#'   list of trimmed dp objects, a result of calling dtriml or
#'   dptriml_s on a dp list with return.fail = FALSE.
#' @return A trimmed dp object, with the beginning and ending
#'   removed, if they were detected. When return.plot = TRUE, it
#'   returns a plot displaying the process.
#' @seealso dptriml, dptrim_s, dptriml_s
#' @export
#' @examples
#' \donttest{
#' ## load a single dp file
#' dp <- dpload(system.file("extdata", "00010001.dpa", package = "densitr"))
#' ## trim the measurements
#' dp.trimmed <- dptrim(dp)
#' ## plot trimming
#' dptrim(dp, return.plot = TRUE)
#' }
dptrim <- function(dp, return.plot = FALSE, return.fail = FALSE, silent = FALSE) {
  if (!inherits(dp, "dp")) {
    stop("not a dp object")
  } # check if dp object
  if (silent == TRUE) {
    start <- suppressWarnings(dpdetect_s(dp))
    end <- suppressWarnings(dpdetect_e(dp))
  } else {
    start <- dpdetect_s(dp)
    end <- dpdetect_e(dp)
  }
  if (return.plot == TRUE) {
    graphics::plot(dp$data$amplitude,
                   type = "l",
                   xlab = paste0("Drilling depth [", dp$footer$xUnit[1], "]"),
                   ylab = paste0("Resistograph density [", dp$footer$yUnit[1], "]"),
                   main = paste0("Density profile ID: ", dp$footer$ID)
                   )
    graphics::abline(v = start, col = "red", lwd = 3, lty = 2)
    graphics::abline(v = end, col = "red", lwd = 3, lty = 2)
    p <- grDevices::recordPlot()
    return(p)
  } else {
    if (return.fail == FALSE) {
      dp$data <- dp$data[start:end, ]
      return(dp)
    } else {
      if (start == 1) {
        start.status <- "failed"
      } else {
        start.status <- "succeeded"
      }
      if (end == nrow(dp$data)) {
        end.status <- "failed"
      } else {
        end.status <- "succeeded"
      }
      dp$data <- dp$data[start:end, ]
      return(list(
        "dp" = dp,
        "detection.start" = start.status,
        "detection.end" = end.status
      ))
    }
  }
}

#' Automatically trim a list of density profiles on both sides
#'
#' Calls \code{dptrim} on a list of dp objects and return a list of
#' trimmed objects. If automatic detection fails, the dp objects are
#' not trimmed. Can be run in parallel on multiple cores, this speeds
#' up the trimming process significantly.
#'
#' @param dp.list A list of dp objects, see \code{dpload}
#' @param rreport Return an embedded report on automatic trim success,
#'   mandatory when using \code{correct_failures} to manually pick
#'   starting/ending.
#' @param cl Number of cores to run the trimming in parallel, passed through to \code{pbapply}.
#'   starting/ending.
#' @return A list of trimmed dp objects. When rreport = TRUE, it return a two-item list of (i) trimmed dp objects and (ii) trimming report data frame.
#' @seealso dptrim, dptrim_s, dptriml_s,
#' @export
#' @examples
#' \donttest{
#' ## load several dpa files
#' dp.list <- dpload(dpa.directory = system.file("extdata", package = "densitr"))
#' ## trim the measurements
#' dp.trimmed <- dptriml(dp.list)
#' }
dptriml <- function(dp.list, rreport = FALSE, cl = 1) {
  if (is.list(dp.list) && !inherits(dp.list[[1]], "dp")) {
    stop("not a dp list")
  }
  message("started trimming ", length(dp.list), " files")
  if (requireNamespace("pbapply", quietly = TRUE)) {
    dp.trimmed <- pbapply::pblapply(dp.list, dptrim, return.fail = T, silent = T, cl = cl)
  } else {
    dp.trimmed <- lapply(dp.list, dptrim, return.fail = T, silent = T)
  }
  data <- unlist(lapply(dp.trimmed, function(x) x[-(2:3)]), recursive = FALSE)
  names(data) <- names(dp.trimmed)
  ## there is a reason why people use dplyr bind_rows
  ## report1  <-  lapply(dp.trimmed, function(x) x[-(1)]) %>%
  ##   dplyr::bind_rows(., .id="ID")
  a <- lapply(dp.trimmed, function(x) x[-(1)])
  report <- do.call("rbind", lapply(a, as.data.frame))
  report$ID <- rownames(report)
  message(
    "########################################\ntrimming report: \nanalysed ",
    length(dp.list),
    " file(s) \nstart detection failed in: ",
    sum(report[, "detection.start"] == "failed"),
    " file(s)\nend detection failed in: ",
    sum(report[, "detection.end"] == "failed"), " file(s).\n",
    "########################################\n"
  )
  if (sum(report[, "detection.start"] == "failed") > 0) {
    message("start fail(s):\n", paste(report[report$detection.start %in% "failed", ]$ID, collapse = ", "), "\n")
  }
  if (sum(report[, "detection.end"] == "failed") > 0) {
    message("end fail(s):\n", paste(report[report$detection.end %in% "failed", ]$ID, collapse = ", "))
  }
  if (rreport == TRUE) {
    return(list("dp" = data, "report" = report))
  } else {
    return(data)
  }
}

#' Automatically trim an individual density profile on the starting side
#'
#' Calls dpdetect_s on a given dpa object and returns a trimmed
#' dpa object with the the row before the starting point removed. If
#' return.plot = TRUE, it will return a plot displaying the dp object
#' with detected starting point. If called with the option
#' return.fail = FALSE and return.plot = FALSE, the returned
#' object will also include information on whether starting cutoff
#' point was detected. If starting point not detected, dp object is
#' returned with no changes. When running on a list of dp objects,
#' use \code{dtriml_s}.
#'
#' @param dp An dp object, see \code{dpload}
#' @param return.plot Return a plot instead of dp object? If TRUE,
#'   returns a plot instead of a dp object. When return.fail = TRUE,
#'   it returns a list of three: dp object, start trimming success
#'   and end trimming success.
#' @param return.fail Should information on the success of trimming be
#'   included when returning a dp object?
#' @param silent Mute detection warnings, used when calling on list. A
#'   list of trimmed dp objects, a result of calling dptriml or
#'   dptriml_s on a dp list with rreport = FALSE.
#' @return A trimmed dp object, with the beginning and ending
#'   removed, if they were detected. When return.plot = TRUE, it
#'   returns a plot displaying the process.
#' @seealso dptrim, dptriml, dptriml_s
#' @export
#' @examples
#' \donttest{
#' ## load a single file
#' dp <- dpload(system.file("extdata", "00010001.dpa", package = "densitr"))
#' ## trim the measurement at start
#' dp.trimmed <- dptrim_s(dp)
#' ## plot trimming
#' dptrim_s(dp, return.plot = TRUE)
#' }
dptrim_s <- function(dp, return.plot = FALSE, return.fail = FALSE, silent = FALSE) {
  ## check if dp object
  if (!inherits(dp, "dp")) {
    stop("not a dp object")
  }

  if (silent == TRUE) {
    start <- suppressWarnings(dpdetect_s(dp))
  } else {
    start <- dpdetect_s(dp)
  }

  if (return.plot == TRUE) {
    graphics::plot(dp$data$amplitude,
                   type = "l",
                   xlab = paste0("Drilling depth [", dp$footer$xUnit[1], "]"),
                   ylab = paste0("Resistograph density [", dp$footer$yUnit[1], "]"),
                   main = paste0("Density profile ID: ", dp$footer$ID)
                   )
    graphics::abline(v = start, col = "red", lwd = 3, lty = 2)
    p <- grDevices::recordPlot()
    return(p)
  } else {
    if (return.fail == FALSE) {
      dp$data <- dp$data[start:nrow(dp$data), ]
      return(dp)
    } else {
      if (start == 1) {
        start.status <- "failed"
      } else {
        start.status <- "succeeded"
      }
      dp$data <- dp$data[start:nrow(dp$data), ]
      return(list(
        "dp" = dp,
        "detection.start" = start.status
      ))
    }
  }
}

#' Automatically trim a list of density profiles on the starting side
#'
#' Calls \code{dptrim} on a list of dp objects and returns a list of
#' trimmed objects. If automatic detection fails, the dp objects are
#' not trimmed. Can be run in parallel on multiple cores, this speeds
#' up the trimming process significantly. Only trims the starting
#' side, see \code{dptriml} for trimming both side simultaneously.
#'
#' @param dp.list A list of dp objects, see \code{dpload}
#' @param rreport Return an embedded report on automatic trim success,
#'   mandatory when using \code{correct_failures} to manually pick
#'   starting/ending.
#' @param cl Number of cores to run the trimming in parallel, passed
#'   through to \code{pbapply}. starting/ending.
#' @return A list of trimmed dp objects. When rreport = TRUE, it
#'   return a two-item list of (i) trimmed dp objects and (ii)
#'   trimming report data frame.
#' @seealso dptrim, dptrim_s, dptriml_s,
#' @export
#' @examples
#' \donttest{
#' ## load several dp files
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densitr"))
#' ## trim the measurements
#' dp.trimmed <- dptrim_sl(dp.list)
#' }
dptriml_s <- function(dp.list, rreport = FALSE, cl = 1) {
  if (is.list(dp.list) && !inherits(dp.list[[1]], "dp")) {
    stop("not a dp list")
  }
  message("started start trimming ", length(dp.list), " files")
  if (requireNamespace("pbapply", quietly = TRUE)) {
    dp.trimmed <- pbapply::pblapply(dp.list, dptrim_s, return.fail = T, silent = T, cl = cl)
  } else {
    dp.trimmed <- lapply(dp.list, dptrim_s, return.fail = T, silent = T)
  }
  data <- unlist(lapply(dp.trimmed, function(x) x[-2]), recursive = FALSE)
  names(data) <- names(dp.trimmed)
  ## there is a reason why people use dplyr bind_rows
  ## report1  <-  lapply(dp.trimmed, function(x) x[-(1)]) %>%
  ##   dplyr::bind_rows(., .id="IDa  <- lapply(dp.trimmed, function(x) x[-(1)])
  a <- lapply(dp.trimmed, function(x) x[-(1)])
  report <- do.call("rbind", lapply(a, as.data.frame))
  report$ID <- rownames(report)
  rownames(report) <- NULL
  message(
    "########################################\ntrimming report: \nanalysed ",
    length(dp.list),
    " files \nstart detection failed in: ",
    sum(report[, "detection.start"] == "failed"),
    " file(s).\n",
    "########################################\n"
  )
  if (sum(report[, "detection.start"] == "failed") > 0) {
    message("start fail(s):\n", paste(report[report$detection.start %in% "failed", ]$ID, collapse = ", "), "\n")
  }
  if (rreport == TRUE) {
    return(list("dp" = data, "report" = report))
  } else {
    return(data)
  }
}
