#' Remove automatic trim failures from a list of trimmed dp objects
#' and return only non-failed trimmed objects
#'
#' Returns a dp list of trimmed dp objects without the failed trim
#' objects. Trimmed dp list should be a result of either calling
#' dtriml on a list of dp objects or calling dtriml_s to remove the
#' starting portions of the measurement. Both functions should be
#' called with the option "return.fail = FALSE", which embeds a
#' trimming report when returning the list of trimmed dp objects.
#'
#' @param dp.trimmed A list of trimmed dp objects, a result of
#'   calling dtriml or dtriml_s on a dp list with
#'   "return.fail = FALSE".
#' @return A dp list of trimmed objects with failures removed.
#' @seealso dptriml, dptriml_s
#' @export
#' @examples
#' \donttest{
#' ## load several dp files
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densitr"))
#' ## trim the measurements
#' dp.trimmed <- dptriml(dp.list, rreport = TRUE)
#' ## remove trimming failures
#' dp.nofailures <- remove_trim_failures(dp.trimmed)
#' }
remove_trim_failures <- function(dp.trimmed) {
  if (names(dp.trimmed[2]) != "report") {
    stop("not report attached, trim again with rreport = TRUE")
  }
  failures.start <- dp.trimmed$report[dp.trimmed$report$detection.start %in% "failed", ]$ID
  dps <- dp.trimmed$dp
  dp.subsetted <- dps[!(names(dps) %in% failures.start)]
  if ("detection.end" %in% colnames(dp.trimmed$report)) {
    failures.end <- dp.trimmed$report[dp.trimmed$report$detection.end %in% "failed", ]$ID
    dp.subsetted <- dps[!(names(dps) %in% failures.end)]
  }
  return(dp.subsetted)
}

#' Remove automatic trim failures from a list of trimmed dp objects
#' and return ONLY failures
#'
#' An inverse of remove_trim_failures, return a list of failed trimming
#' objects from a trimmed dp list. Trimmed dp list should be a
#' result of either calling dtriml on a list of dp objects or calling
#' dtriml_s to remove the starting portions of the measurement. Both
#' functions should be called with the option "rreport = FALSE", which
#' embeds a trimming report when returning the list of trimmed dp
#' objects. If no failures found, it will return a list of trimmed
#' profiles without the report attached.
#'
#' @param dp.trimmed A list of trimmed dp objects, a result of
#'   calling dptriml or dptriml_s on a dp list with
#'   "rreport = FALSE".
#' @return Two lists, one with start failures and one with end failures.
#' @seealso dptriml, dptriml_s
#' @export
#' @examples
#' \donttest{
#' ## load several dp files
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densitr"))
#' ## trim the measurements
#' dp.trimmed <- dptriml(dp.list, rreport = TRUE)
#' ## separate trimming failures
#' dp.nofailures <- separate_trim_failures(dp.trimmed)
#' }
separate_trim_failures <- function(dp.trimmed) {
  if (names(dp.trimmed[2]) != "report") {
    stop("not report attached, trim again with rreport = TRUE")
  }
  dps <- dp.trimmed$dp
  failures.start <- dp.trimmed$report[dp.trimmed$report$detection.start %in% "failed", ]$ID
  if (length(failures.start) > 0) {
    dp.start <- dps[(names(dps) %in% failures.start)]
  } else {
    dp.start <- list()
  }
  if ("detection.end" %in% colnames(dp.trimmed$report)) {
    failures.end <- dp.trimmed$report[dp.trimmed$report$detection.end %in% "failed", ]$ID
    dp.end <- dps[(names(dps) %in% failures.end)]
  } else {
    dp.end <- list()
  }
  return(list("failures.start" = dp.start, "failures.end" = dp.end))
}

#' Manually select a starting or ending location of a density profile
#'
#' Follow-up to automatic trim functions or to be used manually, will
#' display a plot with the density profiles. Most commonly used in
#' automatic failure corrections by the function correct_failures. Use
#' your mouse to select starting/ending point on the plot, your
#' selection will then be displayed on the plot. Either returns a
#' numeric value or NA in case of errors. There are two special cases:
#' when encountering an error with a label = " - PICK START" it will
#' return the starting position, and with label " - PICK STOP" it will
#' return the ending position. These labels are used when correcting
#' several density profiles at once using \code{correct_failures}.
#' This function uses \code{graphics::locator}, which only works on
#' screen devices X11, windows and quartz. It will not work on other
#' devices, returning NA.
#'
#' @param failure A dp object, usually see dpload.
#' @param label Optional label to be displayed on the plot after the
#'   file
#' @return The x position selected on the graph, row number in the dp$data data frame.
#' @seealso dptrim, dptriml
#' @export
#' @examples
#' ## load a single file
#' dp <- dpload(system.file("extdata", "00010001.dpa", package = "densitr"))
#' ## get a starting point on the plot
#' \donttest{
#' manual_trim_detect(dp)
#' }
manual_trim_detect <- function(failure, label = "") {
  x <- NULL
  graphics::plot(failure$data$amplitude,
    type = "l",
    xlab = paste0("Drilling depth [", x$footer$xUnit, "]"),
    ylab = paste0("Resistograph density [", x$footer$yUnit, "]"),
    main = paste0("Density profile ID: ", failure$footer$ID, " ", label)
  )
  message("\n[click on graph to pick a vertical line]")
  cutoff <- tryCatch(
    {
      clicked <- graphics::locator(1)
      xl <- clicked$x
      if ((xl < 0) | (xl > nrow(failure$data))) {
        stop()
      }
      xl
    },
    error = function(e) {
      if (label == " - PICK START") {
        1
      } else if (label == " - PICK STOP") {
        nrow(failure$data)
      } else {
        NA
      }
    }
  )
  graphics::abline(v = cutoff, col = "red", lwd = 3, lty = 2)
  if (is.numeric(cutoff)) {
    return(round(cutoff, 0))
  } else {
    return(cutoff)
  }
}

#' Manually correct failures after automatic trim detection
#'
#' This function will take a list of trimmed dp objects (a result of
#' dptriml or dptriml_s function) and interactively ask the user to
#' assign starting/ending points manually. Follow-up to automatic
#' trim functions or to be used manually, will display a plot with the
#' density profiles for each failure in trim detection sequentially.
#' The plot title will display whether you are selection start or end
#' positions. Use your mouse to select starting/ending point on the
#' plot, your selection will then be displayed on the plot. Will
#' return a complete list, both with the non-failed automatically
#' trimmed dp objects and those corrected manually. The automatic
#' trim functions should be called with the option
#' "rreport = TRUE", which embeds a trimming report when
#' returning the list of trimmed dp objects.
#'
#' @param dp.trimmed A list of dp objects, trimmed, with the report
#'   embedded ("rreport = TRUE").
#' @return A list of trimmed profiles, including both automatic and
#'   manual trimming.
#' @seealso dptrim, dptriml, manual_trim_detect
#' @export
#' @examples
#' ## load all dp files
#' \donttest{
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densitr"))
#' ## trim the list
#' dp.trimmed <- dptriml(dp.list, rreport = TRUE)
#' ## manually correct the failures
#' dp.corrected <- correct_failures(dp.trimmed)
#' }
correct_failures <- function(dp.trimmed) {
  failures <- separate_trim_failures(dp.trimmed)
  message(
    "\nfound:\n",
    length(failures$failures.start),
    " start failures \n",
    length(failures$failures.end),
    " end failures"
  )
  if (length(failures$failures.start) > 0) {
    cutoffs.start <-
      lapply(failures$failures.start, manual_trim_detect, label = " - PICK START")
    for (i in 1:length(failures$failures.start)) {
      start <- unlist(cutoffs.start[names(failures$failures.start[i])], use.names = F)
      end <- nrow(dp.trimmed$dp[names(failures$failures.start[i])][[1]]$data)
      dp.trimmed$dp[names(failures$failures.start[i])][[1]]$data <- utils::tail(dp.trimmed$dp[names(failures$failures.start[i])][[1]]$data, -(start - 1))
    }
  }
  message("\nstart corrections done, starting end corrections\n")
  if (length(failures$failures.end) > 0) {
    cutoffs.end <- lapply(failures$failures.end, manual_trim_detect, label = " - PICK STOP")
    for (i in 1:length(failures$failures.end)) {
      end.old <-
        nrow(dp.trimmed$dp[names(failures$failures.end[i])][[1]]$data)

      end.new <-
        unlist(cutoffs.end[names(failures$failures.end[i])], use.names = F)

      diff <- end.old - end.new
      ## remove the last X values
      dp.trimmed$dp[names(failures$failures.end[i])][[1]]$data <-
        utils::head(dp.trimmed$dp[names(failures$failures.end[i])][[1]]$data, -diff)
    }
  }
  message("\nall corrections done\n")
  return(dp.trimmed$dp)
}

#' Manually trim a list of density profiles
#'
#' This function will take a list of dp objects and interactively ask
#' the user to assign starting/ending points manually for all density
#' profiles in sequentially. Used as alternative to automatic trim
#' functions. The plot title will display whether you are selecting
#' start or end position. Use your mouse to select starting/ending
#' points on the plot.
#'
#' @param dp.list A list of dp objects.
#' @return A list of trimmed density profiles.
#' @seealso dptrim, dptriml, manual_trim_detect
#' @export
#' @examples
#' ## load all dp files
#' \donttest{
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densitr"))
#' ## manually trim the list
#' dp.trimmed <- trim_manually(dp.list)
#' }
trim_manually <- function(dp.list) {
  if (is.list(dp.list) && !inherits(dp.list[[1]], "dp")) {
    stop("not a dp list")
  }
  cutoffs.start <-
    lapply(dp.list, manual_trim_detect, label = " - PICK START")
  message("\nstart corrections done, starting end corrections\n")
  cutoffs.end <- lapply(dp.list, manual_trim_detect, label = " - PICK STOP")
  for (i in 1:length(dp.list)) {
    start <- unlist(cutoffs.start[[i]], use.names = F)
    end.old <-
      nrow(dp.list[[i]]$data)
    end.new <-
      unlist(cutoffs.end[[i]], use.names = F)

    diff <- end.old - end.new

    ## remove the last X values
    dp.list[[i]]$data <-
      utils::head(dp.list[[i]]$data, -diff)

    dp.list[[i]]$data <-
      utils::tail(dp.list[[i]]$data, -(start - 1))
  }
  message("\nall corrections done\n")
  return(dp.list)
}
