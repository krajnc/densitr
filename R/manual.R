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
#' \dontrun{
#' ## load several dp files
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densiter"))
#' ## trim the measurements
#' dp.trimmed <- dptriml(dp.list, rreport = TRUE)
#' ## remove trimming failures
#' dp.nofailures <- remove_trim_failures(dp.trimmed)
#' }
remove_trim_failures  <- function(dp.trimmed) {
  if (names(dp.trimmed[2]) != "report") {
    stop("not report attached, trim again with rreport = TRUE")
  }
  failures.start <- dp.trimmed$report[dp.trimmed$report$detection.start %in% "failed",]$ID
  dps <- dp.trimmed$dp
  dp.subsetted <- dps[!(names(dps) %in% failures.start)]
  if("detection.end" %in% colnames(dp.trimmed$report))
  {
    failures.end <- dp.trimmed$report[dp.trimmed$report$detection.end %in% "failed",]$ID
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
#' \dontrun{
#' ## load several dp files
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densiter"))
#' ## trim the measurements
#' dp.trimmed <- dptriml(dp.list, rreport = TRUE)
#' ## separate trimming failures
#' dp.nofailures <- separate_trim_failures(dp.trimmed)
#' }
separate_trim_failures  <- function(dp.trimmed) {
  if (names(dp.trimmed[2]) != "report") {
    stop("not report attached, trim again with rreport = TRUE")
  }
  dps <- dp.trimmed$dp
  failures.start <- dp.trimmed$report[dp.trimmed$report$detection.start %in% "failed",]$ID
  if (length(failures.start) > 0) {
    dp.start <- dps[(names(dps) %in% failures.start)]
  } else {
    dp.start <- list()
  }
  if("detection.end" %in% colnames(dp.trimmed$report)) {
    failures.end <- dp.trimmed$report[dp.trimmed$report$detection.end %in% "failed",]$ID
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
#' selection will then be displayed on the plot. Use keys y/n to
#' confirm selection, pressing n will restart the selection process.
#' The selection can be repeated once after pressing n, next n will
#' stop the process and it has to be restarted manually. This function
#' uses \code{grDevices::getGraphicsEvent} and
#' \code{graphics::locator}, which only works on screen devices X11,
#' windows and quartz. It will not work on other devices.
#'
#' @param failure A dp object, usually see dpload.
#' @param label Optional label to be displayed on the plot after the
#'   file
#' @return The x position selected on the graph, row number in the dpa$data data frame.
#' @seealso dptrim, dptriml
#' @export
#' @examples
#' ## load a single file
#' dp  <- dpload(system.file("extdata", "00010001.dpa", package = "densiter"))
#' ## get a starting point on the plot
#' \dontrun{
#' manual_trim_detect(dp)
#' }
manual_trim_detect <- function(failure, label = "") {
  graphics::plot(failure$data$amplitude, type = "l",
                 xlab = paste0("Drilling depth"),
                 ylab= paste0("Resistograph density"),
                 main = paste0("Density profile ID: ",failure$footer$ID," ",label))
  message("\n[click on graph then pick a vertical line, then confirm]\n")
  click.loc <- graphics::locator(1)
  ## if (click.loc == "") {
  ##   stop("point selection not supported on your graphics device, see densiter::manual_trim_detect documentation")}
  graphics::abline(v=click.loc$x, col="red",lwd=3, lty=2)
  readkeygraph(paste0("confirm selection, y or n?"))
  if (keyPressed == "y"){
    cutoff <- click.loc$x
    grDevices::dev.off()
  } else {
    grDevices::dev.off()
    graphics::plot(failure$data$amplitude, type = "l",
                   xlab = paste0("Drilling depth"),
                   ylab= paste0("Resistograph density"),
                   main = paste0("Resistograph data: file ",failure$footer$ID))
    click.loc <- graphics::locator(1)
    graphics::abline(v=click.loc$x, col="red",lwd=3, lty=2)
    readkeygraph(paste0("confirm selection, y or n?"))
    if (keyPressed == "y"){
      cutoff <- click.loc$x
      grDevices::dev.off()
    } else {
      cutoff  <- FALSE
      grDevices::dev.off()
    }
  }
  if (is.numeric(cutoff)) {
    return(round(cutoff,0))
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
#' plot, your selection will then be displayed on the plot. Use keys
#' y/n to confirm selection, pressing n will restart the selection
#' process. The selection can be repeated once after pressing n, next
#' n will stop the process and it has to be restarted manually. Will
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
#' \dontrun{
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densiter"))
#' ## trim the list
#' dp.trimmed <- dptriml(dp.list, rreport = TRUE)
#' ## manually correct the failures
#' dp.corrected <- correct_failures(dp.trimmed)
#' }
correct_failures  <- function(dp.trimmed) {
  failures  <-  separate_trim_failures(dp.trimmed)
  message("\nfound:\n",
          length(failures$failures.start),
          " start failures \n",
          length(failures$failures.end),
          " end failures")
  if (length(failures$failures.start) > 0) {
    cutoffs.start <-
      lapply(failures$failures.start, manual_trim_detect, label = " - PICK START")
    for (i in 1:length(failures$failures.start)){
      start <- unlist(cutoffs.start[names(failures$failures.start[i])],use.names = F)
      end  <- nrow(dp.trimmed$dp[names(failures$failures.start[i])][[1]]$data)
      dp.trimmed$dp[names(failures$failures.start[i])][[1]]$data  <- utils::tail(dp.trimmed$dp[names(failures$failures.start[i])][[1]]$data, -(start - 1))
    }
  }
  message("\nstart corrections done, starting end corrections\n")
  if (length(failures$failures.end) > 0) {
    cutoffs.end <- lapply(failures$failures.end, manual_trim_detect, label = " - PICK STOP")
    for (i in 1:length(failures$failures.end)){
      end.old  <-
        nrow(dp.trimmed$dp[names(failures$failures.end[i])][[1]]$data)

      end.new <-
        unlist(cutoffs.end[names(failures$failures.end[i])],use.names = F)

      diff  <- end.old - end.new
      ## remove the last X values
      dp.trimmed$dp[names(failures$failures.end[i])][[1]]$data   <-
        utils::head(dp.trimmed$dp[names(failures$failures.end[i])][[1]]$data, -diff)
    }
  }
  message("\nall corrections done\n")
  return(dp.trimmed$dp)
}
