#' Remove automatic trim failures from a list of trimmed dpa objects
#' and return only non-failed trimmed objects
#'
#' Returns a dpa list of trimmed dpa objects without the failed trim
#' objects. Trimmed dpa list should be a result of either calling
#' dtriml on a list of dpa objects or calling dtriml_s to remove the
#' starting portions of the measurement. Both functions should be
#' called with the option "return.fail = FALSE", which embeds a
#' trimming report when returning the list of trimmed dpa objects.
#'
#' @param dpa.trimmed A list of trimmed dpa objects, a result of
#'   calling dtriml or dtriml_s on a dpa list with
#'   "return.fail = FALSE".
#' @return A dpa list of trimmed objects with failures removed.
#' @seealso dtriml, dtriml_s
#' @export
#' @examples
#' ## load a single file
#' dpa.list <- load_dpa(folder = "data")
#' ## trim the measurements
#' dpa.trimmed <- dtriml(dpa.list, "return.fail = FALSE")
#' ## remove trimming failures
#' dpa.nofailures <- remove_trim_failures(dpa.trimmed)
remove_trim_failures  <- function(dpa.trimmed) {
  if (names(dpa.trimmed[2]) != "report") {
    stop("not report attached, trim again with rreport = TRUE")
  }
  failures.start  <- dpa.trimmed$report %>%
    dplyr::filter(detection.start == "failed") %>%
    dplyr::select(ID) %>%
    unlist(., use.names=FALSE)
  dpas <- dpa.trimmed$dpa
  dpa.subsetted <- dpas[!(names(dpas) %in% failures.start)]
  if("detection.end" %in% colnames(dpa.trimmed$report))
  {
    failures.end  <- dpa.trimmed$report %>%
      dplyr::filter(detection.end == "failed") %>%
      dplyr::select(ID) %>%
      unlist(., use.names=FALSE)
    dpa.subsetted <- dpas[!(names(dpas) %in% failures.end)]
  }
  return(dpa.subsetted)
}

#' Remove automatic trim failures from a list of trimmed dpa objects
#' and return ONLY failures
#'
#' An inverse of remove_dpa_failures, return a list of failed trimming
#' objects from a trimmed dpa list. Trimmed dpa list should be a
#' result of either calling dtriml on a list of dpa objects or calling
#' dtriml_s to remove the starting portions of the measurement. Both
#' functions should be called with the option "return.fail = FALSE",
#' which embeds a trimming report when returning the list of trimmed
#' dpa objects.
#'
#' @param dpa.trimmed A list of trimmed dpa objects, a result of
#'   calling dtriml or dtriml_s on a dpa list with
#'   "return.fail = FALSE".
#' @return A dpa list of trimmed objects with failures removed.
#' @seealso dtriml, dtriml_s
#' @export
#' @examples
#' ## load a single file
#' dpa.list <- load_dpa(folder = "data")
#' ## trim the measurements
#' dpa.trimmed <- dtriml(dpa.list, "return.fail = FALSE")
#' ## separate trimming failures
#' dpa.nofailures <- separate_trim_failures(dpa.trimmed)
separate_trim_failures  <- function(dpa.trimmed) {
  if (names(dpa.trimmed[2]) != "report") {
    stop("not report attached, trim again with rreport = TRUE")
  }
  failures.start  <- dpa.trimmed$report %>%
    dplyr::filter(detection.start == "failed") %>%
    dplyr::select(ID) %>%
    unlist(., use.names=FALSE)
  dpas <- dpa.trimmed$dpa
  dpa.start <- dpas[(names(dpas) %in% failures.start)]
  if("detection.end" %in% colnames(dpa.trimmed$report)) {
    failures.end  <- dpa.trimmed$report %>%
      dplyr::filter(detection.end == "failed") %>%
      dplyr::select(ID) %>%
      unlist(., use.names=FALSE)
    dpa.end <- dpas[(names(dpas) %in% failures.end)]
    return(list("failures.start" = dpa.start, "failures.end" = dpa.end))
  } else {
    return(list("failures.start" = dpa.start))
  }
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
#' stop the process and it has to be restarted manually.
#'
#' @param dpa A dpa object, usually see load_dpa.
#' @param label Optional label to be displayed on the plot after the
#'   file
#' @return The x position selected on the graph, row number in the dpa$data data frame.
#' @seealso dtrim, dtriml
#' @export
#' @examples
#' ## load a single file
#' dpa <- load_dpa("data/test.dpa")
#' ## get a starting point on the plot
#' manual_trim_detect(dpa)
manual_trim_detect <- function(failure, label = "start") {
  plot(failure$data$amplitude, type = "l",
       xlab = paste0("Drilling depth"),
       ylab= paste0("Resistograph density"),
       main = paste0("Resistograph data: file ",failure$footer$ID," ",label))
  message("\n[click on graph then pick a vertical line, then confirm]\n")
  click.loc <- locator(1)
  abline(v=click.loc$x, col="red",lwd=3, lty=2)
  keyPressed = readkeygraph(paste0("confirm selection, y or n?"))
  if (keyPressed == "y"){
    cutoff <- click.loc$x
    dev.off()
  } else {
    dev.off()
    plot(failure$data$amplitude, type = "l",
         xlab = paste0("Drilling depth"),
         ylab= paste0("Resistograph density"),
         main = paste0("Resistograph data: file ",failure$footer$ID))
    click.loc <- locator(1)
    abline(v=click.loc$x, col="red",lwd=3, lty=2)
    keyPressed = readkeygraph(paste0("confirm selection, y or n?"))
    if (keyPressed == "y"){
      cutoff <- click.loc$x
      dev.off()
    } else {
      cutoff  <- FALSE
    }
    cutoff  <- FALSE
  }
  if (is.numeric(cutoff)) {
    return(round(cutoff,0))
  } else {
    return(cutoff)
  }
}

#' Manually correct failures after automatic trim detection
#'
#' This function will take a list of trimmed dpa objects (a result of
#' dtriml or dtriml_s function) and interactively ask the user to
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
#' trimmed dpa objects and those corrected manually. The automatic
#' trim functions should be called with the option
#' "return.fail = FALSE", which embeds a trimming report when
#' returning the list of trimmed dpa objects.
#'
#' @param dpa.trimmed A list of dpa objects, trimmed, with the report
#'   embedded ("return.fail = FALSE").
#' @seealso dtrim, dtriml, manual_trim_detect
#' @export
#' @examples
#' ## load all dpa files
#' dpa <- load_dpa(folder = "data")
#' ## trim the list
#' dpa.trimmed <- dtrim(dpa, "return.fail = FALSE")
#' ## manually correct the failures
#' dpa.corrected <- correct_failures(dpa.trimmed)
correct_failures  <- function(dpa.trimmed) {
  failures  <-  separate_trim_failures(dpa.trimmed)
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
      end  <- nrow(dpa.trimmed$dpa[names(failures$failures.start[i])][[1]]$data)
      dpa.trimmed$dpa[names(failures$failures.start[i])][[1]]$data  <- tail(dpa.trimmed$dpa[names(failures$failures.start[i])][[1]]$data, -(start - 1))
    }
  }
  message("\nstart corrections done, starting end corrections\n")
  if (length(failures$failures.end) > 0) {
    cutoffs.end <- lapply(failures$failures.end, manual_trim_detect, label = " - PICK STOP")
    for (i in 1:length(failures$failures.end)){
      end.old  <-
        nrow(dpa.trimmed$dpa[names(failures$failures.end[i])][[1]]$data)

      end.new <-
        unlist(cutoffs.end[names(failures$failures.end[i])],use.names = F)

      diff  <- end.old - end.new
      ## remove the last X values
      dpa.trimmed$dpa[names(failures$failures.end[i])][[1]]$data   <-
        head(dpa.trimmed$dpa[names(failures$failures.end[i])][[1]]$data, -diff)
    }
  }
  message("\nall corrections done\n")
  return(dpa.trimmed)
}
