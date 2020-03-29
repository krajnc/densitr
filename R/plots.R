## declare keyPressed for graph confirmation
utils::globalVariables(c("keyPressed"))

keydown <- function(key) {
  if (key == "q") {
    grDevices::dev.off()
    stop()
  } else {
    ## keyPressed  <<- key
    ##assign("keyPressed", key, inherits = TRUE, envir = .GlobalEnv)
    assign("keyPressed", key, inherits = TRUE)
  }
}

readkeygraph <- function(prompt)
{
  grDevices::getGraphicsEvent(prompt = prompt,
                              onKeybd = keydown,
                              consolePrompt = "[click on graph then follow top prompt to continue]")
  Sys.sleep(0.01)
  return(keyPressed)
}

#' Display automatic trimming on a list of dp objects
#'
#' Display an automatic trimming of dp list, each dp object
#' individually. Press any key to move to the next dp object. Returns
#' nothing.
#'
#' @param dp.list A list of dp objects, see \code{dpload}
#' @seealso dptrim, dptrim_s, dptriml_s,
#' @export
#' @examples
#' ## load several dp files
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densiter"))
#' ## plot trimming the measurements
#' \dontrun{
#' plot_trimming(dp.list)
#' }
plot_trimming  <- function(dp.list){
  for (i in 1:length(dp.list)){
    print((dptrim(dp.list[[i]], return.plot = T)))
    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dp.list)))
  }
}

#' Display start detection on a list of dp objects
#'
#' Display an automatic start detection of dp list, each dp object
#' individually. Press any key to move to the next dp object. Returns
#' nothing.
#'
#' @param dp.list A list of dp objects, see \code{dpload}
#' @seealso dtrim, dtrim_s, dtriml_s,
#' @export
#' @examples
#' ## load several dp files
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densiter"))
#' ## trim the measurements
#' \dontrun{
#' plot_start_detection(dp.list)
#' }
plot_start_detection  <- function(dp.list){
  for (i in 1:length(dp.list)){
    print((dpdetect_s(dp.list[[i]], return.plot = T)))
    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dp.list)))
  }
}

#' Display end detection on a list of dp objects
#'
#' Display an automatic end detection of dp list, each dp object
#' individually. Press any key to move to the next dp object. Returns
#' nothing.
#'
#' @param dp.list A list of dp objects, see \code{dpload}
#' @seealso dtrim, dtrim_s, dtriml_s,
#' @export
#' @examples
#' ## load several dp files
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densiter"))
#' ## trim the measurements
#' \dontrun{
#' plot_end_detection(dp.list)
#' }
plot_end_detection  <- function(dp.list){
  for (i in 1:length(dp.list)){
    print((dpdetect_e(dp.list[[i]], return.plot = T)))
    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dp.list)))
  }
}

#' Plot a list of dp objects, one by one
#'
#' Plot a list of dp objects, one by one. Press any key to move to
#' the next dp object. Returns nothing.
#'
#' @param dp.list A list of dp objects, see \code{dpload}
#' @seealso dtrim, dtrim_s, dtriml_s,
#' @export
#' @examples
#' ## load several dp files
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densiter"))
#' ## trim the measurements
#' \dontrun{
#' plot_all(dp.list)
#' }
plot_all  <- function(dp.list){
  if (is.list(dp.list) == FALSE) {stop("not a list of density profiles")}
  for (i in 1:length(dp.list)){
    dp  <- dp.list[[i]]
    print(graphics::plot(dp))
    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dp.list)))
  }
}

#' Plot trimming failures one by one
#'
#' Plot each failed trimming detection, one by one. Press any key to
#' move to the next dp object. Returns nothing. The entry list of dp
#' trimmed objects must include the trimming report (rreport = TRUE).
#'
#' @param dp.trimmed A list of trimmed dp objects, see \code{dpload}
#' @seealso dtrim, dtrim_s, dtriml_s,
#' @export
#' @examples
#' ## load several dp files
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densiter"))
#' ## trim the measurements
#' \dontrun{
#' plot_failures(dp.list)
#' }
plot_failures  <- function(dp.trimmed){
  failures  <-  separate_trim_failures(dp.trimmed)
  failures2  <- union(failures$failures.start, failures$failures.end)
  names(failures2)  <-  union(names(failures$failures.start), names(failures$failures.end))
  for (i in 1:length(failures2)){
    dp  <- failures2[[i]]$data
    print(graphics::plot(dp$amplitude, type = "l",
                         xlab = paste0("Drilling depth"),
               ylab= paste0("Resistograph density"),
               main = paste0("Resistograph data, trimming failure ",dp$ID[1])))

    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dp.trimmed)))
  }
}
