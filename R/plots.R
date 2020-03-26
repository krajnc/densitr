keydown <- function(key) {
  if (key == "q") {
    grDevices::dev.off()
    #break
    stop()
  } else {
    keyPressed  <<- key
    ## print("11111111111")
  }
}

readkeygraph <- function(prompt)
{
  keyPressed  <- NULL
  grDevices::getGraphicsEvent(prompt = prompt,
                              onMouseDown = NULL, onMouseMove = NULL,
                              onMouseUp = NULL, onKeybd = keydown,
                              consolePrompt = "[click on graph then follow top prompt to continue]")
  Sys.sleep(0.01)
  return(keyPressed)
}

#' Display automatic trimming on a list of dpa objects
#'
#' Display an automatic trimming of dpa list, each dpa object
#' individually. Press any key to move to the next dpa object. Returns
#' nothing.
#'
#' @param dpa.list A list of dpa objects, see \code{load_dpa}
#' @seealso dtrim, dtrim_s, dtriml_s,
#' @export
#' @examples
#' ## load several dpa files
#' dpa.list <- load_dpa(dpa.directory = system.file("extdata", package = "densiter"))
#' ## plot trimming the measurements
#' \dontrun{
#' plot_trimming(dpa.list)
#' }
plot_trimming  <- function(dpa.list){
  for (i in 1:length(dpa.list)){
    print((dtrim(dpa.list[[i]], return.plot = T)))
    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dpa.list)))
  }
}

#' Display start detection on a list of dpa objects
#'
#' Display an automatic start detection of dpa list, each dpa object
#' individually. Press any key to move to the next dpa object. Returns
#' nothing.
#'
#' @param dpa.list A list of dpa objects, see \code{load_dpa}
#' @seealso dtrim, dtrim_s, dtriml_s,
#' @export
#' @examples
#' ## load several dpa files
#' dpa.list <- load_dpa(dpa.directory = system.file("extdata", package = "densiter"))
#' ## trim the measurements
#' \dontrun{
#' plot_start_detection(dpa.list)
#' }
plot_start_detection  <- function(dpa.list){
  for (i in 1:length(dpa.list)){
    print((dpa_detect_start(dpa.list[[i]], return.plot = T)))
    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dpa.list)))
  }
}

#' Display end detection on a list of dpa objects
#'
#' Display an automatic end detection of dpa list, each dpa object
#' individually. Press any key to move to the next dpa object. Returns
#' nothing.
#'
#' @param dpa.list A list of dpa objects, see \code{load_dpa}
#' @seealso dtrim, dtrim_s, dtriml_s,
#' @export
#' @examples
#' ## load several dpa files
#' dpa.list <- load_dpa(dpa.directory = system.file("extdata", package = "densiter"))
#' ## trim the measurements
#' \dontrun{
#' plot_end_detection(dpa.list)
#' }
plot_end_detection  <- function(dpa.list){
  for (i in 1:length(dpa.list)){
    print((dpa_detect_end(dpa.list[[i]], return.plot = T)))
    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dpa.list)))
  }
}

#' Plot a list of dpa objects, one by one
#'
#' Plot a list of dpa objects, one by one. Press any key to move to
#' the next dpa object. Returns nothing.
#'
#' @param dpa.list A list of dpa objects, see \code{load_dpa}
#' @seealso dtrim, dtrim_s, dtriml_s,
#' @export
#' @examples
#' ## load several dpa files
#' dpa.list <- load_dpa(dpa.directory = system.file("extdata", package = "densiter"))
#' ## trim the measurements
#' \dontrun{
#' plot_all(dpa.list)
#' }
plot_all  <- function(dpa.list){
  for (i in 1:length(dpa.list)){
    dpa  <- dpa.list[[i]]
    print(graphics::plot(dpa))
    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dpa.list)))
  }
}

#' Plot trimming failures one by one
#'
#' Plot each failed trimming detection, one by one. Press any key to
#' move to the next dpa object. Returns nothing. The entry list of dpa
#' trimmed objects must include the trimming report (rreport = TRUE).
#'
#' @param dpa.trimmed A list of trimmed dpa objects, see \code{load_dpa}
#' @seealso dtrim, dtrim_s, dtriml_s,
#' @export
#' @examples
#' ## load several dpa files
#' dpa.list <- load_dpa(dpa.directory = system.file("extdata", package = "densiter"))
#' ## trim the measurements
#' \dontrun{
#' plot_failures(dpa.list)
#' }
plot_failures  <- function(dpa.trimmed){
  failures  <-  separate_trim_failures(dpa.trimmed)
  failures2  <- union(failures$failures.start, failures$failures.end)
  names(failures2)  <-  union(names(failures$failures.start), names(failures$failures.end))
  for (i in 1:length(failures2)){
    dpa  <- failures2[[i]]$data
    print(graphics::plot(dpa$amplitude, type = "l",
                         xlab = paste0("Drilling depth"),
               ylab= paste0("Resistograph density"),
               main = paste0("Resistograph data, trimming failure ",dpa$ID[1])))

    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dpa.trimmed)))
  }
}
