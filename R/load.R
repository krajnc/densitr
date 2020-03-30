#' Extract a file name from a full path
#'
#' A wrapper function for regex extraction of filename. Given a
#' character string ("data/0005/00/00050060.dpa"), it will return only
#' the file name without the extension ("00050060").
#'
#' @param string A path to file, including file name. Can be nested in
#'   many directories or in none.
#' @return An extracted filename, a character string.
#' @examples
#' \dontrun{
#' extract_dpa_name("data/0005/00/00050060.dpa")
#' }
#' @references
#' https://stackoverflow.com/questions/47678725/how-to-do-str-extract-with-base-r
extract_dpa_name  <- function(string){
  if(.Platform$OS.type == "unix") {
    return(sapply(
      regmatches(
        string,
        regexec("[\\w-]+?(?=\\.dpa)",
                string, perl = TRUE)),
      "[", 1))
  } else {
    ## windows uses backward slashes, so convert them to froward slashes for regex
    string  <- gsub("\\\\", "/", string)
    return(sapply(
      regmatches(
        string,
        regexec("[\\w-]+?(?=\\.dpa)",
                string, perl = TRUE)),
      "[", 1))
  }
  ## tidyverse alternative: str_extract( "data/00040001.dpa", regex("[\\w-]+?(?=\\.)"))
}

#' Read a single Resistograph measurement file (*.dpa)
#'
#' Reads a single *.dpa file and returns a \code{dp} object,
#' constructed from two lists: \code{data} and \code{footer}. The
#' former one contains actual measurement values, the latter includes
#' supplementary data recorded by the Resistograph device, such as
#' time, firmware number...
#'
#' @param file A path to file, including file name.
#' @return A \code{dp} object.
#' @seealso dpload
#' @examples
#' \dontrun{
#' read_dpa(system.file("extdata", "00010001.dpa", package = "densiter"))
#' }
read_dpa <- function(file){
  ## check if the file ends in *.dpa
  if (!grepl("\\.dpa$", file)) {stop("not a *.dpa file")}
  dpa.read  <- readLines(file, warn=FALSE)
  data  <- data.frame("amplitude" =  utils::tail(utils::head(dpa.read,n = -14), -3))
  if (nrow(data)!= 0){
    data$position <- 1:nrow(data)
    data$amplitude  <- as.numeric(as.character(data$amplitude))
  } else {
    data <- NULL
    data$position <- NA
    data$amplitude <- NA
  }
  data$ID <- extract_dpa_name(file)
  row.names(data)  <- NULL
  footer  <-  paste(utils::tail(dpa.read, n=13), collapse = "\n")
  footer  <- utils::read.csv(text = footer, check.names=FALSE, header = F, col.names = "footer")
  footer$name <- sapply(strsplit(as.character(footer$footer), "="), "[", 1)
  footer$value <- sapply(strsplit(as.character(footer$footer), "="), "[", 2)
  footer$footer <- NULL
  footer$ID <- extract_dpa_name(file)
  footer <- stats::reshape(footer, idvar = "ID", timevar = "name", direction = "wide")
  names(footer) <- gsub("(value\\.y\\.|value\\.)", "", names(footer))
  attributes(footer)$reshapeWide  <- NULL # strip reshaping attributes
  d  <- list("data" = data, "footer" = footer)
  class(d) <- 'dp'
  return(d)
}

#' Load a single Resistograph measurement file (*.dpa) or a directory
#' of *.dpa files.
#'
#' Loads either a single .dpa file or a list of .dpa files. If
#' dpa.file is specified, it will load a single file. If dp.directory
#' is specified, it will search for all dpa files in that directory
#' (recursively in all subfolders, can be turned off) and return a
#' list of dp files. It will use pbapply to display progress, if
#' loading a directory.
#'
#' @param dp.file A path to a single file, including file name.
#' @param dp.directory A directory with .dpa files.
#' @param recursive Also look for density profiles files in subfolders?
#' @param name Either \code{c("file", "folder")}, used for naming of
#'   list items. If "file", only file name without the complete path
#'   will be used for naming ("00050060"). If "folder", the complete
#'   path along with file name will be used to name the dpa objects
#'   ("data/0005/00/00050060"). *.dpa ending is removed from the name
#'   in both cases.
#' @return A \code{dp} object or a list of \code{dp} objects.
#' @export
#' @examples
#' ## load a single file
#' dpload(system.file("extdata", "00010001.dpa", package = "densiter"))
#' dp <- dpload(system.file("extdata", "00010001.dpa", package = "densiter"))
#' ## load all files in directory
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densiter"))
dpload  <- function(dp.file = NULL, dp.directory = "",
                    recursive = TRUE, name = "file") {
  if (is.null(dp.file)) {
    ## read the whole directory, possibly recursively
    if (dir.exists(dp.directory)) {
      dp.files <- list.files(path = dp.directory, recursive = recursive, pattern="*.dpa$")
      dp.files <- file.path(dp.directory, dp.files)
      message("found ", length(dp.files), " density profiles, loading...")
      if (requireNamespace("pbapply", quietly = TRUE)) {
        dp.list <-  pbapply::pblapply(dp.files, read_dpa)
      } else {
        ptm <- proc.time()
        dp.list <-  lapply(dp.files,  read_dpa)
        stop  <- proc.time() - ptm
        message("loading took ", round(stop[3],0), " seconds, consider installing pbapply to show progress using a progress bar")
      }
      if (name == "file") {
        ## name only using file names
        names(dp.list) <-  extract_dpa_name(dp.files)
      } else if (name == "folder"){
        ## if recursive, name them properly also using folders
        names(dp.list) <-  gsub("*.dpa$","",dp.files)
      }
      message("loaded ", length(dp.files), " density profiles")
      return(dp.list)
    } else {
      ## fail directory doesn't exist
      warning("given directory does not exist")
    }
  } else {
    ## read a single file
    ## check if file exists and is not a directory
    if (utils::file_test("-f", dp.file)) {
      dp  <- read_dpa(dp.file)
      # class(dp) <- 'dp'
      return(dp)
    } else {
      ## fail reading a single file
      warning("file not found")
    }
  }
}

#' Combines footer data from a dp object list into a single data
#' frame
#'
#' Given a dp object list, this function will extract all footers
#' (the additional measurement data) from all dp objects in a given
#' list and combine them in a single data frame.
#'
#' @param dp.list A list of dp objects, either from loading several
#'   files using dpload or combined manually. Note: the list should
#'   include only dp objects!
#' @return A data frame, combining all footer data from dp.list
#' @seealso dpload, combine_data.
#' @export
#' @examples
#' ## load all files in directory
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densiter"))
#' combine_footers(dp.list)
combine_footers  <- function(dp.list){
  info <- do.call("rbind", lapply(dp.list,function(x) x$footer))
  rownames(info) <- NULL
  return(info)
 }

#' Combines density measurement from a dp object list into a single
#' data frame
#'
#' Given a dp object list, this function will extract all density
#' measurement data from all dp objects in a given list and combine
#' them in a single data frame.
#'
#' @param dp.list A list of dp objects, either from loading several
#'   files using dpload or combined manually. Note: the list should
#'   include only dp objects!
#' @return A data frame, combining all density data from dp.list
#' @seealso dpload, combine_footer.
#' @export
#' @examples
#' ## load all files in directory
#' dp.list <- dpload(dp.directory = system.file("extdata", package = "densiter"))
#' combine_data(dp.list)
combine_data  <- function(dp.list){
  message("\ncombining data from", length(dp.list), " denisty profiles...")
  data  <- lapply(dp.list,function(x) x$data)
  data <- do.call("rbind", data)
  rownames(data) <- NULL
  message("\n...done.")
  return(data)
}
