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
    ## widnows uses backward slashes, so convert them to froward slashes for regex
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
#' Reads a single *.dpa file and returns a \code{dpa} object,
#' constructed from two lists: \code{data} and \code{footer}. The
#' former one contains actual measurement values, the latter includes
#' supplementary data recorded by the Resistograph device, such as
#' time, firmware number...
#'
#' @param file A path to file, including file name.
#' @return A \code{dpa} object.
#' @seealso read_dpa.
#' @examples
#' \dontrun{
#' read_dpa(system.file("extdata", "00010001.dpa", package = "densiter"))
#' }
read_dpa <- function(file){
  ## check if the file ends in *.dpa
  if (!grepl("\\.dpa$", file)) {stop("not a *.dpa file")}
  dpa.read  <- readLines(file, warn=FALSE)
  data  <- data.frame("amplitude" =  utils::tail(utils::head(dpa.read,n = -14), -3))
  data$position <- 1:nrow(data)
  data$amplitude  <- as.numeric(as.character(data$amplitude))
  data$ID <- extract_dpa_name(file)
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
  class(d) <- 'dpa'
  return(d)
}

#' Load a single Resistograph measurement file (*.dpa) or a directory
#' of *.dpa files.
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
#' load_dpa(system.file("extdata", "00010001.dpa", package = "densiter"))
#' dpa <- load_dpa(system.file("extdata", "00010001.dpa", package = "densiter"))
#' ## load all files in directory
#' dpa.list <- load_dpa(dpa.directory = system.file("extdata", package = "densiter"))
load_dpa  <- function(dpa.file = NULL, dpa.directory = "",
                      recursive = TRUE, name = "file") {
  if (is.null(dpa.file)) {
    ## read the whole directory, possibly recursively
    if (dir.exists(dpa.directory)) {
      dpa.files <- list.files(path = dpa.directory, recursive = recursive, pattern="*.dpa$")
      dpa.files <- file.path(dpa.directory, dpa.files)
      message("found ", length(dpa.files), " dpa files, loading:")
      if (requireNamespace("pbapply", quietly = TRUE)) {
        dpa.list <-  pbapply::pblapply(dpa.files, read_dpa)
      } else {
        dpa.list <-  lapply(dpa.files,  read_dpa)
      }
      if (name == "file") {
        ## name only using file names
        names(dpa.list) <-  extract_dpa_name(dpa.files)
      } else if (name == "folder"){
        ## if recursive, name them properly also using folders
        names(dpa.list) <-  gsub("*.dpa$","",dpa.files)
      }
      return(dpa.list)
    } else {
      ## fail directory doesn't exist
      warning("given directory does not exist")
    }
  } else {
    ## read a single file
    ## check if file exists and is not a directory
    if (utils::file_test("-f", dpa.file)) {
      dpa  <- read_dpa(dpa.file)
      # class(dpa) <- 'dpa'
      return(dpa)
    } else {
      ## fail reading a single file
      warning("file not found")
    }
  }
}

#' Combines footer data from a dpa object list into a single data
#' frame
#'
#' Given a dpa object list, this function will extract all footers
#' (the additional measurement data) from all dpa objects in a given
#' list and combine them in a single data frame/tibble.
#'
#' @param dpa.list A list of dpa objects, either from loading several
#'   files using load_dpa or combined manually. Note: the list should
#'   include only dpa objects!
#' @return A tibble, combining all footer data from dpa.list
#' @seealso load_dpa, combine_data.
#' @export
#' @examples
#' ## load all files in directory
#' dpa.list <- load_dpa(dpa.directory = system.file("extdata", package = "densiter"))
#' combine_footers(dpa.list)
combine_footers  <- function(dpa.list){
  info <- do.call("rbind", lapply(dpa.list,function(x) x$footer))
  rownames(info) <- NULL
  return(info)
 }

#' Combines density measurement from a dpa object list into a single
#' data frame
#'
#' Given a dpa object list, this function will extract all density
#' measurement data from all dpa objects in a given list and combine
#' them in a single data frame/tibble.
#'
#' @param dpa.list A list of dpa objects, either from loading several
#'   files using load_dpa or combined manually. Note: the list should
#'   include only dpa objects!
#' @return A tibble, combining all density data from dpa.list
#' @seealso load_dpa, combine_footer.
#' @export
#' @examples
#' ## load all files in directory
#' dpa.list <- load_dpa(dpa.directory = system.file("extdata", package = "densiter"))
#' combine_data(dpa.list)
combine_data  <- function(dpa.list){
  data  <- lapply(dpa.list,function(x) x$data)
  return(data)
}
