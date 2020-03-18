### rabiš tudi magitrr
read_dpa_file <- function(dpa.file){
  dpa.read  <- readr::read_lines(dpa.file,skip=3)

  ## get filename using regex
  ## sapply(regmatches(dpa.file, regexec("[\\w-]+?(?=\\.)", dpa.file, perl = TRUE)), "[", 1)
  ## str_extract( "data/00040001.dpa", regex("[\\w-]+?(?=\\.)"))
  ## source https://stackoverflow.com/questions/47678725/how-to-do-str-extract-with-base-r
  dpa.data <- dpa.read  %>%
    head(n = -14) %>%
    dplyr::tibble(amplitude=.) %>%
    dplyr::mutate_at("amplitude", as.numeric) %>%
    tibble::rowid_to_column(var = "position") %>%
    dplyr::mutate(ID=sapply(regmatches(dpa.file,
                                       regexec("[\\w-]+?(?=\\.)", dpa.file, perl = TRUE)),
                            "[", 1))

  dpa.footer <-  dpa.read %>%
    tail(n=13) %>%
    paste(collapse = "\n")  %>%
    readr::read_csv(.,col_names = c("footer")) %>%
    tidyr::separate(footer, into = c("name","value"),sep="=") %>%
    dplyr::mutate(ID=sapply(regmatches(dpa.file,
                                       regexec("[\\w-]+?(?=\\.)", dpa.file, perl = TRUE)),
                            "[", 1)) %>%
    tidyr::pivot_wider(names_from = name, values_from = value)

  ## dpa.complete <- dpa.data %>%
  ##   dplyr::left_join(dpa.footer, by="ID") #%>%    mutate_at(c("ID","position"), as.factor)

  dpa.complete  <- list("data" = dpa.data, "footer" = dpa.footer)

  return(dpa.complete)

}

## funnkcija za load dpa files, if za recursive, uporabi pbapply če obstaja
load_dpa  <- function(dpa.file = NULL, dpa.directory = "", recursive = TRUE) {
  if (is.null(dpa.file)) {
    ## read the whole directory, possibly recursively
    if (dir.exists(dpa.directory)) {
      dpa.files <- list.files(path = dpa.directory, recursive = recursive, pattern="*.dpa$")
      dpa.files <- file.path(dpa.directory, dpa.files)
      print(paste0("found ",length(dpa.files), " dpa files, loading:"))
      if (requireNamespace("pbapply", quietly = TRUE)) {
        dpa.list <-  pbapply::pblapply(dpa.files,  read_dpa_file)
      } else {
        dpa.list <-  lapply(dpa.files,  read_dpa_file)
      }
      ## if recursive, name them properly also using folders
      names(dpa.list) <-  gsub("*.dpa$","",dpa.files)

      ## alternative, only use dpa name
      ## sapply(regmatches(dpa.files, regexec("[\\w-]+?(?=\\.)", dpa.files, perl = TRUE)), "[", 1)
      return(dpa.list)
    } else {
      ## fail directory doesn't exist
      warning("given directory does not exist")
    }
  } else {
    ## read a single file
    ## check if file exists and is not a directory
    if (file_test("-f", dpa.file)) {
      dpa  <- read_dpa_file(dpa.file)
      return(dpa)
    } else {
      ## fail reading a single file
      warning("file not found")
    }
  }
}
