
## get filename using regex
## sapply(regmatches(dpa.file, regexec("[\\w-]+?(?=\\.)", dpa.file, perl = TRUE)), "[", 1)
## str_extract( "data/00040001.dpa", regex("[\\w-]+?(?=\\.)"))
## source https://stackoverflow.com/questions/47678725/how-to-do-str-extract-with-base-r
extract_dpa_name  <- function(string){
  return(sapply(
    regmatches(
      string,
      regexec("[\\w-]+?(?=\\.)",
              string, perl = TRUE)),
    "[", 1))
}

### rabiš tudi magitrr
read_dpa <- function(file){
  ## check ending
  if (!grepl("\\.dpa$", file))  {
    stop("not a *.dpa file")
  }

  dpa.read  <- readr::read_lines(file,skip=3)

  dpa.data <- dpa.read  %>%
    head(n = -14) %>%
    dplyr::tibble(amplitude=.) %>%
    dplyr::mutate_at("amplitude", as.numeric) %>%
    tibble::rowid_to_column(var = "position") %>%
    dplyr::mutate(ID=extract_dpa_name(file))

  dpa.footer <-  dpa.read %>%
    tail(n=13) %>%
    paste(collapse = "\n")  %>%
    readr::read_csv(.,col_names = c("footer")) %>%
    tidyr::separate(footer, into = c("name","value"),sep="=") %>%
    dplyr::mutate(ID=extract_dpa_name(file)) %>%
    tidyr::pivot_wider(names_from = name, values_from = value)

  ## dpa.complete <- dpa.data %>%
  ##   dplyr::left_join(dpa.footer, by="ID") #%>%    mutate_at(c("ID","position"), as.factor)

  d  <- list("data" = dpa.data, "footer" = dpa.footer)
  class(d) <- 'dpa'
  return(d)
}


## funnkcija za load dpa files, if za recursive, uporabi pbapply če obstaja
load_dpa  <- function(dpa.file = NULL, dpa.directory = "",
                      recursive = TRUE, name = "file") {
  if (is.null(dpa.file)) {
    ## read the whole directory, possibly recursively
    if (dir.exists(dpa.directory)) {
      dpa.files <- list.files(path = dpa.directory, recursive = recursive, pattern="*.dpa$")
      dpa.files <- file.path(dpa.directory, dpa.files)
      print(paste0("found ",length(dpa.files), " dpa files, loading:"))
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
    if (file_test("-f", dpa.file)) {
      dpa  <- read_dpa(dpa.file)
      # class(dpa) <- 'dpa'
      return(dpa)
    } else {
      ## fail reading a single file
      warning("file not found")
    }
  }
}


combine_footers  <- function(dpa.list){
   info  <- lapply(dpa.list,function(x) x$footer) %>%
     dplyr::bind_rows()
   return(info)
}

combine_data  <- function(dpa.list){
  data  <- lapply(dpa.list,function(x) x$data)
  return(data)
}
