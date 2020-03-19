

remove_trim_failures  <- function(dpa.trimmed) {
  failures.start  <- dpa.trimmed$report %>%
    dplyr::filter(detection.start == "failed") %>%
    dplyr::select(name) %>%
    unlist(., use.names=FALSE)

  dpa.subsetted <- dpa.trimmed$data[!(names(dpa.trimmed$data) %in% failures.start)]

  if("detection.end" %in% colnames(dpa.trimmed$report))
  {
    failures.end  <- dpa.trimmed$report %>%
      dplyr::filter(detection.end == "failed") %>%
      dplyr::select(name) %>%
      unlist(., use.names=FALSE)
    dpa.subsetted <- dpa.trimmed$data[!(names(dpa.trimmed$data) %in% failures.end)]
  }
  return(dpa.subsetted)
}

separate_trim_failures  <- function(dpa.trimmed) {
  failures.start  <- dpa.trimmed$report %>%
    dplyr::filter(detection.start == "failed") %>%
    dplyr::select(name) %>%
    unlist(., use.names=FALSE)

  dpa.start <- dpa.trimmed$data[(names(dpa.trimmed$data) %in% failures.start)]

  if("detection.end" %in% colnames(dpa.trimmed$report))
  {
    failures.end  <- dpa.trimmed$report %>%
      dplyr::filter(detection.end == "failed") %>%
      dplyr::select(name) %>%
      unlist(., use.names=FALSE)
    dpa.end <- dpa.trimmed$data[(names(dpa.trimmed$data) %in% failures.end)]
    return(list("failures.start" = dpa.start, "failures.end" = dpa.end))
  } else {
    return(list("failures.start" = dpa.start))
  }
}



manual_trim_detect <- function(failure, label = "start") {
  plot(failure$amplitude, type = "l",
       xlab = paste0("Drilling depth"),
       ylab= paste0("Resistograph density"),
       main = paste0("Resistograph data: file ",failure$ID[1]," ",label))
  cat("\n[click on graph then pick a vertical line, then confirm]\n")
  click.loc <- locator(1)
  abline(v=click.loc$x, col="red",lwd=3, lty=2)
  keyPressed = readkeygraph(paste0("confirm selection, y or n?"))
  if (keyPressed == "y"){
    cutoff <- click.loc$x
    dev.off()
  } else {
    dev.off()
    plot(failure$amplitude, type = "l",
         xlab = paste0("Drilling depth"),
         ylab= paste0("Resistograph density"),
         main = paste0("Resistograph data: file ",failure$ID[1]))
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


correct_failures  <- function(dpa.trimmed) {
  failures  <-  separate_trim_failures(dpa.trimmed)

  cat(paste0("\nfound:\n",length(failures$failures.start)," start failures \n",length(failures$failures.end)," end failures" ))

  if (length(failures$failures.start) > 0) {
    cutoffs.start <-    lapply(failures$failures.start, manual_trim_detect, label = "PICK START")
    for (i in 1:length(failures$failures.start)){
      start <- unlist(cutoffs.start[names(failures$failures.start[i])],use.names = F)
      end  <-  nrow(dpa.trimmed$data[names(failures$failures.start[i])]$data)

      dpa.trimmed$data[names(failures$failures.start[i])]  <- list(tail(dpa.trimmed$data[names(failures$failures.start[i])]$data, -(start - 1)))
    }
  }

  cat("\nstart corrections done, starting end corrections\n")
  ## both <- intersect(names(failures$failures.start),
  ##                   names(failures$failures.end))
  if (length(failures$failures.end) > 0) {
    cutoffs.end <- lapply(failures$failures.end, manual_trim_detect, label = "PICK STOP")
    for (i in 1:length(failures$failures.end)){

      end.old  <-
        nrow(dpa.trimmed$data[names(failures$failures.end[i])]$data)

      end.new <-
        unlist(cutoffs.end[names(failures$failures.end[i])],use.names = F)

      diff  <- end.old - end.new

      ## remove the last X values
      dpa.trimmed$data[names(failures$failures.end[i])]   <-
        list(head(dpa.trimmed$data[names(failures$failures.end[i])]$data, -diff))
    }
  }
  return(dpa.trimmed)
}
