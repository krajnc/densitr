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

correct_failures  <- function(dpa.trimmed) {
  failures  <-  separate_trim_failures(dpa.trimmed)
  message("\nfound:\n",
          length(failures$failures.start),
          " start failures \n",
          length(failures$failures.end),
          " end failures" )
  if (length(failures$failures.start) > 0) {
    cutoffs.start <-
      apply(failures$failures.start, manual_trim_detect, label = "PICK START")
    for (i in 1:length(failures$failures.start)){
      start <- unlist(cutoffs.start[names(failures$failures.start[i])],use.names = F)
      end  <- nrow(dpa.trimmed$dpa[names(failures$failures.start[i])][[1]]$data)
      dpa.trimmed$dpa[names(failures$failures.start[i])][[1]]$data  <- tail(dpa.trimmed$dpa[names(failures$failures.start[i])][[1]]$data, -(start - 1))
    }
  }
  message("\nstart corrections done, starting end corrections\n")
  if (length(failures$failures.end) > 0) {
    cutoffs.end <- lapply(failures$failures.end, manual_trim_detect, label = "PICK STOP")
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
  message("\ncorrections done\n")
  return(dpa.trimmed)
}
