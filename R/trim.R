
dpa_trim_both <- function(dpa, return.plot = FALSE, return.fail = FALSE, silent = FALSE){
  if (silent == TRUE) {
    start <- suppressWarnings(dpa_detect_start(dpa))
    end <-  suppressWarnings(dpa_detect_end(dpa))
  } else {
    start <- dpa_detect_start(dpa)
    end <-  dpa_detect_end(dpa)
  }

  if (return.plot == TRUE) {
    par(mfrow=c(1,1))
    plot(dpa$data$amplitude, type = "l",
         xlab = paste0("Drilling depth [", dpa$footer$xUnit[1], "]"),
         ylab = paste0("Resistograph density [", dpa$footer$yUnit[1], "]"),
         main = paste0("Resistograph data: file ",dpa$footer$ID))
    abline(v=start, col="red", lwd=3, lty=2)
    abline(v=end, col="red", lwd=3, lty=2)
    p <- recordPlot()
    return(p)
  } else {
    if (return.fail == FALSE){
      return(dpa$data[start:end, ])
    } else {
      if (start == 1){
        start.status  <- "failed"
      } else {
        start.status <- "succeeded"
      }
      if (end == nrow(dpa$data)){
        end.status <- "failed"
      } else {
        end.status <- "succeeded"
      }
      return(list("data" = dpa$data[start:end, ],
                  "detection.start" = start.status,
                  "detection.end" = end.status))
    }
  }
}

dpa_trim_both_list  <- function(dpa.list, cl = 1) {
  print(paste0("started trimming ", length(dpa.list), " files"))
  dpa.trimmed  <- pbapply::pblapply(dpa.list,  dpa_trim_both, return.fail = T, silent = T, cl = cl)
  data  <- unlist(lapply(dpa.trimmed, function(x) x[-(2:3)]),recursive=FALSE)
  names(data)  <- names(dpa.trimmed)
  report  <- lapply(dpa.trimmed, function(x) x[-(1)]) %>%
    dplyr::bind_rows(., .id="name")
  cat(paste0("########################################\ntrimming report: \nanalysed ",
             length(dpa.list),
             " file(s) \nstart detection failed in: ",
             sum(report[,2] == "failed"),
             " file(s)\nend detection failed in: ",
             sum(report[,3] == "failed"), " file(s).\n",
             "########################################\n"))
  return(list("data" = data, "report" = report))
}

dpa_trim_start <- function(dpa, return.plot = FALSE, return.fail = FALSE, silent = FALSE){
  if (silent == TRUE) {
    start <- suppressWarnings(dpa_detect_start(dpa))
  } else {
    start <- dpa_detect_start(dpa)
  }

  if (return.plot == TRUE) {
    par(mfrow=c(1,1))
    plot(dpa$data$amplitude, type = "l",
         xlab = paste0("Drilling depth [", dpa$footer$xUnit[1], "]"),
         ylab = paste0("Resistograph density [", dpa$footer$yUnit[1], "]"),
         main = paste0("Resistograph data: file ",dpa$footer$ID))
    abline(v=start, col="red", lwd=3, lty=2)
    p <- recordPlot()
    return(p)
  } else {
    if (return.fail == FALSE){
      return(dpa$data[start:nrow(dpa$data), ])
    } else {
      if (start == 1){
        start.status  <- "failed"
      } else {
        start.status <- "succeeded"
      }
      return(list("data" = dpa$data[start:nrow(dpa$data), ],
                  "detection.start" = start.status))
    }
  }
}

dpa_trim_start_list  <- function(dpa.list, cl = 1) {
  print(paste0("started start trimming ", length(dpa.list), " files"))
  dpa.trimmed  <- pbapply::pblapply(dpa.list,  dpa_trim_start, return.fail = T, silent = T, cl = cl)
  data  <- unlist(lapply(dpa.trimmed, function(x) x[-2]),recursive=FALSE)
  names(data)  <- names(dpa.trimmed)
  report  <- lapply(dpa.trimmed, function(x) x[-(1)]) %>%
    dplyr::bind_rows(., .id="name")
  cat(paste0("########################################\ntrimming report: \nanalysed ",
             length(dpa.list),
             " files \nstart detection failed in: ",
             sum(report[,2] == "failed"),
             " file(s).\n",
             "########################################\n"))
  return(list("data" = data, "report" = report))
}
