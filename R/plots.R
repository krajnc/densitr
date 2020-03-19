
keydown <- function(key) {
  if (key == "q") {
    dev.off()
    break
  } else {
    keyPressed <<- key
    ## print("11111111111")
  }
}

readkeygraph <- function(prompt)
{
  getGraphicsEvent(prompt = prompt,
                   onMouseDown = NULL, onMouseMove = NULL,
                   onMouseUp = NULL, onKeybd = keydown,
                   consolePrompt = "[click on graph then follow top prompt to continue]")
  Sys.sleep(0.01)
  return(keyPressed)
}

plot_trimming  <- function(dpa.list){
  for (i in 1:length(dpa.list)){
    print((dpa_trim_both(dpa.list[[i]], return.plot = T)))
    ##Sys.sleep(2)
    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dpa.list)))
  }
}

plot_start_detection  <- function(dpa.list){
  for (i in 1:length(dpa.list)){
    print((dpa_detect_start(dpa.list[[i]], return.plot = T)))
    ##Sys.sleep(2)
    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dpa.list)))
  }
}

plot_end_detection  <- function(dpa.list){
  for (i in 1:length(dpa.list)){
    print((dpa_detect_end(dpa.list[[i]], return.plot = T)))
    ##Sys.sleep(2)
    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dpa.list)))
  }
}

plot_all  <- function(dpa.list){
  for (i in 1:length(dpa.list)){
    dpa  <- dpa.list[[i]]
    print(plot(dpa$data$amplitude, type = "l",
               xlab = paste0("Drilling depth [", dpa$footer$xUnit, "]"),
               ylab= paste0("Resistograph density [", dpa$footer$yUnit, "]"),
               main = paste0("Resistograph data: file ",dpa$footer$ID)))
    ##Sys.sleep(2)
    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dpa.list)))
  }
}


plot_failures  <- function(dpa.trimmed){
  failures  <-  separate_trim_failures(dpa.trimmed)
  failures2  <- union(failures$failures.start, failures$failures.end)
  names(failures2)  <-  union(names(failures$failures.start), names(failures$failures.end))

  for (i in 1:length(failures2)){
    dpa  <- failures2[[i]]
    print(plot(dpa$amplitude, type = "l",
               xlab = paste0("Drilling depth"),
               ylab= paste0("Resistograph density"),
               main = paste0("Resistograph data, trimming failure ",dpa$ID[1])))

    ##Sys.sleep(2)
    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dpa.list)))
  }
}
