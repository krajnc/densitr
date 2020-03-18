
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

plot_start_detection  <- function(dpa.list){
  for (i in 1:length(dpa.list)){
    print((dpa_detect_end(dpa.list[[i]], return.plot = T)))
    ##Sys.sleep(2)
    keyPressed = readkeygraph(paste0("[any key to continue, q to quit] file ",i,"/",length(dpa.list)))
  }
}
