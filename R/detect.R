
dpa_detect_start <- function(dpa, cutoff.sd = 1, return.plot = FALSE){

  ## check if dpa object
  if (!inherits(dpa,"dpa"))  {
    stop("not a dpa object")
  }

  ## dpa <- unclass(dpa)

  data.in <-
    loess(dpa$data$amplitude ~ dpa$data$position, span=0.1) %>%
    predict(.) %>%
    diff(.) %>%
    zoo::rollmeanr(.,100)

  ## fit <- loess(dpa$data$amplitude ~ dpa$data$position, span=0.1)
  ## fitted <- predict(fit)
  ## data.in <-  zoo::rollmeanr(diff(fitted),100)

  limit <- abs(mean(data.in) + (cutoff.sd * sd(data.in)))

  segments.points <- suppressWarnings(changepoint::cpt.meanvar(data.in,
                                                               method="BinSeg", Q=10,
                                                               minseglen=250,class=FALSE))
  segments.list <- splitAt(data.in,segments.points)
  segments.list[length(segments.list)] <- NULL  #remove the last item in a list

  segment.value <- function(number){
    return(abs(mean(segments.list[[number]])))
  }

  ## check the first four segments, if they are outside of the set limit and return the end positions of those segments
  if (segment.value(4) < limit) {
    if (segment.value(3) < limit) {
      if (segment.value(2) < limit) {
        if (segment.value(1) < limit) {
          ## no segments found outside the limit in the first 4 segments
          warning(paste("start not detected in measurement ",dpa$footer$ID[1],sep="" ))
          cutoff <- 1
          ## return(1) #return the first position
        } else {
          cutoff <- segments.points[1]
          ## return(segments.points[1])
        }
      } else {
        ## return the position of the second segment, deleting the first two
        cutoff <- segments.points[2]
        ## return(segments.points[2])
      }
    } else {
      ## return the position of the third segment, deleting the first three
      cutoff <- segments.points[3]
      ## return(segments.points[3])
    }
  } else {
    ## return the position of the fourth segment, deleting the first four segments
    cutoff <- segments.points[4]
    ## return(segments.points[4])
  }

  if (return.plot == TRUE) {
    segments.points2 <- suppressWarnings(changepoint::cpt.meanvar(data.in,
                                                                  method="BinSeg", Q=10,
                                                                  minseglen=250,class=TRUE))
    par(mfrow=c(2,1))
    plot(dpa$data$amplitude, type = "l",
         xlab = paste0("Drilling depth [", dpa$footer$xUnit, "]"),
         ylab= paste0("Resistograph density [", dpa$footer$yUnit, "]"),
         main = paste0("Resistograph data: file ",dpa$footer$ID))
    abline(v=cutoff, col="red",lwd=3, lty=2)
    ## [1:length(dpa$data$amplitude)/2]
    changepoint::plot(segments.points2,
                      xlab = paste0("Drilling depth [", dpa$footer$xUnit, "]"),
         ylab= paste0("Moving average of lagged differences"),
         main="Detected segments, overall mean and cutoff limits")
    abline(h=mean(data.in), col="blue")
    abline(h=limit, col="green")
    abline(h=-limit, col="green")
    abline(v=cutoff, col="red", lwd=3, lty=2)
    p <- recordPlot()
    return(p)
  } else {
    return(cutoff) # add 100 to account for rolling mean right centred
  }
}

dpa_detect_end <- function(dpa, cutoff.sd = 1, return.plot = FALSE){

  ## check if dpa object
  if (!inherits(dpa,"dpa"))  {
    stop("not a dpa object")
  }

  data.in <-
    loess(dpa$data$amplitude ~ dpa$data$position, span=0.1) %>%
    predict(.) %>%
    diff(.) %>%
    zoo::rollmeanr(.,100)

  ## fit <- loess(dpa$amplitude ~ dpa$position, span=0.1)
  ## fitted <- predict(fit)
  ## data.in <- rollmeanr(diff(fitted),100)

  limit <- mean(data.in) - (cutoff.sd * sd(data.in))

  segments.points <- suppressWarnings(changepoint::cpt.meanvar(data.in,
                                                               method="BinSeg", Q=10,
                                                               minseglen=250,class=FALSE))
  segments.list <- splitAt(data.in,segments.points)
  segments.list[length(segments.list)] <- NULL  #remove the last item in a list

  segment.value2 <- function(number){
    return(mean(segments.list[[length(segments.list)-number]]))
  }

  if (segment.value2(3) < limit) {
    #izbrišeš prve 4
    ## return(segments.points[ length(segments.points)-4])
    cutoff <- segments.points[length(segments.points)-4]
    #print("4")
  } else {
    if (segment.value2(2) < limit) {
      #izbrišeš prve 3
      ## return(segments.points[length(segments.points)-3])
      cutoff <- segments.points[length(segments.points)-3]
      #print("3")
    } else {
      if (segment.value2(1) < limit) {
        #print("2")
        ## return(segments.points[length(segments.points)-2])
        cutoff <- segments.points[length(segments.points)-2]
      } else {
        if (segment.value2(0) < limit) {
          #izbrišeš prve 1
          ## return(segments.points[length(segments.points)-1])
          cutoff <- segments.points[length(segments.points)-1]
          #print("1")
        } else {
          #ne izbrišeš nič
          warning(paste("end not detected in file ",dpa$footer$ID[1],sep="" ))
          cutoff <- nrow(dpa$data)
          ## return(nrow(dpa))
        }
      }
    }
  }

  if (return.plot == TRUE) {
    segments.points2 <- suppressWarnings(changepoint::cpt.meanvar(data.in,
                                                                  method="BinSeg", Q=10,
                                                                  minseglen=250,class=TRUE))
    par(mfrow=c(2,1))
    plot(dpa$data$amplitude, type = "l",
         xlab = paste0("Drilling depth [", dpa$footer$xUnit[1], "]"),
         ylab= paste0("Resistograph density [", dpa$footer$yUnit[1], "]"),
         main = paste0("Resistograph data: file ",dpa$footer$ID))
    abline(v=cutoff + 100, col="red", lwd=3, lty=2)
    ## [1:length(dpa$data$amplitude)/2]
    changepoint::plot(segments.points2,
                      xlab = paste0("Drilling depth [", dpa$footer$xUnit[1], "]"),
         ylab= paste0("Moving average of lagged differences"),
         main="Detected segments, overall mean and cutoff limits")
    abline(h=mean(data.in), col="blue")
    abline(h=limit, col="green")
    abline(h=-limit, col="green")
    abline(v=cutoff, col="red", lwd=3, lty=2)
    p <- recordPlot()
    return(p)
  } else {
    ## if end detected, add 100 to account for moving averages
    if (cutoff == nrow(dpa$data)) {
      return(cutoff)
    } else {
      return(cutoff + 100)
    }
  }
}
