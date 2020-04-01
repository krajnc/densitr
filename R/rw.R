## https://github.com/stas-g/findPeaks
find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

get_duplicates  <- function(values){
  removals <- c()
  for (i in 1:(nrow(values) - 1)){
    # i <- 84
    k <- i + 1
    # values[2,]
    if (values[i,]$type == values[k,]$type) {
      ## i je val, k je val, vzames ta nižjega
      if ((values[i,]$type == "0") && (values[k,]$type == "0")) {
        ##  message("dve dolini")
        if (values[i,]$amplitude <= values[k,]$amplitude) {
          removals  <- c(removals, k)
        } else {
          removals  <- c(removals, i)
        }
      } else if ((values[i,]$type == "1") && (values[k,]$type == "1")) {
        ## i je peak, k je peak, vzameš ta višjega
        ## message("dva vrha")
        if (values[i,]$amplitude >= values[k,]$amplitude) {
          removals  <- c(removals, k)
        } else {
          removals  <- c(removals, i)
        }
      }
    }
  }
  return(removals)
}

## this should be run multiple times to remove sequences until we are happy
remove.duplicates <- function(values) {
  success <- FALSE
  while (!success) {
    # do something
    duplicates  <- get_duplicates(values)
    if (length(duplicates) > 0) {
      values <- values[-duplicates ,]
    }
    # check for success
    success <-  length(duplicates) == 0
  }
  return(values)
}

dpid_rw  <- function(dp, min_rw_width = 200, return.plot = FALSE, smooth = FALSE, span = 0.01) {
  if (smooth == FALSE) {
    pk <- find_peaks(dp$data$amplitude, m = min_rw_width / 2)
    val  <- find_peaks(-dp$data$amplitude, m = min_rw_width / 2)
    values  <- rbind(data.frame(value = pk, type = "1"),
                     data.frame(value = val, type = "0"))
    values  <- values[order(values$value),]
    values$amplitude <- dp$data$amplitude[values$value]
  } else {
    y.smooth <- stats::loess(amplitude ~ position, data=dp$data, span=span)$fitted
    pk <- find_peaks(y.smooth, m = min_rw_width / 2)
    val  <- find_peaks(-y.smooth, m = min_rw_width / 2)
    values  <- rbind(data.frame(value = pk, type = "1"),
                     data.frame(value = val, type = "0"))
    values  <- values[order(values$value),]
    values$amplitude <- y.smooth[values$value]
  }
  values2 <- remove.duplicates(values)
  removed <- values[!(values$value %in% values2$value),]
  if (return.plot == TRUE) {
    graphics::par(mfrow=c(1,1))
    if (smooth == FALSE) {
      graphics::plot(x=dp$data$position,y=dp$data$amplitude, type = "l",
                     xlab = paste0("Drilling depth [", dp$footer$xUnit[1], "]"),
                     ylab = paste0("Resistograph density [", dp$footer$yUnit[1], "]"),
                     main = paste0("Density profile ID: ",dp$footer$ID))
      graphics::points(x = dp$data$position[pk],
                       y = dp$data$amplitude[pk], col = "blue", pch = 16)
      graphics::points(x = dp$data$position[val],
                       y = dp$data$amplitude[val], col = "green", pch = 16)
      graphics::points(x = dp$data$position[removed$value],
                       y = removed$amplitude, col = "red", cex = 2, pch = 16)
    } else {
      graphics::plot(x=dp$data$position,y=y.smooth, type = "l",
                     xlab = paste0("Drilling depth [", dp$footer$xUnit[1], "]"),
                     ylab = paste0("Resistograph density [", dp$footer$yUnit[1], "]"),
                     main = paste0("Density profile ID: ",dp$footer$ID))
      graphics::points(x = dp$data$position[pk],
                       y = y.smooth[pk], col = "blue", pch = 16)
      graphics::points(x = dp$data$position[val],
                       y = y.smooth[val], col = "green", pch = 16)
      graphics::points(x = dp$data$position[removed$value],
                       y = removed$amplitude, col = "red", cex = 2, pch = 16)
    }
    p <- grDevices::recordPlot()
    return(p)
  }
  return(values2$value)
}
