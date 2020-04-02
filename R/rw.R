
dprings  <- function(dp, pps = 200, threshold.sd = 0,
                     return.plot = FALSE, smooth = FALSE, span = 0.01) {
  if (smooth == FALSE) {
    cutoff <- mean(dp$data$amplitude, na.rm = TRUE) +
      (threshold.sd * sd(dp$data$amplitude, na.rm = TRUE))
    pk <- find_peaks(dp$data$amplitude, m = pps / 2)
    ## delete all peaks below threshold, valleys will be removed by remove_duplicates()
    pk <- pk[dp$data$amplitude[pk] > cutoff]
    val  <- find_peaks(-dp$data$amplitude, m = pps / 2)
    values  <- rbind(data.frame(value = pk, type = "peak"),
                     data.frame(value = val, type = "valley"))
    values  <- values[order(values$value),]
    values$amplitude <- dp$data$amplitude[values$value]
  } else {
    y.smooth <- stats::loess(amplitude ~ position, data=dp$data, span=span)$fitted
    cutoff <- mean(y.smooth, na.rm = TRUE) +
      (threshold.sd * sd(y.smooth, na.rm = TRUE))
    pk <- find_peaks(y.smooth, m = min_rw_width / 2)
    ## delete all peaks below threshold, valleys will be removed by remove_duplicates()
    pk <- pk[y.smooth[pk] > cutoff]
    val  <- find_peaks(-y.smooth, m = min_rw_width / 2)
    values  <- rbind(data.frame(value = pk, type = "peak"),
                     data.frame(value = val, type = "valley"))
    values  <- values[order(values$value),]
    values$amplitude <- y.smooth[values$value]
  }
  values2 <- remove_duplicates(values)
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
      graphics::abline(h = cutoff, col = "black", lty = 2)
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
      graphics::abline(h = cutoff, col = "black", lty = 2)
    }
    p <- grDevices::recordPlot()
    return(p)
  }
  return(values2)
}

## Will take a df of rings and compare them one by one. Peak should
## always be followed by a valley, this function returns those
## duplicating values
get_duplicates  <- function(values){
  removals <- c()
  for (i in 1:(nrow(values) - 1)){
     k <- i + 1
      if (values[i,]$type == values[k,]$type) {
        ## i je val, k je val, vzames ta nižjega
        if ((values[i,]$type == "valley") && (values[k,]$type == "valley")) {
          ##  message("dve dolini")
          if (values[i,]$amplitude <= values[k,]$amplitude) {
            removals  <- c(removals, k)
          } else {
            removals  <- c(removals, i)
          }
        } else if ((values[i,]$type == "peak") && (values[k,]$type == "peak")) {
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

## If there are peak-peak or valley-valley present, the higher peak
## and the lower valley should stay, the rest will be removed. This is
## run sequentially until there are always peak-valley combinations,
## e.g. will run get_duplicates to remove sequences until we are happy
remove_duplicates <- function(values) {
  success <- FALSE
  while (!success) {
     duplicates  <- get_duplicates(values)
     if (length(duplicates) > 0) {
       values <- values[-duplicates ,]
     }
      success <-  length(duplicates) == 0
  }
  return(values)
}

### Written by Stasia Grinberg, licensed under GPL-3
## A simple algorithm to find local maxima/minima in sequential data
## https://github.com/stas-g/findPeaks
## https://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data/164830#164830
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
