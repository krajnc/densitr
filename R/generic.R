
is.trimmed  <- function(x){
  answer  <- ""
  # compare total length from footer
  if (x$data$position[1] == 1) {
    ## start not trimmed
    answer <- paste0(answer, "start not trimmed")
  } else {
    ## start trimmed
    answer <- paste0(answer, "start trimmed")
  }
  unit  <- eval(str2expression(
    strsplit(x$footer$xUnit, split = " ")[[1]][1]))
  dlen <- floor(x$data$position[length(x$data$position)] * unit)
  if (dlen == strsplit(x$footer$Length, split = " ")[[1]][1]) {
    ## end not trimmed
    answer <- paste0(answer, ", end not trimmed")
  } else {
    ## end trimmed
    answer <- paste0(answer, ", end trimmed")
  }
  return(answer)
}

print.dpa <- function(x, ...) {
  if (!inherits(x,"dpa")) {stop("not a dpa object")}
  cat(paste0("\n\nResistance drilling profile ID:\t",
             x$footer$ID, "\n\n",
             "Total length:\t", nrow(x$data), " x ",
             x$footer$xUnit, "\n",
             "Trimmed: ", is.trimmed(x), "\n\n",
             "Data preview: \n"))
  print.data.frame(head(x$data))
  cat("...\n")
}

plot.dpa <- function(x, ...) {
  if (!inherits(x,"dpa")) {stop("not a dpa object")}
  plot(x=x$data$position, y=x$data$amplitude, type = "l",
       xlab = paste0("Drilling depth [", x$footer$xUnit, "]"),
       ylab= paste0("Resistograph density [", x$footer$yUnit, "]"),
       main = paste0("Density profile ID: ", x$footer$ID))
}
