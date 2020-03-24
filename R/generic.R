#' Print method for dpa objects
#'
#' Generic method for printing dpa objects.
#' @export
print.dpa <- function(x, ...) {
  if (!inherits(dpa,"dpa")) {stop("not a dpa object")}
  cat(paste0("\n\nResistance drilling profile ID:\t",
             x$footer$ID, "\n\n",
             "Total length:\t", nrow(x$data), " x ",
             x$footer$xUnit, "\n",
             "Trimmed: ", is.trimmed(x), "\n\n",
             "Data preview: \n"))
  print.data.frame(head(x$data))
  cat("...\n")
}

#' Plot method for dpa objects
#'
#' Generic method for plotting dpa objects.
#' @export
plot.dpa <- function(x, ...) {
  if (!inherits(dpa,"dpa")) {stop("not a dpa object")}
  plot(x=dpa$data$position, y=dpa$data$amplitude, type = "l",
       xlab = paste0("Drilling depth [", dpa$footer$xUnit, "]"),
       ylab= paste0("Resistograph density [", dpa$footer$yUnit, "]"),
       main = paste0("Density profile ID: ",dpa$footer$ID))
}
