#' Split a data frame by row numbers, return a list of data frames
#'
#' Found somewhere on StackExchange, posibly on https://
#' stackoverflow.com/questions/16357962/r-split-numeric-vector-at-
#' position.
#'
#' @param x A data frame.
#' @param pos A vector of row number where to split the data frame.A data frame.
#'   embedded ("return.fail = FALSE").
splitAt <-function(x, pos) {pos <- c(1L, pos, length(x) + 1L); Map(function(x, i, j) x[i:j], list(x), utils::head(pos, -1L), utils::tail(pos, -1L) - 1L)}
