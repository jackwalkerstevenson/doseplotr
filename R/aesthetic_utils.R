#' Get a pleasing range of the viridis color scale for several discrete values
#'
#' `viridis_range()` returns a range of the viridis default color scale meant
#' for coloring some number of discrete values without using more of the scale
#' than necessary. It is intended to solve the problem where very small numbers
#' of discrete values (e.g. 2 or 3) are by default displayed at the ends of
#' color scale, leading to jarring results like one value being dark purple and
#' the other bright yellow. The output of `viridis_range()` is intended to be
#' used as the "begin" and "end" arguments for `ggplot2::scale_color_viridis`.
#'
#' @param n The number of values that need to fit in the range
#' @param reverse Boolean for whether to return the higher number in the range
#'   first (the default).
#'
#' @return A vector of two numbers: the beginning and end of the range.
#'
viridis_range <- function(n, reverse = TRUE){
  range <- ifelse(n < 6, 0.2*n, 1)
  high <- 0.5 + range / 2
  low <- 0.5 - range / 2
  if(reverse){
    return(c(high, low))
  }
  else{
    return(c(low, high))
  }
}
