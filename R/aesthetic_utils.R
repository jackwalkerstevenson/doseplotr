#' Get a range and option for the viridis scale depending on number of values
#'
#' `viridis_range()` returns a color scale and range for the viridis color scale
#' package meant for coloring some number of discrete values without using more
#' of the scale than necessary. It is intended to solve the problem where very
#' small numbers of discrete values (e.g. 2 or 3) are by default displayed at
#' the ends of color scale, leading to jarring results like one value being dark
#' purple and the other bright yellow. The output of `viridis_range()` is
#' intended to be used as the `begin`, `end` and `option` arguments for
#' `ggplot2::scale_color_viridis`.
#'
#' @param n The number of values that need to fit in the range
#' @param reverse Boolean for whether to return the higher number in the range
#'   first (the default).
#'
#' @return A list of two numbers (the beginning and end of the range) and a
#'   string (the "option" parameter for the color scale), which will be either
#'   "viridis" for n<7 or "turbo" for n>7.
#' @export
viridis_range <- function(n, reverse = TRUE){
  if(n<7){
    option <- "viridis"
    range <- ifelse(n<6, 0.18*n, 1)
  }
  else{
    option <- "turbo"
    range <- ifelse(n<10, 0.1*n, 1)
  }
  high <- 0.5 + range / 2
  low <- 0.5 - range / 2
  if(reverse){
    return(list(high, low, option))}
  else{
    return(c(low, high, option))}
}
