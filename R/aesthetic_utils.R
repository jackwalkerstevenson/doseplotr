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

#' Generate minor breaks for an axis in non-log-transformed units
#'
#' This function generates logistic minor axis breaks to use on an axis that is
#' in original, non-log-transformed units, such that the data being plotted is
#' e.g. "1000" rather than "3".
#' @param min_val The minimum value to plot, or the log floor of same (0.1 and
#'   0.3 will both be converted to 0.1).
#' @param max_val The maximum value to plot, or the log ceiling of same (500 and
#'   1000 will both be converted to 1000).
#' @param log_units Whether the units of the minimum and maximum values are in
#'   log-transformed units. Default is FALSE.
#' @return A vector of minor axis breaks in raw units (not log-transformed).
#' @export
#' @examples
#' minor_breaks(1, 1000)
#' minor_breaks(3, 857)
#' minor_breaks(-12, -4, log = TRUE)
#' minor_breaks(-12.5, -3.8, log = TRUE)
minor_breaks <- function(min_val, max_val, log_units = FALSE){
  # convert min_val and max_val into log units if not already-------------------
  if(log_units){
    min_val <- floor(min_val)
    max_val <- ceiling(max_val)
  } else{
    assertthat::assert_that(min_val > 0 & max_val > 0,
                            msg =
                              glue::glue("Limits must be greater than zero ",
                                         "to generate log-scaled axis breaks. ",
                                         "Did you mean to specify ",
                                         "`log_units = TRUE`?"))
    min_val <- floor(log10(min_val))
    max_val <- ceiling(log10(max_val))
  }
  rep(1:9, max_val - min_val) * # repeat 1-9 for each log interval
    (10^rep(min_val:(max_val - 1), each = 9)) # times appropriate powers of 10
}

#' Generate minor breaks for an axis in log units
#'
#' This function generates logistic minor axis breaks for an axis that uses
#' log-transformed units, such that the data being plotted is e.g. -3 rather
#' than 0.001.
#' @param min_val The minimum value to plot, or log floor of same, in log-transformed units (-8.3 and -8 will both be converted to -8).
#' @param max_val The maximum value to plot, or log ceiling of same, in log-transformed units (-4.5 and -4 will both be converted to -4).
#' @return A vector of minor axis breaks in log-transformed units.
#' @export
#' @examples
#' minor_breaks_log(-12, -4)
#' minor_breaks_log(-12.2, -3.6)
minor_breaks_log <- function(min_val, max_val){
  log10(minor_breaks(min_val, max_val, log_units = TRUE))
}

#' Get a manually-curated general-purpose shape scale for ggplot
#' @return A vector of numbers corresponding to shapes intended for use in
#'   `ggplot2::scale_shape_manual()` for general-purpose shape scales
#' @export
#' @examples
#' shape_scale_default()
shape_scale_default <- function(){
  c(19, # solid circle
    17, # solid triangle
    15, # solid square
    18, # solid diamond
    1, # empty circle
    2, # empty triangle
    0, # empty square
    5, # empty diamond
    6, # empty inverted triangle
    4, # X
    8, # vonnegut
    7, # Xed square
    9, # crossed diamond
    13, # Xed circle
    10, # crossed circle
    12, # crossed square
    3, # plus
    14, # inverted V square
    11 # triangle plus inverted triangle
    )
}
