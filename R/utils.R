#' Convert concentration from log(molar) to nanomolar units
#'
#' @param conc_logmolar Concentration in log(molar) units
#' @return A number: the concentration in nM
#' @export
logM_to_nM <- function(conc_logmolar){
  return(10^conc_logmolar*1e9)
}

#' Convert concentration from nanomolar to log(molar) units
#'
#' @param conc_nM Concentration in nanomolar units
#' @return A number: the concentration in log(molar)
#' @export
nM_to_logM <- function(conc_nM){
  return(log10(conc_nM/1e9))
}

#' Get the length of the longest string in a vector of strings
#'
#' @param strings A vector of strings
#' @return Length of the longest string in the vector
longest <- function(strings){
  lengths <- purrr::map(strings, nchar)
  lengths[[which.max(lengths)]]
}

#' Get a string representation of current system time
#'
#' @return A string representing current system time in YYYY-MM-DDTHHMMSS
#'   format, which is a filesystem-friendly adaptation of ISO 8601 without
#'   colons. For example, 8:17 pm and 40 seconds on July 20, 1969 is represented
#'   as 1969-06-20T201740.
#' @export
#'
#' @examples
#' get_timestamp()
get_timestamp <- function(){
  format(Sys.time(), "%Y-%m-%dT%H%M%S")
}

#' Replace NA values in a vector with Inf or -Inf
#'
#' This method is intended to prepare vectors for use by [get_drda_fixed()] as
#' the arguments for [drda::drda()]'s `lower_bound` and `upper_bound`
#' parameters.
#'
#' @param params A vector of values (generally representing the four parameters
#'   of the logistic function in [drda::drda()]).
#' @param lower Whether to generate Inf or -Inf values for undefined parameters.
#'   Positive is default.
#' @return A vector of the same length as the original vector. NA values in the
#'   input vector are replaced with Inf or -Inf.
param_bounds <- function(params, lower=FALSE){
  substitute <- if(lower) -Inf else Inf
  params |>
    # replace NA values with Inf or -Inf for upper or lower bounds
    purrr::map_vec(\(x) if(is.na(x)) substitute else x) |>
    # with identical upper and lower bounds, drda makes unplottable models
    # so provide a tiny 1e-4 range and then it's happy
    # sorry this is a hack
    purrr::map_vec(\(x) if(lower) x - 1e-4*abs(x) else x)
}
