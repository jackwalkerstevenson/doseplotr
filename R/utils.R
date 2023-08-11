#' Get a variable if a boolean is TRUE, or else a backup option
#' @param var Variable to return if `bool` is TRUE
#' @param bool Whether to return `var` or `otherwise`
#' @param otherwise What to return if `bool` is FALSE. Default is NULL.
#' @return `var`, if `bool` is TRUE, else `otherwise`
#' @export
get_if <- function(var, bool, otherwise = NULL){
  if(bool) var else otherwise}

#' Get the value of a name in a named vector if present and desired
#'
#' This function is intended for setting the "display name" of a group in a plot
#' from its "data name", its name in the underlying dataframe.
#' @param name The data name to be renamed
#' @param display_names Named vector of the form c("data name" = "display name")
#' @param rename Whether to seek to rename at all. This argument is intended for
#'   use with a global parameter that determines whether display names will be
#'   applied across all plots.
#' @return The name to use for the group in the plot. Will be either the
#'   original name or a new display name depending on arguments.
#' @export
#' @examples
#' example_names <- c("foo" = "Display Name for Foo",
#'                    "bar" = "Display Name for Bar")
#' get_display_name("foo", example_names, TRUE)
#' get_display_name("foo", example_names, FALSE)
#' get_display_name("baz", example_names, TRUE)
get_display_name <- function(name, display_names, rename){
  if(rename & name %in% names(display_names)) display_names[name] else name
}

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
#' @export
longest <- function(strings){
  lengths <- purrr::map(strings, nchar)
  lengths[[which.max(lengths)]]
}

#' Get a string representation of current local system time
#'
#' @return A string representing current local system time in YYYY-MM-DDTHHMMSS
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
