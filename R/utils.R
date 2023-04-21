#' Convert concentration from log(molar) to nanomolar units
#'
#' @param conc_logmolar Concentration in log(molar) units
#'
#' @return A number: the concentration in nM
#'
logM_to_nM <- function(conc_logmolar){
  return(10^conc_logmolar*1e9)
}

#' Convert concentration from nanomolar to log(molar) units
#'
#' @param conc_nM Concentration in nanomolar units
#'
#' @return A number: the concentration in log(molar)
#'
nM_to_logM <- function(conc_nM){
  return(log10(conc_nM/1e9))
}

#' Get the length of the longest string in a vector of strings
#'
#' @param strings A vector of strings
#'
#' @return Length of the longest string in the vector
#'
longest <- function(strings){
  lengths <- purrr::map(strings, nchar)
  lengths[[which.max(lengths)]]
}
