#' Convert concentration from log(molar) to nanomolar units
#'
#' @param conc_logmolar Concentration in log(molar) units
#'
#' @return A number: the concentration in nM
#'
#' @examples
#' logmolar_to_nM(-9)
logM_to_nM <- function(conc_logmolar){
  return(10^conc_logmolar*1e9)
}

#' Convert concentration from nanomolar to log(molar) units
#'
#' @param conc_nM Concentration in nanomolar units
#'
#' @return A number: the concentration in log(molar)
#'
#' @examples
#' nM_to_logM(1)
nM_to_logM <- function(conc_nM){
  return(log10(conc_nM/1e9))
}
