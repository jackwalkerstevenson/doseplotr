logmolar_to_nM <- function(conc_logmolar){
  return(10^conc_logmolar*1e9)
}

nM_to_logmolar <- function(conc_nM){
  return(log10(conc_nM/1e9))
}
