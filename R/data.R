#' SelectScreen data for ponatinib and dasatinib on ABL1 and ABL1 T315I
#'
#' In vitro biochemical kinase activity data produced by Thermo SelectScreen for
#' 10-point dose-response curves of the clinical ABL1 kinase inhibitors
#' ponatinib and dasatinib against both wild-type ABL1 kinase and the ABL1 T315I
#' mutant. Imported from a Thermo file with `import_selectscreen()`.
#'
#' @format ## `selectscreen_ponatinib_dasatinib` A data frame with 80 rows and 6
#'   columns:
#' \describe{
#'   \item{treatment}{Name of treatment compound}
#'   \item{target}{Name of target protein}
#'   \item{Compound_Conc_nM}{Concentration of treatment in nanomolar units}
#'   \item{pct_inhibition}{Observed percent inhibition of kinase activity}
#'   \item{conc_logM}{Concentration of treatment in log10(molar) units}
#'   \item{activity}{Kinase activity calculated by 100-pct_inhibition}
#' }
"selectscreen_ponatinib_dasatinib"
