#' Convert conc_uM or conc_nM columns to log(molar)
#'
#' @param df A dataframe containing either a column called "conc_nM" or a column
#'   called "conc_uM", representing concentration in nanomolar or micromolar
#'   units respectively.
#'
#' @return The same dataframe with an additional column "conc_logM" representing
#'   concentration in log(molar) units.
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' df <- data.frame("treatment" = c("foo", "bar", "baz"),
#'                  "conc_nM" = c(1, 10, 100))
#' make_log_conc(df)
make_log_conc <- function(df){
  tryCatch({ # try to convert from conc_uM
    df |> dplyr::mutate(conc_logM = log10(.data$conc_uM/1e6))},
    error = function(e){ # if no conc_uM, try to convert from conc_nM
      df |>  dplyr::mutate(conc_logM = log10(.data$conc_nM/1e9))})}
