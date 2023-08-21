#' Create a log_dose column from dose_uM or dose_nM columns
#'
#' `make_log_dose()` takes a dataframe that contains a column called "dose_uM"
#' or "dose_nM" and creates a column called log_dose". If such a column is
#' already present, it does nothing.
#' @param df A dataframe containing either a column called "dose_nM" or a column
#'   called "dose_uM", representing dose in nanomolar or micromolar units
#'   respectively.
#'
#' @return The same dataframe with an additional column "log_dose" representing
#'   dose in log10(molar) units, or just the original dataframe if "log_dose" is
#'   already present.
#' @export
#' @examples
#' df <- data.frame("treatment" = c("foo", "bar", "baz"),
#'                  "dose_nM" = c(1, 10, 100))
#' make_log_dose(df)
make_log_dose <- function(df){
  if("log_dose" %in% colnames(df)) return(df)
  tryCatch({ # try to convert from dose_uM
    df |> dplyr::mutate(log_dose = log10(.data$dose_uM/1e6))},
    error = function(e){ # if no dose_uM, try to convert from dose_nM
      df |>  dplyr::mutate(log_dose = log10(.data$dose_nM/1e9))})}

#' Normalize dose-response data to 0-dose conditions
#'
#' `normalize_dose_response()` independently normalizes each treatment/target
#' combination in a dataset of dose-response data to its own set of control
#' values. It is intended to be used for situations such as plate-based assays
#' where each plate contains replicate dose series that include control wells,
#' so that is it appropriate to normalize the data from each treatment to the
#' mean of the untreated wells of the same condition on the same plate.
#'
#' Data from each treatment/target combination is normalized to the average of
#' the control data points for that treatment/target combination, which are
#' indicated by a dose of 0. This function assumes that each treatment has its
#' own control data and does not support sharing control data points between
#' treatments. Throws an error if any condition has no controls.
#' @param df A dataframe containing dose-response data. Should contain the
#'   following columns:
#'  - "treatment", the condition being dosed
#'  - "target", the target of the treatment, e.g. a cell line
#'  - "log_dose", the dose of the treatment in log10(molar) units. Should
#'   include control data for 0 dose (-Inf in log(molar) units) for each
#'   treatment condition.
#'  - "response", the response observed at the given dose.
#' @return The input dataframe with the addition of a new column of normalized
#'   data. The name of the new column is "response_norm". If a column called
#'   "response_norm" already exists, the original dataframe is returned.
#' @export
normalize_dose_response <- function(df){
  if("response_norm" %in% colnames(df)) return(df)
  # list every combo of treatment and target for which there are any values
  all_conditions <- df |>
    dplyr::group_by(.data[["treatment"]], .data[["target"]]) |>
    dplyr::summarize()
  # calculate mean control (0-dose) responses for treatment/target combos
  ctrl_means <- df |>
    dplyr::group_by(.data[["treatment"]], .data[["target"]]) |>
    # filter for controls: 0 dose = -Inf log(dose)
    dplyr::filter(.data[["log_dose"]] == -Inf) |>
    dplyr::summarize(ctrl_mean = mean(.data[["response"]]),
                     ctrl_replicates = dplyr::n())
  # join ctrl means to conditions to make sure every condition has a control
  conditions_with_ctrls <- dplyr::inner_join(all_conditions, ctrl_means)
  assertthat::assert_that(nrow(conditions_with_ctrls) == nrow(all_conditions),
                          msg="controls not present for all conditions")
  # join ctrl means to original df and normalize to them as 100
  df |> dplyr::left_join(ctrl_means, by = c("treatment", "target")) |>
    dplyr::mutate(response_norm =
                    .data[["response"]] / .data[["ctrl_mean"]] * 100)
}

#' Preprocess plate-based dose-response data.
#'
#' `preprocess_plate_data()` prepares raw dose-response data for analysis. It
#' converts nonstandard column names to standard ones, converts dose to log
#' units and normalizes response values for each treatment/target combination to
#' their own controls.
#' @param df A dataframe, such as the output of `import_plater_CSVs()`,
#'   containing raw plate data. Should contain the following columns:
#'   - "treatment" or "compound" (convert to "treatment")
#'   - "target"
#'   - "dose_nM" or "dose_uM" or "conc_nM" or "conc_uM" (convert to "log_dose")
#'   - "response" or "readout" (convert to "response_norm")
#' @return A dataframe containing processed data.
#' @export
#' @examples
#' df <- data.frame("compound" = c("foo", "foo", "foo", "foo"),
#'                  "target" = c("subject", "subject", "subject", "subject"),
#'                  "conc_nM" = c(0, 1, 10, 100),
#'                  "readout" = c(100, 100, 90, 2))
#' preprocess_plate_data(df)
preprocess_plate_data <- function(df){
  # list of columns to rename if present
  renames <- c(
    treatment = "compound",
    dose_nM = "conc_nM",
    dose_uM = "conc_uM",
    response = "readout"
  )
  df |>
    dplyr::rename(dplyr::any_of(renames)) |>
    dplyr::filter(.data[["treatment"]] != "N/A") |>  # drop empty wells
    make_log_dose() |>
    normalize_dose_response()
    # dplyr::filter(.data[["log_dose"]] != -Inf) # drop 0-dose values after normalizing)
}
