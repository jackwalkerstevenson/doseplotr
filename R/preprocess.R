#' Create a conc_logM column from conc_uM or conc_nM columns
#'
#' @param df A dataframe containing either a column called "conc_nM" or a column
#'   called "conc_uM", representing concentration in nanomolar or micromolar
#'   units respectively.
#'
#' @return The same dataframe with an additional column "conc_logM" representing
#'   concentration in log10(molar) units.
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

#' Normalize dose-response data to 0-concentration conditions
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
#' indicated by a concentration of 0. This function assumes that each treatment
#' has its own control data and does not support sharing control data points
#' between treatments. Throws an error if any condition has no controls.
#' @param df A dataframe containing dose-response data. Should contain the
#'   following columns:
#'  - "treatment", the condition being dosed
#'  - "target", the target of the treatment, e.g. a cell line
#'  - "conc_logM", the dose of the treatment in log10(molar) units. Should
#'   include control data for 0 concentration (-Inf in log(molar) units) for
#'   each treatment condition.
#'  - A column of response data, the name of which is indicated by
#'   `.col_to_norm`.
#' @param .col_to_norm The name of the column to normalize within each treatment
#'   condition. Default is "response".
#' @return The input dataframe with the addition of a new column of normalized
#'   data. The name of the new column is the name of `col_to_norm` appended with
#'   an underscore and "norm", e.g. "response_norm".
#' @importFrom rlang :=
#' @export
normalize_dose_response <- function(df, .col_to_norm="response"){
  norm_colname <- glue::glue("{.col_to_norm}_norm")
  # list every combo of treatment and target for which there are any values
  all_conditions <- df |>
    dplyr::group_by(.data[["treatment"]], .data[["target"]]) |>
    dplyr::summarize()
  # calculate mean control (0-conc) responses for treatment/target combos
  ctrl_means <- df |>
    dplyr::group_by(.data[["treatment"]], .data[["target"]]) |>
    # filter for controls: 0 conc = -Inf log(conc)
    dplyr::filter(.data[["conc_logM"]] == -Inf) |>
    dplyr::summarize(ctrl_mean = mean(.data[[.col_to_norm]]),
                     ctrl_replicates = dplyr::n())
  # join ctrl means to conditions to make sure every condition has a control
  conditions_with_ctrls <- dplyr::inner_join(all_conditions, ctrl_means)
  assertthat::assert_that(nrow(conditions_with_ctrls) == nrow(all_conditions),
                          msg="controls not present for all conditions")
  # join ctrl means to original df and normalize to them as 100
  df |> dplyr::left_join(ctrl_means, by = c("treatment", "target")) |>
    dplyr::mutate("{norm_colname}" :=
                    .data[[.col_to_norm]] / .data[["ctrl_mean"]] * 100)
}

#' Preprocess plate-based dose-response data
#'
#' @param df A dataframe containing raw plate data
#' @param treatments Optional vector of treatments to filter for
#' @param targets Optional vector of targets to filter for
#'
#' @return A dataframe containing processed data.
#' @export
preprocess_plate_data <- function(df, treatments, targets){
  # filter for treatments and targets if those variables exist
  df |> make_log_conc()
}