#' Summarize model parameters for all combinations of treatments and targets
#'
#' @param data A dataframe containing dose-response data for one or more
#'   combinations of treatments and targets. The dataframe should contain the
#'   following columns:
#'
#' * "response_norm" (default) or other activity column name from `activity_col`
#' * log_dose: dose of treatment in log(molar) units
#' @inheritParams get_drda
#' @return A summary dataframe containing one row for each combination of
#'   treatment and target in `data` and containing the following columns:
#'
#' * EC50_nM: the dose at which 50% of the maximum response is achieved, whether
#'   the response is positive or negative.
#' * IC50 nM: the dose at which the response is exactly 50.
#' * EC75_nM: the dose at which 75% of the maximum response is achieved, whether
#'   the response is positive or negative.
#' * hill_slope: the `eta` parameter from [drda::drda()]. The rate of growth of
#'   the model, unsigned (always positive).
#' @export
summarize_models <- function(data, activity_col="response_norm"){
  data |> dplyr::group_by(.data$treatment, .data$target) |>
  dplyr::summarize(
    # kind of silly to be fitting the whole model multiple times. fine for now
    EC50_nM = data |> filter_trt_tgt(.data$treatment, .data$target) |>
      get_drda(activity_col) |> get_EC_nM(50),
    IC50_nM = data |> filter_trt_tgt(.data$treatment, .data$target) |>
      get_drda(activity_col) |> get_IC_nM(50),
    # get_EC_nM() should correctly take 75 to give EC75, unlike cursed ED.drc
    EC75_nM = data |> filter_trt_tgt(.data$treatment, .data$target) |>
      get_drda(activity_col) |> get_EC_nM(75),
    hill_slope = data |> filter_trt_tgt(.data$treatment, .data$target) |>
      get_drda(activity_col) |> get_hill_slope()
  )
}
