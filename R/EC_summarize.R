#' Summarize model parameters for all combinations of treatments and targets
#'
#' @param data A dataframe containing dose-response data for one or more
#'   combinations of treatments and targets. The dataframe should contain the
#'   following columns:
#'
#' * "response_norm" (default) or other activity column name from `response_col`
#' * log_dose: dose of treatment in log(molar) units
#' @inheritParams get_drda
#' @return A summary dataframe containing one row for each combination of
#'   treatment and target in `data` and containing the following columns:
#'
#' * "model": the actual model object
#' * "IC50_nM": the dose at which the response is exactly 50.
#' * "EC50_nM": the dose at which 50% of the maximum response is achieved, whether
#'   the response is positive or negative.
#' * "EC75_nM": the dose at which 75% of the maximum response is achieved, whether
#'   the response is positive or negative.
#' * "hill_slope": the `eta` parameter from [drda::drda()]. The rate of growth of
#'   the model, unsigned (always positive).
#' * "low_dose_asymptote": the value of the function as dose -> -Inf.
#' * "high_dose_asymptote": the value of the function as dose -> Inf.
#' @export
summarize_models <- function(data, response_col="response_norm"){
  # todo: change default to "response"
  model <- NULL # suppress global variable error
  data |> dplyr::group_by(.data$treatment, .data$target) |>
    dplyr::summarize(
      model = list(data |> filter_trt_tgt(.data$treatment, .data$target) |>
        get_drda(response_col)),
      IC50_nM =
        model |> purrr::map_dbl(\(x){get_IC_nM(x, 50)}),
      EC50_nM =
        model |> purrr::map_dbl(\(x){get_EC_nM(x, 50)}),
      EC75_nM = # get_EC_nM() takes 75 to give EC75, unlike cursed ED.drc
        model |> purrr::map_dbl(\(x){get_EC_nM(x, 75)}),
      hill_slope =
        model |> purrr::map_dbl(\(x){get_hill_slope(x)}),
      low_dose_asymptote =
        model |> purrr::map_dbl(\(x){get_low_dose_asymptote(x)}),
      high_dose_asymptote =
        model |> purrr::map_dbl(\(x){get_high_dose_asymptote(x)})
    )
}
