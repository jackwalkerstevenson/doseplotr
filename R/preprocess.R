#' Normalize dose-response data to 0-concentration conditions
#'
#' `normalize_dose_response()` independently normalizes each treatment/target
#' combination in a dataset of dose-response data. It is intended to be used for
#' situations such as plate-based assays where one plate contains replicate dose
#' series for e.g. one treatment compound on two different target cell lines, so
#' that is it appropriate to normalize the data from each cell line to the mean
#' of the untreated wells of that cell line on that plate (but not untreated
#' wells from the same cell line on other plates).
#'
#' Data from each treatment/target combination is normalized to the average of
#' the control data points for that treatment/target combination, which are
#' indicated by a concentration of 0. This function assumes that each treatment
#' has its own control data and does not support sharing control data points
#' between treatments.
#' @param df A dataframe containing dose-response data. Should contain the
#'   following columns:
#'  - "treatment", the condition being dosed
#'  - "conc_logM", the dose of the treatment in log10(molar) units. Should
#'   include control data for 0 concentration (-Inf in log(molar) units) for
#'   each treatment condition.
#'  - A column of response data, the name of which is indicated by
#'   `col_to_norm`.
#' @param col_to_norm The name of the column to normalize within each treatment
#'   condition. Default is "response".
#' @return The input dataframe with the addition of a new column of normalized
#'   data. The name of the new column is the name of `col_to_norm` appended with
#'   an underscore and "norm", e.g. "response_norm".
#' @export
#' @examples
normalize_dose_response <- function(df, col_to_norm="response"){
# todo: check that there are >0 -Inf values for each treatnent to normalize to
  # make a list of every combo of treatment and target
  # then check that each has >0 -Inf values
  # calculate mean control (0-conc) responses for treatment/target combinations
  ctrl_means <- df |> dplyr::group_by(treatment, target) |>
    dplyr::filter(conc_logM == -Inf) |> # control reads have 0 conc = -Inf log(conc)
    dplyr::summarize(ctrl_mean = mean(.data[[col_to_norm]]),
                     ctrl_replicates = dplyr::n())
  # then mutate original df to normalize to means
  # I think there must be a way to use a summary value calculated from the same
  # df without taking the summary df and filtering it and assuming one value
  # df |> dplyr::mutate(response_norm = .data[[col_to_norm]] / ctrl_means)
}

get_trt_tgt_summary <- function(df, value_col, )

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
