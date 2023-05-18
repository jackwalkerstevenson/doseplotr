#' Normalize dose-response data to 0-concentration conditions
#'
#' `normalize_dose_response()` independently normalizes each treatment condition
#' in a dataset of dose-response data. Data from each treatment condition is
#' normalized to the average of the control data points for that treatment
#' condition, which are indicated by a concentration of 0. This function assumes
#' that each treatment has its own control data and does not support sharing
#' control data points between treatments.
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
#'
#' @return The input dataframe with the addition of a new column of normalized
#'   data. The name of the new column is the name of `col_to_norm` appended with
#'   an underscore and "norm", e.g. "response_norm".
#' @export
#'
#' @examples
normalize_dose_response <- function(df, col_to_norm="response"){

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

}
