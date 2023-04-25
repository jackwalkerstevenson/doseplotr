#' Get a dose-response model object for the activity of a treatment on a target
#'
#' `get_drm()` runs [drc::drm()] to generate a 4-parameter logistic
#' dose-response model for the effect of a given treatment on a target. It takes
#' a dataframe that can include data for more than just the desired combination
#' of treatment and target. The dataframe is filtered to contain only data for
#' the desired combination before the model is fit.
#'
#' @param data A dataframe containing the following columns:
#' * treatment
#' * target
#' * activity: the activity of a treatment on a target, e.g. percent inhibition
#' * conc_logM: concentration of treatment in log(molar) units
#' @param trt Name of the treatment to use in the model
#' @param tgt Name of the target to use in the model
#'
#' @return A dose-response model object of class `drc`.
#' @export
#'
get_drm <- function(data, trt, tgt){
  data_subset <- data |>
    dplyr::filter(.data$treatment == trt, .data$target == tgt)
  # 4-param logistic model on pre-log-transformed data
  return(drc::drm(activity~conc_logM, data = data_subset, fct = drc::L.4()))
}
