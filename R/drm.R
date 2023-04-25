#' Get a dose-response model object for the activity of a treatment on a target
#'
#' `get_drm()` runs [drc::drm()] to generate a 4-parameter logistic
#' dose-response model for the effect of a given treatment on a target. It takes
#' a dataframe that can include data for more than just the desired combination
#' of treatment and target. The dataframe is filtered to contain only data for
#' the desired combination before the model is fit.
#'
#' @param data A dataframe containing the following columns:
#' * "treatment" (default) or other treatment column name (see `trt_colname`)
#' * "target" (default) or other target column name (see `tgt_colname`)
#' * "activity" (default) or other activity column name (see `activity_colname`)
#' * conc_logM: concentration of treatment in log(molar) units
#' @param trt Name of the treatment to use in the model
#' @param tgt Name of the target to use in the model
#' @param trt_colname Name of the column containing treatments.
#'     Default is "treatment". Typical alternatives might include e.g. "drug",
#'     "additive", "supplement".
#' @param tgt_colname Name of the column containing targets.
#'     Default is "target". Typical alternatives might include e.g. "mutant",
#'     "cell_line", "species".
#' @param activity_colname Name of the column containing activity of treatment.
#'     Default is "activity". Typical alternatives might include e.g.
#'     "percent_inhibition", "viability", "growth".
#'
#' @return A dose-response model object of class `drc`.
#' @export
#'
get_drm <- function(data, trt, tgt,
                    trt_colname="treatment",
                    tgt_colname="target",
                    activity_colname="activity"
                    ){
  data_subset <- data |>
    # unclear why you have to wrap embrace in get() but apparently you do
    dplyr::filter(get({{ trt_colname }}) == trt,
                  get({{ tgt_colname }}) == tgt)
  # 4-param logistic model on pre-log-transformed data
  return(drc::drm(activity~conc_logM, data = data_subset, fct = drc::L.4()))
}
