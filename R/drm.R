#' Get a drc dose-response model for the activity of a treatment on a target
#'
#' `get_drm()` runs [drc::drm()] to generate a 4-parameter logistic
#' dose-response model for the effect of a given treatment on a target.
#'
#' @param data A dataframe containing the following columns:
#' * "activity" (default) or other activity column name (see `activity_col`)
#' * conc_logM: concentration of treatment in log(molar) units
#' @param activity_col Name of the column containing activity of treatment.
#'     Default is "activity". Typical alternatives might include e.g.
#'     "percent_inhibition", "viability", "growth".
#' @param ... Additional parameters to pass on to [drc::drm()]
#' @return A dose-response model object of class `drc`.
#' @export
#'
get_drm <- function(data, activity_col="activity", ...){
  assertthat::assert_that(assertthat::has_name(data, activity_col),
                          msg = glue::glue("column {activity_col} not found"))
  # 4-param logistic model on pre-log-transformed data
  return(drc::drm(get({{ activity_col }})~conc_logM,
                  data = data, fct = drc::L.4()))
}

#' Title Get an EC value with drc for the activity of a treatment on a target
#'
#' `get_EC_logM()` runs [filter_trt_tgt()] and [get_drm()] to get a
#' dose-response model of the effect of conc_logM on activity for the specified
#' treatment and target, then extracts the effective concentration value of the
#' model at the specified threshold using [drc::ED()].
#'
#' @param response_level The response level in percent for which to estimate the
#'   effective concentration. This parameter is used as the `respLev` parameter
#'   of [drc::ED()]. Should be between 0 and 100.
#'
#'   Be careful: this parameter can be confusing when the response to treatment
#'   is a decrease in activity. This is because the response level represents
#'   the percent of the way from whichever asymptote is a lower activity level
#'   to whichever is higher rather than from the zero-dose asymptote to the
#'   infinite-dose asymptote. For example, if activity starts at 100 at 0 dose
#'   and goes down to 0 at infinite dose, then `get_ED_logM(response_level=75)`
#'   will return the concentration that gives an activity of 75, even though you
#'   might naively expect that it would return the concentration that gives an
#'   activity of 25 (75% of the way to the infinite-dose effect). This is
#'   debatably a bug in [drc::ED()].
#'
#'   Be careful: this parameter can also be confusing for data not normalized to
#'   100. For example, if activity e.g. starts at 200 at 0 dose and increases to
#'   500 at infinite dose, then `get_ED_logM(response_level=50)` will return the
#'   concentration that gives an activity of 350 (50% of the way from 200 to
#'   500).
#'
#' @inheritParams filter_trt_tgt
#' @inheritParams get_drm
#'
#' @return The effective concentration value at the desired response level in
#'   log10(molar) units.
#' @export
#'
get_EC_logM <- function(data, trt, tgt, response_level=50,
                   trt_col="treatment",
                   tgt_col="target",
                   activity_col="activity"){
  data <- filter_trt_tgt(data, trt, tgt, trt_col, tgt_col)
  drm <- get_drm(data, activity_col=activity_col)
return(drc::ED(drm, response_level, display=FALSE)[1,1])
}
