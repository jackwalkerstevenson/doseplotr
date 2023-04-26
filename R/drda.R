#' Filter data to include only a given treatment and target
#'
#' @param data A dataframe containing the following columns:
#' * "treatment" (default) or other treatment column name (see `trt_col`)
#' * "target" (default) or other target column name (see `tgt_col`)
#' @param trt Name of the treatment to use in the model
#' @param tgt Name of the target to use in the model
#' @param trt_col Name of the column containing treatments. Default is
#'   "treatment". Typical alternatives might include e.g. "drug", "additive",
#'   "supplement".
#' @param tgt_col Name of the column containing targets. Default is "target".
#'   Typical alternatives might include e.g. "mutant", "cell_line", "species".
#'
#' @return A dataframe containing only rows filtered for the desired treatment
#'   and target.
#' @export
#'
#' @examples
#' df <- data.frame(treatment = c("foo", "foo", "foo", "bar", "baz"),
#'                  target = c("apple", "orange", "banana", "apple", "orange"))
#' filter_trt_tgt(df, "foo", "orange")
filter_trt_tgt <- function(data, trt, tgt,
                        trt_col="treatment",
                        tgt_col="target"){
  assertthat::assert_that(assertthat::has_name(data, trt_col),
                          msg = glue::glue("column {trt_col} not found"))
  assertthat::assert_that(assertthat::has_name(data, tgt_col),
                          msg = glue::glue("column {tgt_col} not found"))
  # unclear why you have to wrap embrace in get() but apparently you do
  return(dplyr::filter(data,
                       get({{ trt_col }}) == trt,
                       get({{ tgt_col }}) == tgt))
}

#' Get a drda dose-response model without bounds on parameters
#'
#' `get_drda_unbounded()` runs [drda::drda()] to generate a 4-parameter logistic
#' dose-response model for the effect of a given treatment on a target. It
#' places no bounds on the parameters. It is intended for internal use in
#' generating bounded models.
#'
#' @param data A dataframe containing the following columns:
#' * "activity" (default) or other activity column name (see `activity_col`)
#' * conc_logM: concentration of treatment in log(molar) units
#' @param activity_col Name of the column containing activity of treatment.
#'   Default is "activity". Typical alternatives might include e.g.
#'   "percent_inhibition", "viability", "growth".
#' @return A dose-response model object of class `drda`.
#'
get_drda_unbounded <- function(data, activity_col="activity"){
  assertthat::assert_that(assertthat::has_name(data, activity_col),
                          msg = glue::glue("column {activity_col} not found"))
  # 4-param logistic model on pre-log-transformed data
  return(drda::drda(get({{ activity_col }})~conc_logM, data = data))
}

#' Get a drda dose-response model for the activity of a treatment on a target
#'
#' `get_drda()` runs [get_drda_unbounded()] to generate a 4-parameter logistic
#' dose-response model for the effect of a given treatment on a target. It
#' attempts to bound the parameters of the model to avoid physically unrealistic
#' fits.
#'
#' If the height of the model is negative (if activity decreases at higher
#' dose), the height is bounded at the height of the unbounded 0-concentration
#' asymptote to avoid producing models with infinite-concentration asymptotes
#' far below 0. Note that after the model is refit with this bound, the height
#' of the 0-concentration asymptote may decrease slightly, so the
#' maximum-concentration asymptote may still end up slightly below 0.
#'
#' If the height of the model is positive (if activity increases at higher
#' dose), the height is bounded at 100 minus the height of the unbounded
#' 0-concentration asymptote to avoid producing models with
#' infinite-concentration asymptotes far above 100. This is only valid for
#' activity units like percent inhibition that should not realistically exceed
#' 100. Note that after the model is refit with this bound, the height of the
#' 0-concentration asymptote may increase slightly, so the maximum-concentration
#' asymptote may still end up slightly above 100.
#'
#' @inheritParams get_drda_unbounded
#' @return A dose-response model object of class `drda`.
#' @export
#'
get_drda <- function(data, activity_col="activity", ...){
  # first get coefficients of a model with no bounds on parameters
  unbounded_coeff <- coefficients(get_drda_unbounded(data,
                                                     activity_col))
  unbounded_alpha <- unbounded_coeff["alpha"] # 0 conc asymptote
  unbounded_delta <- unbounded_coeff["delta"] # height of curve
  # bound curve height depending on whether the model increases or decreases
  if(unbounded_delta < 0){
    # bound delta above -alpha so curves can't go way below 0
    return(drda::drda(get({{ activity_col }})~conc_logM, data = data,
                      lower_bound = c(-Inf, -unbounded_alpha, -Inf, -Inf), ...))
  }
  else{
    # bound delta below 100-alpha so curves can't go way above 100
    return(drda::drda(get({{ activity_col }})~conc_logM, data = data,
                      upper_bound = c(Inf, 100-unbounded_alpha, Inf, Inf), ...))
  }
}
