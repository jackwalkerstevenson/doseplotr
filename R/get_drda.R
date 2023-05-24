#' Get a drda dose-response model without bounds on parameters
#'
#' `get_drda_helper()` runs [drda::drda()] to generate a 4-parameter logistic
#' dose-response model for the effect of a given treatment on a target. It
#' places no bounds on the parameters by default but allows arguments to be
#' passed to [drda::drda()]. It is intended for internal use in generating
#' bounded models.
#'
#' @param data A dataframe containing dose-response data for only one treatment
#'   on one target. If needed, filter data in advance with [filter_trt_tgt()].
#'   The dataframe should contain the following columns:
#'
#' * "activity" (default) or other activity column name (see `activity_col`)
#' * log_dose: dose of treatment in log10 units
#' @param activity_col Name of the column containing activity of treatment.
#'   Default is "activity". Typical alternatives might include e.g.
#'   "percent_inhibition", "viability", "growth".
#' @param ... Additional arguments to be passed to [drda::drda()], e.g. bounds.
#' @return A dose-response model object of class `drda`.
#'
get_drda_helper <- function(data, activity_col="activity", ...){
  assertthat::assert_that(assertthat::has_name(data, activity_col),
                          msg = glue::glue("column {activity_col} not found"))
  # 4-param logistic model on pre-log-transformed data
  return(drda::drda(get({{ activity_col }})~log_dose, data = data, ...))
}

#' Get a drda dose-response model for the activity of a treatment on a target
#'
#' `get_drda()` runs [get_drda_helper()] to generate a 4-parameter logistic
#' dose-response model for the effect of a given treatment on a target. It
#' attempts to bound the parameters of the model to avoid physically unrealistic
#' fits.
#'
#' If the height of the model is negative (if activity decreases at higher
#' dose), the height is bounded at the height of the unbounded 0-dose asymptote
#' to avoid producing models with infinite-dose asymptotes far below 0. Note
#' that after the model is refit with this bound, the height of the 0-dose
#' asymptote may decrease slightly, so the infinite-dose asymptote may still end
#' up slightly below 0.
#'
#' If the height of the model is positive (if activity increases at higher
#' dose), the height is bounded at 100 minus the height of the unbounded 0-dose
#' asymptote to avoid producing models with infinite-dose asymptotes far above
#' 100. This is only valid for activity units like percent inhibition that
#' should not realistically exceed 100. Note that after the model is refit with
#' this bound, the height of the 0-dose asymptote may increase slightly, so the
#' maximum-dose asymptote may still end up slightly above 100.
#'
#' @inherit get_drda_helper params return
#' @export
#'
get_drda <- function(data, activity_col="activity"){
  # first get coefficients of a model with no bounds on parameters
  unbounded_coeffs <- stats::coefficients(get_drda_helper(data, activity_col))
  unbounded_alpha <- unbounded_coeffs["alpha"] # 0 dose asymptote
  unbounded_delta <- unbounded_coeffs["delta"] # height of curve
  # bound curve height depending on whether the model increases or decreases
  if(unbounded_delta < 0){
    # bound delta above -alpha so curves can't go way below 0
    return(get_drda_helper(data, activity_col,
                           lower_bound = c(-Inf, -unbounded_alpha, -Inf, -Inf)))
  }
  else{
    # bound delta below 100-alpha so curves can't go way above 100
    return(get_drda_helper(data, activity_col,
                           upper_bound = c(Inf, 100-unbounded_alpha, Inf, Inf)))
  }
}

#' Fit a drda 4-parameter logistic model with one or more parameters fixed
#'
#' `get_drda_fixed()` runs [get_drda_helper()] to generate a 4-parameter
#' logistic dose-response model for the effect of a given treatment on a target
#' with one or more of the parameters fixed at a given value. The four
#' parameters are available as optional arguments, named as in [drda::drda()].
#'
#' @inherit get_drda_helper params return
#' @param alpha Optional: fixed value of asymptote level at 0 dose. See
#'   [drda::drda()].
#' @param delta Optional: fixed value of height of the curve (difference betwene
#'   minimum and maximum dose asymptotes). Signed (can be positive or negative).
#'   See [drda::drda()].
#' @param eta Optional: fixed value of growth rate or Hill slope. Not signed
#'   (always positive). See [drda::drda()].
#' @param phi Optional: fixed value of mid-value or EC50. See [drda::drda()].
#'
get_drda_fixed <- function(data, alpha=NA, delta=NA, eta=NA, phi=NA,
                           activity_col="activity"){
  params <- c(alpha, delta, eta, phi)
  # drda bounds require a non-infinite value. special case for no params:
  if(all(is.na(params))) return(get_drda_helper(data, activity_col))
  # get bounds for fixed drda, substituting Inf and -Inf for free parameters
  upper <- param_bounds(c(alpha, delta, eta, phi), lower = FALSE)
  lower <- param_bounds(c(alpha, delta, eta, phi), lower = TRUE)
  get_drda_helper(data, activity_col, lower_bound = lower,
                  upper_bound = upper)
}
