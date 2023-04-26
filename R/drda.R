#' Filter data to include only a given treatment and target
#'
#' @param data A dataframe containing the following columns:
#' * "treatment" (default) or other treatment column name (see `trt_colname`)
#' * "target" (default) or other target column name (see `tgt_colname`)
#' @param trt Name of the treatment to use in the model
#' @param tgt Name of the target to use in the model
#' @param trt_colname Name of the column containing treatments. Default is
#'   "treatment". Typical alternatives might include e.g. "drug", "additive",
#'   "supplement".
#' @param tgt_colname Name of the column containing targets. Default is
#'   "target". Typical alternatives might include e.g. "mutant", "cell_line",
#'   "species".
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
                        trt_colname="treatment",
                        tgt_colname="target"){
  assertthat::assert_that(assertthat::has_name(data, trt_colname),
                          msg = glue::glue("column {trt_colname} not found in data"))
  assertthat::assert_that(assertthat::has_name(data, tgt_colname),
                          msg = glue::glue("column {tgt_colname} not found in data"))
  # unclear why you have to wrap embrace in get() but apparently you do
  return(dplyr::filter(data,
                       get({{ trt_colname }}) == trt,
                       get({{ tgt_colname }}) == tgt))
}

#' Get a drda dose-response model for the activity of a treatment on a target
#'
#' `get_drda()` runs [drda::drda()] to generate a 4-parameter logistic
#' dose-response model for the effect of a given treatment on a target.
#'
#' @param data A dataframe containing the following columns:
#' * "activity" (default) or other activity column name (see `activity_colname`)
#' * conc_logM: concentration of treatment in log(molar) units
#' @param activity_colname Name of the column containing activity of treatment.
#'     Default is "activity". Typical alternatives might include e.g.
#'     "percent_inhibition", "viability", "growth".
#' @param ... Additional parameters to pass on to [drda::drda()]
#' @return A dose-response model object of class `drda`.
#' @export
#'
get_drda <- function(data, activity_colname="activity", ...){
  assertthat::assert_that(assertthat::has_name(data, activity_colname),
                          msg = glue::glue("column {activity_colname} not found in data"))
  # 4-param logistic model on pre-log-transformed data
  return(drda::drda(get({{ activity_colname }})~conc_logM, data = data, ...))
}
