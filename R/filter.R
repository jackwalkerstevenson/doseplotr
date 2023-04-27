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
