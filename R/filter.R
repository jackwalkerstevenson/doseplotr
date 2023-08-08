#' Filter data to include only given treatments and targets
#'
#' @param data A dataframe containing the following columns:
#' * "treatment" (default) or other treatment column name (see `trt_col`)
#' * "target" (default) or other target column name (see `tgt_col`)
#' @param trt Name of treatments to use in the model. String or character vector.
#' @param tgt Name of targets to use in the model. String or character vector.
#' @param trt_col Name of the column containing treatments. Default is
#'   "treatment". Typical alternatives might include e.g. "drug", "additive",
#'   "supplement".
#' @param tgt_col Name of the column containing targets. Default is "target".
#'   Typical alternatives might include e.g. "mutant", "cell_line", "species".
#'
#' @return A dataframe containing only rows filtered for the desired treatments
#'   and targets.
#' @export
#' @examples
#' df <- data.frame(treatment = c("foo", "foo", "foo", "bar", "baz"),
#'                  target = c("apple", "orange", "banana", "apple", "orange"))
#' filter_trt_tgt(df, "foo", "orange")
filter_trt_tgt <- function(data, trt = NULL, tgt = NULL,
                           trt_col="treatment",
                           tgt_col="target"){
  assertthat::assert_that(assertthat::has_name(data, trt_col),
                          msg = glue::glue("column {trt_col} not found"))
  assertthat::assert_that(assertthat::has_name(data, tgt_col),
                          msg = glue::glue("column {tgt_col} not found"))
  # unclear why you have to wrap embrace in get() but apparently you do
  if(!is.null(trt)){
    data <- data |> dplyr::filter(get({{ trt_col }}) %in% trt)
  }
  if(!is.null(tgt)){
    data <- data |> dplyr::filter(get({{ tgt_col }}) %in% tgt)
  }
  if(nrow(data) == 0){warning("no data left after filtering")}
  return(data)
}

#' Filter, validate and reorder a dataframe column with desired values
#'
#' This function is intended for reconciling a column of a dataframe with an
#' externally-provided vector of desired values for the column. It does three
#' things:
#' * Filter the column so it only contains the desired values
#' * Validate that all of the desired values are actually present in the column.
#' Throws an error if any values are missing from the column.
#' * Reorder the column as a factor to match the order of the desired values
#' @param df A dataframe
#' @param colname The column to filter, validate and reorder
#' @param values The values for which to filter, check and reorder the column
#' @return The dataframe with the desired column filtered, validated and
#'   reordered
#' @export
#' @examples
#' example_df <- data.frame(treatment = c("foo", "foo", "foo", "bar", "baz"),
#'                  target = c("apple", "orange", "banana", "apple", "orange"))
#' filter_validate_reorder(example_df, "target", c("banana", "orange"))
filter_validate_reorder <- function(df, colname, values){
  df <- df |>
    dplyr::filter(get({{ colname }}) %in% values) # remove extraneous values
  data_values <- unique(df[[colname]]) # values actually in data
  for(value in values){ # check all intended values area actually present
    assertthat::assert_that(value %in% data_values, msg = glue::glue(
      "value {value} was not found in the data"))}
  df |> # set order of values in data
    dplyr::mutate({{colname}} :=
                    forcats::fct_relevel(.data[[colname]], values))
}
