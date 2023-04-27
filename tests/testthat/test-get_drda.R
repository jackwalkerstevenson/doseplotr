test_that("filter_trt_tgt successfully filters", {
  df <- data.frame(treatment = c("foo", "foo", "foo", "bar", "baz"),
  target = c("apple", "orange", "banana", "apple", "orange"))
  filtered_df <- data.frame(treatment = c("foo"), target = c("orange"))
  expect_equal(filter_trt_tgt(df, "foo", "orange"), filtered_df)
})

test_that("filter_trt_tgt recognizes a nonstandard column name", {
  df <- data.frame(treatment_test = c("foo", "foo", "foo", "bar", "baz"),
                   target = c("apple", "orange", "banana", "apple", "orange"))
  filtered_df <- data.frame(treatment_test = c("foo"), target = c("orange"))
  expect_equal(filter_trt_tgt(df, "foo", "orange",
                              trt_col = "treatment_test"),
               filtered_df)
})

