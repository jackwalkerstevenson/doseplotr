test_that("converting from log(molar) to nM works", {
  expect_equal(logM_to_nM(-9), 1)
  expect_equal(logM_to_nM(0), 1e9)
  expect_equal(logM_to_nM(1), 1e10)
})

test_that("converting from nM to log(molar) works", {
  expect_equal(nM_to_logM(1), -9)
  expect_equal(nM_to_logM(100), -7)
  expect_warning(val <- nM_to_logM(-1), "NaNs produced")
  expect_true(is.nan(val))
  expect_equal(nM_to_logM(0), -Inf)
})

test_that("finding the longest string in a list works", {
  test_strings <- c("f", "foobar", "foo")
  expect_equal(longest(test_strings), 6)
})

test_that("substituting Inf and -Inf into a vector works", {
  v <- c(68.170, -69.39, 4.267, NA)
  pos_sub <- c(68.170, -69.39, 4.267, Inf)
  neg_sub <- c(68.170 - 68.170*1e-4,
               -69.39 - 69.39*1e-4,
               4.267 - 4.267*1e-4,
               -Inf)
  expect_equal(param_bounds(v), pos_sub)
  expect_equal(param_bounds(v, lower = TRUE), neg_sub)
})
