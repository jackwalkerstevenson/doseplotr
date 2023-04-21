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
