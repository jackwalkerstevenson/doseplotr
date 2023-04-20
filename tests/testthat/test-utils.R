test_that("converting from logmolar to nM works", {
  expect_equal(logmolar_to_nM(-9), 1)
  expect_equal(logmolar_to_nM(0), 1e9)
  expect_equal(logmolar_to_nM(1), 1e10)
})

test_that("converting from nM to logmolar works", {
  expect_equal(nM_to_logmolar(1), -9)
  expect_equal(nM_to_logmolar(100), -7)
  expect_warning(val <- nM_to_logmolar(-1))
  expect_true(is.nan(val))
  expect_equal(nM_to_logmolar(0), -Inf)
})
