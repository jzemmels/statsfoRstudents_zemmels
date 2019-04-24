context("test-bootstrapsample")

test_that("n is valid", {
  expect_error(bootstrapSample(1:30, 'a'))
  expect_error(bootstrapSample(1:30, NA))
  expect_error(bootstrapSample(1:30, 1:3))
  expect_error(bootstrapSample(1:30, NaN))
  expect_error(bootstrapSample(1:30, Inf))
  expect_error(bootstrapSample(1:30, pi))
  expect_error(bootstrapSample(1:30, NULL))
})

test_that("sample dataset is valid", {
  expect_error(bootstrapSample(c(), 8))
  expect_error(bootstrapSample(diag(1, nrow = 2), 8))
  expect_error(bootstrapSample(c(8, 13, NA), 8))
  expect_error(bootstrapSample(rnorm, 8))
})

test_that("output is a list", {
  expect_true(is.list(bootstrapSample(1:30, 8)))
})
