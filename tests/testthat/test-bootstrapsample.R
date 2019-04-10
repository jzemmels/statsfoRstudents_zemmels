context("test-bootstrapsample")

test_that("sample dataset is valid", {
  expect_error(bootstrapSample(1:30, 'a'))
  expect_error(bootstrapSample(1:30, 1.3))
  expect_error(bootstrapSample(1:30, 1:3))
  expect_error(bootstrapSample(1:30, 'a'))
})
