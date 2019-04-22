context("test-bootstrapprocess")

test_that("sample dataset is valid", {
  expect_error(bootstrapProcess(c(), T))
  expect_error(bootstrapProcess(diag(1, nrow = 2), T))
  expect_error(bootstrapProcess(c(8, 13, NA), T))
  expect_error(bootstrapProcess(rnorm, T))
})

test_that("anime value is valid", {
  expect_error(bootstrapProcess(1:30), anime = NA)
  expect_error(bootstrapProcess(1:30), anime = NULL)
  expect_error(bootstrapProcess(1:30), anime = Inf)
  expect_error(bootstrapProcess(1:30), anime = NaN)
  expect_error(bootstrapProcess(1:30), anime = 8)
  expect_error(bootstrapProcess(1:30), anime =  1.3)
  expect_error(bootstrapProcess(1:30), anime = "cats")
  expect_error(bootstrapProcess(1:30), anime = t)
})
