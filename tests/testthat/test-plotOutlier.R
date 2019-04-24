context("test-plotOutlier")

test_that("arguments are checked", {
  expect_error(plotOutlier(randomDist ="a"))
  expect_error(plotOutlier(randomDist = c(1,2)))
  expect_error(plotOutlier(randomDist = NA))
})

test_that("outputs checked",{
  expect_class(plotOutlier(), "list")
  expect_class(plotOutlier()[[1]], "character")
  expect_class(plotOutlier()[[2]], c("gg", "ggplot"))
})
