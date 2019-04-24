context("test-plotShape")

test_that("arguments are checked", {
  expect_error(plotShape(randomDist ="a"))
  expect_error(plotShape(randomDist = c(1,2)))
  expect_error(plotShape(randomDist = NA))
})

test_that("outputs checked",{
  expect_class(plotShape(), "list")
  expect_class(plotShape()[[1]], "character")
  expect_class(plotShape()[[2]], c("gg", "ggplot"))
})
