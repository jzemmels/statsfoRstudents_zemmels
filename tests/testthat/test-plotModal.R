context("test-plotModal")

test_that("arguments are checked", {
  expect_error(plotModal(randomDist ="a"))
  expect_error(plotModal(randomDist = c(1,2)))
  expect_error(plotModal(randomDist = NA))
})

test_that("outputs checked",{
  expect_class(plotModal(), "list")
  expect_class(plotModal()[[1]], "character")
  expect_class(plotModal()[[2]], c("gg", "ggplot"))
})
