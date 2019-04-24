context("test-error")

test_that("arguments are checked", {
  expect_error(ploterrors(means="a"))
  expect_error(ploterrors(sds="a"))
  expect_error(ploterrors(alpha="a"))
  expect_error(ploterrors(direction = 1))
  expect_error(ploterrors(plotly="a"))

  expect_error(ploterrors(means=NA))
  expect_error(ploterrors(sds=NA))
  expect_error(ploterrors(alpha=NA))
  expect_error(ploterrors(direction = NA))
  expect_error(ploterrors(plotly=NA))
})

test_that("output is checked",{
  expect_class(ploterrors(),c("gg","ggplot"))
  expect_class(ploterrors(direction = ">"),c("gg","ggplot"))
  expect_class(ploterrors(direction = "<"),c("gg","ggplot"))
  expect_class(ploterrors(plotly=TRUE),c("plotly","htmlwidget"))
  expect_class(ploterrors(plotly=TRUE,direction = ">"),c("plotly","htmlwidget"))
  expect_class(ploterrors(plotly=TRUE,direction = "<"),c("plotly","htmlwidget"))
})
