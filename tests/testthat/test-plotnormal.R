context("test-plotnormal")

test_that("arguments are checked", {
  expect_error(plotNormal(mu="a"))
  expect_error(plotNormal(sigma="a"))
  expect_error(plotNormal(alpha="a"))
  expect_error(plotNormal(obs="a"))
  expect_error(plotNormal(direction = 1))
  expect_error(plotNormal(plotly="a"))

  expect_error(plotNormal(mu=NA))
  expect_error(plotNormal(sigma=NA))
  expect_error(plotNormal(alpha=NA))
  expect_error(plotNormal(direction = NA))
  expect_error(plotNormal(plotly=NA))
})

test_that("output is checked",{
  expect_class(plotNormal(),c("gg","ggplot"))
  expect_class(plotNormal(direction = ">"),c("gg","ggplot"))
  expect_class(plotNormal(direction = "<"),c("gg","ggplot"))
  expect_class(plotNormal(plotly=TRUE),c("plotly","htmlwidget"))
  expect_class(plotNormal(plotly=TRUE,direction = ">"),c("plotly","htmlwidget"))
  expect_class(plotNormal(plotly=TRUE,direction = "<"),c("plotly","htmlwidget"))
})

