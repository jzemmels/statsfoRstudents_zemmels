context("test-error")


set.seed(4859374)
x <- randomSample()
m <- updateSampleMeans(sampleData = x)

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

test_that("plotup output is checked",{
  expect_class(ploterrors$pltup(sampleData = x),c("gg","ggplot"))
  expect_class(ploterrors$pltup(sampleData = x,plotly=TRUE),c("plotly","htmlwidget"))
})

test_that("plotlw output is checked",{
  expect_class(ploterrors$pltlw(sampleData = x),c("gg","ggplot"))
  expect_class(ploterrors$pltlw(sampleData = x,plotly=TRUE),c("plotly","htmlwidget"))
})


test_that("output is checked",{
  expect_class(ploterrors(),"list")})




