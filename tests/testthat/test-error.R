context("test-error")

plt <- finalProject::ploterrors()
pltly <- finalProject::ploterrors(plotly = TRUE)

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
  expect_class(plt[[1]],c("gg","ggplot"))
  expect_class(pltly[[1]],c("plotly","htmlwidget"))
})

test_that("plotlw output is checked",{
  expect_class(plt[[2]],c("gg","ggplot"))
  expect_class(pltly[[2]],c("plotly","htmlwidget"))
})


test_that("output is checked",{
  expect_class(ploterrors(),"list")})




