# context("test-error")
#
# test_that("arguments are checked", {
#   expect_error(errors(mu="a"))
#   expect_error(errors(sigma="a"))
#   expect_error(errors(alpha="a"))
#   expect_error(errors(direction = 1))
#   expect_error(errors(plotly="a"))
#
#   expect_error(errors(mu=NA))
#   expect_error(errors(sigma=NA))
#   expect_error(errors(alpha=NA))
#   expect_error(errors(direction = NA))
#   expect_error(errors(plotly=NA))
# })
#
# test_that("output is checked",{
#   expect_class(errors(),c("gg","ggplot"))
#   expect_class(errors(direction = ">"),c("gg","ggplot"))
#   expect_class(errors(direction = "<"),c("gg","ggplot"))
#   expect_class(errors(plotly=TRUE),c("plotly","htmlwidget"))
#   expect_class(errors(plotly=TRUE,direction = ">"),c("plotly","htmlwidget"))
#   expect_class(errors(plotly=TRUE,direction = "<"),c("plotly","htmlwidget"))
# })
