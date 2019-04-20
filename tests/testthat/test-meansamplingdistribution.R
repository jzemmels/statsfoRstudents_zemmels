context("test-meansamplingdistribution")

set.seed(4202019)
x <- randomSample()
m <- updateSampleMeans(sampleData = x)

test_that("randomSample arguments are checked", {
  expect_error(randomSample(mu="a"))
  expect_error(randomSample(sigma="a"))
  expect_error(randomSample(sampleSize = "a"))
  expect_error(randomSample(numSamples = "a"))

  expect_error(randomSample(mu=NA))
  expect_error(randomSample(sigma=NA))
  expect_error(randomSample(sampleSize = NA))
  expect_error(randomSample(numSamples = NA))
})

test_that("randomSample output is checked",{
  expect_class(randomSample(),"data.frame")
  #set.seed(4202019)
  #expect_equal(randomSample(),x)
})

test_that("randomSample_histogram arguments are checked",{
  expect_error(randomSample_histogram(sampleData = 1))
  expect_error(randomSample_histogram(sampleData = "a"))
  expect_error(randomSample_histogram(sampleData = data.frame(NA)))
  expect_error(randomSample_histogram(binwidth = "a"))
  expect_error(randomSample_histogram(binwidth = NA))
  expect_error(randomSample_histogram(variableName = 1))
  expect_error(randomSample_histogram(plotly = "a"))
})

test_that("randomSample_histogram output is checked",{
  expect_class(randomSample_histogram(sampleData = x),c("gg","ggplot"))
  expect_class(randomSample_histogram(sampleData = x,plotly=TRUE),c("plotly","htmlwidget"))
})

test_that("updateSampleMeans arguments are checked",{
  expect_error(updateSampleMeans(sampleData = 1))
  expect_error(updateSampleMeans(sampleData = "a"))
  expect_error(updateSampleMeans(sampleData = data.frame(NA)))
  expect_error(updateSampleMeans(sampleMeans = 1))
  expect_error(updateSampleMeans(sampleMeans = "a"))
})

test_that("randomSampleMeans output is checked",{
  expect_class(updateSampleMeans(sampleData = x),"data.frame")
  expect_data_frame(updateSampleMeans(sampleData = x),any.missing = FALSE)
})

test_that("sampleMeans_histogram arguments are checked",{
  expect_error(sampleMeans_histogram(sampleMeans = 1))
  expect_error(sampleMeans_histogram(sampleMeans = "a"))
  expect_error(sampleMeans_histogram(sampleMeans = data.frame(NA)))
  expect_error(sampleMeans_histogram(binwidth = NA))
  expect_error(sampleMeans_histogram(variableName = 1))
  expect_error(sampleMeans_histogram(plotly = "a"))
})

test_that("sampleMeans_histogram output is checked",{
  expect_class(sampleMeans_histogram(sampleMeans = m),c("gg","ggplot"))
  expect_class(sampleMeans_histogram(sampleMeans = m,plotly=TRUE),c("plotly","htmlwidget"))
})
