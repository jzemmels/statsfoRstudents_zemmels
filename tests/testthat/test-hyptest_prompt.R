context("test-hyptest_prompt")

test_that("hypTest_prompt output checked", {
  expect_list(hypTest_prompt())
})

q <- hypTest_prompt()

test_that("ensure that elements of list output are non-empty",{
  expect_numeric(c(q$nullVal,q$altVal,q$testStat,q$signifLevel))
  expect_character(c(q$prompt,q$decision,q$source))
})
