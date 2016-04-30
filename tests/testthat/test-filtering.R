library(testr)
library(testthat)

context("Filtering")

test_that('Basic test filtering works correctly', {
  t <- tempdir()
  dir.create(t)
  func <- function(x) {
    if (x < 10) {
      return(x)
    } else {
      return(x + 1)
    }
  }
  testr:::filter_tests("filtering/simple", t, remove_tests = FALSE, verbose = FALSE, func)
  expect_true(file.exists(file.path(t, "test1.R")))
  expect_true(file.exists(file.path(t, "test2.R")))
  expect_false(file.exists(file.path(t, "test3.R")))
})
