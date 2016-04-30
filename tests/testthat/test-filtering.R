library(testr)
library(testthat)

context("Filtering")

func1 <<- function(x) {
    if (x < 10) {
      return(x)
    } else {
      return(x + 1)
    }
  }

func2 <<- function(x, y) {
      if (x < 10) {
          if (y == 0) {
              res <- x
          } else {
              res <- 5
          }
      } else {
          res <- y
      }
      res
}

test_that('Basic test filtering works correctly', {
  t <- tempdir()
  # one function
  filter_tests("filtering/simple", t, func1, verbose = FALSE)
  expect_true(file.exists(file.path(t, "test1.R")))
  expect_true(file.exists(file.path(t, "test2.R")))
  expect_false(file.exists(file.path(t, "test3.R")))
  # multiple functions
  filter_tests("filtering/simple", t, func1, func2, verbose = FALSE)
  expect_true(file.exists(file.path(t, "test1.R")))
  expect_true(file.exists(file.path(t, "test2.R")))
  expect_false(file.exists(file.path(t, "test3.R")))
  expect_true(file.exists(file.path(t, "test4.R")))
  expect_false(file.exists(file.path(t, "test5.R")))
  rm(func1, func2, envir = .GlobalEnv)
})
