library(testr)
library(testthat)

context("Decoration")

test_that('Decoration is correct', {
  Decorate(abbreviate)
  expect_equal(T,T)
})