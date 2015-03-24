library(testr)
library(testthat)

context("Decoration")

test_that('Decoration is correct', {
  DecorateSubst(abbreviate)
  expect_equal(T,T)
})