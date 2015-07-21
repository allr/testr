library(testr)
library(testthat)

context("R test suite")

test_that("R test suite works correctly", {
    testr_options('verbose', FALSE)
    res <- reg_tests()
    expect_false(any(res))
    res <- all_tests()
    expect_false(any(res))
    testr_options('verbose', TRUE)
})
