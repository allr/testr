library(testr)
library(testthat)

context("R test suite")

test_that("R test suite works correctly", {
    testr_options("IO", FALSE)
    res <- reg_tests()
    expect_false(any(res))
    print(res)
    res <- all_tests()
    expect_false(any(res))
    print(res)
    testr_options("IO", TRUE)
})
