library(testr)
library(testthat)

context("R test suite")

test_that("R test suite works correctly", {
    testr_options("IO", FALSE)
    testr_options("parallel_tests", TRUE)
    if (testr_options("rtests")) {
        res <- reg_tests(file_indexes = 1:8)
        expect_false(any(res))
        res <- reg_tests(file_indexes = 9:14)
        expect_false(any(res))
        res <- all_tests(file_indexes = 1:8)
        expect_false(any(res))
        res <- all_tests(file_indexes = 9:16)
        expect_false(any(res))
        res <- all_tests(file_indexes = 17:24)
        expect_false(any(res))
    }
    testr_options("parallel_tests", FALSE)
    testr_options("IO", TRUE)
})

