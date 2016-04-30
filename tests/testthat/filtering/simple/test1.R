library(testthat)

test_that('Basic test filtering works correctly', {
    expect_equal(func1(5), 5)
    expect_equal(func1(3), 3)
})
