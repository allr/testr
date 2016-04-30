library(testthat)

test_that('Basic test filtering works correctly', {
    expect_equal(func(5), 5)
    expect_equal(func(3), 3)
})
