library(testthat)

test_that('Some function test', {
    expect_equal(func2(15, 2), 2)
    expect_equal(func2(5, 0), 5)
    expect_equal(func2(11, 3), 3)
})
