library(testthat)
library(stringr)

test_that('Some basic functions are fine', {
    fruit <- c("apple", "banana", "pear", "pinapple")
    expect_equal(all(str_detect(fruit, "a")), TRUE)
    expect_equal(any(str_detect(fruit, "^a")), TRUE)
})
