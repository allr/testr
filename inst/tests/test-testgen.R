library(testr)
library(testthat)

context("Generation")

test_that('Generate Abbreviate', {
    expect_warning(test_gen("CaptureInfo/capture"))
    test_gen("CaptureInfo/capture_abbreviate", "abbreviate")
    expect_true(file.exists("abbreviate"))
    expect_true(file.info("abbreviate")$isdir)
    expect_equal(length(list.files("abbreviate")), 2) # one is bad.args file
    unlink("abbreviate", recursive = T)
})

test_that('Generate Warnings/Errors', {
    expect_warning(test_gen("CaptureInfo/capture"))
    test_gen("CaptureInfo/capture_warn_error", "we")
    expect_true(file.exists("we"))
    expect_true(file.info("we")$isdir)
    expect_equal(length(list.files("we",recursive = T)), 3) # one is bad.args file
    expect_true(run_tests("we"))
    unlink("we", recursive = T)
})
