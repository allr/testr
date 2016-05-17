library(testr)
library(testthat)

context("Generation")

test_that('Generate Abbreviate', {
    expect_warning(generate("abbreviate", "CaptureInfo/capture"))
    generate("abbreviate", "CaptureInfo/capture_abbreviate", verbose = FALSE)
    expect_true(file.exists("abbreviate"))
    expect_true(file.info("abbreviate")$isdir)
    expect_equal(length(list.files("abbreviate")), 2) # one is bad.args file
    sink(tempfile())
    tryCatch(testthat::test_dir(cache$output_dir), error=function(x) {
        result <<- FALSE
        x #invisible(x)
    })
    sink()
    unlink("abbreviate", recursive = T)
})

test_that('Generate Warnings/Errors', {
    expect_warning(generate("we", "CaptureInfo/capture"))
    generate("we", "CaptureInfo/capture_warn_error", verbose = FALSE)
    expect_true(file.exists("we"))
    expect_true(file.info("we")$isdir)
    expect_equal(length(list.files("we",recursive = T)), 3) # one is bad.args file
    sink(tempfile())
    tryCatch(testthat::test_dir(cache$output_dir), error=function(x) {
        result <<- FALSE
        x #invisible(x)
    })
    sink()
    unlink("we", recursive = T)
})
