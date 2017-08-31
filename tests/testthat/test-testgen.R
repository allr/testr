library(testr)
library(testthat)

context("Generation")

test_that('Test start_capture()', {
    unlink(file.path(getwd(),"capture"))
    unlink(file.path(getwd(),"tmp"))
    testr::start_capture("stats::dpois")
    dpois(0:7, lambda = 1)
    testr::write_captured_tests("/tmp")
    expect_true(file.exists(file.path(getwd(),"tmp")))
    expect_true(file.info(file.path(getwd(),"tmp"))$isdir)
    expect_true(file.exists(file.path(getwd(),"capture/stats___dpois")))
    expect_true(file.info(file.path(getwd(),"capture/stats___dpois"))$isdir)
    expect_true(file.exists(file.path(getwd(),"capture")))
    expect_true(file.info(file.path(getwd(),"capture"))$isdir)
    expect_true(file.exists(file.path(getwd(),"tmp/test.stats.dpois.tar.gz")))
    expect_false(file.info(file.path(getwd(),"tmp/test.stats.dpois.tar.gz"))$isdir)
    expect_equal(length(list.files(file.path(getwd(),"capture/stats___dpois"), pattern = "*.R$", recursive = FALSE, all.files = FALSE)), 1)
    unlink(file.path(getwd(),"tmp"))
    unlink(file.path(getwd(),"capture"))
})

test_that('Test find_packages_using_function()', {
    x <- testr::find_packages_using_function("cor")
    y <- testr::find_packages_using_function("aksagdK*@&e9dgiakegdkgjqge93yqe")
    expect_true(length(x) > 0)
    expect_true(class(x[1]) == "character")
    expect_true(length(y) == 0)
    expect_true(class(y) == "character")
})

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

