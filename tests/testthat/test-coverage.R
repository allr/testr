library(testr)
library(testthat)

context("Coverage")

test_that("General coverage is correct", {
    res <- testr::measure_gcov("tests/testthat/covdata", verbose = FALSE)
})

test_that('Generate Abbreviate', {
  expect_error(TestGen("CaptureInfo/capture"))
  TestGen("CaptureInfo/capture_warn_error", "we")
  expect_true(file.exists("we"))
  expect_true(file.info("we")$isdir)
  expect_equal(length(list.files("we",recursive = T)), 3) # one is bad.args file
  sink("out")
  expect_true(RunTests("we"))
  sink()
  file.remove("out")
  unlink("we", recursive = T)
})
