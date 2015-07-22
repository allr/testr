library(testr)
library(testthat)

context("Processing tcs")

test_that('Splitting works correctly',{
  dir.create("dir1")
  dir.create("dir2")
  temp_dir1 <- "dir1"
  temp_dir2 <- "dir2"
  testr:::split_tcs("TestFiles/tc_abbreviate.R", temp_dir1, 3)
  expect_equal(length(list.files(temp_dir1, recursive = T)), 6)
  list.files(temp_dir1, recursive = T)
  testr:::split_tcs(temp_dir1, temp_dir2, 1)
  list.files(temp_dir2, recursive = T)
  expect_equal(length(list.files(temp_dir2, recursive = T)), 18)
  unlink("dir1", recursive = T, force = T)
  unlink("dir2", recursive = T, force = T)
})

test_that('FilterTCs works correctly', {
  dir.create("filter")
  dir.create("filter-res")
  testr:::split_tcs("./TestFiles/tc_abbreviate.R", "filter", 1)
#   out <- capture.output(testr:::FilterTCs("filter/", "filter-res"))
#   expect_equal(length(list.files("filter-res", recursive = T)), 4)
  unlink("filter", recursive = T, force = T)
  unlink("filter-res", recursive = T, force = T)
#   file.remove("tmp_source.Rout")
})