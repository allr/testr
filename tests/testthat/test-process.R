library(testr)
library(testthat)

context("Processing tcs")

test_that('Splitting works correctly',{
  dir.create("dir1")
  dir.create("dir2")
  temp_dir1 <- "dir1"
  temp_dir2 <- "dir2"
  testr:::splitTCs("TestFiles/tc_abbreviate.R", temp_dir1, 3)
  expect_equal(length(list.files(temp_dir1, recursive = T)), 6)
  list.files(temp_dir1, recursive = T)
  testr:::splitTCs(temp_dir1, temp_dir2, 1)
  list.files(temp_dir2, recursive = T)
  expect_equal(length(list.files(temp_dir2, recursive = T)), 18)
  unlink("dir1", recursive = T, force = T)
  unlink("dir2", recursive = T, force = T)
})