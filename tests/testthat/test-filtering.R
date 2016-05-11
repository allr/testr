library(testr)
library(testthat)

context("Filtering")

func1 <<- function(x) {
    if (x < 10) {
        return(x)
    } else {
        return(x + 1)
    }
}

func2 <<- function(x, y) {
    if (x < 10) {
        if (y == 0) {
            res <- x
        } else {
            res <- 5
        }
    } else {
        res <- y
    }
    res
}

test_template_f1 <- "
library(testthat)

test_that('Basic test filtering works correctly', {
expect_equal(func1(%d), %d)
expect_equal(func1(%d), %d)
})"

test_template_f2 <- "
library(testthat)

test_that('Basic test filtering works correctly', {
expect_equal(func2(%d, %d), %d)
expect_equal(func2(%d, %d), %d)
})"

# test_that('Basic test filtering works correctly', {
#     t <- tempdir()
#     path_to_filter <- file.path(t, "to-filter")
#     filter_out <- file.path(t, "filter-out")
#     dir.create(path_to_filter)
#     dir.create(filter_out)
#     write(sprintf(test_template_f1, 5, 5, 3, 3), file.path(path_to_filter, "test1.R"))
#     write(sprintf(test_template_f1, 11, 12, 13, 14), file.path(path_to_filter, "test2.R"))
#     write(sprintf(test_template_f1, 5, 5, 3, 3), file.path(path_to_filter, "test3.R"))
#     write(sprintf(test_template_f2, 4, 1, 5, 5, 0, 5), file.path(path_to_filter, "test4.R"))
#     write(sprintf(test_template_f2, 4, 1, 5, 5, 0, 5), file.path(path_to_filter, "test5.R"))
#     # one function
#     filter(path_to_filter, filter_out, func1, verbose = FALSE)
#     expect_true(file.exists(file.path(filter_out, "test1.R")))
#     expect_true(file.exists(file.path(filter_out, "test2.R")))
#     expect_false(file.exists(file.path(filter_out, "test3.R")))
#
#     # multiple functions
#     filter(path_to_filter, filter_out, func1, func2, verbose = FALSE)
#     expect_true(file.exists(file.path(filter_out, "test1.R")))
#     expect_true(file.exists(file.path(filter_out, "test2.R")))
#     expect_false(file.exists(file.path(filter_out, "test3.R")))
#     expect_true(file.exists(file.path(filter_out, "test4.R")))
#     expect_false(file.exists(file.path(filter_out, "test5.R")))
#     rm(func1, func2, envir = .GlobalEnv)
# })

# test_that('Package filtering works correctly', {
#     temp <- "/Users/romantsegelskyi/temp"
#     tar_path <- file.path(temp, "stringr.tar.gz")
#     stringr_path <- file.path(temp, "stringr")
#     output_dir <- file.path(temp, "output")
#     if (!file.exists(output_dir)) dir.create(output_dir)
#     download.file("https://cran.r-project.org/src/contrib/stringr_1.0.0.tar.gz", tar_path, quiet = TRUE)
#     untar(tar_path, exdir = temp)
#     filter("filtering/package", output_dir, package_path = stringr_path, verbose = TRUE)
#     expect_true(file.exists(file.path(output_dir, "test1.R")))
#     expect_false(file.exists(file.path(output_dir, "test2.R")))
#     unlink(tar_path)
#     unlink(stringr_path)
# })
