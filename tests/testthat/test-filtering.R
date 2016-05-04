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

test_that('Basic test filtering works correctly', {
    t <- tempdir()
    # one function
    filter("filtering/simple", t, func1, verbose = FALSE)
    expect_true(file.exists(file.path(t, "test1.R")))
    expect_true(file.exists(file.path(t, "test2.R")))
    expect_false(file.exists(file.path(t, "test3.R")))
    # multiple functions
    filter("filtering/simple", t, func1, func2, verbose = FALSE)
    expect_true(file.exists(file.path(t, "test1.R")))
    expect_true(file.exists(file.path(t, "test2.R")))
    expect_false(file.exists(file.path(t, "test3.R")))
    expect_true(file.exists(file.path(t, "test4.R")))
    expect_false(file.exists(file.path(t, "test5.R")))
    rm(func1, func2, envir = .GlobalEnv)
})

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
