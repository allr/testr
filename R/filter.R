#' @title Test Case filtering based on coverage
#'
#' @description This function attemps to filter the test cases for given functions based on
#' code coverage information collected by covr.
#'
#' @param tc_root directory with test cases to filter
#' @param tc_result_root directory where the filtered test cases should be copied
#' @param functions functions to be filtered aganist
#' @param package_path root of the package to be filtered aganist
#' @param remove_tests if to delete test cases that don't affect coverage from tc_root
#' @param exclude_existing_tests If TRUE, existing tests will be ignored from the code coverage
#' @param compact If TRUE, after filtering, tests will be compacted to a file per function
#' @param verbose if to show additional infomation during filtering
#'
filter_tests <- function(tc_root, tc_result_root, functions, package_path, remove_tests = FALSE, exclude_existing_tests = FALSE, compact = FALSE, verbose = testr_options("verbose")) {
    if (missing(tc_root) || !file.exists(tc_root))
        stop("Specified directory with test cases does not exist!")
    if (missing(functions) && missing(package_path)) {
        stop("Neither list of functions or package path to measure coverage by was specified")
    } else if (package_path != "") {
        if (!file.exists(package_path) || !file.info(package_path)$isdir) {
            stop("Specified package root either doesn't exists or is not a directory")
        }
        is_package <- TRUE
    } else {
        is_package <- FALSE
    }
    if (verbose) cat("Test cases root - ", tc_root, "\n")
    all.tc <- list.files(path = tc_root, all.files = TRUE, recursive = TRUE, pattern = "\\.[rR]$", full.names = TRUE)
    if (verbose) cat("Number of test cases - ", length(all.tc), "\n")
    # create dummy objects
    if (is_package) {
        # for package, run also its tests so that we do not duplicate
        if (exclude_existing_tests)
            total_coverage <- covr::package_coverage(package_path, type = "none")
        else
            total_coverage <- covr::package_coverage(package_path, type = "tests")
    } else {
        total_coverage <- sapply(names(functions), function(fname) {
            covr::function_coverage(fname, 1)
        }, simplify = FALSE)
    }
    tests_so_far <- character()
    if (verbose)
        cat("Pruning tests...\n")

    cov_change <- function(tc) {
        tests_so_far <<- c(tests_so_far, tc)
        if (!is.null(tc_result_root)) result_path <- gsub(tc_root, tc_result_root, tc)
        sink(tempfile())
        # code is all passed tests
        code <- character()
        for (f in tests_so_far)
            code <- paste(code, sprintf("\ntestthat::test_file('%s')", tools::file_path_as_absolute(f)), sep="")
        if (is_package) {
            new_total_coverage <- covr::package_coverage(package_path,
                                                    type = "none",
                                                    code = code)
        } else {
            new_total_coverage <- sapply(names(functions), function(fname) {
                covr::function_coverage(fname, code = parse(text = code))
            }, simplify = FALSE)
        }
        sink()
        coverage_increased <- ifelse(is_package,
                                     covr::percent_coverage(new_total_coverage) - covr::percent_coverage(total_coverage) > 0,
                                     any(sapply(names(functions),function(fname) {
                                         covr::percent_coverage(new_total_coverage[[fname]]) - covr::percent_coverage(total_coverage[[fname]])
                                     }) > 0))
        if (coverage_increased) {
            if (verbose) cat("Test case ", tc, " increased the coverage\n")
            if (compact) {
                # get function name
                fname <- rev(split_path(tc))[[2]]
                testFile <- file.path(tc_result_root, paste("test-", fname, ".R", sep = ""))
                if (file.exists(testFile))
                    # ignore the library and context declarations as they already exist in the file
                    test <- paste(readLines(tc)[c(-1,-2,-3)], collapse="\n")
                else
                    test <- paste(readLines(tc), collapse="\n")
                # create necessary directories
                write(test, file = testFile, append = T)
            } else {
                dir.create(dirname(result_path), showWarnings = F, recursive = T)
                if (!is.null(result_path)) file.copy(tc, result_path, overwrite = FALSE)
            }
            total_coverage <<- new_total_coverage
        } else {
          if (verbose) cat("Test case ", tc, " didn't increase coverage\n")
          if (remove_tests) file.remove(tc)
        }
        new_total_coverage
    }
    sapply(all.tc, cov_change)
    invisible(NULL)
}
