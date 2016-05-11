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
#' @param compact If TRUE, after filtering, tests will be compacted to a file per function
#' @param verbose if to show additional infomation during filtering
#'
filter_tests <- function(tc_root, tc_result_root, functions, package_path, remove_tests = FALSE, compact = FALSE, verbose = testr_options("verbose")) {
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
        total_coverage <- covr::package_coverage(package_path, type = "tests")
    } else {
        total_coverage <- sapply(names(functions), function(fname) {
            covr::function_coverage(fname, 1)
        }, simplify = FALSE)
    }
    cov_change <- function(tc) {
        if (!is.null(tc_result_root)) result_path <- gsub(tc_root, tc_result_root, tc)
        sink(tempfile())
        if (is_package) {
            code <- sprintf("\ntestthat::test_file('%s')", tools::file_path_as_absolute(tc))
            test_coverage <- covr::package_coverage(package_path,
                                                    type = "none",
                                                    code = code)
        } else {
            test_coverage <- sapply(names(functions), function(fname) {
                covr::function_coverage(fname, code = quote(testthat::test_file(tc)))
            }, simplify = FALSE)
        }
        sink()

        if (is_package) {
            new_total_coverage <- covr:::merge_coverage(list(test_coverage, total_coverage))
        } else {
            new_total_coverage <- sapply(names(functions), function(fname) {
                covr:::merge_coverage(list(test_coverage[[fname]], total_coverage[[fname]]))
            }, simplify = FALSE)
        }

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
                write(test, file = testFile, append = T)
            } else {
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
