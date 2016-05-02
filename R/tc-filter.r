#' @title Process Test Case file
#'
#' This function is respinsible for splitting a file into single test cases and filtering out
#' the unneeded file based or R and C code coverage
#' @param tc.file test case file to process
#' @param tc_result_root folder with resulting filtered test cases
#' @param tc.db folder with test case detabase
#' @param r.home home directory of R VM with C code coverage support
#' @param source.folder folder in R VM to measure C code coverage of
#' @seealso Decorate
#' @export
#'
process_tc <- function(tc.file,
                       tc_result_root,
                       tc.db = NULL,
                       r.home = NULL,
                       source.folder = NULL) {
    cache$tc_result_root <- tc_result_root

    if (!file.exists(tc_result_root)) {
        dir.create(tc_result_root)
    }
    n <- ceiling(get_num_tc(tc.file) / 16)

    split_tcs(tc.root = tc.file,
              tc_split_root = cache$temp_dir,
              ntc_file = n)

    filter_tcs(tc.root = cache$temp_dir,
               tc_db_path = tc.db,
               tc_result_root = tc_result_root,
               r.home = r.home,
               source.folder = source.folder,
               clear_cov = TRUE,
               wipe_tc_db = FALSE)

    while(n != 1) {
        n <- ceiling(n / 2)
        split_tcs(tc.root = tc_result_root,
                  tc_split_root = cache$temp_dir,
                  ntc_file = n)
        file.remove(list.files(tc_result_root, recursive = T, full.names = T))
        filter_tcs(tc.root = cache$temp_dir,
                   tc_db_path = tc.db,
                   tc_result_root = tc_result_root,
                   r.home = r.home,
                   source.folder = source.folder,
                   clear_cov = TRUE,
                   wipe_tc_db = FALSE)
    }
}

#' @title Split TestCase files
#'
#' This function takes a test cases files and splits them according to maximum number of tests per file
#' @param tc.root test case file/folder to split
#' @param tc_split_root resulting location of split
#' @param ntc_file maximum number of test cases per file
#' @seealso process_tc
#'
split_tcs <- function(tc.root, tc_split_root, ntc_file = 1) {
    # In case tc is diretory, recursively call this function on all files in directory
    if (file.info(tc.root)$isdir){
        all.tc <- list.files(tc.root,
                             recursive=TRUE,
                             all.files = TRUE,
                             pattern = "\\.[rR]$",
                             full.names = T)
        for (test.case in all.tc)
            split_tcs(test.case, tc_split_root, ntc_file)
        return(invisible())
    }

    function.name <- get_function_name(basename(tc.root))
    tc_split_root <- file.path(tc_split_root, function.name)
    if (!file.exists(tc_split_root))
        dir.create(tc_split_root)
    file.index <- length(list.files(tc_split_root))

    lines <- readLines(tc.root)
    if (length(lines) == 0)
        stop("Empty file\n")

    # based on cat that each test case has line break before it
    tests.starts <- head(c(1, grep("^[ ]*$", lines) + 1), -1)
    if (length(tests.starts) == 0) tests.starts <- head(c(1, grep("^[ ]*$", lines) + 1), -1)
    tests.ends <- grep("^[ ]*$", lines)
    if (length(tests.ends) == 0) tests.ends <- grep("^[ ]*$", lines)
    if (testr_options("verbose")) {
        cat("File ", tc.root, "\n")
        cat("Number of TCs in file - ", length(tests.starts), "\n")
    }

    # bulk fines together and then write it
    res_list <- list()
    for (i in 1:length(tests.starts))
        res_list[[i]] <- lines[tests.starts[i]:tests.ends[i]]
    ntc <- 1
    for (i in 1:length(res_list)) {
        write(res_list[[i]],
              file.path(tc_split_root,
                        paste0("tc_", function.name, "_", ntc + file.index, ".R")),
              append = T)
        if (i %% ntc_file == 0) ntc <- ntc + 1
    }
}

#' @title Test Case filtering based on coverage
#'
#' @description This function attemps to filter the test cases for given functions based on
#' code coverage information collected by covr.
#'
#' @param tc_root directory with test cases to filter
#' @param tc_result_root directory where the filtered test cases should be copied
#' @param remove_tests if to delete test cases that don't affect coverage from tc_root
#' @param verbose if to show additional infomation during filtering
#'
filter <- function(tc_root, tc_result_root, functions, package_path, remove_tests = FALSE, verbose = testr_options("verbose")) {
    require(covr)
    if (missing(tc_root) || !file.exists(tc_root))
        stop("Specified directory with Test Cases does not exist!")
    if (missing(functions) && missing(package_path)) {
        stop("Neither list of functions or package path to measure coverage by was specified")
    } else if (!missing(package_path)) {
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
        total_coverage <- covr::package_coverage(package_path, type = "none")
    } else {
        total_coverage <- sapply(names(functions), function(fname) {
            function_coverage(fname, 1)
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
                function_coverage(fname, code = quote(testthat::test_file(tc)))
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
                                     percent_coverage(new_total_coverage) - percent_coverage(total_coverage) > 0,
                                     any(sapply(names(functions),function(fname) {
                                         percent_coverage(new_total_coverage[[fname]]) - percent_coverage(total_coverage[[fname]])
                                     }) > 0))
        if (coverage_increased) {
            if (verbose) cat("Test case ", tc, " increased the coverage\n")
            if (!is.null(result_path)) file.copy(tc, result_path, overwrite = FALSE)
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

