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

#' @title Test Case filter based on cov Report on Specified R Virtual Machine Source Code
#'
#' @description This function works with the GNU cov tool, gcov, to report code cov of the
#'	tested R virtual machine. The VM must have been compiled with gcov support and executed at least
#'	once before this function is executed for meanful statistics.
#'
#' @param tc.root a directory containg test suite
#' @param tc_result_root a directory where filtered test cases should be store.
#' If not specified, tests that don't increase coverate will be removed from tc_root
#' @param clear.previous.cov wheather to clear accomulated cov of VM.
#' @param wipe_tc_db wheater delete previously accomulated test cases.
#'
filter_tests <- function(tc_root, tc_result_root, remove_tests = FALSE, verbose = testr_options("verbose"), ...) {
    if (missing(tc_root) || !file.exists(tc_root))
        stop("Specified directory with Test Cases does not exist!")
    if (verbose) cat("Test cases root - ", tc_root, "\n")
    all.tc <- list.files(path = tc_root, all.files = TRUE, recursive = TRUE, pattern = "\\.[rR]$", full.names = TRUE)
    if (verbose) cat("Number of test cases - ", length(all.tc), "\n")
    functions <- parseFunctionNames(...)
    # create dummy objects
    total_coverage <- lapply(functions, function(fname) {
        f <- fname["name"]
        covr::function_coverage(f, 1)
        })
    cov_change <- function(tc) {
        if (!is.null(tc_result_root)) {
            result_path <- gsub(tc_root, tc_result_root, tc)
        }
        # test how this will work with compiled code
        test_coverage <- lapply(functions, function(fname) {
            f <- fname["name"]
            covr::function_coverage(f, code = quote(testthat::test_file(tc)))
            })
        new_total_coverage <- lapply(functions, function(func) {
            f <- func["name"]
            covr:::merge_coverage(list(test_coverage[[f]], total_coverage[[f]]))
            })
        if (any(sapply(functions, function(fname) {
            f <- fname["name"]
            covr::percent_coverage(new_total_coverage[[f]]) - covr::percent_coverage(total_coverage[[f]])
            }) > 0)) {
            cat("Test case ", tc, " increased the coverage")
            if (!is.null(result_path)) {
                file.copy(tc, result_path, overwrite = FALSE)
            }
            total_coverage <<- new_total_coverage
        } else {
          cat("Test case ", cache$i, "didn't increase coverage\n")
          if (remove_tests) {
            file.remove(tc_full_path)
          }
        }
    }
    Map(cov_change, all.tc)
}
