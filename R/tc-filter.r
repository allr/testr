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
#'  tested R virtual machine. The VM must have been compiled with gcov support and executed at least
#'  once before this function is executed for meanful statistics. 
#'
#' @param tc.root a directory containg test suite
#' @param tc_db_path a directory containing previosly collected test cases.
#' @param clear.previous.cov wheather to clear accomulated cov of VM.
#' @param wipe_tc_db wheater delete previously accomulated test cases.
#' @export
#' 
filter_tcs <- function(tc.root, tc_result_root, tc_db_path = NULL,
                       r.home = NULL, source.folder = NULL,
                       clear_cov = TRUE,
                       wipe_tc_db = FALSE,
                       verbose = testr_options("verbose")) {
    cache$r.home <- r.home
    cache$source.folder <- source.folder
    # parameter checks
    if (missing(tc.root) || !file.exists(tc.root))
        stop("Specified directory with Test Cases does not exist!")
    if (clear_cov) clear_gcov(file.path(cache$r.home, cache$source.folder))
    if (wipe_tc_db) clean_tcdb(tc_db_path)
    db.cov <- measure_cov(tc_db_path)
    if (verbose) cat("TC Root - ", tc.root, "\n")
    all.tc <- list.files(path = tc.root, all.files = TRUE, recursive = TRUE, pattern = "\\.[rR]$")
    if (verbose) cat("Number of TC Files - ", length(all.tc), "\n")
    function.name <- get_function_name(basename(all.tc[1]))
    tc_fun_path <- file.path(tc_result_root, function.name)
    if (!file.exists(tc_fun_path)) dir.create(tc_fun_path)
    if (verbose) cat("TC function path in filter - ",tc_fun_path, "\n")

    cache$i <- 1
    cov_change <- function(tc) {
        tc_full_path <- file.path(tc.root, tc)
        before_tc_cov_c <- 0
        before_tc_cov <- tryCatch(measure_gcov(root = file.path(cache$r.home, cache$source.folder), verbose=FALSE),
                                  error=function(x) list(file.pcn = 0))
        before_tc_cov_c <- before_tc_cov$file.pcn
        before_tc_cov_r <- rcov::ReportCoveragePercentage(readRDS(file.path(cache$temp_dir, "cov.data.RData")))
        cov.data <- run_tests_cov(tc_full_path)
        after_tc_cov_c <- cov.data$c
        after_tc_cov_r <- cov.data$r

        if (after_tc_cov_c > before_tc_cov_c || after_tc_cov_r > before_tc_cov_r) {
            cat("C code coverage before running TC ", cache$i, " from file ", before_tc_cov_c, "\n")
            cat("C code coverage after running TC ", cache$i, " from file ", after_tc_cov_c, "\n")
            cat("R code coverage before running TC ", cache$i, " from file ", before_tc_cov_r, "\n")
            cat("R code coverage after running TC ", cache$i, " from file ", after_tc_cov_r, "\n")
            file.copy(tc_full_path, tc_fun_path, overwrite = FALSE)
        } else {
            cat("Test case ", cache$i, "didn't increase coverage\n")
        }
        cache$i <- cache$i + 1
        file.remove(tc_full_path)
        cov.data
    }
    result <- Map(cov_change, all.tc)
    final.cov <- result[[length(result)]]
    cat("C coverage gain by TCs - ", final.cov$c - db.cov$c, "\n")
    cat("R coverage gain by TCs - ", final.cov$r - db.cov$r, "\n")
    clean_temp()
}

rtemplate <- "library(rcov)
library(testr)
tmp_folder <- '%s'
cov.data <- file.path(tmp_folder, 'cov.data.RData')
cov.data.clean <- file.path(tmp_folder, 'cov.data.clean.RData')
cov.funcs <- file.path(tmp_folder, 'cov.funcs.RData')
r.func <- %s
PauseMonitorCoverage()
if (file.exists(cov.funcs)){
  cov.funcs <- readRDS(cov.funcs)
  for (func in ls(cov.funcs)){
    rcov:::reassing_in_env(func, cov.funcs[[func]], getNamespace('base'))
  }
  if (file.exists(cov.data)) cov.data <- readRDS(cov.data) else
  if (file.exists(cov.data.clean)) cov.data <- readRDS(cov.data.clean) else cov.data <- new.env()
  rcov:::reassing_in_env('cov.cache', cov.data, getNamespace('rcov'))
} else {
  for (func in r.func) {
    cat(func, '\n')
    MonitorCoverage(func)
  }
  saveRDS(rcov:::cov.cache, cov.data.clean)
  saveRDS(rcov:::cov.funcs, cov.funcs)
}
run_tests('%s', use.rcov = T)
saveRDS(ReportCoveragePercentage(), '%s')
saveRDS(rcov:::cov.cache, file.path(tmp_folder, 'cov.data.RData'))"

#' @title Run Tests and Measure Coverage 
#' 
#' @param tc_full_path path of test.cases
#' @param funcs R functions to measure coverage for
run_tests_cov <- function(tc_full_path, funcs) {
    tmp_source <- file.path(cache$temp_dir, "tmp_source.R")
    cov.info <- file.path(cache$temp_dir, "cov.data.p.RData")
    if (is.null(tc_full_path))
        tc_full_path <- ""
    if (missing(funcs)) {
        funcs <- vector()
        for (tc in tc_full_path) {
            funcs <- c(funcs, get_function_name(basename(tc)))
        }
    }
    command <- sprintf(rtemplate,
                       tools::file_path_as_absolute(cache$temp_dir),
                       paste(deparse(funcs), collapse=""),
                       tc_full_path, cov.info)
    writeChar(con = tmp_source, command, eos = NULL)
    RCMD <- ifelse(is.null(cache$r.home), "R", file.path(cache$r.home, "bin/R"))
    cmd <- paste(RCMD,
                 " CMD BATCH --vanilla --slave -q ",
                 tmp_source,
                 sep = "")
    system(cmd, intern = TRUE, ignore.stderr = F)
    after_tc_cov_r <- readRDS(file = cov.info)
    after_tc_cov_gcov <- measure_gcov(root = file.path(cache$r.home, cache$source.folder), verbose = FALSE)
    after_tc_cov_c <- after_tc_cov_gcov$file.pcn
    file.remove(tmp_source)
    list(r=after_tc_cov_r, c=after_tc_cov_c)
}
#' @title measure cov by database
#' @description measures cov by test cases in TC database. 
#' 
#' @param r.home a directory containing VM.
#' @param source.folder a VM source files directory on which cov should be measured. Must be inside r.home.
#' @param tc_db_path a directory containing previosly collected test cases.
#' @return total percentage of cov by line after running TCs for database on specified VM
measure_cov <- function(tc_db_path) {
    # clear previous information
    cov.data <- file.path(cache$temp_dir, "cov.data.RData")
    suppressWarnings(file.remove(cov.data))
    if (is.null(tc_db_path))
        tc_db_path <- ""
    out <- capture.output(
        db_cov <- run_tests_cov(tc_db_path, funcs = builtins())
    )
    db_cov_c <- db_cov$c
    db_cov_r <- db_cov$r
    cat(paste("C Code coverage by TCs in database ", db_cov_c, "\n", sep=""))
    cat(paste("R Code coverage by TCs in database ", db_cov_r, "\n", sep=""))
    db_cov
}
