#' @title Process Test Case file
#' 
#' This function is respinsible for splitting a file into single test cases and filtering out 
#' the unneeded file based or R and C code coverage
#' @param tc.file test case file to process
#' @param tc.result.root folder with resulting filtered test cases
#' @param tc.db folder with test case detabase
#' @param r.home home directory of R VM with C code coverage support
#' @param source.folder folder in R VM to measure C code coverage of
#' @seealso Decorate
#' @export
#' 
process_tc <- function(tc.file, tc.result.root, tc.db = NULL, r.home = NULL, source.folder = NULL) {
  cache$tc.result.root <- tc.result.root
  
  if (!file.exists(tc.result.root))
    dir.create(tc.result.root)
  
  n <- ceiling(get_num_tc(tc.file)/16)
  
  split_tcs(tc.root = tc.file, 
           tc.split.root = cache$temp_dir, 
           number.of.tc.per.file = n)
  
  filter_tcs(tc.root = cache$temp_dir, 
            tc.db.path = tc.db, 
            tc.result.root = tc.result.root,
            r.home = r.home,
            source.folder = source.folder,
            clear.previous.coverage = TRUE, 
            wipe.tc.database = FALSE) 
  
  while(n != 1) {
    n <- ceiling(n/2)
    split_tcs(tc.root = tc.result.root, 
             tc.split.root = cache$temp_dir, 
             number.of.tc.per.file = n) 
    file.remove(list.files(tc.result.root, recursive = T, full.names = T))
    filter_tcs(tc.root = cache$temp_dir, 
              tc.db.path = tc.db, 
              tc.result.root = tc.result.root,
              r.home = r.home,
              source.folder = source.folder,
              clear.previous.coverage = TRUE, 
              wipe.tc.database = FALSE) 
  }
}

#' @title Split TestCase files
#'
#' This function takes a test cases files and splits them according to maximum number of tests per file 
#' @param tc.root test case file/folder to split
#' @param tc.split.root resulting location of split
#' @param number.of.tc.per.file maximum number of test cases per file
#' @seealso process_tc
#'
split_tcs<- function(tc.root, tc.split.root, number.of.tc.per.file = 1) {
  # In case tc is diretory, recursively call this function on all files in directory
  if (file.info(tc.root)$isdir){
    all.tc <- list.files(tc.root, 
                         recursive=TRUE, 
                         all.files = TRUE, 
                         pattern = "\\.[rR]$", 
                         full.names = T)
    for (test.case in all.tc)
      split_tcs(test.case, tc.split.root, number.of.tc.per.file)
    return(invisible())
  }
  
  function.name <- get_function_name(basename(tc.root))
  tc.split.root <- file.path(tc.split.root, function.name)
  if (!file.exists(tc.split.root)) 
    dir.create(tc.split.root)
  file.index <- length(list.files(tc.split.root))
  
  lines <- readLines(tc.root)
  if (length(lines) == 0)
    stop("Empty file\n")
  
  # based on cat that each test case has line break before it
  tests.starts <- head(c(1, grep("^[ ]*$", lines) + 1), -1)
  if (length(tests.starts) == 0) tests.starts <- head(c(1, grep("^[ ]*$", lines) + 1), -1)
  tests.ends <- grep("^[ ]*$", lines)
  if (length(tests.ends) == 0) tests.ends <- grep("^[ ]*$", lines)
  
  if (testr_options('verbose')) {
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
          file.path(tc.split.root,
                    paste0("tc_", function.name, "_", ntc + file.index, ".R")),
          append = T)  
    if (i %% number.of.tc.per.file == 0) ntc <- ntc + 1
  } 
}

#' @title Test Case filter based on cov Report on Specified R Virtual Machine Source Code
#'
#' @description This function works with the GNU cov tool, gcov, to report code cov of the
#'  tested R virtual machine. The VM must have been compiled with gcov support and executed at least
#'  once before this function is executed for meanful statistics. 
#'
#' @param tc.root a directory containg test suite
#' @param tc.db.path a directory containing previosly collected test cases.
#' @param clear.previous.cov wheather to clear accomulated cov of VM.
#' @param wipe.tc.database wheater delete previously accomulated test cases.
#' @export
#' 
filter_tcs<- function(tc.root, tc.result.root, tc.db.path = NULL, 
                     r.home = NULL, source.folder = NULL,
                     clear.previous.coverage = TRUE, 
                     wipe.tc.database = FALSE, 
                     verbose = testr_options('verbose')) {
  cache$r.home <- r.home
  cache$source.folder <- source.folder
  # parameter checks
  if (missing(tc.root) || !file.exists(tc.root)) stop('Specified directory with Test Cases does not exist!'); 
  if (clear.previous.coverage) clear_gcov(file.path(cache$r.home, cache$source.folder))
  if (wipe.tc.database) clean_tcdb(tc.db.path)
  
  db.cov <- measure_cov(tc.db.path)
  
  if (verbose) cat("TC Root - ", tc.root, "\n")
  all.tc <- list.files(path = tc.root, all.files = TRUE, recursive = TRUE, pattern = "\\.[rR]$")
  if (verbose) cat("Number of TC Files - ", length(all.tc), "\n")
  
  function.name <- get_function_name(basename(all.tc[1]))
  tc.function.path <- file.path(tc.result.root, function.name)
  if (!file.exists(tc.function.path)) dir.create(tc.function.path)
  if (verbose) cat("TC function path in filter - ",tc.function.path, "\n")

  cache$i <- 1
  covChangeMeasureForSingleTCFile <- function(tc) {
    tc.full.path <- file.path(tc.root, tc)
    info.file <- file.path(tc.root, paste(function.name,"info", sep = "_"))
    before.tc.cov.c <- 0
    before.tc.cov.info <- tryCatch(measure_gcov(root = file.path(cache$r.home, cache$source.folder), verbose=FALSE), 
                                     error=function(x) list(file.pcn = 0))
    before.tc.cov.c <- before.tc.cov.info$file.pcn
    before.tc.cov.r <- rcov::ReportCoveragePercentage(readRDS(file.path(cache$temp_dir, "cov.data.RData")))
    cov.data <- run_testsMeasureCoverage(tc.full.path)
    after.tc.cov.c <- cov.data$c
    after.tc.cov.r <- cov.data$r

    if (after.tc.cov.c > before.tc.cov.c || after.tc.cov.r > before.tc.cov.r) {
      cat("C code coverage before running TC ", cache$i, " from file ", before.tc.cov.c, "\n")
      cat("C code coverage after running TC ", cache$i, " from file ", after.tc.cov.c, "\n")
      cat("R code coverage before running TC ", cache$i, " from file ", before.tc.cov.r, "\n")
      cat("R code coverage after running TC ", cache$i, " from file ", after.tc.cov.r, "\n")
      file.copy(tc.full.path, tc.function.path, overwrite = FALSE)
    } else {
      cat("Test case ", cache$i, "didn't increase coverage\n")
    }
    cache$i <- cache$i + 1
    file.remove(tc.full.path)
    cov.data
  }
  result <- Map(covChangeMeasureForSingleTCFile, all.tc)
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
#' @param tc.full.path path of test.cases
#' @param funcs R functions to measure coverage for
run_tests_cov <- function(tc.full.path, funcs) {
  tmp_source <- file.path(cache$temp_dir, "tmp_source.R")
  cov.info <- file.path(cache$temp_dir, "cov.data.p.RData")
  if (is.null(tc.full.path))
    tc.full.path <- ""
  if (missing(funcs)) {
    funcs <- vector()
    for (tc in tc.full.path) 
      funcs <- c(funcs, get_function_name(basename(tc)))
  }
  command <- sprintf(rtemplate, 
                     tools::file_path_as_absolute(cache$temp_dir), 
                     paste(deparse(funcs), collapse=""), 
                     tc.full.path, cov.info)
  writeChar(con = tmp_source, command, eos = NULL)
  RCMD <- ifelse(is.null(cache$r.home), "R", file.path(cache$r.home, "bin/R"))
  cmd <- paste(RCMD,
               " CMD BATCH --vanilla --slave -q ", 
               tmp_source,
               sep = "")
  cmd.output <- system(cmd, intern = TRUE, ignore.stderr = F)
  after.tc.cov.r <- readRDS(file = cov.info)
  after.tc.cov.info.gcov <- measure_gcov(root = file.path(cache$r.home, cache$source.folder), verbose = FALSE)
  after.tc.cov.c <- after.tc.cov.info.gcov$file.pcn
  file.remove(tmp_source)
  list(r=after.tc.cov.r, c=after.tc.cov.c)
}
#' @title measure cov by database
#' @description measures cov by test cases in TC database. 
#' 
#' @param r.home a directory containing VM.
#' @param source.folder a VM source files directory on which cov should be measured. Must be inside r.home.
#' @param tc.db.path a directory containing previosly collected test cases.
#' @return total percentage of cov by line after running TCs for database on specified VM
measure_cov <- function(tc.db.path) {
  # clear previous information
  cov.data <- file.path(cache$temp_dir, 'cov.data.RData')
  suppressWarnings(file.remove(cov.data))
  
  if (is.null(tc.db.path))
    tc.db.path <- ""
  out <- capture.output(
    db.cov.info <- run_testsMeasureCoverage(tc.db.path, funcs = builtins()) 
  )
  db.cov.c <- db.cov.info$c
  db.cov.r <- db.cov.info$r
  cat(paste("C Code coverage by TCs in database ", db.cov.c, '\n',sep=""))
  cat(paste("R Code coverage by TCs in database ", db.cov.r, '\n',sep=""))
  db.cov.info
}
