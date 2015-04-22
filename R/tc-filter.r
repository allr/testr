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
#' @return list(after.tc.cov.percentage)
#' 
FilterTCs<- function(tc.root, tc.result.root, tc.db.path = "", 
                     r.home = "", source.folder = "",
                     clear.previous.coverage = TRUE, 
                     wipe.tc.database = FALSE, 
                     verbose = testrOptions('verbose')) {
  cache$r.home <- r.home
  cache$source.folder <- source.folder
  # parameter checks
  if (missing(tc.root) || !file.exists(tc.root)) stop('Specified directory with Test Cases does not exist!'); 
  if (clear.previous.coverage) ResetGCovInfo(file.path(cache$r.home, cache$source.folder))
  if (wipe.tc.database) CleanTCDB(tc.db.path)
  
  db.cov <- MeasureCoverageByDB(tc.db.path)
  
  if (verbose) cat("TC Root - ", tc.root, "\n")
  all.tc <- list.files(path = tc.root, all.files = TRUE, recursive = TRUE, pattern = "\\.[rR]$")
  if (verbose) cat("Number of TC Files - ", length(all.tc), "\n")
  
  function.name <- GetFunctionName(basename(all.tc[1]))
  tc.function.path <- file.path(tc.result.root, function.name)
  if (!file.exists(tc.function.path)) dir.create(tc.function.path)
  if (verbose) cat("TC function path in filter - ",tc.function.path, "\n")

  cache$i <- 1
  covChangeMeasureForSingleTCFile <- function(tc) {
    tc.full.path <- file.path(tc.root, tc)
    info.file <- file.path(tc.root, paste(function.name,"info", sep = "_"))
    out <- capture.output(
      before.tc.cov.info <- tryCatch(MeasureCoverage(root = file.path(cache$r.home, cache$source.folder)), 
                                     error=function(x) 0)
    )
    before.tc.cov.c <- ifelse(length(before.tc.cov.info) > 1, calculateCoverage(before.tc.cov.info), 0)
    if (is.nan(before.tc.cov.c)) 
      before.tc.cov.c <- 0
    before.tc.cov.r <- rcov::ReportCoveragePercentage(readRDS(file.path(cache$temp_dir, "cov.data")))
    cov.data <- RunTestsMeasureCoverage(tc.full.path)
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
}

rtemplate <- "library(rcov)
library(testr)
tmp_folder <- '%s'
cov.data <- file.path(tmp_folder, 'cov.data')
cov.data.clean <- file.path(tmp_folder, 'cov.data.clean')
cov.funcs <- file.path(tmp_folder, 'cov.funcs')
r.func <- %s
PauseMonitorCoverage()
if (file.exists(cov.funcs)){
  cov.funcs <- readRDS(cov.funcs)
  for (func in ls(cov.funcs)){
    rcov:::reassignInEnv(func, cov.funcs[[func]], getNamespace('base'))
  }
  if (file.exists(cov.data)) cov.data <- readRDS(cov.data) else 
  if (file.exists(cov.data.clean)) cov.data <- readRDS(cov.data.clean) else cov.data <- new.env()
  rcov:::reassignInEnv('cov.cache', cov.data, getNamespace('rcov'))
} else {
  for (func in r.func) {
    cat(func, '\n')
    MonitorCoverage(func)
  }
  saveRDS(rcov:::cov.cache, cov.data.clean)
  saveRDS(rcov:::cov.funcs, cov.funcs)
}
RunTests('%s', use.rcov = T)
saveRDS(ReportCoveragePercentage(), '%s') 
saveRDS(rcov:::cov.cache, file.path(tmp_folder, 'cov.data'))"

#' @title Run Tests and Measure Coverage 
#' 
#' @param tc.full.path path of test.cases
#' @param funcs R functions to measure coverage for
RunTestsMeasureCoverage <- function(tc.full.path, funcs) {
  tmp_source <- file.path(cache$temp_dir, "tmp_source")
  cov.info <- file.path(cache$temp_dir, "cov.data.p")
  if (is.null(tc.full.path))
    tc.full.path <- ""
  if (missing(funcs)) {
    funcs <- vector()
    for (tc in tc.full.path) 
      funcs <- c(funcs, GetFunctionName(basename(tc)))
  }
  command <- sprintf(rtemplate, 
                     tools::file_path_as_absolute(cache$temp_dir), 
                     paste(deparse(funcs), collapse=""), 
                     tc.full.path, cov.info)
  writeChar(con = tmp_source, command, eos = NULL)
  RCMD <- ifelse(cache$r.home == "", "R", file.path(cache$r.home, "bin/R"))
  cmd <- paste(RCMD,
               " CMD BATCH --vanilla --slave -q ", 
               tmp_source,
               sep = "")
  cmd.output <- system(cmd, intern = TRUE, ignore.stderr = F)
  after.tc.cov.r <- readRDS(file = cov.info)
  after.tc.cov.info.gcov <- MeasureGCovCoverage(root = file.path(cache$r.home, cache$source.folder), verbose = FALSE)
  after.tc.cov.c <- after.tc.cov.info.gcov$file.pcn
  list(r=after.tc.cov.r, c=after.tc.cov.c)
}

#' @title measure cov by database
#' @description measures cov by test cases in TC database. 
#' 
#' @param r.home a directory containing VM.
#' @param source.folder a VM source files directory on which cov should be measured. Must be inside r.home.
#' @param tc.db.path a directory containing previosly collected test cases.
#' @return total percentage of cov by line after running TCs for database on specified VM
MeasureCoverageByDB <- function(tc.db.path) {
  file.remove(file.path(cache$temp_dir, "cov.data"))
  out <- capture.output(
    db.cov.info <- RunTestsMeasureCoverage(tc.db.path, funcs = builtins(T)) 
  )
  db.cov.c <- db.cov.info$c
  db.cov.r <- db.cov.info$r
  cat(paste("C Code coverage by TCs in database ", db.cov.c, '\n',sep=""))
  cat(paste("R Code coverage by TCs in database ", db.cov.r, '\n',sep=""))
  db.cov.info
} 

#' @title clean database
#' @description delete previously accomulated TCs in test case database. 
#' 
#' @param tc.db.path a directory containing previosly collected test cases.
CleanTCDB <- function(tc.db.path){
  if (missing(tc.db.path)) 
    stop("A directory containing TC DB files must be specified!");
  if (!file.exists(tc.db.path))
    stop('Specified directory with database of TCs does not exist!'); 
  cmd <- paste("find", tc.db.path, "-name", '"*.[rR]"', "-delete", sep=" ");
  system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE);
}