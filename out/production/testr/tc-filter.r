#' @title Test Case filter based on coverage Report on Specified R Virtual Machine Source Code
#'
#' @description This function works with the GNU coverage tool, gcov, to report code coverage of the
#'  tested R virtual machine. The VM must have been compiled with gcov support and executed at least
#'  once before this function is executed for meanful statistics. 
#'
#' @param tc.root a directory containg test suite
#' @param r.home a directory containing VM.
#' @param source.folder a VM source files directory on which coverage should be measured. Must be inside r.home.
#' @param tc.db.path a directory containing previosly collected test cases.
#' @param clear.previous.coverage wheather to clear accomulated coverage of VM.
#' @param wipe.tc.database wheater delete previously accomulated test cases.
#' @return list(after.tc.coverage.percentage)
#'
library(tools)

filterTCs<- function(tc.root, r.home, source.folder, tc.db.path, tc.result.root, clear.previous.coverage = TRUE, wipe.tc.database = FALSE, use.tc.db = TRUE, k = 1) {
  after.tc.coverage.percentage <- 0
  if (!file.exists("testr/coverage.r") || !file.exists("testr/target.r"))
    stop("Please make sure that current working directory contains testr files")
  source("testr/coverage.r")
  run.script <<- file_path_as_absolute("testr/target.r")
  if (missing(tc.root)) 
    stop('A directory containing Test Cases must be specified!'); 
  if (!file.exists(tc.root))
    stop('Specified directory with Test Cases does not exist!'); 
  if (missing(r.home)) 
    stop('A directory containing VM source files must be specified!'); 
  if (!file.exists(r.home))
    stop('Specified directory of R_HOME does not exist!'); 
  if (missing(source.folder)) 
    stop('A directory containing source files must be specified!'); 
  if (!file.exists(file.path(r.home,source.folder, fsep = .Platform$file.sep)))
    stop('Specified source folder to check coverage does not exist!'); 
  if (missing(tc.db.path)) 
    stop("A directory containing TC DB files must be specified!");
  if (!file.exists(tc.db.path))
    dir.create(tc.db.path)
  if (clear.previous.coverage)
    reset(file.path(r.home, source.folder, fsep = .Platform$file.sep))
  if (wipe.tc.database)
    cleanTCDB(tc.db.path)
  r.home <- file_path_as_absolute(r.home)
  tc.root <- file_path_as_absolute(tc.root)
  if (use.tc.db)
    db.coverage <- measureCoverageByDB(r.home, source.folder, tc.db.path)
  else
    db.coverage <- 0
  cat("TC Root - ", tc.root, "\n")
  all.tc <- list.files(path = tc.root, all.files = TRUE, recursive = TRUE, pattern = "\\.[rR]$")
  indexes <- sapply(all.tc, FUN = determineFileIndex)
  fileIndexMatrix <- matrix(c(indexes, all.tc), nrow=length(all.tc))
  all.tc <- fileIndexMatrix[order(as.numeric(fileIndexMatrix[,1])), 2]
  
  cat("Number of TC Files - ", length(all.tc), "\n")
  filename <- basename(all.tc[1])
  spl <- strsplit(filename, "_")
  if (length(spl[[1]]) == 2)
    function.name <- substr(spl[[1]][2], 1, nchar(spl[[1]][2]) - 2)
  else
    function.name <- spl[[1]][2]

  tc.function.path <- file.path(tc.result.root, function.name, fsep = .Platform$file.sep)
  cat("TC function path in filter - ",tc.function.path, "\n")
  if (!file.exists(tc.function.path))
    dir.create(tc.function.path)
  i <- 1
  coverageChangeMeasureForSingleTCFile <- function(tc) {
    cat(tc, "\n")
    tc.full.path <- file.path(tc.root, tc, fsep = .Platform$file.sep)
    cat(tc.full.path, "\n")
    tc.full.path <- file_path_as_absolute(tc.full.path)
    tc <- basename(tc)
    info.file <- file.path(tc.db.path, paste(function.name,"info", sep = "_"), fsep = .Platform$file.sep)
    sink(info.file, append = TRUE)
    cat(tc, "\n")
    before.tc.coverage.info <<- coverage(root = file.path(r.home, source.folder, fsep = .Platform$file.sep))
    before.tc.coverage.percentage <<- calculateCoverage(before.tc.coverage.info)
    if (is.nan(before.tc.coverage.percentage)) 
      before.tc.coverage.percentage <- 0
    sink()
    cmd <- paste(r.home, 
                   "/bin/Rscript --no-save --no-restore --slave --quiet ", 
                   paste(run.script, shQuote(tc.full.path), sep = " "), 
                   sep = "")
    cmd.output <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    sink(info.file, append = TRUE)
    after.tc.coverage.info <<- coverage(root = file.path(r.home, source.folder, fsep = .Platform$file.sep))
    after.tc.coverage.percentage <<- calculateCoverage(after.tc.coverage.info)
    sink()
    if (after.tc.coverage.percentage > before.tc.coverage.percentage) {
      cat("Coverage before running TC ", i, " from file ", before.tc.coverage.percentage, "\n")
      cat("Coverage after running TC ", i, " from file ", after.tc.coverage.percentage, "\n")
      file.copy(tc.full.path, tc.function.path, overwrite = FALSE)
    }
    i <<- i + 1
    file.remove(tc.full.path)
  }
  result <- Map(coverageChangeMeasureForSingleTCFile, all.tc)
  cat("Coverage gain by TCs - ")
  cat(after.tc.coverage.percentage - db.coverage)
  cat("%\n")
  return (after.tc.coverage.percentage)
}

#' @description Function that measures coverage, returns as a result 2 data frames, 
#' with detailed information of coverage by file and function. 
#' This function preprocesses the result and returns a file percentage coverage. 
#'
#' @param coverage.data 2 dataframes with coverage information 
#' @return total percentage of coverage by line

calculateCoverage <- function(coverage.data) {
  # line file coverage
  totalLine.file <- sum(as.numeric(coverage.data$file$LOC))
  totalCovLine.file <- sum(as.numeric(coverage.data$file$CovLn))
  totalCovLinePcnt.file <- round(totalCovLine.file/totalLine.file * 100, digits = 10)
  # line func coverage
  totalLine.func <- sum(as.numeric(coverage.data$func$LOC))
  totalCovLine.func <- sum(as.numeric(coverage.data$func$CovLn))
  totalCovLinePcnt.func <- round(totalCovLine.func/totalLine.func * 100, digits = 10)
  # func coverage
  totalFunc <- nrow(coverage.data$func)
  totalCovFunc <- nrow(coverage.data$func[as.numeric(coverage.data$func$CovLn) > 0, ])
  totalCovFuncPcnt <- round(totalCovFunc/totalFunc * 100, digits = 2)
  # file coverage
  totalFile <- nrow(coverage.data$file)
  totalCovFile <- nrow(coverage.data$file[as.numeric(coverage.data$file$CovLn) > 0, ])
  totalCovFilePcnt <- round(totalCovFile/totalFile * 100, digits = 2)
  #return(totalCovLinePcnt.file)
  return (totalCovLine.file)
}

#' @description measures coverage by test cases in TC database. 
#' 
#' @param r.home a directory containing VM.
#' @param source.folder a VM source files directory on which coverage should be measured. Must be inside r.home.
#' @param tc.db.path a directory containing previosly collected test cases.
#' @return total percentage of coverage by line after running TCs for database on specified VM

measureCoverageByDB <- function(r.home, source.folder, tc.db.path) {
  if (missing(r.home)) 
    stop('A directory containing VM source files must be specified!'); 
  if (!file.exists(r.home))
    stop('Specified directory of R_HOME does not exist!'); 
  if (missing(source.folder)) 
    stop('A directory containing source files must be specified!'); 
  if (!file.exists(paste(r.home,source.folder, sep="/")))
    stop('Specified source folder to check coverage does not exist!'); 
  if (missing(tc.db.path)) 
    stop("A directory containing TC DB files must be specified!");
  if (!file.exists(tc.db.path))
    stop('Specified directory with database of TCs does not exist!'); 
  if (length(list.files(path = tc.db.path, recursive = TRUE)) == 0){
    cat("TC Database is empty\n")
    return (0)
  }
  tc.db.path <- file_path_as_absolute(tc.db.path)
  sink("db_out")
  before.db.coverage.info <- coverage(root = file.path(r.home, source.folder, fsep = .Platform$file.sep))
  cmd <- paste(r.home, 
               "/bin/Rscript --no-save --no-restore --slave --quiet ", 
               paste(run.script, tc.db.path, sep = " "), 
               sep = "")
  cmd.output <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
  after.db.coverage.info <- coverage(root = file.path(r.home, source.folder, fsep = .Platform$file.sep))
  sink()
#  file.remove("db_out")
  before.db.coverage.percentage <- calculateCoverage(before.db.coverage.info)
  if (is.nan(before.db.coverage.percentage)) 
    before.db.coverage.percentage <- 0
  after.db.coverage.percentage <- calculateCoverage(after.db.coverage.info)
  cat(paste("Coverage by TCs in database ",after.db.coverage.percentage, '\n',sep=""))
  return (after.db.coverage.percentage)
} 

#' @description delete previously accomulated TCs in test case database. 
#' 
#' @param tc.db.path a directory containing previosly collected test cases.

cleanTCDB <- function(tc.db.path){
  if (missing(tc.db.path)) 
    stop("A directory containing TC DB files must be specified!");
  if (!file.exists(tc.db.path))
    stop('Specified directory with database of TCs does not exist!'); 
  cmd <- paste("find", tc.db.path, "-name", '"*.[rR]"', "-delete", sep=" ");
  system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE);
}

determineFileIndex <- function(filename){
  spl <- strsplit(filename, "_")
  if (length(spl[[1]]) == 2)
    index <- 0
  else
    index <- substr(spl[[1]][3], 1, nchar(spl[[1]][3]) - 2)
  return (index)
}

