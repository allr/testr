# intended measuring coverage and evaluationg testcases
library(tools)

calculateCoverage <- function(coverage.data) {
  # line file coverage
  totalLine.file <- sum(as.numeric(coverage.data$file$LOC))
  totalCovLine.file <- sum(as.numeric(coverage.data$file$CovLn))
  totalCovLinePcnt.file <- round(totalCovLine.file/totalLine.file * 100, digits = 2)
  # line func coverage
  totalLine.func <- sum(as.numeric(coverage.data$func$LOC))
  totalCovLine.func <- sum(as.numeric(coverage.data$func$CovLn))
  totalCovLinePcnt.func <- round(totalCovLine.func/totalLine.func * 100, digits = 2)
  # func coverage
  totalFunc <- nrow(coverage.data$func)
  totalCovFunc <- nrow(coverage.data$func[as.numeric(coverage.data$func$CovLn) > 0, ])
  totalCovFuncPcnt <- round(totalCovFunc/totalFunc * 100, digits = 2)
  # file coverage
  totalFile <- nrow(coverage.data$file)
  totalCovFile <- nrow(coverage.data$file[as.numeric(coverage.data$file$CovLn) > 0, ])
  totalCovFilePcnt <- round(totalCovFile/totalFile * 100, digits = 2)
  return(totalCovLinePcnt.file)
}

filterTCs<- function(tc.root, r.home, source.folder, tc.db.path, clear.previous.coverage = TRUE, wipe.tc.database = FALSE) {
  if (!file.exists("testr/coverage.R") || !file.exists("testr/run.r"))
    stop("Please make sure that current working directory contains testr files")
  source("testr/coverage.R")
  run.script <<- file_path_as_absolute("testr/run.r")
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
  if (!file.exists(paste(r.home,source.folder, sep="/")))
    stop('Specified source folder to check coverage does not exist!'); 
  if (missing(tc.db.path)) 
    stop("A directory containing TC DB files must be specified!");
  if (!file.exists(tc.db.path))
    stop('Specified directory with database of TCs does not exist!'); 
  if (clear.previous.coverage)
    reset(paste(r.home, source.folder, sep = ""))
  if (wipe.tc.database)
    cleanTCDB(tc.db.path)
  r.home <- file_path_as_absolute(r.home)
  tc.root <- file_path_as_absolute(tc.root)
  measureCoverageByDB(r.home, source.folder, tc.db.path)
  all.tc <- list.files(path = tc.root, recursive = TRUE)
  coverageChangeMeasureForSingleTCFile <- function(tc) {
    sink(tc)
    tc.full.path <- paste(tc.root, tc, sep = "/")
    before.tc.coverage.info <- coverage(root = paste(r.home, source.folder, sep = "/"))
    cmd <- paste(r.home, 
                 "/bin/Rscript --no-restore --slave --quiet ", 
                 paste(run.script, tc.full.path, sep = " "), 
                 sep = "")
    cmd.output <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    after.tc.coverage.info <- coverage(root = paste(r.home, source.folder, sep = "/"))
    sink()
    file.remove(tc)
    before.tc.coverage.percentage <- calculateCoverage(before.tc.coverage.info)
    if (is.nan(before.tc.coverage.percentage)) 
      before.tc.coverage.percentage <- 0
    after.tc.coverage.percentage <- calculateCoverage(after.tc.coverage.info)
    cat(paste("File ",tc,"\n", sep=""))
    cat(paste("Coverage before running TCs from file ", before.tc.coverage.percentage, "\n", sep=""))
    cat(paste("Coverage after running TCs from file ", after.tc.coverage.percentage, "\n", sep=""))
    if (after.tc.coverage.percentage > before.tc.coverage.percentage) {
      tc.db.file <- paste(tc.db.path, tc, sep = "/")
      if (!file.exists(tc.db.file)) {
        file.create(tc.db.file)
      }
      file.append(tc.db.file, tc.full.path)
    }
  }
  result <- Map(coverageChangeMeasureForSingleTCFile, all.tc)
}

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
    return ()
  }
  tc.db.path <- file_path_as_absolute(tc.db.path)
  sink("db_out")
  before.db.coverage.info <- coverage(root = paste(r.home, source.folder, sep = "/"))
  cmd <- paste(r.home, 
               "/bin/Rscript --no-restore --slave --quiet ", 
               paste(run.script, tc.db.path, sep = " "), 
               sep = "")
  cat(cmd)
  cmd.output <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
  after.db.coverage.info <- coverage(root = paste(r.home, source.folder, sep = "/"))
  sink()
  file.remove("db_out")
  before.db.coverage.percentage <- calculateCoverage(before.db.coverage.info)
  if (is.nan(before.db.coverage.percentage)) 
    before.db.coverage.percentage <- 0
  after.db.coverage.percentage <- calculateCoverage(after.db.coverage.info)
  cat(paste("Coverage by TCs in database ",after.db.coverage.percentage, '\n',sep=""))
} 

cleanTCDB <- function(tc.db.path){
  if (missing(tc.db.path)) 
    stop("A directory containing TC DB files must be specified!");
  if (!file.exists(tc.db.path))
    stop('Specified directory with database of TCs does not exist!'); 
  cmd <- paste("find", tc.db.path, "-name", "*.[rR]", "-delete", sep=" ");
  system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE);
}