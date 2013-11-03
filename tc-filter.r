# intended measuring coverage and evaluationg testcases
library(tools)
source("testr/coverage.R")
run.script <- "testr/run.r"

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
  totalCovFunc <- nrow(coverage.data$func[as.numeric(coverage.data$func$CovLn) > 
                                            0, ])
  totalCovFuncPcnt <- round(totalCovFunc/totalFunc * 100, digits = 2)
  # file coverage
  totalFile <- nrow(coverage.data$file)
  totalCovFile <- nrow(coverage.data$file[as.numeric(coverage.data$file$CovLn) > 
                                            0, ])
  totalCovFilePcnt <- round(totalCovFile/totalFile * 100, digits = 2)
  return(totalCovLinePcnt.file)
}

filterTCs<- function(tc.root, r.home, source.folder, tc.db.path) {
  if (missing(tc.root)) 
    stop('A directory containing Test Cases must be specified!'); 
  if (missing(r.home)) 
    stop('A directory containing VM source files must be specified!'); 
  if (missing(source.folder)) 
    stop('A directory containing source files must be specified!'); 
  if (missing(tc.db.path)) 
    stop("A directory containing TC DB files must be specified!");
  #Clear Previous Coverage Data
  #reset(paste(r.home, source.folder, sep = ""))
  
  r.home <- file_path_as_absolute(r.home)
  tc.root <- file_path_as_absolute(tc.root)
  measureCoverageByDB(r.home, source.folder, tc.db.path)
  
  all.tc <- list.files(path = tc.root, recursive = TRUE)
  coverageChangeMeasureForSingleTCFile <- function(tc) {
    sink(paste("testr/tc_cov_data/", tc, ".info", sep = ""))
    tc.full.path <- paste(tc.dir.path, tc, sep = "/")
    before.tc.coverage.info <- coverage(root = paste(r.home, source.folder, sep = "/"))
    cmd <- paste(r.home, "/bin/Rscript --no-restore --slave --quiet ", paste(run.script, 
                                                                             tc.full.path, sep = " "), sep = "")
    cmd.output <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    after.tc.coverage.info <- coverage(root = paste(r.home, source.folder, sep = "/"))
    sink()
    
    before.tc.coverage.percentage <- calculateCoverage(before.tc.coverage.info)
    if (is.nan(before.tc.coverage.percentage)) 
      before.tc.coverage.percentage <- 0
    after.tc.coverage.percentage <- calculateCoverage(after.tc.coverage.info)
    
    print(tc)
    print(before.tc.coverage.percentage)
    print(after.tc.coverage.percentage)
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
  if (missing(tc.db.path)) 
    stop("A directory containing TC DB files must be specified!");
  if (missing(r.home)) 
    stop('A directory containing VM source files must be specified!'); 
  if (missing(source.folder)) 
    stop('A directory containing source files must be specified!');  
  if (length(list.files(path = tc.db.path, recursive = TRUE)) == 0){
    print("TC Database is empty")
    return ()
  }
  sink("db_out")
  before.db.coverage.info <- coverage(root = paste(r.home, source.folder, sep = "/"))
  cmd <- paste(r.home, 
               "/bin/Rscript --no-restore --slave --quiet ", 
               paste(run.script, tc.db.path, sep = " "), 
               sep = "")
  print(cmd)
  cmd.output <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
  after.db.coverage.info <- coverage(root = paste(r.home, source.folder, sep = "/"))
  sink()
  file.remove("db_out")
  before.db.coverage.percentage <- calculateCoverage(before.db.coverage.info)
  if (is.nan(before.db.coverage.percentage)) 
    before.db.coverage.percentage <- 0
  after.db.coverage.percentage <- calculateCoverage(after.db.coverage.info)
  print(before.db.coverage.percentage)
  print("Coverage by TCs in database")
  print(after.db.coverage.percentage)
} 

cleanTCDB <- function(tc.db.path){
  if (missing(tc.db.path)) 
    stop("A directory containing TC DB files must be specified!");
  cmd <- paste("find", tc.db.path, "-name", "*.[rR]", "-delete", sep=" ");
  system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE);
}