#intended measuring coverage and evaluationg testcases
library(tools)
source('testr/coverage.R')

calculateCoverage <- function(result){
  totalLine.file        <- sum(as.numeric(result$file$'LOC'));
  totalCovLine.file     <- sum(as.numeric(result$file$'CovLn'));
  totalCovLinePcnt.file <- round(totalCovLine.file / totalLine.file * 100, digits=2);
  # line func coverage
  totalLine.func        <- sum(as.numeric(result$func$'LOC'));
  totalCovLine.func     <- sum(as.numeric(result$func$'CovLn'));
  totalCovLinePcnt.func <- round(totalCovLine.func / totalLine.func * 100, digits=2);
  # func coverage
  totalFunc        <- nrow(result$func);
  totalCovFunc     <- nrow(result$func[as.numeric(result$func$'CovLn')>0,]);
  totalCovFuncPcnt <- round(totalCovFunc / totalFunc * 100, digits=2);
  # file coverage
  totalFile        <- nrow(result$file);
  totalCovFile     <- nrow(result$file[as.numeric(result$file$'CovLn')>0,]);
  totalCovFilePcnt <- round(totalCovFile / totalFile * 100, digits=2);
  #if (!is.nan(totalCovLinePcnt.file) && abs(totalCovLinePcnt.file - totalCovLinePcnt.func)>0.01)
   # stop("Something is wrong")
  #else
  return (totalCovLinePcnt.file)
}

filterTests <- function(tcRoot = 'wd/tc', rHome = 'R-3.0.1/', sourceFolder = "src/main"){
  #if (missing(rHome)) stop("A directory containing VM source files must be specified!");
  #if (missing(tcRoot)) stop("A directory containing Test Cases must be specified!");
  reset(paste(rHome,sourceFolder, sep=""));
  
  rHome <- file_path_as_absolute(rHome);
  tcRoot <- file_path_as_absolute(tcRoot);
  #initiazations
  runPath <- file_path_as_absolute("testr/run.r");
  tcDBPath <- file_path_as_absolute("testr/tc_db/");
  tc <- list.files(path=tcRoot, recursive=TRUE)
  coverageIncrease <- fuction(tc_file){
    
  }
  sink('output')
  before.coverage.info <- coverage(root = paste(rHome,sourceFolder, sep="/"));
  cmd <- paste(rHome,"/bin/Rscript --no-restore --slave --quiet ",paste(runPath, tcRoot, sep=" "),sep="");
  print(cmd)
  r <- system(cmd, intern=TRUE, ignore.stderr=TRUE);
  after.coverage.info <- coverage(root = paste(rHome,sourceFolder, sep="/"));
  sink()
  #intermediate printing
  before.coverage.percentage <- calculateCoverage(before.coverage.info)
  if (is.nan(before.coverage.percentage))
    before.coverage.percentage <- 0
  after.coverage.percentage <- calculateCoverage(after.coverage.info)
  print(before.coverage.percentage)
  print(after.coverage.percentage)
  if (after.coverage.percentage > before.coverage.percentage){
    #place to add testcases
  }
  
  
}

