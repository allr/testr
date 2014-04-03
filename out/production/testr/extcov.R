source("testr/coverage.r")

measureCoverage <- function(root, reset.coverage.before = FALSE, reset.coverage.after = FALSE){
  if (reset.coverage.before)
    reset(root)
  result <- coverage(root)
  if (reset.coverage.after)
    reset(root)
}

externalMeasureCoverage <- function(){
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 3)  {
    cat("Args 2 - ", as.logical(args[2]), "\n")
    cat("Args 3 - ", as.logical(args[3]), "\n")
    measureCoverage(args[1], as.logical(args[2]), as.logical(args[3]))
  }else{
  }
}

externalMeasureCoverage()
