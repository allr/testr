source('~/Documents/RProject/testr/target.r')
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 0)  {
  runTests(args[1])
}else{
  # just for testing
  runTests('~/Documents/RProject/wd/tc/')
}
