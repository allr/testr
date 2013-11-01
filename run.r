# change after testing
source('~/Documents/RProject/testr/target.r')
args <- commandArgs(trailingOnly = TRUE)
print("commandLine")
print(args[1])
if (length(args) != 0)  {
  runTests(args[1])
}else{
  # just for testing
  runTests('~/Documents/RProject/wd/tc/')
}
