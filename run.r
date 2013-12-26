source('testr/target.r')

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 1)  {
  runTests(args[1])
}else{
  stop("Wrong number of arguments")
}
