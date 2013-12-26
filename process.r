source("testr/tc-filter.r")
source("testr/target.r")

library(tools)

processTC <- function(tc.file, tc.final.root, r.home, source.folder) {
  tc.file <- file_path_as_absolute(tc.file)

  if (!file.exists(tc.final.root))
    dir.create(tc.final.root)
  if (!file.exists("correct-and-split-tc"))
    dir.create("correct-and-split-tc")
  
  path <- splitAndFindCorrectTCs(tc.file, "correct-and-split-tc")
  
  filterTCs(tc.root = path, r.home = r.home, source.folder = source.folder, 
            tc.db.path = tc.final.root, clear.previous.coverage = TRUE, wipe.tc.database = FALSE, use.tc.db =TRUE) 
}

splitAndFindCorrectTCs<- function(tc, tc.final.root) {
  correctTC <- 0
  failedTC <- 0
  filename <- basename(tc)
  function.name <- substr(filename, 4, nchar(filename) - 2)
  tc.function.path <- paste(tc.final.root, function.name, sep="/")
  tc.full.path <- tc
  
  con <- file(tc.full.path)
  lines <- readLines(con)
  close(con)
  
  tests.starts <- grep("test\\(id",lines)
  if (length(grep("expected", lines[tests.starts - 1]))==length(tests.starts)){
    tests.starts <- tests.starts - 1
  }
  if (length(tests.starts) > 1){
    tests.ends <- tests.starts[2:length(tests.starts)]
    tests.ends <- tests.ends - 1
    tests.ends <- append(tests.ends, length(lines))
  }
  else
    tests.ends <- length(lines)
  
  cat(paste("File ",tc,"\n", sep=""))
  cat(paste("Number of TCs in file - ", length(tests.starts), "\n", sep=""))
  
  for (i in 1:length(tests.starts)){
    cat(paste("Test case number ", i, "\n", sep=""))
    if (!file.exists(tc.function.path)){
      dir.create(tc.function.path)
    }
    
    tc.temp.full.path <- paste(tc.function.path, "/", function.name,"_",i, ".R", sep="")
    
    sink(tc.temp.full.path)
    
    for (j in tests.starts[i]:tests.ends[i]){
      cat(lines[j])
      cat("\n")
    }
    sink()
    
    tc.temp.full.path <- file_path_as_absolute(tc.temp.full.path)
    sink(paste(tc.function.path, "/", function.name, "_info", sep=""), append = TRUE)
    tc.result <- runTests(tc.temp.full.path)
    cat("\n")
    sink()
    
    if (tc.result)
    {
      correctTC <- correctTC + 1
      cat("\tPassed\n")
    }
    else
    {
      failedTC <- failedTC + 1
      file.remove(tc.temp.full.path)
      cat("\tFailed\n")
    }
    
  }
  cat(paste("Correct/Fail: ",correctTC, "/", failedTC, "\n", sep = ""))
  return(tc.function.path)
}

processTCfromCommandLine <- function(){
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 3)  {
    processTC(tc.file = args[2], tc.final.root = args[3], r.home = args[1], source.folder = "src/main/")
  }else{
  }
}

processTCfromCommandLine()