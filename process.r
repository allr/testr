source("testr/tc-filter.r")
source("testr/target.r")

library(tools)

processTC <- function(tc.file, tc.final.root, r.home, source.folder) {
  temp.dir <<- "temp"
  r.home <<- r.home
  source.folder <<- source.folder
  tc.file <- file_path_as_absolute(tc.file)
  
  if (!file.exists(tc.final.root))
    dir.create(tc.final.root)
  if (!file.exists("temp"))
    dir.create("temp")
  
  k <<- 1
  n <- round(getNumberOfTC(tc.file) /4 + 0.00001)
  path <- splitAndFindCorrectTCs(tc.file,tc.final.root, n)
  filterTCs(tc.root = path[1], r.home = r.home, source.folder = source.folder, 
            tc.db.path = tc.final.root, clear.previous.coverage = TRUE, wipe.tc.database = FALSE, use.tc.db = FALSE) 
  temp.tc <- list.files(path[2], full.names=TRUE)
  n <- round(n / 2 + 0.00001)
  while (n >= 1){
    cat("n - ", n, "\n")
    k <<- 1
    path <- splitAndFindCorrectTCs(path[2],tc.final.root, n)
    file.remove(temp.tc)
    filterTCs(tc.root = path[1], r.home = r.home, source.folder = source.folder, 
              tc.db.path = tc.final.root, clear.previous.coverage = TRUE, wipe.tc.database = FALSE, use.tc.db = FALSE) 
    temp.tc <- list.files(path[2], full.names=TRUE)
    if (n == 1)
      n <- 0
    n <- round(n / 2 + 0.00001)
  }
}

splitAndFindCorrectTCs<- function(tc, tc.final.root, n) {
  if (file.info(tc)$isdir){
    all.tc <- list.files(tc, recursive=TRUE)
    for (test.case in all.tc){
      path.temp <<- splitAndFindCorrectTCs(paste(tc, test.case, sep = "/"), tc.final.root, n)
    }
    return (path.temp);
  }
  temp.path <- "temp"
  correctTC <- 0
  failedTC <- 0
  filename <- basename(tc)
  spl <- strsplit(filename, "_")
  if (length(spl[[1]]) == 2)
    function.name <- substr(spl[[1]][2], 1, nchar(spl[[1]][2]) - 2)
  else
    function.name <- spl[[1]][2]
  tc.function.path <- paste(temp.path, function.name, sep="/")
  tc.full.path <- tc

#  if (!file.exists(temp.path))
#    dir.create(temp.path)
  
  con <- file(tc.full.path)
  lines <- readLines(con)
  close(con)
  if (length(lines) == 0)
    stop("Empty file\n")
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
  tc.full.path <- paste(tc.function.path, "/tc_", function.name,"_", k, ".R", sep="")
  for (i in 1:length(tests.starts)){
    if (!file.exists(tc.function.path)){
      cat("TC Function path - ", tc.function.path, "\n")
      dir.create(tc.function.path)
    }
    tc.temp.full.path <- paste(tc.function.path, "/", function.name,"_tmp", ".R", sep="")
    sink(tc.temp.full.path, append = TRUE)
    for (j in tests.starts[i]:tests.ends[i]){
      cat(lines[j], "\n")
    }
    sink()
    tc.temp.full.path <- file_path_as_absolute(tc.temp.full.path)
    sink(paste(tc.function.path, "/", function.name, "_info", sep=""), append = TRUE)
    tc.result <- runTests(tc.temp.full.path)
    cat("\n")
    sink()
    if (tc.result)    {
      correctTC <- correctTC + 1
      file.append(tc.full.path, tc.temp.full.path)
    } else {
      failedTC <- failedTC + 1
    }
    file.remove(tc.temp.full.path)
    if (i %% n == 0 || i == length(tests.starts)){
      k <<- k + 1
      tc.full.path <- paste(tc.function.path, "/tc_", function.name, "_", k, ".R", sep="")
    }
  }
  file.remove(paste(tc.function.path, "/", function.name, "_info", sep=""))
  cat(paste("Correct/Fail: ",correctTC, "/", failedTC, "\n", sep = ""))
  return (c(tc.function.path, paste(tc.final.root, function.name, sep ="/")))
}

getNumberOfTC <- function(path){
  con <- file(path)
  lines <- readLines(con)
  close(con)
  if (length(lines) == 0)
    stop("Empty file\n")
  tests.starts <- grep("test\\(id",lines)
  if (length(grep("expected", lines[tests.starts - 1]))==length(tests.starts)){
    tests.starts <- tests.starts - 1
  }
  return (length(tests.starts))
}

processTCfromCommandLine <- function(){
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 3)  {
    processTC(tc.file = args[2], tc.final.root = args[3], r.home = args[1], source.folder = "src/main/")
  }else{
  }
}

processTCfromCommandLine()