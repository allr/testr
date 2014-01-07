source("testr/tc-filter.r")
source("testr/target.r")

library(tools)

temp.dir <<- "temp"

processTC <- function(tc.file, tc.result.root, r.home, source.folder) {
  r.home <<- r.home
  source.folder <<- source.folder
  tc.file <- file_path_as_absolute(tc.file)
  
  if (!file.exists(tc.result.root))
    dir.create(tc.result.root)
  if (!file.exists(temp.dir))
    dir.create(temp.dir)
  k <<- 1
  n <- round(getNumberOfTC(tc.file) /4 + 0.00001)
  split.paths <- splitAndFindCorrectTCs(tc.file, tc.result.root, n, TRUE)
  filterTCs(tc.root = split.paths[1], 
            r.home = r.home, 
            source.folder = source.folder, 
            tc.db.path = tc.result.root, 
            clear.previous.coverage = TRUE, 
            wipe.tc.database = FALSE, 
            use.tc.db = FALSE) 
  temp.tc <- list.files(split.paths[2], 
                        full.names = TRUE)
  n <- round(n / 2 + 0.00001)
  while (n >= 1){
    cat("n - ", n, "\n")
    k <<- 1
    split.paths <- splitAndFindCorrectTCs(split.paths[2], tc.result.root, n, FALSE)
    file.remove(temp.tc)
    filterTCs(tc.root = split.paths[1], 
              r.home = r.home, 
              source.folder = source.folder, 
              tc.db.path = tc.result.root, 
              clear.previous.coverage = TRUE, 
              wipe.tc.database = FALSE, 
              use.tc.db = FALSE) 
    temp.tc <- list.files(split.paths[2], full.names = TRUE)
    if (n == 1)
      n <- 0
    n <- round(n / 2 + 0.00001)
  }
  
}

splitAndFindCorrectTCs<- function(tc, tc.result.root, n = 1, check.correctness = TRUE) {
  # In case tc is diretory, recursively call this function on all files in directory
  if (file.info(tc)$isdir){
    all.tc <- list.files(tc, recursive=TRUE, all.files = TRUE, pattern = "\\.[rR]$")
    for (test.case in all.tc){
      tc.path <- file.path(tc, test.case, fsep = .Platform$file.sep)
      path.temp <- splitAndFindCorrectTCs(tc.path, tc.result.root, n)
    }
    return (path.temp);
  }
  correct.tcs <- 0
  failed.tcs <- 0
  filename <- basename(tc)
  function.name <- determineFunctionName(filename)  
  function.path <- file.path(temp.dir, function.name, fsep = .Platform$file.sep)
  tc.full.path <- file_path_as_absolute(tc)  
  # split file with bunch of TCs in single TC files
  con <- file(tc.full.path)
  lines <- readLines(con)
  close(con)
  if (length(lines) == 0)
    stop("Empty file\n")
  tests.starts <- grep("test\\(id",lines)
  if (length(grep("expected", lines[tests.starts - 1])) == length(tests.starts)){
    tests.starts <- tests.starts - 1
  }
  if (length(tests.starts) > 1){
    tests.ends <- tests.starts[2 : length(tests.starts)]
    tests.ends <- tests.ends - 1
    tests.ends <- append(tests.ends, length(lines))
  }
  else
    tests.ends <- length(lines)
  
  cat("File ", tc, "\n")
  cat("Number of TCs in file - ", length(tests.starts), "\n")
  
  tc.result <- file.path(function.path, paste0("tc_", function.name, "_", k, ".R", sep = ""), fsep = .Platform$file.sep)
  # Process TCs one by one, by creating a temp file with one TC and running it to check it's correctness. Then it is appended to the result file
  for (i in 1:length(tests.starts)){
    if (!file.exists(function.path)){
      cat("TC Function path - ", function.path, "\n")
      dir.create(function.path)
    }
    tc.temp <- file.path(function.path, paste0(function.name, "_tmp", ".R", sep = ""), fsep = .Platform$file.sep)
    sink(tc.temp)
    for (j in tests.starts[i]:tests.ends[i])
      cat(lines[j], "\n")
    sink()
    tc.temp <- file_path_as_absolute(tc.temp)
    if (check.correctness){
      info.file <- file.path(function.path, paste0(function.name, "_info", sep = ""), fsep = .Platform$file.sep)
      sink(info.file, append = TRUE)
      run.result <- runTests(tc.temp)
      cat("\n")
      sink()
    }else{
      run.result <- TRUE
    }
    if (run.result){
      correct.tcs <- correct.tcs + 1
      file.append(tc.result, tc.temp)
    } else {
      failed.tcs <- failed.tcs + 1
    }
    file.remove(tc.temp)
    if (correct.tcs %% n == 0 || i == length(tests.starts)){
      k <<- k + 1
      tc.result <- file.path(function.path, paste0("tc_", function.name, "_", k, ".r", sep = ""), fsep = .Platform$file.sep)
    }
  }
  file.remove(info.file)
  cat("Correct/Fail: ", correct.tcs, "/", failed.tcs, "\n")
  return (c(function.path, file.path(tc.result.root, function.name, fsep = .Platform$file.sep)))
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

determineFunctionName <- function(filename){
  spl <- strsplit(filename, "_")
  if (length(spl[[1]]) == 2)
    function.name <- substr(spl[[1]][2], 1, nchar(spl[[1]][2]) - 2)
  else
    function.name <- spl[[1]][2]
  return (function.name)
}

processTCfromCommandLine <- function(){
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 3)  {
    processTC(tc.file = args[2], tc.result.root = args[3], r.home = args[1], source.folder = "src/main/")
  }else{
  }
}

processTCfromCommandLine()