#' @title Process Test Case file
#' 
#' This function is respinsible for splitting a file into single test cases and filtering out 
#' the unneeded file based or R and C code coverage
#' @param tc.file test case file to process
#' @param tc.result.root folder with resulting filtered test cases
#' @param tc.db folder with test case detabase
#' @param r.home home directory of R VM with C code coverage support
#' @param source.folder folder in R VM to measure C code coverage of
#' @seealso Decorate
#' @export
#' 
ProcessTC <- function(tc.file, tc.result.root, tc.db = NULL, r.home = NULL, source.folder = NULL) {
  cache$tc.result.root <- tc.result.root
  
  if (!file.exists(tc.result.root))
    dir.create(tc.result.root)
  
  n <- ceiling(GetNumberOfTC(tc.file)/16)
  
  SplitTCs(tc.root = tc.file, 
           tc.split.root = cache$temp_dir, 
           number.of.tc.per.file = n)
  
  FilterTCs(tc.root = cache$temp_dir, 
            tc.db.path = tc.db, 
            tc.result.root = tc.result.root,
            r.home = r.home,
            source.folder = source.folder,
            clear.previous.coverage = TRUE, 
            wipe.tc.database = FALSE) 
  
  while(n != 1) {
    n <- ceiling(n/2)
    SplitTCs(tc.root = tc.result.root, 
             tc.split.root = cache$temp_dir, 
             number.of.tc.per.file = n) 
    file.remove(list.files(tc.result.root, recursive = T, full.names = T))
    FilterTCs(tc.root = cache$temp_dir, 
              tc.db.path = tc.db, 
              tc.result.root = tc.result.root,
              r.home = r.home,
              source.folder = source.folder,
              clear.previous.coverage = TRUE, 
              wipe.tc.database = FALSE) 
  }
}

#' @title Split TestCase files
#'
#' This function takes a test cases files and splits them according to maximum number of tests per file 
#' @param tc.root test case file/folder to split
#' @param tc.split.root resulting location of split
#' @param number.of.tc.per.file maximum number of test cases per file
#' @seealso ProcessTC
#'
SplitTCs<- function(tc.root, tc.split.root, number.of.tc.per.file = 1) {
  # In case tc is diretory, recursively call this function on all files in directory
  if (file.info(tc.root)$isdir){
    all.tc <- list.files(tc.root, 
                         recursive=TRUE, 
                         all.files = TRUE, 
                         pattern = "\\.[rR]$", 
                         full.names = T)
    for (test.case in all.tc)
      SplitTCs(test.case, tc.split.root, number.of.tc.per.file)
    return(invisible())
  }
  
  function.name <- GetFunctionName(basename(tc.root))
  tc.split.root <- file.path(tc.split.root, function.name)
  if (!file.exists(tc.split.root)) 
    dir.create(tc.split.root)
  file.index <- length(list.files(tc.split.root))
  
  lines <- readLines(tc.root)
  if (length(lines) == 0)
    stop("Empty file\n")
  
  # based on cat that each test case has line break before it
  tests.starts <- head(c(1, grep("^[ ]*$", lines) + 1), -1)
  if (length(tests.starts) == 0) tests.starts <- head(c(1, grep("^[ ]*$", lines) + 1), -1)
  tests.ends <- grep("^[ ]*$", lines)
  if (length(tests.ends) == 0) tests.ends <- grep("^[ ]*$", lines)
  
  if (testrOptions('verbose')) {
    cat("File ", tc.root, "\n")
    cat("Number of TCs in file - ", length(tests.starts), "\n")
  }
  
  # bulk fines together and then write it
  res_list <- list()
  for (i in 1:length(tests.starts))
    res_list[[i]] <- lines[tests.starts[i]:tests.ends[i]]
  ntc <- 1
  for (i in 1:length(res_list)) {
    write(res_list[[i]], 
          file.path(tc.split.root,
                    paste0("tc_", function.name, "_", ntc + file.index, ".R")),
          append = T)  
    if (i %% number.of.tc.per.file == 0) ntc <- ntc + 1
  } 
}
