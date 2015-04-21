#' @title Write down capture information 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' @param fname function name
#' @param fbody function body
#' @param args arguments to function call
#' @param retv return value of a specified function call with arguments
#' @param errs caught errors during function call
#' @param warns caught warnings during function call
#' @seealso Decorate
#' @export
#' 
ProcessTC <- function(tc.file, tc.result.root, tc.db = NULL, r.home, source.folder) {
  cache$r.home <- r.home
  cache$source.folder <- source.folder
  cache$tc.result.root <- tc.result.root
  
  if (!file.exists(tc.result.root))
    dir.create(tc.result.root)
  
  n <- ceiling(GetNumberOfTC(tc.file)/16)
  
  SplitTCs(tc = tc.file, 
           tc.result.root = cache$temp_dir, 
           number.of.tc.per.file = n)
  
  FilterTCs(tc.root = cache$temp_dir, 
            tc.db.path = tc.db, 
            tc.result.root = tc.result.root,
            clear.previous.coverage = TRUE, 
            wipe.tc.database = FALSE) 
  
  n <- ceiling(n/2)
  while (n > 1) {
    SplitTCs(tc = tc.result.root, 
             tc.result.root = cache$temp_dir, 
             number.of.tc.per.file = n, 
             check.correctness = TRUE)
    FilterTCs(tc.root = cache$temp_dir, 
              tc.db.path = tc.db, 
              tc.result.root = tc.result.root,
              clear.previous.coverage = TRUE, 
              wipe.tc.database = FALSE) 
    n <- ceiling(n/2)
  }
  ResetCoverageInfo(r.home) 
  rm(list = ls(all = TRUE))
}

SplitTCs<- function(tc.file, tc.split.root, number.of.tc.per.file = 1) {
  # In case tc is diretory, recursively call this function on all files in directory
  if (file.info(tc.file)$isdir){
    all.tc <- list.files(tc.file, 
                         recursive=TRUE, 
                         all.files = TRUE, 
                         pattern = "\\.[rR]$", 
                         full.names = T)
    for (test.case in all.tc)
      SplitTCs(test.case, tc.split.root, number.of.tc.per.file)
    return(NULL);
  }
  
  function.name <- GetFunctionName(basename(tc.file))
  tc.split.root <- file.path(tc.split.root, function.name)
  if (!file.exists(tc.split.root)) 
    dir.create(tc.split.root)
  file.index <- length(list.files(tc.split.root))
  
  lines <- readLines(tc.file)
  if (length(lines) == 0)
    stop("Empty file\n")
  
  # based on cat that each test case has line break before it
  tests.starts <- head(c(1, grep("^[ ]*$", lines) + 1), -1)
  if (length(tests.starts) == 0) tests.starts <- head(c(1, grep("^[ ]*$", lines) + 1), -1)
  tests.ends <- grep("^[ ]*$", lines)
  if (length(tests.ends) == 0) tests.ends <- grep("^[ ]*$", lines)
  
  if (testrOptions('verbose')) {
    cat("File ", tc.file, "\n")
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