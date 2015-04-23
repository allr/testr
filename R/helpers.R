#' @title Get function name from filename
#' 
#' This function extracts a function name from testcase file name
#' @param filename filename
#' @seealso ProcessTC
GetFunctionName <- function(filename){
  spl <- strsplit(filename, "_")
  ifelse((length(spl[[1]]) == 2), substr(spl[[1]][2], 1, nchar(spl[[1]][2]) - 2), spl[[1]][2])
}

#' @title Check if function is S3 generic
#' 
#' Determine if function has a call to UseMethod. In that case there is no need to capture it.
#' @param fname function name
#' @seealso Decorate
IsS3Generic <- function(fname, env) {
  f <- get(fname, mode = "function", envir = env)
  if (is.null(body(f))) return(FALSE)
  uses <- codetools::findGlobals(f, merge = FALSE)$functions
  any(uses == "UseMethod")
}


#' @title Estimate the number of test cases
#'
#' Estimate the number of test cases in the give path
#' @param path path to check
#' @seealso ProcessTC
GetNumberOfTC <- function(path){
  if (file.info(path)$isdir) 
    path <- list.files(path, pattern = "\\.[rR]$", recursive = TRUE, all.files = TRUE)
  lines <- vector()
  for (file in path) 
    lines <- c(lines, readLines(file))
  length(grep("test\\(id",lines))
}

#' @title Clean temporary directory
#'
#' Make sure temp dir is empty by deleting unnecessary files
CleanTempDir <- function() {
  for (file in list.files(cache$temp_dir, full.names = T, pattern = "\\.RData|\\.[rR]$"))
    file.remove(file)
}

ParseAndCheck <- function(what) {
  tryCatch({eval(parse(text=what)); TRUE}, error=function(e){FALSE})
}

#' @title Quote language from evaluation
#'
#' In certain cases, language arguments (like calls), need to be quoated
#' @param arg list of arguments
#' @seealso GenerateTC
Quoter <- function(arg) {
  if (is.list(arg)) {
    org.attrs <- attributes(arg)
    res <- lapply(arg, function(x) if(is.language(x)) enquote(x) else Quoter(x))
    attributes(res) <- org.attrs
    res
  }
  else arg
}

#' @title Removes prefixes and quote from line
#'
#' @description Used for processing capture file information. Deletes prefixes to get essential information
#' @param l input line
#' @seealso ProcessClosure
SubstrLine <- function(l){
  if (grepl("^quote\\(", l)){
    ret.line <- strsplit(l, "\\(")[[1]][2];
    if (substr(ret.line, nchar(ret.line), nchar(ret.line)) == ")")
      ret.line <- substr(ret.line, 0, nchar(ret.line) - 1)
  }else{
    ret.line <- substr(l, 7, nchar(l));     
  }
  ret.line
}

#' @title Check if line starts with prefix
#'
#' @param prefix prefix
#' @param x text to be checked
#' @seealso GenerateTC
StartsWith <- function(prefix, x) {
  grepl(paste("^", prefix, sep=""), x)
}

#' @title Find test directory for package
#'
#' @param path package path
#' @seealso CapturePackage
FindTestDir <- function(path) 
{
  testthat <- file.path(path, "tests", "testthat")
  if (file.exists(testthat) && file.info(testthat)$isdir) 
    return(testthat)
  inst <- file.path(path, "inst", "tests")
  if (file.exists(inst) && file.info(inst)$isdir) 
    return(inst)
  stop("No testthat directories found in ", path, call. = FALSE)
}