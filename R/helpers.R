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
IsS3Generic <- function(fname) {
  f <- get(fname, env = parent.frame(), mode = "function")
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