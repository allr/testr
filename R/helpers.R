#' @title Get function name from filename
#' 
#' This function extracts a function name from testcase file name
#' @param filename filename
#' @seealso ProcessTC
determineFunctionName <- function(filename){
  filename <- basename(filename)
  spl <- strsplit(filename, "_")
  if (length(spl[[1]]) == 2)
    function.name <- substr(spl[[1]][2], 1, nchar(spl[[1]][2]) - 2)
  else
    function.name <- spl[[1]][2]
  return (function.name)
}

#' @title Check if function is S3 generic
#' 
#' Determine if function has a call to UseMethod. In that case there is no need to capture it.
#' @param fname function name
#' @seealso Decorate
is_s3_generic <- function(fname) {
  f <- get(fname, env = parent.frame(), mode = "function")
  if (is.null(body(f))) return(FALSE)
  uses <- findGlobals(f, merge = FALSE)$functions
  any(uses == "UseMethod")
}