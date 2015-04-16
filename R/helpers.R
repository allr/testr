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
