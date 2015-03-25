#' @title Decorates function to capture calls and return values 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' Replaces the function by decorated function in the global environment
#' @param func function name as a character string
#' @export 
#' @seealso WriteCapInfo Decorate
#'
Decorate <- function(func, envir = .GlobalEnv){
  if (class(func) == "function"){
    fname <- as.character(substitute(func))
  } else if (class(func) == "character"){
    fname <- func
  } else {
    stop("wrong argument type!")
  }    
  exit.capturer <- function() {
#     testr:::WriteCapInfo(fname, 
#                    args = cache$arguments[[length(cache$arguments)]], 
#                    retv = NULL, errs = NULL, warns = NULL) 
    cache$arguments <- cache$arguments[-length(cache$arguments)]
  }
  entry.capturer <- function() {
    cache$arguments[[length(cache$arguments) + 1]] <- testr:::GetArgs(sys.frame(sys.nframe() - 5))
  } 
  do.call(trace, list(what=fname, tracer=entry.capturer, exit=exit.capturer, print = FALSE))
  cache$decorated <- c(cache$decorated, fname)
} 

#' @title Undecorate function
#' 
#' Reset previously decorate function
#' @param func function name as a character string
#' @export 
#' @seealso WriteCapInfo Decorate
#'
Undecorate <- function(func) {
  if (class(func) == "function"){
    fname <- as.character(substitute(func))
  } else if (class(func) == "character"){
    fname <- func
  } else {
    stop("wrong argument type!")
  }  
  ind <- which(fname %in% cache$decorated)
  if (length(ind) == 0)
    stop("Function was not decorated!")
  do.call(untrace, list(fname))
  cache$decorated <- cache$decorated[-ind]
}

#' @title Write down capture information 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' @param fname function name
#' @param args arguments to function call
#' @param retv return value of a specified function call with arguments
#' @param errs caught errors during function call
#' @param warns caught warnings during function call
#' @seealso Decorate
#' @useDynLib testr
#' @importFrom Rcpp evalCpp
#' @export
#' 
WriteCapInfo <- function(fname, args, retv, errs, warns){
  if (cache$writing.down)
    return(NULL);
  .Call('testr_WriteCapInfo_cpp', PACKAGE = 'testr', fname, args, retv, errs, warns)
}

#' @title Setup information capturing for list of function
#' 
#' This function is respinsible for setting up capturing for functions
#' 
#' @param flist function or list of functions to turn on capturing for. List should be only as character.
#' @seealso Decorate
#' @export
SetupCapture <- function(flist){
  for (func in flist){
    if (EligibleForCapture(func)){
      Decorate(func)
    }
  }
}

#' @title Check if function is eligible for wrapping to capture arguments and return values
#' 
#' This function checks that supplied function for capture is not a keyword, operator or in the blacklist (functions like rm, .GlobalEnv, etc.)
#' This is an internal function and is supposed to be used in SetupCapture
#' @param func function name to check
#' @return TRUE/FALSE if can be captured or not
#' @seealso SetupCapture
EligibleForCapture <- function(func){
  return (!length(getAnywhere(func)$objs) == 0 &&
            class(getAnywhere(func)[1]) == "function" &&
            !func %in% blacklist &&
            !func %in% operators &&
            !func %in% keywords &&
            !func %in% sys &&
            !func %in% env)
}

#' @title Setup capture of builtin functions
#' 
#' Sets up capturing of builtin functions
#' @param internal wheather only internals should be captured, or all builtins
#' @param functions list of functions to be decorate
#' @param indexes specific indexes from functions vector
#' @seealso SetupCapture
#' @export
BeginBuiltinCapture <- function(internal = FALSE, functions = builtins(internal), indexes){
  if (!missing(indexes))
    SetupCapture(functions)
  else
    SetupCapture(functions[indexes])
}

#' @title Clear decoration
#' 
#' Clear anything previously decorate
#' @seealso Undecorate
#' @export
ClearDecoration <- function() {
  for (fname in cache$decorated)
    Undecorate(fname)
}
